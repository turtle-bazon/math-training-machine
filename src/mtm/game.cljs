(ns mtm.game
  "Round mechanics: question generation, scoring and the tick loop.

  A round is a 60 second time attack. Every question shows a drained
  speed bar; answering while the bar is full pays the biggest speed
  bonus, and letting it run out marks the question as missed."
  (:require [mtm.db :as db]
            [mtm.sound :as sound]
            [mtm.api :as api]
            [clojure.string :as str]))

(def round-ms 60000)
(def question-ms 6000)
(def feedback-ms 550)
(def wrong-feedback-ms 3000)   ;; hold the correct answer on screen after a miss
(def tick-ms 100)
(def difficulties [10 20 50 100])

(defonce ^:private round-ms* (atom round-ms))

(defn round-duration-ms [] @round-ms*)

(defn init-duration!
  "Optional ?rt=<seconds> query parameter shortens the round - handy for
  demos and for the end-to-end test."
  []
  (try
    (when (exists? js/URLSearchParams)
      (let [raw (.get (js/URLSearchParams. (or (.-search js/location) "")) "rt")
            secs (when raw (js/parseInt raw 10))]
        (when (and secs (not (js/isNaN secs)) (<= 1 secs 600))
          (reset! round-ms* (* 1000 secs)))))
    (catch :default _ nil)))

;;; --- question generation ------------------------------------------------

(defn- rnd [lo hi]
  (+ lo (rand-int (inc (- hi lo)))))

(defn- operand [bound positive?]
  ;; `positive?` keeps every operand on the positive side of zero for the
  ;; "all positive numbers" class; otherwise signed operands are used.
  (if positive?
    (let [v (rnd 1 bound)]
      (if (zero? v) 1 v))
    (let [v (rnd (- bound) bound)]
      (if (zero? v) (rnd 1 bound) v))))

(defn- gcd [a b]
  (if (zero? b) (Math/abs a) (gcd b (mod (Math/abs a) (Math/abs b)))))

(defn- reduce-frac [{:keys [n d]}]
  (let [g (gcd n d)
        d (if (neg? d) (- d) d)
        n (if (neg? d) (- n) n)]
    {:n (/ n g) :d (/ d g)}))

(defn- make-fraction [bound positive?]
  (let [d (rnd 2 9)]
    (reduce-frac
     {:n (if positive?
           (rnd 1 (dec d))
           (let [v (rnd (inc (- d)) (dec d))] (if (zero? v) 1 v)))
      :d d})))

(defn- frac+ [a b]
  (reduce-frac {:n (+ (* (:n a) (:d b)) (* (:n b) (:d a)))
                :d (* (:d a) (:d b))}))

(defn- frac- [a b]
  (reduce-frac {:n (- (* (:n a) (:d b)) (* (:n b) (:d a)))
                :d (* (:d a) (:d b))}))

(defn- frac* [a b]
  (reduce-frac {:n (* (:n a) (:n b))
                :d (* (:d a) (:d b))}))

(defn frac-gt? [a b]
  (> (* (:n a) (:d b)) (* (:n b) (:d a))))

(defn format-frac [v]
  (if (and (map? v) (contains? v :n) (contains? v :d))
    (if (= 1 (:d v)) (str (:n v)) (str (:n v) "/" (:d v)))
    (str v)))

(defn format-value [v]
  (format-frac v))

(defn level
  "Difficulty grows every five correct answers, capped at level 4."
  [correct]
  (min (count difficulties) (inc (quot correct 5))))

(defn- bound-for [lvl]
  (dec (nth difficulties (dec lvl))))

(defn- distractors [answer a b op]
  (let [op-candidates (case op
                        :+ [(- a b)]
                        :- [(+ a b)]
                        :* [(+ a b) (- a b) (* (inc a) b) (* a (inc b))]
                        [])
        candidates (concat [(inc answer) (dec answer)
                           (+ answer 10) (- answer 10)
                           (- answer)
                           (+ answer 2) (- answer 2)
                           (+ answer 5) (- answer 5)
                           (+ answer 3) (- answer 3)]
                          op-candidates)
        picked (->> candidates
                    (remove #{answer})
                    distinct
                    (take 3)
                    vec)
        fill (if (< (count picked) 3)
               (->> (repeatedly 12 #(rnd (- answer 20) (+ answer 20)))
                    (remove (into #{answer} picked))
                    distinct
                    (take (- 3 (count picked))))
               ())]
    (shuffle (conj (into picked fill) answer))))

(def op-tokens #{"sum" "add" "plus"
                 "diff" "subtract" "minus" "sub"
                 "mult" "multiply" "prod" "product" "times"
                 "mixed" "all"})

(def class-tokens #{"pos" "positive" "signed" "any"})

(defn mode-tokens [mode]
  (str/split (str/lower-case (name (or mode :sum-signed))) #"[-_ ]+"))

(defn operation-key
  "Normalise a mode (keyword or string, possibly combined like
  `sum-positive` or `positive-sum`) to one of :sum :diff :mult :mixed."
  [mode]
  (let [toks (mode-tokens mode)
        op (some op-tokens toks)]
    (case op
      ("sum" "add" "plus") :sum
      ("diff" "subtract" "minus" "sub") :diff
      ("mult" "multiply" "prod" "product" "times") :mult
      ("mixed" "all") :mixed
      (if (some #{"pos" "positive"} toks) :mixed :mixed))))

(defn fraction-mode?
  "True when MODE asks for the fractions number class."
  [mode]
  (boolean (some #{"frac" "fraction" "fractions" "rational" "rationals"} (mode-tokens mode))))

(defn positive-mode?
  "True when MODE asks for an all-positive number class."
  [mode]
  (if (fraction-mode? mode)
    (not (some #{"signed"} (mode-tokens mode)))
    (boolean (some #{"pos" "positive"} (mode-tokens mode)))))

(defn class-key
  "Normalise a mode to one of :positive :signed :frac :frac-signed."
  [mode]
  (let [toks (mode-tokens mode)]
    (cond
      (fraction-mode? mode)
      (if (some #{"signed"} toks) :frac-signed :frac)
      (some #{"signed"} toks) :signed
      :else :positive)))

(defn combined-mode
  "Build a canonical combined mode keyword from an operation keyword and a
  class keyword (:positive/:signed/:frac/:frac-signed)."
  [op class]
  (keyword (str (name op) "-" (name class))))

(declare frac-distractors)

(defn gen-question [mode lvl]
  (let [pos? (positive-mode? mode)
        frac? (fraction-mode? mode)
        op-key (operation-key mode)
        op (case op-key
             :sum :+
             :diff :-
             :mult :*
             :mixed (rand-nth [:+ :- :*]))
        r (if (= op :*) 9 (bound-for lvl))
        [a b] (if frac?
                (let [x (make-fraction r pos?)
                      y (make-fraction r pos?)]
                  (if (and pos? (= op :-) (not (frac-gt? x y)))
                    [y (make-fraction r pos?)]
                    [x y]))
                (if pos?
                  (let [x (operand r true)
                        y (operand r true)]
                    (if (and (= op :-) (<= x y))
                      [y (max 1 (dec x))]
                      [x y]))
                  [(operand r false) (operand r false)]))
        answer (if frac?
                 (case op :+ (frac+ a b) :- (frac- a b) :* (frac* a b))
                 (case op :+ (+ a b) :- (- a b) :* (* a b)))]
    {:a a :b b :op op :answer answer
     :positive? pos?
     :frac? frac?
     :options (if frac?
                (frac-distractors answer a b op)
                (distractors answer a b op))
     :started-at (js/Date.now)}))


(defn- frac-distractors [answer a b op]
  (let [candidates [(reduce-frac {:n (inc (:n answer)) :d (:d answer)})
                    (reduce-frac {:n (dec (:n answer)) :d (:d answer)})
                    (reduce-frac {:n (- (:n answer)) :d (:d answer)})
                    (reduce-frac {:n (:n answer) :d (inc (:d answer))})
                    (reduce-frac {:n (:n a) :d (:d b)})
                    (reduce-frac {:n (:n b) :d (:d a)})]
        picked (->> candidates
                    (remove #(= % answer))
                    distinct
                    (take 3)
                    vec)
        fill (if (< (count picked) 3)
               (->> (repeatedly 12
                      #(reduce-frac {:n (rnd -9 9)
                                     :d (rnd 2 9)}))
                    (remove #(or (= % answer) (some #{%} picked)))
                    distinct
                    (take (- 3 (count picked))))
               ())]
    (shuffle (conj (into picked fill) answer))))

(defn question-text [{:keys [a b op positive? frac?]}]
  (let [sym (case op :+ "+" :- "-" :* "*")
        fmt (fn [v] (format-frac v))]
    (if frac?
      (if positive?
        (str (fmt a) " " sym " " (fmt b) " = ?")
        (str "(" (fmt a) ") " sym " (" (fmt b) ") = ?"))
      (if positive?
        (str (fmt a) " " sym " " (fmt b) " = ?")
        (str "(" (fmt a) ") " sym " (" (fmt b) ") = ?")))))

(defn new-round [mode]
  (let [now (js/Date.now)
        duration (round-duration-ms)]
    {:mode mode
     :started-at now
     :ends-at (+ now duration)
     :question (gen-question mode 1)
     :answered? false
     :picked nil
     :outcome nil        ;; :correct | :wrong | :timeout
     :points 0
     :streak 0
     :best-streak 0
     :correct 0
     :total 0
     :sum-ms 0
     :last-points 0
     :tick-warned nil
     :last-beep nil}))

(defn- combo [streak]
  (+ 1 (* 0.25 (min 8 (max 0 (dec streak))))))

(defn- speed-bonus [elapsed]
  (Math/round (* 100 (max 0 (- 1 (/ elapsed question-ms))))))

(defn points-for [streak elapsed]
  (Math/round (* (+ 100 (speed-bonus elapsed)) (combo streak))))

(defn- next-question [g]
  (let [lvl (level (:correct g))]
    (assoc g
           :question (gen-question (:mode g) lvl)
           :answered? false
           :picked nil
           :outcome nil
           :last-points 0
           :last-beep nil)))

(defn- settle
  "Common path for correct / wrong / timed out answers."
  [g outcome picked now]
  (if (:answered? g)
    g
    (let [elapsed  (- now (get-in g [:question :started-at]))
          streak   (if (= outcome :correct) (inc (:streak g)) 0)
          points   (if (= outcome :correct) (points-for streak elapsed) 0)]
      (-> g
          (assoc :answered? true
                 :outcome outcome
                 :picked picked
                 :streak streak
                 :last-points points
                 :points (+ (:points g) points)
                 :total (inc (:total g))
                 :correct (if (= outcome :correct) (inc (:correct g)) (:correct g))
                 :sum-ms (if (= outcome :correct) (+ (:sum-ms g) elapsed) (:sum-ms g)))
          (update :best-streak max streak)))))

(defn- feedback-sounds [outcome streak]
  (case outcome
    :correct (do (sound/play! :correct)
                 (when (>= streak 3) (sound/play! :combo)))
    :wrong   (sound/play! :wrong)
    :timeout (sound/play! :timeout)))

;;; --- timers -------------------------------------------------------------

(defonce ^:private timers (atom {:interval nil :timeout nil}))

(defn- clear-timeout! []
  (when-let [t (:timeout @timers)]
    (js/clearTimeout t)
    (swap! timers assoc :timeout nil)))

(defn- schedule-next! []
  (clear-timeout!)
  (let [g     @db/game
        miss? (contains? #{:wrong :timeout} (:outcome g))
        hold  (if miss? wrong-feedback-ms feedback-ms)]
    ;; the extra time the correct answer stays on screen must not eat into
    ;; the round clock, otherwise a miss near the end would cut it short
    (when miss?
      (swap! db/game update :ends-at + (- wrong-feedback-ms feedback-ms)))
    (swap! timers assoc :timeout
           (js/setTimeout
            (fn []
              (swap! timers assoc :timeout nil)
              (when-let [g @db/game]
                (when (:answered? g)
                  (swap! db/game next-question))))
            hold))))

;;; --- events -------------------------------------------------------------

(defn- end-round! []
  (clear-timeout!)
  (when-let [t (:interval @timers)]
    (js/clearInterval t)
    (swap! timers assoc :interval nil))
  (when-let [g @db/game]
    (let [elapsed (min (round-duration-ms) (- (js/Date.now) (:started-at g)))
          results {:mode (:mode g)
                   :score (:points g)
                   :correct (:correct g)
                   :total (:total g)
                   :best-streak (:best-streak g)
                   :avg-ms (if (pos? (:correct g))
                             (Math/round (/ (:sum-ms g) (:correct g)))
                             0)
                   :elapsed-ms elapsed
                   :saved-best (db/save-personal-best! (:mode g) (:points g))}]
      (sound/play! :gameover)
      (reset! db/results results)
      (reset! db/game nil)
      (api/game-played (fn [_]
                         (api/stats (fn [res] (when (:ok res) (reset! db/stats (:data res)))))))
      (swap! db/state assoc :screen :results))))

(defn- tick! []
  (when-let [g @db/game]
    (let [now   (js/Date.now)
          left  (- (:ends-at g) now)
          q     (:question g)
          qleft (- (+ (:started-at q) question-ms) now)]
      (cond
        (<= left 0)
        (end-round!)

        :else
        (do
          ;; countdown warning in the last 10 seconds
          (let [sec (quot left 1000)]
            (when (and (<= sec 10) (pos? sec) (not= sec (:tick-warned g)))
              (swap! db/game assoc :tick-warned sec)
              (sound/play! :tick)))
          ;; accelerating countdown beep as the question timer runs out
          (when (and (not (:answered? g)) (pos? qleft) (<= qleft 3000))
            (let [interval (cond (<= qleft 1000) 250
                                 (<= qleft 2000) 500
                                 :else 1000)
                  last-beep (:last-beep g)]
              (when (or (nil? last-beep) (>= (- now last-beep) interval))
                (sound/unlock!)
                (sound/play! :tick)
                (swap! db/game assoc :last-beep now))))
          ;; a question nobody answered in time
          (when (and (not (:answered? g)) (<= qleft 0))
            (let [resolved (settle g :timeout nil now)]
              (sound/unlock!)
              (feedback-sounds :timeout 0)
              (swap! db/game (constantly resolved))
              (schedule-next!)))
          (swap! db/game assoc :now now))))))

(defn start-round! [mode]
  (sound/unlock!)
  (sound/play! :start)
  (clear-timeout!)
  (when-let [t (:interval @timers)]
    (js/clearInterval t))
  (reset! db/game (new-round mode))
  (swap! db/state assoc :screen :play)
  (swap! timers assoc :interval (js/setInterval tick! tick-ms)))

(defn stop-round! []
  (clear-timeout!)
  (when-let [t (:interval @timers)]
    (js/clearInterval t)
    (swap! timers assoc :interval nil))
  (reset! db/game nil))

(defn answer!
  "Player picked one of the four options."
  [choice]
  (when-let [g @db/game]
    (when-not (:answered? g)
      (let [now    (js/Date.now)
            right? (= choice (get-in g [:question :answer]))
            outcome (if right? :correct :wrong)
            resolved (settle g outcome choice now)]
        (sound/unlock!)
        (sound/play! :select)
        (feedback-sounds outcome (:streak resolved))
        (swap! db/game (constantly resolved))
        (schedule-next!)))))

(defn time-left [g]
  (max 0 (- (:ends-at g) (or (:now g) (js/Date.now)))))

(defn question-left [g]
  (let [q (:question g)]
    (if (:answered? g)
      0
      (max 0 (- (+ (:started-at q) question-ms)
                (or (:now g) (js/Date.now)))))))
