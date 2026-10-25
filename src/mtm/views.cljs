(ns mtm.views
  "Every screen of the machine: menu, play, results and leaderboard."
  (:require [reagent.core :as r]
            [mtm.db :as db]
            [mtm.game :as game]
            [mtm.sound :as sound]
            [mtm.api :as api]))

(def modes
  [[:sum   "SUM"        "(-7) + 12"]
   [:diff  "DIFFERENCE" "5 - (-9)"]
   [:mixed "MIXED"      "both, mixed"]])

(defn- mode-label [m]
  (or (some (fn [[k l _]] (when (= k m) l)) modes) (name m)))

;;; --- shared bits --------------------------------------------------------

(defn- time-bar [g]
  (let [left (game/time-left g)
        pct  (* 100 (/ left (game/round-duration-ms)))]
    [:div.timebar
     [:div.timebar-fill
      {:style {:width (str pct "%")}
       :class (when (< left 10000) "urgent")}]]))

(defn- speed-bar [g]
  (let [left (game/question-left g)
        pct  (* 100 (/ left game/question-ms))]
    [:div.speedbar
     [:div.speedbar-fill {:style {:width (str pct "%")}}]]))

(defn- hud [g]
  (let [left    (game/time-left g)
        seconds (Math/ceil (/ left 1000))]
    [:div.hud
     [:div.hud-cell
      [:span.hud-label "SCORE"]
      [:span.hud-value (str (:points g))]]
     [:div.hud-cell.center
      [:span.hud-label "STREAK"]
      [:span.hud-value (if (pos? (:streak g)) (str "x" (:streak g)) "—")]
      (when (>= (:streak g) 3)
        [:div.combo-meter
         [:div.combo-fill
          {:style {:width (str (min 100 (* 12.5 (min 8 (dec (:streak g))))) "%")}}]])]
     [:div.hud-cell.right
      [:span.hud-label "TIME"]
      [:span.hud-value {:class (when (< seconds 11) "urgent")} (str seconds)]]]))

(defn- feedback [g]
  (when (:answered? g)
    [:div.feedback {:class (name (or (:outcome g) :none))}
     [:div.fb-line
      (case (:outcome g)
        :correct (str "+" (:last-points g))
        :wrong   "MISS"
        :timeout "TOO SLOW"
        "")]
     (when (and (= (:outcome g) :correct) (>= (:streak g) 3))
       [:div.fb-combo (str "COMBO x" (:streak g))])]))

;;; --- menu ---------------------------------------------------------------

(defn- mode-picker []
  (let [current (:mode @db/settings)]
    [:div.mode-picker
     (for [[k label example] modes]
       ^{:key (name k)}
       [:button.mode
        {:class (when (= k current) "active")
         :on-click #(do (sound/play! :click) (db/set-setting! :mode k))}
        [:span.mode-label label]
        [:span.mode-example example]])]))

(defn menu []
  (let [settings @db/settings
        best     (db/personal-best (:mode settings))]
    [:div.screen.menu
     [:div.logo
      [:div.logo-top "MATH"]
      [:div.logo-mid "TRAINING"]
      [:div.logo-bot "MACHINE"]]
     [mode-picker]
     [:div.best-line
      (if best
        [:span "PERSONAL BEST  " [:b (str best)] " pts"]
        [:span.dim "no record yet - set one!"])]
     [:div.big-buttons
      [:button.primary.big
       {:on-click #(game/start-round! (:mode settings))}
       "START ROUND"]
      [:button.ghost
       {:on-click #(do (sound/play! :leaderboard)
                       (swap! db/state assoc :screen :leaderboard))}
       "LEADERBOARD"]]
     [:div.menu-footer
      [:button.icon-btn {:on-click #(sound/toggle-sound!)}
       (if (:sound settings) "♪ SOUND ON" "♪ SOUND OFF")]]]))

;;; --- play ---------------------------------------------------------------

(defn- option [idx value g]
  (let [answered? (:answered? g)
        answer    (get-in g [:question :answer])
        picked?   (= value (:picked g))
        correct?  (= value answer)
        class     (cond
                    (and answered? correct?) "opt correct"
                    (and answered? picked?)  "opt wrong"
                    :else "opt")]
    [:button.option
     {:class class
      :disabled answered?
      :on-click #(game/answer! value)}
     [:span.opt-key (str (inc idx))]
     [:span.opt-value (str value)]]))

(defn play []
  (if-let [g @db/game]
    (let [q (:question g)]
      [:div.screen.play
       [hud g]
       [time-bar g]
       [:div.question-zone
        [:div.question (game/question-text q)]
        [speed-bar g]]
       [:div.options
        (doall
         (map-indexed
          (fn [i v] ^{:key (str i "-" v)} [option i v g])
          (:options q)))]
       [feedback g]])
    [:div.screen.play [:div.dim "getting ready..."]]))

;;; --- results ------------------------------------------------------------

(defn- stat [label value sub]
  [:div.stat
   [:span.stat-label label]
   [:span.stat-value value]
   (when sub [:span.stat-sub sub])])

(defn results []
  (let [name* (r/atom (or (:name @db/settings) ""))
        saved (r/atom nil)
        save! (fn []
                (let [n (api/trim-name @name*)]
                  (when (and (seq n) (not (keyword? @saved)))
                    (db/set-setting! :name n)
                    (reset! saved :saving)
                    (api/submit {:name n
                                 :mode   (name (:mode @db/results))
                                 :score  (:score @db/results)
                                 :correct (:correct @db/results)
                                 :total  (:total @db/results)
                                 :best-streak (:best-streak @db/results)
                                 :avg-ms (:avg-ms @db/results)}
                                (fn [res]
                                  (reset! saved (if (:ok res) (:data res) :error)))))))]
    (fn []
      (let [res      @db/results
            st       @saved
            accuracy (if (pos? (:total res))
                       (Math/round (* 100 (/ (:correct res) (:total res))))
                       0)]
        [:div.screen.results
         [:div.result-head "ROUND OVER"]
         [:div.score-big (str (:score res))]
         [:div.score-caption
          (str (mode-label (:mode res)) " MODE")
          (when (:saved-best res) [:span.newbest " NEW BEST!"])]
         [:div.stats
          [stat "CORRECT"   (str (:correct res) "/" (:total res)) nil]
          [stat "ACCURACY"  (str accuracy "%") nil]
          [stat "BEST STREAK" (str "x" (:best-streak res)) nil]
          [stat "AVG REACTION"
           (if (pos? (:avg-ms res)) (str (:avg-ms res) " ms") "—") nil]]
         (cond
           (= st :saving)
           [:div.save-row [:span.dim "saving score..."]]

           (= st :error)
           [:div.save-row
            [:span.dim "server unreachable - score kept on this device"]
            [:button.ghost {:on-click save!} "RETRY"]]

           (map? st)
           [:div.saved
            [:span "ranked #" (str (or (:rank st) "?"))
             " on the " (mode-label (:mode res)) " board"]]

           :else
           [:div.save-row
            [:input.name-input
             {:value @name*
              :placeholder "your name"
              :maxLength 16
              :on-change #(reset! name* (.. % -target -value))
              :on-key-down #(when (= (.-key %) "Enter") (save!))}]
            [:button.primary {:on-click save!} "SAVE SCORE"]])
         [:div.big-buttons
          [:button.primary.big
           {:on-click #(game/start-round! (:mode res))} "PLAY AGAIN"]
          [:button.ghost
           {:on-click #(do (sound/play! :leaderboard)
                           (swap! db/state assoc :screen :leaderboard))}
           "LEADERBOARD"]
          [:button.ghost
           {:on-click #(swap! db/state assoc :screen :menu)} "MENU"]]]))))

;;; --- leaderboard --------------------------------------------------------

(defn- fetch-board! [mode]
  (swap! db/leaderboard assoc :status :loading :mode mode)
  (api/leaderboard mode 20
                   (fn [res]
                     (if (:ok res)
                       (swap! db/leaderboard assoc
                              :status :ready
                              :mode mode
                              :entries (get-in res [:data :entries] []))
                       (swap! db/leaderboard assoc
                              :status :error :mode mode :entries [])))))

(defn- lb-row [entry rank highlight?]
  [:div.lb-row {:class (when highlight? "mine")}
   [:span.lb-rank
    (if (<= rank 3) (nth ["🥇" "🥈" "🥉"] (dec rank)) (str rank))]
   [:span.lb-name (str (:name entry))]
   [:span.lb-acc (str (:accuracy entry) "%")]
   [:span.lb-score (str (:score entry))]])

(defn leaderboard []
  (let [booted? (atom false)]
    (fn []
      (let [board  @db/leaderboard
            mode   (or (:mode board) (:mode @db/settings))]
        (when-not @booted?
          (reset! booted? true)
          (fetch-board! mode))
        [:div.screen.leaderboard
         [:div.lb-head
          [:button.icon-btn
           {:on-click #(do (sound/play! :click)
                           (swap! db/state assoc :screen :menu))}
           "← BACK"]
          [:button.icon-btn
           {:on-click #(fetch-board! mode)} "↻ REFRESH"]]
         [:div.lb-title "LEADERBOARD"]
         [:div.lb-tabs
          (for [[k label] modes]
            ^{:key (name k)}
            [:button.tab {:class (when (= k mode) "active")
                          :on-click #(do (sound/play! :click)
                                         (fetch-board! k))}
             label])]
         (case (:status board)
           :loading [:div.lb-body.dim "loading..."]
           :error   [:div.lb-body.dim "server unavailable"]
           (if (seq (:entries board))
             [:div.lb-body
              (doall
               (map-indexed
                (fn [i e]
                  ^{:key (str i "-" (:id e))}
                  [lb-row e (inc i)
                   (and (:results @db/results)
                        (= (:name e) (:name @db/settings))
                        (= (:score e) (:score @db/results)))])
                (:entries board)))]
             [:div.lb-body.dim "no scores yet - be the first"]))]))))

;;; --- root ---------------------------------------------------------------

(defn- bulbs []
  [:div.bulbs
   (doall
    (map-indexed (fn [i _] ^{:key i} [:span.bulb]) (range 14)))])

(defn- coarse-pointer?
  "True on touch devices - where the keyboard hints make no sense."
  []
  (try
    (and (exists? js/window.matchMedia)
         (.-matches (js/window.matchMedia "(pointer: coarse)")))
    (catch :default _ false)))

(defn root []
  (let [screen (:screen @db/state)
        hints  (if (coarse-pointer?)
                 [[:span "tap an answer"]
                  [:span "tap to start"]]
                 [[:span "[1-4] answer"]
                  [:span "[ENTER] start"]
                  [:span "[ESC] back"]])]
    [:div.cabinet
     [:header.marquee [bulbs] [:h1 "MATH TRAINING MACHINE"]]
     [:main.screen-wrap
      (case screen
        :play        [play]
        :results     [results]
        :leaderboard [leaderboard]
        [menu])]
     (into [:footer.controls] hints)]))
