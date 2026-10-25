(ns mtm.views
  "Every screen of the machine: menu, play, results and leaderboard."
  (:require [reagent.core :as r]
            [mtm.db :as db]
            [mtm.game :as game]
            [mtm.sound :as sound]
            [mtm.api :as api]))

(def operations
  [[:sum   "+"]
   [:diff  "-"]
   [:mult  "*"]
   [:mixed "∑"]])

(def number-classes
  [[:positive     "1..n"]
   [:signed       "±n"]
   [:frac         "1/2"]
   [:frac-signed  "±1/2"]])

(def translations
  {:en {:sum "SUM" :diff "DIFFERENCE" :mult "PRODUCT" :mixed "MIXED"
        :positive "ALL POSITIVE" :signed "SIGNED" :frac "FRACTIONS"
        :frac-signed "SIGNED FRACTIONS"
        :personal-best "PERSONAL BEST" :no-record "no record yet - set one!"
        :start-round "START ROUND" :leaderboard "LEADERBOARD"
        :sound-on "SOUND ON" :sound-off "SOUND OFF"
        :score "SCORE" :streak "STREAK" :time "TIME"
        :miss "MISS" :too-slow "TOO SLOW"
        :round-over "ROUND OVER" :new-best "NEW BEST!"
        :correct "CORRECT" :accuracy "ACCURACY" :best-streak "BEST STREAK"
        :avg-reaction "AVG REACTION" :saving "saving score..."
        :unreachable "server unreachable - score kept on this device"
        :retry "RETRY" :ranked-on "on the" :board "board"
        :name-placeholder "your name" :save-score "SAVE SCORE"
        :play-again "PLAY AGAIN" :menu "MENU" :mode "MODE"
        :math "MATH" :training "TRAINING" :machine "MACHINE"
        :operation "OPERATION" :numbers "NUMBERS"
        :back "BACK" :refresh "REFRESH" :loading "loading..."
        :no-scores "no scores yet - be the first"
        :server-unavailable "server unavailable"
        :lang "LANG"}
   :tr {:sum "TOPLAMA" :diff "ÇIKARMA" :mult "ÇARPMA" :mixed "KARIŞIK"
        :positive "TÜMÜ POZİTİF" :signed "İŞARETLİ" :frac "KESİRLER"
        :frac-signed "İŞARETLİ KESİRLER"
        :personal-best "KİŞİSEL REKOR" :no-record "henüz rekor yok - bir tane koy!"
        :start-round "TURU BAŞLAT" :leaderboard "LİDER TABLOSU"
        :sound-on "SES AÇIK" :sound-off "SES KAPALI"
        :score "SKOR" :streak "SERİ" :time "SÜRE"
        :miss "HATA" :too-slow "ÇOK YAVAŞ"
        :round-over "TUR BİTTİ" :new-best "YENİ REKOR!"
        :correct "DOĞRU" :accuracy "İSABET" :best-streak "EN İYİ SERİ"
        :avg-reaction "ORT. TEPKİ" :saving "skor kaydediliyor..."
        :unreachable "sunucuya ulaşılamıyor - skor bu cihazda saklandı"
        :retry "YENİDEN DENE" :ranked-on "tablosunda" :board ""
        :name-placeholder "adın" :save-score "SKORU KAYDET"
        :play-again "TEKRAR OYNA" :menu "MENÜ" :mode "MOD"
        :math "MATEMATİK" :training "ANTRENMAN" :machine "MAKİNESİ"
        :operation "İŞLEM" :numbers "SAYILAR"
        :back "GERİ" :refresh "YENİLE" :loading "yükleniyor..."
        :no-scores "henüz skor yok - ilk sen ol"
        :server-unavailable "sunucuya ulaşılamıyor"
        :lang "DİL"}})

(defn t
  [lang k]
  (or (get-in translations [lang k])
      (get-in translations [:en k]
             (name k))))

(defn set-lang! [l]
  (db/set-setting! :lang l))

(defn- telegram-user-id []
  (try
    (let [tg (.-Telegram js/window)]
      (when tg
        (let [init (.-initDataUnsafe tg)]
          (when init
            (let [u (.-user init)]
              (when u
                (str (.-id u))))))))
    (catch :default _ nil)))

(defn- source-name [base]
  (let [tg (telegram-user-id)]
    (if tg
      tg
      (str base "#source:" (db/device-id)))))

(defn share! []
  (let [url (.-href js/location)]
    (if (and (exists? js/navigator) (.-share js/navigator))
      (.share js/navigator #js{:title "Math Training Machine" :url url})
      (do
        (when (.-clipboard js/navigator)
          (.writeText (.-clipboard js/navigator) url))
        (js/alert (if (= (or (:lang @db/settings) :en) :tr) "Bağlantı kopyalandı" "Link copied"))))))

(defn- operation-label [op]
  (or (some (fn [[k l _]] (when (= k op) l)) operations) (name op)))

(defn- mode-label [m lang]
  (str (t (or lang :en) (game/operation-key m))
       " "
       (if (game/positive-mode? m) (t (or lang :en) :positive) (t (or lang :en) :signed))))

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
        seconds (Math/ceil (/ left 1000))
        lang (or (:lang @db/settings) :en)]
    [:div.hud
     [:div.hud-cell
      [:span.hud-label (t lang :score)]
      [:span.hud-value (str (:points g))]]
     [:div.hud-cell.center
      [:span.hud-label (t lang :streak)]
      [:span.hud-value (if (pos? (:streak g)) (str "x" (:streak g)) "—")]
      (when (>= (:streak g) 3)
        [:div.combo-meter
         [:div.combo-fill
          {:style {:width (str (min 100 (* 12.5 (min 8 (dec (:streak g))))) "%")}}]])]
     [:div.hud-cell.right
      [:span.hud-label (t lang :time)]
      [:span.hud-value {:class (when (< seconds 11) "urgent")} (str seconds)]]]))

(defn- feedback [g]
  (let [lang (or (:lang @db/settings) :en)]
    (when (:answered? g)
      [:div.feedback {:class (name (or (:outcome g) :none))}
       [:div.fb-line
        (case (:outcome g)
          :correct (str "+" (:last-points g))
          :wrong   (t lang :miss)
          :timeout (t lang :too-slow)
          "")]
       (when (and (= (:outcome g) :correct) (>= (:streak g) 3))
         [:div.fb-combo (str "COMBO x" (:streak g))])])))

;;; --- menu ---------------------------------------------------------------

(defn- operation-picker []
  (let [current (:mode @db/settings)
        lang (or (:lang @db/settings) :en)]
    [:div.mode-picker
     (for [[k example] operations]
       ^{:key (name k)}
       [:button {:class (str "mode" (when (= k (game/operation-key current)) " active"))

                 :on-click #(do (sound/play! :click)
                                (db/set-setting! :mode
                                                (game/combined-mode k
                                                                   (game/class-key current))))}
        [:span.mode-example example]])]))

(defn- class-picker []
  (let [current (:mode @db/settings)
        lang (or (:lang @db/settings) :en)]
    [:div.class-picker
     (for [[k example] number-classes]
       ^{:key (name k)}
       [:button {:class (str "class" (when (= k (game/class-key current)) " active"))
                 :style (when (= k (game/class-key current))
                          {:border-color "#ffcf4d" :color "#ffcf4d"})
                 :on-click #(do (db/set-setting! :mode
                                                (game/combined-mode (game/operation-key current)
                                                                   k)))}
        [:span.mode-example example]])]))

(defn menu []
  (let [settings @db/settings
        lang (or (:lang settings) :en)
        best (db/personal-best (:mode settings))]
    [:div.screen.menu
     [:div.logo
      [:div.logo-top (t lang :math)]
      [:div.logo-mid (t lang :training)]
      [:div.logo-bot (t lang :machine)]]
     [:div.picker-label (t lang :operation)]
     [operation-picker]
     [:div.picker-label (t lang :numbers)]
     [class-picker]
     [:div.best-line
      (if best
        [:span (str (t lang :personal-best) "  ") [:b (str best)] " pts"]
        [:span.dim (t lang :no-record)])]
     [:div.big-buttons
      [:button.primary.big
       {:on-click #(game/start-round! (:mode settings))}
       (t lang :start-round)]
      [:button.ghost
       {:on-click #(do (sound/play! :leaderboard)
                       (swap! db/state assoc :screen :leaderboard))}
       (t lang :leaderboard)]
      [:button.ghost.share-btn
       {:on-click #(share!)}
       [:svg {:width 20 :height 20 :viewBox "0 0 24 24" :fill "none"
              :stroke "currentColor" :stroke-width 2 :stroke-linecap "round"
              :stroke-linejoin "round"}
        [:circle {:cx 18 :cy 5 :r 3}]
        [:circle {:cx 6 :cy 12 :r 3}]
        [:circle {:cx 18 :cy 19 :r 3}]
        [:line {:x1 8.59 :y1 13.51 :x2 15.42 :y2 17.49}]
        [:line {:x1 15.41 :y1 6.51 :x2 8.59 :y2 10.49}]]]]
     [:div.menu-footer
      [:button.icon-btn {:on-click #(sound/toggle-sound!)}
       (if (:sound settings) (str "♪ " (t lang :sound-on)) (str "♪ " (t lang :sound-off)))]
      [:button.icon-btn
       {:style (when (= lang :en) {:border-color "#ffcf4d" :color "#ffcf4d"})
        :on-click #(set-lang! :en)}
       "EN"]
      [:button.icon-btn
       {:style (when (= lang :tr) {:border-color "#ffcf4d" :color "#ffcf4d"})
        :on-click #(set-lang! :tr)}
       "TR"]]]))

;;; --- play ---------------------------------------------------------------

(defn- option [idx value g]
  (let [answered? (:answered? g)
        answer    (get-in g [:question :answer])
        picked?   (= value (:picked g))
        correct?  (= value answer)
        timeout?  (= (:outcome g) :timeout)
        class     (cond
                    (and answered? correct? timeout?) "opt timeout"
                    (and answered? correct?)          "opt correct"
                    (and answered? picked?)          "opt wrong"
                    :else "opt")]
    [:button.option
     {:class class
      :disabled answered?
      :on-click #(game/answer! value)}
     [:span.opt-key (str (inc idx))]
     [:span.opt-value (game/format-value value)]]))

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
                (let [base (api/trim-name @name*)
                      n (source-name base)]
                  (when (and (seq base) (not (keyword? @saved)))
                    (db/set-setting! :name base)
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
            lang (or (:lang @db/settings) :en)
            st       @saved
            accuracy (if (pos? (:total res))
                       (Math/round (* 100 (/ (:correct res) (:total res))))
                       0)]
        [:div.screen.results
         [:div.result-head (t lang :round-over)]
         [:div.score-big (str (:score res))]
         [:div.score-caption
          (str (mode-label (:mode res) lang) " " (t lang :mode))
          (when (:saved-best res) [:span.newbest (str " " (t lang :new-best))])]
         [:div.stats
          [stat (t lang :correct)   (str (:correct res) "/" (:total res)) nil]
          [stat (t lang :accuracy)  (str accuracy "%") nil]
          [stat (t lang :best-streak) (str "x" (:best-streak res)) nil]
          [stat (t lang :avg-reaction)
           (if (pos? (:avg-ms res)) (str (:avg-ms res) " ms") "—") nil]]
         (cond
           (= st :saving)
           [:div.save-row [:span.dim (t lang :saving)]]

           (= st :error)
           [:div.save-row
            [:span.dim (t lang :unreachable)]
            [:button.ghost {:on-click save!} (t lang :retry)]]

           (map? st)
           [:div.saved
            [:span (if (= lang :tr)
                     (str (mode-label (:mode res) lang) " tablosunda #"
                          (str (or (:rank st) "?")) ". sıradasın")
                     (str "ranked #" (str (or (:rank st) "?"))
                          " on the " (mode-label (:mode res) lang) " board"))]]

           :else
           [:div.save-row
            [:input.name-input
             {:value @name*
              :placeholder (t lang :name-placeholder)
              :maxLength 16
              :on-change #(reset! name* (.. % -target -value))
              :on-key-down #(when (= (.-key %) "Enter") (save!))}]
            [:button.primary {:on-click save!} (t lang :save-score)]])
         [:div.big-buttons
          [:button.primary.big
           {:on-click #(game/start-round! (:mode res))} (t lang :play-again)]
          [:button.ghost
           {:on-click #(do (sound/play! :leaderboard)
                           (swap! db/state assoc :screen :leaderboard))}
           (t lang :leaderboard)]
          [:button.ghost
           {:on-click #(swap! db/state assoc :screen :menu)} (t lang :menu)]]]))))

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
            mode   (or (:mode board) (:mode @db/settings))
            lang   (or (:lang @db/settings) :en)]
        (when-not @booted?
          (reset! booted? true)
          (fetch-board! mode))
        [:div.screen.leaderboard
         [:div.lb-head
          [:button.icon-btn
           {:on-click #(do (sound/play! :click)
                           (swap! db/state assoc :screen :menu))}
           (str "← " (t lang :back))]
          [:button.icon-btn
           {:on-click #(fetch-board! mode)} (str "↻ " (t lang :refresh))]]
         [:div.lb-title (t lang :leaderboard)]
         [:div.lb-tabs
          (for [[k example] operations]
            ^{:key (str "op-" (name k))}
            [:button {:class (str "tab" (when (= k (game/operation-key mode)) " active"))
                      :on-click #(do (sound/play! :click)
                                     (fetch-board! (game/combined-mode k (game/class-key mode))))}
             (t lang k)])]
         [:div.lb-tabs
          (for [[k example] number-classes]
            ^{:key (str "cls-" (name k))}
            [:button {:class (str "tab" (when (= k (game/class-key mode)) " active"))
                      :on-click #(do (sound/play! :click)
                                     (fetch-board! (game/combined-mode (game/operation-key mode) k)))}
             (t lang k)])]
         (case (:status board)
           :loading [:div.lb-body.dim (t lang :loading)]
           :error   [:div.lb-body.dim (t lang :server-unavailable)]
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
             [:div.lb-body.dim (t lang :no-scores)]))]))))

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
        lang (or (:lang @db/settings) :en)
        hints  (if (coarse-pointer?)
                 (if (= lang :tr)
                   [[:span "bir cevaba dokun"]
                    [:span "başlatmak için dokun"]]
                   [[:span "tap an answer"]
                    [:span "tap to start"]])
                 (if (= lang :tr)
                   [[:span "[1-4] cevap"]
                    [:span "[ENTER] başlat"]
                    [:span "[ESC] geri"]]
                   [[:span "[1-4] answer"]
                    [:span "[ENTER] start"]
                    [:span "[ESC] back"]]))]
    [:div.cabinet
     [:header.marquee [bulbs]]
     [:main.screen-wrap
      (case screen
        :play        [play]
        :results     [results]
        :leaderboard [leaderboard]
        [menu])]
     (into [:footer.controls] hints)]))
