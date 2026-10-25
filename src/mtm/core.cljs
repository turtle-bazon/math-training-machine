(ns mtm.core
  "Entry point: mounts the app and wires the global keyboard."
  (:require [reagent.dom.client :as rdom]
            [mtm.db :as db]
            [mtm.game :as game]
            [mtm.sound :as sound]
            [mtm.views :as views]))

(defonce ^:private root-el (atom nil))
(defonce ^:private client (atom nil))

(defn- goto [screen]
  (sound/play! :click)
  (when (= screen :menu)
    (game/stop-round!))
  (swap! db/state assoc :screen screen))

(defn- option-index [key]
  (case key
    ("1" "2" "3" "4") (dec (js/parseInt key 10))
    ("a" "A") 0
    ("s" "S") 1
    ("d" "D") 2
    ("f" "F") 3
    nil))

(defn- on-key-down [e]
  (sound/unlock!)
  (let [key (.-key e)
        screen (:screen @db/state)]
    (when-let [idx (option-index key)]
      (when (and (= screen :play) @db/game)
        (.preventDefault e)
        (let [options (get-in @db/game [:question :options])]
          (when (< idx (count options))
            (game/answer! (nth options idx))))))
    (case screen
      :menu
      (when (or (= key "Enter") (= key " "))
        (.preventDefault e)
        (game/start-round! (:mode @db/settings)))

      :play
      (when (= key "Escape")
        (.preventDefault e)
        (goto :menu))

      :results
      (cond
        (= key "Enter")
        (when-not (.-altKey e)
          (let [input (.querySelector js/document ".name-input")]
            (if (and input (not= (.-value input) ""))
              (do (.preventDefault e) (.click (.querySelector js/document ".save-row .primary")))
              (do (.preventDefault e)
                  (game/start-round! (:mode @db/results))))))
        (= key "Escape") (goto :menu))

      :leaderboard
      (when (= key "Escape") (goto :menu))

      nil)))

(defn- global-click [e]
  (sound/unlock!))

(defn- mount! []
  (when-let [el (.getElementById js/document "app")]
    (reset! root-el el)
    (reset! client (rdom/create-root el))
    (rdom/render @client [views/root])))

(defn reload! []
  (when (and @client @root-el)
    (rdom/render @client [views/root])))

(defn init []
  (db/load-settings!)
  (game/init-duration!)
  (mount!)
  (.addEventListener js/window "keydown" on-key-down true)
  (.addEventListener js/window "click" global-click true)
  (.addEventListener js/window "pointerdown" global-click true)
  (.addEventListener js/window "keydown" on-key-down)
  (.addEventListener js/window "click" global-click)
  (.addEventListener js/window "pointerdown" global-click))
