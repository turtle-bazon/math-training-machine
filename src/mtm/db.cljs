(ns mtm.db
  "Application state. Split into a few small atoms so that screens,
  settings and the active round never clobber each other."
  (:require [reagent.core :as r]
            [cljs.reader :as reader]))

(def state
  "UI-level state: which screen is visible, transient banners, etc."
  (r/atom {:screen :menu}))

(def settings
  "Persisted user settings: game mode, sound toggle, player name."
  (r/atom {:mode :sum-signed :sound true :name "" :lang nil}))

(def game
  "The round currently being played, or nil."
  (r/atom nil))

(def results
  "Final tally of the last finished round."
  (r/atom nil))

(def leaderboard
  "Leaderboard payload fetched from the backend."
  (r/atom {:status :idle :mode nil :entries []}))

(def stats
  "Visitor/game statistics fetched from the backend."
  (r/atom nil))

;;; --- localStorage -------------------------------------------------------

(defonce ^:private storage
  (when (exists? js/localStorage) js/localStorage))

(defn ls-read [k]
  (when storage (.getItem storage k)))

(defn- gen-uuid []
  (if (and (exists? js/crypto) (.-randomUUID js/crypto))
    (.randomUUID js/crypto)
    (str (js/Date.now) "-" (rand-int 1000000))))

(defn ls-write [k v]
  (when storage (.setItem storage k v)))

(defn device-id []
  (or (ls-read "mtm.device")
      (let [id (gen-uuid)]
        (ls-write "mtm.device" id)
        id)))

(defn best-key [mode] (str "mtm.best." (name mode)))

(defn personal-best [mode]
  (some-> (ls-read (best-key mode)) reader/read-string))

(defn save-personal-best! [mode score]
  (let [best (or (personal-best mode) 0)]
    (when (> score best)
      (ls-write (best-key mode) (str score))
      score)))

(defn- detect-lang []
  (if (and (exists? js/navigator)
           (some? (re-find #"^tr" (str (.-language js/navigator)))))
    :tr :en))

(defn load-settings! []
  (when-let [raw (ls-read "mtm.settings")]
    (try
      (swap! settings merge (reader/read-string raw))
      (catch :default _ nil)))
  (when-not (:lang @settings)
    (swap! settings assoc :lang (detect-lang)))
  (ls-write "mtm.settings" (pr-str @settings)))

(defn save-settings! []
  (ls-write "mtm.settings" (pr-str @settings)))

(defn set-setting! [k v]
  (swap! settings assoc k v)
  (save-settings!))
