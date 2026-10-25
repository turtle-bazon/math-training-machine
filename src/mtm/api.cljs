(ns mtm.api
  "Thin fetch wrapper around the Common Lisp backend."
  (:require [clojure.string :as str]))

(def base
  "Same origin when the Lisp server serves the build; override for a
  separate dev server with window.MTM_API_BASE."
  (or (some-> js/window .-MTM_API_BASE) ""))

(defn- parse-json [res]
  (-> (.json res)
      (.then (fn [data] (js->clj data :keywordize-keys true)))))

(defn leaderboard
  "GET /api/leaderboard?mode=sum -> {:entries [...]}"
  [mode limit cb]
  (let [url (str base "/api/leaderboard?mode=" (name mode) "&limit=" limit)]
    (-> (.fetch js/window url)
        (.then (fn [res]
                 (if (.-ok res)
                   (parse-json res)
                   (throw (js/Error. (str "HTTP " (.-status res)))))))
        (.then (fn [data] (cb {:ok true :data data})))
        (.catch (fn [e] (cb {:ok false :error (str e)}))))))

(defn submit
  "POST /api/leaderboard with a finished round payload."
  [payload cb]
  (-> (.fetch js/window
              (str base "/api/leaderboard")
              #js{:method "POST"
                  :headers #js{"content-type" "application/json"}
                  :body (js/JSON.stringify (clj->js payload))})
      (.then (fn [res]
               (if (.-ok res)
                 (parse-json res)
                 (throw (js/Error. (str "HTTP " (.-status res)))))))
      (.then (fn [data] (cb {:ok true :data data})))
      (.catch (fn [e] (cb {:ok false :error (str e)})))))

(defn trim-name [s]
  (let [t (str/trim (or s ""))]
    (subs t 0 (min 16 (count t)))))
