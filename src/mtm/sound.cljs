(ns mtm.sound
  "All sound effects are synthesised with the Web Audio API - no asset
  files needed. The context is created lazily on the first user gesture
  (browsers refuse to start audio before one)."
  (:require [mtm.db :as db]))

(defonce ^:private ctx (atom nil))
(defonce ^:private master (atom nil))

(defn- audio-ctx []
  (when-let [C (or (.-AudioContext js/window)
                   (.-webkitAudioContext js/window))]
    (or @ctx
        (let [c (C.)]
          (reset! ctx c)
          (reset! master (let [g (.createGain c)]
                           (set! (.-value g) 0.5)
                           (.connect g (.-destination c))
                           g))
          c))))

(defn unlock!
  "Called from a click/keypress handler: resumes the audio context."
  []
  (when-let [c (audio-ctx)]
    (when (= "suspended" (.-state c))
      (.resume c))))

(defn- tone
  "Play one oscillator note.

  opts:
    :type   waveform, default \"sine\"
    :delay  seconds to wait before the note starts
    :dur    seconds the note lasts
    :gain   peak amplitude
    :to     end frequency for a glide (from = freq)"
  [freq {:keys [type delay dur gain to curve]
         :or {type "sine" delay 0 dur 0.15 gain 0.18}}]
  (when-let [c (audio-ctx)]
    (let [start  (+ (.-currentTime c) delay)
          osc    (.createOscillator c)
          env    (.createGain c)]
      (set! (.-type osc) type)
      (set! (.-value (.-frequency osc)) freq)
      (when to
        (.exponentialRampToValueAtTime (.-value (.-frequency osc)) (max 1 to) (+ start dur)))
      (when curve
        (.setValueAtTime (.-value (.-frequency osc)) freq start)
        (.linearRampToValueAtTime (.-value (.-frequency osc)) curve (+ start dur)))
      ;; volume envelope: instant attack, exponential release
      (set! (.-value (.-gain env)) 0.0001)
      (.exponentialRampToValueAtTime (.-gain env) gain (+ start 0.012))
      (.exponentialRampToValueAtTime (.-gain env) 0.0001 (+ start dur))
      (.connect osc env)
      (.connect env @master)
      (.start osc start)
      (.stop osc (+ start dur 0.05)))))

(defn- noise-burst [delay dur gain]
  (when-let [c (audio-ctx)]
    (let [start  (+ (.-currentTime c) delay)
          frames (max 1 (js/Math.floor (* 44100 dur)))
          buf    (.createBuffer c 1 frames 44100)
          data   (.getChannelData buf 0)]
      (dotimes [i frames]
        (aset data i (- (* 2 (.random js/Math)) 1)))
      (let [src  (.createBufferSource c)
            env  (.createGain c)
            filt (.createBiquadFilter c)]
        (set! (.-type filt) "lowpass")
        (set! (.-value (.-frequency filt)) 900)
        (set! (.-value (.-gain env)) 0.0001)
        (.exponentialRampToValueAtTime (.-gain env) gain (+ start 0.01))
        (.exponentialRampToValueAtTime (.-gain env) 0.0001 (+ start dur))
        (set! (.-buffer src) buf)
        (.connect src filt)
        (.connect filt env)
        (.connect env @master)
        (.start src start)
        (.stop src (+ start dur 0.02))))))

(def ^:private effects
  {:click     #(tone 620 {:type "square" :dur 0.05 :gain 0.06})
   :select    #(tone 440 {:type "square" :dur 0.07 :gain 0.08})
   :correct   (fn []
               (tone 660 {:type "sine" :dur 0.16 :gain 0.2})
               (tone 990 {:type "sine" :delay 0.09 :dur 0.22 :gain 0.18}))
   :combo     (fn []
               (tone 740 {:type "triangle" :dur 0.1 :gain 0.16})
               (tone 1180 {:type "triangle" :delay 0.06 :dur 0.16 :gain 0.14}))
   :wrong     (fn []
               (tone 190 {:type "sawtooth" :dur 0.28 :gain 0.16 :to 90})
               (noise-burst 0 0.12 0.08))
   :timeout   (fn []
               (tone 320 {:type "sine" :dur 0.2 :gain 0.14 :to 160})
               (tone 160 {:type "sine" :delay 0.14 :dur 0.26 :gain 0.12}))
   :tick      #(tone 1400 {:type "square" :dur 0.045 :gain 0.05})
   :start     (fn []
               (tone 392 {:type "triangle" :dur 0.14 :gain 0.16})
               (tone 587 {:type "triangle" :delay 0.1 :dur 0.2 :gain 0.16}))
   :gameover  (fn []
               (doseq [[i f] (map-indexed vector [523 659 784 1046])]
                 (tone f {:type "triangle" :delay (* i 0.13) :dur 0.3 :gain 0.16}))
               (noise-burst 0.5 0.4 0.06))
   :leaderboard (fn []
                  (doseq [[i f] (map-indexed vector [523 659 784])]
                    (tone f {:type "sine" :delay (* i 0.08) :dur 0.2 :gain 0.12})))})

(defn play!
  "Play a named effect unless the player muted the sound."
  [k]
  (when (and (:sound @db/settings)
             (not= k :unlock))
    (when-let [f (get effects k)]
      (try (f) (catch :default _ nil)))))

(defn toggle-sound! []
  (let [on? (not (:sound @db/settings))]
    (db/set-setting! :sound on?)
    (when on?
      (unlock!)
      (play! :click))
    on?))
