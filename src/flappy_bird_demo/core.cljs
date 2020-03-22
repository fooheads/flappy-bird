(ns flappy-bird-demo.core
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn floor [x] (.floor js/Math x))

(defn translate [start-pos vel time]
  (floor (+ start-pos (* time vel))))


(def starting-state { :timer-running false
                      :jump-count 0

                      :jump-vel 21

                      :horiz-vel -0.15
                      :gravity 0.05

                      :start-time 0
                      :start-x 212
                      :start-y 312

                      :flappy-start-time 0

                      :flappy-x 212
                      :flappy-y 312
                      :flappy-width 57  
                      :flappy-height 34

                      :pillar-width 86
                      :pillar-gap 158
                      :pillar-spacing 324

                      :bottom-y 561

                      :pillar-list
                      [{ :start-time 0
                         :pos-x 900
                         :cur-x 900
                         :gap-top 200}]})

(defn reset-state [_ cur-time]
  (-> starting-state
      (update-in [:pillar-list] (fn [pls] (map #(assoc % :start-time cur-time) pls)))
      (assoc
          :start-time cur-time
          :flappy-start-time cur-time
          :timer-running true)))

(def flap-state (atom starting-state))

(defn curr-pillar-pos [horiz-vel cur-time {:keys [pos-x start-time]}]
  (translate pos-x horiz-vel (- cur-time start-time)))

(defn in-pillar? [{:keys [cur-x pillar-width]} flappy-width flappy-x]
  (and (>= (+ flappy-x flappy-width)
           cur-x)
       (< flappy-x (+ cur-x pillar-width))))

(defn in-pillar-gap? [{:keys [flappy-y flappy-height pillar-gap]} {:keys [gap-top]}]
  (and (< gap-top flappy-y)
       (> (+ gap-top pillar-gap)
          (+ flappy-y flappy-height))))

(defn bottom-collision? [{:keys [bottom-y flappy-y flappy-height]}]
  (>= flappy-y (- bottom-y flappy-height)))

(defn pillar-collision? [st pillar]
  (and (in-pillar? pillar (:flappy-width st) (:flappy-x st))
       (not (in-pillar-gap? st pillar))))

(defn collision? [{:keys [pillar-list] :as st}]
  (or
    (bottom-collision? st)
    (some (partial pillar-collision? st) pillar-list)))

(defn detect-collision [st]
  (if (collision? st)
    (assoc st :timer-running false)
    st))


(defn new-pillar [cur-time bottom-y pos-x pillar-gap]
  {:start-time cur-time
   :pos-x      pos-x
   :cur-x      pos-x
   :gap-top    (+ 60 (rand-int (- bottom-y 120 pillar-gap)))})

(defn update-pillars [{:keys [bottom-y horiz-vel pillar-list pillar-width pillar-gap pillar-spacing cur-time] :as st}]
  (let [pillars-with-pos (map #(assoc % :cur-x (curr-pillar-pos horiz-vel cur-time %)) pillar-list)
        pillars-in-world (sort-by
                          :cur-x
                          (filter #(> (:cur-x %) (- pillar-width)) pillars-with-pos))]
    (assoc st
      :pillar-list
      (if (< (count pillars-in-world) 3)
        (conj pillars-in-world
              (new-pillar
               cur-time
               bottom-y
               (+ pillar-spacing
                  (:cur-x (last pillars-in-world)))
               pillar-gap))
        pillars-in-world))))

(defn sine-wave [v amplitude length time-delta]
  (+ v (* amplitude (.sin js/Math (/ time-delta length)))))

(defn update-flappy [{:keys [gravity time-delta start-y jump-vel bottom-y flappy-y flappy-height jump-count] :as st}]
  (assoc st :flappy-y
    (if (pos? jump-count)
      (let [cur-vel (- jump-vel (* time-delta gravity))
            new-y   (- flappy-y cur-vel)
            new-y   (if (> new-y (- bottom-y flappy-height))
                      (- bottom-y flappy-height)
                      new-y)]
        new-y)
      (sine-wave start-y 30 300 time-delta))))

(defn update-score [{:keys [cur-time horiz-vel start-time pillar-spacing] :as st}]
  (let [score (- (.abs js/Math (floor (/ (- (* (- cur-time start-time) horiz-vel) 544)
                                       pillar-spacing)))
                 4)]
   (assoc st :score (if (neg? score) 0 score))))

(defn update-time [state timestamp]
  (assoc state
    :cur-time timestamp
    :time-delta (- timestamp (:flappy-start-time state))))

(defn advance-game-state [timestamp state]
  (-> state
      (update-time timestamp)
      update-flappy
      update-pillars
      detect-collision
      update-score))

(defn jump [{:keys [cur-time jump-count] :as state}]
  (-> state
      (assoc
          :jump-count (inc jump-count)
          :flappy-start-time cur-time)))

;; derivatives

(defn border [{:keys [horiz-vel cur-time] :as state}]
  (-> state
      (assoc :border-pos (mod (translate 0 horiz-vel cur-time) 23))))

(defn pillar-offset [bottom-y pillar-gap {:keys [gap-top] :as p}]
  (assoc p
    :upper-height gap-top
    :lower-height (- bottom-y gap-top pillar-gap)))

(defn pillar-offsets [state]
  (update-in state [:pillar-list]
             (fn [pillar-list]
               (map (partial pillar-offset (:bottom-y state) (:pillar-gap state)) pillar-list))))

(defn world [state]
  (-> state
      border
      pillar-offsets))

(defn px [n] (str n "px"))

(defn pillar [{:keys [cur-x pos-x upper-height lower-height]}]
  [:div.pillars
   [:div.pillar.pillar-upper {:style {:left (px cur-x)
                                       :height upper-height}}]
   [:div.pillar.pillar-lower {:style {:left (px cur-x)
                                       :height lower-height}}]])

(defn time-loop [time]
  (let [new-state (swap! flap-state (partial advance-game-state time))]
    (when (:timer-running new-state)
      (go
       (<! (timeout 30))
       (.requestAnimationFrame js/window time-loop)))))

(defn start-game []
  (.requestAnimationFrame
   js/window
   (fn [time]
     (reset! flap-state (reset-state @flap-state time))
     (time-loop time))))

(defn main-template [{:keys [score cur-time jump-count
                             timer-running border-pos
                             flappy-x flappy-y pillar-list]}]
  (sab/html [:div.board { :onMouseDown (fn [e]
                                         (swap! flap-state jump)
                                         (.preventDefault e))}
             [:h1.score score]
             (if-not timer-running
               [:a.start-button {:onClick #(start-game)}
                (if (< 1 jump-count) "RESTART" "START")]
               [:span])
             [:div (map pillar pillar-list)]
             [:div.flappy {:style {:top (px flappy-y) :left (px flappy-x)}}]
             [:div.scrolling-border {:style { :background-position-x (px border-pos)}}]]))

(let [node (.getElementById js/document "board-area")]
  (defn renderer [full-state]
    (.render js/ReactDOM (main-template full-state) node)))

(add-watch flap-state :renderer (fn [_ _ _ n]
                                  (renderer (world n))))

(reset! flap-state @flap-state)
