(ns mato.core
  (:gen-class)
  (:require [lanterna.screen :as s])
  (:require [clojure.core.async :as async])
  )

(defn create-coord [x y] {:x x :y y})

(def right {:x 1 :y 0})
(def left {:x -1 :y 0})
(def up {:x 0 :y -1})
(def down {:x 0 :y 1})

(defn change-coord [coord movement]
  (hash-map
    :x (+ (:x coord) (get movement :x 0))
    :y (+ (:y coord) (get movement :y 0))))

(defn move-worm
  ([worm movement grows?]
   (if (nil? movement)
     worm
     (if grows?
       (cons (change-coord (first worm) movement) worm)
       (->> worm
            (cons (change-coord (first worm) movement))
            (drop-last)
            (vec)))))
  ([worm movement]
   (move-worm worm movement false)))

(def scene-width 20)
(def scene-height 10)

(defn collision? [worm]
  (let [head (first worm)
        x (get head :x 0)
        y (get head :y 0)]
    (or (neg? x) (neg? y) (>= x scene-width) (>= y scene-height))))

(def piece-of-worm "X")
(def piece-of-background ".")
(def piece-of-goodies "g")

(defn has-coords-in-it? [coord collection]
  (some #(= % coord) collection))

(defn print-scene [print-f redraw-f worm goodies]
  (doseq [y (range scene-height)]
    (dotimes [x scene-width]
      (let [this-place (create-coord x y)]
        (print-f x y
                 (cond
                   (has-coords-in-it? this-place worm) piece-of-worm
                   (has-coords-in-it? this-place goodies) piece-of-goodies
                   :else piece-of-background)))
      (inc x)))
  (redraw-f))

(defn will-eat? [worm goodies movement]
  (let [head (first worm)
        eating-at (change-coord head movement)]
    (has-coords-in-it? eating-at goodies)))

(defn remove-item-if [condition item collection]
  (if condition
    (remove #(= % item) collection)
    collection))

(def game-over-coordinates (create-coord 10 10))
(def well-done-coordinates (create-coord 10 10))

(def game-over-message "GAME OVER")
(def well-done-message "WELL DONE")

; fn engine-step -> [worm goodies]
;                -> [:game-over nil]
;                -> [:winner nil]
(defn engine-step [next-movement worm goodies]
  (let [next-movement-coord (change-coord next-movement (first worm))
        next-movement-will-eat (will-eat? worm goodies next-movement)]
    (if (collision? worm)
      [:game-over nil]
      (if (empty? goodies) 
        [:winner nil]
        [(move-worm worm next-movement next-movement-will-eat)
         (remove-item-if next-movement-will-eat next-movement-coord goodies)]))))

; refactor? 
; print-f and redraw-f functions could be replaced with a channel.
(defn run-engine [moves-channel print-f redraw-f worm goodies]
  (async/go-loop
   [current-worm worm
    goodies-still-left goodies]
    (print-scene print-f redraw-f current-worm goodies-still-left)
    (when-let [next-movement (async/<! moves-channel)]
      (let [[new-worm new-goodies] (engine-step next-movement current-worm goodies-still-left)]
        (cond
          (= :winner new-worm) 
          (do
            (print-f (:x well-done-coordinates) (:y well-done-coordinates) well-done-message)
            (redraw-f))
          (= :game-over new-worm)
          (do
            (print-f (:x game-over-coordinates) (:y game-over-coordinates) game-over-message)
            (redraw-f))
          :else 
          (recur new-worm new-goodies))))))

(def quit-key \q)
(def left-key \h)
(def right-key \l)
(def up-key \k)
(def down-key \j)

(defn start-screen [screen]
  (let [help (fn [scr] (s/put-string scr 10 20 (str "Use " left-key ", " right-key ", " up-key ", " down-key " to turn. Press " quit-key " key to exit!")))]
    (s/start screen)
    (help screen)
    (s/redraw screen)))

(defn stop-screen [screen]
  (s/stop screen))

(defn pull-input [screen out-channel]
  (let [key-lookup (hash-map left-key left right-key right up-key up down-key down)]
    (loop [previous-movement right]
      (let [input-key (s/get-key-blocking screen {:timeout 350})]
        (when-not (= quit-key input-key)
          (when-let [movement (get key-lookup input-key previous-movement)]
            (async/put! out-channel movement)
            (recur movement)))))
    (async/close! out-channel)))

(defn bootstrap-with-run-engine []
  (let [screen (s/get-screen :swing)
        channel (async/chan 1)
        worm (mapv create-coord [4 3 2] [3 3 3])
        goodies (mapv create-coord [0 0 10] [6 0 3])
        put-string-f (partial s/put-string screen)
        redraw-f (partial s/redraw screen)]
    (start-screen screen)
    (print-scene put-string-f redraw-f worm goodies)
    (run-engine channel put-string-f redraw-f worm goodies)
    (pull-input screen channel)
    (stop-screen screen)))

(comment
  (bootstrap-with-run-engine)
  )

(defn -main [] (bootstrap-with-run-engine))

