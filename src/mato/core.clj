(ns mato.core)
(require '[lanterna.screen :as s])
(require '[clojure.core.async :as async])

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

(defn remove-element-if [condition goodie collection]
  (if condition
    (remove #(= % goodie) collection)
    collection))

(comment
  (remove-element-if true 2 [1 2 3])
  (remove-element-if false 2 [1 2 3]))


(defn engine [moves-channel print-f redraw-f worm goodies]
  (async/go-loop [current-worm worm
                  goodies-still-left goodies]
    (print-scene print-f redraw-f current-worm goodies-still-left)
    (when-let [next-movement (async/<! moves-channel)]
      (if (collision? current-worm)
        (do
          (print-f 10 10 "GAME OVER")
          (redraw-f))
        (let [next-movement-coord (change-coord next-movement (first current-worm))
              next-movement-will-eat (will-eat? current-worm goodies-still-left next-movement)]
          (recur
            (move-worm current-worm next-movement next-movement-will-eat)
            (remove-element-if next-movement-will-eat next-movement-coord goodies-still-left)))))))

(defn start-screen [screen]
  (s/start screen)

  (s/put-string screen 10 21 "Hello, world!")
  (s/put-string screen 30 21 "Press q key to exit!")
  (s/redraw screen))

(defn stop-screen [screen]
  (s/stop screen))

(defn pull-input [screen out-channel]
  (let [key-lookup (hash-map \h left \l right \k up \j down)]
    (loop []
      (let [input-key (s/get-key-blocking screen)]
        (when-not (= \q input-key)
          (println (str "got " input-key))
          (when-let [movement (get key-lookup input-key nil)]
            (println movement)
            (async/put! out-channel movement)
            (recur)))))
    (async/close! out-channel)))

(defn bootstrap []
  (let [screen (s/get-screen :swing)
        channel (async/chan 1)
        worm [(create-coord 4 3) (create-coord 3 3) (create-coord 2 3)]
        goodies [(create-coord 0 6) (create-coord 0 0) (create-coord 10 3)]
        put-string-f (partial s/put-string screen)
        redraw-f (partial s/redraw screen)]
    (start-screen screen)
    (print-scene put-string-f redraw-f worm goodies)
    (engine channel put-string-f redraw-f worm goodies)
    (pull-input screen channel)
    (stop-screen screen)))

(comment
  (bootstrap))

