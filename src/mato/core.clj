(ns mato.core)

(require '[lanterna.screen :as s])

(defn create-coord [x y] {:x x :y y})

(def original-worm
  [(create-coord 0 5) (create-coord 1 5) (create-coord 2 5)]
  )

(def right {:x 1 :y 0})
(def left {:x -1 :y 0})
(def up {:x 0 :y -1})
(def down {:x 0 :y 1})

(defn change-coord [coord movement]
  (hash-map
    :x (+ (:x coord) (get movement :x 0))
    :y (+ (:y coord) (get movement :y 0))))

(comment
  (change-coord (create-coord 1 1) right)
  )

(defn move-v2
  ([worm movement grows?]
   (if (nil? movement)
     worm
     (if grows?
       (cons (change-coord (first worm) movement) worm)
       (->> worm
            (cons (change-coord (first worm) movement))
            (drop-last)
            (vec)
            )
       )
     )
   )
  ([worm movement]
   (move-v2 worm movement false))
  )

(comment
  (move-v2 [(create-coord 1 1)] right true)
  )

(def scene-width 20)
(def scene-height 10)

(defn collision? [worm]
  (let [head (first worm)
        x (get head :x 0)
        y (get head :y 0)]
    (or (neg? x) (neg? y) (>= x scene-width) (>= y scene-height))))

(def piece-of-worm "X")
(def piece-of-background "O")
(def piece-of-goodies "g")

(defn has-coords-in-it? [coord collection]
  (some #(= % coord) collection))

(comment
  (has-coords-in-it? (create-coord 1 2) nil)
  (has-coords-in-it? (create-coord 1 2) (list (create-coord 2 2) (create-coord 1 2))))

(defn print-scene-v2 [worm goodies]
  (doseq [y (range scene-height)]
    (dotimes [x scene-width]
      (let [this-place (create-coord x y)]
        (print
          (cond
            (has-coords-in-it? this-place worm) piece-of-worm
            (has-coords-in-it? this-place goodies) piece-of-goodies
            :else piece-of-background)))
      (inc x))
    (newline)))

(defn will-eat? [worm goodies moves]
  (let [head (first worm)
        next-step (first moves)
        eating-at (change-coord head next-step)]
    (has-coords-in-it? eating-at goodies)))

(comment
  (will-eat? [(create-coord 1 1) (create-coord 2 1)] [(create-coord 0 1)] [left])
  (will-eat? [(create-coord 1 1) (create-coord 2 1)] [(create-coord 0 1)] [up])
  )

(defn remove-element-if [condition goodie collection]
  (if condition
    (remove #(= % goodie) collection)
    collection)
  )

(comment
  (remove-element-if true 2 [1 2 3])
  (remove-element-if false 2 [1 2 3])
  )

(defn next-move-v4 [worm goodies moves]
  (loop [current-worm worm
         goodies-still-left goodies
         moves-still-left moves]
    (if (empty? moves-still-left)
      (if (collision? current-worm)
        (println "oops")
        (do
          (println "ending")
          (print-scene-v2 current-worm goodies-still-left))) ; scene after last move
      (do
        (println "playing")
        (print-scene-v2 current-worm goodies-still-left)
        (let [next-movement (first moves-still-left)
              next-moves (rest moves-still-left)
              next-movement-coord (change-coord next-movement (first current-worm))
              next-movement-will-eat (will-eat? current-worm goodies-still-left next-moves)]
          (recur
            (move-v2 current-worm next-movement next-movement-will-eat)
            (remove-element-if next-movement-will-eat next-movement-coord goodies-still-left)
            next-moves))))))

(comment
  original-worm
  (next-move-v4 original-worm [(create-coord 0 6)] (seq [down down right right right]))
  (next-move-v4 original-worm [(create-coord 0 6) (create-coord 0 0) (create-coord 10 3)] (seq [down down right right right up]))
  (next-move-v4 original-worm () (seq [down down right right right]))
  (next-move-v4 original-worm () (seq [left left]))
  )


(defn run-screen []
  (let [screen (s/get-screen :swing)]
    (s/start screen)

    (s/put-string screen 10 10 "Hello, world!")
    (s/put-string screen 10 11 "Press q key to exit!")
    (s/redraw screen)
    (loop []
      (let [input-key (s/get-key-blocking screen)]
        (when-not (= \q input-key)
          (recur))))

    (s/stop screen)
    )
  )

(comment
  (run-screen)
  )