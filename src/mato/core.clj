(ns mato.core)

(defn create-coord [x y] {:x x :y y})

(def original-mato
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
  ([mato movement grows?]
   (if (nil? movement)
     mato
     (if grows?
       (cons (change-coord (first mato) movement) mato)
       (->> mato
            (cons (change-coord (first mato) movement))
            (drop-last)
            (vec)
            )
       )
     )
   )
  ([mato movement]
   (move-v2 mato movement false))
  )

(comment
  (move-v2 [(create-coord 1 1)] right true)
  )

(defn collision? [mato]
  (let [head (first mato)
        x (get head :x 0)
        y (get head :y 0)]
    (or (neg? x) (neg? y))))

(def piece-of-worm "X")
(def piece-of-background "O")

(defn print-scene [mato]
  (doseq [y (range 10)]
    (dotimes [x 20]
      (if (some #(= % (hash-map :x x :y y)) mato)
        (print piece-of-worm)
        (print piece-of-background)
        )
      (inc x))
    (newline)))

(defn has-coords-in-it? [coord collection]
  (some #(= % coord) collection))

(comment
  (has-coords-in-it? (create-coord 1 2) nil)
  (has-coords-in-it? (create-coord 1 2) (list (create-coord 2 2) (create-coord 1 2))))

(defn will-eat [worm goodies moves]
  (let [head (first worm)
        next-step (first moves)
        eating-at (change-coord head next-step)]
    (has-coords-in-it? eating-at goodies)))

(comment
  (will-eat [(create-coord 1 1) (create-coord 2 1)] [(create-coord 0 1)] [left])
  (will-eat [(create-coord 1 1) (create-coord 2 1)] [(create-coord 0 1)] [up])
  )

(defn next-move-v3 [worm goodies moves]
  (loop [mato worm
         moves-still-left moves]
    (if (empty? moves-still-left)
      (if (collision? mato)
        (println "oops")
        (do
          (println "ending")
          (print-scene mato)))                              ; scene after last move
      (do
        (println "playing")
        (print-scene mato)
        (recur
          (move-v2 mato (first moves-still-left) (will-eat mato goodies (rest moves-still-left)))
          (rest moves-still-left))))))

(comment
  original-mato
  (next-move-v3 original-mato [(create-coord 0 6)] (seq [down down right right right]))
  (next-move-v3 original-mato () (seq [down down right right right]))
  (next-move-v3 original-mato () (seq [left left]))
  )
