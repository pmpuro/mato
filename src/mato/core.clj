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
(def piece-of-goodies "g")

(defn has-coords-in-it? [coord collection]
  (some #(= % coord) collection))

(comment
  (has-coords-in-it? (create-coord 1 2) nil)
  (has-coords-in-it? (create-coord 1 2) (list (create-coord 2 2) (create-coord 1 2))))

(defn print-scene-v2 [mato goodies]
  (doseq [y (range 10)]
    (dotimes [x 20]
      (print
        (cond
          (has-coords-in-it? (create-coord x y) mato) piece-of-worm
          (has-coords-in-it? (create-coord x y) goodies) piece-of-goodies
          :else piece-of-background
          )
        )
      (inc x))
    (newline)))

(defn will-eat [worm goodies moves]
  (let [head (first worm)
        next-step (first moves)
        eating-at (change-coord head next-step)]
    (has-coords-in-it? eating-at goodies)))

(comment
  (will-eat [(create-coord 1 1) (create-coord 2 1)] [(create-coord 0 1)] [left])
  (will-eat [(create-coord 1 1) (create-coord 2 1)] [(create-coord 0 1)] [up])
  )

(defn drop-first-if [condition collection]
  (if condition
    (rest collection)
    collection)
  )

(comment
  (drop-first-if true [1 2 3])
  (drop-first-if false [1 2 3])
  )

(defn next-move-v4 [worm goodies moves]
  (loop [mato worm
         goodies-still-left goodies
         moves-still-left moves]
    (if (empty? moves-still-left)
      (if (collision? mato)
        (println "oops")
        (do
          (println "ending")
          (print-scene-v2 mato goodies-still-left)))        ; scene after last move
      (do
        (println "playing")
        (print-scene-v2 mato goodies-still-left)
        (let [next-movement (first moves-still-left)
              next-moves (rest moves-still-left)]
          (recur
            (move-v2 mato next-movement (will-eat mato goodies-still-left next-moves))
            (drop-first-if (will-eat mato goodies-still-left next-moves) goodies-still-left)
            next-moves))))))

(comment
  original-mato
  (next-move-v4 original-mato [(create-coord 0 6)] (seq [down down right right right]))
  (next-move-v4 original-mato () (seq [down down right right right]))
  (next-move-v4 original-mato () (seq [left left]))
  )
