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
    :x (+ (:x coord) (:x movement))
    :y (+ (:y coord) (:y movement))))

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

(defn collision? [mato]
  (let [head (first mato)
        x (:x head)
        y (:y head)]
    (or (neg? x) (neg? y))))

(defn print-scene [mato]
  (doseq [y (range 10)]
    (dotimes [x 20]
      (if (some #(= % (hash-map :x x :y y)) mato)
        (print "X")
        (print "O")
        )
      (inc x))
    (newline)))

(defn next-move-v3 [worm moves]
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
        (recur (move-v2 mato (first moves-still-left)) (rest moves-still-left))))))

(comment
  (next-move-v3 original-mato (seq [down down right right right]))
  (next-move-v3 original-mato (seq [left left]))
  )
