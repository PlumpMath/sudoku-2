(ns sudoku.core)

(defn get-width [puzzle]
  (Math/sqrt (count puzzle)))

(defn get-rows
  [puzzle]
  (let [width (get-width puzzle)]
    (for [start (range 0 (* width width) width)]
      (for [i (range start (+ start width))]
        (nth puzzle i)))))

(defn get-columns
  [puzzle]
  (let [width (get-width puzzle)]
    (for [start (range 0 width)]
      (for [i (range start (* width width) width)]
        (nth puzzle i)))))

(defn get-sector-indexes
  [puzzle]
  (let [puzzle-width (get-width puzzle)
        sector-width (int (Math/sqrt puzzle-width))]
    (for [sector-x (range 0 sector-width)
          sector-y (range 0 sector-width)]
      (for [inside-x (range 0 sector-width)
            inside-y (range 0 sector-width)]
        (let [x (+ inside-x (* sector-x sector-width))
              y (+ inside-y (* sector-y sector-width))
              w puzzle-width]
          (+ x (* y w)))))))

(defn get-sectors
  [puzzle]
  (for [coordinates (get-sector-indexes puzzle)]
    (for [coord coordinates]
      (nth puzzle coord))))

(defn all-unique? [row]
  (= (count (set row))
     (count row)))

(defn is-solved?
  "Takes a solved puzzle and returns true if the puzzle is solved"
  [puzzle]
  (every? true? 
          (map all-unique? 
               (concat (get-rows puzzle)
                       (get-columns puzzle)
                       (get-sectors puzzle)))))

(defn get-indexes-of-blanks
  [puzzle]
  (map first 
       (filter 
        #(= :_ (second %)) 
        (map-indexed vector puzzle))))

(defn all-combinations [options length]
  ((fn f [result options length]
     (if (>= (count result) length)
       result
       (for [x options]
         (f (conj result x) options length))))
   [] options length))

(defn guesses [nr-of-blanks options]
  (partition nr-of-blanks (flatten (all-combinations options nr-of-blanks))))

(defn possible-solutions
  "Takes an unsolved puzzle and returns all possible solutions"
  [puzzle]
  (let [options (range 1 (+ 1 (get-width puzzle)))
        blanks (get-indexes-of-blanks puzzle)
        guess-combinations (guesses (count blanks) options)]
    (for [guess-vec guess-combinations]
      (let [z (interleave blanks guess-vec)]
        (apply assoc puzzle z)))))

;; Alternative version:
(defn combination [nr nr-of-slots nr-of-options]
  (loop [slot 0
         n nr
         result []]
    (if (>= slot nr-of-slots)
      result
      (let [nr-to-put-in-slot (min n nr-of-options)]
        (recur (inc slot)
               (- n nr-to-put-in-slot)
               (conj result nr-to-put-in-slot))))))

(defn possible-solutions-using-combination-fn
  "Takes an unsolved puzzle and returns all possible solutions"
  [puzzle]
  (let [options (range 1 (+ 1 (get-width puzzle)))
        blanks (get-indexes-of-blanks puzzle)
        blank-count (count blanks)
        option-count (count options)
        combo-count (Math/pow option-count blank-count)]
    (println "combo-count: " combo-count)
    (for [x (range combo-count)]
      (let [z (interleave blanks (combination x blank-count option-count))]
        (apply assoc puzzle z)))))



(defn solve 
  "Takes an unsolved puzzle and returns a solved one"
  [puzzle]
  (filter is-solved? (possible-solutions puzzle)))

(def unsolved [:_ :_  3  4
               4  3  2  1
               1  2  4 :_
               3  4  1 :_])

(def unsolved-hard [:_ :_ :_ :_  2 :_ :_ :_  9
                    :_ :_  3 :_  5  9  7 :_  8
                    :_  7 :_ :_  1 :_  3 :_  4
                    :_  5 :_ :_  8  2  1 :_ :_
                     7  2  8 :_ :_ :_  4  3  6
                    :_ :_  6  3  7 :_ :_  8 :_
                     6 :_  2 :_  4 :_ :_  7 :_
                     8 :_  9  5  3 :_  2 :_ :_
                     5 :_ :_ :_  6 :_ :_ :_ :_])

;(solved? unsolved)
