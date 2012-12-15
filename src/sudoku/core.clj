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

(defn solve 
  "Takes an unsolved puzzle and returns a solved one"
  [puzzle]
  (filter is-solved? (possible-solutions puzzle))

(def unsolved [:_ :_  3  4
               4  3  2  1
               1  2  4 :_
               3  4  1 :_])

;(solved? unsolved)
