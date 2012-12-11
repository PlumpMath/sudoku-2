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

(defn all-true?
  [f puzzle]
  (reduce #(and %1 %2) (map all-unique? (f puzzle))))

(defn is-solved?
  "Takes a solved puzzle and returns true if the puzzle is solved"
  [puzzle]
  (and
   (all-true? get-rows puzzle)
   (all-true? get-columns puzzle)
   (all-true? get-sectors puzzle)))

(defn get-indexes-of-blanks
  [puzzle]
  (map first 
       (filter 
        #(= :_ (second %)) 
	   (map-indexed vector puzzle))))

(defn possible-solutions
  "Takes an unsolved puzzle and returns all possible solutions"
  [puzzle]
  (for [index (get-indexes-of-blanks puzzle)
        candidate (range 1 (+ 1 (get-width puzzle)))]
    (assoc puzzle index candidate)))

(defn solve 
  "Takes an unsolved puzzle and returns a solved one"
  [puzzle]
  (doseq [solution (filter is-solved? (possible-solutions puzzle))]
    solution))

(def unsolved [ 1  2  3  4
                4  3  2  1
                2  1  4  3
                3  4  1 :_])





