(ns sudoku.core)

(def puzzle1 [1 2 3 4
              3 4 1 2
              4 1 2 3
              2 3 4 1])

(defn all-unique? [row]
  (= (count (set row))
     (count row)))

(defn is-solved?
  "Takes a solved puzzle and returns true if the puzzle is solved"
  [puzzle]
  false)

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

(defn solve 
  "Takes an unsolved puzzle and returns a solved one"
  [])