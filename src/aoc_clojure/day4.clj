(ns aoc-clojure.day4
  (:require
   [clojure.java.io]
   [clojure.string :as string]))

(defn load-line [line]
  (vec
   (map
    (fn [chr] (= chr \@))
    line)))

(defn load-grid [lines]
  (->> lines
       (map string/trim)
       (filter (complement empty?))
       (map load-line)
       vec))

(defn roll-present [grid row col]
  (and
   (>= row 0)
   (>= col 0)
   (< row (count grid))
   (< col (count (first grid)))
   (-> grid (get row) (get col))))

(defn roll-accessible [grid row col]
  (and
   (roll-present grid row col)
   (< (count (filter
              (fn [row-col]
                (roll-present grid (first row-col) (second row-col)))
              [[(dec row) (dec col)]
               [(dec row) col]
               [(dec row) (inc col)]
               [row (dec col)]
               [row (inc col)]
               [(inc row) (dec col)]
               [(inc row) col]
               [(inc row) (inc col)]]))
      4)))

(defn remove-accessible-rolls [grid]
  (->> grid
       (map-indexed
        (fn [row-idx row]
          (->> row
               (map-indexed
                (fn [col-idx cell]
                  (and
                   cell
                   (not (roll-accessible grid row-idx col-idx)))))
               vec)))
       vec))

(defn count-rolls [grid]
  (reduce
   (fn [memo row]
     (+ memo
        (reduce
         (fn [memo2 col]
           (+ memo2 (if col 1 0)))
         0
         row)))
   0
   grid))

(defn remove-accessible-rolls-iterated [grid]
  (let [reduced-grid (remove-accessible-rolls grid)]
    (if
     (= (count-rolls grid) (count-rolls reduced-grid))
      grid
      (remove-accessible-rolls-iterated reduced-grid))))

(defn cell-to-string [cell]
  (if cell "@" "."))

(defn row-to-string [row]
  (string/join ""
               (map cell-to-string row)))

(defn grid-to-string [grid]
  (string/join "\n"
               (map row-to-string grid)))

(defn solve-part-1 [grid]
  (- (count-rolls grid) (count-rolls (remove-accessible-rolls grid))))

(defn solve-part-2 [grid]
  (- (count-rolls grid) (count-rolls (remove-accessible-rolls-iterated grid))))


(comment
  (let [grid (load-grid ["..@@.@@@@.\n"
                         "@@@.@.@.@@\n"
                         "@@@@@.@.@@\n"
                         "@.@@@@..@.\n"
                         "@@.@@@@.@@\n"
                         ".@@@@@@@.@\n"
                         ".@.@.@.@@@\n"
                         "@.@@@.@@@@\n"
                         ".@@@@@@@@.\n"
                         "@.@.@@@.@.\n"
                         ""])]
    (solve-part-1 grid))

  (let [s (clojure.java.io/resource "day4.txt")]
    (with-open [r (clojure.java.io/reader s)]
      (solve-part-1 (load-grid (line-seq r)))))


  (let [grid (load-grid ["..@@.@@@@.\n"
                         "@@@.@.@.@@\n"
                         "@@@@@.@.@@\n"
                         "@.@@@@..@.\n"
                         "@@.@@@@.@@\n"
                         ".@@@@@@@.@\n"
                         ".@.@.@.@@@\n"
                         "@.@@@.@@@@\n"
                         ".@@@@@@@@.\n"
                         "@.@.@@@.@.\n"
                         ""])]
    (solve-part-2 grid))
  :rcf)




(let [s (clojure.java.io/resource "day4.txt")]
  (with-open [r (clojure.java.io/reader s)]
    (solve-part-2 (load-grid (line-seq r)))))
