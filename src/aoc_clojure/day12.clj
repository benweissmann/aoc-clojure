(ns aoc-clojure.day12
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-puzzle [puzzle]
  (let [[area counts-str] (-> puzzle string/trim (string/split #": "))
        [width height] (map Integer/parseInt (string/split area #"x" 2))
        counts (-> counts-str string/trim (string/split #" "))]
    {:width width :height height :counts (vec (map Integer/parseInt counts))}))

(defn parse-puzzles [puzzles]
  (vec (map parse-puzzle puzzles)))

(defn classify-puzzle [puzzle shape-counts]
  (let [puzzle-area (* (get puzzle :width) (get puzzle :height))
        puzzle-area-blocks (*
                            (quot (get puzzle :width) 3)
                            (quot (get puzzle :height) 3))
        total-shape-count (reduce + shape-counts)
        total-shape-area (->> (get puzzle :counts)
                              (map-indexed #(* (get shape-counts %1) %2))
                              (reduce +))]
    (cond (< puzzle-area total-shape-area) :no
          (>= puzzle-area-blocks total-shape-count) :yes
          :else :maybe)))

(defn classify-puzzles [input]
  (frequencies (map #(classify-puzzle % (get input :shape-counts)) (get input :puzzles))))

(defn small-input []
  {:shape-counts [7 7 7 7 7 7]
   :puzzles (parse-puzzles ["4x4: 0 0 0 0 2 0"
                            "12x5: 1 0 1 0 2 2"
                            "12x5: 1 0 1 0 3 2"])})

(defn input []
  (let [s (io/resource "day12.txt")]
    (with-open [r (io/reader s)]
      {:shape-counts [7 7 6 7 7 5]
       :puzzles (parse-puzzles (line-seq r))})))


(comment
  (classify-puzzles (small-input))
  (classify-puzzles (input))

  :rcf)
