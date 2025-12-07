(ns aoc-clojure.day6
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))


(defn parse-input-part-1 [lines]
  (let [split-lines (vec (map
                          #(-> %1 string/trim (string/split #"\s+"))
                          lines))]

    (map (fn [idx]
           {:op (-> split-lines last (get idx))
            :vals (map #(-> split-lines  (get %1) (get idx) Long/parseLong)
                       (range 0 (dec (count split-lines))))})
         (-> split-lines first count range))))

(defn parse-input-part-2 [lines-coll]
  (let [lines (vec lines-coll)
        ops-line (-> lines last string/trim (string/split #"\s+"))
        vals-lines (pop lines)]
    (reduce
     (fn [memo, col-idx]
       (let
        [col (->> vals-lines (map #(get %1 col-idx)) (string/join ""))]
         (if (-> col string/trim count (= 0))
           ; empty column starts new problem: use next op and empty vals
           (conj memo {:op (get ops-line (count memo)) :vals []})
           ; otherwise, add the column as a number to the current problem
           (conj
            (pop memo)
            (update (last memo) :vals conj (-> col string/trim Long/parseLong))))))
     [{:op (first ops-line) :vals []}]
     (range (count (first lines))))))


(defn solve-problem [problem]
  (reduce
   (if (= (get problem :op) "+") + *)
   (get  problem :vals)))

(defn solve-problems [input]
  (->> input (map solve-problem) (reduce +)))


(comment

  (let [s (io/resource "day6.txt")]
    (with-open [r (io/reader s)]
      (solve-problems (parse-input-part-1 (line-seq r)))))
  :rcf)

(let [s (io/resource "day6.txt")]
  (with-open [r (io/reader s)]
    (solve-problems (parse-input-part-2 (line-seq r)))))
