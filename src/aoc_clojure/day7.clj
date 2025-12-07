(ns aoc-clojure.day7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn solve-part-1 [lines-coll]
  (let [lines (vec lines-coll)
        first-line (first lines)
        data-lines (drop 1 lines)]

    (reduce
     (fn [memo line]
       (let [new-beams (map
                        #(if (= (get line %1) \^)
                           ; split
                           [(inc %1) (dec %1)]
                           ; no split
                           [%1])
                        (get memo :beams))]
         {:split-count (+
                        (get memo :split-count)
                        (->> new-beams (filter #(= (count %) 2)) count))
          :beams (->> new-beams flatten (apply hash-set))}))
     {:split-count 0 :beams #{(string/index-of first-line "S")}}
     data-lines)))


; Takes a collection of {n: int, pos: int} and combines it into
; a smaller collection, combined by pos and summing the n's for each pos.
(defn combine-positions [items]
  (let [n-by-pos (reduce
                  (fn [memo {n :n pos :pos}]
                    (update memo  pos  #(+ (or % 0)  n)))
                  {}
                  items)]
    (map
     #(hash-map :n (second %1) :pos (first %1))
     n-by-pos)))

(defn solve-part-2 [lines-coll]
  (let [lines (vec lines-coll)
        first-line (first lines)
        data-lines (drop 1 lines)]

    (reduce
     (fn [memo line]
       (->> memo
            (map
             (fn [{n :n pos :pos}]
               (if (= (get line pos) \^)
                ; split
                 [{:n n :pos (inc pos)} {:n n :pos (dec pos)}]
                ; no split
                 [{:n n :pos pos}])))
            flatten
            combine-positions))
     [{:n 1 :pos (string/index-of first-line "S")}]
     data-lines)))

(defn sum-n [sln]
  (->> sln (map #(get % :n)) (reduce +)))

(comment

  (let [s (io/resource "day7small.txt")]
    (with-open [r (io/reader s)]
      (solve-part-1 (line-seq r))))

  (let [s (io/resource "day7.txt")]
    (with-open [r (io/reader s)]
      (get (solve-part-1 (line-seq r)) :split-count)))

  (let [s (io/resource "day7small.txt")]
    (with-open [r (io/reader s)]
      (sum-n (solve-part-2 (line-seq r)))))

  (let [s (io/resource "day7.txt")]
    (with-open [r (io/reader s)]
      (sum-n (solve-part-2 (line-seq r)))))


  :rcf)

