(ns aoc-clojure.day11
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-line [line]
  (let [[key vals] (string/split line #":" 2)]
    [(keyword key)
     (map keyword (-> vals
                      string/trim
                      (string/split #" ")))]))

(defn parse-input [lines]
  (reduce
   (fn [memo line]
     (apply assoc memo (parse-line line)))
   {}
   lines))

(defn count-paths [mapping start target visited]
  (if (=  start target)
    1
    (->>  (get mapping start)
          (filter #(not (contains? visited %)))
          (map #(count-paths mapping %1 target (conj visited %1)))
          (reduce +))))

(defn count-paths-2 [mapping start target visited]
  (if (=  start target)
    1
    (->>  (get mapping start)
          (filter #(not (contains? visited %)))
          (map #(count-paths mapping %1 target (conj visited %1)))
          (reduce +))))

(defn solve-part-1 [input]
  (count-paths input :you :out #{:you}))

(defn solve-part-2 [input]
  (let [count-paths
        (fn [mem-fn start v-dac v-fft]
          (if (= start :out)
            (if (and v-dac v-fft) 1 0)
            (->>  (get input start)
                  (map #(mem-fn mem-fn %1 (or v-dac (= %1 :dac)) (or v-fft (= %1 :fft))))
                  (reduce +))))
        count-paths-memo (memoize count-paths)]
    (count-paths-memo count-paths-memo :svr false false)))

(defn small-input []
  (let [s (io/resource "day11small.txt")]
    (with-open [r (io/reader s)]
      (parse-input (line-seq r)))))

(defn small-input-2 []
  (let [s (io/resource "day11small2.txt")]
    (with-open [r (io/reader s)]
      (parse-input (line-seq r)))))

(defn input []
  (let [s (io/resource "day11.txt")]
    (with-open [r (io/reader s)]
      (parse-input (line-seq r)))))

(comment
  (solve-part-1 (input))

  (solve-part-2 (input))

  :rcf)
