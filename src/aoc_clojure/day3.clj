(ns aoc-clojure.day3
  (:require  [clojure.java.io]
             [clojure.math :as math]))


(defn index-of-max [bank]
  (.indexOf bank
            (apply max bank)))

(defn max-joltage [bank n]
  (if
   (= n 0)
    0
    (let [next-digit-index (index-of-max (drop-last (dec n) bank))
          next-digit (nth bank next-digit-index)]
      (+
       (* next-digit (math/pow 10 (dec n)))
       (max-joltage (subvec (vec bank) (inc next-digit-index)) (dec n))))))

(defn str-to-bank [str]
  (map
   (fn [d] (Character/digit d 10))
   str))

(defn solve-part-1 [lines]
  (long (reduce + (map
                   (fn [line] (-> line str-to-bank (max-joltage 2)))
                   lines))))

(defn solve-part-2 [lines]
  (long (reduce + (map
                   (fn [line] (-> line str-to-bank (max-joltage 12)))
                   lines))))

(comment
  (index-of-max (str-to-bank "818181911112111"))
  (max-joltage (str-to-bank "987654321111111") 2)

  (let [s (clojure.java.io/resource "day3small.txt")]
    (with-open [r (clojure.java.io/reader s)]
      (solve-part-1 (line-seq r))))

  (let [s (clojure.java.io/resource "day3.txt")]
    (with-open [r (clojure.java.io/reader s)]
      (solve-part-1 (line-seq r))))

  (let [s (clojure.java.io/resource "day3small.txt")]
    (with-open [r (clojure.java.io/reader s)]
      (solve-part-2 (line-seq r))))

  :rcf)

(let [s (clojure.java.io/resource "day3.txt")]
  (with-open [r (clojure.java.io/reader s)]
    (solve-part-2 (line-seq r))))
