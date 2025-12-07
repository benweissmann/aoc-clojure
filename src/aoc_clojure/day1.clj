(ns aoc-clojure.day1
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:require [clojure.math :as math]))

(defn parse-instruction [instr]
  (cond
    (string/starts-with? instr "L")
    (- 0 (Integer/parseInt (subs instr 1)))
    (string/starts-with? instr "R")
    (Integer/parseInt (subs instr 1))
    :else 0))

(defn apply-instructions [instrs]
  (reduce
   (fn [acc, instr]
     (conj acc
           (+ (last acc) (parse-instruction instr))))
   [50]
   instrs))

(defn solve-part-1 [instrs]
  (reduce
   (fn [count, pos]
     (+ count (if (= (mod pos 100) 0) 1 0)))
   0
   (apply-instructions instrs)))

(defn zero-clicks-between [a, b]
  (+
     ; Count number of times we cross a boundary of 100

   (int (abs (-
              (math/floor (/ a 100.0))
              (math/floor (/ b 100.0)))))

   ; When going down (left), add 1 if we end on 0 (we didn't cross the boundary,
   ; but we did hit 0), and subtract one if we started on 0 (the first boundary
   ; cross doesn't count because we already hit 0)
   (if (and (> a b) (= (mod a 100) 0)) -1 0)
   (if (and (> a b) (= (mod b 100) 0)) 1 0)))


(defn solve-part-2 [instrs]
  (reduce
   (fn [count, [a, b]]
     (+ count
        (zero-clicks-between a b)))
   0
   (partition 2 1 (apply-instructions instrs))))

(comment

  (parse-instruction "L10")
  (parse-instruction "R3")
  (parse-instruction "X")

  (zero-clicks-between -1 0)

  (solve-part-2 ["L50", "R1", "L1", "L1", "R1"]) ; 1 + 0 + 1 + 0 + 1 = 3

  (let [s (io/resource "day1small.txt")]
    (with-open [r (io/reader s)]
      (solve-part-1 (line-seq r))))


  (let [s (io/resource "day1.txt")]
    (with-open [r (io/reader s)]
      (solve-part-1 (line-seq r))))


  (let [s (io/resource "day1small.txt")]
    (with-open [r (io/reader s)]
      (solve-part-2 (line-seq r))))

  (let [s (io/resource "day1.txt")]
    (with-open [r (io/reader s)]
      (partition 2 1 (apply-instructions (line-seq r)))))

  :rcf)


(let [s (io/resource "day1.txt")]
  (with-open [r (io/reader s)]
    (solve-part-2 (line-seq r))))
