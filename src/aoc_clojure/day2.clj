(ns aoc-clojure.day2
  (:require [clojure.string :as string]
            [clojure.java.io]))

(defn parse-range [range-str]
  (map Long/parseLong
       (string/split range-str #"\-" 2)))

(defn parse-ranges [ranges-str]
  (map parse-range
       (string/split ranges-str #",")))

(defn combine-ranges [ranges]
  (apply concat
         (map
          (fn [start-end]
            (range (first start-end) (+ 1 (last start-end))))
          ranges)))

(defn is-invalid-id-for-chunk-size [id-str chunk-size]
  (if (and
       (> chunk-size 0)
       (<= chunk-size (/ (count id-str) 2))
       (= 0 (mod (count id-str) chunk-size)))
    (apply =
           (map (fn [chrs] (apply str chrs))
                (partition chunk-size id-str)))
    false))


(defn is-invalid-id-part-1 [id]
  (let [id-str (str id)]
    (is-invalid-id-for-chunk-size id-str
                                  (-> id-str count (/ 2) Math/ceil int))))

(defn is-invalid-id-part-2 [id]
  (let [id-str (str id)]
    (some (fn [chunk-size] (is-invalid-id-for-chunk-size id-str  chunk-size))
          (range 1 (-> id-str count (/ 2) Math/ceil int inc)))))


(defn solve-part-1 [ranges-str]
  (->> ranges-str
       parse-ranges
       combine-ranges
       (filter is-invalid-id-part-1)
       (reduce +)))

(defn solve-part-2 [ranges-str]
  (->> ranges-str
       parse-ranges
       combine-ranges
       (filter is-invalid-id-part-2)
       (reduce +)))

(comment
  (combine-ranges (parse-ranges "10-20,30-40"))
  (is-invalid-id-for-chunk-size "1212" 2)
  (is-invalid-id-part-2 121212)
  (solve-part-1 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

  (is-invalid-id-part-2 "1212")


  (let [s (clojure.java.io/resource "day2.txt")]
    (solve-part-1 (string/trim (slurp s))))

  (apply str (take 2 "foo"))
  :rcf)

