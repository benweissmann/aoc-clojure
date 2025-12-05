(ns aoc-clojure.day5
  (:require
   [clojure.java.io]
   [clojure.string :as string]))

(defn range-includes? [range n]
  (<= (get range :min) n (get range :max)))

(defn parse-range [range-str]
  (let [ends (string/split range-str #"\-" 2)]
    {:min (Long/parseLong (first ends)) :max (Long/parseLong (second ends))}))

(defn parse-ingredient [ingredient-str]
  (Long/parseLong ingredient-str))

(defn parse-input [lines]
  (select-keys
   (reduce
    (fn [memo line]
      (if (= (get memo :parser-state) :ranges)
        (if (= 0 (count (string/trim line)))
          (merge memo {:parser-state :ingredients})
          (update memo :ranges concat [(parse-range (string/trim line))]))
        (update memo :ingredients concat [(parse-ingredient (string/trim line))])))
    {:ranges [] :ingredients [] :parser-state :ranges}
    lines)
   [:ranges :ingredients]))

(defn is-fresh [ranges ingredient]
  (some
   (fn [range] (range-includes? range ingredient))
   ranges))

(defn count-fresh [input]
  (->> (get input :ingredients)
       (filter (fn [ingredient] (is-fresh (get input :ranges) ingredient)))
       count))

(defn add-range-to-ranges [ranges new-range]
  (let [merged-ranges
        (reduce
         (fn [memo range]
           (if (or
                (< (get new-range :max) (get range :min))
                (> (get new-range :min) (get range :max)))
             ; no overlap: pass range through unmodified
             (update memo :ranges concat [range])
             ; overlap: expand range with the new range and remember that we
             ; found an overlap
             {:ranges (concat
                       (get memo :ranges)
                       [{:min (min (get new-range :min) (get range :min))
                         :max (max (get new-range :max) (get range :max))}])
              :found true}))
         {:ranges [] :found false}
         ranges)]

    (if (get merged-ranges :found)
      ; found an overlapping range: we added the new range by expanding the
      ; existing one
      (get merged-ranges :ranges)

      ; no overlapping range found: add this as a new range
      (concat ranges [new-range]))))

(defn merge-ranges [ranges]
  (reduce
   add-range-to-ranges
   []
   (sort-by (fn [range] [(get range :min) (get range :max)]) ranges)))


(defn solve-part-2 [input]
  (reduce
   +
   (map
    (fn [range] (inc (- (get range :max) (get range :min))))
    (merge-ranges (get input :ranges)))))

(def small-input
  (parse-input ["3-5\n"
                "10-14\n"
                "16-20\n"
                "12-18\n"
                "\n"
                "1\n"
                "5\n"
                "8\n"
                "11\n"
                "17\n"
                "32\n"]))

(comment

  (count-fresh small-input)



  (let [s (clojure.java.io/resource "day5.txt")]
    (with-open [r (clojure.java.io/reader s)]
      (count-fresh (parse-input (line-seq r)))))

  (merge-ranges (get small-input :ranges))
  (solve-part-2 small-input)


  :rcf)




(let [s (clojure.java.io/resource "day5.txt")]
  (with-open [r (clojure.java.io/reader s)]
    (solve-part-2 (parse-input (line-seq r)))))

