(ns aoc-clojure.day10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))
(defn padded-bin [int width]
  (let [bin (Integer/toBinaryString int)]
    (str
     "0b"
     (apply str (repeat (- width (count bin)) "0"))
     bin)))

(defrecord Machine [width lights buttons joltages])

(defmethod print-method Machine [m ^java.io.Writer writer]
  (.write writer
          (format "lights:   %s\nbuttons:  %s\njoltages: %s\n"
                  (padded-bin (:lights m) (:width m))
                  (string/join ", " (map #(padded-bin % (:width m)) (:buttons m)))
                  (:joltages m))))

(defn parse-lights [lights]
  (reduce
   (fn [memo, chr]
     (+ (bit-shift-left memo 1)
        (if (= chr \.) 0 1)))
   0
   (rest (butlast lights))))

(defn parse-button [button width]
  (reduce + (map
             #(bit-shift-left 1 (- (dec width) (Integer/parseInt %)))
             (string/split
              (subs button 1 (-> button count dec))
              #","))))

(defn parse-buttons [buttons width]
  (vec (map #(parse-button % width) buttons)))

(defn parse-joltages [joltages]
  (vec (map Integer/parseInt
            (string/split
             (subs joltages 1 (-> joltages count dec))
             #","))))

(defn parse-input-line [line]
  (let [[lights & buttons-and-joltage] (string/split line #" ")
        width (- (count lights) 2)]
    (Machine.
     width
     (parse-lights lights)
     (parse-buttons (butlast buttons-and-joltage) width)
     (parse-joltages (last buttons-and-joltage)))))

(defn parse-input [lines]
  (vec (map parse-input-line lines)))


; Generates all bitmasks equal to or less than the given width
; sorted from fewest 1s to most 1s
(defn bitmasks [width]
  (sort-by Integer/bitCount (range (bit-shift-left 1 width))))

(defn bitmask-matches? [bitmask machine]
  (= (:lights machine)
     (reduce (fn [memo button-idx]
               (if (bit-test bitmask button-idx)
                 (bit-xor memo (get (:buttons machine) button-idx))
                 memo))
             0
             (range (count (:buttons machine))))))

(defn find-button-bitmask [machine]
  (first (filter (fn [bitmask] (bitmask-matches? bitmask machine))
                 (bitmasks (count (:buttons machine))))))

(defn small-input []
  (let [s (io/resource "day10small.txt")]
    (with-open [r (io/reader s)]
      (parse-input (line-seq r)))))

(defn input []
  (let [s (io/resource "day10.txt")]
    (with-open [r (io/reader s)]
      (parse-input (line-seq r)))))

(defn solve-part-1 [input]
  (reduce + (map #(Integer/bitCount (find-button-bitmask %)) input)))

; coverts the bitmask representation of a button to a set of indexes
(defn button-to-indexes [button-bitmask width]
  (set (filter
        (fn [idx]
          (bit-test button-bitmask (- (dec width) idx)))
        (range width))))

; decrements the joltages in the positions matching the given button
; (as an index set)
(defn dec-joltages [button-indexes joltages]
  (map-indexed
   (fn [idx j] (if (contains? button-indexes idx) (dec j) j))
   joltages))

; returns true if the given joltages (vec<int>) can be satisfied by
; the given buttons (as indexes - set<int>) within the given number of presses.
(defn can-satisfy-joltages [joltages buttons presses]
  (cond
    ; failure case: if any joltage is <0, this is an unsuccessful branch
    (some neg? joltages) false

    ; base case: if all joltages are 0, we've found a solution
    (every? zero? joltages) true

    ; failure case: if we're out of presses, we can't recurse any further
    (zero? presses) false

    ; otherwise, try pressing each button
    (some
     (fn [button] (can-satisfy-joltages (dec-joltages button joltages)
                                        buttons
                                        (dec presses)))
     buttons) true))

(defn min-buttons-for-joltage [machine]
  (let [joltages (:joltages machine)
        buttons (map #(button-to-indexes % (:width machine)) (:buttons machine))]
    (first (filter (fn [depth] (can-satisfy-joltages joltages buttons depth)) (range 100)))))

(defn solve-part-2 [input]
  (reduce + (pmap min-buttons-for-joltage input)))

(comment
  (println (string/join "\n" (map #(pr-str %) (small-input))))

  (format (str "0b%0" 5 "s") "123")

  (map #(padded-bin % 4) (bitmasks 4))

  (bitmask-matches? 2r110000 (first (small-input)))

  (padded-bin (find-button-bitmask (first (small-input))) 6)

  (map #(padded-bin (find-button-bitmask %1) (count (:buttons %1))) (small-input))

  (solve-part-1 (input))

  (button-to-indexes 2r0111 4)

  (min-buttons-for-joltage (second (small-input)))

  (solve-part-2 (small-input))
  :rcf)


