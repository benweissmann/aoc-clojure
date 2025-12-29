(ns aoc-clojure.day10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import [org.chocosolver.solver Model]
           [org.chocosolver.solver.variables IntVar]
           [org.chocosolver.solver.search.strategy Search]
           [org.chocosolver.solver.search.strategy.strategy AbstractStrategy]))

(defn padded-bin [int width]
  (let [bin (Integer/toBinaryString int)]
    (str
     "0b"
     (apply str (repeat (- width (count bin)) "0"))
     bin)))

(defrecord Machine [index model width lights buttons button-indexes joltages])

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


; coverts the bitmask representation of a button to a set of indexes
(defn button-to-indexes [button-bitmask width]
  (set (filter
        (fn [idx]
          (bit-test button-bitmask (- (dec width) idx)))
        (range width))))

(defn parse-joltages [joltages]
  (vec (map Integer/parseInt
            (string/split
             (subs joltages 1 (-> joltages count dec))
             #","))))

(defn parse-input-line [idx line]
  (let [[lights & buttons-and-joltage] (string/split line #" ")
        width (- (count lights) 2)
        buttons (parse-buttons (butlast buttons-and-joltage) width)]
    (Machine.
     idx
     (Model/new)
     width
     (parse-lights lights)
     buttons
     (vec (map #(button-to-indexes % width) buttons))
     (parse-joltages (last buttons-and-joltage)))))

(defn parse-input [lines]
  (vec (map-indexed parse-input-line lines)))


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

(defn min-buttons-for-joltage-slow [machine]
  (let [joltages (:joltages machine)
        buttons (:button-indexes machine)]
    (first (filter (fn [depth] (can-satisfy-joltages joltages buttons depth)) (range 100)))))

(defn add-constraint [machine btn-vars idx]
  (let [joltage (get (:joltages machine) idx)
        var-btn-indexes (filter
                         (fn [btn-idx]
                           (contains? (get (:button-indexes machine) btn-idx) idx))
                         (range (count btn-vars)))]
    (if (> (count var-btn-indexes) 0)
      (.post (.sum (:model machine)
                   (into-array IntVar (map #(get btn-vars %) var-btn-indexes))
                   "="
                   joltage))
      nil)))
(defn get-sum-var [machine btn-vars]
  (let [total-joltage (reduce + (:joltages machine))
        sum-var (.intVar (:model machine) "totalSum" 0 total-joltage)]
    (.post (.sum (:model machine)
                 (into-array IntVar btn-vars)
                 "="
                 sum-var))
    sum-var))

(defn min-buttons-for-joltage-fast [machine]
  (let [width (:width machine)
        model (:model machine)
        btn-count (count (:buttons machine))
        max-joltage (-> (:joltages machine) sort reverse first)
        btn-vars (vec (map #(.intVar model (str "btn" %) 0 max-joltage) (range btn-count)))
        _ (vec (map #(add-constraint machine btn-vars %) (range width)))
        sum-var (get-sum-var machine btn-vars)
        sln (.findOptimalSolution (.getSolver model) sum-var false nil)]
    (.getIntVal sln sum-var)))

(defn solve-part-2 [input]
  (reduce + (pmap
             (fn [machine]
               (let [sln (min-buttons-for-joltage-fast machine)]
                 (println (str "Solved " (:index machine) ": " sln))
                 sln))
             input)))

(comment
  (println (string/join "\n" (map #(pr-str %) (small-input))))

  (format (str "0b%0" 5 "s") "123")

  (map #(padded-bin % 4) (bitmasks 4))

  (bitmask-matches? 2r110000 (first (small-input)))

  (padded-bin (find-button-bitmask (first (small-input))) 6)

  (map #(padded-bin (find-button-bitmask %1) (count (:buttons %1))) (small-input))

  (solve-part-1 (input))

  (button-to-indexes 2r0111 4)

  (min-buttons-for-joltage-slow (first (small-input)))

  (min-buttons-for-joltage-fast (first (small-input)))

  (solve-part-2 (input))
  :rcf)


