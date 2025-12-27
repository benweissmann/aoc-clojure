(ns aoc-clojure.day8
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.math :as math]))

(defn parse-input-line [line]
  (vec (map Integer/parseInt (string/split line #","))))

(defn parse-input [lines]
  (vec (map parse-input-line lines)))

(defn dist-between [a b]
  (math/sqrt (+ (math/pow (- (get a 0) (get b 0)) 2)
                (math/pow (- (get a 1) (get b 1)) 2)
                (math/pow (- (get a 2) (get b 2)) 2))))

(defn pairs-from [l]
  (for [x (range (count l))
        y (range (inc x) (count l))]
    [(get l  x) (get l  y)]))

(defn closest-neighbors [points]
  (sort-by
   #(get % :dist)
   (map
    #(hash-map :a (get %1 0),
               :b (get %1 1),
               :dist (dist-between (get %1 0) (get %1 1)))
    (pairs-from points))))

; Given a set of [x, y, z] points, initializes the circuit map:
; a map of {point -> circuit ID} where point is an [x, y, z] list
; and the circuit ID is an arbitrary integer
(defn initialize-circuits [points]
  (into {} (map-indexed (fn [idx itm] [itm idx]) points)))

; Given a new connection to make (map with {a: [x, y, z] and b: [x, y, z]})
; and a circuit map, returns a new circuit map with the connection made
(defn connect-circuit [circuits connection]
  (let [new-circuit (get circuits (get connection :a))
        old-circuit (get circuits (get connection :b))]
    (->> circuits
         (map (fn
                [[k v]]
                [k (if (= v old-circuit) new-circuit v)]))
         (into {}))))

(defn connect-points [connections points]
  (reduce
   connect-circuit
   (initialize-circuits points)
   connections))

(defn count-val-freqs [map]
  (reduce
   (fn [memo, v] (update memo v #(inc (or % 0))))
   {}
   (vals map)))

(defn product-top-vals [n map]
  (->> map vals sort reverse (take n) (apply *)))

(defn solve-part-1 [points n-connections product-top-n]
  (->> points
       (connect-points (take n-connections (closest-neighbors points)))
       count-val-freqs
       (product-top-vals product-top-n)))

(defn part-2-inner [circuits ordered-connections connection-n]
  (if
   (= 1 (-> circuits vals distinct count))
    ; done: no more circuits
    (let [last-connection (get ordered-connections (dec connection-n))]
      (* (-> last-connection (get :a) (get 0))
         (-> last-connection (get :b) (get 0))))

    ; recurse
    (part-2-inner
     (connect-circuit circuits  (get  ordered-connections connection-n))
     ordered-connections
     (inc connection-n))))

(defn solve-part-2 [points]
  (part-2-inner
   (initialize-circuits points)
   (vec (closest-neighbors points))
   0))


(defn small-input [] (parse-input
                      ["162,817,812",
                       "57,618,57",
                       "906,360,560",
                       "592,479,940",
                       "352,342,300",
                       "466,668,158",
                       "542,29,236",
                       "431,825,988",
                       "739,650,466",
                       "52,470,668",
                       "216,146,977",
                       "819,987,18",
                       "117,168,530",
                       "805,96,715",
                       "346,949,466",
                       "970,615,88",
                       "941,993,340",
                       "862,61,35",
                       "984,92,344",
                       "425,690,689"]))

(defn input []
  (let [s (io/resource "day8.txt")]
    (with-open [r (io/reader s)]
      (parse-input (line-seq r)))))


(comment
  (take 10 (closest-neighbors (small-input)))

  (solve-part-1 (small-input) 10 3)

  (solve-part-1 (input) 1000 3)

  (solve-part-2 (small-input))

  :rcf)

(solve-part-2 (input))

