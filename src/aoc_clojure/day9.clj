(ns aoc-clojure.day9
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [meridian.clj-jts :as jts]))

(defn parse-input-line [line]
  (let [[x, y] (map Integer/parseInt (string/split line #","))]
    (jts/point [x, y])))

(defn parse-input [lines]
  (vec (map parse-input-line lines)))

(defn area-between [a b]
  (* (inc (abs (- (.getX a) (.getX b))))
     (inc (abs (- (.getY a) (.getY b))))))

(defn rect-from [a b]
  (jts/polygon [[[(.getX a) (.getY a)]
                 [(.getX a) (.getY b)]
                 [(.getX b) (.getY b)]
                 [(.getX b) (.getY a)]
                 [(.getX a) (.getY a)]]]))

(defn all-rects [points]
  (for [a points
        b points]
    {:a a :b b :area (area-between a b) :poly (rect-from a b)}))

(defn poly-from [points]
  (jts/polygon
   [(vec (concat  (->> points
                       (map (fn [p] [(.getX p) (.getY p)])))
                  [[(.getX (get points 0))
                    (.getY (get points 0))]]))]))

(defn solve-part-1 [points]
  (-> (apply max-key #(get % :area)
             (all-rects points))
      (get :area)
      long))

(defn solve-part-2 [points]
  (let [poly (poly-from points)]
    (-> (apply max-key #(get % :area)
               (filter
                (fn [rect]  (.contains poly  (get rect :poly)))
                (all-rects points)))
        (get :area)
        long)))

(defn small-input []
  (let [s (io/resource "day9small.txt")]
    (with-open [r (io/reader s)]
      (parse-input (line-seq r)))))

(defn input []
  (let [s (io/resource "day9.txt")]
    (with-open [r (io/reader s)]
      (parse-input (line-seq r)))))

(comment
  (area-between {:x 1 :y 2} {:x 2 :y 4})

  (solve-part-1 (small-input))

  (solve-part-1 (input))

  (.contains (poly-from (small-input)) (poly-from (small-input)))

  (.getY (jts/point [1 2]))

  (solve-part-2 (small-input))

  (solve-part-2 (input))

  :rcf)


