(ns day-3.core
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.pprint :as pp]))

(defn to-azimuth [v]
  (case (first v)
    \U {:direction :up, :value (Integer/parseInt (apply str (rest v)))}
    \D {:direction :down, :value (Integer/parseInt (apply str (rest v)))}
    \R {:direction :right, :value (Integer/parseInt (apply str (rest v)))}
    \L {:direction :left, :value (Integer/parseInt (apply str (rest v)))}))

(defn read-data [f]
  (->> f
       (slurp)
       (string/split-lines)
       (map #(string/split % #","))))

(defn walk [[x y]
            {:keys [direction value]}]
  (case direction
    :up [x (+ y value)]
    :down [x (- y value)]
    :right [(+ x value) y]
    :left [(- x value) y]))

(defn to-coordinates
  ([azimuths] (reductions walk [0 0] azimuths))
  ([origin-ref azimuths] (reductions walk origin-ref azimuths)))

(defn to-wire
  "Given a list of points representing lines,
  returns all point pairs for every line"
  [raw-wire-data]
  (->> raw-wire-data
       (map to-azimuth)
       (to-coordinates)
       (partition 2 1)))

(def wire-colors [:red :black :green :blue :brown :white])

(defn color-wires
  [d]
  (zipmap wire-colors d))

(defn intersection
  "Finds the intersection point of
  two lines (AB and CD) defined by 2 coordinates x and y,
  if the lines do not intersect returns nil."
  [[[xa ya] [xb yb]] [[xc yc] [xd yd]]]
  ;(println [xa ya] "-" [xb yb] ".." [xc yc] "-" [xd yd])
  (let [z (- (* xa yb) (* ya xb))
        wx (- xc xd)
        kx (- xa xb)
        t (- (* xc yd) (* yc xd))
        dv (- (* (- xa xb) (- yc yd)) (* (- ya yb) (- xc xd)))
        wy (- yc yd)
        ky (- ya yb)]
    (when (not= dv 0)
      (let [x (/ (- (* z wx) (* kx t)) dv)
            y (/ (- (* z wy) (* ky t)) dv)]
        (when (and (<= (min xa xb) x (max xa xb))
                   (<= (min ya yb) y (max ya yb)))
          ;(println "intersection: " [x y])
          [x y])))))

(defn intersection-line
  "Finds the intersection point of
  two lines (AB and CD) defined by 2 coordinates x and y,
  if the lines do not intersect returns nil."
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  ;(println [x1 y1] "-" [x2 y2] ".." [x3 y3] "-" [x4 y4])
  (let [dv (- (* (- x1 x2) (- y3 y4)) (* (- y1 y2) (- x3 x4)))]
    (when (not= dv 0)
      (let [t (/ (- (* (- x1 x3) (- y3 y4)) (* (- y1 y3) (- x3 x4))) dv)
            u (/ (- (* (- x1 x2) (- y1 y3)) (* (- y1 y2) (- x1 x3))) dv)]
        (cond
          (<= 0 t 1) (let [x (+ x1 (* t (- x2 x1))) y (+ y1 (* t (- y2 y1)))]
                       (when (and (<= (min x1 x2) x (max x1 x2))
                                  (<= (min x3 x4) x (max x3 x4))
                                  (<= (min y1 y2) y (max y1 y2))
                                  (<= (min y3 y4) y (max y3 y4)))
                         ;(println [x y])
                         [(int x) (int y)]))
          (<= 0 u 1) (let [x (+ x3 (* u (- x4 x3))) y (+ y3 (* u (- y4 y3)))]
                       (when (and (<= (min x1 x2) x (max x1 x2))
                                  (<= (min x3 x4) x (max x3 x4))
                                  (<= (min y1 y2) y (max y1 y2))
                                  (<= (min y3 y4) y (max y3 y4)))
                         ;(println [x y])
                         [(int x) (int y)])))))))

(defn manhattan-distance
  ([[ox oy] [x y]]
   ;(println [x y])
   (+ (Math/abs (- x ox))
      (Math/abs (- y oy))))
  ([[x y]]
   ;(println [x y])
   (+ (Math/abs x) (Math/abs y))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (time
             (let [raw-data (read-data (first args))
                   wires (color-wires (map to-wire raw-data))]
               (println "raw data: " raw-data)
               (pp/pprint wires)
               (let [intrs (remove #(= [0 0] %)
                             (remove nil? (for [r (:red wires)
                                                b (:black wires)]
                                            (intersection-line r b))))]
                 (println intrs)
                 (println (apply min (map manhattan-distance intrs))))))))
