(ns anr-liga.util
  (:require [instaparse.core :as insta]
            [clojure.string :as s]))

(def a->z sort)
(def z->a (comp reverse sort))

(defn map-keys [f m]
  "apply f to each key in map m"
  (zipmap (map f (keys m)) (vals m)))

(defn map-values [f m]
  "apply f to each value in map m"
  (zipmap (keys m) (map f (vals m))))

(defn center [width s]
  "ex: (center 5 'x') -> '  x  '"
  (let [len (count s)
        right (quot (- width len) 2)
        left (- width (+ len right))
        make-space (fn [n] (apply str (repeat n \space)))]
    (str (make-space left) s (make-space right))))

(defn accumulate [f m]
  "for any map with list values, takes entries in order and apply f to previous and current value"
  (loop [k (a->z (keys m))
         s (take (count (first (vals m)))(repeat 0))
         r m]
    (if (empty? k)
      r
      (let [fk (first k)
            v (map f s (r fk))]
        (recur (rest k) v (assoc r fk v))))))

(defn map->csv
  "takes map of lists, ex: {'joe' (1 2 3), ...}"
  ([m]
    (->> m
         (map-values (partial s/join ","))
         a->z
         (map (partial s/join ","))
         (interpose "\n")
         s/join))
  ([m header] (str header "\n" (map->csv m))))
