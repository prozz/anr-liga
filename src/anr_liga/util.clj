(ns anr-liga.util)

(defn map-keys [f m]
  "apply f to each key in map m"
  (zipmap (map f (keys m)) (vals m)))

(defn map-values [f m]
  "apply f to each value in map m"
  (zipmap (keys m) (map f (vals m))))
