(ns anr-liga.util)

(defn map-map
    "Returns a new map with each key-value pair in `m` transformed by `f`. `f` takes the arguments `[key value]` and should return a value castable to a map entry, such as `{transformed-key transformed-value}`."
    [f m]
    (into (empty m) (map #(apply f %) m)) )

(defn map-map-keys [f m]
    (map-map (fn [key value] {(f key) value}) m) )

(defn map-map-values [f m]
    (map-map (fn [key value] {key (f value)}) m) )
