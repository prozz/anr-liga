(ns anr-liga.util)

(defn map-keys [f m]
  "apply f to each key in map m"
  (zipmap (map f (keys m)) (vals m)))

(defn map-values [f m]
  "apply f to each value in map m"
  (zipmap (keys m) (map f (vals m))))

(defn spit-rows
  ([file rows] (write-rows file, rows, "\n"))
  ([file rows separator] (spit file (s/join separator rows))))

(defn center [width s]
  "ex: (center 5 'x') -> '  x  '"
  (let [len (count s)
        right (quot (- width len) 2)
        left (- width (+ len right))
        make-space (fn [n] (apply str (repeat n \space)))]
    (str (make-space left) s (make-space right))))

