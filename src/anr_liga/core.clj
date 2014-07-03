(ns anr-liga.core
  (:use [anr-liga.util])
  (:require [instaparse.core :as insta])
  (:gen-class))

(def max-games 2)
(def allowed-players ["Kuba M.", "Pawel R.", "David C."])


(def parser
    (insta/parser
      "<games> = game*
       game = <whitespace>* date <whitespace>* players <whitespace> score whitespace* <'\\n'>*
       date = #'\\d{4}-\\d{2}-\\d{2}'
       players = player <whitespace 'vs' whitespace> player
       player = name
       name = word whitespace letter dot?
       score = number <':'> number
       <word> = #'[a-zA-Z]+'
       <letter> = #'[a-zA-Z]'
       number = #'[0-9]'
       <dot> = #'\\.'
       <whitespace> = #'\\s+'"))

(defn parse-games [data]
  (insta/transform
     {:players list
      :player str
      :name str
      :score list
      :number read-string
      :game interleave}
   (parser data)))

(def data "Marek T. vs Kuba M.        3:1
    Dawid S. vs Maciej C.     2:2
    Pawel R. vs Mikolaj K.    3:1
    Dawid S. vs Sebastian Z.  1:3
    Kuba M. vs David C.       1:3
    Maciej C. vs Mateusz N.   1:3
    Pawel R. vs David C.      2:2
   David C. vs Kuba M.        1:3")

(def example-games
  (parse-games data))

example-games

(defn all-players [games]
  (->> games
      flatten
      (filter string?)
      distinct))


(defn count-points [games]
  (->>
    games
    flatten
    (partition 2)
    (group-by first)
    (map-map-values (comp (partial apply +) (partial filter number?) flatten)
   )))


(defn count-matches [games]
  (->> games
       (map (partial filter string?))
       (map sort)
       (group-by identity)
       (map-map-values count)
       (map-map-keys (partial clojure.string/join " vs "))))

(defn valid-players? [games]
  (= (sort allowed-players) (sort (all-players games))))

(defn valid-matches? [games]
  (let [m (->> (count-matches games) vals (apply max))]
    (< m max-games)))

(count-points example-games)
(all-players example-games)
(count-matches example-games)
(valid-matches? example-games)
(valid-players? example-games)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

