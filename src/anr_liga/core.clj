(ns anr-liga.core
  (:use [anr-liga.util])
  (:require [instaparse.core :as insta]
            [clojure.string :as s])
  (:gen-class))

(def max-games 2)

(def allowed-players
     ["Mateusz K."
      "Pawel R."
      "Marek T."
      "Jan S."
      "Maciej C."
      "Mateusz N."
      "David C."
      "Kuba M."
      "Sebastian Z."
      "Michal R."
      "Sebastian S."
      "Dawid S."
      "Mikolaj K."])

(def allowed-runner-ids
     ["Gabe"
      "Andy"
      "Ken"
      "Silhouette"
      "Iain"
      "Kate"
      "CT"
      "Kit"
      "Noise"
      "Whizzard"
      "Reina"])

(def allowed-corp-ids
     ["NBN:MN"
      "NBN:TWiY"
      "HB:EtF"
      "HB:CI"
      "HB:ST"
      "HB:ND"
      "J:PE"
      "J:RP"
      "J:TI"
      "J:HM"
      "W:BaBW"
      "W:BWBI"
      "GRNDL"])

(def parser
  (let [quotify  (fn [x] (map #(str "'" % "'") x))
        tokenify (fn [x] (s/join "|" x))
        corps    (-> allowed-corp-ids   quotify tokenify)
        runners  (-> allowed-runner-ids quotify tokenify)
        players  (-> allowed-players    quotify tokenify)]
    (insta/parser
      (str
       "days = day*
       day = date <':'> <eol>? duels
       duels = duel+
       date = #'\\d{4}-\\d{2}-\\d{2}'
       duel = players <w> score <w> <(comments)?> <w> games
       players = player <w> <'vs'> <w> player
       score = digit <':'> digit
       games = ((game) | (game game))? <eol>?
       game = (<w> runner-id <w> result <w> corp-id | <w> corp-id <w> result <w> runner-id <w>) <w> <(comments)?>
       <result> = '>'|'<'|'='
       <runner-id> = "runners"
       <corp-id> = "corps"
       <player> = "players"
       comments = '(' #'[a-zA-Z0-9: ]'+ ')' <w> <eol>
       digit = #'\\d'
       w = #'\\s'*
       eol = #'\\n'
      "))))


(defn parse [data]
  (insta/transform
       {:days list
        :day list
        :date str
        :players list
        :games list
        :game list
        :duels list
        :duel (fn [players scores games]
                  (conj games
                        (->> (interleave players scores)
                             (partition 2))))
        :score list
        :digit read-string}
     (parser data)))

(clojure.pprint/pprint (parse (slurp "resources/data.txt")))

(def data (parse (slurp "resources/data-short.txt")))

(defn nodes [tree]
  (tree-seq sequential? identity tree))

(defn score? [node]
  "ex: ('joe' 3)"
  (and (seq? node)
       (= 2 (count node))
       (string? (first node))
       (integer? (last node))))

(defn duel? [node]
  "ex: (('joe' 3) ('bob' 1))"
  (and (seq? node)
       (= 2 (count node))
       (score? (first node))
       (score? (last node))))

(defn score-nodes [data]
  (filter score? (nodes data)))

(defn duel-nodes [data]
  (filter duel? (nodes data)))

(defn scores-by-player [data]
  (map-values (partial map last) (group-by first (score-nodes data))))

(defn total-score-by-player [data]
  (map-values (partial apply +) (scores-by-player data)))

(defn duel-count-by-player [data]
  (map-values count (scores-by-player data)))

(defn scores-table [data]
  (let [scores (total-score-by-player data)
        duel-count (duel-count-by-player data)
        table (merge-with list duel-count scores)]
    (reverse (sort-by (comp last last) table))))

(defn generate-score-table [data]
  "txt version of score table"
  (->> (scores-table data)
       (map cons (iterate inc 1))
       (map flatten)
       (map (partial apply format "%2d. | %-15s | %2d | %2d\n"))
       s/join
       (str "some header\n")))




(defn normalize-match-result [match]
  "note: result is sorted, ex: (('joe' 3) ('bob' 1)) -> (('bob' 'joe') (1 3))"
  (let [[names scores] (map list (first match) (last match))]
    (if (= names (sort names))
      (list names scores)
      (list (reverse names) (reverse scores)))))

(defn match-results [data]
  (->> (duel-nodes data)
       (map normalize-match-result)
       (group-by first)
       (map-values (partial map last))))

(match-results data)

(defn invalid-matches [results]
  "no more than max-games between 2 players"
  (filter (comp (partial < max-games) count last) results))

(invalid-matches (match-results data))

(defn generate-match-table [data]
  (let [r (match-results data)]
    (for [p1 allowed-players
          p2 allowed-players]
      (let [x (r (list p1 p2))]
        (if (nil? x)
          (list p1 p2)
          x)))))

allowed-players
(match-results data)
(generate-match-table data)

(generate-score-table data)
(spit "table.txt" (generate-score-table data))


