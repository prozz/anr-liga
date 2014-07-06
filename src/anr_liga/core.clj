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

(defn result? [node]
  (and (seq? node)
       (= 2 (count node))
       (string? (first node))
       (integer? (last node))))

(defn duel? [node]
  (and (seq? node)
       (= 2 (count node))
       (result? (first node))
       (result? (last node))))

(defn result-nodes [data]
  (filter result? (nodes data)))

(defn duel-nodes [data]
  (filter duel? (nodes data)))

(defn player-total-scores [data]
  "map: player -> score, ex. {'joe' 3, 'bob' 6}"
  (letfn [(sum-results [results] (->> results flatten (filter integer?) (apply +)))]
    (->> (result-nodes data)
         (group-by first)
         (map-values sum-results))))

(defn generate-score-table [data]
  "txt version of score table"
  (->> (player-total-scores data)
       (sort-by last)
       reverse
       (map cons (iterate inc 1))
       (map (partial apply format "%2d. | %-15s | %2d\n"))
       s/join))

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
      (r (list p1 p2)))))

(match-results data)
allowed-players
(generate-match-table data)

(generate-score-table data)
(spit "table.txt" (generate-score-table data))


