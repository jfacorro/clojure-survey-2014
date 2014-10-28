(ns cs2014.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(declare lines)

(def stop-words 
  (-> (io/resource "stop-words.txt")
    slurp
    lines
    set))

(defn remove-stop-words
  "Remove all stop words from string."
  [ws]
  (filter (comp not stop-words) ws))

(defn ngrams
  [n ws]
  (->> ws
    (partition n 1)
    (map (partial interpose "-"))
    (map (partial apply str))))

(defn tokenize
  [s]
  (->> (string/split s #"[^\d\w'\.<]+")
    (filter (partial re-matches #"[\w\d'<]+"))
    (filter (comp not empty?))
    (map string/lower-case)))

(defn lines
  [s]
  (string/split s #"[\n\r][\r]?"))

(defn sentences
  [filename]
  (->> (io/resource filename)
    slurp
    lines
    (map tokenize)
    (map remove-stop-words)
    (map #(mapcat (fn [n] (ngrams n %)) [1 2 3]))))

(defn update-count
  [[ngram n]]
  (let [f (-> ngram (string/split #"-") count)]
    [ngram (* n f (Math/sqrt f))]))

(defn process-file
  [filename exceptions]
  (->> (sentences filename)
    (reduce into [])
    (filter (comp not exceptions))
    frequencies
    (filter #(< 1 (second %)))
    (map update-count)
    (sort-by second >)))

(defn generate-words
  [fqs filename]
  (->> (mapcat (fn [[w n]] (repeat n w)) fqs)
    (interpose " ")
    (apply str)
    (spit filename)))

(def features (process-file "clj-feature.txt" #{"clojure" "clojurescript" "better" "feature" "support" "like"}))
(generate-words (take 250 features) "clj-features.words.txt")

(def weakness (process-file "clj-weakness.txt" #{"clojure" "clojurescript" "time" "startup" "lack" "like" "can"}))
(generate-words (take 250 weakness) "clj-weakness.words.txt")

(def general (process-file "clj-general.txt" #{"clojure" "clojurescript" "language"}))
(generate-words (take 250 general) "clj-general.words.txt")


