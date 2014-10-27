(ns cs2014.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

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
  (->> (string/split s #"[^\d\w'\.]+")
    (filter (partial re-matches #"[\w\d']*"))
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

(defn process-file
  [filename exceptions]
  (->> (sentences "clj-feature.txt")
    (reduce into [])
    (filter (comp not exceptions))
    frequencies
    (sort-by second >)
    (filter #(< 1 (second %)))))

(def features (process-file "clj-feature.txt" #{"clojure" "clojurescript" "better" "feature" "support" "like"}))
(def features (process-file "clj-feature.txt" #{"clojure" "clojurescript" "better" "feature" "support" "like"}))

(take 100 features)


