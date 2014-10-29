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
    (map #(mapcat (fn [n] (ngrams n %)) [2 3]))))

(defn update-count
  [[ngram n]]
  (let [f (-> ngram (string/split #"-") count)]
    [ngram (* n 1)]))

(defn process-file
  [filename exceptions]
  (->> (sentences filename)
    (reduce into [])
    (filter (comp not exceptions))
    frequencies
    (filter #(< 1 (second %)))
    (map update-count)
    (sort-by second >)))

(defn camel-case
  [w]
  (->> (string/split w #"-")
    (map string/capitalize)
    (apply str)))

(defn generate-words
  [fqs filename]
  (->> (mapcat (fn [[w n]] (repeat n (camel-case w))) fqs)
    (interpose " ")
    (apply str)
    (spit filename)))

(def features (process-file "clj-feature.txt" #{"clojure" "clojurescript" "better" "feature" "support" "like"}))
(generate-words (take 250 features) "clj-features.words.txt")

(def weakness (process-file "clj-weakness.txt" #{"clojure" "clojurescript" "time" "startup" "lack" "like" "can"}))
(generate-words (take 250 weakness) "clj-weakness.words.txt")

(def general (process-file "clj-general.txt" #{"clojure" "clojurescript" "language"}))
(generate-words (take 250 general) "clj-general.words.txt")

;; ClojureScript

(def cljs-features (process-file "cljs-feature.txt" #{"clojure" "clojurescript" "better" "feature" "support" "like"}))
(generate-words (take 250 cljs-features) "cljs-features.words.txt")

(def cljs-weakness (process-file "cljs-weakness.txt" #{"clojure" "clojurescript" "time" "startup" "lack" "like" "can"}))
(generate-words (take 250 cljs-weakness) "cljs-weakness.words.txt")

(def cljs-general (process-file "cljs-general.txt" #{"clojure" "clojurescript" "language"}))
(generate-words (take 250 cljs-general) "cljs-general.words.txt")
