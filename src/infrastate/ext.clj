(ns infrastate.ext
  (:require [clojure.data]
            [clojure.edn]
            [clojure.java.io]
            [clojure.pprint]
            [clojure.set]))

(defn from-file!
  "Useful interceptor in :before-all to load state from a file."
  [f]
  (fn [state] (merge state (clojure.edn/read-string (slurp f)))))

(defn to-file!
  "Useful hook in :do-after or :do-after-all to save state to a file"
  [f & [selector]]
  (fn [s]
    (clojure.pprint/pprint
     ((or selector :current) s) (clojure.java.io/writer f))))

(def print-diff-keys!
  "Useful hook in :do-after to print what has been changed by a resource-fn"
  (fn [{:keys [before after]}]
    (let [[went came _] (clojure.data/diff before after)
          went (set (keys went))
          came (set (keys came))
          both (clojure.set/intersection went came)]
      (doseq [k (clojure.set/union went came)]
        (condp contains? k
          both (println "UPD" k "(" (-> before k :state) "->" (-> after k :state) ")")
          went (println "REM" k)
          came (println "ADD" k))))))
