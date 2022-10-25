(ns hundredrps.core
  (:gen-class))

(defn -main
  "Entry point."
  [& args]
  (println "hi"))

(defn sum [a b]
  (+ a b))

(require '[jsonista.core :as j])
(j/write-value-as-string {:dog {:name "Teppo"}})
