(ns hundredrps.text
  (:require [clojure.java.io :as io]
            [malli.core :as m]))

;; (defmethod ig/init-key :text/templates [_ val]
;;   val)

(defn fill-text-template
  [{:keys [data] :as ctx} template]
  (reduce
   (fn [acc [pattern path]]
     (if (m/validate pattern data)
       (conj acc (get-in ctx path))
       acc))
   []
   template))

(defn eval-template
  [ctx template]
  (apply format (fill-text-template ctx template)))

(defn eval-templates
  [{:keys [text-templates] :as ctx}]
  (->> #(eval-template ctx %)
       (update-vals text-templates)))

(defn get-texts
  [system data card]
  (->
   {:data           data
    :strings        (get-in system [:cards/resources :strings card])
    :text-templates (get-in system [:text/templates card])}
   eval-templates))
