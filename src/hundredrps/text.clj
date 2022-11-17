(ns hundredrps.text
  (:require [clojure.java.io :as io]
            [malli.core :as m]))

;; (defmethod ig/init-key :text/templates [_ val]
;;   val)

(def form-data
  {:childhood-memories "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat.",
   :what-mom-cooked    nil,
   :payment            ":letter-for-mother",
   :maybe-edit         :ok,
   :signature          "Best regards",
   :add-warm-memories? true,
   :photo-with-mom     (hundredrps.utils/load-file-as-byte-array
                        (io/file (io/resource "face.jpg"))),
   :sibling            :son,
   :have-children?     false,
   :greeting           :understood,
   :live-together?     false})

(def template
  [[:any [:photo-separate]]
   [[:map [:sibling [:= :daugther]]] [:photo-separate-daughter]]
   [[:map [:sibling [:= :son]]] [:photo-separate-son]]])

(def texts
  (get-in integrant.repl.state/system
          [:cards/resources :texts :letter-for-mother]))

;; (fill-text-template
;;  form-data strings template)

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

;; (get-texts integrant.repl.state/system form-data :letter-for-mother)
;; (eval-templates (merge texts {:data form-data}))
