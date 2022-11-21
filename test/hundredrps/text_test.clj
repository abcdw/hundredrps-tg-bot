(ns hundredrps.text-test
  (:require
   [clojure.java.io :as io]
   [hundredrps.core]
   [integrant.core :as ig]))

(def form-data
  {:childhood-memories "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat.",
   :what-mom-cooked    nil,
   :payment            ":letter-for-mother",
   :maybe-edit         :ok,
   :signature          "Best regards",
   :add-warm-memories? true,
   :photo-with-mom     nil,
   :sibling            :son,
   :have-children?     false,
   :greeting           :understood,
   :live-together?     false})

(def template
  [[:any [:photo-separate]]
   [[:map [:sibling [:= :daugther]]] [:photo-separate-daughter]]
   [[:map [:sibling [:= :son]]] [:photo-separate-son]]])

(def texts
  (get-in (ig/init (hundredrps.core/get-config) [:text/templates])
          [:text/templates :letter-for-mother]))

;; (fill-text-template
;;  form-data strings template)

;; (get-texts integrant.repl.state/system form-data :letter-for-mother)
;; (eval-templates (merge texts {:data form-data}))
