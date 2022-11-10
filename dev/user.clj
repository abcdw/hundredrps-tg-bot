(ns user
  (:require
   [clojure.java.io :as io]
   [hundredrps.cards :as cards]
   [hundredrps.core :refer [get-config]]
   [integrant.repl]
   [jsonista.core :as j]))

(integrant.repl/set-prep! #(get-config))

(defn get-db []
  @(:db/value integrant.repl.state/system))

(defn emulate-requests
  [updates]
  (let [conf    {:api-token (:tg/token integrant.repl.state/system)
                 :db        (:db/value integrant.repl.state/system)}

        verbose-handler (cards/get-handler (assoc conf :verbose? true))
        silent-handler  (cards/get-handler (assoc conf :silent? true))
        to-request      #(identity {:body (j/write-value-as-string %)})]
    (doall (map #(silent-handler (to-request %)) (butlast updates)))
    (verbose-handler (to-request (last updates)))))

(defn pass-letter-for-mother
  []
  (-> "assets/00-letter-for-mother.edn"
      io/resource io/file slurp read-string
      emulate-requests))

(comment
  (pass-letter-for-mother)
  @(cards/async-call
    (:tg/token integrant.repl.state/system)
    {:method         "sendInvoice"
     :chat_id        67562087
     :title          "Цифровая открытка «Письмо маме»"
     :description    "Формат PDF файл"
     :payload        ":letter-for-mother"
     :provider_token "381764678:TEST:45134"
     :currency       "RUB"
     :prices         [{:label "«Письмо маме»" :amount 19900}]}))
