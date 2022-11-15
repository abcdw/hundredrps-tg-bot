(ns user
  (:require
   [clojure.java.io :as io]
   [hundredrps.cards :as cards]
   [hundredrps.core :refer [get-config]]
   [integrant.repl]
   [jsonista.core :as j]
   [org.httpkit.client :as http]
   [clojure.pprint]))

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
  (-> "assets/01-letter-for-mother-with-photos-and-errors.edn"
      io/resource io/file slurp read-string
      emulate-requests))

(def updates
  (-> "assets/00-letter-for-mother.edn"
      io/resource io/file slurp read-string))

(defn to-request
  [upd]
  {:body (j/write-value-as-string upd)})

(def url
  "http://hundredrps.project.trop.in:50080"
  ;; "http://localhost:8080"
  )

(defn make-async-request
  [upd]
  (http/post url (to-request upd)))

(defn get-updates-series [_]
  (let [new-id (rand-int 5000)]
    (mapv #(cond-> %
             (:message %)
             (assoc-in [:message :chat :id] new-id)
             (:pre_checkout_query %)
             (assoc-in [:pre_checkout_query :from :id] new-id))
          updates)))

(defn reset []
  (integrant.repl/reset)
  (pass-letter-for-mother))

(defn persist-updates [file]
  (spit (io/resource file)
        (with-out-str
          (clojure.pprint/pprint
           (get-in (get-db) [67562087 :state :updates])))))

(comment
  (persist-updates "assets/01-letter-for-mother-with-photos-and-errors.edn")
  (let [series-count 50]
    (dotimes [n 40]
      (Thread/sleep 500)
      (time
       (doall
        (map make-async-request
             (apply concat
                    (map get-updates-series (range series-count))))))))
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
