(ns user
  (:require [hundredrps.core :refer [get-config]]
   [clojure.java.io :as io]
   [hundredrps.cards :as cards]
   [jsonista.core :as j]
   [integrant.repl]
   [org.httpkit.client :as http]
   [clojure.pprint]))

(integrant.repl/set-prep! #(get-config))

(defn get-db []
  @(:db/value integrant.repl.state/system))

;; (defn emulate-requests
;;   [updates]
;;   (let [conf (select-keys integrant.repl.state/system
;;                           (keys (:handler/webhook integrant.repl.state/config)))

;;         verbose-handler (cards/get-handler (assoc conf :verbose? true))
;;         silent-handler  (cards/get-handler (assoc conf :silent? true))
;;         to-request      #(identity {:body (j/write-value-as-string %)})]
;;     (doall (map #(silent-handler (to-request %)) (butlast updates)))
;;     (verbose-handler (to-request (last updates)))))

;; (defn pass-letter-for-mother
;;   []
;;   (-> "assets/01-letter-for-mother-with-photos-and-errors.edn"
;;       io/resource io/file slurp read-string
;;       emulate-requests))

(def updates
  (-> ;; "assets/00-letter-for-mother.edn"
      "assets/01-letter-for-mother-with-photos-and-errors-new.edn"
      io/resource io/file slurp read-string))

(defn to-request
  [upd]
  {:body (j/write-value-as-string upd)})

(def url
  ;; "http://hundredrps.project.trop.in:50080"
  "http://localhost:8080"
  )

(defn set-webhook
  [{:tg/keys [api-url]}]
  (http/request
   {:url          (str api-url "/setWebhook")
    :query-params {:url "https://hundredrps.project.trop.in/cardsandcare/webhook"
                   :max_connections 100}
    :multipart    [{:name     "certificate"
                    :content  (io/file "keys/hundredrps.pem")
                    :filename "cert.pem"}]}))

(defn delete-webhook
  [{:tg/keys [api-url]}]
  (http/request {:method :post :url (str api-url "/deleteWebhook")}))

(defn get-webhook-info
  [{:tg/keys [api-url]}]
  (http/request {:method :post :url (str api-url "/getWebhookInfo")}))

(def tg (integrant.core/init (get-config) [:tg/api-url]))
(comment
  (def
    resp
    @(get-webhook-info tg)
    ;; (delete-webhook )
    @(set-webhook tg)
    ))

(defn make-async-request
  [upd & [callback]]
  (http/post url (to-request upd) callback))

(defn make-async-request-verbose
  [upd & [callback]]
  (http/post (str url "?verbose=true") (to-request upd) callback))

(defn make-async-request-silent
  [upd & [callback]]
  (http/post (str url "?silent=true") (to-request upd) callback))

(defn make-sync-request-verbose
  [upd & [callback]]
  @(http/post (str url "?verbose=true") (to-request upd) callback))

(defn make-sync-request-silent
  [upd & [callback]]
  @(http/post (str url "?silent=true") (to-request upd) callback))

(comment
  (let [cnt 22]
    (doall (map make-sync-request-silent (take (dec cnt) updates)))
    (make-sync-request-verbose (get-in updates [(dec cnt)]))
    ;; (doall (map make-sync-request-verbose (take (- (count updates) cnt) updates)))
    ))


(defn get-updates-series [_]
  (let [new-id (rand-int 5000)]
    (mapv #(cond-> %
             (:message %)
             (assoc-in [:message :chat :id] new-id)
             (:pre_checkout_query %)
             (assoc-in [:pre_checkout_query :from :id] new-id))
          updates)))

(defn make-consequent-async
  [updates]
  ;; (println "running cons requsets")
  ((reduce
    (fn [acc x]
      (fn [_] (make-async-request x acc)))
    (fn [_]
      ;; (println "series complete")
      "heh")
    (reverse updates)) {}))

(defn make-arbitrary-async
  [updates]
  ;; (println "running arbit requsets")
  (doall (map make-async-request updates)))

(defn load-test
  [{:keys [waves-count series-count consequent-percent]}]
  (dotimes [n waves-count] ; waves count
    (println "sending wave " n)
    (Thread/sleep 500)
    (dotimes [i series-count]
      (future
        (if (< consequent-percent (rand-int 100))
          (make-arbitrary-async (get-updates-series {}))
          (make-consequent-async (get-updates-series {})))))))

(comment
  (load-test {:waves-count        10
              :series-count       5 ;; ~burst (22 request per series)
              :consequent-percent 100})
  (load-test {:waves-count (* (* 60 2) 1) ;; ~time seconds/2
              :series-count 20 ;; ~burst (22 request per series)
              :consequent-percent 20 ;; ~generated pdfs
              }))
;; 12:03
;; 14:04
;; 105582

;; 8800
;; 8:57
;; 41000
;; 9:29


(defn reset []
  (integrant.repl/reset)
  ;; (pass-letter-for-mother)
  )

(defn persist-updates [file]
  (spit (io/resource file)
        (with-out-str
          (clojure.pprint/pprint
           (get-in (get-db) [67562087 :state :updates])))))

(comment
  (-> integrant.repl.state/system :chat/registry)

  (def file-to-send (io/file "new.pdf"))
  (def req
    (cards/send-pdf (:tg/api-url integrant.repl.state/system)
                    {:file    file-to-send
                     :chat-id 67562087}))
  (persist-updates "assets/01-letter-for-mother-with-photos-and-errors.edn")

  (pass-letter-for-mother)

  (def file-request-cache
    @(cards/async-call
      (:tg/api-url integrant.repl.state/system)
      {:method  "getFile"
       :file_id "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADeAADKwQ"}))

  (def photo-url
    (-> file-request-cache
        :body
        (j/read-value j/keyword-keys-object-mapper)
        :result :file_path
        (#(str "https://api.telegram.org/file/bot"
               (:tg/token integrant.repl.state/system) "/" %))))

  (def photo (cards/get-file
              (:tg/token integrant.repl.state/system)
              "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADeAADKwQ"))

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
