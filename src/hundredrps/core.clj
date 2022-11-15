(ns hundredrps.core
  (:gen-class)
  (:require
   [aero.core :as aero]
   [clojure.java.io :as io]
   [hundredrps.cards :as cards]
   [hundredrps.tg :as tg]
   [integrant.core :as ig]
   [org.httpkit.client :as http]
   [org.httpkit.server :as http-kit]
   [malli.core :as m]))

(defn sum [a b]
  "Can be used for writing a simple test."
  (+ a b))



(defmethod aero/reader 'ig/ref
  [_ tag value]
  (ig/ref value))

(defmethod aero/reader 'm/parser
  [_ tag value]
  (let [parser (m/parser value)]
    (fn [x]
      (let [result (parser x)]
        (if (= ::m/invalid result) nil result)))))

(defmethod aero/reader 'tg/message-text
  [_ tag value]
  [:map [:message [:orn [[:text] [:map [:text [:and :string value]]]]]]])

(defmethod aero/reader 'tg/message-text-0
  [_ tag value]
  [:map [:message [:orn [[:text 0] [:map [:text [:and :string value]]]]]]])

(defmethod aero/reader 'tg/message-maybe-text
  [_ tag [maybe text]]
  [:map [:message
         [:orn
          [[:text 0]
           [:map [:text [:and :string
                         [:orn [nil maybe]]]]]]
          [[:text]
           [:map [:text [:and :string text]]]]]]])

(defmethod aero/reader 'cost->amount
  [_ tag value]
  (* 100 value))


(defmethod ig/init-key :tg/api-token [_ val] val)
(defmethod ig/init-key :tg/api-url [_ {:keys [base-url api-token]}]
  (str base-url api-token))
(defmethod ig/init-key :tg/file-url [_ {:keys [base-url api-token]}]
  (str base-url api-token))

(defmethod ig/init-key :http/server [_ {:keys [handler] :as opts}]
  (http-kit/run-server handler (-> opts
                                   (dissoc :handler)
                                   (assoc :legacy-return-value? false))))

(defmethod ig/halt-key! :http/server [_ server]
  (http-kit/server-stop! server))

(defmethod ig/init-key :db/value [_ val] (atom val))

(defmethod ig/init-key :handler/webhook [_ {:keys [api-url file-url db] :as ctx}]
  (cards/get-handler ctx))

(defn get-config
  "Read integrant system description from config.edn."
  []
  (aero/read-config (io/resource "config.edn")))

(defn -main
  "Entry point."
  [& args]
  (ig/init (get-config))
  (println "hundredrps is on duty."))
