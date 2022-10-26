(ns hundredrps.core
  (:gen-class)
  (:require
   [aero.core :as aero]
   [clojure.java.io :as io]
   [integrant.core :as ig]
   [jsonista.core :as j]
   [org.httpkit.client :as http]
   [org.httpkit.server :as http-kit]))

(defn -main
  "Entry point."
  [& args]
  (println "hi"))

(defn sum [a b]
  "Can be used for writing a simple test."
  (+ a b))




(defmethod aero/reader 'ig/ref
  [_ tag value]
  (ig/ref value))

(defmethod ig/init-key :tg/token [_ val] val)

(defmethod ig/init-key :http/server [_ {:keys [handler] :as opts}]
  (http-kit/run-server handler (-> opts
                                   (dissoc :handler)
                                   (assoc :legacy-return-value? false))))

(defmethod ig/halt-key! :http/server [_ server]
  (http-kit/server-stop! server))

(defmethod ig/init-key :handler/webhook [_ {:keys [api-token]}]
  (fn [{:keys [body]}]
    (let [input (j/read-value body j/keyword-keys-object-mapper)
          {{{chat-id :id} :chat
            text          :text} :message} input
          reply-msg '()]

      (->
       @(http/post
         (str "https://api.telegram.org/bot" api-token "/sendMessage")
         {:form-params {:chat_id chat-id
                        :text    text}})
       println)
      {:status  200
       :headers {"Content-Type" "text/plain"}
       :body    (str "Hey")})))

(defn get-config
  "Read integrant system description from config.edn."
  []
  (aero/read-config (io/resource "config.edn")))
