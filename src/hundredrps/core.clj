(ns hundredrps.core
  (:gen-class)
  (:require
   [org.httpkit.server :as http-kit]
   [aero.core :as aero]
   [clojure.java.io :as io]
   [integrant.core :as ig]))

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

  (fn [_]
    {:status  200
     :headers {"Content-Type" "text/plain"}
     :body    (str "Huey " name)}))
(defmethod ig/init-key :handler/webhook [_ {:keys [api-token]}]

(defn get-config
  "Read integrant system description from config.edn."
  []
  (aero/read-config (io/resource "config.edn")))
