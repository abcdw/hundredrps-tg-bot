(ns hundredrps.core
  (:gen-class)
  (:require
   [aero.core :as aero]
   [clojure.java.io :as io]
   [hundredrps.cards :as cards]
   [hundredrps.tg :as tg]
   [hundredrps.pdf :as pdf]
   [hundredrps.utils :as utils]
   [integrant.core :as ig]
   [org.httpkit.client :as http]
   [org.httpkit.server :as http-kit]
   [clojure.string]
   [malli.core :as m])
  (:import java.util.zip.ZipInputStream))

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

;; (file-seq (io/file (io/resource "cards/fonts")))

(defrecord HELPER [])

(defn get-code-location []
  (when-let [src (.getCodeSource (.getProtectionDomain HELPER))]
    (.getLocation src)))

(defn list-zip-contents [zip-location]
  (with-open [zip-stream (ZipInputStream. (.openStream zip-location))]
    (loop [dirs []]
      (if-let [entry (.getNextEntry zip-stream)]
        (recur (conj dirs (.getName entry)))
        dirs))))

(defn get-files-in-directory
  [dir]
  (if (= "jar" (.getProtocol (io/resource "config.edn")))
    (some->> (get-code-location)
             list-zip-contents
             (remove #(clojure.string/ends-with? % "/"))
             (filter #(clojure.string/starts-with? % dir)))
    (map #(str dir "/" %) (seq (. (io/file (io/resource dir)) list)))))

(defn load-files-from-dir
  "Load files into memory as byte-arrays."
  [dir]
  (->> (get-files-in-directory dir)
       (map (fn [x]
              (let [res  (io/resource x)
                    name (keyword (clojure.string/replace x #".*/" ""))]
                [name (utils/load-file-as-byte-array res)])))
       (into {})))

(defmethod aero/reader 'cards/resource-dir
  [_ tag value]
  (load-files-from-dir value))

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

(defmethod ig/init-key :db/value [_ val] (atom {}))

(defmethod ig/init-key :cards/resources [_ val]
  (-> (update-in val [:fonts] load-files-from-dir)
      (update-in [:pdfs] #(update-vals % load-files-from-dir))))

(defmethod ig/init-key :pdf/templates [_ val] val)

(defmethod ig/init-key :pdf/cards [_ val] val)

(defmethod ig/init-key :text/templates [_ val] val)

(defmethod ig/init-key :amplitude/api-token [_ val] val)
(defmethod ig/init-key :amplitude/api-url [_ val] val)
(defmethod ig/init-key :analytics/enabled? [_ val] val)

(defmethod ig/init-key :handler/webhook [_ ctx]
  (cards/get-handler-new ctx))

(defmethod ig/init-key :chat/config [_ val] val)

(defmethod ig/init-key :chat/registry [_ {:chat/keys [config] :as ctx}]
  (cards/get-registry config))

(defmethod ig/init-key :chat/logic [_ {:chat/keys [config registry] :as ctx}]
  (cards/prepare-chat-logic config registry))

(defmethod ig/init-key :payment/config [_ val] val)

(defmethod ig/init-key :pdf/generator [_ val]
  (fn [data card] (pdf/generate-pdf val data card)))

(defn get-config
  "Read integrant system description from config.edn."
  ([] (get-config nil))
  ([profile]
   (aero/read-config (io/resource "config.edn") {:profile (or profile :dev)})))

;; TODO: Add text unicode cleanup tg:telega:@rus_garifullin#875070
;; translation: https://github.com/ptaoussanis/tempura

(defn -main
  "Entry point."
  [& args]
  (ig/init (get-config :default))
  (println "hundredrps is on duty."))
