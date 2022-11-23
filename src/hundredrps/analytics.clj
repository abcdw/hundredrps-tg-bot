(ns hundredrps.analytics
  (:require [org.httpkit.client :as http])
  (:require [jsonista.core :as j]))

(defn send-analytics
  "Takes amplitude `api-url` and `api-token`
   user-id - Telegram user id
   event-type - current bot state
   event-properties - error or success with params"
  [{:amplitude/keys [api-url api-token]} user-id event-type event-properties]
  (let [amp-event {:user_id          user-id
                   :event_type       event-type
                   :platform         "Telegram"
                   :user_properties  {}
                   :event_properties event-properties}]
    (http/request
     {:url    api-url
      :method :post
      :body   (j/write-value-as-string {:api_key api-token :events [amp-event]})})))
