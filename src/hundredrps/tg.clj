(ns hundredrps.tg
  (:require [malli.core :as m]
            [malli.experimental.lite :as l]
            [malli.registry :as mr]
            [malli.util :as mu]))


;;; Schemas

(def registry
  (merge
   (m/default-schemas)
   (mu/schemas)
   {:telegram/photo-size
    [:map
     [:file_id :string]
     [:file_unique_id :string]
     [:width :int]
     [:height :int]
     [:file_size {:optional true} :int]]

    :telegram/successful-payment
    [:map
     [:currency :string]
     [:total_amount :int]
     [:invoice_payload :string]]

    :telegram/user
    [:map
     [:id :int]
     [:is_bot :boolean]
     [:first_name :string]
     [:first_name {:optional true} :string]
     [:language_code {:optional true} :string]]

    :telegram/chat
    [:map
     [:id :int]
     [:type :string]]

    :telegram/pre-checkout-query
    [:map
     [:id :string]
     [:currency :string]
     [:from :telegram/user]
     [:invoice_payload :string]]

    :telegram/message
    [:map
     [:message_id :int]
     [:text {:optional true} :string]
     [:photo {:optional true} [:vector :telegram/photo-size]]
     [:successful_payment {:optional true} :telegram/successful-payment]]

    :telegram/edited_message :telegram/message

    :telegram/update
    [:or
     [:map [:message :telegram/message]]
     [:map [:edited_message :telegram/edited_message]]]

    :telegram/chat-id [:or :int :string]
    :telegram/labeled-price [:map
                             [:label :string]
                             [:amount :int]]

    :telegram/send-message [:map [:text :string]]
    :telegram/send-photo [:map [:photo :string]]

    :telegram/send-invoice
    [:map
     [:title :string]
     [:description :string]
     [:payload :string]
     [:provider_token :string]
     [:currency :string]
     [:prices [:vector :telegram/labeled-price]]]

    :telegram/answer-pre-checkout-query
    [:map
     [:pre_checkout_query_id :string]
     [:ok :boolean]]

    :tg/outgoing-message
    [:merge
     [:map [:chat_id :telegram/chat-id]]
     [:or
      :telegram/send-message
      :telegram/send-photo
      :telegram/send-invoice
      :telegram/answer-pre-checkout-query]]

    :tg/message-base [:map [:message_id :int]]
    :tg/text         [:merge :tg/message-base
                      [:map [:text :string]]]

    :tg/photo [:merge :tg/message-base
               [:map [:photo [:vector :telegram/photo-size]]]]

    :tg/message [:orn
                 [:text :tg/text]
                 [:photo :tg/photo]]

    :tg/update [:orn
                [:message [:map [:message :tg/message]]]
                [:edited_message [:map [:edited_message :tg/message]]]]}))

(def fast-registry
  (mr/fast-registry registry))


;;; Helpers

(defn get-payload
  "Get message from update."
  [upd]
  (->>
   (get upd :pre_checkout_query)
   (get upd :edited_message)
   (get upd :message)))

(defn get-chat-id
  "Extracts chat-id from update. It backups to from id, if chat entry
  isn't available."
  [upd]
  (let [payload (get-payload upd)]
    (->>
     (get-in payload [:from :id])
     (get-in payload [:chat :id]))))

(defn get-photo-file-id
  "Get file id of the photo with the best resolution."
  [upd]
  (-> upd get-payload :photo last :file_id))

(defn get-message-type
  "Get message type from update."
  [upd]
  (let [msg (-> upd get-payload)]
    (cond
      (:text msg) :text
      (:photo msg) :photo
      :else :unknown)))

(defn get-message-id
  "Get message id."
  [upd]
  (-> upd get-payload :message_id))

(defn file?
  "Checks if `x` is a telegram File object."
  [x]
  (and (map? x)
       (contains? x :file_id)))


;;; tg api

(defn send-message
  "Return a send-message request body."
  [ctx]
  (merge ctx {:method "sendMessage"}))

(defn send-text-message
  "A convinience wrapper around `send-message`."
  [chat-id text]
  (send-message {:chat_id chat-id :text text}))
