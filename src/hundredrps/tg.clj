(ns hundredrps.tg)

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
