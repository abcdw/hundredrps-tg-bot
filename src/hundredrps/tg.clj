(ns hundredrps.tg)

(defn get-message
  "Get message from update."
  [upd]
  (if (get upd :edited_message)
    (get upd :edited_message)
    (get upd :message)))

(defn get-chat-id
  "Extracts chat-id from update."
  [upd]
  (get-in (get-message upd) [:chat :id]))

(defn get-photo-file-id
  "Get file id of the photo with the best resolution."
  [upd]
  (-> upd get-message :photo last :file_id))
