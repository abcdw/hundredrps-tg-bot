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
