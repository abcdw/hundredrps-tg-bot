(ns hundredrps.tg)

(defn get-chat-id
  "Finds a chat-id in both plain messages and edited messages."
  [msg]
  (if (get msg :edited_message)
    (get-in msg [:edited_message :chat :id])
    (get-in msg [:message :chat :id])))
