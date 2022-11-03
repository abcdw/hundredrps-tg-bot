(ns hundredrps.cards
  (:require [hundredrps.tg :as tg]))

(defn deconstruct-update
  "Extract message, message-type and possible photo and text from
  update."
  [upd]
  (let [message      (tg/get-message upd)
        message-type (tg/get-message-type upd)]
    {:message-type message-type
     :message      message
     :text         (:text message)
     :photo        (tg/get-photo-file-id message)}))

(defn update-state-value
  [value upd]
  (let [{:keys [text photo]} (deconstruct-update upd)]
    (merge value {:value (or text photo)})))

(defn process-update
  [state upd]
  (let [chat-id      (tg/get-chat-id upd)
        message-type (tg/get-message-type upd)
        message-id   (tg/get-message-id upd)
        text         (or (:text (tg/get-message upd))
                         "No text in previous message.")

        new-state (-> state
                      (update :updates #(if % (conj % upd) [upd]))
                      (update-in [:values message-id] update-state-value upd))

        reply (tg/send-text-message chat-id text)]
    {:reply reply
     :state new-state}))
