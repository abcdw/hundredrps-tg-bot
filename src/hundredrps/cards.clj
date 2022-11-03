(ns hundredrps.cards
  (:require [hundredrps.tg :as tg]))

(defn process-update
  [state upd]
  (let [chat-id      (tg/get-chat-id upd)
        message-type (tg/get-message-type upd)
        text         (or (:text (tg/get-message upd))
                         "No text in previous message.")

        new-state (update state :updates #(if % (conj % upd) [upd]))

        reply (tg/send-text-message chat-id text)]
    {:reply reply
     :state new-state}))
