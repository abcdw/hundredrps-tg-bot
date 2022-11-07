(ns hundredrps.cards
  (:require [hundredrps.tg :as tg]
            [jsonista.core :as j]
            [malli.core :as m]))

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

;; https://github.com/metosin/malli#built-in-schemas

(defn update-state-value
  [value upd & [step]]
  (let [{:keys [text photo]} (deconstruct-update upd)]
    (merge {:step step} value {:value (or text photo)})))

(defn try-to-make-step
  [state steps upd]
  (let [msg (tg/get-message upd)
        msg-id (tg/get-message-id upd)
        edited-message (:edited_message upd)
        chat-id (tg/get-chat-id upd)

        step (if edited-message
               (get-in state [:values msg-id :step])
               (:step state))

        {:keys [transitions]} (get steps step)

        result (m/parse transitions msg)

        matched? (not= ::m/invalid result)]

    (cond-> state
      edited-message
      (assoc :reply
             (if matched?
               (tg/send-text-message
                (tg/get-chat-id upd)
                "Мы поредактировали чё вы хотели.")
               (tg/send-text-message
                (tg/get-chat-id upd)
                "Сорян, попробуй ещё раз.")))

      (not edited-message)
      (assoc :reply
             (if (get-in result [0 :message])
               (merge {:chat_id chat-id :method "sendMessage"}
                      (get-in result [0 :message]))
               (tg/send-text-message
                (tg/get-chat-id upd)
                (with-out-str
                  (clojure.pprint/pprint result)))))

      (and matched? (not edited-message))
      (assoc :step (get-in result [0 :to]))

      matched?
      (update-in [:values (tg/get-message-id upd)]
                 update-state-value upd step))))

(defn process-update
  [state steps upd]
  (let [chat-id      (tg/get-chat-id upd)
        message-type (tg/get-message-type upd)
        message-id   (tg/get-message-id upd)
        text         (or (:text (tg/get-message upd))
                         "No text in previous message.")

        new-state (-> state
                      (update :updates #(if % (conj % upd) [upd]))
                      (try-to-make-step steps upd))

        reply (:reply new-state)]
    {:reply reply
     :state (dissoc new-state :reply)}))

(defn get-handler
  "Returns a function, which process http requests."
  [{:keys [api-token db]}]
  (fn [{:keys [body] :as request}]
    (let [input (j/read-value body j/keyword-keys-object-mapper)

          chat-id (tg/get-chat-id input)

          steps (get-in @db [:logic :steps])

          {:keys [reply state]}
          (process-update (get-in @db [chat-id :state] {}) steps input)

          _ (swap! db assoc-in [chat-id :state] state)

          reply-body (j/write-value-as-string reply)]

      {:status  200
       :headers {"Content-Type"   "application/json"
                 "Content-Length" (count reply-body)}
       :body    reply-body})))
