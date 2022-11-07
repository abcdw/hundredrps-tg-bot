(ns hundredrps.cards
  (:require [hundredrps.tg :as tg]
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

(def start-message
  {:method "sendMessage"
   :text   "Выбери открытку /letterForMother, /bestWoman, /loveLetter."})

(defn message-template [text]
  {:method "sendMessage"
   :text   text})

(def steps
  {nil
   {:transitions
    [:orn
     [{:id      :start
       :to      :start
       :message start-message}
      [:map [:text [:and :string [:= "/start"]]]]]
     [{:id      :anything
       :to      :start
       :message start-message}
      [:map]]]}

   :start
   {:transitions
    [:orn
     [{:to      :letter-for-mother
       :message (message-template "Юхууу! вы выбрали открытку Письмо Маме.")}
      [:map [:text [:and :string [:= "/letterForMother"]]]]]
     [{:to      :start
       :message (message-template "Чёт не понял какая откртыка.")}
      [:map [:text :string]]]
     ;; [{:id      :best-huishes
     ;;   :to      :best-huishes
     ;;   :message (message-template "Юхууу! вы выбрали открытку бест хуишес.")}
     ;;  [:map [:text [:and :string [:= "/bestHuishes"]]]]]
     ]}

   :letter-for-mother
   {:transitions
    [:orn
     [{:to      nil
       :message (message-template "Всё насобирались.")}
      [:map [:text [:and :string [:= "/stop"]]]]]
     [{:to      :letter-for-mother
       :message (message-template "О, пасиб за сообщение, подумаю чё с ним делать.")}
      [:map]]
     ;; [{:id      :best-huishes
     ;;   :to      :best-huishes
     ;;   :message (message-template "Юхууу! вы выбрали открытку бест хуишес.")}
     ;;  [:map [:text [:and :string [:= "/bestHuishes"]]]]]
     ]}
   })

;; https://github.com/metosin/malli#built-in-schemas

(defn update-state-value
  [value upd & [step]]
  (let [{:keys [text photo]} (deconstruct-update upd)]
    (merge {:step step} value {:value (or text photo)})))

(defn try-to-make-step
  [state upd]
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
               (merge {:chat_id chat-id} (get-in result [0 :message]))
               (tg/send-text-message
                (tg/get-chat-id upd)
                (with-out-str
                  (clojure.pprint/pprint result)))))

      (and matched? (not edited-message))
      (assoc :step (get-in result [0 :to]))

      matched?
      (update-in [:values (tg/get-message-id upd)] update-state-value upd step))))

(defn process-update
  [state upd]
  (let [chat-id      (tg/get-chat-id upd)
        message-type (tg/get-message-type upd)
        message-id   (tg/get-message-id upd)
        text         (or (:text (tg/get-message upd))
                         "No text in previous message.")

        new-state (-> state
                      (update :updates #(if % (conj % upd) [upd]))
                      (try-to-make-step upd))

        reply (:reply new-state)]
    {:reply reply
     :state (dissoc new-state :reply)}))
