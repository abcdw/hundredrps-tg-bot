(ns hundredrps.cards
  (:require [hundredrps.tg :as tg]
            [org.httpkit.client :as http]
            [jsonista.core :as j]
            [malli.core :as m]))

(defn map->json-http-request
  "Convert map to json, and then wrap in ring form so it can be used in
  POST query."
  [x]
  (let [body (j/write-value-as-string x)]
    {:headers {"Content-Type"   "application/json"
               "Content-Length" (count body)}
     :body    body}))

(defn msg->http-response
  "Convert msg to json and wrap in ring form, uses sendMessage if no
  method specified."
  [msg]
  (-> (merge {:method "sendMessage"} msg)
      map->json-http-request
      (assoc  :status 200)))

(defn async-call [api-token x & [callback]]
  (let [{:keys [method] :or {method "sendMessage"}} x

        url (str "https://api.telegram.org/bot" api-token "/" method)
        request (-> x (dissoc :method) map->json-http-request)]
    (http/post url request callback)))

(defn deconstruct-update
  "Extract message, message-type and possible photo and text from
  update."
  [upd]
  (let [message      (tg/get-payload upd)
        message-type (tg/get-message-type upd)]
    {:message-type message-type
     :message      message
     :text         (:text message)
     :photo        (tg/get-photo-file-id message)}))

;; https://github.com/metosin/malli#built-in-schemas

(defn values->pdf-data
  "Generates data for pdf from values extracted from tg updates."
  [values]
  (reduce (fn [acc [msg-id {:keys [step value]}]]
            (assoc-in acc step value))
          ;; Sort is needed to make sure that messages with lower id
          ;; processed earlier
          {} (sort values)))

(defn update-state-value
  [old-value parsed-upd & [step]]
  (let [[path payload] (tg/get-payload parsed-upd)]
    (merge {:step step} old-value
           (when (and path payload)
             {:value (get-in payload path)}))))

(defn try-to-make-step
  [state logic upd]
  (let [msg            (tg/get-payload upd)
        msg-id         (tg/get-message-id upd)
        edited-message (:edited_message upd)
        chat-id        (tg/get-chat-id upd)

        step (if edited-message
               (get-in state [:values msg-id :step])
               (:step state))

        global-menu (get-in logic [:global-menu :transitions])
        transitions (get-in logic [:steps step :transitions])

        [{:keys [data-mapper] :as result} parsed-upd]
        (or
         (global-menu upd)
         (transitions upd))

        parsed-correctly? (vector? (tg/get-payload parsed-upd))

        mapper-ctx  {:state state
                     :upd   upd
                     :parsed-upd parsed-upd}
        msg-updater #(if data-mapper
                       (reduce
                        (fn [acc [from to]]
                          (->> (get-in mapper-ctx from)
                               (assoc-in acc to)))
                               % data-mapper)
                       %)]

    (if parsed-correctly?
      (cond-> state
        edited-message
        (assoc :replies
               [(if result
                  (tg/send-text-message
                   (tg/get-chat-id upd)
                   "Мы поредактировали чё вы хотели.")
                  (tg/send-text-message
                   (tg/get-chat-id upd)
                   "Сорян, попробуй ещё раз."))])

        (not edited-message)
        (assoc :replies
               (if (:messages result)
                 (msg-updater (:messages result))
                 [(tg/send-text-message
                   (tg/get-chat-id upd)
                   (with-out-str
                     (clojure.pprint/pprint result)))]))

        (and result (contains? result :to) (not edited-message))
        (assoc :step (:to result))

        (and result (tg/get-message-id upd))
        (update-in [:values (tg/get-message-id upd)]
                   update-state-value parsed-upd step))

      (do
        (println "parsed incorrectly")
        state))))

(defn process-update
  [state logic upd]
  (let [new-state (-> state
                      (update :updates #(if % (conj % upd) [upd]))
                      (try-to-make-step logic upd))

        replies (:replies new-state)]
    {:replies replies
     :state (dissoc new-state :replies)}))

(defn get-handler
  "Returns a function, which process http requests.
  `silent?` control if handler send messages at all it useful to setup
  the needed state before testing. `verbose?` forces handler to reply
  always with api call rather than return value to webhook."
  [{:keys [api-token db silent? verbose?]}]
  (fn [{:keys [body] :as request}]
    (let [input (j/read-value body j/keyword-keys-object-mapper)

          chat-id (tg/get-chat-id input)

          logic (get-in @db [:logic])

          {:keys [state] :as result}
          (process-update (get-in @db [chat-id :state] {}) logic input)

          replies (map #(assoc % :chat_id chat-id) (:replies result))
          _ (swap! db assoc-in [chat-id :state] state)]

      (println "chat-id ================> " chat-id)
      ;; (println "replies=> " replies)

      (if-not silent?
        (if (and (= 1 (count replies)) (not verbose?))
          (msg->http-response (first replies))
          (do
            (doall (map #(deref (async-call api-token %)) replies))
            {:status 200}))))))
