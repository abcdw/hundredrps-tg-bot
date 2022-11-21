(ns hundredrps.cards
  (:require
   [clojure.java.io :as io]
   [hundredrps.tg :as tg]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.registry :as mr]
   [malli.util :as mu]
   [org.httpkit.client :as http]))


;;; Helpers

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

(defn async-call [api-url x & [callback]]
  (let [{:keys [method] :or {method "sendMessage"}} x

        url (str api-url "/" method)
        request (-> x (dissoc :method) map->json-http-request)]
    (http/post url request callback)))

(defn send-pdf
  [api-url & {:keys [name chat_id file] :or {name "card.pdf"}}]
  (http/request
   {:url          (str api-url "/sendDocument")
    :method       :post
    :query-params {"chat_id" chat_id}
    :multipart    [{:name         "document"
                    :filename     name
                    :content-type "application/pdf"
                    :content      file}]}))

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

(defn get-file [{:tg/keys [api-url file-url]} file-id]
  "Takes `api-url` and `file-id` and return `BufferedInputStream`."
  (->
   (async-call api-url {:method "getFile" :file_id file-id})
   deref
   :body (j/read-value j/keyword-keys-object-mapper)
   :result :file_path

   (#(str file-url "/" %))
   (http/get {:as :byte-array})
   deref
   :body))

(defn values->pdf-data
  "Generates data for pdf from values extracted from tg updates."
  [ctx values]
  (reduce (fn [acc [msg-id {:keys [step value]}]]
            (assoc-in acc step (if (tg/file? value)
                                 (get-file ctx (:file_id value))
                                 value)))
          ;; Sort is needed to make sure that messages with lower id
          ;; processed earlier
          {} (sort values)))


;;; Chat

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

        values-extracted? (vector? (tg/get-payload parsed-upd))

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

    (if values-extracted?
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
                   (with-out-str (println result)))]))

        (and result (contains? result :to) (not edited-message))
        (assoc :step (:to result))

        (and result (tg/get-message-id upd))
        (update-in [:values (tg/get-message-id upd)]
                   update-state-value parsed-upd step))

      (do
        (println "Values not extracted")
        (assoc state :replies (get-in logic [:messages :wrong-input]))))))

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
  [{:keys    [silent? verbose?]
    :tg/keys [api-url file-url]
    :as      ctx}]
  (let [stats (atom {:request-count 0})]
    (fn [{:keys [body] :as request}]
      (let [input (j/read-value body j/keyword-keys-object-mapper)

            chat-id (tg/get-chat-id input)

            db    (:db/value ctx)
            logic (get-in @db [:logic])

            {:keys [state] :as result}
            (process-update (get-in @db [chat-id :state] {}) logic input)

            replies (map #(assoc % :chat_id chat-id) (:replies result))
            _       (swap! db assoc-in [chat-id :state] state)]

        ;; (println "update-id ================> " (:update_id input))
        ;; (swap! stats update :request-count inc)
        ;; (println (:request-count @stats))
        ;; (clojure.pprint/pprint input)
        ;; (clojure.pprint/pprint replies)


        (if-not silent?
          (if (and (= 1 (count replies)) (not verbose?))
            (msg->http-response (first replies))
            (do
              (future
                (doall (map #(deref (async-call api-url %)) replies)))
              {:status 200})))))))

(defn prepare-chat-logic
  [config registry]
  (let [transitions
        (->> (:cards config)
             (reduce (fn [acc [k v]] (conj acc [k (:transitions v)]))
                     [[:menu (get-in config [:general :menu])]])
             (into [:orn])
             (#(conj % [:fallback (get-in config [:general :fallback])])))]
    (m/parser transitions {:registry
                           (mr/fast-registry
                            (merge tg/registry registry))})))

(defn get-registry
  [config]
  (merge
   (get-in config [:general :schemas])
   (apply into {}
          (map :schemas (or (some-> config :cards vals) [])))))

(defmulti perform-action
  (fn [_ a] (:action a)))

(defmethod perform-action :to-step
  [ctx {:keys [step]}]
  (assoc-in ctx [:state :step] step))

(defmethod perform-action :extract-text
  [ctx _]
  (->>
   (get-in ctx [:update :message :text])
   (assoc-in ctx [:data :text])))

(defmethod perform-action :save-value
  [ctx {:keys [value-path]}]
  (assoc-in ctx [:state :values (tg/get-message-id (:update ctx))]
            (get-in ctx value-path)))

(defmethod perform-action :save-text
  [ctx {:keys [value-path]}]
  (perform-action ctx {:action :save-value :value-path [:data :text]}))

(defmethod perform-action :add-message
  [{{:keys [chat-id]} :data :as ctx} {:keys [message]}]
  (update ctx :messages conj (merge {:chat_id chat-id} message)))

(defmethod perform-action :add-messages
  [ctx {:keys [messages]}]
  (reduce #(perform-action %1 {:action  :add-message
                               :message %2})
          ctx messages))

;; TODO: check config uses correct actions
;; (m/validate (into [:enum] (keys (methods perform-action))) :add-message)

(defn process-update-new
  [ctx update]
  (let [db (:db/value ctx)

        chat-id       (tg/get-chat-id update)
        chat-state    (get-in @db [chat-id :state] {})
        chat-logic    (get-in ctx [:chat/logic])

        chat-context (prepare-chat-context
                      update chat-state chat-id)

        response {:status 200}]
    response))

(defn get-handler-new
  "Returns a function, which process http requests.
  `silent?` control if handler send messages at all it useful to setup
  the needed state before testing. `verbose?` forces handler to reply
  always with api call rather than return value to webhook."
  [{:keys    [silent? verbose?]
    :tg/keys [api-url file-url]
    :as      ctx}]
  (let [stats (atom {:request-count 0})]
    (fn [{:keys [body] :as request}]
      (let [update (j/read-value body j/keyword-keys-object-mapper)

            response
            (if (m/validate :telegram/update update
                            {:registry tg/fast-registry})
              (process-update-new ctx update)
              {:status 400})]

        response))))
