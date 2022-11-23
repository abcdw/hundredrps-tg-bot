(ns hundredrps.cards
  (:require
   [clojure.java.io :as io]
   [clojure.string]
   [hundredrps.tg :as tg]
   [hundredrps.text :as text]
   [hundredrps.utils :as utils]
   [hundredrps.analytics :as analytics]
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
  (let [body (j/write-value-as-bytes x)]
    {:headers {"Content-Type"   "application/json"}
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
  "Takes `api-url` and `file-id` and return `ByteArray`."
  (->
   (async-call api-url {:method "getFile" :file_id file-id})
   deref
   :body (j/read-value j/keyword-keys-object-mapper)
   :result :file_path

   (#(str file-url "/" %))
   (http/get {:as :byte-array}) ;; coersion doesn't work
   deref
   :body
   utils/load-file-as-byte-array))

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

(defn values->raw-data
  [values]
  (reduce (fn [acc [msg-id {:keys [step value]}]]
            (assoc-in acc step value))
          ;; Sort is needed to make sure that messages with lower id
          ;; processed earlier
          {} (sort values)))

(defn lift-up-data
  [data]
  (update-vals data (fn [[p v]]
                      (if (vector? p) (get-in v p) p))))

(defn download-files
  [ctx data files]
  (let [download-file #(get-file ctx (:file_id %))]
    (reduce #(update-in %1 %2 download-file) data files)))


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
    (m/parser transitions {:registry registry})))

(defn get-registry
  [config]
  (let [schemas (merge
                 (get-in config [:general :schemas])
                 (apply into {}
                        (map :schemas (or (some-> config :cards vals) []))))
        reg     (merge tg/registry schemas)]
    ;; A little bit of magic to check schemas compiles
    (doall
      (update-vals
       schemas
       #(-> (m/schema % {:registry reg})
            (m/deref-all)
            (m/form))))
    (mr/fast-registry reg)))

(defmulti perform-action
  (fn [_ a] (:action a)))

(defmethod perform-action :to-step
  [ctx {:keys [step]}]
  (assoc-in ctx [:state :step] step))

(defmethod perform-action :clear-state
  [ctx _]
  (assoc-in ctx [:state] {}))

(defmacro assert-schema
  "Use this macro when you pine for the notation of your childhood"
  [schema val]
  `(assert
    (m/validate ~schema ~val {:registry tg/fast-registry})
    (m/explain ~schema ~val {:registry tg/fast-registry})))

(defmethod perform-action :extract-text
  [{:keys [update] :as ctx} _]
  (assert-schema :telegram/update update)
  (->>
   (get-in ctx [:update :message :text])
   (assoc-in ctx [:data :text])))

(defmethod perform-action :save-value
  [{:keys [update] :as ctx} {:keys [value-path]}]
  (assert-schema :telegram/update update)
  (assoc-in ctx [:state :values (tg/get-message-id (:update ctx))]
            {:step (get-in ctx [:data :step])
             :value (get-in ctx value-path)}))

(defmethod perform-action :save-text
  [ctx {:keys [cleanup-unicode?] :or {cleanup-unicode? true}}]
  (cond-> ctx
    cleanup-unicode? (update-in [:data :text] text/remove-some-unicode-symbols)

    true (perform-action {:action :save-value :value-path [:data :text]})))

(defmethod perform-action :extract-and-save-text
  [ctx _]
  (-> ctx
   (perform-action {:action :extract-text})
   (perform-action {:action :save-text})))

(defmethod perform-action :extract-photo
  [{:keys [update] :as ctx} _]
  (assert-schema :telegram/update update)
  (->>
   (some-> ctx :update :message :photo)
   (assoc-in ctx [:data :photo])))

(defmethod perform-action :extract-and-save-photo
  [ctx _]
  (-> ctx
   (perform-action {:action :extract-photo})
   (perform-action {:action :save-value :value-path [:data :photo]})))

(defmethod perform-action :add-message
  [{{:keys [chat-id]} :data :as ctx} {:keys [message]}]
  (update ctx :messages conj (merge {:chat_id chat-id} message)))

(defmethod perform-action :add-multiple-messages
  [ctx {:keys [messages]}]
  (reduce #(perform-action %1 {:action  :add-message
                               :message %2})
          ctx messages))

(defmethod perform-action :send-messages!
  [{:keys [messages send-messages-async?] :tg/keys [api-url] :as ctx} _]
  (assert-schema [:vector :tg/outgoing-message] messages)
  (let [f (fn [] (doall (map #(deref (async-call api-url %)) messages)))]
    (if send-messages-async? (future-call f) (f)))
  (assoc ctx :messages-sent? true))

(defmethod perform-action :make-response-from-message
  [{:keys [messages messages-sent?] :as ctx} _]
  (if (and (= 1 (count messages)) (not messages-sent?))
    (do
      (assert-schema :tg/outgoing-message (first messages))
      (assoc ctx :response (msg->http-response (first messages))))
    ctx))

(defmethod perform-action :noop [ctx _] ctx)

(defmethod perform-action :prepare-data
  [{:chat/keys [registry] :as ctx} {:keys [parse-schema-name]}]
  (let [card          (keyword (namespace parse-schema-name))
        parser        (m/parser parse-schema-name {:registry registry})
        prepared-data (-> (get-in ctx [:state :values])
                          values->raw-data
                          card
                          parser
                          lift-up-data)]
    (assoc-in ctx [:prepared-data] prepared-data)))

(defmethod perform-action :download-file!
  [ctx {:keys [path]}]
  (if (tg/file? (get-in ctx path))
    (download-files ctx ctx [path])
    ctx))

(defmethod perform-action :map-data
  [ctx {:keys [mappings]}]
  (reduce
   (fn [acc [from to]]
     (->> (get-in acc from)
          (assoc-in acc to)))
   ctx mappings))

(defn get-start-parameter
  [text]
  (clojure.string/replace text #"/start\s*" ""))

(defmethod perform-action :send-analytics-async!
  [{:analytics/keys [enabled?]
    {:keys [chat-id step]} :data
    :as ctx} _]
  (when enabled?
    (let [maybe-text (get-in ctx [:update :message :text])
          start-regex #"/start.*"
          props (if (and maybe-text (re-find start-regex maybe-text))
                  {:start (get-start-parameter maybe-text)}
                  {})]
      (analytics/send-analytics ctx chat-id step props)))
  ctx)

(defmethod perform-action :format-string
  [ctx {:keys [path values-path values-keys]}]
  (let [format-string (get-in ctx path)
        format-values (get-in ctx values-path)
        format-arguments ((apply juxt values-keys) format-values)]
    (assoc-in ctx path (apply format format-string format-arguments))))

;; TODO: check config uses correct actions
;; (m/validate (into [:enum] (keys (methods perform-action))) :add-message)

(def default-actions
  ;; TODO: Move to chat config
  [{:action :make-response-from-message}
   {:action :send-analytics-async!}])

(defn eval-update
  [ctx chat-logic]
  (let [[_ [actions parsed-ctx]] (chat-logic ctx)]
    (reduce #(perform-action %1 %2) ctx (into actions default-actions))))

(def keys-to-forward-to-chat-context
  [:tg/api-url :tg/file-url :chat/registry :payment/config :analytics/enabled?])

(defn prepare-chat-context
  [ctx update chat-state]
  (merge
   (select-keys ctx keys-to-forward-to-chat-context)
   {:messages []
    :data     {:chat-id (tg/get-chat-id update)
               :step    (get-in chat-state [:step])}
    :update   update
    :state    chat-state}))

(defn process-update-new
  [ctx update]
  (let [db (:db/value ctx)

        chat-id    (tg/get-chat-id update)
        chat-state (get-in @db [chat-id :state] {})
        chat-logic (get-in ctx [:chat/logic])

        chat-context (prepare-chat-context
                      ctx update chat-state)

        {:keys [state response] :as result}
        (eval-update chat-context chat-logic)

        _ (swap! db assoc-in [chat-id :state] state)

        response (merge {:status 200} response)]
    response))

(defn get-handler-new
  "Returns a function, which process http requests.
  `silent?` control if handler send messages at all it useful to setup
  the needed state before testing. `verbose?` forces handler to reply
  always with api call rather than return value to webhook."
  [{:keys    [silent? verbose?]
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
