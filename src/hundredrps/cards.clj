(ns hundredrps.cards
  (:require
   [clojure.string]
   [clojure.pprint]
   [hundredrps.analytics :as analytics]
   [hundredrps.image :as image]
   [hundredrps.text :as text]
   [hundredrps.tg :as tg]
   [hundredrps.utils :as utils]
   [jsonista.core :as j]
   [malli.core :as m]
   [malli.registry :as mr]
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

(defn send-file
  [api-url method {:keys [chat_id file filename]} & [callback]]
  (http/request
   {:url          (str api-url "/" method)
    :method       :post
    :query-params {"chat_id" chat_id}
    :multipart    [{:name         ({"sendDocument" "document"
                                    "sendPhoto"    "photo"} method)
                    :filename     (or filename "1.jpg")
                    :content-type ({"sendDocument" "application/pdf"
                                    "sendPhoto"    "image/jpeg"} method)
                    :content      file}]}
   callback))

(defn async-call [api-url x & [callback]]
  (let [{:keys [method file filename chat_id] :or {method "sendMessage"}} x

        url     (str api-url "/" method)
        request (-> x (dissoc :method) map->json-http-request)]
    (if file
      (send-file api-url method {:chat_id  chat_id
                                 :file     file
                                 :filename filename} callback)
      (http/post url request callback))))

(defn send-pdf
  [api-url & {:keys [name chat_id file callback] :or {name "card.pdf"}}]
  (http/request
   {:url          (str api-url "/sendDocument")
    :method       :post
    :query-params {"chat_id" chat_id}
    :multipart    [{:name         "document"
                    :filename     name
                    :content-type "application/pdf"
                    :content      file}]}
   callback))

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
  [config payment-config]
  (let [schemas (merge
                 {:payment/free-ids (into [:enum] (:free-ids payment-config))}
                 (get-in config [:general :schemas])
                 (into {} (map :schemas (or (some-> config :cards vals) []))))
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
  [{{:keys [chat-id]} :data :as ctx} {:keys [message message-path-path]}]
  (let [message (if message-path-path
                  (get-in ctx (get-in ctx message-path-path))
                  message)]
    (update ctx :messages conj (merge {:chat_id chat-id} message))))

(defmethod perform-action :add-multiple-messages
  [ctx {:keys [messages]}]
  (reduce #(perform-action %1 {:action  :add-message
                               :message %2})
          ctx messages))

(defmethod perform-action :send-messages!
  [{:keys [messages send-messages-async? silent? silent2?]
    :tg/keys [api-url] :as ctx} _]
  (let [f (fn [] (doall (map #(deref (async-call api-url %)) messages)))]
    (when-not (or silent? silent2?)
      (assert-schema [:vector :tg/outgoing-message] messages)
      (if send-messages-async? (future-call f) (f))))
  (assoc ctx :messages-sent? true))

(defmethod perform-action :make-response-from-message
  [{:keys [messages messages-sent?] :as ctx} _]
  (if (and (= 1 (count messages)) (not messages-sent?))
    (do
      (assert-schema :tg/outgoing-message (first messages))
      (assoc ctx :response (msg->http-response (first messages))))
    ctx))

(defmethod perform-action :send-one-message!
  [{:keys [messages messages-sent?] :as ctx} _]
  (if (and (= 1 (count messages)) (not messages-sent?))
    (do
      (assert-schema :tg/outgoing-message (first messages))
      (perform-action ctx {:action :send-messages!}))
    ctx))

(defmethod perform-action :noop [ctx _] ctx)

(defmethod perform-action :get-raw-data
  [ctx _]
  (let [card     (get-in ctx [:data :step 0])
        raw-data (-> (get-in ctx [:state :values])
                     values->raw-data
                     card)]
    (assoc-in ctx [:raw-data] raw-data)))

(defmethod perform-action :prepare-data
  [{:chat/keys [registry] :as ctx} {:keys [parse-schema-name]}]
  (let [;; can be obtained from step
        card          (keyword (namespace parse-schema-name))
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

(defmethod perform-action :save-start-message
  [ctx _]
  (let [maybe-text    (get-in ctx [:update :message :text])
        start-regex   #"/start.*"
        original-text (get-in ctx [:state :start])
        start-text    (if (and maybe-text (re-find start-regex maybe-text))
                        (get-start-parameter maybe-text)
                        "")]
    (assoc-in ctx [:state :start] (or original-text start-text))))

(defmethod perform-action :send-analytics-async!
  [{:analytics/keys   [enabled?]
    {:keys [chat-id]} :data
    {:keys [step]}    :state
    :as               ctx} _]
  (when enabled?
    (let [props       {:start (get-in ctx [:state :start])}
          event-type  (if step (str step) "before-universe")]
      (analytics/send-analytics ctx chat-id event-type props)))
  ctx)

(defmethod perform-action :format-string
  [ctx {:keys [path values-path values-keys]}]
  (let [format-string (get-in ctx path)
        format-values (get-in ctx values-path)
        format-arguments ((apply juxt values-keys) format-values)]
    (assoc-in ctx path (apply format format-string format-arguments))))

(defmethod perform-action :is-photo-vertical?
  [ctx {:keys [path result-path] :or {result-path [:data :result]}}]
  (let [img       (get-in ctx path)
        vertical? (when img (image/vertical? img))]
    (assoc-in ctx result-path vertical?)))

(defmethod perform-action :send-pdf!
  [{{[default-card _] :step :keys [chat-id]} :data

    :keys     [prepared-data silent? silent2? send-messages-async?]
    :pdf/keys [generator]
    :tg/keys  [api-url]
    :as       ctx}
   {:keys [callback-message-path card filename]
    :or {card default-card filename "card.pdf"}}]
  (let [pdf-bytes (generator prepared-data card)
        cb-msg    (get-in ctx callback-message-path)
        msg       (when (and callback-message-path cb-msg)
                    (merge cb-msg {:chat_id chat-id}))
        callback  (fn [_] (async-call api-url msg))]
    ;; (.write (io/output-stream (io/file "new.pdf")))
    (when-not (or silent? silent2?)
      (if send-messages-async?
        (send-pdf api-url {:file     pdf-bytes :chat_id chat-id
                           :name     filename
                           :callback callback})
        (do
          @(send-pdf api-url {:file pdf-bytes :name filename :chat_id chat-id})
          @(callback {}))))
    ctx))

(defmethod perform-action :count
  [ctx {:keys [path result-path] :or {result-path [:data :count]}}]
  (assoc-in ctx result-path (count (get-in ctx path))))

(defmethod perform-action :string-replace
  [ctx {:keys [path match replacement]}]
  (->> (clojure.string/replace (get-in ctx path) (re-pattern match) replacement)
       (assoc-in ctx path)))

(defmethod perform-action :lower-case
  [ctx {:keys [path]}]
  (->> (clojure.string/lower-case (get-in ctx path))
       (assoc-in ctx path)))


(defmethod perform-action :assoc-in
  [ctx {:keys [path value]}]
  (assoc-in ctx path value))

(defmethod perform-action :if
  [{:chat/keys [registry] :as ctx}
   {:keys [result-path condition if-value-path else-value-path]
    :or {result-path [:data :if]}}]
  (assoc-in ctx result-path (if (m/validate condition ctx {:registry registry})
                              (get-in ctx if-value-path)
                              (get-in ctx else-value-path))))

;; TODO: check config uses correct actions
;; (m/validate (into [:enum] (keys (methods perform-action))) :add-message)

(def default-actions
  ;; TODO: Move to chat config
  [{:action :make-response-from-message}
   {:action :send-analytics-async!}])

(def default-actions-verbose
  ;; TODO: Move to chat config
  [{:action :send-one-message!}
   {:action :send-analytics-async!}])

(defn eval-update
  [{:keys [verbose?] :as ctx} chat-logic]
  (let [res (chat-logic ctx)]
    (if (= ::m/invalid res)
      (do
        (println "request is not parsed")
        (clojure.pprint/pprint ctx)
        ctx)
      (let [[_ [actions parsed-ctx]] res
            def-actions (if verbose? default-actions-verbose default-actions)]
        (reduce #(perform-action %1 %2) ctx (into actions def-actions))))))

(def keys-to-forward-to-chat-context
  [:tg/api-url :tg/file-url :chat/registry :payment/config :analytics/enabled?
   :pdf/generator :silent? :verbose? :cards/resources
   :amplitude/api-url :amplitude/api-token])

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
  "Returns a function, which process http requests."
  [{:stats/keys [config]
    :as         ctx}]
  (let [stats (atom {:request-count 0})]
    (fn [{:keys [body query-string] :as request}]

      ;; TODO: get silent? from query-params
      (let [update (j/read-value body j/keyword-keys-object-mapper)
            ctx    (cond-> ctx
                     (and query-string
                          (clojure.string/includes? query-string "verbose"))
                     (assoc :verbose? true)
                     (and query-string
                          (clojure.string/includes? query-string "silent"))
                     (assoc :silent? true))

            response
            (if (m/validate :telegram/update update
                            {:registry tg/fast-registry})
              (process-update-new ctx update)
              {:status 400})]

        (swap! stats update-in [:request-count] inc)
        (when (= 0 (mod (:request-count @stats) config))
          (swap! stats assoc-in [:user-count] (count @(:db/value ctx)))
          (println @stats))

        response))))

(defn get-updates
  [{:tg/keys [api-url] :as ctx} update-id]
  (->
   @(async-call api-url {:offset update-id :method "getUpdates"})
   :body
   (j/read-value j/keyword-keys-object-mapper)
   :result))

(defn get-polling-future
  [{:keys [verbose?] :as ctx}]
  (future
    (when verbose?
      (let [update-id (atom 0)]
        (loop []
          (doseq [update (get-updates ctx @update-id)]
            (clojure.pprint/pprint update)
            (process-update-new ctx update)
            (reset! update-id (inc (:update_id update))))
          (Thread/sleep 1000)
          (recur))))))
