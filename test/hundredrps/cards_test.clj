(ns hundredrps.cards-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing] :as t]
   [malli.core :as m]
   [malli.error :as me]
   [malli.experimental.lite :as ml]
   [hundredrps.cards :as cards]
   [hundredrps.tg :as tg]
   [hundredrps.core]
   [integrant.core :as ig]
   [jsonista.core :as j]
   [org.httpkit.fake :refer [with-fake-http]]))

(defmethod t/assert-expr 'valid
  [msg [_ schema data]]
  `(let [is-valid?# (m/validate ~schema ~data)]
     (t/do-report {:actual ~data
                   :expected (-> ~schema
                                 (m/explain ~data)
                                 (me/humanize))
                   :message ~msg
                   :type (if is-valid?# :pass :fail)})))

(defmethod t/assert-expr 'validl
  [msg [_ schema data]]
  `(let [is-valid?# (m/validate (ml/schema ~schema) ~data)]
     (t/do-report {:actual ~data
                   :expected (-> (ml/schema ~schema)
                                 (m/explain ~data)
                                 (me/humanize))
                   :message ~msg
                   :type (if is-valid?# :pass :fail)})))

(def raw-values-01-new
  {805 {:step nil, :value "/start"},
   832 {:step [:letter-for-mother :live-together?], :value "Раздельно"},
   836 {:step [:letter-for-mother :sibling], :value "От сына"},
   850 {:step  [:letter-for-mother :photo-with-mom],
        :value [{:file_id        "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADcwADKwQ",
                 :file_size      1386,
                 :file_unique_id "AQADCr4xG-BnoEp4",
                 :height         82,
                 :width          90}
                {:file_id        "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADbQADKwQ",
                 :file_size      9735,
                 :file_unique_id "AQADCr4xG-BnoEpy",
                 :height         291,
                 :width          320}
                {:file_id        "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADeAADKwQ",
                 :file_size      18397,
                 :file_unique_id "AQADCr4xG-BnoEp9",
                 :height         533,
                 :width          587}]},
   852 {:step [:letter-for-mother :what-mom-cooked], :value "moho   lho"},
   854 {:step [:letter-for-mother :have-children?], :value "Нет"},
   856 {:step  [:letter-for-mother :childhood-memories],
        :value "childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much, almost 300 hundreds."},
   858 {:step [:letter-for-mother :add-warm-memories?], :value "Да"},
   860 {:step [:letter-for-mother :signature], :value "Best regards"}})

(def raw-data-01-new
  {:add-warm-memories? "Да",
   :childhood-memories "childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much, almost 300 hundreds.",
   :have-children?     "Нет",
   :live-together?     "Раздельно",
   :photo-with-mom     [{:file_id        "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADcwADKwQ",
                         :file_size      1386,
                         :file_unique_id "AQADCr4xG-BnoEp4",
                         :height         82,
                         :width          90}
                        {:file_id        "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADbQADKwQ",
                         :file_size      9735,
                         :file_unique_id "AQADCr4xG-BnoEpy",
                         :height         291,
                         :width          320}
                        {:file_id        "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADeAADKwQ",
                         :file_size      18397,
                         :file_unique_id "AQADCr4xG-BnoEp9",
                         :height         533,
                         :width          587}],
   :sibling            "От сына",
   :signature          "Best regards",
   :what-mom-cooked    "moho   lho"})

(def parsed-data-01-new
  {:add-warm-memories? true,
   :childhood-memories "childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much, almost 300 hundreds.",
   :have-children?     false,
   :live-together?     false,
   :photo-with-mom     {:file_id        "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADeAADKwQ",
                        :file_size      18397,
                        :file_unique_id "AQADCr4xG-BnoEp9",
                        :height         533,
                        :width          587},
   :sibling            :son,
   :signature          "Best regards",
   :what-mom-cooked    "moho   lho"})

(deftest letter-for-mother-new
  (testing "Dialog with photo and errors."
    (def updates (-> "assets/01-letter-for-mother-with-photos-and-errors-new.edn"
                     io/resource io/file slurp read-string))
    (let [system (ig/init (hundredrps.core/get-config)
                          (conj
                           cards/keys-to-forward-to-chat-context
                           :chat/logic))

          {:chat/keys [registry]
           :tg/keys   [api-url file-url]} system

          chat-id    (tg/get-chat-id (get-in updates [0]))
          chat-logic (:chat/logic system)

          file-name         "file_1.jpg"
          file-path         (str "photos/" file-name)
          get-file-url      (str api-url "/getFile")
          get-file-response (-> {:result {:file_path file-path}}
                                j/write-value-as-string)
          photo-url         (str file-url "/" file-path)

          photo (-> "assets/01photo.jpg"
                    io/resource
                    io/file)

          update-context #(-> (cards/prepare-chat-context
                               system
                               %2
                               (:state %1))
                              (cards/eval-update chat-logic))]
      (with-fake-http [get-file-url {:body get-file-response}
                       photo-url    {:body photo :status 200}
                       #"sendInvoice" {:status 200}
                       #"sendDocument" {:status 200}
                       #"sendPhoto" {:status 200}
                       #"sendMessage" {:status 200}]

        (def new-context
          (reduce update-context {:state {}} (take 22 updates)))

        (is (= raw-values-01-new (get-in new-context [:state :values])))

        ;; TODO: Rewrite to actions
        (def raw-data
          (-> (get-in new-context [:state :values])
              cards/values->raw-data
              :letter-for-mother))

        (is (= raw-data-01-new raw-data))

        (def parsed-data
          (-> (m/parse :letter-for-mother/raw-data
                       raw-data
                       {:registry registry})
              cards/lift-up-data))
        (is (= parsed-data-01-new parsed-data))

        (def prepared-data
          (cards/download-files system parsed-data [[:photo-with-mom]]))
        (is (not (m/explain :letter-for-mother/prepared-data
                            prepared-data {:registry registry})))
        ))))

;; (def system
;;   (ig/init (hundredrps.core/get-config)
;;            [:chat/logic :chat/registry]))

;; (m/form
;;  (:chat/logic system)
;;  {:registry (:chat/registry system)})

;; (m/validate
;;  (m/form
;;   (:chat/logic system)
;;   {:registry (:chat/registry system)}
;;   )
;;  {})
