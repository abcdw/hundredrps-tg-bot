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

(def values-00
  {670 {:step nil, :value "hi"},
   675 {:step nil, :value "/start"},
   680 {:step [:start], :value "/letterForMother"},
   684 {:step [:letter-for-mother :greeting], :value :understood},
   686 {:step [:letter-for-mother :live-together?], :value true},
   688 {:step [:letter-for-mother :sibling], :value :son},
   690 {:step [:letter-for-mother :photo-with-mom], :value nil},
   692 {:step [:letter-for-mother :what-mom-cooked], :value nil}
   694 {:step [:letter-for-mother :have-children?], :value false},
   696 {:step [:letter-for-mother :childhood-memories],
        :value "Childhood memories here"},
   698 {:step [:letter-for-mother :add-warm-memories?], :value true},
   700 {:step [:letter-for-mother :signature], :value "Best regards"},
   702 {:step [:letter-for-mother :maybe-edit], :value :ok},
   735 {:step [:letter-for-mother :payment], :value ":letter-for-mother"}})

(def values-01
  {801 {:step nil, :value "hi"},
   803 {:step nil, :value "o"},
   805 {:step nil, :value "/start"},
   809 {:step [:start], :value "/letterForMother"},
   815 {:step [:letter-for-mother :greeting], :value :understood},
   832 {:step [:letter-for-mother :live-together?], :value false},
   836 {:step [:letter-for-mother :sibling], :value :son},
   850 {:step [:letter-for-mother :photo-with-mom],
        :value {:width          587,
                :file_size      18397,
                :file_unique_id "AQADCr4xG-BnoEp9",
                :file_id
                "AgACAgIAAxkBAAIDUmNzVgEeORTZ9Rh7MXK6rwRdWEHQAAIKvjEb4GegSmP7vfyjBNwiAQADAgADeAADKwQ",
                :height         533}},
   852 {:step [:letter-for-mother :what-mom-cooked], :value "ho"},
   854 {:step [:letter-for-mother :have-children?], :value false},
   856 {:step [:letter-for-mother :childhood-memories],
        :value "childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much, almost 300 hundreds."},
   858 {:step [:letter-for-mother :add-warm-memories?], :value true}
   860 {:step [:letter-for-mother :signature], :value "Best regards"},
   864 {:step [:letter-for-mother :maybe-edit], :value :ok},
   868 {:step [:letter-for-mother :payment], :value ":letter-for-mother"},})

(deftest letter-for-mother
  (testing "Dialog with most nil values."
    (let [updates (-> "assets/00-letter-for-mother.edn"
                      io/resource io/file slurp read-string)

          logic  (get-in (hundredrps.core/get-config) [:db/value :logic])
          values (-> (reduce
                      #(cards/process-update (:state %1) logic %2)
                      {:state {}}
                      updates)
                     (get-in [:state :values]))]
      (is (= values-00 values))))
  (testing "Dialog with photo and errors."
    (let [updates (-> "assets/01-letter-for-mother-with-photos-and-errors.edn"
                      io/resource io/file slurp read-string)

          logic  (get-in (hundredrps.core/get-config) [:db/value :logic])
          values (-> (reduce
                      #(cards/process-update (:state %1) logic %2)
                      {:state {}}
                      updates)
                     (get-in [:state :values]))]
      (is (= values-01 values)))))

(def data-for-pdf-00
  {:letter-for-mother
   {:childhood-memories "Childhood memories here",
    :what-mom-cooked    nil,
    :payment            ":letter-for-mother",
    :maybe-edit         :ok,
    :signature          "Best regards",
    :add-warm-memories? true,
    :photo-with-mom     nil,
    :sibling            :son,
    :have-children?     false,
    :greeting           :understood,
    :live-together?     true},
   nil    "/start",
   :start "/letterForMother"})

(def data-for-pdf-01
  {:letter-for-mother
   {:childhood-memories "childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much childhood memories here wery long to check that it wraps correctly, but less than 300 hundreds of course, but not much, almost 300 hundreds."
    :what-mom-cooked    "ho",
    :payment            ":letter-for-mother",
    :maybe-edit         :ok,
    :signature          "Best regards",
    :add-warm-memories? true,
    :photo-with-mom     (-> "assets/01photo.jpg"
                            io/resource
                            io/file
                            io/input-stream
                            slurp)
    :sibling            :son,
    :have-children?     false,
    :greeting           :understood,
    :live-together?     false},
   nil    "/start",
   :start "/letterForMother"})

(deftest values->pdf-data

  (testing "Conversion of simple values to data suitable for pdf."
    (is (= data-for-pdf-00
           (cards/values->pdf-data {} values-00))))

  (let [{:tg/keys [file-url api-url] :as ctx}
        (-> (hundredrps.core/get-config)
            (ig/init [:tg/file-url :tg/api-url]))

        file-name         "file_1.jpg"
        file-path         (str "photos/" file-name)
        get-file-url      (str api-url "/getFile")
        get-file-response (-> {:result {:file_path file-path}}
                              j/write-value-as-string)
        photo-url         (str file-url "/" file-path)

        photo (-> "assets/01photo.jpg"
                  io/resource
                  io/file
                  io/input-stream)]
    (with-fake-http [get-file-url {:body get-file-response}
                     photo-url    {:body photo :status 200}]
      (testing "Obtaining image from tg servers."
        (let [prepared-data (-> (cards/values->pdf-data ctx values-01)
                                (update-in [:letter-for-mother :photo-with-mom]
                                           slurp))]
          (is (= data-for-pdf-01 prepared-data)))))))

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

(deftest letter-for-mother-new
  (testing "Dialog with photo and errors."
    (def updates (-> "assets/01-letter-for-mother-with-photos-and-errors.edn"
                      io/resource io/file slurp read-string))
    (let [system       (ig/init (hundredrps.core/get-config)
                                [:chat/logic :chat/registry])
          chat-id      (tg/get-chat-id (get-in updates [0]))
          chat-logic   (:chat/logic system)

          update-context #(-> (cards/prepare-chat-context
                               system
                               (get-in updates [%1])
                               (:state %2))
                              (cards/eval-update chat-logic))]

      (def lfmn-hi (update-context 0 {:state {}}))
      (is (validl {:messages [:vector {:min 1} :map]
                   :state    [:map {:closed true}]} lfmn-hi))

      (def lfmn-o (update-context 1 lfmn-hi))
      (is (validl {:messages [:vector {:min 1} :map]
                   :state    [:map {:closed true}]} lfmn-o))

      (def start (update-context 2 lfmn-o))
      (is (validl {:messages [:vector {:min 1} :map]
                   :state    {:step [:= [:start]]}} start))

      ;; (is (= values-01 values))
      )))

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
