(ns hundredrps.cards-test
  (:require
   [clojure.test :refer [deftest are is testing]]
   [clojure.java.io :as io]
   [hundredrps.cards :as sut]
   [hundredrps.core]))

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
                      #(sut/process-update (:state %1) logic %2)
                      {:state {}}
                      updates)
                     (get-in [:state :values]))]
      (is (= values-00 values))))
  (testing "Dialog with photo and errors."
    (let [updates (-> "assets/01-letter-for-mother-with-photos-and-errors.edn"
                      io/resource io/file slurp read-string)

          logic  (get-in (hundredrps.core/get-config) [:db/value :logic])
          values (-> (reduce
                      #(sut/process-update (:state %1) logic %2)
                      {:state {}}
                      updates)
                     (get-in [:state :values]))]
      (is (= values-01 values)))))

(def data-for-pdf
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

(deftest values->pdf-data
  (testing "Conversion of values to data suitable for pdf."
    (is (= data-for-pdf (sut/values->pdf-data values-00)))))
