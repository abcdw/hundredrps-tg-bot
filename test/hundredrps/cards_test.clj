(ns hundredrps.cards-test
  (:require
   [clojure.test :refer [deftest are is testing]]
   [clojure.java.io :as io]
   [hundredrps.cards :as sut]
   [hundredrps.core]))

(def values-00
  {675 {:step nil, :value "/start"},
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

(deftest letter-for-mother
  (testing
      "Form with most nil values."
    (let [updates (-> "assets/00-letter-for-mother.edn"
                      io/resource io/file slurp read-string)

          logic  (get-in (hundredrps.core/get-config) [:db/value :logic])
          values (-> (reduce
                      #(sut/process-update (:state %1) logic %2)
                      {:state {}}
                      updates)
                     (get-in [:state :values]))]
      (is (= values-00 values)))))
