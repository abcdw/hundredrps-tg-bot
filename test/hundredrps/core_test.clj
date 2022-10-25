(ns hundredrps.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [hundredrps.core :as hrps]))

(deftest summing-test
  (is (= 3 (hrps/sum 1 2)))
  (is (= 7 (hrps/sum 4 3))))
