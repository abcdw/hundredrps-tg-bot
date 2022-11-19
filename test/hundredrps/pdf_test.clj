(ns hundredrps.pdf-test
  (:require [hundredrps.pdf :as sut]
            [hundredrps.utils]
            [integrant.repl.state]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest are is testing]]))

(def face-photo
   (hundredrps.utils/load-file-as-byte-array
    (io/file (io/resource "face.jpg"))))

(def form-data
  {:childhood-memories "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat.",
   :what-mom-cooked    nil,
   :payment            ":letter-for-mother",
   :maybe-edit         :ok,
   :signature          "Best regards",
   :add-warm-memories? true,
   :photo-with-mom     (hundredrps.utils/load-file-as-byte-array
                        (io/file (io/resource "face.jpg"))),
   :sibling            :son,
   :have-children?     false,
   :greeting           :understood,
   :live-together?     true})

(defn generate-test-pdfs
  []
  (for [sibling   [:daughter :son]
        together? [true false]
        photo     [nil face-photo]
        vertical? [true false]
        :let      [file-name (str "target/pdfs/"
                                  (if together? "together" "separate") "-"
                                  (name sibling) "-"
                                  (if photo "photo" "no-photo") "-"
                                  (if vertical? "vertical" "landscape")
                                  ".pdf")
                   data (merge form-data
                               {:sibling        sibling
                                :live-together? together?
                                :photo-with-mom photo
                                :vertical?      vertical?})
                   system integrant.repl.state/system
                   pdf-bytes (sut/generate-pdf system  data :letter-for-mother)]]
    (with-open [os (io/output-stream (io/file file-name))]
      (.write os pdf-bytes))))

;; (generate-test-pdfs)
;; (generate-pdf integrant.repl.state/system form-data :letter-for-mother)

(def form-data-best-woman
  {:mother-name        "мамочка",
   :sibling            :son,
   :what-mom-cooked    "блинах и печенье",
   :mom-consists-of    "невероятное терпение и теплые объятия",
   :mom-consists-of-right-part "невероятное терпение",
   :mom-consists-of-left-part "теплые объятия",
   :ability            "умеешь сопереживать",
   :signature          "Best regards",
   :honor?             false,
   :is-mom-always-right? true,
   :payment            ":best-woman",
   :abuse?              false,
   :maybe-edit         :ok,
   :photo-with-mom     (hundredrps.utils/load-file-as-byte-array
                           (io/file (io/resource "face.jpg"))),
   :vertical?          false})


(defn generate-tests-best-for-woman-pdfs
  []
  (for [sibling   [:daughter :son]
        what-mom-cooked ["блинах и печенье" ""]
        photo     [nil face-photo]
        vertical? [true false]
        is-mom-always-right? [true false]
        abuse?   [true false]
        :let      [file-name (str "target/pdfs/bestwoman-"
                                  (if what-mom-cooked "cooked" "not-cooked") "-"
                                  (name sibling) "-"
                                  (if photo "photo" "no-photo") "-"
                                  (if is-mom-always-right? "mom-always-right" "not-always-right") "-"
                                  (if abuse? "abuse" "not-abuse") "-"
                                  (if vertical? "vertical" "landscape")
                                  ".pdf")
                   data (merge form-data-best-woman
                               {:sibling        sibling
                                :what-mom-cooked what-mom-cooked
                                :photo-with-mom photo
                                :vertical?      vertical?
                                :is-mom-always-right? is-mom-always-right?
                                :abuse? abuse?})
                   system integrant.repl.state/system
                   pdf-bytes (sut/generate-pdf system  data :best-woman)]]
    (.write (io/output-stream (io/file file-name)) pdf-bytes)))


;; (generate-pdf integrant.repl.state/system form-data-best-woman :best-woman)

;; (.write (io/output-stream (io/file "best-woman-test.pdf"))
;;  (generate-pdf integrant.repl.state/system form-data-best-woman :best-woman))
;; (generate-tests-best-for-woman-pdfs)
