(ns hundredrps.tg-test
  (:require [hundredrps.tg :as sut]
            [clojure.test :refer [deftest are is testing]]))

(def usual-message
  {:update_id 961354173,
   :message
   {:date       1667456119,
    :chat
    {:first_name "Andrew",
     :username   "andrewtropin",
     :type       "private",
     :id         67562087,
     :last_name  "Tropin"},
    :message_id 39,
    :from
    {:first_name    "Andrew",
     :language_code "en",
     :is_bot        false,
     :username      "andrewtropin",
     :id            67562087,
     :last_name     "Tropin"},
    :text       "hi"}})

(def photo-message
  {:update_id 961354175,
   :message
   {:date       1667456251,
    :chat
    {:first_name "Andrew",
     :username   "andrewtropin",
     :type       "private",
     :id         67562087,
     :last_name  "Tropin"},
    :message_id 41,
    :photo
    [{:width          90,
      :file_size      1386,
      :file_unique_id "AQADCr4xG-BnoEp4",
      :file_id
      "AgACAgIAAxkBAAMpY2Nc-yqDSJesQ2Rj30jpDbyeNxAAAgq-MRvgZ6BKY_u9_KME3CIBAAMCAANzAAMqBA",
      :height         82}
     {:width          320,
      :file_size      9735,
      :file_unique_id "AQADCr4xG-BnoEpy",
      :file_id
      "AgACAgIAAxkBAAMpY2Nc-yqDSJesQ2Rj30jpDbyeNxAAAgq-MRvgZ6BKY_u9_KME3CIBAAMCAANtAAMqBA",
      :height         291}
     {:width          587,
      :file_size      18397,
      :file_unique_id "AQADCr4xG-BnoEp9",
      :file_id
      "AgACAgIAAxkBAAMpY2Nc-yqDSJesQ2Rj30jpDbyeNxAAAgq-MRvgZ6BKY_u9_KME3CIBAAMCAAN4AAMqBA",
      :height         533}],
    :from
    {:first_name    "Andrew",
     :language_code "en",
     :is_bot        false,
     :username      "andrewtropin",
     :id            67562087,
     :last_name     "Tropin"}}})

(def edited-message
  {:update_id 961354174,
   :edited_message
   {:date       1667456119,
    :edit_date  1667456158,
    :chat
    {:first_name "Andrew",
     :username   "andrewtropin",
     :type       "private",
     :id         67562087,
     :last_name  "Tropin"},
    :message_id 39,
    :from
    {:first_name    "Andrew",
     :language_code "en",
     :is_bot        false,
     :username      "andrewtropin",
     :id            67562087,
     :last_name     "Tropin"},
    :text       "hello"}})

(def pre-checkout-query
  {:update_id 961354476,
   :pre_checkout_query
   {:invoice_payload ":letter-for-mother",
    :currency        "RUB",
    :from
    {:first_name    "Andrew",
     :language_code "en",
     :is_bot        false,
     :username      "andrewtropin",
     :id            67562087,
     :last_name     "Tropin"},
    :id              "290176956541333535",
    :total_amount    19900}})

(deftest get-chat-id
  (testing "Usual message"
    (is (= 67562087 (sut/get-chat-id usual-message))))

  (testing "Photo message"
    (is (= 67562087 (sut/get-chat-id photo-message))))

  (testing "Edited message"
    (is (= 67562087 (sut/get-chat-id edited-message))))

  (testing "Update without chat key (pre-checkout-query)"
    (is (= 67562087 (sut/get-chat-id pre-checkout-query)))))

(deftest get-photo-file-id
  (testing "Best photo id"
    (is (= "AgACAgIAAxkBAAMpY2Nc-yqDSJesQ2Rj30jpDbyeNxAAAgq-MRvgZ6BKY_u9_KME3CIBAAMCAAN4AAMqBA"
             (sut/get-photo-file-id photo-message))))
  (testing "Non-photo message"
    (is (nil? (sut/get-photo-file-id usual-message)))))

(deftest get-message-type
  (testing "Message type"
    (is (= :text (sut/get-message-type usual-message)))
    (is (= :text (sut/get-message-type edited-message)))
    (is (= :photo (sut/get-message-type photo-message)))
    (is (= :unknown (sut/get-message-type {})))))
