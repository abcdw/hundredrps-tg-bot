(ns hundredrps.tg-test
  (:require [hundredrps.tg :as sut]
            [clojure.test :refer [deftest are is testing]]))

(deftest chat-id-extract
  (testing "Usual message"
    (let [msg {:update_id 961354173,
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
                :text       "hi"}}]
      (is (= 67562087 (sut/get-chat-id msg)))))

  (testing "Photo message"
    (let [msg {:update_id 961354175,
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
                 :last_name     "Tropin"}}}]
      (is (= 67562087 (sut/get-chat-id msg)))))

  (testing "Edited message"
    (let [msg {:update_id 961354174,
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
                :text       "hello"}}]
      (is (= 67562087 (sut/get-chat-id msg))))))
