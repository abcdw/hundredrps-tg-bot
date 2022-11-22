(ns hundredrps.text
  (:require [clojure.java.io :as io]
            [malli.core :as m])
  (:import (java.util.regex Pattern)))

;; (defmethod ig/init-key :text/templates [_ val]
;;   val)

(defn fill-text-template
  [{:keys [data] :as ctx} template]
  (reduce
   (fn [acc [pattern path]]
     (if (m/validate pattern data)
       (conj acc (get-in ctx path))
       acc))
   []
   template))

(defn eval-template
  [ctx template]
  (apply format (fill-text-template ctx template)))

(defn eval-templates
  [{:keys [text-templates] :as ctx}]
  (->> #(eval-template ctx %)
       (update-vals text-templates)))

(defn get-texts
  [system data card]
  (->
   {:data           data
    :strings        (get-in system [:cards/resources :strings card])
    :text-templates (get-in system [:text/templates card])}
   eval-templates))

(defn remove-some-unicode-symbols
  [s]
  (let [regex (Pattern/compile
               (str
                "["
                "\uD83D\uDE00-\uD83D\uDE4F" ; emoticons
                ;; "\U0001F600-\U0001F64F"

                ;; TODO: Convert the rest with
                ;; https://codepoints.net/U+1F64F?lang=en
                ;; "\U0001F300-\U0001F5FF"  ; symbols & pictographs
                ;; "\U0001F680-\U0001F6FF"  ; transport & map symbols
                ;; "\U0001F1E0-\U0001F1FF"  ; flags (iOS)
                ;; "\U00002500-\U00002BEF"  ; chinese char
                ;; "\U00002702-\U000027B0"
                ;; "\U00002702-\U000027B0"
                ;; "\U000024C2-\U0001F251"
                ;; "\U0001f926-\U0001f937"
                ;; "\U00010000-\U0010ffff"
                ;; "\u2640-\u2642"
                ;; "\u2600-\u2B55"
                ;; "\u200d"
                ;; "\u23cf"
                ;; "\u23e9"
                ;; "\u231a"
                ;; "\ufe0f"  ; dingbats
                ;; "\u3030"
                ;; "\U00002702-\U000027B0"
                ;; "\U000024C2-\U0001F251"
                ;; "\U0001f926-\U0001f937"
                ;; "\u200d"
                ;; "\u2640-\u2642"
                "]+"))]
    (clojure.string/replace s regex "")))

;; (remove-some-unicode-symbols "test 😃")
