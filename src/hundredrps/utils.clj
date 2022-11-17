(ns hundredrps.utils
  (:require [clojure.java.io :as io])
  (:import java.io.ByteArrayOutputStream))

(defn load-file-as-byte-array
  [file]
  (with-open [xin  (io/input-stream file)
              xout (new ByteArrayOutputStream)]
    (io/copy xin xout)
    (.toByteArray xout)))
