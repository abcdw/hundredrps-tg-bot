(ns hundredrps.utils
  (:require [clojure.java.io :as io])
  (:import java.io.ByteArrayOutputStream
           java.io.InputStream))

(defn load-file-as-byte-array
  [file]
  (with-open [xin  (if (instance? InputStream file)
                     file
                     (io/input-stream file))
              xout (new ByteArrayOutputStream)]
    (io/copy xin xout)
    (.toByteArray xout)))
