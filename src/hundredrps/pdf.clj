(ns hundredrps.pdf
  (:require [clojure.java.io :as io])
  (:import org.apache.pdfbox.pdmodel.PDDocument
           org.apache.pdfbox.pdmodel.PDPage
           org.apache.pdfbox.pdmodel.PDPageContentStream
           org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject
           org.apache.pdfbox.pdmodel.font.PDTrueTypeFont
           org.apache.pdfbox.pdmodel.font.PDType1Font))

(defn generate-pdf
  "Docstring here"
  []
  (let [file           (io/file (io/resource "template.pdf"))
        document       (. PDDocument load file)
        page           (.getPage document 0)
        font           (. PDTrueTypeFont loadTTF document (io/file (io/resource "Kosko.ttf")))
        content-stream (new PDPageContentStream document page true true)
        face-file      (io/file (io/resource "face.jpg"))

        image (. PDImageXObject createFromFile "resources/face.jpg"
                 ;; face-file
                 document)]
    (doto content-stream
      (.beginText)
      (.setFont font 21)
      ;; (.newLineAtOffset 25 500)
      (.moveTextPositionByAmount 70 271)
      (.drawString "hello this is very lorem ipsum string")
      (.endText)
      (.drawImage image 189.0 348.0)
      (.close))
    (doto document
      (.save (io/file "new.pdf"))
      (.close))))
