(ns hundredrps.pdf
  (:require [clojure.java.io :as io])
  (:import
   java.io.ByteArrayOutputStream

   org.apache.pdfbox.pdmodel.PDDocument
   org.apache.pdfbox.pdmodel.PDPage
   org.apache.pdfbox.pdmodel.PDPageContentStream
   org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject

   org.apache.pdfbox.pdmodel.font.PDFont
   org.apache.pdfbox.pdmodel.font.PDType0Font

   org.apache.pdfbox.multipdf.PDFMergerUtility))

(defn wrap-line [text size]
  (loop [left size line [] lines []
         words (clojure.string/split text #"\s+")]
    (if-let [word (first words)]
      (let [wlen (count word)
            spacing (if (== left size) "" " ")
            alen (+ (count spacing) wlen)]
        (if (<= alen left)
          (recur (- left alen) (conj line spacing word) lines (next words))
          (recur (- size wlen) [word] (conj lines (apply str line)) (next words))))
      (when (seq line)
        (conj lines (apply str line))))))

(defn get-obj
  [ctx x]
  (if (vector? x)
    (get-in ctx x)
    x))

(defn get-font
  [{:keys [document] :as ctx} x]
  (let [obj (get-obj ctx x)]
    (if (instance? PDFont x)
      x
      (as-> (get-obj ctx x) t
        (io/input-stream t)
        (. PDType0Font load document t false)))))

(defmulti add-pdf-part!
  (fn [p x] (:type x)))

(defmethod add-pdf-part! :text
  [{:keys [page-cs]
    :as   ctx}
   {:keys [text position fill-column horizontal-spacing font font-size color]
    :or   {fill-column        45
           horizontal-spacing 4.0
           font               [:resources :fonts :kosko-bold.ttf]
           font-size          21}
    :as   action}]

  ;; #p ctx
  ;; #p action
  (let [wrapped-text (wrap-line (get-obj ctx text) fill-column)]
    (.beginText page-cs)
    (.setFont page-cs (get-font ctx font) font-size)
    (when color (.setNonStrokingColor page-cs color))
    (.newLineAtOffset page-cs (:x position 0) (:y position 0))
    (doseq [index (range (count wrapped-text))
            :let  [line (get wrapped-text index)]]
      (.drawString page-cs line)
      (.newLineAtOffset page-cs 0 (* (+ font-size horizontal-spacing) -1)))
    (.endText page-cs)))

(defmethod add-pdf-part! :image
  [{:keys [page-cs document]
    :as   ctx}
   {:keys [image position size]
    :as   action}]

  ;; #p ctx
  ;; #p action
  (let [image-bytes (get-in ctx image)
        image (. PDImageXObject createFromByteArray document image-bytes "hi")]
    (.drawImage page-cs
                image
                (float (:x position 0.0))
                (float (:y position 0.0))
                (float (:width size 100.0))
                (float (:height size 100.0)))))

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

(defn create-document
  "Creates `PDDocument` from template."
  [{:keys [resources] :as ctx} template]
  (let [doc  (. PDDocument load (get-in resources (:pdf template)))
        page (. doc getPage 0)]

    (with-open [page-cs (new PDPageContentStream doc page true true)]
      (doseq [part (:parts template)]
        (add-pdf-part!
         (merge ctx
                {:document doc
                 :page-cs  page-cs})
         part)))
    doc))

(defn gen-pdf
  []
  (let [merger (new PDFMergerUtility)]
    (with-open [final-doc (new PDDocument)
                buffer    (new ByteArrayOutputStream)]

      (doseq [template (vals (-> integrant.repl.state/system
                                 :pdf/templates
                                 :letter-for-mother))]
        (with-open [page (create-document
                          {:data form-data
                           :resources
                           (:pdf/resources integrant.repl.state/system)}
                          template)]
          (. merger appendDocument final-doc page)))

      (.save final-doc buffer)

      ;; TODO Return bytearray
      (.writeTo buffer (io/output-stream (io/file "new.pdf"))))))

;; (gen-pdf)
