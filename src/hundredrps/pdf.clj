(ns hundredrps.pdf
  (:require [clojure.java.io :as io]
            [hundredrps.text :as text]
            [hundredrps.image :as img]
            [malli.core :as m])
  (:import
   java.io.ByteArrayOutputStream
   java.awt.Color

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
  [{:keys [document resources] :as ctx} x]
  (let [obj (get-obj ctx x)]
    (if (instance? PDFont x)
      x
      (as-> (get-obj resources x) t
        (io/input-stream t)
        (. PDType0Font load document t false)))))

(defn get-color
  [ctx [r g b :as x]]
  (if (vector? x)
    (new Color r g b)
    x))

(defmulti add-pdf-part!
  (fn [p x] (:type x)))

(defmethod add-pdf-part! :text
  [{:keys [page-cs]
    :as   ctx}
   {:keys [text position fill-column horizontal-spacing font font-size color character-spacing]
    :or   {fill-column        45
           horizontal-spacing 4.0
           font               [:fonts :kosko-bold.ttf]
           font-size          21
           character-spacing  0}
    :as   action}]

  ;; #p ctx
  ;; #p action
  (let [wrapped-text (wrap-line (get-obj ctx text) fill-column)]
    (.beginText page-cs)
    (.setFont page-cs (get-font ctx font) font-size)
    (.setCharacterSpacing page-cs character-spacing)
    (when color (.setNonStrokingColor page-cs (get-color ctx color)))
    (.newLineAtOffset page-cs (:x position 0) (:y position 0))
    (doseq [index (range (count wrapped-text))
            :let  [line (get wrapped-text index)]]
      (.drawString page-cs line)
      (.newLineAtOffset page-cs 0 (* (+ font-size horizontal-spacing) -1)))
    (.endText page-cs)))

(defmethod add-pdf-part! :centered-text
  [{:keys [page-cs]
    :as   ctx}
   {:keys [text position fill-column horizontal-spacing font font-size color character-spacing]
    :or   {fill-column        45
           horizontal-spacing 4.0
           font               [:fonts :kosko-bold.ttf]
           font-size          21
           character-spacing  0}
    :as   action}]
  (let [wrapped-text (wrap-line (get-obj ctx text) fill-column)
        maxWidth (reduce max (map #(* (/ (.getStringWidth (get-font ctx font) %) 1000) font-size) wrapped-text))
        textHeight (* (count wrapped-text) font-size)
        centerX    (:x position 0)
        centerY    (:y position 0)
        x          (- centerX (/ maxWidth 2))
        y          (+ centerY (/ textHeight 2))]
    (.beginText page-cs)
    (.setFont page-cs (get-font ctx font) font-size)
    (.setCharacterSpacing page-cs character-spacing)
    (when color (.setNonStrokingColor page-cs (get-color ctx color)))
    (.newLineAtOffset page-cs x y)
    (doseq [index (range (count wrapped-text))
            :let  [line (get wrapped-text index)]]
      (.drawString page-cs line)
      (.newLineAtOffset page-cs 0 (* (+ font-size horizontal-spacing) -1)))
    (.endText page-cs)))

(defmethod add-pdf-part! :signature
  [{:keys [page-cs page]
    :as   ctx}
   {:keys [text position font font-size color character-spacing]
    :or   {font      [:fonts :kosko-bold.ttf]
           font-size 21
           character-spacing 0}
    :as   action}]
  (let [text           (get-obj ctx text)
        font           (get-font ctx font)
        pdf-width      (.getWidth (.getMediaBox page))
        text-insert    (* (:x position 0) 2)
        max-text-width (- pdf-width text-insert)
        text-width     (* (/ (.getStringWidth font text) 1000) font-size)
        y              (:y position 0)
        x              (+ (:x position 0) (max 0 (- max-text-width text-width)))]

    (.beginText page-cs)
    (.setFont page-cs font font-size)
    (.setCharacterSpacing page-cs character-spacing)
    (when color (.setNonStrokingColor page-cs (get-color ctx color)))
    (.newLineAtOffset page-cs x y)
    (.drawString page-cs text)
    (.endText page-cs)))

(defmethod add-pdf-part! :image
  [{:keys [page-cs document]
    :as   ctx}
   {:keys [image position size]
    :as   action}]

  ;; #p ctx
  ;; #p action
  (let [aspect-ratio (/ (:width size 100) (:height size 100))
        image-bytes  (img/crop-img (get-in ctx image) aspect-ratio)

        image (. PDImageXObject createFromByteArray document image-bytes "hi")]
    (.drawImage page-cs
                image
                (float (:x position 0))
                (float (:y position 0))
                (float (:width size 100))
                (float (:height size 100)))))

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
                 :page     page
                 :page-cs  page-cs})
         part)))
    doc))

(defn get-resources
 [system card]
 (let [all-resources (:cards/resources system)
       pdfs (get-in all-resources [:pdfs card])]
   (-> all-resources
       (select-keys [:fonts])
       (assoc :pdfs pdfs))))

(defn get-matched-card-parts
  "Return template names for mached rules."
  [data card-rules]
  (->>
   (filter (fn [[pattern value]]
             (when (m/validate pattern data) value)) card-rules)
   (map second)))

(defn generate-pdf
  [system data card]
  (let [merger        (new PDFMergerUtility)
        card-rules    (-> system :pdf/cards card)
        resources     (get-resources system card)
        pdf-templates (-> system :pdf/templates card)
        card-parts    (get-matched-card-parts data card-rules)
        templates     (map pdf-templates card-parts)]
    (with-open [final-doc (new PDDocument)
                buffer    (new ByteArrayOutputStream)]

      (doseq [template templates]
        (with-open [page (create-document
                          {:data      data
                           :texts     (text/get-texts system data card)
                           :resources resources}
                          template)]

          (. merger appendDocument final-doc page)))

      (.save final-doc buffer)
      (.toByteArray buffer))))
