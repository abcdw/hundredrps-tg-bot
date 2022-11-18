(ns hundredrps.pdf
  (:require [clojure.java.io :as io]
            [hundredrps.text :as text]
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
   {:keys [text position fill-column horizontal-spacing font font-size color]
    :or   {fill-column        45
           horizontal-spacing 4.0
           font               [:fonts :kosko-bold.ttf]
           font-size          21}
    :as   action}]

  ;; #p ctx
  ;; #p action
  (let [wrapped-text (wrap-line (get-obj ctx text) fill-column)]
    (.beginText page-cs)
    (.setFont page-cs (get-font ctx font) font-size)
    (when color (.setNonStrokingColor page-cs (get-color ctx color)))
    (.newLineAtOffset page-cs (:x position 0) (:y position 0))
    (doseq [index (range (count wrapped-text))
            :let  [line (get wrapped-text index)]]
      (.drawString page-cs line)
      (.newLineAtOffset page-cs 0 (* (+ font-size horizontal-spacing) -1)))
    (.endText page-cs)))

(defmethod add-pdf-part! :signature
  [{:keys [page-cs page]
    :as   ctx}
   {:keys [text position font font-size color]
    :or   {font      [:fonts :kosko-bold.ttf]
           font-size 21}
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

(defn gen-pdf
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


(def face-photo
   (hundredrps.utils/load-file-as-byte-array
    (io/file (io/resource "face.jpg"))))

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
                   pdf-bytes (gen-pdf system  data :letter-for-mother)]]
    (with-open [os (io/output-stream (io/file file-name))]
      (.write os pdf-bytes))))

;; (generate-test-pdfs)
;; (gen-pdf integrant.repl.state/system form-data :letter-for-mother)
