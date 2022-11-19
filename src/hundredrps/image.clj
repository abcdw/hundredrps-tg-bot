(ns hundredrps.image
  (:require [clojure.java.io :as io])
  (:import javax.imageio.ImageIO
           java.io.ByteArrayOutputStream))

(defn vertical?
  [img]
  (let [img    (. ImageIO read (io/input-stream img))
        height (.getHeight img)
        width  (.getWidth img)]
    (if (< width height)
      true
      false)))

(defn crop-img
  [img aspect-ratio]
  (let [img           (. ImageIO read (io/input-stream img))
        target-ratio  aspect-ratio
        width         (.getWidth img)
        height        (.getHeight img)
        current-ratio (/ width height)

        new-width (if (< target-ratio current-ratio)
                    (int (* target-ratio height))
                    width)
        x         (int (/ (- width new-width) 2))

        new-height  (if (< target-ratio current-ratio)
                      height
                      (int (/ width target-ratio)))
        y           (int (/ (- height new-height) 2))
        cropped-img (.getSubimage img x y new-width new-height)]
    (with-open [buffer (new ByteArrayOutputStream)]
      (. ImageIO write cropped-img "jpg" buffer)
      (.toByteArray buffer))))

(comment
  (def img-input-stream
    (io/input-stream
     (io/file (io/resource "face.jpg"))))

  (def cropped-img
    (crop-img img-input-stream (/ 16 9)))

  (with-open [os (io/output-stream (io/file "resources/cropped.jpg"))]
    (.write os cropped-img)))
