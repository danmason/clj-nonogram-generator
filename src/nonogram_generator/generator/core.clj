(ns nonogram-generator.generator.core
  (:require
    [yada.yada :as yada]
    [integrant.core :as ig]
    [opencv4.utils :as u]
    [opencv4.core :refer :all]))

(defn string-resource
  [x]
  (yada/as-resource x))

(defmethod ig/init-key ::string
  [_ x]
  (string-resource x))


(defn load-image [image width height]
  (-> image
      (u/mat-from-url CV_8UC1)
      (resize! (new-size width height))))

(defn binarize-image [image]
  (-> image
      (threshold! 80.0 1.0 THRESH_BINARY_INV)
      ;(adaptive-threshold! 1.0 ADAPTIVE_THRESH_MEAN_C THRESH_BINARY_INV 3 4)
      ))

(defn get-image-array [image width height]
  (loop [x 0 img-array []]
    (if (= width x)
      img-array
      (recur
       (inc x)
       (conj img-array
             (loop [y 0 row-array []]
               (if (= height y)
                 row-array
                 (recur
                  (inc y)
                  (conj row-array (first (.get image x y)))))))))))

(defn count-row [line]
  (loop [orig line split []]
    (cond
      (empty? orig) (map count split)
      (= 0.0  (first orig)) (recur (drop-while #(= % 0.0) orig) split)
      :else
      (let [zs (split-with #(= % 1.0) orig)]
        (recur (second zs) (conj split (first zs)))))))

(defn process-image [image width height]
  (let [xs (byte-array [])]
    (-> image
        (load-image width height)
        (binarize-image)
        (get-image-array width height)
        ;(#(map count-row %))
        ;(#(map count-row (apply mapv vector %)))
        )))
