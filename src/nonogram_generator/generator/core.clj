(ns nonogram-generator.generator.core
  (:require
   [clojure.string :as s]
   [yada.yada :as yada]
   [integrant.core :as ig]
   [opencv4.utils :as u]
   [opencv4.core :refer :all :exclude [min reduce sort merge
                                       compare repeat max]]))

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

(defn binarize-image [image invert?]
  (if invert?
    (threshold! image 0.0 1.0 (+ THRESH_BINARY_INV THRESH_OTSU))
    (threshold! image 0.0 1.0 THRESH_OTSU)))

(defn get-image-array [image width]
  (->  image
       .dump
       (s/replace #"[^01]" "")
       ((fn [x] (partition width x)))))

(defn count-row [line]
  (loop [orig line split []]
    (cond
      (empty? orig) (map count split)
      (= \0  (first orig)) (recur (drop-while #(= % \0) orig) split)
      :else
      (let [zs (split-with #(= % \1) orig)]
        (recur (second zs) (conj split (first zs)))))))

(defn print-image-row [image-row]
  (->> image-row
   (map #(s/replace (str %) #"0" " "))
   (map #(s/replace (str %) #"1" "X"))
   (apply str)))

(defn print-image-array [image-array]
  "Print results of 'get-image-array' to console to quickly check the appearance of the nonogram board"
  (doseq [x (map print-image-row image-array)]
    (prn x)))

(defn switch-at-pos [image-array x y]
  (let [row (nth image-array y)
        val-at-pos (nth row x)
        new-val (if (= \1 val-at-pos) \0 \1)]
    (assoc image-array y (assoc row x new-val))
    )
  )

(defn process-image [image width height invert?]
  (-> image
      (load-image width height)
      (binarize-image invert)
      (get-image-array width)))

(defn generate-empty-board [width height]
  (into [] (repeat height (into [] (repeat width \0)))))

(defn generate-nonogram-board [image width height invert?]
  (let [image-array (process-image image width height invert?)
        row-info (map count-row image-array)
        col-info (map count-row (apply mapv vector image-array))]
    (print-image-array image-array)
    (prn "Row counts: " row-info)
    (prn "Col counts: " col-info)))

(generate-nonogram-board "https://image.shutterstock.com/z/stock-photo-red-apple-on-white-background-158989157.jpg" 20 20 true)

(print-image-array (generate-empty-board 20 20))
(switch-at-pos (generate-empty-board 5 5) 1 1)
