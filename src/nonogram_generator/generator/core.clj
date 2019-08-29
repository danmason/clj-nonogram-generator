(ns nonogram-generator.generator.core
  (:require
   [clojure.string :as s]
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

(defn binarize-image [image invert]
  (-> image
      ;; Add 'binary inversion' option
      (threshold! 0.0 1.0 (+ THRESH_BINARY_INV THRESH_OTSU))
      ))

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
  "Print results of 'get-image-array' to console toquickly check the appearance of the nonogram board"
  (doseq [x (map print-image-row image-array)]
    (prn x)))

(defn process-image [image width height]
  (-> image
      (load-image width height)
      (binarize-image)
      (get-image-array width)

      ;;(print-image-array)
      ;;(#(map count-row %))
      ;;(#(map count-row (apply mapv vector %)))
      ))
