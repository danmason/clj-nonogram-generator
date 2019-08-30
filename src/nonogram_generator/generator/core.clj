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
  "Currently loads in an image from a given URL, as a grayscale openCV matrix,
   then resizes to a given width and height"
  (-> image
      (u/mat-from-url CV_8UC1)
      (resize! (new-size width height))))

(defn binarize-image [image invert?]
  "Thresholds a grayscale image matrix to return a binary image. Can optionally
   invert the binarization (by setting 'invert?' to true"
  (if invert?
    (threshold! image 0.0 1.0 (+ THRESH_BINARY_INV THRESH_OTSU))
    (threshold! image 0.0 1.0 THRESH_OTSU)))

(defn get-image-array [image width]
  "Uses the openCV matrix 'dump' function to dump the contents of the matrix as
   a string, removes all contents of the string which do not equal '1'
   or '0' and then partitions the string to return a list of lists describing
   the image."
  (->  image
       .dump
       (s/replace #"[^01]" "")
       ((fn [x] (partition width x)))))

(defn count-row [line]
  "Counts the number of filled in continuous blocks (areas in the image list
   where the value is equal to the character 1) for a current row (list)."
  (loop [orig line split []]
    (cond
      (empty? orig) (map count split)
      (= \0  (first orig)) (recur (drop-while #(= % \0) orig) split)
      :else
      (let [zs (split-with #(= % \1) orig)]
        (recur (second zs) (conj split (first zs)))))))

(defn format-row [image-row]
  "Prepares a current row for printing to the terminal by replacing all
   instances of '1' with an 'X' and all instances of '0' with a blank space.
   Contents of the row are then joined together as a string to be printed"
  (->> image-row
   (map #(s/replace (str %) #"0" " "))
   (map #(s/replace (str %) #"1" "X"))
   (apply str)))

(defn print-image-array [image-array]
  "Print results of 'get-image-array' to console to quickly check the
   appearance of the nonogram board"
  (doseq [x (map format-row image-array)]
    (prn x)))

(defn switch-at-pos [nono-board x y]
  "Takes a position, (x,y) and switches the value at the current position
   within the passed in Nonogram board."
  (let [val-at-pos (get-in nono-board [y x])
        new-val (if (= \1 val-at-pos) \0 \1)]
    (assoc-in nono-board [y x] new-val)))

(defn generate-empty-board [width height]
  "Generates an empty board (full of zeros) with dimensions (width x height)"
  (into [] (repeat height (into [] (repeat width \0)))))

(defn process-image [image-path width height invert?]
  "Takes a path to an image along with desired parameters for the output
   image. Image is first loaded, then binarized and then converted to an
   image array before being returned."
  (-> image-path
      (load-image width height)
      (binarize-image invert)
      (get-image-array width)))

(defn generate-nonogram-board [image width height invert?]
  (let [image-array (process-image image width height invert?)
        row-info (map count-row image-array)
        col-info (map count-row (apply mapv vector image-array))]
    (print-image-array image-array)
    (prn "Row counts: " row-info)
    (prn "Col counts: " col-info)))

(generate-nonogram-board "https://image.shutterstock.com/z/stock-photo-red-apple-on-white-background-158989157.jpg" 20 20 true)
