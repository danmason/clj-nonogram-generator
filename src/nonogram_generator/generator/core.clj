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
