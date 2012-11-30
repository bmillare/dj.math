(ns dj.differentiation
  (:require [dj.math :as dm]
            [dj.math.parser :as dmp]))

(def d nil)
(defmulti d (fn [exp variable]
              (dmp/symbolic-expression-dispatcher exp)))

(defmethod d
  java.lang.Long
  [x variable]
  0)

(defmethod d
  java.lang.Double
  [x variable]
  0.0)

(defmethod d
  #{:variable}
  [x variable]
  (let [exp-var-name (:variable x)
        var-name (:variable variable)]
    (if (= var-name exp-var-name)
      1
      0)))

(defmulti d-op :op)

(defmethod d-op "+" [{:keys [op children]} variable]
  (dmp/s {:op "+"
          :children (mapv (fn [exp]
                            (d exp variable))
                          children)}))

(defmethod d-op "*" [{:keys [op children]} variable]
  (dmp/s {:op "*"
          :children (mapv (fn [exp]
                            (d exp variable))
                          children)}))

(defmethod d
  #{:op :children}
  [exp variable]
  (d-op exp variable))

(d (dmp/s {:op "+"
           :children [1 2 3]})
   (dmp/s {:variable "x"}))