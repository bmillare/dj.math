(ns dj.math.expression
  (:require [dj.math.parser :as dmp]))

(defn dependents [e]
  (reduce (fn [m child]
            )
          #{}
          (:children e)))

