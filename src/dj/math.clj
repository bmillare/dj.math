(ns dj.math
  (:refer-clojure :exclude [* + -])
  (:require [dj.peg :as dp]
            [dj.math.parser :as dmp]))

(defn dispatch
  "assumes arity greater than 2 is the same type"
  ([x]
     [(type x)])
  ([x y]
     [(type x)
      (type y)])
  ([x y z]
     [(type x)
      (type y)
      (type z)])
  ([x y z & args]
     [(type x)
      (type y)
      (type z)]))

(defmulti * dispatch)
(defmulti + dispatch)
(defmulti - dispatch)
(defmulti d dispatch)
(defmulti sqrt dispatch)
(defmulti pow dispatch)
(defmulti copy-sign dispatch)
(defmulti ln dispatch)
(defmulti exp dispatch)

(defn ? [c t f]
  (dmp/s {:op "if"
          :children [c t f]}))

(defmacro def-commutative-method
  "sugar for defining a commutative method"
  [name
   dispatch-value
   args
   & body]
  `(do
     (defmethod ~name ~dispatch-value ~args
       ~@body)
     (defmethod ~name ~(vec (reverse dispatch-value)) ~(vec (reverse args))
       ~@body)))

(defmacro def-type-commutative-method
  "sugar for defining a type (only) commutative method"
  [name
   dispatch-value
   args
   & body]
  `(do
     (defmethod ~name ~dispatch-value ~args
       ~@body)
     (defmethod ~name ~(vec (reverse dispatch-value)) ~args
       ~@body)))
