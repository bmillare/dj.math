(ns dj.math
  (:refer-clojure :exclude [* + -])
  (:require [dj.peg :as dp]))

(defn dispatch
  "assumes arity greater than 2 is the same type"
  ([x]
     [(type x)])
  ([x y & args]
     [(type x)
      (type y)]))

(defmulti * dispatch)
(defmulti + dispatch)
(defmulti - dispatch)
(defmulti d dispatch)
(defmulti sqrt dispatch)
(defmulti pow dispatch)
(defmulti copy-sign dispatch)

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
