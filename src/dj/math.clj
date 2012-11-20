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

(defmacro def-commutative-method [name
                                  dispatch-value
                                  args
                                  & body]
  `(do
     (defmethod ~name ~dispatch-value ~args
       ~@body)
     (defmethod ~name ~(vec (reverse dispatch-value)) ~(vec (reverse args))
       ~@body)))
