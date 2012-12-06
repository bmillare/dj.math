(ns dj.math
  (:refer-clojure :exclude [* + - gensym])
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

;; auto-let, create bindings to expression, but pass through constants
(defmulti auto-let type)

(defn letm
  "may or may not return a let exp"
  ([f x]
     (let [alx (auto-let x)]
       (case (type alx)
         :symbolic-expression #_ (let [xb (:bindings alx)]
                                (if (empty? xb)
                                  (f x)
                                  (let [ret (f (first (:children alx)))]
                                    (if (number? ret)
                                      ret
                                      (dmp/s {:op "let"
                                              :bindings xb
                                              :children [ret]})))))
         (case (set (keys alx))
           #{:op :children :bindings}
           (let [xb (:bindings alx)
                 ret (f (first (:children alx)))]
             (dmp/s {:op "let"
                     :bindings xb
                     :children [ret]}))
           (f x))
         (f x))))
  ([f x y]
     (let [alx (auto-let x)
           aly (auto-let y)]
       (let [[xb xc] (if (= (type alx)
                            :symbolic-expression)
                       [(:bindings alx) (first (:children alx))]
                       [[] x])
             [yb yc] (if (= (type aly)
                            :symbolic-expression)
                       [(:bindings aly) (first (:children aly))]
                       [[] y])
             ;; not sure if this is fast enough BUG?
             bindings #_ (vec (apply concat (distinct (partition 2 (into xb yb)))))
             (into xb yb)]
         (if (empty? bindings)
           (f x y)
           (let [ret (f xc
                        yc)]
             (if (number? ret)
               ret
               (dmp/s {:op "let"
                       :bindings bindings
                       :children [ret]})))))))
  ([f x y z]
     (let [alx (auto-let x)
           aly (auto-let y)
           alz (auto-let z)]
       (let [[xb xc] (if (= (type alx)
                            :symbolic-expression)
                       [(:bindings alx) (first (:children alx))]
                       [[] x])
             [yb yc] (if (= (type aly)
                            :symbolic-expression)
                       [(:bindings aly) (first (:children aly))]
                       [[] y])
             [zb zc] (if (= (type alz)
                            :symbolic-expression)
                       [(:bindings alz) (first (:children alz))]
                       [[] z])
             ;; not sure if this is fast enough BUG?
             bindings #_ (vec (apply concat (distinct (partition 2 (into xb (into yb zb))))))
             (into xb (into yb zb))]
         (if (empty? bindings)
           (f x y z)
           (let [ret (f xc
                        yc
                        zc)]
             (if (number? ret)
               ret
               (dmp/s {:op "let"
                       :bindings bindings
                       :children [ret]}))))))))

;; not sure if I want to overwrite let name right now....
(defmulti elet (fn [_ x]
                 (type x)))
(def ^:dynamic gensym-counter (atom 0))
(defn gensym [prefix]
  (str prefix
       "_"
       (format "%05X"
               (swap! gensym-counter inc))))


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
