(ns dj.math
  (:refer-clojure :exclude [* + - gensym])
  (:require [dj.peg :as dp]
            [dj.math.parser :as dmp]
            [dj.math.bindings :as dmb]))

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

(defmacro letm [bindings ret]
  (let [pairs (partition 2 bindings)
        cp (count pairs)
        als (take cp
                  (repeatedly (fn []
                                (clojure.core/gensym "al"))))
        bs (take cp
                 (repeatedly (fn []
                               (clojure.core/gensym "b"))))
        cs (take cp
                 (repeatedly (fn []
                               (clojure.core/gensym "c"))))
        bindings-sym (clojure.core/gensym "bindings")
        syms (map first pairs)
        xs (take cp
                 (repeatedly (fn []
                               (clojure.core/gensym "x"))))
        es (map second pairs)]
    `(let ~(into (vec
                  (mapcat (fn [x e]
                            `(~x ~e))
                          xs
                          es))
                 (into (vec
                        (mapcat (fn [alx x]
                                  `(~alx (auto-let ~x)))
                                als
                                xs))
                       (into (vec
                              (mapcat (fn [alx xb xc x]
                                        `[[~xb ~xc] (if (= (type ~alx)
                                                           :symbolic-expression)
                                                      [(:bindings ~alx) (first (:children ~alx))]
                                                      [(dmb/pairs->bindings []) ~x])])
                                      als
                                      bs
                                      cs
                                      xs))
                             [bindings-sym `(dmb/join ~@bs)])))
       (if (empty? ~bindings-sym)
         (let ~(vec (mapcat (fn [s x]
                              [s x])
                            syms
                            xs))
           ~ret)
         (let [result# (let ~(vec (mapcat (fn [s cx]
                                            [s cx])
                                          syms
                                          cs))
                         ~ret)]
           (if (number? result#)
             result#
             (case (type result#)
               :symbolic-expression
               (case (set (keys result#))
                 #{:op :children :bindings}
                 (dmp/s {:op "let"
                         :bindings (dmb/join ~bindings-sym
                                             (:bindings result#)
                                             (let [fcr# (first (:children result#))]
                                               (if (= (type fcr#)
                                                      :symbolic-expression)
                                                 (:bindings fcr#)
                                                 nil)))
                         :children (:children result#)})
                 (dmp/s {:op "let"
                         :bindings ~bindings-sym
                         :children [result#]}))
               (dmp/s {:op "let"
                       :bindings ~bindings-sym
                       :children [result#]}))))))))

(def ^:dynamic gensym-counter (atom 0))

(defn gensym [prefix]
  (str prefix
       "_"
       (format "%05X"
               (swap! gensym-counter inc))))


(defn ? [c t f]
  (if (and (number? t)
           (number? f)
           (= (double t) (double f)))
    t
    (dmp/s {:op "if"
            :children [c t f]})))

;; New Special forms

;; Akin to clojure recur or a tail call, sets values to variables that
;; is expected to leave scope. Like prepping for next loop
;; iteration. From consts -> vars
#_ (dmp/s {:op "bounce"
           :bindings nil})

;; :op "let" can have a returns form now. Akin to a function
;; returning. Declares variables and sets values to variables that is
;; expected to capture them in the future.

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
