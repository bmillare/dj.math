(ns dj.math.differentiation
  (:require [dj]
            [dj.math :as dm]
            [dj.math.parser :as dmp]
            [dj.math.expression :as dme]))

;; Note that the chain rule must be implemented for each type

(def d nil)
(defmulti d (fn [exp variable]
              (type exp)))

(defmethod d
  java.lang.Long
  [x variable]
  0)

(defmethod d
  java.lang.Double
  [x variable]
  0.0)

;; Note if we want to have recursive looking up of symbol definitions,
;; then we need dependency information. The only way we can do this
;; elegantly without passing in this data everywhere else (even though
;; its only used in this function), we need to use bindings. Until
;; multimethods are anonymous and can be treated like data, we won't
;; be able to create closures programatically and thus we will have to
;; use bindings. In the future, we can fix this.

(def ^:dynamic symbolic-d (fn [x variable]
                            (let [exp-var-name (:name x)
                                  var-name (:name variable)]
                              (if (= var-name exp-var-name)
                                1
                                0))))

(defn recursive-symbolic-d
  [exp-map dep-map]
  (fn [x variable]
    (user/t [x variable])
    (let [exp-var-name (:name x)
          var-name (:name variable)]
      (if (= var-name exp-var-name)
        1
        ;; if x depends on variable, return derivative of x
        (if ((dep-map exp-var-name) var-name)
          (d (exp-map exp-var-name)
             variable)
          0)))))

(defn symbolic-lookup-differentiation
  "

returns a function that differentiates expressions but can also lookup
definitions

The returned fn already binds symbolic-d

"
  [exp-map dep-map]
  (fn [x variable]
    (binding [symbolic-d (recursive-symbolic-d exp-map
                                               dep-map)]
      (d x variable))))

(defmulti d-op :op)

(defmethod d-op "var" [x variable]
  (symbolic-d x variable))

(defmethod d-op "+" [{:keys [op children]} variable]
  (reduce (fn [r n]
            (dm/+ r (d n variable)))
          (d (first children) variable)
          (rest children)))

(defmethod d-op "*" [{:keys [op children]} variable]
  (let [f (first children)
        g (reduce dm/*
                  (rest children))]
    (dm/+ (dm/* (d f
                   variable)
                g)
          (dm/* f
                (d g
                   variable)))))

(defmethod d-op "/" [{:keys [op children]} variable]
  (let [f (first children)
        g (reduce dm/d
                  (rest children))]
    (dm/d (dm/- (dm/* (d f
                         variable)
                      g)
                (dm/* f
                      (d g
                         variable)))
          (dm/* g g))))

(defmethod d-op "-" [{:keys [op children]} variable]
  (if (= (count children)
         1)
    (dm/- (d (first children) variable))
    (reduce (fn [r n]
              (dm/- r (d n variable)))
            (d (first children) variable)
            (rest children))))

(defmethod d-op "sqrt" [{:keys [op children]} variable]
  (dm/d 1 (dm/* 2 (dm/sqrt (first children)))))

(defmethod d-op "ln" [{:keys [op children]} variable]
  (let [x (first children)]
    (dm/d (d x variable)
          x)))

(defmethod d-op "log" [{:keys [op children]} variable]
  (let [x (first children)]
    (dm/d (d x variable)
          x)))

(defmethod d-op "pow" [{:keys [op children]} variable]
  (let [[x e] children
        types [(number? x) (number? e)]]
    (case types
      [true true] 0
      [true false] (dm/* (dm/* (dm/ln x)
                               (dm/pow x e))
                         (d x variable))
      [false true] (dm/* (dm/* e
                               (dm/pow x (dm/- e 1)))
                         (d x variable))
      (d (dm/exp (dm/* e
                       (dm/ln x)))
         variable))))

(defmethod d-op "if" [{:keys [op children]} variable]
  (let [[c t f] children
        t' (d t variable)
        f' (d f variable)]
    (if (and (number? t')
             (number? f')
             (= (double t') (double f')))
      t'
      (dmp/s {:op "if"
              :children [c
                         t'
                         f']}))))

(defmethod d-op "exp" [{:keys [op children]} variable]
  (let [x (first children)]
    (dm/* (dm/exp x)
          (d x variable))))

(defmethod d-op "float" [{:keys [op children]} variable]
  (let [x (first children)]
    (dmp/s {:op "float"
            :children [(d x variable)]})))

(defmethod d
  :symbolic-expression
  [exp variable]
  (d-op exp variable))

(defn jacobian-map
  "

takes the derivative of all expressions in exp-map relative to
bindings using diff-fn
"
  [exp-map diff-fn]
  (dj/update-vals exp-map
                  (fn [e]
                    (reduce (fn [m sv]
                              (assoc m
                                sv
                                (diff-fn e (dm/vare sv))))
                            {}
                            (keys exp-map)))))
