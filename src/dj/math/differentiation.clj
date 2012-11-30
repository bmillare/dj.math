(ns dj.math.differentiation
  (:require [dj.math :as dm]
            [dj.math.parser :as dmp]))

;; Note that the chain rule must be implemented for each type

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
  (reduce (fn [r n]
            (dm/- r (d n variable)))
          (d (first children) variable)
          (rest children)))

(defmethod d-op "sqrt" [{:keys [op children]} variable]
  (dm/d 1 (dm/* 2 (dm/sqrt (first children)))))

(defmethod d-op "ln" [{:keys [op children]} variable]
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
      (throw (Exception. "fully symbolic pow not implemented")))))

(defmethod d-op "if" [{:keys [op children]} variable]
  (let [[c t f] children]
    (dmp/s {:op "if"
            :children [c
                       (d t variable)
                       (d f variable)]})))

(defmethod d-op "exp" [{:keys [op children]} variable]
  (let [x (first children)]
    (dm/* (dmp/s {:op "exp"
                  :children [x]})
          (d x variable))))

(defmethod d
  #{:op :children}
  [exp variable]
  (d-op exp variable))

