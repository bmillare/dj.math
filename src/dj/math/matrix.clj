(ns dj.math.matrix
  (:require [dj.math :as dm]
            [dj.math.parser :as dmp]))

(defprotocol Itranspose
  (t [m]))

;; The current default concretion
(deftype VectorVectorMatrix
    [vvm]
  Object
  (toString [this]
    (str vvm))
  clojure.lang.IPersistentMap 
  clojure.lang.ILookup 
  (valAt [this key]
    (let [[x y] key]
      ((vvm x) y))) 
  (valAt [this key notfound]
    (let [[x y] key]
      (get (get vvm x notfound) y notfound)))
  clojure.lang.IFn
  (invoke [this arg]
    (cond
     (and (sequential? arg)
          (= (count arg) 2))
     (let [[x y] arg]
       ((vvm x) y))

     (map? arg)
     vvm))
  clojure.lang.IPersistentCollection 
  (count [this] (apply + (map count vvm))) 
  (empty [this] []) 
  (equiv [this gs] (or (identical? this gs)
                       (when (identical? (class this)
                                         (class gs))
                         (= vvm
                            (.vvm gs)))))
  clojure.lang.Associative 
  (containsKey [this k]
    (let [[xi yi] k
          y-dim (count vvm)]
      (if (or (< xi 0)
              (< yi 0))
        (throw (Exception. (str "Indexes cannot be negative: " k)))
        (if (zero? y-dim)
          nil
          (if (> y-dim yi)
            (let [x-dim (count (first vvm))]
              (if (zero? x-dim)
                nil
                (> x-dim xi)))))))) 
  (entryAt [this k]
    (let [[x y] key]
      ((vvm x) y)))
  clojure.lang.Seqable
  (seq [this] (seq (apply concat vvm)))
  clojure.lang.IPersistentMap
  (assoc [this k v]
    (let [[x y] k]
      (VectorVectorMatrix. (assoc-in vvm [x y] v))))
  (assocEx [this k v]
    (let [[x y] k]
      (VectorVectorMatrix. (assoc-in vvm [x y] v))))
  Itranspose
  (t [this]
    (VectorVectorMatrix. (apply mapv vector vvm))))

(defn v [vvm]
  (VectorVectorMatrix. vvm))

(defn map-vvm
  ([f x]
     (let [xm (.vvm x)]
       (VectorVectorMatrix. (mapv (fn [xr]
                                    (mapv f
                                          xr))
                                  xm))))
  ([f x y]
     (let [xm (.vvm x)
           ym (.vvm y)]
       (VectorVectorMatrix. (mapv (fn [xr yr]
                                    (mapv f
                                          xr
                                          yr))
                                  xm
                                  ym)))))

(defmethod dm/+ [VectorVectorMatrix VectorVectorMatrix] [x y]
  (map-vvm dm/+ x y))

(defmethod dm/- [VectorVectorMatrix VectorVectorMatrix] [x y]
  (map-vvm dm/- x y))

(defmethod dm/* [VectorVectorMatrix VectorVectorMatrix] [xm ym]
  (let [ymt (t ym)]
    (v (mapv vec
             (partition (count (first (.vvm ym)))
                        (for [xr (.vvm xm)
                              yc (.vvm ymt)]
                          (reduce dm/+
                                  (map dm/*
                                       xr
                                       yc))))))))

(dm/def-commutative-method dm/* [java.lang.Long VectorVectorMatrix] [x ym]
  (map-vvm (fn [y]
             (dm/* x y))
           ym))

(dm/def-commutative-method dm/* [java.lang.Double VectorVectorMatrix] [x ym]
  (map-vvm (fn [y]
             (dm/* x y))
           ym))

(dm/def-commutative-method dm/* [:symbolic-expression VectorVectorMatrix] [x ym]
  (map-vvm (fn [y]
             (dm/* x y))
           ym))

(defmethod dm/d [VectorVectorMatrix java.lang.Long] [xm y]
  (map-vvm (fn [x]
             (dm/d x y))
           xm))

(defmethod dm/d [VectorVectorMatrix java.lang.Double] [xm y]
  (map-vvm (fn [x]
             (dm/d x y))
           xm))

(defmethod dm/d [VectorVectorMatrix :symbolic-expression] [xm y]
  (map-vvm (fn [x]
             (dm/d x y))
           xm))

(defmethod dm/d [:symbolic-expression VectorVectorMatrix] [y xm]
  (map-vvm (fn [x]
             (dm/d x y))
           xm))

(defmethod dm/- [VectorVectorMatrix java.lang.Long] [xm y]
  (map-vvm (fn [x]
             (dm/- x y))
           xm))

(defmethod dm/- [VectorVectorMatrix java.lang.Double] [xm y]
  (map-vvm (fn [x]
             (dm/- x y))
           xm))

(defmethod dm/- [VectorVectorMatrix :symbolic-expression] [xm y]
  (map-vvm (fn [x]
             (dm/- x y))
           xm))

;; work out literal syntax for this
(defmethod clojure.core/print-dup VectorVectorMatrix [o w] 
  (.write w "#dj.math.matrix.VectorVectorMatrix[")
  (.write w (.toString o))
  (.write w "]"))

(defmethod clojure.core/print-method VectorVectorMatrix [o w]
  (.write w "<dj.math.matrix.VectorVectorMatrix\n[\n")
  (let [vvm (.vvm o)]
    (.write w (apply str
                     (apply concat
                            (interpose '("\n")
                                       (for [v vvm]
                                         (interpose " " v)))))))
  (.write w "\n]>"))

(defn identity-m [size]
  (VectorVectorMatrix.
   (mapv (fn [i]
           (into (vec (repeat i 0))
                 (into [1] (vec (repeat (- size i 1) 0)))))
         (range size))))

(defn vnorm [m]
  (dm/sqrt (reduce dm/+
                   (map (fn [n]
                          (dm/pow n
                                  2))
                        (seq m)))))

(defmethod dm/+ [java.lang.Long java.lang.Long] [x y]
  (+ x y))

(defmethod dm/- [java.lang.Long java.lang.Long] [x y]
  (- x y))

(defmethod dm/* [java.lang.Long java.lang.Long] [x y]
  (* x y))

(defmethod dm/d [java.lang.Long java.lang.Long] [x y]
  (/ x y))

(defmethod dm/sqrt [java.lang.Long] [x]
  (Math/sqrt x))

(defmethod dm/pow [java.lang.Long java.lang.Long] [x e]
  (Math/pow x e))

(defmethod dm/+ [java.lang.Double java.lang.Double] [x y]
  (+ x y))

(defmethod dm/- [java.lang.Double java.lang.Double] [x y]
  (- x y))

(defmethod dm/* [java.lang.Double java.lang.Double] [x y]
  (* x y))

(defmethod dm/d [java.lang.Double java.lang.Double] [x y]
  (/ x y))

(defmethod dm/sqrt [java.lang.Double] [x]
  (Math/sqrt x))

(defmethod dm/pow [java.lang.Double java.lang.Double] [x e]
  (Math/pow x e))

(dm/def-commutative-method dm/+ [:symbolic-expression java.lang.Long] [x y]
  (if (zero? y)
    x
    (dmp/s {:op "+"
            :children [x y]})))

(defmethod dm/- [:symbolic-expression java.lang.Long] [x y]
  (if (zero? y)
    x
    (dmp/s {:op "-"
            :children [x y]})))

(defmethod dm/- [java.lang.Long :symbolic-expression] [y x]
  (if (zero? y)
    x
    (dmp/s {:op "-"
            :children [x y]})))

(dm/def-commutative-method dm/* [:symbolic-expression java.lang.Long] [x y]
  (if (zero? y)
    0
    (if (= 1 y)
      x
      (dmp/s {:op "*"
              :children [x y]}))))

(defmethod dm/d [:symbolic-expression java.lang.Long] [x y]
  (dmp/s {:op "/"
          :children [x y]}))

(defmethod dm/d [java.lang.Long :symbolic-expression] [y x]
  (if (zero? y)
    0
    (dmp/s {:op "/"
            :children [y x]})))

(defmethod dm/sqrt [:symbolic-expression] [x]
  (dmp/s {:op "sqrt"
          :children [x]}))

(defmethod dm/pow [:symbolic-expression java.lang.Long] [x e]
  (if (zero? e)
    1
    (if (= 1 e)
      x
      (dmp/s {:op "pow"
              :children [x e]}))))

(defmethod dm/pow [java.lang.Long :symbolic-expression] [x e]
  (dmp/s {:op "pow"
          :children [x e]}))

(dm/def-commutative-method dm/+ [:symbolic-expression java.lang.Double] [x y]
  (if (zero? y)
    x
    (dmp/s {:op "+"
            :children [x y]})))

(defmethod dm/- [:symbolic-expression java.lang.Double] [x y]
  (if (zero? y)
    x
    (dmp/s {:op "-"
            :children [x y]})))

(defmethod dm/- [java.lang.Double :symbolic-expression] [y x]
  (if (zero? y)
    x
    (dmp/s {:op "-"
            :children [x y]})))

(dm/def-commutative-method dm/* [:symbolic-expression java.lang.Double] [x y]
  (if (zero? y)
    0
    (if (= 1 y)
      x
      (dmp/s {:op "*"
              :children [x y]}))))

(defmethod dm/d [:symbolic-expression java.lang.Double] [x y]
  (dmp/s {:op "/"
          :children [x y]}))

(defmethod dm/d [java.lang.Double :symbolic-expression] [y x]
  (if (zero? y)
    0
    (dmp/s {:op "/"
            :children [y x]})))

(defmethod dm/sqrt [:symbolic-expression] [x]
  (dmp/s {:op "sqrt"
          :children [x]}))

(defmethod dm/pow [:symbolic-expression java.lang.Double] [x e]
  (if (zero? e)
    1
    (if (= 1 e)
      x
      (dmp/s {:op "pow"
              :children [x e]}))))

(defmethod dm/pow [java.lang.Double :symbolic-expression] [x e]
  (dmp/s {:op "pow"
          :children [x e]}))