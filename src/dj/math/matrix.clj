(ns dj.math.matrix
  (:require [dj.math :as dm]
            [dj.math.parser :as dmp]
            [dj.math.bindings :as dmb]))

(defprotocol Itranspose
  (t [m]))

(defprotocol Irowseq
  (row-seq [m]))

(defprotocol Icolseq
  (col-seq [m]))

(defprotocol Iassocminor
  (assoc-minor [m start-idx minor-matrix]))

(defprotocol Idimensions
  (width [m])
  (height [m]))

(defprotocol Igetminor
  (get-minor [m top-left-idx bottom-right-limit]))

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
      ((vvm y) x))) 
  (valAt [this key notfound]
    (let [[x y] key]
      (get (get vvm y notfound) x notfound)))
  clojure.lang.IFn
  (invoke [this arg]
    (let [[x y] arg]
      ((vvm y) x)))
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
      ((vvm y) x)))
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
    (VectorVectorMatrix. (apply mapv vector vvm)))
  Irowseq
  (row-seq [this]
    (map (fn [r]
           (VectorVectorMatrix. [r]))
         vvm))
  Icolseq
  (col-seq [this]
    (let [width (count (first vvm))
          height (count vvm)]
      (map (fn [c]
             (VectorVectorMatrix.
              (mapv (fn [r]
                      [((vvm r) c)])
                    (range height))))
           (range width))))
  Iassocminor
  (assoc-minor [m start-idx minor-matrix]
    (let [[sx sy] start-idx
          m-width (count (first vvm))
          m-height (count vvm)
          minor-width (width minor-matrix)
          minor-height (height minor-matrix)]
      (if (and (< (+ sx (dec minor-width))
                  m-width)
               (< (+ sy (dec minor-height))
                  m-height))
        (VectorVectorMatrix. (reduce (fn [ret-m [xi yi]]
                                       (assoc-in ret-m
                                                 [(+ yi sy)
                                                  (+ xi sx)]
                                                 (minor-matrix [xi yi])))
                                     vvm
                                     (for [x (range minor-width)
                                           y (range minor-height)]
                                       [x y])))
        (throw (Exception. "minor does not fit within matrix starting at index")))))
  Idimensions
  (width [this]
    (count (first vvm)))
  (height [this]
    (count vvm))
  Igetminor
  (get-minor [m top-left-idx bottom-right-limit]
    (let [[tlx tly] top-left-idx
          [brx bry] bottom-right-limit]
      (VectorVectorMatrix. (reduce (fn [vv y]
                                     (conj vv
                                           (subvec (vvm y)
                                                   tlx 
                                                   brx)))
                                   []
                                   (range tly bry))))))

(defn minor
  ([m top-left-idx bottom-right-limit]
     (get-minor m top-left-idx bottom-right-limit))
  ([m top-left-idx]
     (get-minor m top-left-idx [(width m)
                                (height m)])))



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

(defmethod dm/* [:symbolic-expression :symbolic-expression] [x y]
  (dmp/s {:op "*"
          :children [x y]}))

(defmethod dm/+ [:symbolic-expression :symbolic-expression] [x y]
  (dmp/s {:op "+"
          :children [x y]}))

(defmethod dm/- [:symbolic-expression :symbolic-expression] [x y]
  (if (= x y)
    0
    (dmp/s {:op "-"
           :children [x y]})))

(defmethod dm/d [:symbolic-expression :symbolic-expression] [x y]
  (dmp/s {:op "/"
          :children [x y]}))

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

;; not sure if this is really needed
#_ (defmethod dm/d [:symbolic-expression VectorVectorMatrix] [y xm]
  (map-vvm (fn [x]
             (dm/d x y))
           xm))

(defmethod dm/- [java.lang.Long] [x]
  (- x))

(defmethod dm/- [java.lang.Double] [x]
  (- x))

(defmethod dm/- [:symbolic-expression] [x]
  (dmp/s {:op "-"
          :children [x]}))

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

(defn vnorm
  "operates on anything that can be represented as a seq"
  [m]
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

(defmethod dm/copy-sign [java.lang.Double java.lang.Double] [m s]
  (Math/copySign m s))

(defmethod dm/copy-sign [java.lang.Double java.lang.Long] [m s]
  (Math/copySign m (double s)))

(defmethod dm/copy-sign [java.lang.Long java.lang.Double] [m s]
  (Math/copySign (double m) s))

(defmethod dm/copy-sign [:symbolic-expression java.lang.Double] [m s]
  (if (pos? s)
    m
    (dm/- m)))

(defmethod dm/copy-sign [:symbolic-expression java.lang.Long] [m s]
  (if (pos? s)
    m
    (dm/- m)))

(defmethod dm/copy-sign [:symbolic-expression :symbolic-expression] [m s]
  (dmp/s {:op "copy-sign"
          :children [m s]}))

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

(dm/def-type-commutative-method dm/+ [java.lang.Long java.lang.Double] [x y]
  (+ x y))

(dm/def-type-commutative-method dm/- [java.lang.Long java.lang.Double] [x y]
  (- x y))

(dm/def-type-commutative-method dm/* [java.lang.Long java.lang.Double] [x y]
  (* x y))

(dm/def-type-commutative-method dm/d [java.lang.Long java.lang.Double] [x y]
  (/ x y))

(dm/def-type-commutative-method dm/pow [java.lang.Long java.lang.Double] [x e]
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

(defmethod dm/- [java.lang.Long :symbolic-expression] [x y]
  (if (zero? x)
    (dmp/s {:op "-"
            :children [y]})
    (dmp/s {:op "-"
            :children [x y]})))

(dm/def-commutative-method dm/* [:symbolic-expression java.lang.Long] [x y]
  (cond
   (zero? y) 0
   (= 1 y) x
   (= -1 y) (dm/- x)
   :else (dmp/s {:op "*"
                 :children [x y]})))

(defmethod dm/d [:symbolic-expression java.lang.Long] [x y]
  (dmp/s {:op "/"
          :children [x y]}))

(defmethod dm/d [java.lang.Long :symbolic-expression] [x y]
  (if (zero? x)
    0
    (dmp/s {:op "/"
            :children [x y]})))

(defmethod dm/sqrt [:symbolic-expression] [x]
  (if (and (= (:op x) "pow")
           (or (= (second (:children x))
                  2)
               (= (second (:children x))
                  2.0)))
    (dmp/s {:op "abs"
            :children [(first (:children x))]})
    (dmp/s {:op "sqrt"
            :children [x]})))

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

(defmethod dm/- [java.lang.Double :symbolic-expression] [x y]
  (if (zero? x)
    (dmp/s {:op "-"
            :children [y]})
    (dmp/s {:op "-"
            :children [x y]})))

(dm/def-commutative-method dm/* [:symbolic-expression java.lang.Double] [x y]
  (cond
   (zero? y) 0
   (= 1 y) x
   (= -1 y) (dm/- x)
   :else (dmp/s {:op "*"
              :children [x y]})))

(defmethod dm/d [:symbolic-expression java.lang.Double] [x y]
  (dmp/s {:op "/"
          :children [x y]}))

(defmethod dm/d [java.lang.Double :symbolic-expression] [x y]
  (if (zero? x)
    0
    (dmp/s {:op "/"
            :children [x y]})))

(defmethod dm/pow [:symbolic-expression java.lang.Double] [x e]
  (if (zero? e)
    1
    (if (= 1.0 e)
      x
      (dmp/s {:op "pow"
              :children [x e]}))))

(defmethod dm/pow [java.lang.Double :symbolic-expression] [x e]
  (dmp/s {:op "pow"
          :children [x e]}))

(defmethod dm/ln [java.lang.Long] [x]
  (Math/log (double x)))

(defmethod dm/ln [java.lang.Double] [x]
  (Math/log x))

(defmethod dm/ln [:symbolic-expression] [x]
  (dmp/s {:op "ln"
          :children [x]}))

(defmethod dm/exp [java.lang.Long] [x]
  (Math/exp (double x)))

(defmethod dm/exp [java.lang.Double] [x]
  (Math/exp x))

(defmethod dm/exp [:symbolic-expression] [x]
  (dmp/s {:op "exp"
          :children [x]}))

;; Is there a way to generalize this to any Matrix type??

;; auto-let weirdness, all nested lets reduce to concatenation of bindings
;; operations are always performed on body
(defmethod dm/auto-let
  VectorVectorMatrix
  [m]
  (let [sym-names (mapv (fn [n]
                          (dmp/s {:op "var"
                                  :name (dm/gensym (str "m" n))}))
                        (range (count m)))
        ms (seq m)
        ;; if I want to remove redundant symbols this is where

        ;; the redudancy is this: if v is a variable, I should be able
        ;; to pass the variable symbol straight through. What's
        ;; important is that I make sure mvs also gets updated in that
        ;; ms also passes through.
        bindings (reduce (fn [ret [k v]]
                           (if (number? v)
                             ret
                             (if-let [child-bindings (:bindings v)]
                               (dmb/join ret
                                         (dmb/append child-bindings
                                                     k
                                                     (-> v
                                                         :children
                                                         first)))
                               (if (= (:op v)
                                      "var")
                                 ret
                                 (dmb/append ret
                                             k
                                             v)))))
                         (dmb/pairs->bindings [])
                         (seq (dmb/zip->bindings sym-names
                                                 ms)))
        mvs (map (fn [s v]
                   (if (number? v)
                     v
                     (if (= (:op v)
                            "var")
                       v
                       s)))
                 sym-names
                 ms)]
    (if (empty? bindings)
      m
      (dmp/s {:op "let"
              :bindings bindings
              :children [(v (mapv vec
                                  (partition (width m)
                                             mvs)))]}))))

(defmethod dm/auto-let
  clojure.lang.PersistentVector
  [vec']
  (let [sym-names (mapv (fn [n]
                          (dmp/s {:op "var"
                                  :name (dm/gensym (str "v" n))}))
                        (range (count vec')))
        bindings (reduce (fn [ret [k v]]
                           (if (number? v)
                             ret
                             (if-let [child-bindings (:bindings v)]
                               (dmb/join ret
                                         (dmb/append child-bindings
                                                     k
                                                     (-> v
                                                         :children
                                                         first)))
                               (dmb/append ret
                                           k
                                           v))))
                         (dmb/pairs->bindings [])
                         (seq (dmb/zip->bindings sym-names
                                                 vec')))]
    (if (empty? bindings)
      vec'
      (dmp/s {:op "let"
              :bindings bindings
              :children [(mapv (fn [s v]
                                 (if (number? v)
                                   v
                                   s))
                               sym-names
                               vec')]}))))

(defmethod dm/auto-let
  :symbolic-expression
  [e]
  (let [g (dmp/s {:op "var"
                  :name (dm/gensym "g")})]
    (case (:op e)
      "let" e
      "var" e
      (dmp/s {:op "let"
              :bindings (dmb/pairs->bindings [[g e]])
              :children [g]}))))

(defmethod dm/auto-let
  java.lang.Long
  [x]
  x)

(defmethod dm/auto-let
  java.lang.Double
  [x]
  x)

#_ (defmethod dmp/emit
  dj.math.matrix.VectorVectorMatrix
  [m]
  #_ (v (mapv vec
           (partition (width m)
                      (map dmp/emit (seq m)))))
  (mapv vec
        (partition (width m)
                   (map dmp/emit (seq m)))))

(defn symbolic-jacobian-template
  "
state-vars specifies the order

creates a jacobian template matrix
"
  [jacobian-map state-vars id->Ji]
  (v
   (vec (for [rsv state-vars]
          (vec (for [csv state-vars]
                 (let [e ((jacobian-map rsv) csv)]
                   (if (number? e)
                     e
                     (dmp/s {:op "var"
                             :name (id->Ji rsv csv)})))))))))