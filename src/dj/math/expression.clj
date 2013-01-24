(ns dj.math.expression
  (:require [dj.math :as dm]
            [dj.math.parser :as dmp]
            [clojure.core.logic :as ccl]
            [clojure.set :as cs]
            [dj]))

(defn direct-dependents [e]
  (reduce (fn [m child]
            (let [tc (type child)]
              (if (= tc :symbolic-expression)
                (if (:children child)
                  (cs/union m (direct-dependents child))
                  (if (= (:op child) "var")
                    (conj m (:name child))
                    m))
                m)))
          #{}
          (:children e)))

(defn direct-dependents-map [dmap]
  (dj/update-vals dmap
                  (comp direct-dependents :result dmp/parse)))

(defn has-dependents
  "
:var-uses-map
symbols ('variable' names) -> sets of symbol names the expression uses

:variables
list of variables to check if an symbol depends on

returns map
symbols -> variables (passed as an arg) that the symbols depends on
"
  [var-uses-map variables]
  (let [vs (set variables)]
    (dj/var-let [rdependents (memoize
                              (fn [s]
                                (let [ds (var-uses-map s)
                                      ds-vs (cs/difference ds vs)]
                                  (if (empty? ds-vs)
                                    (cs/intersection vs ds)
                                    (cs/intersection vs
                                                     (apply cs/union
                                                            ds
                                                            (map rdependents
                                                                 ds-vs)))))))]
                (reduce (fn [m s]
                          (assoc m
                            s
                            (rdependents s)))
                        {}
                        (keys var-uses-map)))))

(defn reverse-dependencies
  "
converts map of
symbols -> variables they depend on

variables -> symbols that depend on variables
"
  [dmap]
  (reduce-kv (fn [m k ds]
               (reduce (fn [m' d]
                         (update-in m'
                                    [d]
                                    cs/union
                                    #{k}))
                       m
                       ds))
             {}
             dmap))

(defn recursive-dependents-map
  "
returns map
symbols -> variables (passed as an arg) that the symbols depends on
"
  [exp-map variables]
  (-> exp-map
      (dj/update-vals direct-dependents)
      (has-dependents variables)))

(defn unbound-dependents-map
  "
convenience fn

returns map
symbols -> empty sets
" [variables]
  (zipmap variables (repeat #{})))

(defn inline-expression
  "
exp: any expression, ie. a number, var, or operator
exp-map: hashmap from var names -> expressions

returns expression with all nested expressions inlined
"
  [exp exp-map]
  ((fn recur-inline-exp [exp]
     (case (type exp)
       :symbolic-expression (let [{:keys [op children name]} exp]
                              (case op
                                "var" (if-let [e (exp-map name)]
                                        (recur-inline-exp e)
                                        exp)
                                (assoc exp
                                  :children (mapv recur-inline-exp children))))
       (if (number? exp)
         exp
         (throw (Exception. "expression type not recognized")))))
   exp))

(defn arraymap->symbolic-expression [m]
  (if (number? m)
    m
    (dmp/s (if-let [c (:children m)]
             (assoc m
               :children
               (mapv arraymap->symbolic-expression c))
             m))))

(defn reduce-constants
  "
evalutes expression as much as possible by calling math fn
"
  [e]
  (case (type e)
    :symbolic-expression (if (:name e)
                           e
                           (let [c (map reduce-constants (:children e))
                                 commutative-simplify (fn [f]
                                                        (let [{:keys [symbolic constant]}
                                                              (group-by (fn [e]
                                                                          (case (type e)
                                                                            :symbolic-expression :symbolic
                                                                            :constant))
                                                                        c)
                                                              all (concat constant symbolic)]
                                                          (if (dj.math/one? (count all))
                                                            (first all)
                                                            (reduce f all))))]
                             (case (:op e)
                               "*" (commutative-simplify dm/*)
                               "+" (commutative-simplify dm/+)
                               "-" (if (= 1 (count c))
                                     (dm/- (first c))
                                     (reduce dm/- c))
                               "/" (dm/d (first c) (second c))
                               "sqrt" (dm/sqrt (first c))
                               "pow" (apply dm/pow c)
                               "copy-sign" (apply dm/pow c)
                               "ln" (dm/ln (first c))
                               "log" (dm/ln (first c))
                               "exp" (dm/exp (first c))
                               e)))
    e))

(letfn [ ;;e is guaranteed to be a normalized ratio if possible
        (split-ratio-expression [state e]
          (case (type e)
            :symbolic-expression (let [{:keys [op children name]} e]
                                   (case op
                                     "/" (let [[child-n child-d] children]
                                           (-> state
                                               (update-in [:numerator]
                                                          conj
                                                          child-n)
                                               (update-in [:denominator]
                                                          conj
                                                          child-d)))
                                     (update-in state
                                                [:numerator]
                                                conj
                                                e))),
            (update-in state
                       [:numerator]
                       conj
                       e)))
        (swap-ratio [state]
          {:numerator (:denominator state)
           :denominator (:numerator state)})
        (flatten-* [es]
          (reduce (fn [v e]
                    (case (type e)
                      :symbolic-expression (case (:op e)
                                             "*" (into v (:children e))
                                             (conj v e))
                      (conj v e)))
                  []
                  es))
        (multiply-expression [es]
          (case (count es)
            1 (first es)
            (dmp/s {:op "*"
                    :children es})))
        (callo [f n1 d1 n2 d2]
          (ccl/fresh [ln1 ld1 ln2 ld2]
                     (ccl/== ln1 n1)
                     (ccl/== ld1 d1)
                     (ccl/== ln2 n2)
                     (ccl/== ld2 d2)
                     (f ln1 ld1 ln2 ld2)))
        (copysign [number magnitude sign]
          (ccl/conda
           [(ccl/== number {:op "-" :children [magnitude]})
            (ccl/== sign -1)]
           [(ccl/== number magnitude)
            (ccl/== sign 1)]))
        (simplifyo [n1 d1 n2 d2]
          (ccl/conda
           [(ccl/== n1 {:op "-" :children [d1]})
            (ccl/== n2 -1)
            (ccl/== d2 1)]
           [(ccl/== {:op "-" :children [n1]} d1)
            (ccl/== n2 -1)
            (ccl/== d2 1)]
           [(ccl/fresh [exponent magnitude sign]
                       (copysign d1 magnitude sign)
                       (ccl/== {:op "pow" :children [magnitude exponent]} n1)
                       (ccl/project [exponent magnitude]
                                    (ccl/== n2 (dj.math/pow (dj.math.parser/s magnitude)
                                                            (dec exponent))))
                       (ccl/== d2 sign))]
           [(ccl/fresh [exponent magnitude sign]
                       (copysign n1 magnitude sign)
                       (ccl/== {:op "pow" :children [magnitude exponent]} d1)
                       (ccl/project [exponent magnitude]
                                    (ccl/== d2 (dj.math/pow (dj.math.parser/s magnitude)
                                                            (dec exponent))))
                       (ccl/== n2 sign))]
           [(ccl/fresh [nv dv n-exponent d-exponent]
                       (ccl/== {:op "pow" :children [nv n-exponent]} n1)
                       (ccl/== {:op "pow" :children [dv d-exponent]} d1)
                       (ccl/project [nv n-exponent d-exponent]
                                    (let [nv (dj.math.parser/s nv)
                                          diff (- n-exponent
                                                  d-exponent)]
                                      (if (pos? diff)
                                        (ccl/all
                                         (ccl/== n2 (dj.math/pow nv diff))
                                         (ccl/== d2 1))
                                        (ccl/all
                                         (ccl/== n2 1)
                                         (ccl/== d2 (dj.math/pow nv (- diff))))))))]))
        (simplify [n d]
          (if (= n d)
            [1 1]
            (-> (ccl/run 1 [q]
                         (ccl/fresh [qn qd]
                                    (ccl/== q [qn qd])
                                    (callo simplifyo n d qn qd)))
                first)))
        (cancel [state]
          (let [{:keys [numerator denominator]} state
                n' (flatten-* numerator)
                d' (flatten-* denominator)]
            (loop [n n'
                   n-idx 0
                   d d'
                   d-idx 0]
              (if (< n-idx (count n))
                (if (< d-idx (count d))
                  (let [ne (n n-idx)]
                    (case (type ne)
                      :symbolic-expression
                      (let [de (d d-idx)]
                        (case (type de)
                          :symbolic-expression
                          (if-let [s (simplify ne de)]
                            (let [[ne' de'] s]
                              (recur (assoc n
                                       n-idx
                                       (arraymap->symbolic-expression ne')) (inc n-idx)
                                       (assoc d
                                         d-idx
                                         (arraymap->symbolic-expression de')) 0))
                            (recur n n-idx
                                   d (inc d-idx)))
                          (recur n n-idx
                                 d (inc d-idx))))
                      (recur n (inc n-idx)
                             d 0)))
                  (recur n (inc n-idx)
                         d 0))
                (dj.math/d (reduce dj.math/* n)
                           (reduce dj.math/* d))))))]
  (defn normalize-ratio
    "

A normalized ratio is a division put into the form in prefix notation

 (/ (* numerator...) (* denominator...))

 and (* numerator...) and (* denominator) reduce to just numerator or
 denominator if singular

"
    [e]
    (-> (case (type e)
          :symbolic-expression (let [{:keys [op children name]} e]
                                 (case op
                                   "/" (case (count children)
                                         0 (throw (Exception. "empty division"))
                                         1 (normalize-ratio (first children))
                                         (let [numerator (normalize-ratio (first children))
                                               denominator (map normalize-ratio (rest children))]
                                           (-> {:numerator []
                                                :denominator []}
                                               (as-> state
                                                     (reduce split-ratio-expression
                                                             state
                                                             denominator))
                                               swap-ratio
                                               (split-ratio-expression numerator)
                                               cancel))),
                                   "var" e,
                                   (assoc e
                                     :children
                                     (mapv normalize-ratio children))))
          e)
        reduce-constants)))
