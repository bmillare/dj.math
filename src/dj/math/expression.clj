(ns dj.math.expression
  (:require [dj.math.parser :as dmp]
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
