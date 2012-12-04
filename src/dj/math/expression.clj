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
                  (if-let [vc (:variable child)]
                    (conj m vc)
                    m))
                m)))
          #{}
          (:children e)))

(defn direct-dependents-map [dmap]
  (dj/update-vals dmap
                  (comp direct-dependents :result dmp/parse)))

(defn has-dependents
  "
:expression-map
symbols ('variable' names) -> sets of symbol names the expression uses

:variables
list of variables to check if an symbol depends on

returns map
symbols -> variables (passed as an arg) that the symbols depends on
"
  [expression-map variables]
  (let [vs (set variables)]
    (dj/var-let [rdependents (memoize
                              (fn [s]
                                (let [ds (expression-map s)
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
                        (keys expression-map)))))

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

#_ (do
     (dj.dependencies/resolve-project "dj.cuda")
     (load "dj/view"
           "dj/source"
           "dj/cuda/translator")
     (def e (dj.view/default-generator dj.view/emit))
     (def re (dj.view/default-generator dj.view/raw-emit))
     (e (:result (dj.math.parser/parse "( VmDT_75 - VmDT_20 * ATPi_ADP * ADPm_ATPm * exp(-F_over_RT * Dpsi) ) /\n    ( ( 1.0 + tenDiv9 * ATPi_ADP * exp( -hm_F_over_RT * (Dpsi) ) ) * (1.0 + 18 * ADPm_ATPm ) );")))
     (dj.source/search (dj.io/file dj/system-root
                                   "usr/src/dj.cuda")
                       #"clj"
                       #"profiles")
     (let [combine (fn [m]
                     (concat (:algebra-assignment m)
                             (:differential-assignment m)))]
       (-> {:model-file (dj.io/file "dj/cuda/models/mitomodel.c")
            :record-dt 100
            :dt 10
            :end-time 1000}
           dj.cuda.translator/full-model-config
           combine
           dj.cuda.translator/pairs->map
           (dj/update-vals (comp dj.math.parser/emit :result dj.math.parser/parse))
           #_ (dj/update-vals (comp empty? :unconsumed-input dj.math.parser/parse))
           vals
           (->> (interpose "\n"))
           (->> (apply str ))
           re))

     (re-find #"\d+(?![eE])"
              "70e")

     {:state-vars nil
      :algebra-assignment nil
      :differential-assignment nil}

     (let [combine (fn [m]
                     (concat (:algebra-assignment m)
                             (:differential-assignment m)))
           data (-> {:model-file (dj.io/file "dj/cuda/models/mitomodel.c")
                     :record-dt 100
                     :dt 10
                     :end-time 1000}
                    dj.cuda.translator/full-model-config)]
       (-> data
           combine
           dj.cuda.translator/pairs->map
           dj.math.expression/direct-dependents-map
           (dj.math.expression/has-dependents (:state-vars data))
           dj.math.expression/reverse-dependencies
           e)))