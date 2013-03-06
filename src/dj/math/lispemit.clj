(ns dj.math.lispemit
  (:require [dj]
            [dj.math.parser :as dmp]
            [dj.plurality :as dp]))

(defn lisp-emitter
  ([op-alias-map]
     (dp/->recursive-simple-multi-fn
      {"recur"
       (fn [emit]
         (fn [{:keys [op bindings]}]
           (list* (symbol op)
                  (map (comp emit second) (seq bindings)))))
       "return"
       (fn [emit]
         (fn [{:keys [op bindings]}]
           (reduce (fn [m [s e]]
                     (assoc m
                       (keyword (:name s))
                       (emit e)))
                   {}
                   (seq bindings))))
       "destructure"
       (fn [emit]
         (fn [{:keys [op symbols]}]
           {:keys (mapv emit symbols)}))
       "let"
       (fn [emit]
         (fn [{:keys [op bindings children]}]
           (list* (symbol op)
                  ;; work around for 64kb limit
                  #_ (vec (mapcat (fn [[s e]]
                                    [(emit s)
                                     `((fn []
                                         ~(emit e)))])
                                  (.pairs bindings)))
                  (mapv emit (apply concat (.pairs bindings)))
                  (map emit children))))
       "loop"
       (fn [emit]
         (fn [{:keys [op init-bindings children]}]
           (list* (symbol op)
                  (mapv emit (apply concat (.pairs init-bindings)))
                  (map emit children))))
       "var"
       (fn [emit]
         (fn [{:keys [name]}]
           (symbol name)))
       java.lang.Long
       (fn [emit]
         identity)
       java.lang.Float
       (fn [emit]
         identity)
       java.lang.Double
       (fn [emit]
         identity)
       clojure.lang.Ratio
       (fn [emit]
         identity)
       clojure.lang.PersistentVector
       (fn [emit]
         (fn [x]
           (mapv emit x)))}
      (fn [emit]
        (fn [{:keys [op children] :as x}]
          (list* (symbol (or (op-alias-map op)
                             op))
                 (map emit children))))
      dmp/symbolic-expression-dispatcher))
  ([]
     (lisp-emitter {"sqrt" "Math/sqrt"
                    "pow" "Math/pow"
                    "log" "Math/log"
                    "ln" "Math/log"
                    "exp" "Math/exp"
                    "copy-sign" "Math/copySign"})))






