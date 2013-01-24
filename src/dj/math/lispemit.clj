(ns dj.math.lispemit
  (:require [dj]
            [dj.math.parser :as dmp]
            [dj.plurality :as dp]))

;; There is an issue of extensibility with mutual different
;; plural-fns, how can we access or extend called plural-fns?

;; For now, for the sake of moving forward, we will just define
;; non-extensible plural-fns

(defn lisp-emitter
  ([op-alias-map]
     (dj/var-let
      [emit (let [emit-fn (fn [op children]
                            (list* (symbol (or (op-alias-map op)
                                               op))
                                   (map emit children)))]
              (dp/->simple-multi-fn
               {#_ "pow"
                #_ (fn [{:keys [op children]}]
                  (if (and (number? (second children))
                           (= 2.0 (double (second children))))
                    (let [v (emit (first children))]
                      `(* ~v ~v))
                    (emit-fn "pow" children)))
                "recur"
                (fn [{:keys [op bindings]}]
                  (list* (symbol op)
                         (map (comp emit second) (seq bindings))))
                "return"
                (fn [{:keys [op bindings]}]
                  (reduce (fn [m [s e]]
                            (assoc m
                              (keyword (:name s))
                              (emit e)))
                          {}
                          (seq bindings)))
                "destructure"
                (fn [{:keys [op symbols]}]
                  {:keys (mapv emit symbols)})
                "let"
                (fn [{:keys [op bindings children]}]
                  (list* (symbol op)
                         ;; work around for 64kb limit
                         #_ (vec (mapcat (fn [[s e]]
                                           [(emit s)
                                            `((fn []
                                                ~(emit e)))])
                                         (.pairs bindings)))
                         (mapv emit (apply concat (.pairs bindings)))
                         (map emit children)))
                "loop"
                (fn [{:keys [op init-bindings children]}]
                  (list* (symbol op)
                         (mapv emit (apply concat (.pairs init-bindings)))
                         (map emit children)))
                "var"
                (fn [{:keys [name]}]
                  (symbol name))
                java.lang.Long
                identity
                java.lang.Float
                identity
                java.lang.Double
                identity
                clojure.lang.Ratio
                identity
                clojure.lang.PersistentVector
                (fn [x]
                  (mapv emit x))}
               (fn [{:keys [op children] :as x}]
                 (emit-fn op children))
               dmp/symbolic-expression-dispatcher))]
      @emit))
  ([]
     (lisp-emitter {"sqrt" "Math/sqrt"
                    "pow" "Math/pow"
                    "log" "Math/log"
                    "ln" "Math/log"
                    "exp" "Math/exp"
                    "copy-sign" "Math/copySign"})))






