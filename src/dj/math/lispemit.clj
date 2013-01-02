(ns dj.math.lispemit
  (:require [dj]
            [dj.math.parser :as dmp]
            [dj.plurality :as dp]))

(defn lisp-emitter
  ([op-alias-map]
     (dj/var-let
      [emit (dp/->simple-multi-fn
             {#{:op :children}
              (fn [{:keys [op children]}]
                (reset! user/log @emit)
                (let [default (fn []
                                (list* (symbol (or (op-alias-map op)
                                                   op))
                                       (map emit children)))]
                  (case op
                    "pow" (if (and (number? (second children))
                                   (= 2.0 (double (second children))))
                            (let [v (emit (first children))]
                              `(* ~v ~v))
                            (default))
                    (default))))
              #{:op :bindings}
              (fn [{:keys [op bindings]}]
                (case op
                  "recur" (list* (symbol op)
                                 (map (comp emit second) (seq bindings)))
                  "return" (reduce (fn [m [s e]]
                                     (assoc m
                                       (keyword s)
                                       (emit e)))
                                   {}
                                   (seq bindings))
                  (throw (Exception. (str "op/bindings form not supported:" op)))))
              #{:op :bindings :children}
              (fn [{:keys [op bindings children]}]
                (case op
                  "let" (list* (symbol op)
                               (mapv emit (apply concat (.pairs bindings)))
                               (map emit children))
                  (throw (Exception. (str "op/bindings/children form not supported:" op)))))
              #{:op :init-bindings :return-declarations :children}
              (fn [{:keys [op init-bindings children]}]
                (case op
                  "loop" (list* (symbol op)
                                (mapv emit (apply concat (.pairs init-bindings)))
                                (map emit children))
                  (throw (Exception. (str "op/init-bindings/children form not supported:" op)))))
              #{:variable}
              (fn [{:keys [variable]}]
                (symbol variable))
              clojure.lang.PersistentVector
              (fn [x]
                (mapv emit x))}
             (fn [x]
               (cond
                (map? x) (reduce-kv (fn [ret k v]
                                      (assoc ret
                                        k
                                        (emit v)))
                                    {}
                                    x)
                :else x))
             dmp/symbolic-expression-dispatcher)]
                 @emit))
  ([]
     (lisp-emitter {"sqrt" "Math/sqrt"
                    "pow" "Math/pow"
                    "log" "Math/log"
                    "copy-sign" "Math/copySign"})))