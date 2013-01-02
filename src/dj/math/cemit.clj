(ns dj.math.cemit
  (:require [dj]
            [dj.math.parser :as dmp]
            [dj.plurality :as dp]
            [dj.math.bindings :as dmb]
            [dj.math.matrix :as dmm]))

(defn c-emitter
  ([op-alias-map]
     (dj/var-let
      [emit (dp/->simple-multi-fn
             {#{:op :children}
              (fn [{:keys [op children]}]
                (let [interpose-children (fn [sep]
                                           (str "("
                                                (apply str (interpose sep (map emit children)))
                                                ")"))]
                  (case op
                    "+" (interpose-children "+")
                    "-" (interpose-children "-")
                    "*" (interpose-children "*")
                    "/" (interpose-children "/")
                    "==" (interpose-children "==")
                    ">" (interpose-children ">")
                    "<" (interpose-children "<")
                    "!=" (interpose-children "!=")
                    "float" (apply str "(float)" (map emit children))
                    "double" (apply str "(double)" (map emit children))
                    "if" (let [[c t f] children]
                           (if (and (number? t)
                                    (number? f)
                                    (= (double t) (double f)))
                             t
                             (str "("(emit (first children)) ") ? "
                                  (emit (second children)) " : "
                                  (emit (nth children 2)))))
                    (cond
                     (and (= op "pow")
                          (number? (second children))
                          (= 2.0 (double (second children))))
                     (let [v (emit (first children))]
                       (str v "*" v))
                                                                  
                     :else (str (or (op-alias-map op)
                                    op)
                                (interpose-children ","))))))
              #{:op :bindings}
              (fn [{:keys [op bindings]}]
                (case op
                  "recur" (str (apply str (for [[s e] (seq bindings)]
                                            (str (emit s) " = " (emit e) ";\n")))
                               "continue;\n")
                  "return" (str (apply str (for [[s e] (seq bindings)]
                                             (str (emit s) " = " (emit e) ";\n")))
                                "break;\n")
                  (throw (Exception. (str "op/bindings form not supported:" op)))))
              #{:op :bindings :children}
              (fn [{:keys [op bindings children]}]
                (case op
                  "let" (str "{\n"
                             (apply str (for [[s e] (seq bindings)]
                                          (str "const float " (emit s) " = " (emit e) ";\n")))
                             (apply str (map emit children))
                             "}\n")
                  (throw (Exception. (str "binding form not supported:" op)))))
              #{:op :init-bindings :variable-map :children}
              (fn [{:keys [op init-bindings variable-map children]}]
                (case op
                  "loop" (str (apply str (for [[s _] (seq init-bindings)]
                                           (str "float " (emit s)";\n")))
                              (apply str (for [s (keys variable-map)]
                                           (str "float " (emit (keys s))";\n")))
                              "for (;;) {\n"
                              (apply str (map emit children))
                              "}\n")
                  (throw (Exception. (str "op/init-bindings/children form not supported:" op)))))
              #{:variable}
              (fn [{:keys [variable]}]
                variable)
              dj.math.matrix.VectorVectorMatrix
              (fn [m]
                (apply str (map emit (seq m))))
              clojure.lang.PersistentVector
              (fn [m]
                (apply str (map emit m)))
              java.lang.Long
              (fn [x]
                (str (double x)))}
             (fn [x]
               (cond
                (map? x) (str (reduce-kv (fn [ret k v]
                                           (assoc ret
                                             k
                                             (emit v)))
                                         {}
                                         x))
                :else x))
             dmp/symbolic-expression-dispatcher)]
      @emit))
  ([]
     (c-emitter {"sqrt" "sqrtf"
                 "pow" "powf"
                 "copy-sign" "copysignf"})))