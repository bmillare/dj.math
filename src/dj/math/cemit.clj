(ns dj.math.cemit
  (:require [dj]
            [dj.math.parser :as dmp]
            [dj.plurality :as dp]
            [dj.math.bindings :as dmb]
            [dj.math.matrix :as dmm]))

(defn inverse-sqrt? [children]
  (and (= (count children)
          2)
       (number? (first children))
       (= 1 (long (first children)))
       (= (type (second children))
          :symbolic-expression)
       (= (:op (second children))
          "sqrt")))

(defn c-emitter
  ([op-alias-map]
     (dj/var-let
      [emit (dp/->simple-multi-fn
             {"recur"
              (fn [{:keys [op bindings]}]
                (str (apply str (for [[s e] (seq bindings)]
                                  (str (emit s) " = " (emit e) ";\n")))
                     "continue;\n"))
              "return"
              (fn [{:keys [op bindings]}]
                (str (apply str (for [[s e] (seq bindings)]
                                  (str (emit s) " = " (emit e) ";\n")))
                     #_ "break;\n" ;; break only works when inside a loop, need to account for this case
                     ))
              "destructure"
              (fn [{:keys [op symbols]}]
                (apply str (for [s symbols]
                             (str "float " (emit s) ";\n"))))
              "let"
              (fn [{:keys [op bindings children]}]
                (str #_ "{\n" ;; while technically most correct, problematic when wanting to export values
                     (apply str (for [[s e] (seq bindings)]
                                  ;; clean up this condition once we refactor how we dispatch

                                  ;; want to dispatch on pair
                                  (if (let [ts (type s)]
                                        (and (= ts
                                                :symbolic-expression)
                                             (= (set (keys s))
                                                #{:op :symbols})))
                                    (str (emit s) (emit e))
                                    (str "const float " (emit s) " = " (emit e) ";\n")
                                    #_ (str "float " (emit s) " = " (emit e) ";\n"))))
                     (apply str (map emit children))
                     #_ "}\n"))
              "loop"
              (fn [{:keys [op init-bindings children]}]
                (str (apply str (for [[s e] (seq init-bindings)]
                                  (str "float " (emit s) " = " (emit e) ";\n")))
                     "for (;;) {\n"
                     (apply str (map emit children))
                     "}\n"))
              "var"
              (fn [{:keys [name]}]
                name)
              dj.math.matrix.VectorVectorMatrix
              (fn [m]
                (apply str (map emit (seq m))))
              clojure.lang.PersistentVector
              (fn [m]
                (apply str "//" (interpose " " (map emit m))))
              java.lang.Long
              (fn [x]
                (str (double x)))
              java.lang.Double
              (fn [x]
                (str x))}
             (fn [{:keys [op children]}]
               (let [interpose-children (fn [sep]
                                          (str "("
                                               (apply str (interpose sep (map emit children)))
                                               ")"))]
                 (case op
                   "+" (interpose-children "+")
                   "-" (if (= (count children) 1)
                         (str "(-" (emit (first children)) ")")
                         (interpose-children "-"))
                   "*" (interpose-children "*")
                   "/" (if (inverse-sqrt? children)
                         (emit (dmp/s {:op "rsqrt"
                                       :children (:children (second children))}))
                         (interpose-children "/"))
                   "==" (interpose-children "==")
                   ">" (interpose-children ">")
                   "<" (interpose-children "<")
                   "!=" (interpose-children "!=")
                   "or" (interpose-children "||")
                   "and" (interpose-children "&&")
                   "float" #_ (apply str "(float)" (map emit children))
                   (apply str (map emit children))
                   "double" (apply str "(double)" (map emit children))
                   "long" (if (= (type (first children))
                                 java.lang.Long)
                            (str (first children))
                            (apply str "(long)" (map emit children)))
                   "if" (let [[c t f] children]
                          (if (and (number? t)
                                   (number? f)
                                   (= (double t) (double f)))
                            t
                            (str "(("(emit (first children)) ") ? "
                                 (emit (second children)) " : "
                                 (emit (nth children 2)) ")")))
                   (cond
                    (and (= op "pow")
                         (number? (second children))
                         (= 2.0 (double (second children))))
                    (let [v (emit (first children))]
                      (str v "*" v))
                    
                    :else (str (or (op-alias-map op)
                                   op)
                               (interpose-children ","))))))
             dmp/symbolic-expression-dispatcher)]
      @emit))
  ([]
     (c-emitter {"sqrt" "sqrtf"
                 "pow" "powf"
                 "copy-sign" "copysignf"
                 "ln" "logf"
                 "exp" "expf"
                 "abs" "fabs"
                 "rsqrt" "rsqrtf"})))