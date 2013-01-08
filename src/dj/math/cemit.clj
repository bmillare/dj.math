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
                (str "{\n"
                     (apply str (for [[s e] (seq bindings)]
                                  ;; clean up this condition once we refactor how we dispatch

                                  ;; want to dispatch on pair
                                  (if (let [ts (type s)]
                                        (and (= ts
                                                :symbolic-expression)
                                             (= (set (keys s))
                                                #{:op :symbols})))
                                    (str (emit s) (emit e))
                                    (str "const float " (emit s) " = " (emit e) ";\n"))))
                     (apply str (map emit children))
                     "}\n"))
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
             dmp/symbolic-expression-dispatcher)]
      @emit))
  ([]
     (c-emitter {"sqrt" "sqrtf"
                 "pow" "powf"
                 "copy-sign" "copysignf"})))