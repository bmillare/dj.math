(ns dj.math.cemit
  (:require [dj.math.parser :as dmp]
            [dj.math.bindings :as dmb]
            [dj.math.matrix :as dmm]))

(defmulti emit dmp/symbolic-expression-dispatcher)

(def ^:dynamic op-alias-map {"sqrt" "sqrtf"
                             "pow" "powf"
                             "copy-sign" "copysignf"})

(defn c-emitter [op-aliases]
  (fn [s]
    (binding [op-alias-map op-aliases]
      (emit s))))

(defmethod emit
  #{:op :children}
  [{:keys [op children]}]
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

(defmethod emit
  #{:op :bindings}
  [{:keys [op bindings]}]
  (case op
    "recur" (str (apply str (for [[s e] (seq bindings)]
                              (str (emit s) " = " (emit e) ";\n")))
                 "continue;\n")
    "return" (str (apply str (for [[s e] (seq bindings)]
                               (str (emit s) " = " (emit e) ";\n")))
                  "break;\n")
    (throw (Exception. (str "op/bindings form not supported:" op)))))

(defmethod emit
  #{:op :bindings :children}
  [{:keys [op bindings children]}]
  (case op
    "let" (str "{\n"
               (apply str (for [[s e] (seq bindings)]
                            (str "const float " (emit s) " = " (emit e) ";\n")))
               (apply str (map emit children))
               "}\n")
    (throw (Exception. (str "binding form not supported:" op)))))

(defmethod emit
  #{:op :init-bindings :variable-map :children}
  [{:keys [op init-bindings variable-map children]}]
  (case op
    "loop" (str (apply str (for [[s _] (seq init-bindings)]
                             (str "float " (emit s)";\n")))
                (apply str (for [s (keys variable-map)]
                             (str "float " (emit (keys s))";\n")))
                "for (;;) {\n"
                (apply str (map emit children))
                "}\n")
    (throw (Exception. (str "op/init-bindings/children form not supported:" op)))))

(defmethod emit
  #{:variable}
  [{:keys [variable]}]
  variable)

(defmethod emit
  dj.math.matrix.VectorVectorMatrix
  [m]
  #_ (apply str (mapv vec
                   (partition (dmm/width m)
                              (map emit (seq m)))))
  (apply str (map emit (seq m))))

(defmethod emit
  clojure.lang.PersistentVector
  [m]
  #_ (apply str (mapv vec
                   (partition (dmm/width m)
                              (map emit (seq m)))))
  (apply str (map emit m)))

(defmethod emit
  java.lang.Long
  [x]
  (str (double x)))

(defmethod emit
  :default
  [x]
  (cond
   (map? x) (str (reduce-kv (fn [ret k v]
                              (assoc ret
                                k
                                (emit v)))
                            {}
                            x))
   
   :else x))