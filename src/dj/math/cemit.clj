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
  #{:op :bindings :children :returns}
  [{:keys [op bindings children returns]}]
  (let [return-pairs (seq returns)]
    (case op
      "let" (str (apply str (for [[s _] return-pairs]
                              (str "float " (emit s)";\n")))
                 "{\n"
                 (apply str (for [[s e] (seq bindings)]
                              (str "const float " (emit s) " = " (emit e) ";\n")))
                 (apply str (map emit children))
                 (apply str (for [[s e] return-pairs]
                              (str (emit s) " = " (emit e) ";\n")))
                 "}\n")
      (throw (Exception. (str "binding form not supported:" op))))))

;; Not sure if useful
#_ (defmethod emit
     #{:op :bindings}
     [{:keys [op bindings]}]
     (let [binding-pairs (seq bindings)]
       (case op
         "bounce" (apply str (for [[s e] binding-pairs]
                               (str "const float " (emit s) " = " (emit e) ";\n")))
         "return" (apply str (for [[s e] binding-pairs]
                               (str (emit s) " = " (emit e) ";\n")))
         (throw (Exception. (str "binding form not supported:" op))))))

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