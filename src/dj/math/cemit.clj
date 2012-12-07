(ns dj.math.cemit
  (:require [dj.math.parser :as dmp]
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
      (cond
       (and (= op "pow")
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
               (apply str (for [[s e] (partition 2 bindings)]
                            (str "const float " (emit s) " = " (emit e) ";\n")))
               (apply str (map emit children))
               ";}\n")
    (throw (Exception. "Not a let but has bindings"))))

(defmethod emit
  #{:variable}
  [{:keys [variable]}]
  variable)

(defmethod emit
  dj.math.matrix.VectorVectorMatrix
  [m]
  (apply str (mapv vec
         (partition (dmm/width m)
                    (map emit (seq m))))))

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