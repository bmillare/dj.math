(ns dj.math.parser
  (:require [dj.peg :as dp]))

(defn s [x]
  (with-meta x {:type :symbolic-expression}))

(defn parse [txt]
  (let [id (dp/t #"\p{Alpha}\w*")
	ws (dp/t #"(?:\s|;)*")
        wrap-ws (fn [t]
                  (dp/s ws t ws))
        int-num (dp/alt (dp/t #"\d+")
                        (fn [x]
                          (long (Integer/parseInt x))))
	double-num (dp/alt (dp/t #"[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?")
                           (fn [x]
                             (Double/parseDouble x)))
	plus-minus (dp/t #"[+\-]")
        mult-div (dp/t #"[*/]")
        equalities (dp/t #"==|[><]|\!=")
        comma (dp/t #",")
	lparen (dp/t #"\(")
	rparen (dp/t #"\)")
        cast (dp/t #"double|float")
	infix-node (fn pm [[f r]]
                     (if r
                       (s {:op (first (first r))
                           :children (vec (list* f (map second r)))})
                       f))
        infix-couple (fn [c nc]
                       (fn [[f r]]
                         (if r
                           (let [ret (reduce (fn [ret [op e]]
                                               (if (= op c)
                                                 (update-in ret
                                                            [c]
                                                            conj
                                                            e)
                                                 (update-in ret
                                                            [nc]
                                                            conj
                                                            e)))
                                             {c [f]
                                              nc []}
                                             r)]
                             (let [rnc (ret nc)]
                               (if (empty? rnc)
                                 (s {:op c
                                     :children (ret c)})
                                 (s {:op nc
                                     :children (into [(let [rc (ret c)]
                                                        (if (= 1
                                                               (count rc))
                                                          (first rc)
                                                          (s {:op c
                                                              :children rc})))]
                                                     rnc)}))))
                           f)))]
    (dj/var-let [mult-expr (dp/alt (dp/s atom
                                         (dp/*
                                          (dp/s mult-div
                                                atom)))
                                   (infix-couple "*" "/"))
                 plus-expr (dp/alt (dp/s mult-expr
                                         (dp/*
                                          (dp/s plus-minus
                                                mult-expr)))
                                   (infix-couple "+" "-"))
                 equality-expr (dp/alt (dp/s plus-expr
                                             (dp/* (dp/s equalities
                                                         plus-expr)))
                                       infix-node)
                 cond-expr (dp/alt (dp/s equality-expr
                                         (dp/? (dp/s (wrap-ws (dp/t #"\?"))
                                                     equality-expr
                                                     (wrap-ws (dp/t #":"))
                                                     equality-expr)))
                                   (fn [[c r]]
                                     (if r
                                       (let [[_ t _ f] r]
                                         (s {:op "if"
                                             :children [c t f]}))
                                       c)))
                 expr @cond-expr
                 atom-no-ws (dp/alt (dp/s (dp/? (dp/t #"-"))
                                          (dp/| (dp/alt (dp/s int-num (dp/!? (dp/| (dp/t #"\.")
                                                                                   (dp/t #"[eE]"))))
                                                        first)
                                                double-num
                                                (dp/alt (dp/s lparen
                                                              ws
                                                              cast
                                                              ws
                                                              rparen
                                                              ws
                                                              expr)
                                                        (fn [[_ _ id _ _ _ e]]
                                                          (s {:op id
                                                              :children [e]})))
                                                (dp/alt (dp/s id
                                                              ws
                                                              lparen
                                                              rparen)
                                                        (fn [[id _ _ _]]
                                                          (s {:op id
                                                              :children []})))
                                                (dp/alt (dp/s id
                                                              ws
                                                              lparen
                                                              expr
                                                              (dp/* (dp/s comma expr))
                                                              rparen)
                                                        (fn [[id _ _ f r _]]
                                                          (s {:op id
                                                              :children (vec (list* f (map second r)))})))
                                                (dp/alt id
                                                        (fn [x]
                                                          (s {:variable x})))
                                                (dp/alt (dp/s lparen
                                                              expr
                                                              rparen)
                                                        second)))
                                    (fn [[minus atom]]
                                      (if minus
                                        (s {:op "-"
                                            :children [atom]})
                                        atom)))
                 atom (dp/alt (dp/s ws atom-no-ws ws)
                              second)]
                (dp/parse expr txt))))

(defn symbolic-expression-dispatcher
  [x]
  (let [tx (type x)]
    (if (= tx
           :symbolic-expression)
      (set (keys x))
      tx)))

(defmulti emit symbolic-expression-dispatcher)

(def ^:dynamic op-alias-map {"sqrt" "Math/sqrt"
                             "pow" "Math/pow"
                             "copy-sign" "Math/copySign"})

(defn prefix-emitter [op-aliases]
  (fn [s]
    (binding [op-alias-map op-aliases]
      (emit s))))

(defmethod emit
  #{:op :children}
  [{:keys [op children]}]
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

(defmethod emit
  #{:op :bindings}
  [{:keys [op bindings]}]
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

(defmethod emit
  #{:op :bindings :children}
  [{:keys [op bindings children]}]
  (case op
    "let" (list* (symbol op)
                 (mapv emit (apply concat (.pairs bindings)))
                 (map emit children))
    (throw (Exception. (str "op/bindings/children form not supported:" op)))))

(defmethod emit
  #{:op :init-bindings :return-declarations :children}
  [{:keys [op init-bindings children]}]
  (case op
    "loop" (list* (symbol op)
                  (mapv emit (apply concat (.pairs init-bindings)))
                  (map emit children))
    (throw (Exception. (str "op/init-bindings/children form not supported:" op)))))

(defmethod emit
  #{:variable}
  [{:keys [variable]}]
  (symbol variable))

(defmethod emit
  clojure.lang.PersistentVector
  [x]
  (mapv emit x))

(defmethod emit
  :default
  [x]
  (cond
   (map? x) (reduce-kv (fn [ret k v]
                         (assoc ret
                           k
                           (emit v)))
                       {}
                       x)
   :else x))