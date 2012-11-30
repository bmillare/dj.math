(ns dj.math.parser
  (:require [dj.peg :as dp]))

(defn s [x]
  (with-meta x {:type :symbolic-expression}))

(defn parse [txt]
  (let [id (dp/t #"\p{Alpha}\w*")
	ws (dp/t #"\s*")
        int-num (dp/alt (dp/t #"\d+")
                        (fn [x]
                          (Integer/parseInt x)))
	double-num (dp/alt (dp/t #"[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?")
                           (fn [x]
                             (Double/parseDouble x)))
	plus-minus (dp/t #"[+\-]")
	mult-div (dp/t #"[*/]")
        equalities (dp/t #"==|[><]|\!=")
        comma (dp/t #",")
	lparen (dp/t #"\(")
	rparen (dp/t #"\)")
	infix-node (fn pm [[f r]]
                     (if r
                       (s {:op (first (first r))
                           :children (vec (list* f (map second r)))})
                       f))]
    (dj/var-let [mult-expr (dp/alt (dp/s atom
                                         (dp/*
                                          (dp/s mult-div
                                                atom)))
                                   infix-node)
                 plus-expr (dp/alt (dp/s mult-expr
                                         (dp/*
                                          (dp/s plus-minus
                                                mult-expr)))
                                   infix-node)
                 equality-expr (dp/alt (dp/s plus-expr
                                             (dp/* (dp/s equalities
                                                         plus-expr)))
                                       infix-node)
                 cond-expr (dp/alt (dp/s equality-expr
                                         (dp/? (dp/s (dp/t #"\?")
                                                     equality-expr
                                                     (dp/t #":")
                                                     equality-expr)))
                                   (fn [[c r]]
                                     (if r
                                       (let [[_ t _ f] r]
                                         (s {:op "if"
                                             :children [c t f]}))
                                       c)))
                 expr @cond-expr
                 atom-no-ws (dp/alt (dp/s (dp/? (dp/t #"-"))
                                          (dp/| (dp/alt (dp/s int-num (dp/!? (dp/t #"\.")))
                                                        first)
                                                double-num
                                                (dp/alt (dp/s id
                                                              lparen
                                                              rparen)
                                                        (fn [[id _ _]]
                                                          (s {:op id
                                                              :children []})))
                                                (dp/alt (dp/s id
                                                              lparen
                                                              expr
                                                              (dp/* (dp/s comma expr))
                                                              rparen)
                                                        (fn [[id _ f r _]]
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

(defmethod emit
  #{:op :children}
  [{:keys [op children]}]
  (list* (symbol op)
         (map emit children)))

(defmethod emit
  #{:variable}
  [{:keys [variable]}]
  (symbol variable))

(defmethod emit
  :default
  [x]
  x)