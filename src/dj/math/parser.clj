(ns dj.math.parser
  (:require [dj.peg :as dp]
            [dj.compose :as dc]))

(defn s [x]
  (with-meta x {:type :symbolic-expression}))

(def parse-base
  {:id (dc/fnc #{} #{}
               (dp/t #"\p{Alpha}\w*"))
   :ws (dc/fnc #{} #{} (dp/t #"(?:\s|;)*"))
   :wrap-ws (dc/fnc #{ws} #{}
                    (fn [t]
                      (dp/s ws t ws)))
   :int-num (dc/fnc #{} #{}
                    (dp/alt (dp/t #"\d+")
                            (fn [x]
                              (long (Integer/parseInt x)))))
   :double-num (dc/fnc #{} #{}
                       (dp/alt (dp/t #"(?:\d+(\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?")
                               (fn [x]
                                 (Double/parseDouble x))))
   :plus-minus (dc/fnc #{} #{}
                       (dp/t #"[+\-]"))
   :mult-div (dc/fnc #{} #{}
                     (dp/t #"[*/]"))
   :equalities (dc/fnc #{} #{}
                       (dp/t #"==|\!=|>=|<=|[><]"))
   :logicals (dc/fnc #{} #{}
                     (dp/t #"\|\||&&"))
   :comma (dc/fnc #{} #{}
                  (dp/t #","))
   :lparen (dc/fnc #{} #{}
                   (dp/t #"\("))
   :rparen (dc/fnc #{} #{}
                   (dp/t #"\)"))
   :cast (dc/fnc #{} #{}
                 (dp/t #"double|float|long"))
   :carrot (dc/fnc #{} #{}
                   (dp/t #"\^"))
   :infix-node (dc/fnc #{} #{}
                       (fn pm [[f r]]
                         (if r
                           (s {:op (let [op (first (first r))]
                                     (case op
                                       "||" "or"
                                       "&&" "and"
                                       "^" "pow"
                                       op))
                               :children (vec (list* f (map second r)))})
                           f)))
   :infix-couple (dc/fnc #{} #{}
                         (fn [c nc]
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
                               f))))
   
   :mult-expr (dc/fnc #{mult-div infix-couple} #{atom}
                      (dp/alt (dp/s atom
                                    (dp/*
                                     (dp/s mult-div
                                           atom)))
                              (infix-couple "*" "/")))
   :plus-expr (dc/fnc #{plus-minus infix-couple} #{mult-expr}
                      (dp/alt (dp/s mult-expr
                                    (dp/*
                                     (dp/s plus-minus
                                           mult-expr)))
                              (infix-couple "+" "-")))
   :equality-expr (dc/fnc #{equalities infix-node} #{plus-expr}
                          (dp/alt (dp/s plus-expr
                                        (dp/* (dp/s equalities
                                                    plus-expr)))
                                  infix-node))
   :logicals-expr (dc/fnc #{logicals infix-node} #{equality-expr}
                          (dp/alt (dp/s equality-expr
                                        (dp/* (dp/s logicals
                                                    equality-expr)))
                                  infix-node))
   :cond-expr (dc/fnc #{wrap-ws} #{logicals-expr}
                      (dp/alt (dp/s logicals-expr
                                    (dp/? (dp/s (wrap-ws (dp/t #"\?"))
                                                logicals-expr
                                                (wrap-ws (dp/t #":"))
                                                logicals-expr)))
                              (fn [[c r]]
                                (if r
                                  (let [[_ t _ f] r]
                                    (s {:op "if"
                                        :children [c t f]}))
                                  c))))
   :atom-no-ws (dc/fnc #{int-num
                         double-num
                         lparen
                         rparen
                         ws
                         cast
                         comma
                         id}
                       #{atom
                         expr}
                       (dp/alt (dp/s (dp/? (dp/t #"-"))
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
                                                         atom)
                                                   (fn [[_ _ id _ _ _ a]]
                                                     (s {:op id
                                                         :children [a]})))
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
                                                     (s {:op "var"
                                                         :name x})))
                                           (dp/alt (dp/s lparen
                                                         expr
                                                         rparen)
                                                   second)))
                               (fn [[minus atom]]
                                 (if minus
                                   (s {:op "-"
                                       :children [atom]})
                                   atom))))
   :atom (dc/fnc #{ws} #{atom-no-ws}
                 (dp/alt (dp/s ws atom-no-ws ws)
                         second))})

(let [expr (-> parse-base
               (dc/->fn-map :expr
                            {:expr :cond-expr})
               :expr
               deref)]
  (defn parse [txt]
    (dp/parse expr txt)))

(defn symbolic-expression-dispatcher
  [x]
  (let [tx (type x)]
       (if (= tx
              :symbolic-expression)
         (:op x)
         tx)))