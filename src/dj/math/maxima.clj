(ns dj.math.maxima
  (:require [clojure.java.shell :as cjs]
            [dj.math.parser :as dmp]
            [dj.math.cemit]
            [dj.plurality :as dp]
            [dj.peg :as dpe]
            [dj.compose :as dc]))

(let [expr (-> dmp/parse-base
               (assoc
                   :mult-expr
                 (dc/fnc #{mult-div infix-couple} #{exp-expr}
                         (dpe/alt (dpe/s exp-expr
                                         (dpe/*
                                          (dpe/s mult-div
                                                 exp-expr)))
                                  (infix-couple "*" "/")))
                 :id (dc/fnc #{} #{}
                             (dpe/t #"%?\p{Alpha}\w*"))
                 :exp-expr (dc/fnc #{carrot infix-node} #{atom}
                                   (dpe/alt (dpe/s atom
                                                   (dpe/*
                                                    (dpe/s carrot
                                                           atom)))
                                            infix-node)))
               (dc/->fn-map :expr
                            {:expr :cond-expr})
               :expr
               deref)]
  (defn parse [txt]
    (dpe/parse expr txt)))

(defn call-maxima* [commands]
  (cjs/sh "maxima"
          "--very-quiet"
          :in commands))

(defn call-maxima [commands]
  (let [{:keys [out err]} (call-maxima* (str "display2d:false;" commands))]
    (if (empty? err)
      (-> (.split #"\n" out)
          (->> (drop 2)
               (apply str)))
      (throw (Exception. err)))))

(def default-settings "display2d:false$ratprint:false$")

;; $ terminates command but also silences, need to figure out way to remove comments

(def emit
  (dp/update-implementation (dj.math.cemit/c-emitter {"exp" "exp"})
                            assoc
                            "pow"
                            (fn [emit]
                              (fn [{:keys [op children]}]
                                (let [[base exponent] children]
                                  (str (emit base)
                                       "^"
                                       (emit exponent)))))
                            java.lang.Long
                            (fn [emit]
                              (fn [x]
                                (str x)))))

#_ (do
     (call-maxima "diff(x^2,x);")
     (let [ce (dj.math.cemit/c-emitter {"exp" "exp"})]
       ce)
     "(Vmuni_ktrans * Cai * (FRT2 * ( Dpsi - 91.0 )) * Cai_ktrans_plus1_p3 / ((Cai_ktrans_plus1_p3 * Cai_ktrans_plus1 + L / pow((float)(1.0 + Cai * inv_kact), na) ) * (1.0 - exp(-(FRT2 * ( Dpsi - 91.0 ))))));"
     
     
     "((Cai*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na)*Vmuni_ktrans)/(L+(Cai_ktrans_plus1*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na))))"
     "((Cai*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na)*Vmuni_ktrans)/(L+(Cai_ktrans_plus1*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na))))"
     "((Cai*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na)*Vmuni_ktrans*FRT2)/(2.0*(L+(Cai_ktrans_plus1*Cai_ktrans_plus1_p3*pow(((Cai*inv_kact)+1.0),na)))))"

     ;; Need to expand out algebra so that variable that approaches singular value is explicit.
     ;; Want to specify simply the variable in question, the singular value to take the limit, and the range we want to extrapolate over
     
     "((Vmuni_ktrans*Cai*FRT2_Dpsi*Cai_ktrans_plus1_p3)/(((Cai_ktrans_plus1_p3*Cai_ktrans_plus1)+(L/(1.0+(Cai*inv_kact))^na))*(1.0-exp((-FRT2_Dpsi)))))"

     )
