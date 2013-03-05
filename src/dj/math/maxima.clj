(ns dj.math.maxima
  (:require [clojure.java.shell :as cjs]
            [dj.plurality :as dp]))

(defn call-maxima* [commands]
  (cjs/sh "maxima"
          "--very-quiet"
          :in commands))

(defn call-maxima [commands]
  (let [{:keys [out err]} (call-maxima* (str "display2d:false;" commands))]
    (if (empty? err)
      (-> (.split #"\n" out)
          last)
      (throw (Exception. err)))))



#_ (do
     (call-maxima "diff(x^2,x);")
     
     (let [es "(Vmuni_ktrans * Cai * FRT2_Dpsi * Cai_ktrans_plus1_p3 / ((Cai_ktrans_plus1_p3 * Cai_ktrans_plus1 + L / pow((float)(1.0 + Cai * inv_kact), na) ) * (1.0 - exp(-FRT2_Dpsi))));"
           ce (dj.math.cemit/c-emitter {"exp" "exp"})
           me (dp/update-implementation ce
                                        assoc
                                        "pow"
                                        (fn [emit]
                                          (fn [{:keys [op children]}]
                                            (let [[base exponent] children]
                                              (str (emit base)
                                                   "^"
                                                   (emit exponent))))))]
       (-> es
           dj.math.parser/parse
           :result
           me))
     "((Vmuni_ktrans*Cai*FRT2_Dpsi*Cai_ktrans_plus1_p3)/(((Cai_ktrans_plus1_p3*Cai_ktrans_plus1)+(L/pow((1.0+(Cai*inv_kact)),na)))*(1.0-exp((-FRT2_Dpsi)))))"

     )
