(require '[dj.cljs.install])
(dj.cljs.install/add-cljs-to-classpath!)
(require '[dj.cljs])
(def app (dj.cljs/->cljs-browser-env {:port 9000
                                      :working-dir "tmp/dj.view"}))
(dj.repl/start app)

(dj.dependencies/resolve-project "dj.view")
(load "dj/view"
      "dj/view/cljs")
(def e (dj.view/generator dj.view/emit))
(def re (dj.view/generator dj.view/raw-emit))
(def de (dj.view.cljs/generator app (dj.view.cljs/feed-out
                                     dj.view/emit)))
(def dre (dj.view.cljs/generator app (dj.view.cljs/feed-out
                                      dj.view/raw-emit)))
(def store (atom []))
(def ! (dj.repl/map-logger store))

(dj.dependencies/resolve-project "dj.math")
(dj.dependencies/resolve-project "dj.cuda")

(load "dj/cuda/translator"
      "dj/math/expression"
      "dj/math/matrix"
      "dj/math/differentiation")
(let [data (-> {:model-file (dj.io/file "dj/cuda/models/mitomodel.c")
                :record-dt 100
                :dt 10
                :end-time 1000}
               dj.cuda.translator/full-model-config)
      shared-algebra-dep-map (-> data
                                 :shared-algebra-assignment
                                 dj.cuda.translator/pairs->map
                                 keys
                                 (zipmap (repeat #{})))
      state-vars (:state-vars data)
      state-vars-dep-map (zipmap state-vars (repeat #{}))
      algebra-exp-map (-> (:algebra-assignment data)
                          dj.cuda.translator/pairs->map
                          (dj/update-vals (comp :result dj.math.parser/parse)))
      differential-exp-map (-> (:differential-assignment data)
                               dj.cuda.translator/pairs->map
                               (dj/update-vals (comp :result dj.math.parser/parse)))
      dep-map (-> algebra-exp-map
                  (dj/update-vals dj.math.expression/direct-dependents)
                  (dj.math.expression/has-dependents state-vars)
                  (merge shared-algebra-dep-map
                         state-vars-dep-map))
      sd (dj.math.differentiation/symbolic-lookup-differentiation algebra-exp-map
                                                                  dep-map)
      jacobian-map (reduce (fn [m sv]
                             (assoc m
                               sv
                               (dj/update-vals differential-exp-map
                                               sd
                                               {:variable sv})))
                           {}
                           state-vars)
      sjt (dj.math.matrix/symbolic-jacobian-template jacobian-map
                                                     state-vars)]
  #_ (-> jacobian-map
      (dj/update-vals #(dj/update-vals % (comp str dj.math.parser/emit)))
      e)
  (-> sjt
      dj.math.example/qr-decomp'
      dj.math.parser/emit
      pr-str
      re)
  #_ (-> sjt
      (.vvm)
      re)
  #_ (-> dep-map
      (select-keys (dj.math.expression/direct-dependents (algebra-exp-map "VANT")))
      e)
  #_ (-> differential-exp-map
      (dj/update-vals sd {:variable "ADPm"})
      e)
  #_ (-> algebra-exp-map
      (dj/update-vals sd {:variable "ADPm"})
      e))

(dre (:error (dj.io/capture-out-err (clojure.repl/pst))))
(dre nil)

(require '[datomic.api :as d])
(dre (seq (d/q '[:find ?ret ?g 
                :where
                 [?e :g ?g]
                 [?e :return ?ret]]
              @store)))
(dre @store)

(reset! store [])

(load "dj/math/example")


(let [[m0 m1 m2 m3 m4 m5 m6 m7 m8 m9] [12 -51 4 6 167 -68 -4 24 -41]]
  (let [m0_00001 (- m0 (Math/copySign (Math/sqrt (+ (+ (Math/pow m0 2) (Math/pow m3 2)) (Math/pow m6 2))) m0))]
    (let [m0_00004 (/ m0_00001 (Math/sqrt (+ (+ (Math/pow m0_00001 2) (Math/pow m1_00002 2)) (Math/pow m2_00003 2)))) m1_00005 (/ m1_00002 (Math/sqrt (+ (+ (Math/pow m0_00001 2) (Math/pow m1_00002 2)) (Math/pow m2_00003 2)))) m2_00006 (/ m2_00003 (Math/sqrt (+ (+ (Math/pow m0_00001 2) (Math/pow m1_00002 2)) (Math/pow m2_00003 2))))]
      (let [m0_00007 (+ (+ (* (- (* (* m1_00005 m0_00004) 2)) m1) (* (- 1 (* (* m1_00005 m1_00005) 2)) m4)) (* (- (* (* m1_00005 m2_00006) 2)) m7)) m1_00008 (+ (+ (* (- (* (* m1_00005 m0_00004) 2)) m2) (* (- 1 (* (* m1_00005 m1_00005) 2)) m5)) (* (- (* (* m1_00005 m2_00006) 2)) m8)) m2_00009 (+ (+ (* (- (* (* m2_00006 m0_00004) 2)) m1) (* (- (* (* m2_00006 m1_00005) 2)) m4)) (* (- 1 (* (* m2_00006 m2_00006) 2)) m7)) m3_0000A (+ (+ (* (- (* (* m2_00006 m0_00004) 2)) m2) (* (- (* (* m2_00006 m1_00005) 2)) m5)) (* (- 1 (* (* m2_00006 m2_00006) 2)) m8)) m0_0000B (- 1 (* (* m0_00004 m0_00004) 2)) m1_0000C (- (* (* m1_00005 m0_00004) 2)) m2_0000D (- (* (* m2_00006 m0_00004) 2)) m3_0000E (- (* (* m0_00004 m1_00005) 2)) m4_0000F (- 1 (* (* m1_00005 m1_00005) 2)) m5_00010 (- (* (* m2_00006 m1_00005) 2)) m6_00011 (- (* (* m0_00004 m2_00006) 2)) m7_00012 (- (* (* m1_00005 m2_00006) 2)) m8_00013 (- 1 (* (* m2_00006 m2_00006) 2)) m0_00014 (Math/copySign (Math/sqrt (+ (+ (Math/pow m0 2) (Math/pow m3 2)) (Math/pow m6 2))) m0) m1_00015 (+ (+ (* (- 1 (* (* m0_00004 m0_00004) 2)) m1) (* (- (* (* m0_00004 m1_00005) 2)) m4)) (* (- (* (* m0_00004 m2_00006) 2)) m7)) m2_00016 (+ (+ (* (- 1 (* (* m0_00004 m0_00004) 2)) m2) (* (- (* (* m0_00004 m1_00005) 2)) m5)) (* (- (* (* m0_00004 m2_00006) 2)) m8)) m4_00018 (+ (+ (* (- (* (* m1_00005 m0_00004) 2)) m1) (* (- 1 (* (* m1_00005 m1_00005) 2)) m4)) (* (- (* (* m1_00005 m2_00006) 2)) m7)) m5_00019 (+ (+ (* (- (* (* m1_00005 m0_00004) 2)) m2) (* (- 1 (* (* m1_00005 m1_00005) 2)) m5)) (* (- (* (* m1_00005 m2_00006) 2)) m8)) m7_0001B (+ (+ (* (- (* (* m2_00006 m0_00004) 2)) m1) (* (- (* (* m2_00006 m1_00005) 2)) m4)) (* (- 1 (* (* m2_00006 m2_00006) 2)) m7)) m8_0001C (+ (+ (* (- (* (* m2_00006 m0_00004) 2)) m2) (* (- (* (* m2_00006 m1_00005) 2)) m5)) (* (- 1 (* (* m2_00006 m2_00006) 2)) m8))]
        (let [m0_0001D (- m0_00007 (Math/copySign (Math/sqrt (+ (Math/pow m0_00007 2) (Math/pow m2_00009 2))) m0_00007))]
          (let [m0_0001F (/ m0_0001D (Math/sqrt (+ (Math/pow m0_0001D 2) (Math/pow m1_0001E 2)))) m1_00020 (/ m1_0001E (Math/sqrt (+ (Math/pow m0_0001D 2) (Math/pow m1_0001E 2))))]
            (let [m0_00021 (+ (* (- (* (* m1_00020 m0_0001F) 2)) m1_00008) (* (- 1 (* (* m1_00020 m1_00020) 2)) m3_0000A)) m1_00023 (+ (* m1_0000C (- 1 (* (* m0_0001F m0_0001F) 2))) (* m2_0000D (- (* (* m0_0001F m1_00020) 2)))) m2_00024 (+ (* m1_0000C (- (* (* m1_00020 m0_0001F) 2))) (* m2_0000D (- 1 (* (* m1_00020 m1_00020) 2)))) m4_00026 (+ (* m4_0000F (- 1 (* (* m0_0001F m0_0001F) 2))) (* m5_00010 (- (* (* m0_0001F m1_00020) 2)))) m5_00027 (+ (* m4_0000F (- (* (* m1_00020 m0_0001F) 2))) (* m5_00010 (- 1 (* (* m1_00020 m1_00020) 2)))) m7_00029 (+ (* m7_00012 (- 1 (* (* m0_0001F m0_0001F) 2))) (* m8_00013 (- (* (* m0_0001F m1_00020) 2)))) m8_0002A (+ (* m7_00012 (- (* (* m1_00020 m0_0001F) 2))) (* m8_00013 (- 1 (* (* m1_00020 m1_00020) 2)))) m4_0002F (Math/copySign (Math/sqrt (+ (Math/pow m0_00007 2) (Math/pow m2_00009 2))) m0_00007) m5_00030 (+ (* (- 1 (* (* m0_0001F m0_0001F) 2)) m1_00008) (* (- (* (* m0_0001F m1_00020) 2)) m3_0000A)) m8_00033 (+ (* (- (* (* m1_00020 m0_0001F) 2)) m1_00008) (* (- 1 (* (* m1_00020 m1_00020) 2)) m3_0000A))]
              {:R [[m0_0002B m1_0002C m2_0002D] [0 m4_0002F m5_00030] [0 0 m0_00021]], :Q [[m0_00022 m1_00023 m2_00024] [m3_00025 m4_00026 m5_00027] [m6_00028 m7_00029 m8_0002A]]})))))))


max-key
