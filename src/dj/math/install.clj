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
  (-> jacobian-map
      (dj/update-vals #(dj/update-vals % (comp str dj.math.parser/emit)))
      e)
  #_ (-> sjt
      (dj.math.example/solve' (dj.math.matrix/t (dj.math.matrix/v [(mapv (fn [x]
                                                                           (dj.math.parser/s {:variable x})) state-vars)])))
      dj.math.cemit/emit
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
(dre "asdf")

(require '[datomic.api :as d])
(dre (let [s (seq (d/q '[:find ?ret
                         :where
                         [?e :pos :k-check]
                         [?e :v ?ret]]
                       @store))]
       [s (count s)]))

(reset! store [])

(load "dj/math/example")

(dj.math.example/test-run)

(defn stop [_]
  (throw (Exception. "stop")))

(defn id? [obj id]
  (= (:id (meta obj))
     id))
(defn ifstop [bool' obj]
  (if bool'
    (throw (Exception. "stop"))
    obj))

(defn ifprint [bool' obj]
  (if bool'
    (do (user/dre (pr-str obj))
        obj)
    obj))

(defn ifdo [bool' obj]
  (if bool'
    (do (def user/ret obj)
        obj)
    obj))

(defn run []
  (load "dj/math"
        "dj/math/example")
  (dj.math.example/test-run))

(defn add-meta [obj m]
  (let [m' (meta obj)]
    (with-meta obj (merge m' m))))

(user/run)

(str ret)
(load "dj/math/cemit")