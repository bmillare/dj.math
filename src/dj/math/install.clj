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

(load "dj/math/linearalgebra")

(dj.math.linearalgebra/test-run)

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
        "dj/math/linearalgebra")
  (dj.math.linearalgebra/test-run))

(defn add-meta [obj m]
  (let [m' (meta obj)]
    (with-meta obj (merge m' m))))

(user/run)

(str ret)
(load "dj/math/cemit")

(e (dj.git/changed-projects))
