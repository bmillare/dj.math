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
(dj.dependencies/resolve-project "gnuplot")
(load "gnuplot")
(def fe gnuplot/emit-static-plot)
(def de (dj.view.cljs/generator app (dj.view.cljs/feed-out
                                     dj.view/emit)))
(def dre (dj.view.cljs/generator app (dj.view.cljs/feed-out
                                      dj.view/raw-emit)))
(def dfe (dj.view.cljs/generator app (dj.view.cljs/feed-out
                                      dj.view/iframe-emit)))

(dj.dependencies/resolve-project "dj.math")
(dj.dependencies/resolve-project "dj.cuda")

(load "dj/cuda/translator"
      "dj/math/expression"
      "dj/math/matrix"
      "dj/math/differentiation"
      "dj/math/linearalgebra")

(require '[datomic.api :as d])
(dre (let [s (seq (d/q '[:find ?ret
                         :where
                         [?e :pos :k-check]
                         [?e :v ?ret]]
                       @store))]
       [s (count s)]))

(de (dj.git/changed-projects))
(def v (atom []))

(dj.repl/deftracer t (dj.repl/->trace-walker (dj.repl/->simple-trace-logger 'user/v)
                                             2))

