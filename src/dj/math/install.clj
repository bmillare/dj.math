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
(def dhe (dj.view.cljs/generator app (dj.view.cljs/feed-out
                                      identity)))
(def dfe (dj.view.cljs/generator app (dj.view.cljs/feed-out
                                      dj.view/iframe-emit)))

(dj.dependencies/resolve-project "dj.math")
(dj.dependencies/resolve-project "dj.cuda")

(load "dj/cuda/translator"
      "dj/math/expression"
      "dj/math/matrix"
      "dj/math/differentiation"
      "dj/math/linearalgebra")

(load "dj/cuda/install")
(dj.cuda.install/load-jcuda
 {:architecture :64
  :version "5.0"
  :os :linux})

(require '[datomic.api :as d])

(dj.dependencies/resolve-project "dj.plot")
(load "dj/cuda"
      "dj/plot")

(de (dj.git/changed-projects))

(dj.repl/deftracer t (dj.repl/->trace-walker (dj.repl/->tuple-trace-logger 'user/v)
                                             2))
(defn get-val [code d]
  (-> (d/q
       '[:find ?v
         :in $ ?code
         :where
         [?id :result ?v]
         [?id :code ?c]
         [(= ?c ?code)]]
       d
       code)
      vec
      first
      first))

(def v (atom []))
@v

(let [settings {:num-nodes 1
                :dt 1.0
                :record-dt 40.0
                :end-time 4000.0
                :model-file (dj.io/file "dj/cuda/models/stiff.model")

                ;; cuda specific
                :kernel-name "mitomodel"
                :cuda-device-id 0
                :ptxas-path "/usr/local/cuda/bin/ptxas"
                :nvcc-path "/usr/local/cuda/bin/nvcc"
         
                :integrator
                #_ dj.cuda.translator/forward-euler-integrator-fn
                (dj.cuda.translator/->rosenbrock-wrap-integrator-fn dj.cuda.translator/backward-formulation)}
      r (dj.cuda.translator/full-model-config settings)
      initial-conditions (mapv (fn [n_idx]
                                 (reduce (fn [m [s v]]
                                           (assoc m
                                             (keyword s)
                                             (Double/parseDouble (dj/substring v
                                                                               0
                                                                               -1))))
                                         {}
                                         (:initial-conditions-assignment r)))
                               (range (:num-nodes r)))
      r #_ (let [r (dj.cuda.translator/->emit-lisp-solver r)
              r (eval (r initial-conditions))
              r (merge settings
                       {:records r})
              r (dj.cuda.translator/lisp-plot-format r)]
          (first r))
      (-> r
          dj.cuda/compile-run
          dj.cuda/plot-format
          first)]
  (let [
        r (select-keys r ["x" "z"])
        r (reduce (fn [m k]
                    (-> m
                        (assoc-in [k :width]
                                  3)
                        (assoc-in [k :series-label]
                                  (str k))))
                  r
                  (keys r))
        r (dj.plot/multiplot {:title (dj.io/get-path (:model-file settings))
                              :x-label "time"
                              :y-label "z(t)"}
                             (vals r))
        r (dj.plot/chart->image r 640 480)
        r (dj.plot/image->png-binary r)]
    (dhe {:tag :img
          :attrs {:src (dj.view/inline-binary-url r "image/png")}}))
  
  )