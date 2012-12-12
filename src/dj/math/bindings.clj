(ns dj.math.bindings)

(deftype bindings
    [pairs bmap]
  Object
  (toString [this]
    (str {:pairs pairs
          :bmap bmap}))
  clojure.lang.IPersistentMap 
  clojure.lang.ILookup 
  (valAt [this key]
    (get bmap key)) 
  (valAt [this key notfound]
    (get bmap key notfound))
  clojure.lang.IFn
  (invoke [this arg]
    (get bmap arg))
  clojure.lang.IPersistentCollection 
  (count [this] (count pairs)) 
  (empty [this] (bindings. [] {})) 
  (equiv [this gs] (or (identical? this gs)
                       (when (identical? (class this)
                                         (class gs))
                         (and (= pairs
                                 (.pairs gs))
                              (= bmap
                                 (.bmap gs))))))
  clojure.lang.Associative 
  (containsKey [this k]
    (.containsKey bmap k))
  (entryAt [this k]
    (get bmap k))
  clojure.lang.Seqable
  (seq [this] (seq pairs)))

(defmethod clojure.core/print-method bindings [o w]
  (.write w "<dj.math.bindings.bindings\n[\n")
  (.write w (.toString o))
  (.write w "\n]>"))

;; work out literal syntax for this
(defmethod clojure.core/print-dup bindings [o w] 
  (.write w "#dj.math.bindings.bindings")
  (.write w (.toString o)))

(defn pairs->bindings [pairs]
  (bindings. (vec pairs)
             (reduce (fn [m [k v]]
                       (assoc m k v))
                     {}
                     pairs)))

(defn zip->bindings
  "
takes two sequences and creates a bindings from x->y
"
  [x y]
  (pairs->bindings (map vector x y)))

(defn append [bs k v]
  (bindings. (conj (.pairs bs)
                   [k v])
             (assoc (.bmap bs)
               k
               v)))

(defn prepend [bs k v]
  (bindings. (into [[k v]]
                   (.pairs bs))
             (assoc (.bmap bs)
               k
               v)))

(defn join
  ([x] x)
  ([x & args]
     (reduce (fn [bs bs-x]
               (if bs-x
                 (let [pbs (.pairs bs)
                       bbs (.bmap bs)
                       pbsx (.pairs bs-x)
                       bbsx (.bmap bs-x)]
                   (bindings. (into pbs pbsx)
                              (merge bbs bbsx)))
                 bs))
             x
             args)))

