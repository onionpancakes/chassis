(ns dev.onionpancakes.chassis.core)

(defprotocol Node
  (branch? [this])
  (children ^Iterable [this]))

(defprotocol Token
  (fragment [this]))

;; Serializers

(deftype TokenSerializer [root]
  clojure.lang.IReduceInit
  (reduce [this rf init]
    (let [stack (java.util.ArrayDeque. 32)]
      (loop [cur (.iterator ^Iterable (list root)) ret init]
        (if cur
          (if (.hasNext cur)
            (let [node (.next cur)]
              (if (branch? node)
                (do
                  (.addFirst stack cur)
                  (recur (.iterator (children node)) ret))
                (recur cur (rf ret node))))
            (recur (.pollFirst stack) ret))
          ret))))
  clojure.lang.ISeq
  (seq [this]
    (->> (tree-seq branch? children root)
         (remove branch?))))

(defn token-serializer
  [root]
  (TokenSerializer. root))

(defn html-serializer
  [root]
  (eduction (map fragment)
            (token-serializer root)))

;; Node impl

(defn element-children-0
  [elem]
  [])

(defn element-children-1
  [elem]
  (let [tag (.nth ^clojure.lang.Indexed elem 0)]
    [::begin-open tag ::end-open ::begin-close tag ::end-close]))

(defn element-children-2-attrs
  [elem]
  (let [tag   (.nth ^clojure.lang.Indexed elem 0)
        attrs (.nth ^clojure.lang.Indexed elem 1)]
    [::begin-open tag attrs ::end-open
     ::begin-close tag ::end-close]))

(defn element-children-2
  [elem]
  (let [tag (.nth ^clojure.lang.Indexed elem 0)]
    [::begin-open tag ::end-open
     (.nth ^clojure.lang.Indexed elem 1)
     ::begin-close tag ::end-close]))

(defn element-children-3-attrs
  [elem]
  (let [tag   (.nth ^clojure.lang.Indexed elem 0)
        attrs (.nth ^clojure.lang.Indexed elem 1)]
    [::begin-open tag attrs ::end-open
     (.nth ^clojure.lang.Indexed elem 2)
     ::begin-close tag ::end-close]))

(defn element-children-3
  [elem]
  (let [tag (.nth ^clojure.lang.Indexed elem 0)]
    [::begin-open tag ::end-open
     (.nth ^clojure.lang.Indexed elem 1)
     (.nth ^clojure.lang.Indexed elem 2)
     ::begin-close tag ::end-close]))

(defn element-children-n
  [elem]
  (let [tag   (.nth ^clojure.lang.Indexed elem 0)
        anode (.nth ^clojure.lang.Indexed elem 1)]
    (if (map? anode)
      [::begin-open tag anode ::end-open
       (drop 2 elem)
       ::begin-close tag ::end-close]
      [::begin-open tag ::end-open
       (drop 1 elem)
       ::begin-close tag ::end-close])))

(defn element-children
  [elem]
  (case (* (.count ^clojure.lang.Counted elem)
           (if (map? (.nth ^clojure.lang.Indexed elem 1 nil)) -1 1))
    0  (element-children-0 elem)
    -1 (element-children-1 elem)
    1  (element-children-1 elem)
    -2 (element-children-2-attrs elem)
    2  (element-children-2 elem)
    -3 (element-children-3-attrs elem)
    3  (element-children-3 elem)
    (element-children-n elem)))

(defn constantly-true
  [_]
  true)

(defn constantly-false
  [_]
  false)

(defn constantly-empty
  [_]
  [])

(extend clojure.lang.IPersistentVector
  Node
  {:branch?  constantly-true
   :children element-children})

(extend clojure.lang.IPersistentMap
  Node
  {:branch?  constantly-false
   :children constantly-empty})

(extend clojure.lang.ISeq
  Node
  {:branch?  constantly-true
   :children identity})

(extend clojure.lang.Keyword
  Node
  {:branch?  constantly-false
   :children constantly-empty})

(extend Object
  Node
  {:branch?  constantly-false
   :children constantly-empty})

(extend nil
  Node
  {:branch?  constantly-false
   :children constantly-empty})

;; Token impl

(extend-protocol Token
  clojure.lang.Keyword
  (fragment [this]
    (case this
      ::begin-open  "<"
      ::end-open    ">"
      ::begin-close "</"
      ::end-close   ">"
      (name this)))
  String
  (fragment [this] this)
  Object
  (fragment [this] (.toString this))
  nil
  (fragment [_] ""))

(defn attribute-fragment-rf
  [^StringBuilder sb k v]
  (doto sb
    (.append " ")
    (.append (name k))
    (.append "=\"")
    (.append v)
    (.append "\"")))

(defn attribute-fragment
  [m]
  (let [sb (StringBuilder.)
        _  (reduce-kv attribute-fragment-rf sb m)]
    (.toString sb)))

(extend clojure.lang.IPersistentMap
  Token
  {:fragment attribute-fragment})
