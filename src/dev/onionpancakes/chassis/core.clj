(ns dev.onionpancakes.chassis.core)

(defprotocol Token
  (fragment [this]))

(defprotocol Node
  (branch? [this])
  (children ^Iterable [this]))

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

;; Token impl

(defn append-attr-kv
  [^StringBuilder sb k v]
  (.. sb
      (append " ")
      (append (name k))
      (append "=\"")
      (append v)
      (append "\"")))

(deftype OpeningTag [tag attrs]
  Token
  (fragment [this]
    (let [sb (StringBuilder.)
          _  (.append sb "<")
          _  (.append sb (name tag))
          _  (reduce-kv append-attr-kv sb attrs)
          _  (.append sb ">")]
      (.toString sb)))
  Node
  (branch? [this] false)
  (children [this] []))

(deftype ClosingTag [tag]
  Token
  (fragment [this]
    (.. (StringBuilder.)
        (append "</")
        (append (name tag))
        (append ">")
        (toString)))
  Node
  (branch? [this] false)
  (children [this] []))

(extend-protocol Token
  String
  (fragment [this] this)
  Object
  (fragment [this] (.toString this))
  nil
  (fragment [_] ""))

;; Node impl

(defn element-children-0
  [elem]
  [])

(defn element-children-1
  [elem]
  (let [tag (.nth ^clojure.lang.Indexed elem 0)]
    [(OpeningTag. tag nil)
     (ClosingTag. tag)]))

(defn element-children-2-attrs
  [elem]
  (let [tag   (.nth ^clojure.lang.Indexed elem 0)
        attrs (.nth ^clojure.lang.Indexed elem 1)]
    [(OpeningTag. tag attrs)
     (ClosingTag. tag)]))

(defn element-children-2
  [elem]
  (let [tag (.nth ^clojure.lang.Indexed elem 0)]
    [(OpeningTag. tag nil)
     (.nth ^clojure.lang.Indexed elem 1)
     (ClosingTag. tag)]))

(defn element-children-3-attrs
  [elem]
  (let [tag   (.nth ^clojure.lang.Indexed elem 0)
        attrs (.nth ^clojure.lang.Indexed elem 1)]
    [(OpeningTag. tag attrs)
     (.nth ^clojure.lang.Indexed elem 2)
     (ClosingTag. tag)]))

(defn element-children-3
  [elem]
  (let [tag (.nth ^clojure.lang.Indexed elem 0)]
    [(OpeningTag. tag nil)
     (.nth ^clojure.lang.Indexed elem 1)
     (.nth ^clojure.lang.Indexed elem 2)
     (ClosingTag. tag)]))

(defn element-children-n
  [elem]
  (let [tag   (.nth ^clojure.lang.Indexed elem 0)
        anode (.nth ^clojure.lang.Indexed elem 1)]
    (if (map? anode)
      [(OpeningTag. tag anode)
       (drop 2 elem)
       (ClosingTag. tag)]
      [(OpeningTag. tag nil)
       (drop 1 elem)
       (ClosingTag. tag)])))

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


