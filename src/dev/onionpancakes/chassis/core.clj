(ns dev.onionpancakes.chassis.core)

(defprotocol Token
  (fragment ^String [this])
  (append-fragment-to-appendable [this appendable]))

(defprotocol Node
  (branch? [this])
  (children ^Iterable [this]))

;; Reduce

(defn reduce-node
  [rf init root]
  (let [stack (java.util.ArrayDeque. 32)]
    (loop [cur (.iterator ^Iterable (list root)) ret init]
      (if (reduced? ret)
        (.deref ^clojure.lang.IDeref ret)
        (if cur
          (if (.hasNext cur)
            (let [node (.next cur)]
              (if (branch? node)
                (do
                  (.addFirst stack cur)
                  (recur (.iterator (children node)) ret))
                (recur cur (rf ret node))))
            (recur (.pollFirst stack) ret))
          ret)))))

(defn append-fragment
  [appendable token]
  (append-fragment-to-appendable token appendable))

(defn html
  [root]
  (let [sb (StringBuilder. 16384)
        _  (reduce-node append-fragment sb root)]
    (.toString sb)))

;; Token impl

(defn escape-text
  ^String
  [^String s]
  (.. s
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")))

(defn escape-attr-value
  ^String
  [^String s]
  (.. s
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")
      (replace "\"" "&quot;")
      (replace "'" "&apos;")))

(defn append-attr-kv
  [^Appendable appendable k v]
  (.. appendable
      (append " ")
      (append (name k))
      (append "=\"")
      (append (escape-attr-value v))
      (append "\"")))

(deftype OpeningTag [tag ^clojure.lang.IKVReduce attrs]
  Token
  (fragment [this]
    (let [sb (StringBuilder. 64)
          _  (.append sb "<")
          _  (.append sb (name tag))
          _  (if attrs
               (.kvreduce attrs append-attr-kv sb))
          _  (.append sb ">")]
      (.toString sb)))
  (append-fragment-to-appendable [this appendable]
    (.append ^Appendable appendable "<")
    (.append ^Appendable appendable (name tag))
    (if attrs
      (.kvreduce attrs append-attr-kv appendable))
    (.append ^Appendable appendable ">")
    appendable)
  Node
  (branch? [this] false)
  (children [this] [])
  Object
  (toString [this]
    (fragment this)))

(deftype ClosingTag [tag]
  Token
  (fragment [this]
    (.. (StringBuilder.)
        (append "</")
        (append (name tag))
        (append ">")
        (toString)))
  (append-fragment-to-appendable [this appendable]
    (.. ^Appendable appendable
        (append "</")
        (append (name tag))
        (append ">")))
  Node
  (branch? [this] false)
  (children [this] [])
  Object
  (toString [this]
    (fragment this)))

(extend-protocol Token
  String
  (fragment [this]
    (escape-text this))
  (append-fragment-to-appendable [this appendable]
    (.append ^Appendable appendable (escape-text this)))
  Object
  (fragment [this]
    (escape-text (.toString this)))
  (append-fragment-to-appendable [this appendable]
    (.append ^Appendable appendable (escape-text (.toString this))))
  nil
  (fragment [_] "")
  (append-fragment-to-appendable [this appendable] appendable))

;; Node impl

(defn element-children-0
  [elem]
  [])

(defn element-children-1
  [^clojure.lang.Indexed elem]
  (let [tag (.nth elem 0)]
    [(OpeningTag. tag nil)
     (ClosingTag. tag)]))

(defn element-children-2-attrs
  [^clojure.lang.Indexed elem]
  (let [tag   (.nth elem 0)
        attrs (.nth elem 1)]
    [(OpeningTag. tag attrs)
     (ClosingTag. tag)]))

(defn element-children-2
  [^clojure.lang.Indexed elem]
  (let [tag (.nth elem 0)]
    [(OpeningTag. tag nil)
     (.nth elem 1)
     (ClosingTag. tag)]))

(defn element-children-3-attrs
  [^clojure.lang.Indexed elem]
  (let [tag   (.nth elem 0)
        attrs (.nth elem 1)]
    [(OpeningTag. tag attrs)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-3
  [^clojure.lang.Indexed elem]
  (let [tag (.nth elem 0)]
    [(OpeningTag. tag nil)
     (.nth elem 1)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-n-attrs
  [^clojure.lang.Indexed elem]
  (let [tag   (.nth elem 0)
        attrs (.nth elem 1)]
    [(OpeningTag. tag attrs)
     (next (next elem))
     (ClosingTag. tag)]))

(defn element-children-n
  [^clojure.lang.Indexed elem]
  (let [tag (.nth elem 0)]
    [(OpeningTag. tag nil)
     (next elem)
     (ClosingTag. tag)]))

(extend-protocol Node
  clojure.lang.IPersistentVector
  (branch? [_] true)
  (children [this]
    (if (keyword? (.nth this 0 nil))
      (if (map? (.nth this 1 nil))
        (case (.count this)
          2 (element-children-2-attrs this)
          3 (element-children-3-attrs this)
          (element-children-n-attrs this))
        (case (.count this)
          0 (element-children-0 this)
          1 (element-children-1 this)
          2 (element-children-2 this)
          3 (element-children-3 this)
          (element-children-n this)))
      this))
  clojure.lang.ISeq
  (branch? [_] true)
  (children [this] this)
  Object
  (branch? [_] false)
  (children [_] [])
  nil
  (branch? [_] false)
  (children [_] []))
