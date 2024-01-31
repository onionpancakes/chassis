(ns dev.onionpancakes.chassis.core
  (:require [clojure.string]))

(defprotocol AttributeValue
  (append-attribute-to-string-builder [this sb attr-name]))

(defprotocol AttributeValueToken
  (append-attribute-value-space-for-next? [this])
  (append-attribute-value-fragment-to-string-builder [this sb]))

(defprotocol Token
  (append-fragment-to-string-builder [this sb])
  (fragment ^String [this]))

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

(defn append-string-builder-fragment
  [sb token]
  (append-fragment-to-string-builder token sb))

(defn html
  [root]
  (let [sb (StringBuilder. 16384)
        _  (reduce-node append-string-builder-fragment sb root)]
    (.toString sb)))

;; Serializer

(deftype TokenSerializer [root]
  clojure.lang.IReduceInit
  (reduce [_ rf init]
    (reduce-node rf init root))
  clojure.lang.Seqable
  (seq [this]
    (->> (tree-seq branch? children root)
         (remove branch?))))

(defn token-serializer
  [root]
  (TokenSerializer. root))

(defn html-serializer
  [root]
  (eduction (map fragment)
            (TokenSerializer. root)))

;; Attributes impl

(defn escape-attribute-value
  ^String
  [^String s]
  (.. s
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")
      (replace "\"" "&quot;")
      (replace "'" "&apos;")))

(defn append-string-builder-attribute-kv-except-id-class
  [^StringBuilder sb k v]
  (when-not (or (identical? k :class)
                (identical? k :id)
                (namespace k))
    (append-attribute-to-string-builder v sb (name k)))
  sb)

(defn append-string-builder-attribute-map-except-id-class
  [^StringBuilder sb ^clojure.lang.IKVReduce attrs]
  (if attrs
    (.kvreduce attrs append-string-builder-attribute-kv-except-id-class sb)))

(extend-protocol AttributeValue
  Boolean
  (append-attribute-to-string-builder [this ^StringBuilder sb attr-name]
    (when this
      (.append sb " ")
      (.append sb attr-name))
    sb)
  Object
  (append-attribute-to-string-builder [this ^StringBuilder sb attr-name]
    (.append sb " ")
    (.append sb attr-name)
    (.append sb "=\"")
    (append-attribute-value-fragment-to-string-builder this sb)
    (.append sb "\""))
  nil
  (append-attribute-to-string-builder [_ sb _] sb))

(extend-protocol AttributeValueToken
  clojure.lang.MapEntry
  (append-attribute-value-space-for-next? [this]
    (append-attribute-value-space-for-next? (val this)))
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (when (append-attribute-value-space-for-next? (val this))
      (append-attribute-value-fragment-to-string-builder (key this) sb)
      (.append sb ": ")
      (append-attribute-value-fragment-to-string-builder (val this) sb)
      (.append sb ";"))
    sb)
  clojure.lang.IPersistentMap
  (append-attribute-value-space-for-next? [this]
    (and (pos? (.count this))
         (append-attribute-value-space-for-next? (val (first this)))))
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (let [append-space (volatile! false)]
      (doseq [t this]
        (if @append-space
          (when (append-attribute-value-space-for-next? t)
            (.append sb " "))
          (vreset! append-space true))
        (append-attribute-value-fragment-to-string-builder t sb)))
    sb)
  clojure.lang.IPersistentSet
  (append-attribute-value-space-for-next? [this]
    (and (pos? (.count this))
         (append-attribute-value-space-for-next? (first this))))
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (let [append-space (volatile! false)]
      (doseq [t this]
        (if @append-space
          (when (append-attribute-value-space-for-next? t)
            (.append sb " "))
          (vreset! append-space true))
        (append-attribute-value-fragment-to-string-builder t sb)))
    sb)
  clojure.lang.IPersistentVector
  (append-attribute-value-space-for-next? [this]
    (and (pos? (.count this))
         (append-attribute-value-space-for-next? (.nth this 0))))
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (loop [idx 0 cnt (.count this)]
      (when (< idx cnt)
        (let [x (.nth this idx)]
          (when (and (pos? idx) (append-attribute-value-space-for-next? x))
            (.append sb " "))
          (append-attribute-value-fragment-to-string-builder x sb)
          (recur (inc idx) cnt))))
    sb)
  clojure.lang.Keyword
  (append-attribute-value-space-for-next? [this]
    (not (namespace this)))
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (if-not (namespace this)
      (.append sb (escape-attribute-value (.getName this)))
      ;; Handle namespaced keywords as special attribute values?
      )
    sb)
  String
  (append-attribute-value-space-for-next? [this] true)
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (.append sb (escape-attribute-value this)))
  Object
  (append-attribute-value-space-for-next? [this] true)
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (.append sb (escape-attribute-value (.toString this))))
  Boolean
  (append-attribute-value-space-for-next? [_] false)
  (append-attribute-value-fragment-to-string-builder [_ sb] sb)
  nil
  (append-attribute-value-space-for-next? [this] false)
  (append-attribute-value-fragment-to-string-builder [_ sb] sb))

;; Token impl

(defn escape-text
  ^String
  [^String s]
  (.. s
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")))

(deftype OpeningTag [tag tag-id tag-class attrs]
  Token
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb "<")
    (.append ^StringBuilder sb (name tag))
    (if tag-id
      (append-attribute-to-string-builder tag-id sb "id")
      (if (contains? attrs :id)
        (append-attribute-to-string-builder (get attrs :id) sb "id")))
    (if tag-class
      (if (contains? attrs :class)
        (append-attribute-to-string-builder [tag-class (get attrs :class)] sb "class")
        (append-attribute-to-string-builder tag-class sb "class"))
      (if (contains? attrs :class)
        (append-attribute-to-string-builder (get attrs :class) sb "class")))
    (append-string-builder-attribute-map-except-id-class sb attrs)
    (.append ^StringBuilder sb ">"))
  (fragment [this]
    (let [sb (StringBuilder. 64)
          _  (.append-fragment-to-string-builder this sb)]
      (.toString sb)))
  Node
  (branch? [this] false)
  (children [this] [])
  Object
  (toString [this]
    (fragment this)))

(deftype ClosingTag [tag]
  Token
  (append-fragment-to-string-builder [this sb]
    (.. ^StringBuilder sb
        (append "</")
        (append (name tag))
        (append ">")))
  (fragment [this]
    (let [sb (StringBuilder.)
          _  (.append-fragment-to-string-builder this sb)]
      (.toString sb)))
  Node
  (branch? [this] false)
  (children [this] [])
  Object
  (toString [this]
    (fragment this)))

(deftype Raw [token]
  AttributeValueToken
  (append-attribute-value-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (str token)))
  Token
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (str token)))
  (fragment [this]
    (str token))
  Node
  (branch? [this] false)
  (children [this] [])
  Object
  (toString [_]
    (str token)))

(defn raw
  [token]
  (Raw. token))

(extend-protocol Token
  String
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (escape-text this)))
  (fragment [this]
    (escape-text this))
  Object
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (escape-text (.toString this))))
  (fragment [this]
    (escape-text (.toString this)))
  nil
  (append-fragment-to-string-builder [this sb] sb)
  (fragment [_] ""))

;; Node impl

;; https://developer.mozilla.org/en-US/docs/Glossary/Void_element

(defn void-tag?
  [tag]
  (case tag
    (:area
     :base
     :br
     :col
     :embed
     :hr
     :img
     :input
     :link
     :meta
     :param
     :source
     :track
     :wbr) true
    false))

(defn base-tag
  [^clojure.lang.Keyword tag]
  (let [tname  (.getName tag)
        id-idx (.indexOf tname (int \#))
        cl-idx (.indexOf tname (int \.))]
    (if (pos? cl-idx)
      (if (pos? id-idx)
        (clojure.lang.Keyword/intern (.substring tname 0 (min id-idx cl-idx)))
        (clojure.lang.Keyword/intern (.substring tname 0 cl-idx)))
      (if (pos? id-idx)
        (clojure.lang.Keyword/intern (.substring tname 0 id-idx))
        tag))))

(defn tag-id
  [^clojure.lang.Keyword tag]
  (let [tname     (.getName tag)
        start-idx (.indexOf tname (int \#))
        end-idx   (.indexOf tname (int \.) start-idx)]
    (if (pos? start-idx)
      (if (pos? end-idx)
        (.substring tname (inc start-idx) end-idx)
        (.substring tname (inc start-idx))))))

(defn tag-class
  [^clojure.lang.Keyword tag]
  (let [tname     (.getName tag)
        start-idx (.indexOf tname (int \.))
        end-idx   (.indexOf tname (int \#) start-idx)]
    (if (pos? start-idx)
      (if (pos? end-idx)
        (.. tname
            (substring (inc start-idx) end-idx)
            (replace "." " "))
        (.. tname
            (substring (inc start-idx))
            (replace "." " "))))))

(defn element-children-0
  [elem]
  [])

(defn element-children-1
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)]
    (if (void-tag? tag)
      [(OpeningTag. tag tid tcl nil)]
      [(OpeningTag. tag tid tcl nil)
       (ClosingTag. tag)])))

(defn element-children-2-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    (if (void-tag? tag)
      [(OpeningTag. tag tid tcl attrs)]
      [(OpeningTag. tag tid tcl attrs)
       (ClosingTag. tag)])))

(defn element-children-2
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)]
   (if (and (void-tag? tag)
            (nil? (.nth elem 1)))
     [(OpeningTag. tag tid tcl nil)]
     [(OpeningTag. tag tid tcl nil)
      (.nth elem 1)
      (ClosingTag. tag)])))

(defn element-children-3-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-3
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-n-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (next (next elem))
     (ClosingTag. tag)]))

(defn element-children-n
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
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
