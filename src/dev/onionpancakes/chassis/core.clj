(ns dev.onionpancakes.chassis.core
  (:require [clojure.string]))

(defprotocol AttributeValue
  (append-attribute-to-string-builder [this sb attr-name]))

(defprotocol AttributeValueToken
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

(defn append-string-builder-attribute-kv
  [^StringBuilder sb k v]
  (when-not (namespace k)
    (append-attribute-to-string-builder v sb (name k)))
  sb)

(defn append-string-builder-attributes
  [^StringBuilder sb ^clojure.lang.IKVReduce attrs]
  (if attrs
    (.kvreduce attrs append-string-builder-attribute-kv sb))
  sb)

(extend-protocol AttributeValue
  Object
  (append-attribute-to-string-builder [this ^StringBuilder sb attr-name]
    (.append sb " ")
    (.append sb attr-name)
    (.append sb "=\"")
    (append-attribute-value-fragment-to-string-builder this sb)
    (.append sb "\"")
    sb)
  nil
  (append-attribute-to-string-builder [_ sb _] sb))

(extend-protocol AttributeValueToken
  clojure.lang.IPersistentVector
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (loop [i 0 cnt (count this)]
      (when (< i cnt)
        (if (pos? i)
          (.append sb " "))
        (-> (.nth this i)
            (append-attribute-value-fragment-to-string-builder sb))
        (recur (inc i) cnt)))
    sb)
  String
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (.append sb (escape-attribute-value this)))
  Object
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (.append sb (escape-attribute-value (.toString this))))
  nil
  (append-attribute-value-fragment-to-string-builder [_ sb] sb))

;; Token impl

(defn escape-text
  ^String
  [^String s]
  (.. s
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")))

(deftype OpeningTag [tag ^clojure.lang.IKVReduce attrs]
  Token
  (append-fragment-to-string-builder [this sb]
    (doto ^StringBuilder sb
      (.append "<")
      (.append (name tag))
      (append-string-builder-attributes attrs)
      (.append ">")))
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

(defn tag-classes
  [^clojure.lang.Keyword tag]
  (let [tname     (.getName tag)
        start-idx (.indexOf tname (int \.))
        end-idx   (.indexOf tname (int \#) start-idx)
        class-str (if (pos? start-idx)
                    (if (pos? end-idx)
                      (.substring tname (inc start-idx) end-idx)
                      (.substring tname (inc start-idx))))]
    (clojure.string/split #"." class-str)))

(defn element-children-0
  [elem]
  [])

(defn element-children-1
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        attrs (cond-> {}
                tid (assoc :id tid))]
    (if (void-tag? tag)
      [(OpeningTag. tag attrs)]
      [(OpeningTag. tag attrs)
       (ClosingTag. tag)])))

(defn element-children-2-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        amap  (.nth elem 1)
        attrs (cond-> amap
                tid (assoc :id tid))]
    (if (void-tag? tag)
      [(OpeningTag. tag attrs)]
      [(OpeningTag. tag attrs)
       (ClosingTag. tag)])))

(defn element-children-2
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        attrs (cond-> {}
                tid (assoc :id tid))]
   (if (and (void-tag? tag)
            (nil? (.nth elem 1)))
     [(OpeningTag. tag attrs)]
     [(OpeningTag. tag attrs)
      (.nth elem 1)
      (ClosingTag. tag)])))

(defn element-children-3-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        amap  (.nth elem 1)
        attrs (cond-> amap
                tid (assoc :id tid))]
    [(OpeningTag. tag attrs)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-3
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        attrs (cond-> {}
                tid (assoc :id tid))]
    [(OpeningTag. tag nil)
     (.nth elem 1)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-n-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        amap  (.nth elem 1)
        attrs (cond-> amap
                tid (assoc :id tid))]
    [(OpeningTag. tag attrs)
     (next (next elem))
     (ClosingTag. tag)]))

(defn element-children-n
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        attrs (cond-> {}
                tid (assoc :id tid))]
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
