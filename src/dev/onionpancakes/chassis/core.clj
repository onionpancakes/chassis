(ns dev.onionpancakes.chassis.core)

(defprotocol AttributeValue
  (append-attribute-to-string-builder [this sb attr-name]))

(defprotocol AttributeValueToken
  (append-attribute-value-space-for-next? [this])
  (append-attribute-value-fragment-to-string-builder [this sb]))

(defprotocol Token
  (append-fragment-to-string-builder [this sb])
  (fragment ^String [this]))

(defprotocol Node
  (children ^Iterable [this]))

;; Implementation notes:
;; - HTML serialization is implemented as depth first search (DFS) traversal over Node.
;; - DFS is used to emit a series of Tokens (leaf nodes from the Node HTML tree).
;; - Tokens then write HTML fragments to a singular StringBuilder.
;; For performance reasons:
;; - DFS is implemented as reduction over Node.
;; - DFS is implemented using a stack of Iterators. (java.util.Deque<Iterator>)
;;   Note that head of stack is held by loop binding rather than the actual head of stack.
;; - Node children returns a value that is as flat as possible
;;   to minimized the depth of search (size of Deque).
;;   - See the count varying node-children-n implementations
;; - Iterables returned by Node children should prefer implementations
;;   that are internally indexes to arrays.
;;   - Iterators over Vectors are fast, Iterators over Seqs are not as fast.
;; - DFS emits a minimum number of Tokens. Therefore Node children emits
;;   OpeningTag and ClosingTag types as "fat" tokens capturing the bracket,
;;   tag name, and tag attributes data into one Token instance.
;; - Tokens append fragment Strings to StringBuilder. Testing (adhoc in my repl) showed
;;   that String appends to StringBuilder is the fastest implementation possible.
;;   - It beat out CharSequence appends to Appendable. (2nd fastest, ~20% slower)
;;   - It beat out String writes to Writer. (2nd fastest, ~20% slower)
;;   - It beat out String prints to PrintStream. (slowest, +100% slower)
;;   Ultimately, HTML fragments are written to some server I/O interface.
;;   Appending fragments to StringBuilder seems wasteful and although
;;   it is possible to avoid intermediate memory allocation and I/O
;;   by avoiding StringBuilder, it is not a performant option at this time.

;; Reduce / HTML

(defn reduce-node
  [rf init root]
  (let [stack (java.util.ArrayDeque. 32)]
    (loop [cur (.iterator ^Iterable (vector root)) ret init]
      (if (reduced? ret)
        (.deref ^clojure.lang.IDeref ret)
        (if cur
          (if (.hasNext cur)
            (let [node (.next cur)
                  ch   (children node)]
              (if ch
                (do
                  (.addFirst stack cur)
                  (recur (.iterator ch) ret))
                (recur cur (rf ret node))))
            (recur (.pollFirst stack) ret))
          ret)))))

(defn append-fragment
  [sb token]
  (append-fragment-to-string-builder token sb))

(defn html
  [root]
  (let [sb (StringBuilder. 16384)
        _  (reduce-node append-fragment sb root)]
    (.toString sb)))

;; Serializer

(deftype TokenSerializer [root]
  clojure.lang.IReduceInit
  (reduce [_ rf init]
    (reduce-node rf init root))
  clojure.lang.Seqable
  (seq [this]
    (seq (vec this))))

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

(defn escape-attribute-value-fragment
  ^String
  [s]
  (escape-attribute-value s))

(defn append-string-builder-attribute-kv-except-id-class
  [^StringBuilder sb k v]
  (when-not (or (identical? k :class)
                (identical? k :id)
                (and (keyword? k) (namespace k)))
    (append-attribute-to-string-builder v sb (name k)))
  sb)

(defn append-string-builder-attribute-map-except-id-class
  [^StringBuilder sb ^clojure.lang.IKVReduce attrs]
  (if attrs
    (.kvreduce attrs append-string-builder-attribute-kv-except-id-class sb)))

(deftype TagId [tag-id]
  AttributeValue
  (append-attribute-to-string-builder [_ sb _]
    (.append ^StringBuilder sb " id=\"")
    (.append ^StringBuilder sb (escape-attribute-value-fragment tag-id))
    (.append ^StringBuilder sb "\"")))

(deftype TagClass [tag-class attr-class]
  AttributeValue
  (append-attribute-to-string-builder [_ sb _]
    (.append ^StringBuilder sb " class=\"")
    (.append ^StringBuilder sb (escape-attribute-value-fragment tag-class))
    ;; ::none is needed to tell whether attr-class is absent from the the attr map.
    ;; nil is insufficient since it represents both absence and presence (with nil value).
    (when-not (identical? attr-class ::none)
      (.append ^StringBuilder sb " ")
      (append-attribute-value-fragment-to-string-builder attr-class sb))
    (.append ^StringBuilder sb "\"")))

(extend-protocol AttributeValue
  clojure.lang.Keyword
  (append-attribute-to-string-builder [this ^StringBuilder sb attr-name]
    (if (namespace this)
      nil ;; Handle namespaced keywords?
      (do
        (.append sb " ")
        (.append sb attr-name)
        (.append sb "=\"")
        (append-attribute-value-fragment-to-string-builder this sb)
        (.append sb "\"")))
    sb)
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
    (when (val this)
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
        (let [t (.nth this idx)]
          (when (and (pos? idx) (append-attribute-value-space-for-next? t))
            (.append sb " "))
          (append-attribute-value-fragment-to-string-builder t sb)
          (recur (inc idx) cnt))))
    sb)
  clojure.lang.Keyword
  (append-attribute-value-space-for-next? [this]
    (not (namespace this)))
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (if (namespace this)
      nil ;; Handle namespaced keywords as special attribute values?
      (.append sb (escape-attribute-value-fragment (.getName this))))
    sb)
  ;; Not escaped. Should be safe.
  java.util.UUID
  (append-attribute-value-space-for-next? [this] true)
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (.append sb (.toString this)))
  ;; Not escaped. Should be safe.
  Number
  (append-attribute-value-space-for-next? [this] true)
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (.append sb (.toString this)))
  String
  (append-attribute-value-space-for-next? [this] true)
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (.append sb (escape-attribute-value-fragment this)))
  Object
  (append-attribute-value-space-for-next? [this] true)
  (append-attribute-value-fragment-to-string-builder [this ^StringBuilder sb]
    (.append sb (escape-attribute-value-fragment (.toString this))))
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

(defn escape-text-fragment
  ^String
  [s]
  (escape-text s))

(deftype OpeningTag [tag tag-id tag-class attrs]
  Token
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb "<")
    (.append ^StringBuilder sb (name tag))
    (if tag-id
      (append-attribute-to-string-builder (TagId. tag-id) sb "id")
      (if (contains? attrs :id)
        (append-attribute-to-string-builder (get attrs :id) sb "id")))
    (if tag-class
      (append-attribute-to-string-builder (TagClass. tag-class (get attrs :class ::none)) sb "class")
      (if (contains? attrs :class)
        (append-attribute-to-string-builder (get attrs :class) sb "class")))
    (append-string-builder-attribute-map-except-id-class sb attrs)
    (.append ^StringBuilder sb ">"))
  (fragment [this]
    (let [sb (StringBuilder. 64)
          _  (.append-fragment-to-string-builder this sb)]
      (.toString sb)))
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
  Object
  (toString [this]
    (fragment this)))

(deftype RawString [value]
  AttributeValueToken
  (append-attribute-value-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (str value)))
  Token
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (str value)))
  (fragment [this]
    (str value))
  Object
  (toString [this]
    (fragment this)))

(defn raw-string
  [value]
  (RawString. value))

(defn raw
  [value]
  (raw-string value))

(extend-protocol Token
  ;; Not escaped. Should be safe.
  java.util.UUID
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (.toString this)))
  (fragment [this]
    (.toString this))
  ;; Not escaped. Should be safe.
  Number
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (.toString this)))
  (fragment [this]
    (.toString this))
  String
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (escape-text-fragment this)))
  (fragment [this]
    (escape-text-fragment this))
  Object
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (escape-text-fragment (.toString this))))
  (fragment [this]
    (escape-text-fragment (.toString this)))
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
        id-idx (.indexOf tname 35 #_(int \#))
        cl-idx (.indexOf tname 46 #_(int \.))]
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
        start-idx (.indexOf tname 35 #_(int \#))
        end-idx   (.indexOf tname 46 #_(int \.) start-idx)]
    (if (pos? start-idx)
      (if (pos? end-idx)
        (.substring tname (inc start-idx) end-idx)
        (.substring tname (inc start-idx))))))

(defn tag-class
  [^clojure.lang.Keyword tag]
  (let [tname     (.getName tag)
        start-idx (.indexOf tname 46 #_(int \.))
        end-idx   (.indexOf tname 35 #_(int \#) start-idx)]
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
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
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
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
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
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-4-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (.nth elem 2)
     (.nth elem 3)
     (ClosingTag. tag)]))

(defn element-children-4
  [^clojure.lang.Indexed elem]
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (ClosingTag. tag)]))

(defn element-children-5-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (ClosingTag. tag)]))

(defn element-children-5
  [^clojure.lang.Indexed elem]
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (ClosingTag. tag)]))

(defn element-children-6-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (ClosingTag. tag)]))

(defn element-children-6
  [^clojure.lang.Indexed elem]
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (ClosingTag. tag)]))

(defn element-children-7-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (ClosingTag. tag)]))

(defn element-children-7
  [^clojure.lang.Indexed elem]
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (ClosingTag. tag)]))

(defn element-children-8-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (ClosingTag. tag)]))

(defn element-children-8
  [^clojure.lang.Indexed elem]
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (ClosingTag. tag)]))

(defn element-children-9-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (.nth elem 8)
     (ClosingTag. tag)]))

(defn element-children-9
  [^clojure.lang.Indexed elem]
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (.nth elem 8)
     (ClosingTag. tag)]))

(defn element-children-10-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (.nth elem 8)
     (.nth elem 9)
     (ClosingTag. tag)]))

(defn element-children-10
  [^clojure.lang.Indexed elem]
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (.nth elem 8)
     (.nth elem 9)
     (ClosingTag. tag)]))

(defn element-children-n-attrs
  [^clojure.lang.Indexed elem]
  (let [tic   (.nth elem 0)
        tag   (base-tag tic)
        tid   (tag-id tic)
        tcl   (tag-class tic)
        attrs (.nth elem 1)]
    [(OpeningTag. tag tid tcl attrs)
     ;; Emit a coll Node in the element body.
     ;; Note that this increases depth of search by 2 instead of 1.
     ;; Use Subvec iterators as they are faster than Seq iterators.
     ;; Reify as anon Node to avoid being interpreted as element.
     (reify Node
       (children [_]
         (subvec elem 2)))
     (ClosingTag. tag)]))

(defn element-children-n
  [^clojure.lang.Indexed elem]
  (let [tic (.nth elem 0)
        tag (base-tag tic)
        tid (tag-id tic)
        tcl (tag-class tic)]
    [(OpeningTag. tag tid tcl nil)
     ;; Emit a coll Node in the element body.
     ;; Note that this increases depth of search by 2 instead of 1.
     ;; Use Subvec iterators as they are faster than Seq iterators.
     ;; Reify as anon Node to avoid being interpreted as element.
     (reify Node
       (children [_]
         (subvec elem 1)))
     (ClosingTag. tag)]))

(extend-protocol Node
  clojure.lang.IPersistentVector
  (children [this]
    (if (keyword? (.nth this 0 nil))
      (if (map? (.nth this 1 nil))
        (case (.count this)
          2 (element-children-2-attrs this)
          3 (element-children-3-attrs this)
          4 (element-children-4-attrs this)
          5 (element-children-5-attrs this)
          6 (element-children-6-attrs this)
          7 (element-children-7-attrs this)
          8 (element-children-8-attrs this)
          9 (element-children-9-attrs this)
          10 (element-children-10-attrs this)
          (element-children-n-attrs this))
        (case (.count this)
          0 (element-children-0 this)
          1 (element-children-1 this)
          2 (element-children-2 this)
          3 (element-children-3 this)
          4 (element-children-4 this)
          5 (element-children-5 this)
          6 (element-children-6 this)
          7 (element-children-7 this)
          8 (element-children-8 this)
          9 (element-children-9 this)
          10 (element-children-10 this)
          (element-children-n this)))
      this))
  clojure.lang.ISeq
  (children [this] this)
  clojure.lang.IDeref
  (children [this]
    (vector (.deref this)))
  clojure.lang.Fn
  (children [this]
    (vector (this)))
  Object
  (children [_] nil)
  nil
  (children [_] nil))

;; Raw consts

(def doctype-html5
  (raw "<!DOCTYPE html>"))
