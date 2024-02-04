(ns dev.onionpancakes.chassis.core)

(defprotocol AttributeValue
  (append-attribute-fragment-to-string-builder [this sb attr-name]))

(defprotocol AttributeValueToken
  (attribute-value-fragment ^String [this]))

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

(extend-protocol AttributeValue
  clojure.lang.Keyword
  (append-attribute-fragment-to-string-builder [this ^StringBuilder sb attr-name]
    (if (namespace this)
      nil ;; Handle namespaced keywords?
      (let [val-frag (attribute-value-fragment this)]
        (.append sb " ")
        (.append sb attr-name)
        (.append sb "=\"")
        (.append sb val-frag)
        (.append sb "\"")))
    sb)
  Boolean
  (append-attribute-fragment-to-string-builder [this ^StringBuilder sb attr-name]
    (when this
      (.append sb " ")
      (.append sb attr-name))
    sb)
  Object
  (append-attribute-fragment-to-string-builder [this ^StringBuilder sb attr-name]
    (let [val-frag (attribute-value-fragment this)]
      (.append sb " ")
      (.append sb attr-name)
      (.append sb "=\"")
      (.append sb val-frag)
      (.append sb "\"")))
  nil
  (append-attribute-fragment-to-string-builder [_ sb _] sb))

(defn append-sb
  ([^StringBuilder sb] sb)
  ([^StringBuilder sb ^String s]
   (.append sb s)))

(defn join-attribute-value-fragment-kv
  [^StringBuilder sb k v]
  (when-some [v-frag (attribute-value-fragment v)]
    (let [k-frag (escape-attribute-value-fragment (name k))]
      (if (pos? (.length sb))
        (do
          (.append sb " ")
          (.append sb k-frag)
          (.append sb ": ")
          (.append sb v-frag)
          (.append sb ";"))
        (do
          (.append sb k-frag)
          (.append sb ": ")
          (.append sb v-frag)
          (.append sb ";")))))
  sb)

(extend-protocol AttributeValueToken
  java.util.Collection
  (attribute-value-fragment [this]
    (let [sb (StringBuilder.)
          xf (comp (keep attribute-value-fragment)
                   (interpose " "))
          _  (transduce xf append-sb sb this)]
      (.toString sb)))
  java.util.Map
  (attribute-value-fragment [this]
    (let [sb (StringBuilder.)
          _  (reduce-kv join-attribute-value-fragment-kv sb this)]
      (.toString sb)))
  java.util.UUID
  (attribute-value-fragment [this]
    ;; Not escaped. Should be safe.
    (.toString this))
  Number
  (attribute-value-fragment [this]
    ;; Not escaped. Should be safe.
    (.toString this))
  String
  (attribute-value-fragment [this]
    (escape-attribute-value-fragment this))
  Object
  (attribute-value-fragment [this]
    (escape-attribute-value-fragment (.toString this)))
  Boolean
  (attribute-value-fragment [this] nil)
  nil
  (attribute-value-fragment [this] nil))

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

(defn append-attribute-fragment-kv-except-id-class
  [^StringBuilder sb k v]
  (when-not (or (nil? v)
                (identical? k :class)
                (identical? k :id)
                (and (keyword? k) (namespace k)))
    (append-attribute-fragment-to-string-builder v sb (name k)))
  sb)

(defn append-attribute-fragment-kv-except-id
  [^StringBuilder sb k v]
  (when-not (or (nil? v)
                (identical? k :id)
                (and (keyword? k) (namespace k)))
    (append-attribute-fragment-to-string-builder v sb (name k)))
  sb)

(defn append-attribute-fragment-kv-except-class
  [^StringBuilder sb k v]
  (when-not (or (nil? v)
                (identical? k :class)
                (and (keyword? k) (namespace k)))
    (append-attribute-fragment-to-string-builder v sb (name k)))
  sb)

(defn append-attribute-fragment-kv
  [^StringBuilder sb k v]
  (when-not (or (nil? v)
                (and (keyword? k) (namespace k)))
    (append-attribute-fragment-to-string-builder v sb (name k)))
  sb)

(defn append-opening-tag-with-id-class-attrs-id-class
  [^StringBuilder sb tag-name tag-id tag-class ^java.util.Map attrs]
  (let [tag-id-frag    (escape-attribute-value-fragment tag-id)
        tag-class-frag (escape-attribute-value-fragment tag-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-id-class-attrs-id
  [^StringBuilder sb tag-name tag-id tag-class ^java.util.Map attrs]
  (let [tag-id-frag    (escape-attribute-value-fragment tag-id)
        tag-class-frag (escape-attribute-value-fragment tag-class)]
    (if (== (.size attrs) 1)
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb tag-id-frag)
        (.append sb "\" class=\"")
        (.append sb tag-class-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb tag-id-frag)
        (.append sb "\" class=\"")
        (.append sb tag-class-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-id-class-attrs-class
  [^StringBuilder sb tag-name tag-id tag-class ^java.util.Map attrs]
  (let [tag-id-frag    (escape-attribute-value-fragment tag-id)
        tag-class-frag (escape-attribute-value-fragment tag-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-id-class-attrs
  [^StringBuilder sb tag-name tag-id tag-class ^java.util.Map attrs]
  (let [tag-id-frag    (escape-attribute-value-fragment tag-id)
        tag-class-frag (escape-attribute-value-fragment tag-class)]
    (if (zero? (.size attrs))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb tag-id-frag)
        (.append sb "\" class=\"")
        (.append sb tag-class-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb tag-id-frag)
        (.append sb "\" class=\"")
        (.append sb tag-class-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-id-class
  [^StringBuilder sb tag-name tag-id tag-class]
  (let [tag-id-frag    (escape-attribute-value-fragment tag-id)
        tag-class-frag (escape-attribute-value-fragment tag-class)]
    (.append sb "<")
    (.append sb tag-name)
    (.append sb " id=\"")
    (.append sb tag-id-frag)
    (.append sb "\" class=\"")
    (.append sb tag-class-frag)
    (.append sb "\">")))

(defn append-opening-tag-with-id-attrs-id-class
  [^StringBuilder sb tag-name tag-id ^java.util.Map attrs]
  (let [tag-id-frag (escape-attribute-value-fragment tag-id)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb attr-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-id-attrs-id
  [^StringBuilder sb tag-name tag-id ^java.util.Map attrs]
  (let [tag-id-frag (escape-attribute-value-fragment tag-id)]
    (if (== (.size attrs) 1)
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb tag-id-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb tag-id-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-id-attrs-class
  [^StringBuilder sb tag-name tag-id ^java.util.Map attrs]
  (let [tag-id-frag (escape-attribute-value-fragment tag-id)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\" class=\"")
          (.append sb attr-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb tag-id-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-id-attrs
  [^StringBuilder sb tag-name tag-id tag-class ^java.util.Map attrs]
  (let [tag-id-frag (escape-attribute-value-fragment tag-id)]
    (if (zero? (.size attrs))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb tag-id-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb tag-id-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-id
  [^StringBuilder sb tag-name tag-id]
  (let [tag-id-frag (escape-attribute-value-fragment tag-id)]
    (.append sb "<")
    (.append sb tag-name)
    (.append sb " id=\"")
    (.append sb tag-id-frag)
    (.append sb "\">")))

(defn append-opening-tag-with-class-attrs-id-class
  [^StringBuilder sb tag-name tag-class ^java.util.Map attrs]
  (let [tag-class-frag (escape-attribute-value-fragment tag-class)]
    (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
      (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
        (if (== (.size attrs) 2)
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " id=\"")
            (.append sb attr-id-frag)
            (.append sb "\" class=\"")
            (.append sb tag-class-frag)
            (.append sb " ")
            (.append sb attr-class-frag)
            (.append sb "\">"))
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " id=\"")
            (.append sb attr-id-frag)
            (.append sb "\" class=\"")
            (.append sb tag-class-frag)
            (.append sb " ")
            (.append sb attr-class-frag)
            (.append sb "\"")
            (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
            (.append sb ">")))
        (if (== (.size attrs) 2)
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " id=\"")
            (.append sb attr-id-frag)
            (.append sb "\" class=\"")
            (.append sb tag-class-frag)
            (.append sb "\">"))
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " id=\"")
            (.append sb attr-id-frag)
            (.append sb "\" class=\"")
            (.append sb tag-class-frag)
            (.append sb "\"")
            (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
            (.append sb ">"))))
      (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
        (if (== (.size attrs) 2)
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " class=\"")
            (.append sb tag-class-frag)
            (.append sb " ")
            (.append sb attr-class-frag)
            (.append sb "\">"))
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " class=\"")
            (.append sb tag-class-frag)
            (.append sb " ")
            (.append sb attr-class-frag)
            (.append sb "\"")
            (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
            (.append sb ">")))
        (if (== (.size attrs) 2)
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " class=\"")
            (.append sb tag-class-frag)
            (.append sb "\">"))
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " class=\"")
            (.append sb tag-class-frag)
            (.append sb "\"")
            (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
            (.append sb ">")))))))

(defn append-opening-tag-with-class-attrs-id
  [^StringBuilder sb tag-name tag-class ^java.util.Map attrs]
  (let [tag-class-frag (escape-attribute-value-fragment tag-class)]
    (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb attr-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb attr-id-frag)
          (.append sb "\" class=\"")
          (.append sb tag-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb tag-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb tag-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-class-attrs-class
  [^StringBuilder sb tag-name tag-class ^java.util.Map attrs]
  (let [tag-class-frag (escape-attribute-value-fragment tag-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb tag-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb tag-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb tag-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb tag-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-class-attrs
  [^StringBuilder sb tag-name tag-class ^java.util.Map attrs]
  (let [tag-class-frag (escape-attribute-value-fragment tag-class)]
    (if (zero? (.size attrs))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " class=\"")
        (.append sb tag-class-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " class=\"")
        (.append sb tag-class-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-class
  [^StringBuilder sb tag-name tag-class]
  (let [tag-class-frag (escape-attribute-value-fragment tag-class)]
    (.append sb "<")
    (.append sb tag-name)
    (.append sb " class=\"")
    (.append sb tag-class-frag)
    (.append sb "\">")))

(defn append-opening-tag-with-attrs-id-class
  [^StringBuilder sb tag-name ^java.util.Map attrs]
  (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb attr-id-frag)
          (.append sb "\" class=\"")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb attr-id-frag)
          (.append sb "\" class=\"")
          (.append sb attr-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb attr-id-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb attr-id-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb attr-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb ">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-attrs-id
  [^StringBuilder sb tag-name ^java.util.Map attrs]
  (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
    (if (== (.size attrs) 1)
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb attr-id-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb attr-id-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
        (.append sb ">")))
    (if (== (.size attrs) 1)
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb ">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-attrs-class
  [^StringBuilder sb tag-name ^java.util.Map attrs]
  (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
    (if (== (.size attrs) 1)
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " class=\"")
        (.append sb attr-class-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " class=\"")
        (.append sb attr-class-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
        (.append sb ">")))
    (if (== (.size attrs) 1)
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb ">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-attrs
  [^StringBuilder sb tag-name ^java.util.Map attrs]
  (if (zero? (.size attrs))
    (do
      (.append sb "<")
      (.append sb tag-name)
      (.append sb ">"))
    (do
      (.append sb "<")
      (.append sb tag-name)
      (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
      (.append sb ">"))))

(defn append-opening-tag
  [^StringBuilder sb tag-name]
  (.append sb "<")
  (.append sb tag-name)
  (.append sb ">"))

(deftype OpeningTag [^clojure.lang.Keyword tag tag-id tag-class ^java.util.Map attrs]
  Token
  (append-fragment-to-string-builder [this sb]
    (let [tag-name (.getName tag)]
      (if tag-id
        (if tag-class
          ;; +tag-id, +tag-class
          (if attrs
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-class-attrs-id-class sb tag-name tag-id tag-class attrs)
                (append-opening-tag-with-id-class-attrs-id sb tag-name tag-id tag-class attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-class-attrs-class sb tag-name tag-id tag-class attrs)
                (append-opening-tag-with-id-class-attrs sb tag-name tag-id tag-class attrs)))
            (append-opening-tag-with-id-class sb tag-name tag-id tag-class))
          ;; +tag-id, -tag-class
          (if attrs
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-attrs-id-class sb tag-name tag-class attrs)
                (append-opening-tag-with-id-attrs-id sb tag-name tag-class attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-attrs-class sb tag-name tag-class attrs)
                (append-opening-tag-with-id-attrs sb tag-name tag-class attrs)))
            (append-opening-tag-with-id sb tag-name tag-id)))
        (if tag-class
          ;; -tag-id, +tag-class
          (if attrs
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-class-attrs-id-class sb tag-name tag-class attrs)
                (append-opening-tag-with-class-attrs-id sb tag-name tag-class attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-class-attrs-class sb tag-name tag-class attrs)
                (append-opening-tag-with-class-attrs sb tag-name tag-class attrs)))
            (append-opening-tag-with-class sb tag-name tag-class))
          ;; -tag-id, -tag-class
          (if attrs
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-attrs-id-class sb tag-name attrs)
                (append-opening-tag-with-attrs-id sb tag-name attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-attrs-class sb tag-name attrs)
                (append-opening-tag-with-attrs sb tag-name attrs)))
            (append-opening-tag sb tag-name))))))
  (fragment [this]
    (let [sb (StringBuilder. 64)
          _  (.append-fragment-to-string-builder this sb)]
      (.toString sb)))
  Object
  (toString [this]
    (fragment this)))

(deftype ClosingTag [^clojure.lang.Keyword tag]
  Token
  (append-fragment-to-string-builder [this sb]
    (let [tag-name (.getName tag)]
      (.append ^StringBuilder sb "</")
      (.append ^StringBuilder sb tag-name)
      (.append ^StringBuilder sb ">")))
  (fragment [this]
    (let [sb (StringBuilder.)
          _  (.append-fragment-to-string-builder this sb)]
      (.toString sb)))
  Object
  (toString [this]
    (fragment this)))

(deftype RawString [value]
  AttributeValueToken
  (attribute-value-fragment [this] (str value))
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
  java.util.UUID
  (append-fragment-to-string-builder [this sb]
    ;; Not escaped. Should be safe.
    (.append ^StringBuilder sb (.toString this)))
  (fragment [this]
    (.toString this))
  Number
  (append-fragment-to-string-builder [this sb]
    ;; Not escaped. Should be safe.
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

(defn head-tag
  [^clojure.lang.Keyword head]
  (let [head-name (.getName head)
        id-idx    (.indexOf head-name 35 #_(int \#))
        cl-idx    (.indexOf head-name 46 #_(int \.))]
    (if (pos? cl-idx)
      (if (pos? id-idx)
        (clojure.lang.Keyword/intern (.substring head-name 0 (min id-idx cl-idx)))
        (clojure.lang.Keyword/intern (.substring head-name 0 cl-idx)))
      (if (pos? id-idx)
        (clojure.lang.Keyword/intern (.substring head-name 0 id-idx))
        head))))

(defn head-id
  [^clojure.lang.Keyword head]
  (let [head-name (.getName head)
        start-idx (.indexOf head-name 35 #_(int \#))
        end-idx   (.indexOf head-name 46 #_(int \.) start-idx)]
    (if (pos? start-idx)
      (if (pos? end-idx)
        (.substring head-name (inc start-idx) end-idx)
        (.substring head-name (inc start-idx))))))

(defn head-class
  [^clojure.lang.Keyword head]
  (let [head-name (.getName head)
        start-idx (.indexOf head-name 46 #_(int \.))
        end-idx   (.indexOf head-name 35 #_(int \#) start-idx)]
    (if (pos? start-idx)
      (if (pos? end-idx)
        (.. head-name
            (substring (inc start-idx) end-idx)
            (replace \. \space))
        (.. head-name
            (substring (inc start-idx))
            (replace \. \space))))))

(defn element-children-0
  [elem]
  [])

(defn element-children-1
  [^clojure.lang.Indexed elem]
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    (if (void-tag? tag)
      [(OpeningTag. tag id cls nil)]
      [(OpeningTag. tag id cls nil)
       (ClosingTag. tag)])))

(defn element-children-2-attrs
  [^clojure.lang.Indexed elem]
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    (if (void-tag? tag)
      [(OpeningTag. tag id cls attrs)]
      [(OpeningTag. tag id cls attrs)
       (ClosingTag. tag)])))

(defn element-children-2
  [^clojure.lang.Indexed elem]
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
   (if (and (void-tag? tag)
            (nil? (.nth elem 1)))
     [(OpeningTag. tag id cls nil)]
     [(OpeningTag. tag id cls nil)
      (.nth elem 1)
      (ClosingTag. tag)])))

(defn element-children-3-attrs
  [^clojure.lang.Indexed elem]
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-3
  [^clojure.lang.Indexed elem]
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
     (.nth elem 1)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-4-attrs
  [^clojure.lang.Indexed elem]
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
     (.nth elem 2)
     (.nth elem 3)
     (ClosingTag. tag)]))

(defn element-children-4
  [^clojure.lang.Indexed elem]
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (ClosingTag. tag)]))

(defn element-children-5-attrs
  [^clojure.lang.Indexed elem]
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (ClosingTag. tag)]))

(defn element-children-5
  [^clojure.lang.Indexed elem]
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (ClosingTag. tag)]))

(defn element-children-6-attrs
  [^clojure.lang.Indexed elem]
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (ClosingTag. tag)]))

(defn element-children-6
  [^clojure.lang.Indexed elem]
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (ClosingTag. tag)]))

(defn element-children-7-attrs
  [^clojure.lang.Indexed elem]
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (ClosingTag. tag)]))

(defn element-children-7
  [^clojure.lang.Indexed elem]
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (ClosingTag. tag)]))

(defn element-children-8-attrs
  [^clojure.lang.Indexed elem]
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (ClosingTag. tag)]))

(defn element-children-8
  [^clojure.lang.Indexed elem]
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
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
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
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
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
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
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
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
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
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

(defn element-children-n-attrstcl
  [^clojure.lang.Indexed elem]
  (let [head  (.nth elem 0)
        tag   (head-tag head)
        id    (head-id head)
        cls   (head-class head)
        attrs (.nth elem 1)]
    [(OpeningTag. tag id cls attrs)
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
  (let [head (.nth elem 0)
        tag  (head-tag head)
        id   (head-id head)
        cls  (head-class head)]
    [(OpeningTag. tag id cls nil)
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
    (let [head (.nth this 0 nil)]
      (if (and (keyword? head) (nil? (namespace head)))
        (if (instance? java.util.Map (.nth this 1 nil))
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
        this)))
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
