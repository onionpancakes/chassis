(ns dev.onionpancakes.chassis.core)

(defprotocol AttributeValue
  (attribute-fragment-append-to [this sb attr-name] "Appends attribute key and value html fragment."))

(defprotocol AttributeValueFragment
  (attribute-value-fragment ^String [this] "Returns attribute value fragment string or nil if none."))

(defprotocol Token
  (fragment-append-to [this sb] "Appends html fragment.")
  (fragment ^String [this] "Returns HTML fragment."))

(defprotocol Node
  (branch? [this] "Returns true if branch node.")
  (children ^Iterable [this] "Returns children as Iterable."))

(defmulti resolve-alias
  "Resolves alias given tag, attrs map, and content vector, returning the resolved Node."
  (fn [tag _ _] tag))

;; Implementation notes:
;; - HTML serialization is implemented as depth first search (DFS) traversal over Node.
;; - DFS is used to emit a series of Tokens (leaf nodes from the Node HTML tree).
;; - Tokens then write HTML fragments to a singular StringBuilder.
;; - DFS is implemented as reduction over Node.
;; - DFS is implemented using a stack of Iterators. (java.util.Deque<Iterator>)
;;   Note that head of stack is held by loop binding rather than the actual head of stack.
;; - Node children returns a value that is as flat as possible
;;   to minimized the depth of search (size of Deque).
;;   See the count varying node-children-n implementations
;; - Iterables returned by Node children should prefer implementations
;;   that are internally indexes to arrays.
;;   Iterators over Vectors are fast, Iterators over Seqs are not as fast.
;; - DFS emits a minimum number of Tokens. Therefore Node children emits
;;   OpeningTag and ClosingTag types as "fat" tokens capturing the bracket,
;;   tag name, and tag attributes data into one Token instance.
;; - Coalesce appends in large chunks showed 20% performance boost compared to
;;   to fragmented appends. Interleaving appends with branches and computation
;;   is detrimental. Therefore, the code is structured like a decision
;;   tree so the branches and computation happens early and the appends are
;;   executed together at the leaves.

(comment
  ;; Bad - Branching inbetween appends
  (do
    (if (is-a? data)
      (append sb "foo")
      (append sb "bar"))
    (if (is-b? data)
      (append sb "123")
      (append sb "456")))

  ;; Good
  (if (is-a? data)
    (if (is-b? data)
      (do (append sb "foo") (append sb "123"))
      (do (append sb "foo") (append sb "456")))
    (if (is-b? data)
      (do (append sb "bar") (append sb "123"))
      (do (append sb "bar") (append sb "456"))))

  ;; Bad - Computing inbetween appends
  (do
    (append sb "foo")
    (append sb (do-thing data))
    (append sb "bar"))

  ;; Good
  (let [thing (do-thing data)]
    (append sb "foo")
    (append sb thing)
    (append sb "bar"))
  )

;; Reduce / HTML

(def stack-max-depth 1024)

(defn reduce-node
  "Like reduce, but iterates over root Node in depth first search order.
  Leaf nodes are accumulated with the reduction function in order they are encountered."
  [rf init root]
  (let [stack     (java.util.ArrayDeque. 32)
        max-depth (int stack-max-depth)]
    (loop [cur (.iterator ^Iterable (vector root)) ret init]
      (if (reduced? ret)
        (.deref ^clojure.lang.IDeref ret)
        (if (some? cur)
          (if (.hasNext cur)
            (let [node (.next cur)]
              (if (branch? node)
                (do
                  (if (>= (.size stack) max-depth)
                    (throw (IllegalArgumentException. "Stack max depth exceeded.")))
                  (.addFirst stack cur)
                  (recur (.iterator (children node)) ret))
                (recur cur (rf ret node))))
            (recur (.pollFirst stack) ret))
          ret)))))

(defn append-fragment
  "Appends HTML token fragment to an appendable target.

  By default, appendable targets is set to types implementing java.lang.Appendable
  but can be made to target other types by altering the append-to var."
  [sb token]
  (fragment-append-to token sb))

(defn write-html
  "Writes HTML string to an appendable target.

  By default, appendable targets is set to types implementing java.lang.Appendable
  but can be made to target other types by altering the append-to var."
  [sb root]
  (reduce-node append-fragment sb root))

(defn html
  "Returns HTML string given a HTML Node tree."
  [root]
  (let [sb (StringBuilder. 16384)
        _  (write-html sb root)]
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
  (eduction (map fragment) (TokenSerializer. root)))

;; Append to

(defn append-to-appendable
  ([this] this)
  ([^Appendable this a]
   (doto this
     (.append ^String a)))
  ([^Appendable this a b]
   (doto this
     (.append ^String a)
     (.append ^String b)))
  ([^Appendable this a b c]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)))
  ([^Appendable this a b c d]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)))
  ([^Appendable this a b c d e]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)))
  ([^Appendable this a b c d e f]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)
     (.append ^String f)))
  ([^Appendable this a b c d e f g]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)
     (.append ^String f)
     (.append ^String g)))
  ([^Appendable this a b c d e f g h]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)
     (.append ^String f)
     (.append ^String g)
     (.append ^String h)))
  ([^Appendable this a b c d e f g h i]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)
     (.append ^String f)
     (.append ^String g)
     (.append ^String h)
     (.append ^String i))))

(defn append-to-string-builder
  ([this] this)
  ([^StringBuilder this a]
   (doto this
     (.append ^String a)))
  ([^StringBuilder this a b]
   (doto this
     (.append ^String a)
     (.append ^String b)))
  ([^StringBuilder this a b c]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)))
  ([^StringBuilder this a b c d]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)))
  ([^StringBuilder this a b c d e]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)))
  ([^StringBuilder this a b c d e f]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)
     (.append ^String f)))
  ([^StringBuilder this a b c d e f g]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)
     (.append ^String f)
     (.append ^String g)))
  ([^StringBuilder this a b c d e f g h]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)
     (.append ^String f)
     (.append ^String g)
     (.append ^String h)))
  ([^StringBuilder this a b c d e f g h i]
   (doto this
     (.append ^String a)
     (.append ^String b)
     (.append ^String c)
     (.append ^String d)
     (.append ^String e)
     (.append ^String f)
     (.append ^String g)
     (.append ^String h)
     (.append ^String i))))

(def append-to
  append-to-appendable)

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

(def escape-attribute-value-fragment
  escape-attribute-value)

(extend-protocol AttributeValue
  Boolean
  (attribute-fragment-append-to [this sb attr-name]
    (when this
      (append-to sb " " attr-name))
    sb)
  Object
  (attribute-fragment-append-to [this sb attr-name]
    (when-some [val-frag (attribute-value-fragment this)]
      (append-to sb " " attr-name "=\"" val-frag "\""))
    sb)
  nil
  (attribute-fragment-append-to [_ sb _] sb))

(defn join-attribute-value-fragment-kv
  [^StringBuilder sb k v]
  (when-some [v-frag (attribute-value-fragment v)]
    (let [k-frag (escape-attribute-value-fragment (name k))]
      (if (pos? (.length sb))
        (append-to-string-builder sb " " k-frag ": " v-frag ";")
        (append-to-string-builder sb k-frag ": " v-frag ";"))))
  sb)

(extend-protocol AttributeValueFragment
  clojure.lang.Keyword
  (attribute-value-fragment [this]
    (if-let [ns (namespace this)]
      (let [ns-frag   (escape-attribute-value-fragment ns)
            name-frag (escape-attribute-value-fragment (.getName this))
            sb        (doto (StringBuilder.)
                        (append-to-string-builder ns-frag "/" name-frag))]
        (.toString sb))
      (escape-attribute-value-fragment (.getName this))))
  java.util.Collection
  (attribute-value-fragment [this]
    (let [sb (StringBuilder.)
          xf (comp (keep attribute-value-fragment)
                   (interpose " "))
          _  (transduce xf append-to-string-builder sb this)]
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
  (attribute-value-fragment [this] (if this "" nil))
  nil
  (attribute-value-fragment [this] nil))

;; Opening Tag

(defn attribute-key?
  [k]
  (or (and (instance? clojure.lang.Named k)
           (not (namespace k)))
      (string? k)))

(defn append-attribute-fragment-kv-except-id-class
  [sb k v]
  (when (and (not (or (nil? v)
                      (identical? k :id)
                      (identical? k :class)))
             (attribute-key? k))
    (attribute-fragment-append-to v sb (name k)))
  sb)

(defn append-attribute-fragment-kv-except-id
  [sb k v]
  (when (and (not (or (nil? v)
                      (identical? k :id)))
             (attribute-key? k))
    (attribute-fragment-append-to v sb (name k)))
  sb)

(defn append-attribute-fragment-kv-except-class
  [sb k v]
  (when (and (not (or (nil? v)
                      (identical? k :class)))
             (attribute-key? k))
    (attribute-fragment-append-to v sb (name k)))
  sb)

(defn append-attribute-fragment-kv
  [sb k v]
  (when (and (some? v)
             (attribute-key? k))
    (attribute-fragment-append-to v sb (name k)))
  sb)

(defn append-opening-tag-with-id-class-attrs-id-class
  [sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 2)
        (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag " " attr-class-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag " " attr-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (append-to sb ">")))
      (if (== (.size attrs) 2)
        (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
          (append-to sb ">"))))))

(defn append-opening-tag-with-id-class-attrs-id
  [sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (if (== (.size attrs) 1)
      (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\">")
      (do
        (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (append-to sb ">")))))

(defn append-opening-tag-with-id-class-attrs-class
  [sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 1)
        (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag " " attr-class-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag " " attr-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-class sb attrs)
          (append-to sb ">")))
      (if (== (.size attrs) 1)
        (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv sb attrs)
          (append-to sb ">"))))))

(defn append-opening-tag-with-id-class-attrs
  [sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (if (zero? (.size attrs))
      (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\">")
      (do
        (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\"")
        (reduce-kv append-attribute-fragment-kv sb attrs)
        (append-to sb ">")))))

(defn append-opening-tag-with-id-class
  [sb tag-name head-id head-class]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" head-class-frag "\">")))

(defn append-opening-tag-with-id-attrs-id-class
  [sb tag-name head-id ^java.util.Map attrs]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 2)
        (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" attr-class-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" attr-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (append-to sb ">")))
      (if (== (.size attrs) 2)
        (append-to sb "<" tag-name " id=\"" head-id-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" head-id-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
          (append-to sb ">"))))))

(defn append-opening-tag-with-id-attrs-id
  [sb tag-name head-id ^java.util.Map attrs]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (if (== (.size attrs) 1)
      (append-to sb "<" tag-name " id=\"" head-id-frag "\">")
      (do
        (append-to sb "<" tag-name " id=\"" head-id-frag "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (append-to sb ">")))))

(defn append-opening-tag-with-id-attrs-class
  [sb tag-name head-id ^java.util.Map attrs]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 1)
        (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" attr-class-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" head-id-frag "\" class=\"" attr-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-class sb attrs)
          (append-to sb ">")))
      (if (== (.size attrs) 1)
        (append-to sb "<" tag-name " id=\"" head-id-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" head-id-frag "\"")
          (reduce-kv append-attribute-fragment-kv sb attrs)
          (append-to sb ">"))))))

(defn append-opening-tag-with-id-attrs
  [sb tag-name head-id ^java.util.Map attrs]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (if (zero? (.size attrs))
      (append-to sb "<" tag-name " id=\"" head-id-frag "\">")
      (do
        (append-to sb "<" tag-name " id=\"" head-id-frag "\"")
        (reduce-kv append-attribute-fragment-kv sb attrs)
        (append-to sb ">")))))

(defn append-opening-tag-with-id
  [sb tag-name head-id]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (append-to sb "<" tag-name " id=\"" head-id-frag "\">")))

(defn append-opening-tag-with-class-attrs-id-class
  [sb tag-name head-class ^java.util.Map attrs]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
      (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
        (if (== (.size attrs) 2)
          (append-to sb "<" tag-name " id=\"" attr-id-frag "\" class=\"" head-class-frag " " attr-class-frag "\">")
          (do
            (append-to sb "<" tag-name " id=\"" attr-id-frag "\" class=\"" head-class-frag " " attr-class-frag "\"")
            (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
            (append-to sb ">")))
        (if (== (.size attrs) 2)
          (append-to sb "<" tag-name " id=\"" attr-id-frag "\" class=\"" head-class-frag "\">")
          (do
            (append-to sb "<" tag-name " id=\"" attr-id-frag "\" class=\"" head-class-frag "\"")
            (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
            (append-to sb ">"))))
      (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
        (if (== (.size attrs) 2)
          (append-to sb "<" tag-name " class=\"" head-class-frag " " attr-class-frag "\">")
          (do
            (append-to sb "<" tag-name " class=\"" head-class-frag " " attr-class-frag "\"")
            (reduce-kv append-attribute-fragment-kv-except-class sb attrs)
            (append-to sb ">")))
        (if (== (.size attrs) 2)
          (append-to sb "<" tag-name " class=\"" head-class-frag "\">")
          (do
            (append-to sb "<" tag-name " class=\"" head-class-frag "\"")
            (reduce-kv append-attribute-fragment-kv sb attrs)
            (append-to sb ">")))))))

(defn append-opening-tag-with-class-attrs-id
  [sb tag-name head-class ^java.util.Map attrs]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
      (if (== (.size attrs) 1)
        (append-to sb "<" tag-name " id=\"" attr-id-frag "\" class=\"" head-class-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" attr-id-frag "\" class=\"" head-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
          (append-to sb ">")))
      (if (== (.size attrs) 1)
        (append-to sb "<" tag-name " class=\"" head-class-frag "\">")
        (do
          (append-to sb "<" tag-name " class=\"" head-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv sb attrs)
          (append-to sb ">"))))))

(defn append-opening-tag-with-class-attrs-class
  [sb tag-name head-class ^java.util.Map attrs]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
      (if (== (.size attrs) 1)
        (append-to sb "<" tag-name " class=\"" head-class-frag " " attr-class-frag "\">")
        (do
          (append-to sb "<" tag-name " class=\"" head-class-frag " " attr-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-class sb attrs)
          (append-to sb ">")))
      (if (== (.size attrs) 1)
        (append-to sb "<" tag-name " class=\"" head-class-frag "\">")
        (do
          (append-to sb "<" tag-name " class=\"" head-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv sb attrs)
          (append-to sb ">"))))))

(defn append-opening-tag-with-class-attrs
  [sb tag-name head-class ^java.util.Map attrs]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (if (zero? (.size attrs))
      (append-to sb "<" tag-name " class=\"" head-class-frag "\">")
      (do
        (append-to sb "<" tag-name " class=\"" head-class-frag "\"")
        (reduce-kv append-attribute-fragment-kv sb attrs)
        (append-to sb ">")))))

(defn append-opening-tag-with-class
  [sb tag-name head-class]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (append-to sb "<" tag-name " class=\"" head-class-frag "\">")))

(defn append-opening-tag-with-attrs-id-class
  [sb tag-name ^java.util.Map attrs]
  (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
      (if (== (.size attrs) 2)
        (append-to sb "<" tag-name " id=\"" attr-id-frag "\" class=\"" attr-class-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" attr-id-frag "\" class=\"" attr-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (append-to sb ">")))
      (if (== (.size attrs) 2)
        (append-to sb "<" tag-name " id=\"" attr-id-frag "\">")
        (do
          (append-to sb "<" tag-name " id=\"" attr-id-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
          (append-to sb ">"))))
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
      (if (== (.size attrs) 2)
        (append-to sb "<" tag-name " class=\"" attr-class-frag "\">")
        (do
          (append-to sb "<" tag-name " class=\"" attr-class-frag "\"")
          (reduce-kv append-attribute-fragment-kv-except-class sb attrs)
          (append-to sb ">")))
      (if (== (.size attrs) 2)
        (append-to sb "<" tag-name ">")
        (do
          (append-to sb "<" tag-name)
          (reduce-kv append-attribute-fragment-kv sb attrs)
          (append-to sb ">"))))))

(defn append-opening-tag-with-attrs-id
  [sb tag-name ^java.util.Map attrs]
  (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
    (if (== (.size attrs) 1)
      (append-to sb "<" tag-name " id=\"" attr-id-frag "\">")
      (do
        (append-to sb "<" tag-name " id=\"" attr-id-frag "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (append-to sb ">")))
    (if (== (.size attrs) 1)
      (append-to sb "<" tag-name ">")
      (do
        (append-to sb "<" tag-name)
        (reduce-kv append-attribute-fragment-kv sb attrs)
        (append-to sb ">")))))

(defn append-opening-tag-with-attrs-class
  [sb tag-name ^java.util.Map attrs]
  (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
    (if (== (.size attrs) 1)
      (append-to sb "<" tag-name " class=\"" attr-class-frag "\">")
      (do
        (append-to sb "<" tag-name " class=\"" attr-class-frag "\"")
        (reduce-kv append-attribute-fragment-kv-except-class sb attrs)
        (append-to sb ">")))
    (if (== (.size attrs) 1)
      (append-to sb "<" tag-name ">")
      (do
        (append-to sb "<" tag-name)
        (reduce-kv append-attribute-fragment-kv sb attrs)
        (append-to sb ">")))))

(defn append-opening-tag-with-attrs
  [sb tag-name ^java.util.Map attrs]
  (if (zero? (.size attrs))
    (append-to sb "<" tag-name ">")
    (do
      (append-to sb "<" tag-name)
      (reduce-kv append-attribute-fragment-kv sb attrs)
      (append-to sb ">"))))

(defn append-opening-tag
  [sb tag-name]
  (append-to sb "<" tag-name ">"))

(deftype OpeningTag [^clojure.lang.Keyword tag head-id head-class ^java.util.Map attrs]
  Token
  (fragment-append-to [this sb]
    (let [tag-name (.getName tag)]
      (if head-id
        (if head-class
          ;; +head-id, +head-class
          (if attrs
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-class-attrs-id-class sb tag-name head-id head-class attrs)
                (append-opening-tag-with-id-class-attrs-id sb tag-name head-id head-class attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-class-attrs-class sb tag-name head-id head-class attrs)
                (append-opening-tag-with-id-class-attrs sb tag-name head-id head-class attrs)))
            (append-opening-tag-with-id-class sb tag-name head-id head-class))
          ;; +head-id, -head-class
          (if attrs
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-attrs-id-class sb tag-name head-id attrs)
                (append-opening-tag-with-id-attrs-id sb tag-name head-id attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-attrs-class sb tag-name head-id attrs)
                (append-opening-tag-with-id-attrs sb tag-name head-id attrs)))
            (append-opening-tag-with-id sb tag-name head-id)))
        (if head-class
          ;; -head-id, +head-class
          (if attrs
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-class-attrs-id-class sb tag-name head-class attrs)
                (append-opening-tag-with-class-attrs-id sb tag-name head-class attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-class-attrs-class sb tag-name head-class attrs)
                (append-opening-tag-with-class-attrs sb tag-name head-class attrs)))
            (append-opening-tag-with-class sb tag-name head-class))
          ;; -head-id, -head-class
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
          _  (.fragment-append-to this sb)]
      (.toString sb)))
  Object
  (toString [this]
    (fragment this)))

(defn make-opening-tag
  ^OpeningTag
  [^clojure.lang.Keyword head attrs]
  (let [head-ns   (namespace head)
        head-name (.getName head)
        pound-idx (.indexOf head-name 35 #_(int \#))
        dot-idx   (.indexOf head-name 46 #_(int \.))]
    (if (pos? pound-idx)
      (if (pos? dot-idx)
        (if (< dot-idx pound-idx)
          (let [dot-idx-after (.indexOf head-name 46 #_(int \.) pound-idx)]
            (if (pos? dot-idx-after)
              ;; +head-id, +head-class-before, +head-class-after
              (let [tag           (->> (.substring head-name 0 dot-idx)
                                       (clojure.lang.Keyword/intern head-ns))
                    head-id       (.substring head-name (inc pound-idx) dot-idx-after)
                    head-class-sb (doto (StringBuilder. (.length head-name))
                                    ;; Append class before # without (via inc) dot prefix.
                                    (.append head-name (inc dot-idx) pound-idx)
                                    ;; Append class after # WITH (no inc) dot prefix.
                                    (.append head-name dot-idx-after (.length head-name)))
                    head-class    (-> (.toString head-class-sb)
                                      (.replace \. \space))]
                (OpeningTag. tag head-id head-class attrs))
              ;; +head-id, +head-class-before, -head-class-after
              (let [tag        (->> (.substring head-name 0 dot-idx)
                                    (clojure.lang.Keyword/intern head-ns))
                    head-id    (.substring head-name (inc pound-idx))
                    head-class (-> (.substring head-name (inc dot-idx) pound-idx)
                                   (.replace \. \space))]
                (OpeningTag. tag head-id head-class attrs))))
          ;; +head-id, -head-class-before, +head-class-after
          (let [tag        (->> (.substring head-name 0 pound-idx)
                                (clojure.lang.Keyword/intern head-ns))
                head-id    (.substring head-name (inc pound-idx) dot-idx)
                head-class (-> (.substring head-name (inc dot-idx))
                               (.replace \. \space))]
            (OpeningTag. tag head-id head-class attrs)))
        ;; +head-id, -head-class
        (let [tag     (->> (.substring head-name 0 pound-idx)
                           (clojure.lang.Keyword/intern head-ns))
              head-id (.substring head-name (inc pound-idx))]
          (OpeningTag. tag head-id nil attrs)))
      (if (pos? dot-idx)
        ;; -head-id, +head-class
        (let [tag        (->> (.substring head-name 0 dot-idx)
                              (clojure.lang.Keyword/intern head-ns))
              head-class (-> (.substring head-name (inc dot-idx))
                             (.replace \. \space))]
          (OpeningTag. tag nil head-class attrs))
        ;; -head-id, -head-class
        (OpeningTag. head nil nil attrs)))))

;; Closing tag

(deftype ClosingTag [^clojure.lang.Keyword tag]
  Token
  (fragment-append-to [this sb]
    (let [tag-name (.getName tag)]
      (append-to sb "</" tag-name ">")))
  (fragment [this]
    (let [sb (StringBuilder.)
          _  (.fragment-append-to this sb)]
      (.toString sb)))
  Object
  (toString [this]
    (fragment this)))

;; Raw string

(deftype RawString [value]
  AttributeValueFragment
  (attribute-value-fragment [this]
    (str value))
  Token
  (fragment-append-to [this sb]
    (append-to sb (str value)))
  (fragment [this]
    (str value))
  Object
  (toString [this]
    (fragment this)))

(defn raw-string
  [value]
  (RawString. value))

(def raw
  raw-string)

;; Token impl

(defn escape-text
  ^String
  [^String s]
  (.. s
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")))

(def escape-text-fragment
  escape-text)

(extend-protocol Token
  clojure.lang.Keyword
  (fragment-append-to [this sb]
    (if-let [ns (namespace this)]
      (let [ns-frag   (escape-text-fragment ns)
            name-frag (escape-text-fragment (.getName this))]
        (append-to sb ns-frag "/" name-frag))
      (let [name-frag (escape-text-fragment (.getName this))]
        (append-to sb name-frag))))
  (fragment [this]
    (escape-text-fragment this))
  java.util.UUID
  (fragment-append-to [this sb]
    ;; Not escaped. Should be safe.
    (append-to sb (.toString this)))
  (fragment [this]
    (.toString this))
  Number
  (fragment-append-to [this sb]
    ;; Not escaped. Should be safe.
    (append-to sb (.toString this)))
  (fragment [this]
    (.toString this))
  String
  (fragment-append-to [this sb]
    (append-to sb (escape-text-fragment this)))
  (fragment [this]
    (escape-text-fragment this))
  Object
  (fragment-append-to [this sb]
    (append-to sb (escape-text-fragment (.toString this))))
  (fragment [this]
    (escape-text-fragment (.toString this)))
  nil
  (fragment-append-to [_ sb] sb)
  (fragment [_] ""))

;; Element utils

(defn has-attrs?
  [^clojure.lang.IPersistentVector elem]
  (let [attrs (.nth elem 1 ::none)]
    (or (instance? java.util.Map attrs) (nil? attrs))))

(defn content-subvec*
  [v ^long start ^long end]
  (clojure.lang.APersistentVector$SubVector. {::content true} v start end))

(defn content-subvec
  ([^clojure.lang.IPersistentVector v ^long start]
   (content-subvec v start (.count v)))
  ([^clojure.lang.IPersistentVector v ^long start ^long end]
   (if (or (< end start) (< start 0) (> end (.count v)))
     (throw (IndexOutOfBoundsException.)))
   (if (== start end)
     (with-meta [] {::content true})
     (content-subvec* v start end))))

;; Alias element

(defn merge-alias-element-attrs
  [attrs head-id head-class]
  (if (some? head-id)
    (if (some? head-class)
      (if (contains? attrs :class)
        (-> (assoc attrs :id head-id)
            (assoc :class [head-class (get attrs :class)]))
        (-> (assoc attrs :id head-id)
            (assoc :class head-class)))
      (assoc attrs :id head-id))
    (if (some? head-class)
      (if (contains? attrs :class)
        (assoc attrs :class [head-class (get attrs :class)])
        (assoc attrs :class head-class))
      attrs)))

(defn resolve-alias-element-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head         (.nth elem 0)
        attrs        (.nth elem 1)
        opening      (make-opening-tag head attrs)
        tag          (.-tag opening)
        head-id      (.-head-id opening)
        head-class   (.-head-class opening)
        merged-attrs (if (or (map? attrs) (nil? attrs)) ; check if clj attrs map
                       (merge-alias-element-attrs attrs head-id head-class)
                       (-> (into {} attrs) ; else copy java map into clj map
                           (merge-alias-element-attrs head-id head-class)))
        elem-count   (.count elem)
        content      (if (> elem-count 2)
                       (content-subvec* elem 2 elem-count))]
    (resolve-alias tag merged-attrs content)))

(defn resolve-alias-element
  [^clojure.lang.IPersistentVector elem]
  (let [head       (.nth elem 0)
        opening    (make-opening-tag head nil)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)
        attrs      (if head-id
                     (if head-class
                       {:id head-id :class head-class}
                       {:id head-id})
                     (if head-class
                       {:class head-class}
                       nil))
        elem-count (.count elem)
        content    (if (> elem-count 1)
                     (content-subvec* elem 1 elem-count))]
    (resolve-alias tag attrs content)))

(defn alias-element-children
  [elem]
  ;; Note: alias elements adds an additional depth to the search stack.
  (if (has-attrs? elem)
    [(resolve-alias-element-attrs elem)]
    [(resolve-alias-element elem)]))

;; Normal element

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

(defn element-children-1
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    (if (void-tag? tag)
      [opening]
      [opening
       (ClosingTag. tag)])))

(defn element-children-2-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    (if (void-tag? tag)
      [opening]
      [opening
       (ClosingTag. tag)])))

(defn element-children-2
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
     (.nth elem 1)
     (ClosingTag. tag)]))

(defn element-children-3-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-3
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-4-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (ClosingTag. tag)]))

(defn element-children-4
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (ClosingTag. tag)]))

(defn element-children-5-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (ClosingTag. tag)]))

(defn element-children-5
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (ClosingTag. tag)]))

(defn element-children-6-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (ClosingTag. tag)]))

(defn element-children-6
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (ClosingTag. tag)]))

(defn element-children-7-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (ClosingTag. tag)]))

(defn element-children-7
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (ClosingTag. tag)]))

(defn element-children-8-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (ClosingTag. tag)]))

(defn element-children-8
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (ClosingTag. tag)]))

(defn element-children-9-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (.nth elem 8)
     (ClosingTag. tag)]))

(defn element-children-9
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
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
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
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
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
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
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        attrs   (.nth elem 1)
        opening (make-opening-tag head attrs)
        tag     (.-tag opening)]
    [opening
     ;; Note: adds an additional depth to the search stack.
     (content-subvec elem 2 (.count elem))
     (ClosingTag. tag)]))

(defn element-children-n
  [^clojure.lang.IPersistentVector elem]
  (let [head    (.nth elem 0)
        opening (make-opening-tag head nil)
        tag     (.-tag opening)]
    [opening
     ;; Note: adds an additional depth to the search stack.
     (content-subvec elem 1 (.count elem))
     (ClosingTag. tag)]))

(defn element-children
  [^clojure.lang.IPersistentVector elem]
  (if (has-attrs? elem)
    (case (.count elem)
      2  (element-children-2-attrs elem)
      3  (element-children-3-attrs elem)
      4  (element-children-4-attrs elem)
      5  (element-children-5-attrs elem)
      6  (element-children-6-attrs elem)
      7  (element-children-7-attrs elem)
      8  (element-children-8-attrs elem)
      9  (element-children-9-attrs elem)
      10 (element-children-10-attrs elem)
      (element-children-n-attrs elem))
    (case (.count elem)
      1  (element-children-1 elem)
      2  (element-children-2 elem)
      3  (element-children-3 elem)
      4  (element-children-4 elem)
      5  (element-children-5 elem)
      6  (element-children-6 elem)
      7  (element-children-7 elem)
      8  (element-children-8 elem)
      9  (element-children-9 elem)
      10 (element-children-10 elem)
      (element-children-n elem))))

;; Node impl

(defn element-vector?
  [^clojure.lang.IPersistentVector v]
  (let [head (.nth v 0 nil)]
    (and (keyword? head) (let [m (meta v)]
                           (or (nil? m) (not (get m ::content)))))))

(defn alias-element?
  [^clojure.lang.IPersistentVector elem]
  (let [head (.nth elem 0 nil)]
    (some? (namespace head))))

(extend-protocol Node
  clojure.lang.IPersistentVector
  (branch? [this] true)
  (children [this]
    (if (element-vector? this)
      (if (alias-element? this)
        (alias-element-children this)
        (element-children this))
      this))
  clojure.lang.ISeq
  (branch? [this] true)
  (children [this] this)
  clojure.lang.IDeref
  (branch? [this] true)
  (children [this]
    ;; Note: adds an additional depth to the search stack.
    [(.deref this)])
  clojure.lang.Fn
  (branch? [this] true)
  (children [this]
    ;; Note: adds an additional depth to the search stack.
    [(this)])
  Object
  (branch? [this] false)
  (children [_] nil)
  nil
  (branch? [this] false)
  (children [_] nil))

;; Raw consts

(def doctype-html5
  (raw "<!DOCTYPE html>"))
