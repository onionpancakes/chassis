(ns dev.onionpancakes.chassis.core)

(defprotocol AttributeValue
  (append-attribute-fragment-to [this sb attr-name] "Appends attribute key and value html fragment."))

(defprotocol AttributeValueFragment
  (^String attribute-value-fragment [this] "Returns attribute value fragment string or nil if none."))

(defprotocol Token
  (append-fragment-to [this sb] "Appends html fragment.")
  (^String fragment [this] "Returns HTML fragment."))

(defprotocol Node
  (^Boolean branch? [this] "Returns true if branch node.")
  (^Iterable children [this] "Returns children as Iterable."))

(defmulti resolve-alias
  "Resolves alias given tag, attrs map, and content vector,
  returning the resolved Node."
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

(defn reduce-tree
  "Like reduce, but iterates over a tree depth first search order.
  Leaf nodes are accumulated with the reduction function in order they are encountered."
  [branch? children rf init root]
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
                  (recur (.iterator ^Iterable (children node)) ret))
                (recur cur (rf ret node))))
            (recur (.pollFirst stack) ret))
          ret)))))

(defn reduce-node
  "Like reduce, but iterates over root Node in depth first search order.
  Leaf nodes are accumulated with the reduction function in order they are encountered."
  [rf init root]
  (reduce-tree branch? children rf init root))

(defn append-fragment
  "Appends HTML token fragment to an appendable target.

  By default, appendable targets is set to types implementing java.lang.Appendable
  but can be made to target other types by altering the append-to var."
  ([sb] sb)
  ([sb token]
   (append-fragment-to token sb)))

(defn write-html
  "Writes HTML string to an appendable target.

  By default, appendable targets is set to types implementing java.lang.Appendable
  but can be made to target other types by altering the append-to var."
  [sb root]
  (reduce-node append-fragment sb root))

(defn html
  "Returns HTML string given a HTML Node tree."
  {:tag String}
  [root]
  (let [sb (StringBuilder. 16384)
        _  (write-html sb root)]
    (.toString sb)))

;; Serializer

(deftype TreeSerializer [branch-fn children-fn root]
  clojure.lang.IReduceInit
  (reduce [_ rf init]
    (reduce-tree branch-fn children-fn rf init root))
  clojure.lang.Seqable
  (seq [this]
    (seq (vec this))))

(defn tree-serializer
  [branch-fn children-fn root]
  (TreeSerializer. branch-fn children-fn root))

(defn token-serializer
  "Returns a reducible and seqable emitting Tokens."
  [root]
  (tree-serializer branch? children root))

(defn html-serializer
  "Returns a reducible and seqable emitting HTML fragments."
  [root]
  (eduction (map fragment) (token-serializer root)))

;; Append to

(defn append-to-appendable
  "Batch append strings to Appendable."
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

(def append-to
  "Fascade to function used for fragment appends. By default, it is set to
  append-to-appendable. Alter this var to change the appendable target type."
  append-to-appendable)

;; Attributes impl

(defn escape-attribute-value*
  "Escapes an attribute value string. Escapes &, <, >, \", and '."
  {:tag String}
  [^String s]
  (.. s
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")
      (replace "\"" "&quot;")
      (replace "'" "&apos;")))

(defn escape-attribute-value
  "Escapes an attribute value string. Escapes &, <, >, \", and '."
  {:tag String}
  [^String s]
  (if (some? s)
    (escape-attribute-value* s)))

(def escape-attribute-value-fragment
  "Fascade to the function used for escaping attribute value fragments.
  By default, it is set to escape-attribute-value*. Alter this var to
  change the behavior of escaping attribute value fragments."
  escape-attribute-value*)

(defn attribute-key?
  {:tag Boolean}
  [k]
  (or (and (keyword? k) (not (namespace k)))
      (string? k)))

(extend-protocol AttributeValue
  Boolean
  (append-attribute-fragment-to [this sb attr-name]
    (when this
      (append-to sb " " attr-name))
    sb)
  Object
  (append-attribute-fragment-to [this sb attr-name]
    (when-some [val-frag (attribute-value-fragment this)]
      (append-to sb " " attr-name "=\"" val-frag "\""))
    sb)
  nil
  (append-attribute-fragment-to [_ sb _] sb))

(defn join-attribute-value-fragment-kv
  "Key-value reduction function for joining attribute style maps."
  [^StringBuilder sb k v]
  (when (attribute-key? k) ; Same rules as attribute keys.
    (when-some [v-frag (attribute-value-fragment v)]
      (let [k-frag (escape-attribute-value-fragment (name k))]
        (if (pos? (.length sb)) ; Note: if not empty, appends space as prefix!
          (doto sb
            (.append " ")
            (.append k-frag)
            (.append ": ")
            (.append v-frag)
            (.append ";"))
          (doto sb
            (.append k-frag)
            (.append ": ")
            (.append v-frag)
            (.append ";"))))))
  sb)

(extend-protocol AttributeValueFragment
  clojure.lang.Keyword
  (attribute-value-fragment [this]
    (if-let [ns (namespace this)]
      (let [ns-frag   (escape-attribute-value-fragment ns)
            name-frag (escape-attribute-value-fragment (.getName this))
            sb        (doto (StringBuilder.)
                        (.append ns-frag)
                        (.append "/")
                        (.append name-frag))]
        (.toString sb))
      (escape-attribute-value-fragment (.getName this))))
  clojure.lang.IDeref
  (attribute-value-fragment [this]
    (escape-attribute-value-fragment (.deref this)))
  clojure.lang.Fn
  (attribute-value-fragment [this]
    (escape-attribute-value-fragment (this)))
  java.util.Collection
  (attribute-value-fragment [this]
    (let [sb (StringBuilder.)
          xf (comp (keep attribute-value-fragment)
                   (interpose " "))
          rf (completing (memfn ^StringBuilder append s))
          _  (transduce xf rf sb this)]
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
  (attribute-value-fragment [this]
    ;; Without special Boolean processing, reasoning
    ;; with the default AttributeValue Object case.
    ;; The correct behavior is:
    ;;   [:div {:foo true}]  => <div foo=\"\"></div>
    ;;   [:div {:foo false}] => <div></div>
    ;; This suggests true has a fragment value of empty string,
    ;; and false has a fragment value of nil.
    (if this "" nil))
  nil
  (attribute-value-fragment [this] nil))

;; Opening Tag

(defn append-attribute-fragment-kv-except-id-class
  [sb k v]
  (when (and (not (or (nil? v)
                      (identical? k :id)
                      (identical? k :class)))
             (attribute-key? k))
    (append-attribute-fragment-to v sb (name k)))
  sb)

(defn append-attribute-fragment-kv-except-id
  [sb k v]
  (when (and (not (or (nil? v)
                      (identical? k :id)))
             (attribute-key? k))
    (append-attribute-fragment-to v sb (name k)))
  sb)

(defn append-attribute-fragment-kv-except-class
  [sb k v]
  (when (and (not (or (nil? v)
                      (identical? k :class)))
             (attribute-key? k))
    (append-attribute-fragment-to v sb (name k)))
  sb)

(defn append-attribute-fragment-kv
  [sb k v]
  (when (and (some? v)
             (attribute-key? k))
    (append-attribute-fragment-to v sb (name k)))
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

(deftype OpeningTag [metadata ^clojure.lang.Keyword tag head-id head-class ^java.util.Map attrs]
  Token
  (append-fragment-to [this sb]
    (let [tag-name (.getName tag)]
      (if (some? head-id)
        (if (some? head-class)
          ;; +head-id, +head-class
          (if (some? attrs)
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-class-attrs-id-class sb tag-name head-id head-class attrs)
                (append-opening-tag-with-id-class-attrs-id sb tag-name head-id head-class attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-class-attrs-class sb tag-name head-id head-class attrs)
                (append-opening-tag-with-id-class-attrs sb tag-name head-id head-class attrs)))
            (append-opening-tag-with-id-class sb tag-name head-id head-class))
          ;; +head-id, -head-class
          (if (some? attrs)
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-attrs-id-class sb tag-name head-id attrs)
                (append-opening-tag-with-id-attrs-id sb tag-name head-id attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-attrs-class sb tag-name head-id attrs)
                (append-opening-tag-with-id-attrs sb tag-name head-id attrs)))
            (append-opening-tag-with-id sb tag-name head-id)))
        (if (some? head-class)
          ;; -head-id, +head-class
          (if (some? attrs)
            (if (.containsKey attrs :id)
              (if (.containsKey attrs :class)
                (append-opening-tag-with-class-attrs-id-class sb tag-name head-class attrs)
                (append-opening-tag-with-class-attrs-id sb tag-name head-class attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-class-attrs-class sb tag-name head-class attrs)
                (append-opening-tag-with-class-attrs sb tag-name head-class attrs)))
            (append-opening-tag-with-class sb tag-name head-class))
          ;; -head-id, -head-class
          (if (some? attrs)
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
          _  (.append-fragment-to this sb)]
      (.toString sb)))
  clojure.lang.IMeta
  (meta [this] metadata)
  Object
  (toString [this]
    (fragment this)))

(defn make-opening-tag
  {:tag OpeningTag}
  [metadata ^clojure.lang.Keyword head attrs]
  (let [head-ns   (namespace head)
        head-name (.getName head)
        pound-idx (.indexOf head-name 35 #_(int \#))
        dot-idx   (.indexOf head-name 46 #_(int \.))]
    (if (>= pound-idx 0)
      (if (>= dot-idx 0)
        (if (< dot-idx pound-idx)
          (let [dot-idx-after (.indexOf head-name 46 #_(int \.) pound-idx)]
            (if (>= dot-idx-after 0)
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
                (OpeningTag. metadata tag head-id head-class attrs))
              ;; +head-id, +head-class-before, -head-class-after
              (let [tag        (->> (.substring head-name 0 dot-idx)
                                    (clojure.lang.Keyword/intern head-ns))
                    head-id    (.substring head-name (inc pound-idx))
                    head-class (-> (.substring head-name (inc dot-idx) pound-idx)
                                   (.replace \. \space))]
                (OpeningTag. metadata tag head-id head-class attrs))))
          ;; +head-id, -head-class-before, +head-class-after
          (let [tag        (->> (.substring head-name 0 pound-idx)
                                (clojure.lang.Keyword/intern head-ns))
                head-id    (.substring head-name (inc pound-idx) dot-idx)
                head-class (-> (.substring head-name (inc dot-idx))
                               (.replace \. \space))]
            (OpeningTag. metadata tag head-id head-class attrs)))
        ;; +head-id, -head-class
        (let [tag     (->> (.substring head-name 0 pound-idx)
                           (clojure.lang.Keyword/intern head-ns))
              head-id (.substring head-name (inc pound-idx))]
          (OpeningTag. metadata tag head-id nil attrs)))
      (if (>= dot-idx 0)
        ;; -head-id, +head-class
        (let [tag        (->> (.substring head-name 0 dot-idx)
                              (clojure.lang.Keyword/intern head-ns))
              head-class (-> (.substring head-name (inc dot-idx))
                             (.replace \. \space))]
          (OpeningTag. metadata tag nil head-class attrs))
        ;; -head-id, -head-class
        (OpeningTag. metadata head nil nil attrs)))))

;; Closing tag

(deftype ClosingTag [metadata ^clojure.lang.Keyword tag]
  Token
  (append-fragment-to [this sb]
    (let [tag-name (.getName tag)]
      (append-to sb "</" tag-name ">")))
  (fragment [this]
    (let [sb (StringBuilder.)
          _  (.append-fragment-to this sb)]
      (.toString sb)))
  clojure.lang.IMeta
  (meta [this] metadata)
  Object
  (toString [this]
    (fragment this)))

;; Raw string

(deftype RawString [value]
  AttributeValueFragment
  (attribute-value-fragment [this] value)
  Token
  (append-fragment-to [this sb]
    (append-to sb value))
  (fragment [this] value)
  Object
  (toString [this]
    (fragment this)))

(defn raw-string
  "Wraps value as an unescaped string."
  ([] (RawString. ""))
  ([value]
   (RawString. (str value)))
  ([value & more]
   (RawString. (apply str value more))))

(def raw
  "Alias for raw-string."
  raw-string)

;; Token impl

(defn escape-text*
  "Escapes a text string. Escapes &, <, and >."
  {:tag String}
  [^String s]
  (.. s
      (replace "&" "&amp;")
      (replace "<" "&lt;")
      (replace ">" "&gt;")))

(defn escape-text
  "Escapes a text string. Escapes &, <, and >."
  {:tag String}
  [^String s]
  (if (some? s)
    (escape-text* s)))

(def escape-text-fragment
  "Fascade to the function used for escaping text fragments.
  By default, it is set to escape-text*. Alter this var to
  change the behavior of escaping text fragments."
  escape-text*)

(extend-protocol Token
  clojure.lang.Keyword
  (append-fragment-to [this sb]
    (if-some [ns-str (namespace this)]
      (let [ns-frag   (escape-text-fragment ns-str)
            name-frag (escape-text-fragment (.getName this))]
        (append-to sb ns-frag "/" name-frag))
      (let [name-frag (escape-text-fragment (.getName this))]
        (append-to sb name-frag))))
  (fragment [this]
    (escape-text-fragment this))
  java.util.UUID
  (append-fragment-to [this sb]
    ;; Not escaped. Should be safe.
    (append-to sb (.toString this)))
  (fragment [this]
    (.toString this))
  Number
  (append-fragment-to [this sb]
    ;; Not escaped. Should be safe.
    (append-to sb (.toString this)))
  (fragment [this]
    (.toString this))
  String
  (append-fragment-to [this sb]
    (append-to sb (escape-text-fragment this)))
  (fragment [this]
    (escape-text-fragment this))
  Object
  (append-fragment-to [this sb]
    (append-to sb (escape-text-fragment (.toString this))))
  (fragment [this]
    (escape-text-fragment (.toString this)))
  nil
  (append-fragment-to [_ sb] sb)
  (fragment [_] ""))

;; Element utils

(defn attrs?
  [attrs]
  (or (instance? java.util.Map attrs) (nil? attrs)))

(defn has-attrs?
  {:tag Boolean}
  [^clojure.lang.IPersistentVector elem]
  (attrs? (.nth elem 1 ::none)))

(defn content-subvec*
  [v ^long start ^long end]
  (clojure.lang.APersistentVector$SubVector. {::content true} v start end))

(defn content-subvec
  "Creates a subvector with metadata key ::content set to true."
  ([^clojure.lang.IPersistentVector v ^long start]
   (content-subvec v start (.count v)))
  ([^clojure.lang.IPersistentVector v ^long start ^long end]
   (if (or (< end start) (< start 0) (> end (.count v)))
     (throw (IndexOutOfBoundsException.)))
   (if (== start end)
     (with-meta [] {::content true})
     (content-subvec* v start end))))

;; Alias element

(defn make-head-attrs
  ([head-id head-class]
   (if (some? head-id)
     (if (some? head-class)
       {:id head-id :class head-class}
       {:id head-id})
     (if (some? head-class)
       {:class head-class}
       nil)))
  ([head-id head-class attrs]
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
       attrs))))

(defn apply-normalized-with-meta-attrs*
  [f ^clojure.lang.IPersistentVector elem]
  (let [metadata     (meta elem)
        head         (.nth elem 0)
        attrs        (.nth elem 1)
        opening      (make-opening-tag metadata head attrs)
        tag          (.-tag opening)
        head-id      (.-head-id opening)
        head-class   (.-head-class opening)
        merged-attrs (if (or (map? attrs) (nil? attrs)) ; check if clj attrs map
                       (make-head-attrs head-id head-class attrs)
                       ;; Copy java map into clj map.
                       (make-head-attrs head-id head-class (into {} attrs)))
        content      (content-subvec elem 2)]
    (f metadata tag merged-attrs content)))

(defn apply-normalized-with-meta*
  [f ^clojure.lang.IPersistentVector elem]
  (let [metadata   (meta elem)
        head       (.nth elem 0)
        opening    (make-opening-tag metadata head nil)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)
        attrs      (make-head-attrs head-id head-class)
        content    (content-subvec elem 1)]
    (f metadata tag attrs content)))

(defn apply-normalized-with-meta
  [f elem]
  (if (has-attrs? elem)
    (apply-normalized-with-meta-attrs* f elem)
    (apply-normalized-with-meta* f elem)))

(defn merge-meta
  [obj metadata]
  (if (instance? clojure.lang.IObj obj)
    (with-meta obj (merge (meta obj) metadata))
    obj))

(defn resolve-with-meta-fn
  [f]
  (fn [metadata tag attrs content]
    (-> (f tag attrs content)
        (merge-meta metadata))))

(defn apply-normalized
  [f elem]
  (apply-normalized-with-meta (resolve-with-meta-fn f) elem))

(defn resolve-alias-with-meta
  [metadata tag attrs content]
  (-> (resolve-alias tag attrs content)
      (merge-meta metadata)))

(defn resolve-alias-element
  [elem]
  (apply-normalized-with-meta resolve-alias-with-meta elem))

(defn alias-element-children
  [elem]
  [(resolve-alias-element elem)])

;; Normal element

;; https://developer.mozilla.org/en-US/docs/Glossary/Void_element

(defn void-tag?
  {:tag Boolean}
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
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    (if (void-tag? tag)
      [opening]
      [opening
       (ClosingTag. metadata tag)])))

(defn element-children-2-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    (if (void-tag? tag)
      [opening]
      [opening
       (ClosingTag. metadata tag)])))

(defn element-children-2
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     (.nth elem 1)
     (ClosingTag. metadata tag)]))

(defn element-children-3-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     (.nth elem 2)
     (ClosingTag. metadata tag)]))

(defn element-children-3
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (ClosingTag. metadata tag)]))

(defn element-children-4-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (ClosingTag. metadata tag)]))

(defn element-children-4
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (ClosingTag. metadata tag)]))

(defn element-children-5-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (ClosingTag. metadata tag)]))

(defn element-children-5
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (ClosingTag. metadata tag)]))

(defn element-children-6-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (ClosingTag. metadata tag)]))

(defn element-children-6
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (ClosingTag. metadata tag)]))

(defn element-children-7-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (ClosingTag. metadata tag)]))

(defn element-children-7
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (ClosingTag. metadata tag)]))

(defn element-children-8-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (ClosingTag. metadata tag)]))

(defn element-children-8
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (ClosingTag. metadata tag)]))

(defn element-children-9-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (.nth elem 8)
     (ClosingTag. metadata tag)]))

(defn element-children-9
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     (.nth elem 1)
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (.nth elem 8)
     (ClosingTag. metadata tag)]))

(defn element-children-10-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     (.nth elem 2)
     (.nth elem 3)
     (.nth elem 4)
     (.nth elem 5)
     (.nth elem 6)
     (.nth elem 7)
     (.nth elem 8)
     (.nth elem 9)
     (ClosingTag. metadata tag)]))

(defn element-children-10
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
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
     (ClosingTag. metadata tag)]))

(defn element-children-n-attrs
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        attrs    (.nth elem 1)
        opening  (make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    [opening
     ;; Note: adds an additional depth to the search stack.
     (content-subvec elem 2 (.count elem))
     (ClosingTag. metadata tag)]))

(defn element-children-n
  [^clojure.lang.IPersistentVector elem]
  (let [metadata (meta elem)
        head     (.nth elem 0)
        opening  (make-opening-tag metadata head nil)
        tag      (.-tag opening)]
    [opening
     ;; Note: adds an additional depth to the search stack.
     (content-subvec elem 1 (.count elem))
     (ClosingTag. metadata tag)]))

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
  {:tag Boolean}
  [^clojure.lang.IPersistentVector v]
  (let [head (.nth v 0 nil)]
    (and (keyword? head) (let [m (meta v)]
                           (or (nil? m) (not (get m ::content)))))))

(defn alias-element?
  {:tag Boolean}
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
  clojure.core.Eduction
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
  "RawString for <!DOCTYPE html>"
  (raw "<!DOCTYPE html>"))

(def nbsp
  "RawString for &nbsp;"
  (raw "&nbsp;"))
