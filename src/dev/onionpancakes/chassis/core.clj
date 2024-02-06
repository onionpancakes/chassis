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

(defmulti resolve-alias
  (fn [tag _ _] tag))

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

(def stack-max-depth 1024)

(defn reduce-node
  [rf init root]
  (let [stack (java.util.ArrayDeque. 32)]
    (loop [cur (.iterator ^Iterable (vector root)) ret init]
      (if (reduced? ret)
        (.deref ^clojure.lang.IDeref ret)
        (if (some? cur)
          (if (.hasNext cur)
            (let [node (.next cur)]
              (if-some [ch (children node)]
                (do
                  (if (>= (.size stack) stack-max-depth)
                    (throw (IllegalArgumentException. "Stack max depth exceeded.")))
                  (.addFirst stack cur)
                  (recur (.iterator ch) ret))
                (recur cur (rf ret node))))
            (recur (.pollFirst stack) ret))
          ret)))))

;; vector check
(defn reduce-node2
  [rf init root]
  (let [stack (java.util.ArrayDeque. 32)]
    (loop [cur (.iterator ^Iterable (vector root)) ret init]
      (if (reduced? ret)
        (.deref ^clojure.lang.IDeref ret)
        (if (some? cur)
          (if (.hasNext cur)
            (let [node (.next cur)]
              (if-some [ch (children node)]
                (if (vector? ch)
                  (if (== (.count ^clojure.lang.IPersistentVector ch) 1)
                    (let [node2 (.nth ^clojure.lang.IPersistentVector ch 0)]
                      (if-some [ch2 (children node2)]
                        (recur (.iterator ch2) ret)
                        (recur cur (rf ret node2))))
                    (do
                      (if (>= (.size stack) stack-max-depth)
                        (throw (IllegalArgumentException. "Stack max depth exceeded.")))
                      (.addFirst stack cur)
                      (recur (.iterator ch) ret)))
                  (do
                    (if (>= (.size stack) stack-max-depth)
                      (throw (IllegalArgumentException. "Stack max depth exceeded.")))
                    (.addFirst stack cur)
                    (recur (.iterator ch) ret)))
                (recur cur (rf ret node))))
            (recur (.pollFirst stack) ret))
          ret)))))

;; hasNext after next
(defn reduce-node3
  [rf init root]
  (let [stack (java.util.ArrayDeque. 32)]
    (loop [cur (.iterator ^Iterable (vector root)) ret init]
      (if (reduced? ret)
        (.deref ^clojure.lang.IDeref ret)
        (if (some? cur)
          (let [node (.next cur)]
            (if-some [ch (children node)]
              (let [iter-next (.iterator ch)]
                (if (.hasNext iter-next)
                  (if (.hasNext cur)
                    (do
                      (if (< (.size stack) stack-max-depth)
                        (.addFirst stack cur)
                        (throw (IllegalArgumentException. "Stack max depth exceeded.")))
                      (recur iter-next ret))
                    (recur iter-next ret))
                  (if (.hasNext cur)
                    (recur cur ret)
                    (recur (.pollFirst stack) ret))))
              (if (.hasNext cur)
                (recur cur (rf ret node))
                (recur (.pollFirst stack) (rf ret node)))))
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

;; Opening Tag

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
  [^StringBuilder sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
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
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-id-class-attrs-id
  [^StringBuilder sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (if (== (.size attrs) 1)
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb head-id-frag)
        (.append sb "\" class=\"")
        (.append sb head-class-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb head-id-frag)
        (.append sb "\" class=\"")
        (.append sb head-class-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-id-class-attrs-class
  [^StringBuilder sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
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
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-id-class-attrs
  [^StringBuilder sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (if (zero? (.size attrs))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb head-id-frag)
        (.append sb "\" class=\"")
        (.append sb head-class-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb head-id-frag)
        (.append sb "\" class=\"")
        (.append sb head-class-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-id-class
  [^StringBuilder sb tag-name head-id head-class]
  (let [head-id-frag    (escape-attribute-value-fragment head-id)
        head-class-frag (escape-attribute-value-fragment head-class)]
    (.append sb "<")
    (.append sb tag-name)
    (.append sb " id=\"")
    (.append sb head-id-frag)
    (.append sb "\" class=\"")
    (.append sb head-class-frag)
    (.append sb "\">")))

(defn append-opening-tag-with-id-attrs-id-class
  [^StringBuilder sb tag-name head-id ^java.util.Map attrs]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 2)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
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
          (.append sb head-id-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-id-attrs-id
  [^StringBuilder sb tag-name head-id ^java.util.Map attrs]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (if (== (.size attrs) 1)
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb head-id-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb head-id-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-id-attrs-class
  [^StringBuilder sb tag-name head-id ^java.util.Map attrs]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))] 
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\" class=\"")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
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
          (.append sb head-id-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb head-id-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-id-attrs
  [^StringBuilder sb tag-name head-id head-class ^java.util.Map attrs]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (if (zero? (.size attrs))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb head-id-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " id=\"")
        (.append sb head-id-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-id
  [^StringBuilder sb tag-name head-id]
  (let [head-id-frag (escape-attribute-value-fragment head-id)]
    (.append sb "<")
    (.append sb tag-name)
    (.append sb " id=\"")
    (.append sb head-id-frag)
    (.append sb "\">")))

(defn append-opening-tag-with-class-attrs-id-class
  [^StringBuilder sb tag-name head-class ^java.util.Map attrs]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
      (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
        (if (== (.size attrs) 2)
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " id=\"")
            (.append sb attr-id-frag)
            (.append sb "\" class=\"")
            (.append sb head-class-frag)
            (.append sb " ")
            (.append sb attr-class-frag)
            (.append sb "\">"))
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " id=\"")
            (.append sb attr-id-frag)
            (.append sb "\" class=\"")
            (.append sb head-class-frag)
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
            (.append sb head-class-frag)
            (.append sb "\">"))
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " id=\"")
            (.append sb attr-id-frag)
            (.append sb "\" class=\"")
            (.append sb head-class-frag)
            (.append sb "\"")
            (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
            (.append sb ">"))))
      (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
        (if (== (.size attrs) 2)
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " class=\"")
            (.append sb head-class-frag)
            (.append sb " ")
            (.append sb attr-class-frag)
            (.append sb "\">"))
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " class=\"")
            (.append sb head-class-frag)
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
            (.append sb head-class-frag)
            (.append sb "\">"))
          (do
            (.append sb "<")
            (.append sb tag-name)
            (.append sb " class=\"")
            (.append sb head-class-frag)
            (.append sb "\"")
            (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
            (.append sb ">")))))))

(defn append-opening-tag-with-class-attrs-id
  [^StringBuilder sb tag-name head-class ^java.util.Map attrs]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-id-frag (attribute-value-fragment (.get attrs :id))]
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb attr-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " id=\"")
          (.append sb attr-id-frag)
          (.append sb "\" class=\"")
          (.append sb head-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">")))
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb head-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb head-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-class-attrs-class
  [^StringBuilder sb tag-name head-class ^java.util.Map attrs]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (if-some [attr-class-frag (attribute-value-fragment (.get attrs :class))]
      (if (== (.size attrs) 1)
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb head-class-frag)
          (.append sb " ")
          (.append sb attr-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb head-class-frag)
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
          (.append sb head-class-frag)
          (.append sb "\">"))
        (do
          (.append sb "<")
          (.append sb tag-name)
          (.append sb " class=\"")
          (.append sb head-class-frag)
          (.append sb "\"")
          (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
          (.append sb ">"))))))

(defn append-opening-tag-with-class-attrs
  [^StringBuilder sb tag-name head-class ^java.util.Map attrs]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (if (zero? (.size attrs))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " class=\"")
        (.append sb head-class-frag)
        (.append sb "\">"))
      (do
        (.append sb "<")
        (.append sb tag-name)
        (.append sb " class=\"")
        (.append sb head-class-frag)
        (.append sb "\"")
        (reduce-kv append-attribute-fragment-kv-except-id-class sb attrs)
        (.append sb ">")))))

(defn append-opening-tag-with-class
  [^StringBuilder sb tag-name head-class]
  (let [head-class-frag (escape-attribute-value-fragment head-class)]
    (.append sb "<")
    (.append sb tag-name)
    (.append sb " class=\"")
    (.append sb head-class-frag)
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

(deftype OpeningTag [^clojure.lang.Keyword tag head-id head-class ^java.util.Map attrs]
  Token
  (append-fragment-to-string-builder [this sb]
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
                (append-opening-tag-with-id-attrs-id-class sb tag-name head-class attrs)
                (append-opening-tag-with-id-attrs-id sb tag-name head-class attrs))
              (if (.containsKey attrs :class)
                (append-opening-tag-with-id-attrs-class sb tag-name head-class attrs)
                (append-opening-tag-with-id-attrs sb tag-name head-class attrs)))
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
          _  (.append-fragment-to-string-builder this sb)]
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

;; Raw string

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

(extend-protocol Token
  clojure.lang.Keyword
  (append-fragment-to-string-builder [this sb]
    (if-let [ns (namespace this)]
      (let [ns-frag   (escape-text-fragment ns)
            name-frag (escape-text-fragment (.getName this))]
        (.append ^StringBuilder sb ns-frag)
        (.append ^StringBuilder sb "/")
        (.append ^StringBuilder sb name-frag))
      (let [name-frag (escape-text-fragment (.getName this))]
        (.append ^StringBuilder sb name-frag))))
  (fragment [this]
    (escape-text-fragment this))
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

;; Element utils

(defn has-attrs?
  [^clojure.lang.IPersistentVector elem]
  (let [attrs (.nth elem 1 ::none)]
    (or (instance? java.util.Map attrs) (nil? attrs))))

(defn content-subvec*
  [v start end]
  (clojure.lang.APersistentVector$SubVector. {::content true} v start end))

(defn content-subvec
  ([^clojure.lang.IPersistentVector v start]
   (content-subvec v start (.count v)))
  ([^clojure.lang.IPersistentVector v start end]
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
  (children [this]
    (if (element-vector? this)
      (if (alias-element? this)
        (alias-element-children this)
        (element-children this))
      this))
  clojure.lang.ISeq
  (children [this] this)
  clojure.lang.IDeref
  (children [this]
    ;; Note: adds an additional depth to the search stack.
    (vector (.deref this)))
  clojure.lang.Fn
  (children [this]
    ;; Note: adds an additional depth to the search stack.
    (vector (this)))
  Object
  (children [_] nil)
  nil
  (children [_] nil))

;; Raw consts

(def doctype-html5
  (raw "<!DOCTYPE html>"))
