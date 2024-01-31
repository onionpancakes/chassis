(ns dev.onionpancakes.chassis.core)

(defprotocol Token
  (fragment ^String [this])
  (append-fragment-to-string-builder [this sb]))

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
  [^StringBuilder sb k v]
  (.. sb
      (append " ")
      (append (name k))
      (append "=\"")
      (append (escape-attr-value v))
      (append "\"")))

(deftype OpeningTag [tag ^clojure.lang.IKVReduce attrs]
  Token
  (fragment [this]
    (let [sb (StringBuilder. 64)
          _  (.append-fragment-to-string-builder this sb)]
      (.toString sb)))
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb "<")
    (.append ^StringBuilder sb (name tag))
    (if attrs
      (.kvreduce attrs append-attr-kv sb))
    (.append ^StringBuilder sb ">")
    sb)
  Node
  (branch? [this] false)
  (children [this] [])
  Object
  (toString [this]
    (fragment this)))

(deftype ClosingTag [tag]
  Token
  (fragment [this]
    (let [sb (StringBuilder.)
          _  (.append-fragment-to-string-builder this sb)]
      (.toString sb)))
  (append-fragment-to-string-builder [this sb]
    (.. ^StringBuilder sb
        (append "</")
        (append (name tag))
        (append ">")))
  Node
  (branch? [this] false)
  (children [this] [])
  Object
  (toString [this]
    (fragment this)))

(deftype Raw [token]
  Token
  (fragment [this]
    (str token))
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (str token)))
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
  (fragment [this]
    (escape-text this))
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (escape-text this)))
  Object
  (fragment [this]
    (escape-text (.toString this)))
  (append-fragment-to-string-builder [this sb]
    (.append ^StringBuilder sb (escape-text (.toString this))))
  nil
  (fragment [_] "")
  (append-fragment-to-string-builder [this sb] sb))

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
  [tag])

(defn tag-classes
  [tag])

(defn element-children-0
  [elem]
  [])

(defn element-children-1
  [^clojure.lang.Indexed elem]
  (let [tag (base-tag (.nth elem 0))]
    (if (void-tag? tag)
      [(OpeningTag. tag nil)]
      [(OpeningTag. tag nil)
       (ClosingTag. tag)])))

(defn element-children-2-attrs
  [^clojure.lang.Indexed elem]
  (let [tag   (base-tag (.nth elem 0))
        attrs (.nth elem 1)]
    (if (void-tag? tag)
      [(OpeningTag. tag attrs)]
      [(OpeningTag. tag attrs)
       (ClosingTag. tag)])))

(defn element-children-2
  [^clojure.lang.Indexed elem]
  (let [tag (base-tag (.nth elem 0))]
   (if (and (void-tag? tag)
            (nil? (.nth elem 1)))
     [(OpeningTag. tag nil)]
     [(OpeningTag. tag nil)
      (.nth elem 1)
      (ClosingTag. tag)])))

(defn element-children-3-attrs
  [^clojure.lang.Indexed elem]
  (let [tag   (base-tag (.nth elem 0))
        attrs (.nth elem 1)]
    [(OpeningTag. tag attrs)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-3
  [^clojure.lang.Indexed elem]
  (let [tag (base-tag (.nth elem 0))]
    [(OpeningTag. tag nil)
     (.nth elem 1)
     (.nth elem 2)
     (ClosingTag. tag)]))

(defn element-children-n-attrs
  [^clojure.lang.Indexed elem]
  (let [tag   (base-tag (.nth elem 0))
        attrs (.nth elem 1)]
    [(OpeningTag. tag attrs)
     (next (next elem))
     (ClosingTag. tag)]))

(defn element-children-n
  [^clojure.lang.Indexed elem]
  (let [tag (base-tag (.nth elem 0))]
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
