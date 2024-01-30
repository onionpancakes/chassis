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
        (if (reduced? ret)
          (deref ret)
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
  clojure.lang.ISeq
  (seq [this]
    (->> (tree-seq branch? children root)
         (remove branch?))))

(defn append-string
  [^StringBuilder sb ^String s]
  (.append sb s))

(deftype HtmlSerializer [root]
  clojure.lang.IReduceInit
  (reduce [this rf init]
    (let [stack (java.util.ArrayDeque. 32)]
      (loop [cur (.iterator ^Iterable (list root)) ret init]
        (if (reduced? ret)
          (deref ret)
          (if cur
            (if (.hasNext cur)
              (let [node (.next cur)]
                (if (branch? node)
                  (do
                    (.addFirst stack cur)
                    (recur (.iterator (children node)) ret))
                  (recur cur (rf ret (fragment node)))))
              (recur (.pollFirst stack) ret))
            ret)))))
  clojure.lang.ISeq
  (seq [this]
    (->> (tree-seq branch? children root)
         (remove branch?)
         (map fragment)))
  Object
  (toString [this]
    (let [sb (StringBuilder. 16384)
          _  (.reduce this append-string sb)]
      (.toString sb))))

(defn token-serializer
  ^TokenSerializer
  [root]
  (TokenSerializer. root))

(defn html-serializer
  ^HtmlSerializer
  [root]
  (HtmlSerializer. root))

(defn html
  ^String
  [root]
  (.toString (html-serializer root)))

;; Token impl

(defn append-attr-kv
  [^StringBuilder sb k v]
  (.. sb
      (append " ")
      (append (name k))
      (append "=\"")
      (append v)
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
  Node
  (branch? [this] false)
  (children [this] [])
  Object
  (toString [this]
    (fragment this)))

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
    (if (map? (.nth this 1 nil))
      (case (.count this)
        0 (element-children-0 this)
        1 (element-children-1 this)
        2 (element-children-2-attrs this)
        3 (element-children-3-attrs this)
        (element-children-n-attrs this))
      (case (.count this)
        0 (element-children-0 this)
        1 (element-children-1 this)
        2 (element-children-2 this)
        3 (element-children-3 this)
        (element-children-n this))))
  clojure.lang.ISeq
  (branch? [_] true)
  (children [this] this)
  Object
  (branch? [_] false)
  (children [_] [])
  nil
  (branch? [_] false)
  (children [_] []))
