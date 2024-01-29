(ns dev.onionpancakes.chassis.core)

(defprotocol Node
  (branch? [this])
  (children ^Iterable [this]))

(defprotocol Token
  (fragment [this]))

(extend-protocol Node
  clojure.lang.IPersistentVector
  (branch? [this] true)
  (children [[tag & content]]
    [::begin-open tag ::end-open content ::begin-close tag ::end-close]
    #_
    [\< tag \> content \< \/ tag \>])
  clojure.lang.ISeq
  (branch? [_] true)
  (children [this] this)
  clojure.lang.IPersistentMap
  (branch? [_] true)
  (children [this] [])
  Object
  (branch? [_] false)
  (children [_] nil)
  nil
  (branch? [_] false)
  (children [_] nil))

(extend-protocol Token
  clojure.lang.Keyword
  (fragment [this]
    #_
    (name this)
    (case this
      ::begin-open  "<"
      ::end-open    ">"
      ::begin-close "</"
      ::end-close   ">"
      (name this)))
  Character
  (fragment [this] (.toString this))
  String
  (fragment [this] this)
  Object
  (fragment [this] (.toString this))
  nil
  (fragment [_] ""))

;;

(deftype TokenSerializer [root]
  clojure.lang.IReduceInit
  (reduce [this rf init]
    (let [stack (java.util.ArrayDeque. 32)]
      (loop [cur (.iterator ^Iterable (list root)) ret init]
        (if cur
          (if (.hasNext cur)
            (let [node (.next cur)]
              (if (branch? node)
                (do
                  (.addFirst stack cur)
                  (recur (.iterator (children node)) ret))
                (recur cur (rf ret node))))
            (recur (.pollFirst stack) ret))
          ret))))
  clojure.lang.ISeq
  (seq [this]
    (->> (tree-seq branch? children root)
         (remove branch?))))

(defn token-serializer
  [root]
  (TokenSerializer. root))

(defn html-serializer
  [root]
  (eduction (map fragment)
            (token-serializer root)))
