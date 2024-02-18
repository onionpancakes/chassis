(ns dev.onionpancakes.chassis.compiler
  (:refer-clojure :exclude [compile])
  (:require [dev.onionpancakes.chassis.core :as c]))

(defprotocol CompilableForm
  (constant? [this] "Returns true if form is constant, safe to make fragments with at compile time.")
  (evaluated? [this] "Returns true if form is evaluated, safe to make tokens with at compile time.")
  (resolved [this] "Returns the form in which symbols and coll of symbols are resolved."))

(defprotocol CompilableNode
  (compilable-node [this] "Returns Node when traversed via reduce-node emits compiled token forms."))

;; Binding of macro &env, for resolving symbols.
(def ^:dynamic *env*)

;; Compile

(defn compacted-form
  [forms]
  (let [sb (StringBuilder.)
        tf (fn [_ form]
             (throw (IllegalArgumentException. (str "Not constant form: " form))))
        xf (halt-when (complement constant?) tf)
        _  (transduce xf c/append-fragment sb forms)]
    (c/raw (.toString sb))))

(defn compact
  [tokens]
  (eduction (partition-by constant?)
            (mapcat (fn [forms]
                      (if (constant? (first forms))
                        [(compacted-form forms)]
                        forms)))
            tokens))

(defmacro compile*
  [node]
  (binding [*env* &env]
    (-> (compilable-node node)
        (c/token-serializer)
        (compact)
        (vec)
        (vary-meta assoc ::c/content true))))

(defmacro compile
  [node]
  (binding [*env* &env]
    (let [ret (-> (compilable-node node)
                  (c/token-serializer)
                  (compact)
                  (vec)
                  (vary-meta assoc ::c/content true))]
      (if (== (count ret) 1)
        (nth ret 0)
        ret))))

;; CompilableForm

(extend-protocol CompilableForm
  dev.onionpancakes.chassis.core.OpeningTag
  (constant? [this]
    ;; Attrs is evaluated, but may not constant.
    (constant? (.-attrs this)))
  (evaluated? [this] true)
  (resolved [this]
    (c/->OpeningTag (.-metadata this)
                    (.-tag this)
                    (.-head-id this)
                    (.-head-class this)
                    (resolved (.-attrs this))))
  dev.onionpancakes.chassis.core.ClosingTag
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  dev.onionpancakes.chassis.core.RawString
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  clojure.lang.MapEntry
  (constant? [this]
    (every? constant? this))
  (evaluated? [this]
    (every? evaluated? this))
  (resolved [this]
    (clojure.lang.MapEntry. (resolved (key this))
                            (resolved (val this))))
  clojure.lang.IPersistentCollection
  (constant? [this]
    (every? constant? this))
  (evaluated? [this]
    (every? evaluated? this))
  (resolved [this]
    (into (empty this) (map resolved) this))
  clojure.lang.Keyword
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  ;; Lists are compilation barriers.
  ;; Not constants, not evaluated, not resolved.
  clojure.lang.ISeq
  (constant? [_] false)
  (evaluated? [_] false)
  (resolved [this] this)
  clojure.lang.Symbol
  (constant? [_] false)
  (evaluated? [_] false)
  (resolved [this]
    (if-some [resolved (resolve *env* this)]
      (if (var? resolved)
        (deref resolved)
        resolved)
      this))
  ;; This catches Strings, constant Numbers, and a bit more.
  java.lang.constant.Constable
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Object
  (constant? [_] false)
  (evaluated? [_] true)
  (resolved [this] this)
  nil
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [_] nil))

;; CompilableNode

(defn attrs-present?
  [elem]
  (c/has-attrs? elem))

(defn attrs-present-evaluated?
  [elem]
  (let [attrs (nth elem 1)
        _     (assert (c/attrs? attrs))]
    (evaluated? attrs)))

(defn attrs-absent?
  [[_ x :as elem]]
  (or (== (count elem) 1)
      (and (vector? x) (c/element-vector? x))
      (and (not (c/attrs? x)) (evaluated? x))))

(defn compilable-alias-element-children-attrs-present-evaluated
  [elem]
  (let [metadata   (meta elem)
        head       (nth elem 0)
        attrs      (nth elem 1)
        opening    (c/make-opening-tag metadata head attrs)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)]
    [`(c/resolve-alias ~metadata
                       ~tag
                       ~(c/make-head-attrs head-id head-class attrs)
                       (compile* ~(c/content-subvec elem 2)))]))

(defn compilable-alias-element-children-attrs-present
  [elem]
  (let [metadata   (meta elem)
        head       (nth elem 0)
        attrs      (nth elem 1)
        opening    (c/make-opening-tag metadata head attrs)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)]
    [`(c/resolve-alias ~metadata
                       ~tag
                       ~(if (or head-id head-class)
                         `(c/make-head-attrs ~head-id ~head-class ~attrs)
                         attrs)
                       (compile* ~(c/content-subvec elem 2)))]))

(defn compilable-alias-element-children-attrs-absent
  [elem]
  (let [metadata   (meta elem)
        head       (nth elem 0)
        opening    (c/make-opening-tag metadata head nil)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)]
    [`(c/resolve-alias ~metadata
                       ~tag
                       ~(c/make-head-attrs head-id head-class)
                       (compile* ~(c/content-subvec elem 1)))]))

(defn compilable-alias-element-children-attrs-ambig
  [elem]
  (let [metadata   (meta elem)
        head       (nth elem 0)
        attrs      (nth elem 1)
        opening    (c/make-opening-tag metadata head nil)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)
        attrs-sym  (gensym "attrs")]
    [`(let [~attrs-sym ~attrs]
        (if (c/attrs? ~attrs-sym)
          (c/resolve-alias ~metadata
                           ~tag
                           ~(if (or head-id head-class)
                              `(c/make-head-attrs ~head-id ~head-class ~attrs-sym)
                              attrs-sym)
                           (compile* ~(c/content-subvec elem 2)))
          (c/resolve-alias ~metadata
                           ~tag
                           ~(c/make-head-attrs head-id head-class)
                           (compile* ~[attrs-sym (c/content-subvec elem 2)]))))]))

(defn compilable-alias-element-children
  [elem]
  (if (attrs-present? elem)
    (if (attrs-present-evaluated? elem)
      (compilable-alias-element-children-attrs-present-evaluated elem)
      (compilable-alias-element-children-attrs-present elem))
    (if (attrs-absent? elem)
      (compilable-alias-element-children-attrs-absent elem)
      (compilable-alias-element-children-attrs-ambig elem))))

(defn compilable-element-children-attrs-present-evaluated
  [elem]
  (let [metadata (meta elem)
        head     (nth elem 0)
        attrs    (nth elem 1)
        opening  (c/make-opening-tag metadata head attrs)
        tag      (.-tag opening)]
    (if (and (c/void-tag? tag) (<= (count elem) 2))
      [opening]
      [opening
       (c/content-subvec elem 2)
       (c/->ClosingTag metadata tag)])))

(defn compilable-element-children-attrs-present
  [elem]
  (let [metadata   (meta elem)
        head       (nth elem 0)
        attrs      (nth elem 1)
        opening    (c/make-opening-tag metadata head attrs)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)]
    (if (and (c/void-tag? tag) (<= (count elem) 2))
      [`(c/->OpeningTag ~metadata ~tag ~head-id ~head-class ~attrs)]
      [`(c/->OpeningTag ~metadata ~tag ~head-id ~head-class ~attrs)
       (c/content-subvec elem 2)
       (c/->ClosingTag metadata tag)])))

(defn compilable-element-children-attrs-absent
  [elem]
  (let [metadata   (meta elem)
        head       (nth elem 0)
        opening    (c/make-opening-tag metadata head nil)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)]
    (if (and (c/void-tag? tag) (<= (count elem) 1))
      [(c/->OpeningTag metadata tag head-id head-class nil)]
      [(c/->OpeningTag metadata tag head-id head-class nil)
       (c/content-subvec elem 1)
       (c/->ClosingTag metadata tag)])))

(defn compilable-element-children-attrs-ambig
  [elem]
  (let [metadata   (meta elem)
        head       (nth elem 0)
        attrs      (nth elem 1)
        opening    (c/make-opening-tag metadata head nil)
        tag        (.-tag opening)
        head-id    (.-head-id opening)
        head-class (.-head-class opening)
        attrs-sym  (gensym "attrs")]
    (if (and (c/void-tag? tag) (<= (count elem) 2))
      [`(let [~attrs-sym ~attrs]
          (if (c/attrs? ~attrs-sym)
            (c/->OpeningTag ~metadata ~tag ~head-id ~head-class ~attrs-sym)
            [~(c/->OpeningTag metadata tag head-id head-class nil)
             ~attrs-sym
             ~(c/->ClosingTag metadata tag)]))]
      [`(let [~attrs-sym ~attrs]
          (if (c/attrs? ~attrs-sym)
            (c/->OpeningTag ~metadata ~tag ~head-id ~head-class ~attrs-sym)
            [~(c/->OpeningTag metadata tag head-id head-class nil)
             ~attrs-sym]))
       (c/content-subvec elem 2)
       (c/->ClosingTag metadata tag)])))

(defn compilable-element-children
  [elem]
  (if (attrs-present? elem)
    (if (attrs-present-evaluated? elem)
      (compilable-element-children-attrs-present-evaluated elem)
      (compilable-element-children-attrs-present elem))
    (if (attrs-absent? elem)
      (compilable-element-children-attrs-absent elem)
      (compilable-element-children-attrs-ambig elem))))

(extend-protocol CompilableNode
  clojure.lang.IPersistentVector
  (compilable-node [this]
    (reify c/Node
      (branch? [_] true)
      (children [_]
        (if (c/element-vector? this)
          (if (c/alias-element? this)
            (mapv compilable-node (compilable-alias-element-children (resolved this)))
            (mapv compilable-node (compilable-element-children (resolved this))))
          (mapv compilable-node (resolved this))))))
  clojure.lang.ISeq
  (compilable-node [this]
    (-> (resolved this) ; Default no-op in CompilableForm for lists.
        (vary-meta assoc ::c/leaf true)))
  Object
  (compilable-node [this] (resolved this))
  nil
  (compilable-node [_] (resolved nil)))
