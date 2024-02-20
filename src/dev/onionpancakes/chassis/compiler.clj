(ns dev.onionpancakes.chassis.compiler
  (:refer-clojure :exclude [compile])
  (:require [dev.onionpancakes.chassis.core :as c]))

;; Examine compiler binding expressions for attrs maps.
(defprotocol AttributesCompilerExpr
  (attrs-compiler-expr? [this] "Returns true if clojure.lang.Compiler$Expr is attrs."))

(defprotocol CompilableForm
  (attrs? [this] "Returns true if form is attrs. Returns false if it might be attrs.")
  (not-attrs? [this] "Returns true if form is not attrs. Returns false if it might be attrs.")
  (constant? [this] "Returns true if form is constant, safe to make fragments with at compile time.")
  (evaluated? [this] "Returns true if form is evaluated, safe to make tokens with at compile time.")
  (resolved [this] "Returns the form in which symbols are resolved and outer macros expanded."))

(defprotocol CompilableNode
  (branch? [this] "Returns true if branch node.")
  (children [this] "Returns children of node."))

;; Binding of macro &env, for resolving symbols.
(def ^:dynamic *env* nil)
(def ^:dynamic *form* nil)

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
  (binding [*env*  &env
            *form* &form]
    (let [rch (fn [node]
                (mapv resolved (children node)))
          ret (->> (resolved node)
                   (c/tree-serializer branch? rch)
                   (compact)
                   (vec))]
      (vary-meta ret assoc ::c/content true))))

(defmacro compile
  [node]
  (binding [*env*  &env
            *form* &form]
    (let [rch (fn [node]
                (mapv resolved (children node)))
          ret (->> (resolved node)
                   (c/tree-serializer branch? rch)
                   (compact)
                   (vec))]
      (case (count ret)
        0 nil
        1 (nth ret 0)
        (vary-meta ret assoc ::c/content true)))))

;; CompilableForm

(def attrs-invocable-vars
  #{#'clojure.core/array-map
    #'clojure.core/hash-map
    #'clojure.core/sorted-map
    #'clojure.core/sorted-map-by
    #'clojure.core/assoc
    #'clojure.core/assoc-in
    #'clojure.core/merge
    #'clojure.core/select-keys
    #'clojure.core/update-keys
    #'clojure.core/update-vals})

(defn attrs-invocation?
  [[sym & _]]
  (and (symbol? sym)
       (contains? attrs-invocable-vars (resolve sym))))

(defn attrs-type?
  [clazz]
  (or (isa? clazz java.util.Map)
      (isa? clazz clojure.lang.IPersistentMap)))

(defn attrs-meta-tag?
  [tag]
  {:pre [(symbol? tag)]}
  (attrs-type? (resolve tag)))

(defn attrs-type-hinted?
  [obj]
  (if-some [tag (:tag (meta obj))]
    (attrs-meta-tag? tag)
    false))

(defn attrs-compiler-binding?
  [^clojure.lang.Compiler$LocalBinding b]
  (and (some? b)
       (attrs-compiler-expr? (.-init b))))

(extend-protocol AttributesCompilerExpr
  clojure.lang.Compiler$NilExpr
  (attrs-compiler-expr? [_] true)
  clojure.lang.Compiler$MapExpr
  (attrs-compiler-expr? [this] true)
  clojure.lang.Compiler$EmptyExpr
  (attrs-compiler-expr? [this]
    (attrs-type? (.getJavaClass this)))
  clojure.lang.Compiler$ConstantExpr
  (attrs-compiler-expr? [this]
    ;; ConstantExpr not public class, can't call getJavaClass() method.
    ;; (attrs-type? (.getJavaClass this))
    ;; ConstantExpr extends LiteralExpr.
    ;; LiteralExpr is public class, and eval() is public method.
    ;; Therefore use eval() instead.
    ;; Should be safe. LiteralExpr.eval() just returns the literal object.
    (attrs-type? (type (.eval this))))
  #_#_
  clojure.lang.Compiler$InvokeExpr
  (attrs-compiler-expr? [this]
    ;; InvokeExpr is not public class
    ;; Type hints on invokes not accessible.
    (println (.-tag this))
    false)
  Object
  (attrs-compiler-expr? [_] false)
  nil
  (attrs-compiler-expr? [_] false))

(extend-protocol CompilableForm
  dev.onionpancakes.chassis.core.OpeningTag
  (attrs? [this] false)
  (not-attrs? [this] true)
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
  (attrs? [this] false)
  (not-attrs? [this] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  dev.onionpancakes.chassis.core.RawString
  (attrs? [this] false)
  (not-attrs? [this] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  clojure.lang.MapEntry
  (attrs? [this] false)
  (not-attrs? [this] true)
  (constant? [this]
    (and (constant? (key this))
         (constant? (val this))))
  (evaluated? [this]
    (and (evaluated? (key this))
         (evaluated? (val this))))
  (resolved [this]
    (clojure.lang.MapEntry. (resolved (key this))
                            (resolved (val this))))
  clojure.lang.IPersistentCollection
  (attrs? [this]
    (map? this))
  (not-attrs? [this]
    (not (map? this)))
  (constant? [this]
    (every? constant? this))
  (evaluated? [this]
    (every? evaluated? this))
  (resolved [this]
    (into (empty this) (map resolved) this))
  clojure.lang.Keyword
  (attrs? [this] false)
  (not-attrs? [this] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  clojure.lang.ISeq
  (attrs? [this]
    (or (attrs-type-hinted? this)
        (attrs-invocation? this)))
  (not-attrs? [this] false)
  ;; Lists are compilation barriers.
  ;; Not constants, not evaluated.
  (constant? [_] false)
  (evaluated? [_] false)
  ;; But not macro barriers.
  (resolved [this]
    (macroexpand this))
  clojure.lang.Symbol
  (attrs? [this]
    (or (attrs-type-hinted? this)
        (if-some [entry (find *env* this)]
          (or (attrs-type-hinted? (key entry))
              (attrs-compiler-binding? (val entry)))
          false)))
  (not-attrs? [this] false)
  (constant? [_] false)
  (evaluated? [_] false)
  (resolved [this]
    (if-some [res (resolve *env* this)]
      (let [val (if (var? res) @res res)]
        ;; Use constant? as guard against un-embedable code.
        ;; Works for now...
        (if (constant? val) val this))
      this))
  ;; This catches Strings, constant Numbers, and a bit more.
  java.lang.constant.Constable
  (attrs? [this] false)
  (not-attrs? [this] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Object
  (attrs? [this] false)
  (not-attrs? [this] false)
  (constant? [_] false)
  (evaluated? [_] true)
  (resolved [this] this)
  nil
  (attrs? [this] true)
  (not-attrs? [this] false)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [_] nil))

;; Warn

(defn send-warn-on-ambig-attrs!
  [form elem]
  (tap> {::type :warn-on-ambig-attrs
         ::form form
         ::elem elem}))

(defonce warn-on-ambig-attrs
  (fn [x]
    (if (identical? (::type x) :warn-on-ambig-attrs)
      (binding [*out* *err*]
        (println "Compiling element with ambiguous attrs:" (::elem x))
        (println "Found within form:" (meta (::form x)))
        (println (::form x))
        (println)))))

(defn set-warn-on-ambig-attrs! []
  (add-tap warn-on-ambig-attrs))

(defn unset-warn-on-ambig-attrs! []
  (remove-tap warn-on-ambig-attrs))

;; CompilableNode

(defn attrs-present?
  [elem]
  (and (>= (count elem) 2)
       (attrs? (nth elem 1))))

(defn attrs-absent?
  [[_ x :as elem]]
  (or (<= (count elem) 1)
      (not-attrs? (nth elem 1))))

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
    (if (evaluated? (nth elem 1))
      (compilable-alias-element-children-attrs-present-evaluated elem)
      (compilable-alias-element-children-attrs-present elem))
    (if (attrs-absent? elem)
      (compilable-alias-element-children-attrs-absent elem)
      (do
        (send-warn-on-ambig-attrs! *form* elem)
        (compilable-alias-element-children-attrs-ambig elem)))))

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
    (if (evaluated? (nth elem 1))
      (compilable-element-children-attrs-present-evaluated elem)
      (compilable-element-children-attrs-present elem))
    (if (attrs-absent? elem)
      (compilable-element-children-attrs-absent elem)
      (do
        (send-warn-on-ambig-attrs! *form* elem)
        (compilable-element-children-attrs-ambig elem)))))

(extend-protocol CompilableNode
  clojure.lang.IPersistentVector
  (branch? [_] true)
  (children [this]
    (if (c/element-vector? this)
      (if (c/alias-element? this)
        (compilable-alias-element-children this)
        (compilable-element-children this))
      this))
  Object
  (branch? [_] false)
  (children [_] nil)
  nil
  (branch? [_] false)
  (children [_] nil))
