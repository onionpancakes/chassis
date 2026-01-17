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

(def ^:dynamic *evaluated*
  "Set true if current evaluation context is considered evaluated.
  i.e. True when compiling at runtime. False when compiling at macro time."
  true)

;; Binding of macro &env.
(def ^:dynamic *env*)

;; Binding of macro &form.
(def ^:dynamic *form*)

;; Compile

(defn compacted-form
  [tokens]
  (let [sb (StringBuilder.)
        tf (fn [_ form]
             (throw (IllegalArgumentException. (str "Not constant form: " form))))
        xf (halt-when (complement constant?) tf)
        _  (transduce xf c/append-fragment sb tokens)]
    (c/raw (.toString sb))))

(defn compact
  [tokens]
  (eduction (partition-by constant?)
            (mapcat (fn [forms]
                      (if (constant? (first forms))
                        [(compacted-form forms)]
                        forms)))
            tokens))

(defn resolved-children
  [node]
  (mapv resolved (children node)))

(defn compile-node*
  "Compiles the node, returning a compacted equivalent as a content vector.

  This is a callable function for compiling forms at runtime.
  For normal use case, use the macros."
  [node]
  (let [ret (->> (resolved node)
                 (c/tree-serializer branch? resolved-children)
                 (compact)
                 (vec))]
    (with-meta ret {::c/content true})))

(defn compile-node
  "Compiles the node, returning a compacted equivalent node.
  The return value may be a content vector or an unwrapped value
  if fewer than two forms are returned.

  This is a callable function for compiling forms at runtime.
  For normal use case, use the macros."
  [node]
  (let [ret (compile-node* node)]
    (case (count ret)
      0 nil
      1 (nth ret 0)
      ret)))

(defmacro compile*
  "Compiles the node form, returning a compacted equivalent form as a content vector."
  [node]
  (binding [*evaluated* false
            *env*       &env
            *form*      &form]
    (compile-node* node)))

(defmacro compile
  "Compiles the node form, returning a compacted equivalent form.
  The return value may be a content vector or an unwrapped value
  if fewer than two forms are returned."
  [node]
  (binding [*evaluated* false
            *env*       &env
            *form*      &form]
    (compile-node node)))

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
  [[sym & _ :as this]]
  ;; Check seq? because it catches both Cons and List types.
  (and (seq? this)
       (symbol? sym)
       (bound? #'*env*)
       (contains? attrs-invocable-vars (resolve *env* sym))))

(defn attrs-type?
  [clazz]
  (or (isa? clazz java.util.Map)
      (isa? clazz clojure.lang.IPersistentMap)))

(defn attrs-meta-tag?
  [tag]
  ;; Tags are symbols at compile time.
  ;; Tags auto resolve to class by runtime.
  ;; Impl class case just for completeness.
  (or (and (symbol? tag)
           ;; Do not resolve with *env*.
           ;; It's not possible to shadow a
           ;; class symbol in a binding anyways.
           (attrs-type? (resolve tag)))
      (and (class? tag)
           (attrs-type? tag))))

(defn attrs-type-hinted?
  [obj]
  (attrs-meta-tag? (:tag (meta obj))))

(defn attrs-compiler-binding?
  [^clojure.lang.Compiler$LocalBinding b]
  (and (instance? clojure.lang.Compiler$LocalBinding b)
       (attrs-compiler-expr? (.-init b))))

(extend-protocol AttributesCompilerExpr
  #_#_
  clojure.lang.Compiler$NilExpr
  (attrs-compiler-expr? [_] true)
  #_#_
  clojure.lang.Compiler$MapExpr
  (attrs-compiler-expr? [_] true)
  #_#_
  clojure.lang.Compiler$EmptyExpr
  (attrs-compiler-expr? [this]
    (attrs-type? (.getJavaClass this)))
  #_#_
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
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [this]
    ;; Attrs is evaluated, but may not constant.
    (constant? (.-attrs this)))
  (evaluated? [_] true)
  (resolved [this]
    (c/->OpeningTag (.-metadata this)
                    (.-tag this)
                    (.-head-id this)
                    (.-head-class this)
                    (resolved (.-attrs this))))
  dev.onionpancakes.chassis.core.ClosingTag
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  dev.onionpancakes.chassis.core.RawString
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  clojure.lang.MapEntry
  (attrs? [_] false)
  (not-attrs? [_] true)
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
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  ;; ISeq catches both Cons and List types.
  clojure.lang.ISeq
  (attrs? [this]
    (and (not *evaluated*)
         (or (attrs-type-hinted? this)
             (attrs-invocation? this))))
  (not-attrs? [_]
    (boolean *evaluated*))
  ;; Lists are compilation barriers.
  ;; Not constants, not evaluated.
  (constant? [this]
    (and (boolean *evaluated*)
         (every? constant? this)))
  (evaluated? [_]
    (boolean *evaluated*))
  ;; But not macro barriers.
  (resolved [this]
    (if *evaluated*
      this
      (macroexpand this)))
  clojure.lang.Symbol
  (attrs? [this]
    (and (not *evaluated*)
         (or (attrs-type-hinted? this)
             (if-let [entry (and (bound? #'*env*)
                                 (find *env* this))]
               (or (attrs-type-hinted? (key entry))
                   (attrs-compiler-binding? (val entry)))
               false))))
  (not-attrs? [_]
    (boolean *evaluated*))
  (constant? [_]
    (boolean *evaluated*))
  (evaluated? [_]
    (boolean *evaluated*))
  (resolved [this]
    (if-let [res (and (not *evaluated*)
                      (bound? #'*env*)
                      (resolve *env* this))]
      (if (var? res)
        (if (or (:dynamic (meta res))
                (:redef (meta res)))
          this
          (let [val @res]
            ;; Use constant? as guard against un-embedable code.
            ;; Works for now...
            (if (constant? val) val this)))
        (if (constant? res) res this))
      this))
  java.util.Date
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] false) ; Not constant because not immutable.
  (evaluated? [_] true)
  (resolved [this] this)
  java.util.UUID
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Class
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Boolean
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Character
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  String
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Short
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Integer
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Long
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  BigInteger
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Float
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Double
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  BigDecimal
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  clojure.lang.BigInt
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  clojure.lang.Ratio
  (attrs? [_] false)
  (not-attrs? [_] true)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [this] this)
  Object
  (attrs? [this]
    (and (boolean *evaluated*)
         (c/attrs? this)))
  (not-attrs? [this]
    (and (boolean *evaluated*)
         (not (c/attrs? this))))
  (constant? [_] false)
  (evaluated? [_] true)
  (resolved [this] this)
  nil
  (attrs? [_] true)
  (not-attrs? [_] false)
  (constant? [_] true)
  (evaluated? [_] true)
  (resolved [_] nil))

;; Warn

(defn send-warn-on-ambig-attrs!
  [form elem]
  (tap> {::warning :warn-on-ambig-attrs
         ::form    form
         ::elem    elem}))

(defonce warn-on-ambig-attrs
  (fn [{::keys [warning form elem]}]
    (if (identical? warning :warn-on-ambig-attrs)
      (binding [*out* *err*]
        (println "Compiling element with ambiguous attrs:" elem)
        (println "Found within form:" (meta form))
        (println form)
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
    [`(c/resolve-alias-with-meta
       ~metadata
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
    [`(c/resolve-alias-with-meta
       ~metadata
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
    [`(c/resolve-alias-with-meta
       ~metadata
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
          (c/resolve-alias-with-meta
           ~metadata
           ~tag
           ~(if (or head-id head-class)
              `(c/make-head-attrs ~head-id ~head-class ~attrs-sym)
              attrs-sym)
           (compile* ~(c/content-subvec elem 2)))
          (c/resolve-alias-with-meta
           ~metadata
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
        (if (bound? #'*form*)
          (send-warn-on-ambig-attrs! *form* elem))
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
        tag        (.-tag opening)]
    (if (and (c/void-tag? tag) (<= (count elem) 1))
      [opening]
      [opening
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
            (compile
             [~(c/->OpeningTag metadata tag head-id head-class nil)
              ~attrs-sym
              ~(c/->ClosingTag metadata tag)])))]
      [`(let [~attrs-sym ~attrs]
          (if (c/attrs? ~attrs-sym)
            (c/->OpeningTag ~metadata ~tag ~head-id ~head-class ~attrs-sym)
            (compile
             [~(c/->OpeningTag metadata tag head-id head-class nil)
              ~attrs-sym])))
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
        (if (bound? #'*form*)
          (send-warn-on-ambig-attrs! *form* elem))
        (compilable-element-children-attrs-ambig elem)))))

(defn compilable-vector-children
  [this]
  (if (c/element-vector? this)
    (if (c/alias-element? this)
      (compilable-alias-element-children this)
      (compilable-element-children this))
    this))

(defn vector-children
  [this]
  (if *evaluated*
    (c/vector-children this)
    (compilable-vector-children this)))

(extend clojure.lang.IPersistentVector
  CompilableNode
  {:branch?  (fn [_] true)
   :children vector-children})

(extend-protocol CompilableNode
  clojure.lang.ISeq
  (branch? [_]
    (boolean *evaluated*))
  (children [this] this)
  Object
  (branch? [_] false)
  (children [_] nil)
  nil
  (branch? [_] false)
  (children [_] nil))
