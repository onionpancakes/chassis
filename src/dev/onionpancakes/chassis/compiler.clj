(ns dev.onionpancakes.chassis.compiler
  (:refer-clojure :exclude [compile])
  (:require [dev.onionpancakes.chassis.core :as c]))

(defprotocol CompilableForm
  (attrs? [this] "Returns true if form is attrs. Returns false if it might be attrs.")
  (not-attrs? [this] "Returns true if form is not attrs. Returns false if it might be attrs.")
  (constant? [this] "Returns true if form is constant, safe to make fragments with at compile time.")
  (evaluated? [this] "Returns true if form is evaluated, safe to make tokens with at compile time.")
  (resolved [this] "Returns the form in which symbols and coll of symbols are resolved."))

(defprotocol CompilableNode
  (branch? [this] "Returns true if branch node.")
  (children [this] "Returns children as Iterable."))

;; Binding of macro &env, for resolving symbols.
(def ^:dynamic *env* nil)

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
    (-> (c/tree-serializer branch? children node)
        (compact)
        (vec)
        (vary-meta assoc ::c/content true))))

(defmacro compile
  [node]
  (binding [*env* &env]
    (let [ret (-> (c/tree-serializer branch? children node)
                  (compact)
                  (vec)
                  (vary-meta assoc ::c/content true))]
      (case (count ret)
        0 nil
        1 (nth ret 0)
        ret))))

;; CompilableForm

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
  (attrs? [this] false)  ; todo detect by examining invoked var
  (not-attrs? [this] false)
  ;; Lists are compilation barriers.
  ;; Not constants, not evaluated.
  (constant? [_] false)
  (evaluated? [_] false)
  ;; But not macro barriers.
  (resolved [this]
    (macroexpand-1 this))
  clojure.lang.Symbol
  (attrs? [this]
    ;; todo detect by examining *env*, global var, and type hints
    #_
    (when-some [b (get *env* this)]
      (println :tag (.-tag b))
      (println :init (.-init b))
      (println :type (type (.-init b)))
      #_#_
      (println :fexpr (.eval (.-fexpr (.-init b))))
      (println :init-tag (.-tag (.-init b))))
    false)
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

;; CompilableNode

(defn attrs-present?
  [elem]
  (and (>= (count elem) 2)
       (attrs? (nth elem 1))))

(defn attrs-present-evaluated?
  [elem]
  {:pre [(attrs-present? elem)]}
  (evaluated? (nth elem 1)))

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
                       (compile ~(c/content-subvec elem 2)))]))

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
                       (compile ~(c/content-subvec elem 2)))]))

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
                       (compile ~(c/content-subvec elem 1)))]))

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
                           (compile ~(c/content-subvec elem 2)))
          (c/resolve-alias ~metadata
                           ~tag
                           ~(c/make-head-attrs head-id head-class)
                           (compile ~[attrs-sym (c/content-subvec elem 2)]))))]))

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
  (branch? [_] true)
  (children [this]
    (let [resv (resolved this)]
      (if (c/element-vector? resv)
        (if (c/alias-element? resv)
          (compilable-alias-element-children resv)
          (compilable-element-children resv))
        resv)))
  Object
  (branch? [_] false)
  (children [_] nil)
  nil
  (branch? [_] false)
  (children [_] nil))
