(ns dev.onionpancakes.chassis.compiler
  (:refer-clojure :exclude [compile])
  (:require [dev.onionpancakes.chassis.core :as c]))

(defprotocol CompilableForm
  ;; 99.99% cases, evaluated == constant.
  ;; Via data readers, it is possible to generate
  ;; an evaluated stateful value.
  ;; This split is the cover that pendantic case.
  (constant? [this])
  (evaluated? [this]))

(defprotocol CompilableNode
  (compilable-node [this]))

;; Compile

(defn compact-forms
  [forms]
  (let [sb (StringBuilder.)
        _  (reduce c/append-fragment sb forms)]
    (c/raw (.toString sb))))

(defn compact
  [forms]
  (eduction (partition-by constant?)
            (mapcat (fn [tokens]
                      (if (constant? (first tokens))
                        [(compact-forms tokens)]
                        tokens)))
            forms))

(defmacro compile
  [node]
  (->> (compilable-node node)
       (c/token-serializer)
       (compact)
       (vec)))

;; CompilableForm

(extend-protocol CompilableForm
  dev.onionpancakes.chassis.core.OpeningTag
  (constant? [this]
    ;; Attrs is evaluated, but may not constant.
    (constant? (.-attrs this)))
  (evaluated? [this] true)
  dev.onionpancakes.chassis.core.ClosingTag
  (constant? [_] true)
  (evaluated? [_] true)
  dev.onionpancakes.chassis.core.RawString
  (constant? [_] true)
  (evaluated? [_] true)
  clojure.lang.IPersistentCollection
  (constant? [this]
    (every? constant? this))
  (evaluated? [this]
    (every? evaluated? this))
  clojure.lang.Keyword
  (constant? [_] true)
  (evaluated? [_] true)
  clojure.lang.ISeq
  (constant? [_] false)
  (evaluated? [_] false)
  clojure.lang.Symbol
  (constant? [_] false)
  (evaluated? [_] false)
  ;; This catches Strings, constant Numbers, and a bit more.
  java.lang.constant.Constable
  (constant? [_] true)
  (evaluated? [_] true)
  Object
  (constant? [_] false)
  (evaluated? [_] true)
  nil
  (constant? [_] true)
  (evaluated? [_] true))

;; CompilableNode

(defn compilable-element-children-attrs-present-evaluated
  [elem]
  (let [metadata (meta elem)
        head     (nth elem 0)
        opening  (c/make-opening-tag metadata head nil)
        attrs    (nth elem 1)]
    [(c/->OpeningTag metadata
                     (.-tag opening)
                     (.-head-id opening)
                     (.-head-class opening)
                     attrs)
     (c/content-subvec elem 2)
     (c/->ClosingTag metadata (.-tag opening))]))

(defn compilable-element-children-attrs-present
  [elem]
  (let [metadata (meta elem)
        head     (nth elem 0)
        opening  (c/make-opening-tag metadata head nil)
        attrs    (nth elem 1)]
    [`(c/->OpeningTag ~metadata
                      ~(.-tag opening)
                      ~(.-head-id opening)
                      ~(.-head-class opening)
                      ~attrs)
     (c/content-subvec elem 2)
     (c/->ClosingTag metadata (.-tag opening))]))

(defn compilable-element-children-attrs-absent
  [elem]
  (let [metadata (meta elem)
        head     (nth elem 0)
        opening  (c/make-opening-tag metadata head nil)]
    [(c/->OpeningTag metadata
                     (.-tag opening)
                     (.-head-id opening)
                     (.-head-class opening)
                     nil)
     (c/content-subvec elem 1)
     (c/->ClosingTag metadata (.-tag opening))]))

(defn compilable-element-children-attrs-ambig
  [elem]
  (let [metadata  (meta elem)
        head      (nth elem 0)
        opening   (c/make-opening-tag metadata head nil)
        attrs     (nth elem 1)
        attrs-sym (gensym "attrs")]
    [`(let [~attrs-sym ~attrs]
        (if (c/attrs? ~attrs-sym)
          (c/->OpeningTag ~metadata
                          ~(.-tag opening)
                          ~(.-head-id opening)
                          ~(.-head-class opening)
                          ~attrs-sym)
          [~(c/->OpeningTag metadata
                           (.-tag opening)
                           (.-head-id opening)
                           (.-head-class opening)
                           nil)
           ~attrs-sym]))
     (c/content-subvec elem 2)
     (c/->ClosingTag metadata (.-tag opening))]))

(defn attrs-present?
  [elem]
  (c/has-attrs? elem))

(defn attrs-present-evaluated?
  [elem]
  (let [attrs (nth elem 1)
        _     (assert (c/attrs? attrs))]
    (evaluated? attrs)))

(defn attrs-absent?
  [elem]
  (or (== (count elem) 1)
      (and (vector? (nth elem 1 nil))
           (c/element-vector? (nth elem 1 nil)))
      (string? (nth elem 1 nil))
      (number? (nth elem 1 nil))))

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
          (mapv compilable-node (compilable-element-children this))
          (mapv compilable-node this)))))
  clojure.lang.ISeq
  (compilable-node [this]
    (vary-meta this assoc ::c/leaf true))
  Object
  (compilable-node [this] this)
  nil
  (compilable-node [_] nil))
