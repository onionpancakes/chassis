(ns dev.onionpancakes.chassis.compiler
  (:refer-clojure :exclude [compile])
  (:require [dev.onionpancakes.chassis.core :as c]))

(defprotocol CompilableNode
  (compilable-node [this]))

;; Compile

(defn compactable?
  [token]
  (or (string? token)
      (instance? dev.onionpancakes.chassis.core.OpeningTag token)
      (instance? dev.onionpancakes.chassis.core.ClosingTag token)))

(defn compact-tokens
  [tokens]
  (let [sb (StringBuilder.)
        _  (reduce c/append-fragment sb tokens)]
    (c/raw (.toString sb))))

(def compact-xf
  (comp (partition-by compactable?)
        (mapcat (fn [tokens]
                  (if (compactable? (first tokens))
                    [(compact-tokens tokens)]
                    tokens)))))

(defmacro compile
  [node]
  (->> (compilable-node node)
       (c/token-serializer)
       (eduction compact-xf)
       (vec)))

;; Compilable

(defn attrs-present?
  [elem]
  (c/has-attrs? elem))

(defn attrs-absent?
  [elem]
  (or (== (count elem) 1)
      (and (vector? (nth elem 1 nil))
           (c/element-vector? (nth elem 1 nil)))
      (string? (nth elem 1 nil))
      (number? (nth elem 1 nil))))

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

(defn compilable-element-children
  [elem]
  (if (attrs-present? elem)
    (compilable-element-children-attrs-present elem)
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
    (vary-meta this assoc ::c/branch? false))
  Object
  (compilable-node [this] this)
  nil
  (compilable-node [_] nil))
