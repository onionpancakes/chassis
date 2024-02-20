(ns dev.onionpancakes.chassis.tests.test-compiler
  (:require [dev.onionpancakes.chassis.core :as c]
            [dev.onionpancakes.chassis.compiler :as cc]
            [clojure.test :refer [deftest are is]]))

(def example-constant
  "foobar")

(def example-deref
  (delay "foobar"))

(defn example-fn []
  "foobar")

(def example-attrs
  {:foo "bar"})

(defn example-elem-fn
  [arg]
  [:p arg])

(defmacro example-elem-macro
  [arg]
  [:p arg])

(defmacro example-elem-macro-nested
  [arg]
  `(example-elem-macro ~arg))

(defmethod c/resolve-alias ::Foo
  [_ _ attrs content]
  [:p.alias attrs content])

(deftest test-compile
  (are [node] (= (c/html (cc/compile node))
                 (c/html (cc/compile* node))
                 (c/html node))
    nil
    0
    0.0
    ""
    "foobar"
    {}
    #{}
    '()
    []
    [:div]
    [:div "foo"]
    [:div#foo "foo"]
    [:div.123 "foo"]
    [:div#foo.123 "foo"]
    [:div [:p 123] [:p 456]]
    [:div example-constant]
    [:div example-deref]
    [:div example-fn]
    [:div nil]
    [:div {:foo "bar"}]
    [:div {:foo "bar"} "baz"]
    [:div example-attrs]
    [:div (example-elem-fn "foo")]
    [:div (example-elem-macro "foo")]
    [:div (example-elem-macro-nested "foo")]
    [:div (for [i (range 4)]
            [:p i])]
    (map inc (range 5))
    [:div (map inc (range 5))]
    [:div nil (map inc (range 5))]
    [::Foo]
    [::Foo nil]
    [::Foo nil "foobar"]
    [::Foo nil "foo" [::Foo "bar"]]
    [::Foo {:foo "bar"}]
    [::Foo example-fn]
    [::Foo#foo.bar]
    [c/doctype-html5 [:div "foo" c/nbsp "bar"]]))

(deftest test-compile-full-compaction
  (are [node] (let [ret (cc/compile node)]
                (and (instance? dev.onionpancakes.chassis.core.RawString ret)
                     (= (c/fragment ret) (c/html node))))
    nil
    ""
    [:div]
    [:div#foo.bar "123"]
    [:div {:foo "bar"} "123"]
    [:div {:foo example-constant}]
    [:div [:p "foo"] [:p "bar"]]
    (example-elem-macro "123")
    (example-elem-macro-nested "123")
    [:div [:p "foo"] (example-elem-macro "123") [:p "bar"]]
    [:div [:p "foo"] (example-elem-macro-nested "123") [:p "bar"]]
    [c/doctype-html5 [:div "foo" c/nbsp "bar"]]))

;; Attributes reflection tests
;; Warnings are emitted at compile time,
;; so warning detection is a side effect?

(def ambig-attrs-count
  (atom 0))

(defonce count-ambig-attrs
  (fn [m]
    (when (identical? (::cc/type m) :warn-on-ambig-attrs)
      (swap! (deref #'ambig-attrs-count) inc))))

(add-tap count-ambig-attrs)

(do
  ;; Compile attrs reflection examples
  (let [attrs nil]
    (cc/compile [:div ^java.util.Map attrs "foobar"]))
  (let [^java.util.Map attrs nil]
    (cc/compile [:div attrs "foobar"]))
  (defmethod c/resolve-alias ::ReflectiveAttrsAlias
    [_ _ ^java.util.Map attrs content]
    (cc/compile [:div.reflective-alias-attrs attrs content]))
  ;; Type hinted invocation
  (cc/compile [:div ^java.util.Map (:foo {:foo {}}) "foobar"])
  ;; Vetted attrs fns
  (cc/compile [:div (array-map :foo "bar") "foobar"])
  (cc/compile [:div (hash-map :foo "bar") "foobar"])
  (cc/compile [:div (sorted-map :foo "bar") "foobar"])
  (cc/compile [:div (sorted-map-by compare :foo "bar") "foobar"])
  (cc/compile [:div (assoc {} :foo "bar") "foobar"])
  (cc/compile [:div (assoc-in {} [:foo :bar] "bar") "foobar"])
  (cc/compile [:div (merge {} {:foo "bar"}) "foobar"])
  (cc/compile [:div (select-keys {} [:foo]) "foobar"])
  (cc/compile [:div (update-keys {} identity) "foobar"])
  (cc/compile [:div (update-vals {} identity) "foobar"])
  ;; LocalBinded attrs literals
  (let [attrs nil]
    (cc/compile [:div attrs "foobar"]))
  (let [attrs {}]
    (cc/compile [:div attrs "foobar"]))
  ;; ConstantExpr not public, can't determine constant attrs case.
  #_
  (let [attrs {:foo "bar"}]
    (cc/compile [:div attrs "foobar"]))
  (let [attrs {:foo (identity "bar")}]
    (cc/compile [:div attrs "foobar"]))
  )

(remove-tap count-ambig-attrs)

(deftest test-compile-attrs-reflection
  (is (zero? @ambig-attrs-count)))
