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
    [:div [:p "foo"] (example-elem-macro "123") [:p "bar"]]
    [c/doctype-html5 [:div "foo" c/nbsp "bar"]]))

(deftest test-compile-attrs-reflection
  (let [attrs nil
        ret   (cc/compile [:div ^java.util.Map attrs "foobar"])]
    ;; how to write test???"
    ;; macroexpand doesn't capture &env
    )
  (let [^java.util.Map attrs nil
        ret                  (cc/compile [:div attrs "foobar"])]
    ;; how to write test???"
    ;; macroexpand doesn't capture &env
    ))
