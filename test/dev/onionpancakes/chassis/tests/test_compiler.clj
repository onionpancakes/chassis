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
  [_ attrs content]
  [:p.alias attrs content])

(deftest test-compile
  (are [node] (= (c/html (cc/compile-node* node))
                 (c/html (cc/compile-node node))
                 (c/html (cc/compile* node))
                 (c/html (cc/compile node))
                 (c/html node))
    nil
    0
    0.0
    \a
    ""
    "foobar"
    :foo
    true
    {}
    #{}
    '()
    []
    #inst "2007-01-04"
    #uuid "00000000-0000-0000-0000-000000000000"
    java.lang.String
    
    [:div]
    [:div nil]
    [:div nil "foo"]
    [:div "foo"]
    [:div#foo "foo"]
    [:div.123 "foo"]
    [:div#foo.123 "foo"]
    [:div 'foo]
    [:div 'example-constant]
    [:div [:p 123] [:p 456]]

    ;; Var syms
    [:div example-constant]
    [:div example-deref]
    [:div example-fn]

    [:div nil]
    [:div nil "foo"]
    [:div nil example-constant]
    [:div nil example-deref]
    [:div nil example-fn]
    [:div {:foo "bar"}]
    [:div {:foo "bar"} "baz"]
    [:div {:foo example-constant}]
    [:div {:foo example-deref}]
    [:div {:foo example-fn}]
    [:div example-attrs]    
    
    ;; Function calls
    (map inc (range 5))
    [:div (map inc (range 5))]
    [:div nil (map inc (range 5))]

    ;; Element function calls
    (example-elem-fn "foo")
    [:div (example-elem-fn "foo")]
    [:div (example-elem-macro "foo")]
    [:div (example-elem-macro-nested "foo")]

    ;; Macros expanding into lists
    [:div (for [i (range 4)]
            [:p i])]
    [:div (for [i (range 4)]
            (cc/compile [:p i]))]
    [:div (for [i (range 4)]
            (for [j (range 4)]
              [:p i j]))]

    ;; Voids
    [:hr]
    [:hr nil]
    [:hr {}]
    [:hr {:foo "bar"}]
    [:hr {:foo example-fn}]
    [:hr "foo"]
    [:hr nil "foo"]
    (let [foo nil]
      [:hr foo])
    (let [foo {:foo "bar"}]
      [:hr foo])
    (let [foo "foo"]
      [:hr foo "bar"])
    
    ;; Alias
    [::Foo]
    [::Foo nil]
    [::Foo nil "foobar"]
    [::Foo nil "foo" [::Foo "bar"]]
    [::Foo {:foo "bar"}]
    [::Foo example-fn]
    [::Foo#foo.bar]
    [:div [::Foo]]
    [:div [::Foo [::Foo]]]
    [:div
     [::Foo
      [:p "foobar"]
      [::Foo
       [:p 123]]]]

    ;; Var consts
    [c/doctype-html5 [:div "foo" c/nbsp "bar"]]))

(deftest test-compile-full-compaction
  (are [node] (let [ret (cc/compile node)]
                (and (instance? dev.onionpancakes.chassis.core.RawString ret)
                     (= (c/fragment ret) (c/html node))))
    nil
    0
    0N
    0.0
    0.0M
    3/2
    \a
    ""
    :foo
    true
    #uuid "00000000-0000-0000-0000-000000000000"
    java.lang.String
   
    [:div]
    [:div#foo.bar "123"]
    [:div {:foo "bar"} "123"]
    [:div [:p "foo"] [:p "bar"]]
    [:div [1 2 3 4]]
    [:div #{1 2 3 4}]
    [:div \a]
    [:div :foo]
    [:div true]
    [:div #uuid "00000000-0000-0000-0000-000000000000"]
    [:div java.lang.String]

    ;; Macros
    (example-elem-macro "123")
    (example-elem-macro-nested "123")
    [:div [:p "foo"] (example-elem-macro "123") [:p "bar"]]
    [:div [:p "foo"] (example-elem-macro-nested "123") [:p "bar"]]

    ;; Var consts
    [:div {:foo example-constant}]
    [c/doctype-html5 [:div "foo" c/nbsp "bar"]]))

(deftest test-compile-node-full-compaction
  (are [node] (let [ret (cc/compile-node node)]
                (and (instance? dev.onionpancakes.chassis.core.RawString ret)
                     (= (c/fragment ret) (c/html node))))
    nil
    0
    0N
    0.0
    0.0M
    3/2
    \a
    ""
    :foo
    true
    #uuid "00000000-0000-0000-0000-000000000000"
    java.lang.String

    (short 0)
    (int 0)
    (long 0)
    (bigint 0)
    (biginteger 0)
    (float 0)
    (double 0)
    (bigdec 0)
    
    [:div]
    [:div#foo.bar "123"]
    [:div {:foo "bar"} "123"]
    [:div [:p "foo"] [:p "bar"]]
    [:div [1 2 3 4]]
    [:div #{1 2 3 4}]
    [:div \a]
    [:div :foo]
    [:div true]
    [:div #uuid "00000000-0000-0000-0000-000000000000"]
    [:div java.lang.String]

    ;; Alias
    [::Foo]
    [::Foo nil "foo"]

    ;; Fns
    (example-elem-fn "123")
    [:div (example-elem-fn "123")]
    
    ;; Macros
    (example-elem-macro "123")
    (example-elem-macro-nested "123")
    [:div [:p "foo"] (example-elem-macro "123") [:p "bar"]]
    [:div [:p "foo"] (example-elem-macro-nested "123") [:p "bar"]]

    ;; Var consts
    [:div {:foo example-constant}]
    [c/doctype-html5 [:div "foo" c/nbsp "bar"]]))

(def ^:dynamic *example-dynamic*
  "foobar")

(defn example-fn-dynamic []
  (cc/compile [:div nil *example-dynamic*]))

(deftest test-compile-dynamic
  (is (= (c/html (example-fn-dynamic)) "<div>foobar</div>"))
  (binding [*example-dynamic* "foobarbaz"]
    (is (= (c/html (example-fn-dynamic)) "<div>foobarbaz</div>"))))

(def ^:redef example-redef
  "foobar")

(defn example-fn-redef []
  (cc/compile [:div nil example-redef]))

(deftest test-compile-redef
  (is (= (c/html (example-fn-redef)) "<div>foobar</div>"))
  (with-redefs [example-redef "foobarbaz"]
    (is (= (c/html (example-fn-redef)) "<div>foobarbaz</div>"))))

;; Attributes reflection tests
;; Warnings are emitted at compile time,
;; so warning detection is a side effect?

(def ambig-attrs-warnings
  (atom {}))

(defonce track-ambig-attrs-warnings
  (fn [{::cc/keys [warning form elem] :as msg}]
    (when (identical? warning :warn-on-ambig-attrs)
      (swap! ambig-attrs-warnings assoc elem msg))))

(add-tap track-ambig-attrs-warnings)

(do
  ;; Compile attrs reflection examples

  ;; Tagged literals
  (cc/compile [:div #inst "2007-01-04"])
  (cc/compile [:div #uuid "00000000-0000-0000-0000-000000000000"])

  ;; java.util.Map
  (let [attrs nil]
    (cc/compile [:div ^java.util.Map attrs "foobar"]))
  (let [^java.util.Map attrs nil]
    (cc/compile [:div attrs "foobar"]))
  (cc/compile [:div ^java.util.Map (:foo {:foo {}}) "foobar"])
  (defmethod c/resolve-alias ::ReflectiveAttrsAliasMap
    [_ ^java.util.Map attrs content]
    (cc/compile [:div attrs content]))

  ;; IPersistentMap
  (let [attrs nil]
    (cc/compile [:div ^clojure.lang.IPersistentMap attrs "foobar"]))
  (let [^clojure.lang.IPersistentMap attrs nil]
    (cc/compile [:div attrs "foobar"]))
  (cc/compile [:div ^clojure.lang.IPersistentMap (:foo {:foo {}}) "foobar"])
  (defmethod c/resolve-alias ::ReflectiveAttrsAliasIPersistentMap
    [_ ^clojure.lang.IPersistentMap attrs content]
    (cc/compile [:div attrs content]))

  ;; PersistentArrayMap
  (let [attrs nil]
    (cc/compile [:div ^clojure.lang.PersistentArrayMap attrs "foobar"]))
  (let [^clojure.lang.PersistentArrayMap attrs nil]
    (cc/compile [:div attrs "foobar"]))
  (cc/compile [:div ^clojure.lang.PersistentArrayMap (:foo {:foo {}}) "foobar"])
  (defmethod c/resolve-alias ::ReflectiveAttrsAliasPersistentArrayMap
    [_ ^clojure.lang.PersistentArrayMap attrs content]
    (cc/compile [:div attrs content]))
  
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
  
  ;; In thread macro
  (cc/compile [:div (-> {}
                        (assoc :foo "bar"))
               "foobar"])
  (cc/compile [:div (-> {}
                        ^java.util.Map (identity))
               "foobar"])
  
  ;; LocalBinded attrs literals
  (let [attrs nil]
    (cc/compile [:div attrs "foobar"]))
  (let [attrs {}]
    (cc/compile [:div attrs "foobar"]))
  (let [attrs {:foo "bar"}]
    (cc/compile [:div attrs "foobar"]))
  (let [attrs {:foo (identity "bar")}]
    (cc/compile [:div attrs "foobar"]))
  )

(remove-tap track-ambig-attrs-warnings)

(deftest test-compile-attrs-reflection
  (doseq [[elem warning] @ambig-attrs-warnings]
    (is false (str "Ambig attrs with elem: " elem))))

(deftest test-compile-shadow-attrs-core-fns
  (let [assoc (fn [& _] "assoc-shadowed")
        merge (fn [& _] "merge-shadowed")]
    (is (= (c/html (cc/compile [:div (assoc {} :foo :bar)]))
           "<div>assoc-shadowed</div>"))
    (is (= (c/html (cc/compile [:div (merge {} {:foo :bar})]))
           "<div>merge-shadowed</div>"))))

;; Alias

(defmethod c/resolve-alias ::TestAliasContent
  [_ _ content]
  (is (vector? content))
  (is (::c/content (meta content)))
  [:p content])

(deftest test-alias-content
  (are [node] (c/html (cc/compile node))
    [::TestAliasContent]
    [::TestAliasContent 0]
    [::TestAliasContent [:span "foobar"]]))
