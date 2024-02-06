(ns dev.onionpancakes.chassis.tests.test-core
  (:require [dev.onionpancakes.chassis.core :as c]
            [clojure.test :refer [deftest are is]]))

(deftest test-html
  (are [node s] (= (c/html node) s)
    ;; Primitives, non-elements
    nil      ""
    ""       ""
    "foo"    "foo"
    0        "0"
    :foo     "foo"
    :foo/bar "foo/bar"

    ;; List non-elements
    ()                     ""
    (list "")              ""
    (list "foo")           "foo"
    (list "foo" "bar")     "foobar"
    (list "foo" "bar" 123) "foobar123"
    (list :foo)            "foo"
    (list :foo "bar")      "foobar"

    ;; Vector non-elements
    []                ""
    ["foo"]           "foo"
    ["foo" "bar"]     "foobar"
    ["foo" "bar" 123] "foobar123"
    [0]               "0"

    ;; Element, no attr
    [:div]    "<div></div>"
    [:div {}] "<div></div>"

    ;; Element with id
    [:div {:id nil}]   "<div></div>"
    [:div {:id ""}]    "<div id=\"\"></div>"
    [:div {:id "foo"}] "<div id=\"foo\"></div>"
    [:div {:id 0}]     "<div id=\"0\"></div>"

    ;; Element with class
    [:div {:class nil}]   "<div></div>"
    [:div {:class ""}]    "<div class=\"\"></div>"
    [:div {:class "foo"}] "<div class=\"foo\"></div>"
    [:div {:class 0}]     "<div class=\"0\"></div>"

    ;; Element with id, class
    [:div {:id nil :class nil}]     "<div></div>"
    [:div {:id "foo" :class nil}]   "<div id=\"foo\"></div>"
    [:div {:id nil :class "foo"}]   "<div class=\"foo\"></div>"
    [:div {:id "foo" :class "foo"}] "<div id=\"foo\" class=\"foo\"></div>"

    ;; Element with id, class, attr
    [:div {:foo "bar"}]                        "<div foo=\"bar\"></div>"
    [:div {:id "foo" :foo "bar"}]              "<div id=\"foo\" foo=\"bar\"></div>"
    [:div {:class "foo" :foo "bar"}]           "<div class=\"foo\" foo=\"bar\"></div>"
    [:div {:id "foo" :class "foo" :foo "bar"}] "<div id=\"foo\" class=\"foo\" foo=\"bar\"></div>"

    ;; Element body
    [:div nil]               "<div></div>"
    [:div ""]                "<div></div>"
    [:div "foo"]             "<div>foo</div>"
    [:div {:id "foo"} "foo"] "<div id=\"foo\">foo</div>"

    ;; Elements nested
    [:div [:p]]                  "<div><p></p></div>"
    [:div [:p "foo"]]            "<div><p>foo</p></div>"
    [:div [:p "foo"] [:p "bar"]] "<div><p>foo</p><p>bar</p></div>"
    [:div {:id 0}
     [:p {:id 1} "foo"]
     [:p {:id 2} "bar"]]         "<div id=\"0\"><p id=\"1\">foo</p><p id=\"2\">bar</p></div>"
    [:div {:id 0}
     [:p {:id 1}
      [:span {:id 2}
       "foo"]]]                  "<div id=\"0\"><p id=\"1\"><span id=\"2\">foo</span></p></div>"

    ;; Attribute values
    [:div {:foo nil}]      "<div></div>"
    [:div {:foo 0}]        "<div foo=\"0\"></div>"
    [:div {:foo :bar}]     "<div foo=\"bar\"></div>"
    [:div {:foo :foo/bar}] "<div foo=\"foo/bar\"></div>"
    [:div {:foo true}]     "<div foo></div>"
    [:div {:foo false}]    "<div></div>"

    ;; Attribute vector value
    [:div {:foo []}]                      "<div foo=\"\"></div>"
    [:div {:foo [nil]}]                   "<div foo=\"\"></div>"
    [:div {:foo [true]}]                  "<div foo=\"\"></div>"
    [:div {:foo [false]}]                 "<div foo=\"\"></div>"
    [:div {:foo [:a]}]                    "<div foo=\"a\"></div>"
    [:div {:foo [:a/b]}]                  "<div foo=\"a/b\"></div>"
    [:div {:foo ["a"]}]                   "<div foo=\"a\"></div>"
    [:div {:foo ["a" "b"]}]               "<div foo=\"a b\"></div>"
    [:div {:foo ["a" "b" nil 1 2 3]}]     "<div foo=\"a b 1 2 3\"></div>"
    [:div {:foo [["a"] #{"b"} {:c "d"}]}] "<div foo=\"a b c: d;\"></div>"

    ;; Attribute set value
    [:div {:foo #{}}]        "<div foo=\"\"></div>"
    [:div {:foo #{nil}}]     "<div foo=\"\"></div>"
    [:div {:foo #{true}}]    "<div foo=\"\"></div>"
    [:div {:foo #{false}}]   "<div foo=\"\"></div>"
    [:div {:foo #{:a}}]      "<div foo=\"a\"></div>"
    [:div {:foo #{0}}]       "<div foo=\"0\"></div>"
    [:div {:foo #{[1 2 3]}}] "<div foo=\"1 2 3\"></div>"

    ;; Attribute map value
    [:div {:style {}}]                             "<div style=\"\"></div>"
    [:div {:style {:color nil}}]                   "<div style=\"\"></div>"
    [:div {:style {:color "red"}}]                 "<div style=\"color: red;\"></div>"
    [:div {:style {:color :red}}]                  "<div style=\"color: red;\"></div>"
    [:div {:style {:border [:1px :solid :black]}}] "<div style=\"border: 1px solid black;\"></div>"
    [:div {:style (sorted-map :border [:1px :solid :black]
                              :color :red)}]       "<div style=\"border: 1px solid black; color: red;\"></div>"))

(deftest test-html-tag-id-class
  (are [node s] (= (c/html node) s)
    ;; id
    [:div#foo] "<div id=\"foo\"></div>"

    ;; class
    [:div.a]     "<div class=\"a\"></div>"
    [:div.a.b]   "<div class=\"a b\"></div>"
    [:div.a.b.c] "<div class=\"a b c\"></div>"

    ;; id, class
    [:div#foo.a]     "<div id=\"foo\" class=\"a\"></div>"
    [:div#foo.a.b]   "<div id=\"foo\" class=\"a b\"></div>"
    [:div#foo.a.b.c] "<div id=\"foo\" class=\"a b c\"></div>"

    ;; class, id
    [:div.a#foo]     "<div id=\"foo\" class=\"a\"></div>"
    [:div.a.b#foo]   "<div id=\"foo\" class=\"a b\"></div>"
    [:div.a.b.c#foo] "<div id=\"foo\" class=\"a b c\"></div>"

    ;; class, id, class
    [:div.a.b.c#foo.d.e.f] "<div id=\"foo\" class=\"a b c d e f\"></div>"

    ;; class, id, class, pound
    [:div.a.b.c#foo.d.e.f#bar.baz] "<div id=\"foo\" class=\"a b c d e f#bar baz\"></div>"))

(deftest test-html-tag-id-class-attrs-merge
  (are [node s] (= (c/html node) s)
    ;; No tag id class
    [:div]                                       "<div></div>"
    [:div nil]                                   "<div></div>"
    ;;
    [:div {:id nil}]                             "<div></div>"
    [:div {:id "bar"}]                           "<div id=\"bar\"></div>"
    [:div {:class nil}]                          "<div></div>"
    [:div {:class "3 4 5"}]                      "<div class=\"3 4 5\"></div>"
    [:div {:foo nil}]                            "<div></div>"
    [:div {:foo "abc"}]                          "<div foo=\"abc\"></div>"
    ;;
    [:div {:id nil :class nil}]                  "<div></div>"
    [:div {:id nil :class "3 4 5"}]              "<div class=\"3 4 5\"></div>"
    [:div {:id nil :foo nil}]                    "<div></div>"
    [:div {:id nil :foo "abc"}]                  "<div foo=\"abc\"></div>"
    [:div {:id "foo" :class nil}]                "<div id=\"foo\"></div>"
    [:div {:id "foo" :class "3 4 5"}]            "<div id=\"foo\" class=\"3 4 5\"></div>"
    [:div {:id "foo" :foo nil}]                  "<div id=\"foo\"></div>"
    [:div {:id "foo" :foo "abc"}]                "<div id=\"foo\" foo=\"abc\"></div>"
    [:div {:class nil :foo nil}]                 "<div></div>"
    [:div {:class nil :foo "abc"}]               "<div foo=\"abc\"></div>"
    [:div {:class "3 4 5" :foo nil}]             "<div class=\"3 4 5\"></div>"
    [:div {:class "3 4 5" :foo "abc"}]           "<div class=\"3 4 5\" foo=\"abc\"></div>"
    ;;
    [:div {:id nil :class nil :foo nil}]         "<div></div>"
    [:div {:id nil :class nil :foo "abc"}]       "<div foo=\"abc\"></div>"
    [:div {:id nil :class "3 4 5" :foo nil}]     "<div class=\"3 4 5\"></div>"
    [:div {:id nil :class "3 4 5" :foo "abc"}]   "<div class=\"3 4 5\" foo=\"abc\"></div>"
    [:div {:id "bar" :class nil :foo nil}]       "<div id=\"bar\"></div>"
    [:div {:id "bar" :class nil :foo "abc"}]     "<div id=\"bar\" foo=\"abc\"></div>"
    [:div {:id "bar" :class "3 4 5" :foo nil}]   "<div id=\"bar\" class=\"3 4 5\"></div>"
    [:div {:id "bar" :class "3 4 5" :foo "abc"}] "<div id=\"bar\" class=\"3 4 5\" foo=\"abc\"></div>"

    ;; With tag id
    [:div#foo]                                       "<div id=\"foo\"></div>"
    [:div#foo nil]                                   "<div id=\"foo\"></div>"
    ;;
    [:div#foo {:id nil}]                             "<div id=\"foo\"></div>"
    [:div#foo {:id "bar"}]                           "<div id=\"foo\"></div>"
    [:div#foo {:class nil}]                          "<div id=\"foo\"></div>"
    [:div#foo {:class "3 4 5"}]                      "<div id=\"foo\" class=\"3 4 5\"></div>"
    [:div#foo {:foo nil}]                            "<div id=\"foo\"></div>"
    [:div#foo {:foo "abc"}]                          "<div id=\"foo\" foo=\"abc\"></div>"
    ;;
    [:div#foo {:id nil :class nil}]                  "<div id=\"foo\"></div>"
    [:div#foo {:id nil :class "3 4 5"}]              "<div id=\"foo\" class=\"3 4 5\"></div>"
    [:div#foo {:id nil :foo nil}]                    "<div id=\"foo\"></div>"
    [:div#foo {:id nil :foo "abc"}]                  "<div id=\"foo\" foo=\"abc\"></div>"
    [:div#foo {:id "foo" :class nil}]                "<div id=\"foo\"></div>"
    [:div#foo {:id "foo" :class "3 4 5"}]            "<div id=\"foo\" class=\"3 4 5\"></div>"
    [:div#foo {:id "foo" :foo nil}]                  "<div id=\"foo\"></div>"
    [:div#foo {:id "foo" :foo "abc"}]                "<div id=\"foo\" foo=\"abc\"></div>"
    [:div#foo {:class nil :foo nil}]                 "<div id=\"foo\"></div>"
    [:div#foo {:class nil :foo "abc"}]               "<div id=\"foo\" foo=\"abc\"></div>"
    [:div#foo {:class "3 4 5" :foo nil}]             "<div id=\"foo\" class=\"3 4 5\"></div>"
    [:div#foo {:class "3 4 5" :foo "abc"}]           "<div id=\"foo\" class=\"3 4 5\" foo=\"abc\"></div>"
    ;;
    [:div#foo {:id nil :class nil :foo nil}]         "<div id=\"foo\"></div>"
    [:div#foo {:id nil :class nil :foo "abc"}]       "<div id=\"foo\" foo=\"abc\"></div>"
    [:div#foo {:id nil :class "3 4 5" :foo nil}]     "<div id=\"foo\" class=\"3 4 5\"></div>"
    [:div#foo {:id nil :class "3 4 5" :foo "abc"}]   "<div id=\"foo\" class=\"3 4 5\" foo=\"abc\"></div>"
    [:div#foo {:id "bar" :class nil :foo nil}]       "<div id=\"foo\"></div>"
    [:div#foo {:id "bar" :class nil :foo "abc"}]     "<div id=\"foo\" foo=\"abc\"></div>"
    [:div#foo {:id "bar" :class "3 4 5" :foo nil}]   "<div id=\"foo\" class=\"3 4 5\"></div>"
    [:div#foo {:id "bar" :class "3 4 5" :foo "abc"}] "<div id=\"foo\" class=\"3 4 5\" foo=\"abc\"></div>"
    
    ;; With tag class
    [:div.0.1.2]                                       "<div class=\"0 1 2\"></div>"
    [:div.0.1.2 nil]                                   "<div class=\"0 1 2\"></div>"
    ;;
    [:div.0.1.2 {:id nil}]                             "<div class=\"0 1 2\"></div>"
    [:div.0.1.2 {:id "foo"}]                           "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div.0.1.2 {:class nil}]                          "<div class=\"0 1 2\"></div>"
    [:div.0.1.2 {:class "3 4 5"}]                      "<div class=\"0 1 2 3 4 5\"></div>"
    [:div.0.1.2 {:foo nil}]                            "<div class=\"0 1 2\"></div>"
    [:div.0.1.2 {:foo "abc"}]                          "<div class=\"0 1 2\" foo=\"abc\"></div>"
    ;;
    [:div.0.1.2 {:id nil :class nil}]                  "<div class=\"0 1 2\"></div>"
    [:div.0.1.2 {:id nil :class "3 4 5"}]              "<div class=\"0 1 2 3 4 5\"></div>"
    [:div.0.1.2 {:id nil :foo nil}]                    "<div class=\"0 1 2\"></div>"
    [:div.0.1.2 {:id nil :foo "abc"}]                  "<div class=\"0 1 2\" foo=\"abc\"></div>"
    [:div.0.1.2 {:id "bar" :class nil}]                "<div id=\"bar\" class=\"0 1 2\"></div>"
    [:div.0.1.2 {:id "bar" :class "3 4 5"}]            "<div id=\"bar\" class=\"0 1 2 3 4 5\"></div>"
    [:div.0.1.2 {:id "bar" :foo nil}]                  "<div id=\"bar\" class=\"0 1 2\"></div>"
    [:div.0.1.2 {:id "bar" :foo "abc"}]                "<div id=\"bar\" class=\"0 1 2\" foo=\"abc\"></div>"
    [:div.0.1.2 {:class nil :foo nil}]                 "<div class=\"0 1 2\"></div>"
    [:div.0.1.2 {:class nil :foo "abc"}]               "<div class=\"0 1 2\" foo=\"abc\"></div>"
    [:div.0.1.2 {:class "3 4 5" :foo nil}]             "<div class=\"0 1 2 3 4 5\"></div>"
    [:div.0.1.2 {:class "3 4 5" :foo "abc"}]           "<div class=\"0 1 2 3 4 5\" foo=\"abc\"></div>"
    ;;
    [:div.0.1.2 {:id nil :class nil :foo nil}]         "<div class=\"0 1 2\"></div>"
    [:div.0.1.2 {:id nil :class nil :foo "abc"}]       "<div class=\"0 1 2\" foo=\"abc\"></div>"
    [:div.0.1.2 {:id nil :class "3 4 5" :foo nil}]     "<div class=\"0 1 2 3 4 5\"></div>"
    [:div.0.1.2 {:id nil :class "3 4 5" :foo "abc"}]   "<div class=\"0 1 2 3 4 5\" foo=\"abc\"></div>"
    [:div.0.1.2 {:id "bar" :class nil :foo nil}]       "<div id=\"bar\" class=\"0 1 2\"></div>"
    [:div.0.1.2 {:id "bar" :class nil :foo "abc"}]     "<div id=\"bar\" class=\"0 1 2\" foo=\"abc\"></div>"
    [:div.0.1.2 {:id "bar" :class "3 4 5" :foo nil}]   "<div id=\"bar\" class=\"0 1 2 3 4 5\"></div>"
    [:div.0.1.2 {:id "bar" :class "3 4 5" :foo "abc"}] "<div id=\"bar\" class=\"0 1 2 3 4 5\" foo=\"abc\"></div>"

    ;; With tag id class
    [:div#foo.0.1.2]                                       "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 nil]                                   "<div id=\"foo\" class=\"0 1 2\"></div>"
    ;;
    [:div#foo.0.1.2 {:id nil}]                             "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:id "foo"}]                           "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:class nil}]                          "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:class "3 4 5"}]                      "<div id=\"foo\" class=\"0 1 2 3 4 5\"></div>"
    [:div#foo.0.1.2 {:foo nil}]                            "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:foo "abc"}]                          "<div id=\"foo\" class=\"0 1 2\" foo=\"abc\"></div>"
    ;;
    [:div#foo.0.1.2 {:id nil :class nil}]                  "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:id nil :class "3 4 5"}]              "<div id=\"foo\" class=\"0 1 2 3 4 5\"></div>"
    [:div#foo.0.1.2 {:id nil :foo nil}]                    "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:id nil :foo "abc"}]                  "<div id=\"foo\" class=\"0 1 2\" foo=\"abc\"></div>"
    [:div#foo.0.1.2 {:id "bar" :class nil}]                "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:id "bar" :class "3 4 5"}]            "<div id=\"foo\" class=\"0 1 2 3 4 5\"></div>"
    [:div#foo.0.1.2 {:id "bar" :foo nil}]                  "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:id "bar" :foo "abc"}]                "<div id=\"foo\" class=\"0 1 2\" foo=\"abc\"></div>"
    [:div#foo.0.1.2 {:class nil :foo nil}]                 "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:class nil :foo "abc"}]               "<div id=\"foo\" class=\"0 1 2\" foo=\"abc\"></div>"
    [:div#foo.0.1.2 {:class "3 4 5" :foo nil}]             "<div id=\"foo\" class=\"0 1 2 3 4 5\"></div>"
    [:div#foo.0.1.2 {:class "3 4 5" :foo "abc"}]           "<div id=\"foo\" class=\"0 1 2 3 4 5\" foo=\"abc\"></div>"
    ;;
    [:div#foo.0.1.2 {:id nil :class nil :foo nil}]         "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:id nil :class nil :foo "abc"}]       "<div id=\"foo\" class=\"0 1 2\" foo=\"abc\"></div>"
    [:div#foo.0.1.2 {:id nil :class "3 4 5" :foo nil}]     "<div id=\"foo\" class=\"0 1 2 3 4 5\"></div>"
    [:div#foo.0.1.2 {:id nil :class "3 4 5" :foo "abc"}]   "<div id=\"foo\" class=\"0 1 2 3 4 5\" foo=\"abc\"></div>"
    [:div#foo.0.1.2 {:id "bar" :class nil :foo nil}]       "<div id=\"foo\" class=\"0 1 2\"></div>"
    [:div#foo.0.1.2 {:id "bar" :class nil :foo "abc"}]     "<div id=\"foo\" class=\"0 1 2\" foo=\"abc\"></div>"
    [:div#foo.0.1.2 {:id "bar" :class "3 4 5" :foo nil}]   "<div id=\"foo\" class=\"0 1 2 3 4 5\"></div>"
    [:div#foo.0.1.2 {:id "bar" :class "3 4 5" :foo "abc"}] "<div id=\"foo\" class=\"0 1 2 3 4 5\" foo=\"abc\"></div>"))

(deftest test-html-attributes
  (are [node s] (= (c/html node) s)
    ;; id and class behave differently around boolean attributes.
    [:div {:id nil}]                   "<div></div>"
    [:div {:id ""}]                    "<div id=\"\"></div>"
    [:div {:id "bar"}]                 "<div id=\"bar\"></div>"
    [:div {:id :bar}]                  "<div id=\"bar\"></div>"
    [:div {:id :foo/bar}]              "<div id=\"foo/bar\"></div>"
    [:div {:id 0}]                     "<div id=\"0\"></div>"
    [:div {:id 0.0}]                   "<div id=\"0.0\"></div>"
    [:div {:id (java.util.UUID. 0 0)}] "<div id=\"00000000-0000-0000-0000-000000000000\"></div>"
    [:div {:id true}]                  "<div id=\"\"></div>"
    [:div {:id false}]                 "<div></div>"

    [:div {:class nil}]                   "<div></div>"
    [:div {:class ""}]                    "<div class=\"\"></div>"
    [:div {:class "bar"}]                 "<div class=\"bar\"></div>"
    [:div {:class :bar}]                  "<div class=\"bar\"></div>"
    [:div {:class :foo/bar}]              "<div class=\"foo/bar\"></div>"
    [:div {:class 0}]                     "<div class=\"0\"></div>"
    [:div {:class 0.0}]                   "<div class=\"0.0\"></div>"
    [:div {:class (java.util.UUID. 0 0)}] "<div class=\"00000000-0000-0000-0000-000000000000\"></div>"
    [:div {:class true}]                  "<div class=\"\"></div>"
    [:div {:class false}]                 "<div></div>"

    [:div {:foo nil}]                   "<div></div>"
    [:div {:foo ""}]                    "<div foo=\"\"></div>"
    [:div {:foo "bar"}]                 "<div foo=\"bar\"></div>"
    [:div {:foo :bar}]                  "<div foo=\"bar\"></div>"
    [:div {:foo :foo/bar}]              "<div foo=\"foo/bar\"></div>"
    [:div {:foo 0}]                     "<div foo=\"0\"></div>"
    [:div {:foo 0.0}]                   "<div foo=\"0.0\"></div>"
    [:div {:foo (java.util.UUID. 0 0)}] "<div foo=\"00000000-0000-0000-0000-000000000000\"></div>"
    [:div {:foo true}]                  "<div foo></div>"
    [:div {:foo false}]                 "<div></div>"

    ;; Valid attribute keys.
    [:div {:foo-bar "foo"}] "<div foo-bar=\"foo\"></div>"
    [:div {:& "foo"}]       "<div &=\"foo\"></div>"
    [:div {:💀 "foo"}]      "<div 💀=\"foo\"></div>"
    [:div {"foo" "bar"}]    "<div foo=\"bar\"></div>"
    [:div {'foo "bar"}]     "<div foo=\"bar\"></div>"

    ;; Non-attribute keys.
    [:div {::foo "foo"}]            "<div></div>"
    [:div {'foo/bar "foo"}]         "<div></div>"
    [:div {nil "foo"}]              "<div></div>"
    [:div {0 "foo"}]                "<div></div>"
    [:div#foo {::foo "foo"}]        "<div id=\"foo\"></div>"
    [:div#foo {'foo/bar "foo"}]     "<div id=\"foo\"></div>"
    [:div#foo {nil "foo"}]          "<div id=\"foo\"></div>"
    [:div#foo {0 "foo"}]            "<div id=\"foo\"></div>"
    [:div.bar {::foo "foo"}]        "<div class=\"bar\"></div>"
    [:div.bar {'foo/bar "foo"}]     "<div class=\"bar\"></div>"
    [:div.bar {nil "foo"}]          "<div class=\"bar\"></div>"
    [:div.bar {0 "foo"}]            "<div class=\"bar\"></div>"
    [:div#foo.bar {::foo "foo"}]    "<div id=\"foo\" class=\"bar\"></div>"
    [:div#foo.bar {'foo/bar "foo"}] "<div id=\"foo\" class=\"bar\"></div>"
    [:div#foo.bar {nil "foo"}]      "<div id=\"foo\" class=\"bar\"></div>"
    [:div#foo.bar {0 "foo"}]        "<div id=\"foo\" class=\"bar\"></div>"))

(deftest test-html-tokens
  (are [node s] (= (c/html node) s)
    nil                     ""
    ""                      ""
    "foo"                   "foo"
    :foo                    "foo"
    :foo/bar                "foo/bar"
    0                       "0"
    0.0                     "0.0"
    (java.util.UUID. 0 0)   "00000000-0000-0000-0000-000000000000"
    (reify Object
      (toString [_] "foo")) "foo"))

(deftest test-html-nodes
  (are [node s] (= (c/html node) s)
    [:div]                  "<div></div>"
    '(:div)                 "div"
    (seq [:div])            "div"
    (delay :div)            "div"
    (fn [] :div)            "div"
    (reify Object
      (toString [_] "div")) "div"
    nil                     ""))

(defmethod c/resolve-alias ::Foo
  [tag attrs content]
  [:div.foo attrs content])

(defmethod c/resolve-alias ::Bar
  [tag attrs content]
  [:span.bar attrs content])

(defmethod c/resolve-alias ::Recursive
  [tag {::keys [idx] :as attrs} content]
  (let [idx (long idx)]
    (if (and idx (>= idx 0))
      [:div {:id idx}
       [::Recursive {::idx (dec idx)}]])))

(deftest test-html-alias
  (are [node s] (= (c/html node) s)
    [::Foo]                                   "<div class=\"foo\"></div>"
    [::Foo#this]                              "<div id=\"this\" class=\"foo\"></div>"
    [::Foo#this.bar]                          "<div id=\"this\" class=\"foo bar\"></div>"
    [::Foo#this.bar {:class "baz"}]           "<div id=\"this\" class=\"foo bar baz\"></div>"
    [::Foo#this.bar {:class "baz"} "abc" 123] "<div id=\"this\" class=\"foo bar baz\">abc123</div>"

    [::Foo {:id "that"}]      "<div id=\"that\" class=\"foo\"></div>"
    [::Foo#this {:id "that"}] "<div id=\"this\" class=\"foo\"></div>"

    ;; Nested alias
    [::Foo#this.bar {:class "baz"}
     [::Bar#that.baz {:class "buz"}
      "xyz"]] "<div id=\"this\" class=\"foo bar baz\"><span id=\"that\" class=\"bar baz buz\">xyz</span></div>"

    ;; Recursive
    [::Recursive {::idx 3}] "<div id=\"3\"><div id=\"2\"><div id=\"1\"><div id=\"0\"></div></div></div></div>"))

(deftest test-html-void
  (are [node s] (= (c/html node) s)
    [:br]     "<br>"
    [:hr]     "<hr>"
    [:hr nil] "<hr>"

    [:hr {:id "foo"}]  "<hr id=\"foo\">"
    [:hr {:foo "bar"}] "<hr foo=\"bar\">"

    [:hr nil nil]           "<hr></hr>"
    [:hr {:id "foo"} nil]   "<hr id=\"foo\"></hr>"
    [:hr {:id "foo"} "foo"] "<hr id=\"foo\">foo</hr>"))

(deftest test-html-element-children
  (are [node s] (= (c/html node) s)
    [:div {}]                            "<div></div>"
    [:div {} 1]                          "<div>1</div>"
    [:div {} 1 2]                        "<div>12</div>"
    [:div {} 1 2 3]                      "<div>123</div>"
    [:div {} 1 2 3 4]                    "<div>1234</div>"
    [:div {} 1 2 3 4 5]                  "<div>12345</div>"
    [:div {} 1 2 3 4 5 6]                "<div>123456</div>"
    [:div {} 1 2 3 4 5 6 7]              "<div>1234567</div>"
    [:div {} 1 2 3 4 5 6 7 8]            "<div>12345678</div>"
    [:div {} 1 2 3 4 5 6 7 8 9]          "<div>123456789</div>"
    [:div {} 1 2 3 4 5 6 7 8 9 10]       "<div>12345678910</div>"
    [:div {} 1 2 3 4 5 6 7 8 9 10 11]    "<div>1234567891011</div>"
    [:div {} 1 2 3 4 5 6 7 8 9 10 11 12] "<div>123456789101112</div>"
    [:div]                               "<div></div>"
    [:div 1]                             "<div>1</div>"
    [:div 1 2]                           "<div>12</div>"
    [:div 1 2 3]                         "<div>123</div>"
    [:div 1 2 3 4]                       "<div>1234</div>"
    [:div 1 2 3 4 5]                     "<div>12345</div>"
    [:div 1 2 3 4 5 6]                   "<div>123456</div>"
    [:div 1 2 3 4 5 6 7]                 "<div>1234567</div>"
    [:div 1 2 3 4 5 6 7 8]               "<div>12345678</div>"
    [:div 1 2 3 4 5 6 7 8 9]             "<div>123456789</div>"
    [:div 1 2 3 4 5 6 7 8 9 10]          "<div>12345678910</div>"
    [:div 1 2 3 4 5 6 7 8 9 10 11]       "<div>1234567891011</div>"
    [:div 1 2 3 4 5 6 7 8 9 10 11 12]    "<div>123456789101112</div>"))

(deftest test-html-doctype-html5
  (are [node s] (= (c/html node) s)
    c/doctype-html5   "<!DOCTYPE html>"
    [c/doctype-html5] "<!DOCTYPE html>"
    [c/doctype-html5
     [:html]]         "<!DOCTYPE html><html></html>"
    [c/doctype-html5
     [:html
      [:body "foo"]]] "<!DOCTYPE html><html><body>foo</body></html>"))
