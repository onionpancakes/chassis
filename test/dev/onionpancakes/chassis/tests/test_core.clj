(ns dev.onionpancakes.chassis.tests.test-core
  (:require [dev.onionpancakes.chassis.core :as c]
            [clojure.test :refer [deftest are is]]))

(deftest test-html
  (are [node s] (= (c/html node) s)
    ;; Primitives, non-elements
    nil   ""
    ""    ""
    "foo" "foo"
    0     "0"

    ;; List non-elements
    ()                     ""
    (list "")              ""
    (list "foo")           "foo"
    (list "foo" "bar")     "foobar"
    (list "foo" "bar" 123) "foobar123"
    (list :foo)            ":foo"
    (list :foo "bar")      ":foobar"

    ;; Vector non-elements
    []                ""
    ["foo"]           "foo"
    ["foo" "bar"]     "foobar"
    ["foo" "bar" 123] "foobar123"
    [0]               "0"
    [:foo/foo]        ":foo/foo"
    [:foo/foo 123]    ":foo/foo123"

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
       "foo"]]]                  "<div id=\"0\"><p id=\"1\"><span id=\"2\">foo</span></p></div>"))

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

(deftest test-html-doctype-html5
  (are [node s] (= (c/html node) s)
    c/doctype-html5   "<!DOCTYPE html>"
    [c/doctype-html5] "<!DOCTYPE html>"
    [c/doctype-html5
     [:html]]         "<!DOCTYPE html><html></html>"
    [c/doctype-html5
     [:html
      [:body "foo"]]] "<!DOCTYPE html><html><body>foo</body></html>"))
