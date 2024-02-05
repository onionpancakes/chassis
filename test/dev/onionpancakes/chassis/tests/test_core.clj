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
                              :color :red )}]      "<div style=\"border: 1px solid black; color: red;\"></div>"
    ))

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
