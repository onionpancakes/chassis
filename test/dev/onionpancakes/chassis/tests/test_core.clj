(ns dev.onionpancakes.chassis.tests.test-core
  (:require [dev.onionpancakes.chassis.core :as c]
            [clojure.test :refer [deftest are is]]))

(deftest test-html
  (are [node s] (= (c/html node) s)
    nil   ""
    ""    ""
    "foo" "foo"
    0     "0"

    ()                     ""
    (list "")              ""
    (list "foo")           "foo"
    (list "foo" "bar")     "foobar"
    (list "foo" "bar" 123) "foobar123"
    (list :foo)            ":foo"
    (list :foo "bar")      ":foobar"
    
    []        ""
    [:div]    "<div></div>"
    [:div {}] "<div></div>"
    
    [:div {:id nil}]   "<div></div>"
    [:div {:id ""}]    "<div id=\"\"></div>"
    [:div {:id "foo"}] "<div id=\"foo\"></div>"
    [:div {:id 0}]     "<div id=\"0\"></div>"

    [:div {:class nil}]   "<div></div>"
    [:div {:class ""}]    "<div class=\"\"></div>"
    [:div {:class "foo"}] "<div class=\"foo\"></div>"
    [:div {:class 0}]     "<div class=\"0\"></div>"
    
    [:div {:id nil :class nil}]     "<div></div>"
    [:div {:id "foo" :class nil}]   "<div id=\"foo\"></div>"
    [:div {:id nil :class "foo"}]   "<div class=\"foo\"></div>"
    [:div {:id "foo" :class "foo"}] "<div id=\"foo\" class=\"foo\"></div>"

    [:div {:foo "bar"}]                        "<div foo=\"bar\"></div>"
    [:div {:id "foo" :foo "bar"}]              "<div id=\"foo\" foo=\"bar\"></div>"
    [:div {:class "foo" :foo "bar"}]           "<div class=\"foo\" foo=\"bar\"></div>"
    [:div {:id "foo" :class "foo" :foo "bar"}] "<div id=\"foo\" class=\"foo\" foo=\"bar\"></div>"

    [:div nil]               "<div></div>"
    [:div ""]                "<div></div>"
    [:div "foo"]             "<div>foo</div>"
    [:div {:id "foo"} "foo"] "<div id=\"foo\">foo</div>"

    [:div [:p]]                  "<div><p></p></div>"
    [:div [:p "foo"]]            "<div><p>foo</p></div>"
    [:div [:p "foo"] [:p "bar"]] "<div><p>foo</p><p>bar</p></div>"
    ))

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
