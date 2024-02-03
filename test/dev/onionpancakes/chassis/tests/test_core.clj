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
    
    []         ""
    [:div]     "<div></div>"
    [:div nil] "<div></div>"
    [:div {}]  "<div></div>"
    
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

    [:div {:foo "bar"}] "<div foo=\"bar\"></div>"))

(deftest test-html-void
  (are [node s] (= (c/html node) s)
    [:br]     "<br>"
    [:hr]     "<hr>"
    [:hr nil] "<hr>"

    [:hr {:id "foo"}]  "<hr id=\"foo\">"
    [:hr {:foo "bar"}] "<hr foo=\"bar\">"

    [:hr nil nil]         "<hr></hr>"
    [:hr {:id "foo"} nil] "<hr id=\"foo\"></hr>"))
