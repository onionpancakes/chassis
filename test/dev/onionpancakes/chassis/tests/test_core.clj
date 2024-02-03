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
    
    []                 ""
    [:div]             "<div></div>"
    [:div nil]         "<div></div>"
    [:div {}]          "<div></div>"
    [:div {:id nil}]   "<div></div>"
    [:div {:id ""}]    "<div id=\"\"></div>"
    [:div {:id "foo"}] "<div id=\"foo\"></div>"
    [:div {:id 0}]     "<div id=\"0\"></div>"))
