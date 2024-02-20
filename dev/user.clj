(ns user
  (:require [bench.chassis
             :refer [page
                     chassis-page
                     chassis-page-alias chassis-page-alias-compiled
                     chassis-page-alias-compiled-unambig
                     chassis-page-compiled chassis-page-compiled-unambig
                     chassis-page-print-writer chassis-page-print-stream
                     chassis-page-output-stream-writer]]
            [bench.hiccup
             :refer [hiccup-page hiccup-page-compiled
                     hiccup-page-compiled-unambig]]
            [bench.selmer :refer [selmer-page]]
            [bench.enlive
             :refer [enlive-page-item-html enlive-page-item-template]]
            [dev.onionpancakes.chassis.core :as c]
            [dev.onionpancakes.chassis.compiler :as cc]
            [dev.onionpancakes.chassis.tests.test-core :as t]
            [dev.onionpancakes.chassis.tests.test-compiler :as tc]
            [criterium.core :refer [bench quick-bench]]
            [hiccup2.core :as hiccup]
            [clojure.walk :refer [macroexpand-all]]))

(defmethod c/resolve-alias ::Foo
  [_ _ attrs content]
  [:div.foo attrs content])

(defmethod c/resolve-alias ::Fooc
  [_ _ ^java.util.Map attrs content]
  (cc/compile
   [:div.fooc attrs content]))

(defmethod c/resolve-alias ::Layoutc
  [_ _ attrs content]
  (cc/compile
   [:html
    [:head
     [:link {:href "/foobar" :rel "stylesheet"}]]
    [:body
     [:main.layout ^java.util.Map attrs content]]]))

(def lorem
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defn make-data
  [n]
  {:title "Test Items"
   :items (vec (for [i (range n)]
                 {:idx  i
                  :type "foo"
                  :uuid (random-uuid)
                  :name (str "Item: " i)
                  :date (java.time.LocalDate/of 2000 10 5)
                  :text lorem}))})

(def data-big
  (make-data 160))

(def data-mid
  (make-data 40))

(def data-small
  (make-data 10))

(defn quick-bench-all
  [data]
  (println "Chassis")
  (println "-------------------------------------")
  (quick-bench (chassis-page data))
  (println)
  (println "Chassis alias")
  (println "-------------------------------------")
  (quick-bench (chassis-page-alias data))
  (println)
  (println "Hiccup")
  (println "-------------------------------------")
  (quick-bench (hiccup-page data))
  (println)
  (println "Hiccup runtime")
  (println "-------------------------------------")
  (quick-bench (hiccup-page-runtime data))
  (println)
  (println "Selmer")
  (println "-------------------------------------")
  (quick-bench (selmer-page data))
  (println)
  (println "Enlive item html")
  (println "-------------------------------------")
  (quick-bench (enlive-page-item-html data))
  (println)
  (println "Enlive item template")
  (println "-------------------------------------")
  (quick-bench (enlive-page-item-template data)))

(defn bench-all
  [data]
  (println "Chassis")
  (println "-------------------------------------")
  (bench (chassis-page data))
  (println)
  (println "Chassis alias")
  (println "-------------------------------------")
  (bench (chassis-page-alias data))
  (println)
  (println "Hiccup")
  (println "-------------------------------------")
  (bench (hiccup-page data))
  (println)
  (println "Hiccup runtime")
  (println "-------------------------------------")
  (bench (hiccup-page-runtime data))
  (println)
  (println "Selmer")
  (println "-------------------------------------")
  (bench (selmer-page data))
  (println)
  (println "Enlive item html")
  (println "-------------------------------------")
  (bench (enlive-page-item-html data))
  (println)
  (println "Enlive item template")
  (println "-------------------------------------")
  (bench (enlive-page-item-template data)))

(defn bench-gen-results
  ([]
   (with-open [wtr (clojure.java.io/writer "resources/bench/results_big.txt")]
     (binding [*out* wtr]
       (bench-all data-big)))
   (with-open [wtr (clojure.java.io/writer "resources/bench/results_mid.txt")]
     (binding [*out* wtr]
       (bench-all data-mid)))
   (with-open [wtr (clojure.java.io/writer "resources/bench/results_small.txt")]
     (binding [*out* wtr]
       (bench-all data-small))))
  ([_] (bench-gen-results)))
