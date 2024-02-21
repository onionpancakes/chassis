(ns user
  (:require [bench.chassis
             :refer [page
                     page-doall
                     chassis-page
                     chassis-page-compiled
                     chassis-page-compiled-unambig
                     chassis-page-alias
                     chassis-page-alias-compiled
                     chassis-page-alias-compiled-unambig
                     chassis-page-print-writer
                     chassis-page-print-writer-compiled-unambig
                     chassis-page-print-stream
                     chassis-page-print-stream-compiled-unambig
                     chassis-page-output-stream-writer
                     chassis-page-output-stream-writer-compiled-unambig
                     chassis-page-output-stream-writer-html-string
                     chassis-page-output-stream-writer-html-string-compiled-unambig]]
            [bench.hiccup
             :refer [hiccup-page
                     hiccup-page-compiled
                     hiccup-page-compiled-unambig]]
            [bench.selmer
             :refer [selmer-page]]
            [bench.enlive
             :refer [enlive-page-item-html]]
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

(defn gen-bench-chassis
  [data]
  (let [file-name (str "resources/bench/chassis_" (count (:items data)) ".txt")]
    (with-open [wtr (clojure.java.io/writer file-name)]
      (binding [*out* wtr]
        (println "Chassis")
        (println "-------------------------------------")
        (bench (chassis-page data))
        (println)
        (println "Chassis Compiled")
        (println "-------------------------------------")
        (bench (chassis-page-compiled data))
        (println)
        (println "Chassis Compiled Unambig")
        (println "-------------------------------------")
        (bench (chassis-page-compiled-unambig data))
        (println)
        (println "Chassis Alias")
        (println "-------------------------------------")
        (bench (chassis-page-alias data))
        (println)
        (println "Chassis Alias Compiled")
        (println "-------------------------------------")
        (bench (chassis-page-alias-compiled data))
        (println)
        (println "Chassis Alias Compiled Unambig")
        (println "-------------------------------------")
        (bench (chassis-page-alias-compiled-unambig data))
        (println)))))

(defn gen-bench-chassis-write-html
  [data]
  (let [file-name (str "resources/bench/chassis_write_html_" (count (:items data)) ".txt")]
    (with-open [wtr (clojure.java.io/writer file-name)]
      (binding [*out* wtr]
        (println "Chassis PrintWriter")
        (println "-------------------------------------")
        (bench (chassis-page-print-writer data))
        (println)
        (println "Chassis PrintWriter Compiled Unambig")
        (println "-------------------------------------")
        (bench (chassis-page-print-writer-compiled-unambig data))
        (println)
        (println "Chassis PrintStream")
        (println "-------------------------------------")
        (bench (chassis-page-print-stream data))
        (println)
        (println "Chassis PrintStream Compiled Unambig")
        (println "-------------------------------------")
        (bench (chassis-page-print-stream-compiled-unambig data))
        (println)
        (println "Chassis OutputStreamWriter")
        (println "-------------------------------------")
        (bench (chassis-page-output-stream-writer data))
        (println)
        (println "Chassis OutputStreamWriter Compiled Unambig")
        (println "-------------------------------------")
        (bench (chassis-page-output-stream-writer-compiled-unambig data))
        (println)
        (println "Chassis OutputStreamWriter HTML String")
        (println "-------------------------------------")
        (bench (chassis-page-output-stream-writer-html-string data))
        (println)
        (println "Chassis OutputStreamWriter HTML String Compiled Unambig")
        (println "-------------------------------------")
        (bench (chassis-page-output-stream-writer-html-string-compiled-unambig data))
        (println)))))

(defn gen-bench-hiccup
  [data]
  (let [file-name (str "resources/bench/hiccup_" (count (:items data)) ".txt")]
    (with-open [wtr (clojure.java.io/writer file-name)]
      (binding [*out* wtr]
        (println "Hiccup")
        (println "-------------------------------------")
        (bench (hiccup-page data))
        (println)
        (println "Hiccup Compiled")
        (println "-------------------------------------")
        (bench (hiccup-page-compiled data))
        (println)
        (println "Hiccup Compiled Unambig")
        (println "-------------------------------------")
        (bench (hiccup-page-compiled-unambig data))
        (println)))))

(defn gen-bench-selmer
  [data]
  (let [file-name (str "resources/bench/selmer_" (count (:items data)) ".txt")]
    (with-open [wtr (clojure.java.io/writer file-name)]
      (binding [*out* wtr]
        (println "Selmer")
        (println "-------------------------------------")
        (bench (selmer-page data))
        (println)))))

(defn gen-bench-enlive
  [data]
  (let [file-name (str "resources/bench/enlive_" (count (:items data)) ".txt")]
    (with-open [wtr (clojure.java.io/writer file-name)]
      (binding [*out* wtr]
        (println "Enlive Item HTML")
        (println "-------------------------------------")
        (bench (enlive-page-item-html data))
        (println)))))

(defn gen-bench-all
  [data]
  (gen-bench-chassis data)
  (gen-bench-chassis-write-html data)
  (gen-bench-hiccup data)
  (gen-bench-selmer data)
  (gen-bench-enlive data))

(defn run-gen-bench-all
  [_]
  (gen-bench-all data-big)
  (gen-bench-all data-mid)
  (gen-bench-all data-small))

;; Make sure examples don't drift.

(assert (= (c/html [c/doctype-html5 (page data-small)])
           (c/html [c/doctype-html5 (page-doall data-small)])
           (chassis-page data-small)
           (chassis-page-compiled data-small)
           (chassis-page-compiled-unambig data-small)
           (chassis-page-alias data-small)
           (chassis-page-alias-compiled data-small)
           (chassis-page-alias-compiled-unambig data-small)
           (slurp (chassis-page-print-writer data-small))
           (slurp (chassis-page-print-writer-compiled-unambig data-small))
           (slurp (chassis-page-print-stream data-small))
           (slurp (chassis-page-print-stream-compiled-unambig data-small))
           (slurp (chassis-page-output-stream-writer data-small))
           (slurp (chassis-page-output-stream-writer-compiled-unambig data-small))
           (slurp (chassis-page-output-stream-writer-html-string data-small))
           (slurp (chassis-page-output-stream-writer-html-string-compiled-unambig data-small))))

(assert (= (hiccup-page data-small)
           (hiccup-page-compiled data-small)
           (hiccup-page-compiled-unambig data-small)))
