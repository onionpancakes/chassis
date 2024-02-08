(ns user
  (:require [dev.onionpancakes.chassis.core :as c]
            [dev.onionpancakes.chassis.tests.test-core :as t]
            [criterium.core :refer [quick-bench]]
            [hiccup2.core :as hiccup]
            [hiccup.page :as hiccup.page]
            [selmer.parser :as selmer]
            [net.cgrand.enlive-html :as enlive]))

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

(defn item-element
  [item]
  [:div.item {:id    (:uuid item)
              :class (:type item)}
   [:h2 (:name item)]
   [:p (:date item)]
   [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
        "See more details."]]
   [:h3 "Description"]
   [:p (:text item)]])

(defn page
  [data]
  [:html {:lang "en"}
   [:head
    [:link {:href "/foobar1" :rel "stylesheet"}]
    [:link {:href "/foobar2" :rel "stylesheet"}]
    [:link {:href "/foobar3" :rel "stylesheet"}]
    [:link {:href "/foobar4" :rel "stylesheet"}]
    [:title (:title data)]]
   [:body
    [:header
     [:h1 (:title data)]]
    [:main
     (->> (:items data)
          (map item-element)
          (interpose [:hr]))]
    [:footer "Footer"]]])

(defmethod c/resolve-alias ::Layout
  [_ {title ::title :as attrs} content]
  [:html (into {:lang "en"} attrs)
   [:head
    [:link {:href "/foobar1" :rel "stylesheet"}]
    [:link {:href "/foobar2" :rel "stylesheet"}]
    [:link {:href "/foobar3" :rel "stylesheet"}]
    [:link {:href "/foobar4" :rel "stylesheet"}]
    [:title title]]
   [:body
    [:header
     [:h1 title]]
    [:main content]
    [:footer "Footer"]]])

(defmethod c/resolve-alias ::Item
  [_ {item ::item :as attrs} content]
  [:div.item (into {:id    (:uuid item)
                    :class (:type item)} attrs)
   [:h2 (:name item)]
   [:p (:date item)]
   [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
        "See more details."]]
   [:h3 "Description"]
   [:p (:text item)]])

(defn page-alias
  [data]
  [::Layout {::title (:title data)}
   (->> (:items data)
        (map #(vector ::Item {::item %}))
        (interpose [:hr]))])

(defn chassis-page
  [data]
  (c/html [c/doctype-html5 (page data)]))

(defn chassis-page-alias
  [data]
  (c/html [c/doctype-html5 (page-alias data)]))

(defn chassis-page-writer
  [data]
  (let [wtr (java.io.CharArrayWriter. 16384)]
    (with-open [w wtr]
      (c/write-html w [c/doctype-html5 (page data)]))
    (.toString wtr)))

(defn chassis-page-print-writer
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [w (java.io.PrintWriter. out false (java.nio.charset.Charset/forName "UTF-8"))]
      (c/write-html w [c/doctype-html5 (page data)]))
    (String. (.toByteArray out) "UTF-8")))

(defn chassis-page-print-stream
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [pout (-> (java.io.BufferedOutputStream. out 16384)
                         (java.io.PrintStream. false "UTF-8"))]
      (c/write-html pout [c/doctype-html5 (page data)]))
    (String. (.toByteArray out) "UTF-8")))

(defn chassis-page-output-stream-writer
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [pout (java.io.OutputStreamWriter. out "UTF-8")]
      (c/write-html pout [c/doctype-html5 (page data)]))
    (String. (.toByteArray out) "UTF-8")))

;; Hiccup

(defn hiccup-item-element
  [item]
  (hiccup/html
   [:div.item {:id    (:uuid item)
               :class (:type item)}
    [:h2 (:name item)]
    [:p (:date item)]
    [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
         "See more details."]]
    [:h3 "Description"]
    [:p (:text item)]]))

(defn hiccup-page
  [data]
  (str
   (hiccup/html
    {:mode :html}
    (hiccup.page/doctype :html5)
    [:html {:lang "en"}
     [:head
      [:link {:href "/foobar1" :rel "stylesheet"}]
      [:link {:href "/foobar2" :rel "stylesheet"}]
      [:link {:href "/foobar3" :rel "stylesheet"}]
      [:link {:href "/foobar4" :rel "stylesheet"}]
      [:title (:title data)]]
     [:body
      [:header
       [:h1 (:title data)]]
      [:main
       (->> (:items data)
            (map hiccup-item-element)
            (interpose (hiccup/html [:hr])))]
      [:footer "Footer"]]])))

(defn hiccup-page-runtime
  [data]
  (hiccup/html
   {:mode :html}
   (hiccup.page/doctype :html5)
   (page data)))

;; Selmer

(defn selmer-page
  [data]
  (selmer/render-file "selmer/page.html" data))

;; Enlive

(defn enlive-item-element
  [item]
  (enlive/html
   [:div {:id    (:uuid item)
          :class (str "item " (:type item))}
    [:h2 (:name item)]
    [:p (:date item)]
    [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
         "See more details."]]
    [:h3 "Description"]
    [:p (:text item)]]))

(enlive/deftemplate enlive-item-template "enlive/item.html"
  [item]
  [:.item] (comp (enlive/add-class (:type item))
                 (enlive/set-attr :id (:uuid item)))
  [:.name] (enlive/content (:name item))
  [:.date] (enlive/content (str (:date item)))
  [:a] (enlive/set-attr :href (str "/item/" (:uuid item)))
  [:.description] (enlive/content (:text item)))

(enlive/deftemplate enlive-page-template-item-html "enlive/page.html"
  [data]
  [:head :title] (enlive/content (:title data))
  [:body :header :h1] (enlive/content (:title data))
  [:body :main] (enlive/content
                 (->> (:items data)
                      (map enlive-item-element)
                      (interpose (enlive/html [:hr]))))
  [:body :footer] (enlive/content "Footer"))

(enlive/deftemplate enlive-page-template-item-template "enlive/page.html"
  [data]
  [:head :title] (enlive/content (:title data))
  [:body :header :h1] (enlive/content (:title data))
  [:body :main] (enlive/content
                 (->> (:items data)
                      (map enlive-item-template)
                      (interpose (enlive/html [:hr]))))
  [:body :footer] (enlive/content "Footer"))

(defn enlive-page-item-html
  [data]
  (apply str (enlive-page-template-item-html data)))

(defn enlive-page-item-template
  [data]
  (apply str (enlive-page-template-item-template data)))

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
  []
  (with-open [wtr (clojure.java.io/writer "resources/bench/results_big.txt")]
    (binding [*out* wtr]
      (bench-all data-big)))
  (with-open [wtr (clojure.java.io/writer "resources/bench/results_mid.txt")]
    (binding [*out* wtr]
      (bench-all data-mid)))
  (with-open [wtr (clojure.java.io/writer "resources/bench/results_small.txt")]
    (binding [*out* wtr]
      (bench-all data-small))))
