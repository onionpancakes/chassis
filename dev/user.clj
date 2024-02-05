(ns user
  (:require [dev.onionpancakes.chassis.core :as c]
            [dev.onionpancakes.chassis.tests.test-core :as t]
            [criterium.core :refer [quick-bench]]
            [hiccup2.core :as hiccup]
            [hiccup.page :as hiccup.page]
            [selmer.parser :as selmer]
            [net.cgrand.enlive-html :as enlive]))

(defmethod c/alias-element ::foo
  [tag attrs content]
  [:div.foo attrs
   [:p.bar content]])

(defmethod c/alias-element ::item-link
  [tag attrs content]
  [:a.baz.buz attrs content])

(def data
  {:title "Test Items"
   :items (for [i (range 200)]
            {:type "foo"
             :uuid (random-uuid)
             :name (str "Item: " i)})})

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
    [:h1 (:title data)]
    [:div
     (for [m (:items data)]
       [:div.foobar
        [:a.baz.buz {:id    (:uuid m)
                     :class (:type m)
                     :href  (str "/item/" (:uuid m))}
         (:name m)]])]]])

(defn page-alias
  [data]
  [:html {:lang "en"}
   [:head
    [:link {:href "/foobar1" :rel "stylesheet"}]
    [:link {:href "/foobar2" :rel "stylesheet"}]
    [:link {:href "/foobar3" :rel "stylesheet"}]
    [:link {:href "/foobar4" :rel "stylesheet"}]
    [:title (:title data)]]
   [:body
    [:h1 (:title data)]
    [:div
     (for [m (:items data)]
       [:div.foobar
        [::item-link {:id    (:uuid m)
                      :class (:type m)
                      :href  (str "/item/" (:uuid m))}
         (:name m)]])]]])

(defn chassis-page
  [data]
  (c/html [c/doctype-html5 (page data)]))

(defn chassis-page-alias
  [data]
  (c/html [c/doctype-html5 (page-alias data)]))

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
      [:h1 (:title data)]
      [:div
       (for [m (:items data)]
         [:div.foobar
          [:a.baz.buz {:id    (:uuid m)
                       :class (:type m)
                       :href  (str "/item/" (:uuid m))}
           (:name m)]])]]])))

(defn hiccup-page-runtime
  [data]
  (hiccup/html
   {:mode :html}
   (hiccup.page/doctype :html5)
   (page data)))

(defn selmer-page
  [data]
  (selmer/render-file "selmer/page.html" data))

(enlive/deftemplate enlive-page-template "enlive/page.html"
  [data]
  [:body :h1] (enlive/content (:title data))
  [:body :div] (enlive/content
                (for [m (:items data)]
                  (enlive/html
                   [:div.foobar
                    [:a.baz.buz {:id    (:uuid m)
                                 :class (:type m)
                                 :href  (str "/item/" (:uuid m))}
                     (:name m)]]))))

(defn enlive-page
  [data]
  (apply str (enlive-page-template data)))
