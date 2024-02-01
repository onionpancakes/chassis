(ns user
  (:require [dev.onionpancakes.chassis.core :as c]
            [criterium.core :refer [quick-bench]]
            [hiccup2.core :as h]))

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

(defn hiccup-page
  [data]
  (h/html
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
          (:name m)]])]]]))

(def data
  {:items (for [i (range 200)]
            {:type "foo"
             :uuid (random-uuid)
             :name (str "Item: " i)})})
