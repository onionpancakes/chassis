(ns bench.rum
  (:require [rum.core :as rum]))

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

(defn rum-page
  [data]
  (str "<DOCTYPE html>" (rum/render-static-markup (page data))))
