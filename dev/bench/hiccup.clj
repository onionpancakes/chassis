(ns bench.hiccup
  (:require [hiccup2.core :as hiccup]
            [hiccup.page :as hiccup.page]))

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

(defn item-element-compiled
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

(defn item-element-compiled-unambig
  [item]
  (hiccup/html
   [:div.item {:id    (:uuid item)
               :class (:type item)}
    [:h2 nil (:name item)]
    [:p nil (:date item)]
    [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
         "See more details."]]
    [:h3 "Description"]
    [:p nil (:text item)]]))

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

(defn page-compiled
  [data]
  (hiccup/html
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
           (map item-element-compiled)
           (interpose (hiccup/html [:hr])))]
     [:footer "Footer"]]]))

(defn page-compiled-unambig
  [data]
  (hiccup/html
   [:html {:lang "en"}
    [:head
     [:link {:href "/foobar1" :rel "stylesheet"}]
     [:link {:href "/foobar2" :rel "stylesheet"}]
     [:link {:href "/foobar3" :rel "stylesheet"}]
     [:link {:href "/foobar4" :rel "stylesheet"}]
     [:title nil (:title data)]]
    [:body
     [:header
      [:h1 nil (:title data)]]
     [:main nil
      (->> (:items data)
           (map item-element-compiled-unambig)
           (interpose (hiccup/html [:hr])))]
     [:footer "Footer"]]]))

(defn hiccup-page
  [data]
  (str
   (hiccup/html
    {:mode :html}
    (hiccup.page/doctype :html5)
    (page data))))

(defn hiccup-page-compiled
  [data]
  (str
   (hiccup/html
    {:mode :html}
    (hiccup.page/doctype :html5)
    (page-compiled data))))

(defn hiccup-page-compiled-unambig
  [data]
  (str
   (hiccup/html
    {:mode :html}
    (hiccup.page/doctype :html5)
    (page-compiled-unambig data))))
