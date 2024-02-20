(ns bench.enlive
  (:require [net.cgrand.enlive-html :as enlive]))

(defn item-element
  [item]
  [:div {:id    (:uuid item)
         :class (str "item " (:type item))}
   [:h2 (:name item)]
   [:p (:date item)]
   [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
        "See more details."]]
   [:h3 "Description"]
   [:p (:text item)]])

(enlive/deftemplate page-template-item-html "enlive/page.html"
  [data]
  [:head :title] (enlive/content (:title data))
  [:body :header :h1] (enlive/content (:title data))
  [:body :main] (enlive/content
                 (->> (:items data)
                      (map (comp enlive/html item-element))
                      (interpose (enlive/html [:hr]))))
  [:body :footer] (enlive/content "Footer"))

(defn enlive-page-item-html
  [data]
  (apply str (page-template-item-html data)))
