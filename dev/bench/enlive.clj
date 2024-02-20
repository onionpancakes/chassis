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

(enlive/deftemplate item-element-template "enlive/item.html"
  [item]
  [:.item] (comp (enlive/add-class (:type item))
                 (enlive/set-attr :id (:uuid item)))
  [:.name] (enlive/content (:name item))
  [:.date] (enlive/content (str (:date item)))
  [:a] (enlive/set-attr :href (str "/item/" (:uuid item)))
  [:.description] (enlive/content (:text item)))

(enlive/deftemplate page-template-item-html "enlive/page.html"
  [data]
  [:head :title] (enlive/content (:title data))
  [:body :header :h1] (enlive/content (:title data))
  [:body :main] (enlive/content
                 (->> (:items data)
                      (map (comp enlive/html item-element))
                      (interpose (enlive/html [:hr]))))
  [:body :footer] (enlive/content "Footer"))

(enlive/deftemplate page-template-item-template "enlive/page.html"
  [data]
  [:head :title] (enlive/content (:title data))
  [:body :header :h1] (enlive/content (:title data))
  [:body :main] (enlive/content
                 (->> (:items data)
                      (map item-element-template)
                      (interpose (enlive/html [:hr]))))
  [:body :footer] (enlive/content "Footer"))

(defn enlive-page-item-html
  [data]
  (apply str (page-template-item-html data)))

(defn enlive-page-item-template
  [data]
  (apply str (page-template-item-template data)))
