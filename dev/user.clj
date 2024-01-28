(ns user
  (:require [dev.onionpancakes.chassis.core :as c]))

(defn page
  [data]
  [:html {:lang "en"}
   [:body
    [:h1 (:title data)]
    [:div
     (for [m (:items data)]
       [:div
        [:a {:href (str "/item/" (:uuid m))}
         (:name m)]])]]])

(def data
  {:items (for [i (range 200)]
            {:uuid (random-uuid)
             :name (str "Item: " i)})})
