(ns bench.chassis
  (:require [dev.onionpancakes.chassis.core :as c]
            [dev.onionpancakes.chassis.compiler :as cc]))

(defmethod c/resolve-alias ::Item
  [_ {item ::item :as attrs} content]
  [:div.item (merge {:id    (:uuid item)
                     :class (:type item)} attrs)
   [:h2 (:name item)]
   [:p (:date item)]
   [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
        "See more details."]]
   [:h3 "Description"]
   [:p (:text item)]])

(defmethod c/resolve-alias ::ItemCompiled
  [_ {item ::item :as attrs} content]
  (cc/compile
   [:div.item (merge {:id    (:uuid item)
                      :class (:type item)} attrs)
    [:h2 (:name item)]
    [:p (:date item)]
    [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
         "See more details."]]
    [:h3 "Description"]
    [:p (:text item)]]))

(defmethod c/resolve-alias ::ItemCompiledUnambig
  [_ {item ::item :as attrs} content]
  (cc/compile
   [:div.item (merge {:id    (:uuid item)
                      :class (:type item)} attrs)
    [:h2 nil (:name item)]
    [:p nil (:date item)]
    [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
         "See more details."]]
    [:h3 "Description"]
    [:p nil (:text item)]]))

(defmethod c/resolve-alias ::Layout
  [_ {title ::title :as attrs} content]
  [:html {:lang "en"}
   [:head
    [:link {:href "/foobar1" :rel "stylesheet"}]
    [:link {:href "/foobar2" :rel "stylesheet"}]
    [:link {:href "/foobar3" :rel "stylesheet"}]
    [:link {:href "/foobar4" :rel "stylesheet"}]
    [:title title]]
   [:body
    [:header
     [:h1 title]]
    [:main attrs content]
    [:footer "Footer"]]])

(defmethod c/resolve-alias ::LayoutCompiled
  [_ {title ::title :as attrs} content]
  (cc/compile
   [:html {:lang "en"}
    [:head
     [:link {:href "/foobar1" :rel "stylesheet"}]
     [:link {:href "/foobar2" :rel "stylesheet"}]
     [:link {:href "/foobar3" :rel "stylesheet"}]
     [:link {:href "/foobar4" :rel "stylesheet"}]
     [:title title]]
    [:body
     [:header
      [:h1 title]]
     [:main attrs content]
     [:footer "Footer"]]]))

(defmethod c/resolve-alias ::LayoutCompiledUnambig
  [_ {title ::title :as attrs} content]
  (cc/compile
   [:html {:lang "en"}
    [:head
     [:link {:href "/foobar1" :rel "stylesheet"}]
     [:link {:href "/foobar2" :rel "stylesheet"}]
     [:link {:href "/foobar3" :rel "stylesheet"}]
     [:link {:href "/foobar4" :rel "stylesheet"}]
     [:title nil title]]
    [:body
     [:header
      [:h1 nil title]]
     [:main ^java.util.Map attrs content]
     [:footer "Footer"]]]))

(defn item-element
  [item]
  [:div.item {:id    (str (:uuid item))
              :class (:type item)}
   [:h2 (:name item)]
   [:p (str (:date item))]
   [:p [:a.baz.buz {:href (str "/item/" (:uuid item))}
        "See more details."]]
   [:h3 "Description"]
   [:p (:text item)]])

(defn item-element-compiled
  [item]
  (cc/compile
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
  (cc/compile
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

(defn page-doall
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
          (interpose [:hr])
          (doall))]
    [:footer "Footer"]]])

(defn page-compiled
  [data]
  (cc/compile
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
           (interpose (cc/compile [:hr])))]
     [:footer "Footer"]]]))

(defn page-compiled-unambig
  [data]
  (cc/compile
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
           (interpose (cc/compile [:hr])))]
     [:footer "Footer"]]]))

(defn page-alias
  [data]
  [::Layout {::title (:title data)}
   (->> (:items data)
        (map #(vector ::Item {::item %}))
        (interpose [:hr]))])

(defn page-alias-compiled
  [data]
  (cc/compile
   [::LayoutCompiled {::title (:title data)}
    (->> (:items data)
         (map #(cc/compile [::ItemCompiled {::item %}]))
         (interpose (cc/compile [:hr])))]))

(defn page-alias-compiled-unambig
  [data]
  (cc/compile
   [::LayoutCompiledUnambig {::title (:title data)}
    (->> (:items data)
         (map #(cc/compile [::ItemCompiledUnambig {::item %}]))
         (interpose (cc/compile [:hr])))]))

;;

(defn chassis-page
  [data]
  (c/html [c/doctype-html5 (page data)]))

(defn chassis-page-compiled
  [data]
  (c/html [c/doctype-html5 (page-compiled data)]))

(defn chassis-page-compiled-unambig
  [data]
  (c/html [c/doctype-html5 (page-compiled-unambig data)]))

(defn chassis-page-alias
  [data]
  (c/html [c/doctype-html5 (page-alias data)]))

(defn chassis-page-alias-compiled
  [data]
  (c/html [c/doctype-html5 (page-alias-compiled data)]))

(defn chassis-page-alias-compiled-unambig
  [data]
  (c/html [c/doctype-html5 (page-alias-compiled-unambig data)]))

(defn chassis-page-print-writer
  [data]
  (let [out     (java.io.ByteArrayOutputStream. 16384)
        charset (java.nio.charset.Charset/forName "UTF-8")]
    (with-open [wtr (java.io.PrintWriter. out false charset)]
      (c/write-html wtr [c/doctype-html5 (page data)]))
    (.toByteArray out)))

(defn chassis-page-print-writer-compiled-unambig
  [data]
  (let [out     (java.io.ByteArrayOutputStream. 16384)
        charset (java.nio.charset.Charset/forName "UTF-8")]
    (with-open [wtr (java.io.PrintWriter. out false charset)]
      (c/write-html wtr [c/doctype-html5 (page-compiled-unambig data)]))
    (.toByteArray out)))

(defn chassis-page-print-stream
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [pout (java.io.PrintStream. out false "UTF-8")]
      (c/write-html pout [c/doctype-html5 (page data)]))
    (.toByteArray out)))

(defn chassis-page-print-stream-compiled-unambig
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [pout (java.io.PrintStream. out false "UTF-8")]
      (c/write-html pout [c/doctype-html5 (page-compiled-unambig data)]))
    (.toByteArray out)))

(defn chassis-page-output-stream-writer
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [pout (java.io.OutputStreamWriter. out "UTF-8")]
      (c/write-html pout [c/doctype-html5 (page data)]))
    (.toByteArray out)))

(defn chassis-page-output-stream-writer-compiled-unambig
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [pout (java.io.OutputStreamWriter. out "UTF-8")]
      (c/write-html pout [c/doctype-html5 (page-compiled-unambig data)]))
    (.toByteArray out)))

(defn chassis-page-output-stream-writer-html-string
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [pout (java.io.OutputStreamWriter. out "UTF-8")]
      (.write pout (c/html [c/doctype-html5 (page data)])))
    (.toByteArray out)))

(defn chassis-page-output-stream-writer-html-string-compiled-unambig
  [data]
  (let [out (java.io.ByteArrayOutputStream. 16384)]
    (with-open [pout (java.io.OutputStreamWriter. out "UTF-8")]
      (.write pout (c/html [c/doctype-html5 (page-compiled-unambig data)])))
    (.toByteArray out)))
