# Chassis

Fast HTML5 serialization for Clojure.

Renders [Hiccup](https://github.com/weavejester/hiccup/) style HTML vectors to strings.

# Status

Currently for my personal use. Future breaking changes possible.

# Deps

```clojure
{:deps
  {dev.onionpancakes/chassis
    {:git/url "https://github.com/onionpancakes/chassis"
     :git/sha "<GIT SHA>"}}}
```

# Usage

Require the namespace.

```clojure
(require '[dev.onionpancakes.chassis.core :as c])
```

Use `html` function to generate HTML strings from vectors.

Vectors with global keywords in the head position are treated as normal HTML elements. The keyword's name is used as the element's tag name.

```clojure
(c/html [:div "foo"])

;; "<div>foo</div>"
```

Maps in the second position are treated as attributes. Use global keywords to name attribute keys.

```clojure
(c/html [:div {:id "my-id"} "foo"])

;; "<div id=\"my-id\">foo</div>"
```

```clojure
;; Strings also accepted, but discouraged.
;; Use when keywords cannot encode the desired attribute name.
(c/html [:div {"id" "my-id"} "foo"])

;; "<div id=\"my-id\">foo</div>"
```

Like Hiccup, id and class attributes can be specified along with the tag name using the `#` and `.` syntax.

```clojure
(c/html [:div#my-id.my-class "foo"])

;; <div id=\"my-id\" class=\"my-class\">foo</div>
```

```clojure
;; '#' id takes precedence over :id keyword
(c/html [:div#my-id {:id "not-my-id"} "foo"])

;; <div id=\"my-id\">foo</div>
```

```clojure
;; Multiple '.' classes concatenates
(c/html [:div.my-class-1.my-class-2 "foo"])

;; <div id=\"my-id\" class=\"my-class-1 my-class-2\">foo</div>
```

```clojure
;; '.' classes concatenates with :class keyword
(c/html [:div.my-class-1 {:class "my-class-2"} "foo"])

;; <div class=\"my-class-1 my-class-2\">foo</div>
```

Nest vector elements and wrap them in functions to create composable HTML documents.

```clojure
(defn my-post
  [post]
  [:div.post
   [:h2.title (:title post)]
   [:p.content (:content post)]])

(defn my-blog
  [posts]
  [c/doctype-html5 ; Chassis provided constant for !DOCTYPE
   [:html
    [:head [:title "My Blog"]]
     [:body
      [:h1 "My Blog"]
       (for [p posts]
         (my-post p))]]])

(let [data [{:title "foo" :content "bar"}]]
  (c/html (my-blog data)))

;; "<!DOCTYPE html><html><head><title>My Blog</title></head><body><h1>My Blog</h1><div class=\"post\"><h2 class=\"title\">foo</h2><p class=\"content\">bar</p></div></body></html>"
```

# License

Released under the MIT License.