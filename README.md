# Chassis

HTML5 serialization for Clojure.

Renders [Hiccup](https://github.com/weavejester/hiccup/) style HTML vectors to strings.

Highly optimized runtime serialization, no macros. See [Performance](#performance).

# Status

Currently for my personal use. Future breaking changes possible.

# Deps

```clojure
{:deps
  {dev.onionpancakes/chassis
    {:git/url "https://github.com/onionpancakes/chassis"
     :git/sha "<GIT SHA>"}}}
```

# Example

```clojure
(require '[dev.onionpancakes.chassis.core :as c])

(defn my-post
  [post]
  [:div {:id (:id post)}
   [:h2.title (:title post)]
   [:p.content (:content post)]])

(defn my-blog
  [data]
  [c/doctype-html5
   [:html
    [:head
     [:link {:href "/css/styles.css" :rel "stylesheet"}]
     [:title "My Blog"]]
    [:body
     [:h1 "My Blog"]
      (for [p (:posts data)]
        (my-post p))]]])

(let [data {:posts [{:id "1" :title "foo" :content "bar"}]}]
  (c/html (my-blog data)))

;; "<!DOCTYPE html><html><head><link href=\"/css/styles.css\" rel=\"stylesheet\"><title>My Blog</title></head><body><h1>My Blog</h1><div id=\"1\"><h2 class=\"title\">foo</h2><p class=\"content\">bar</p></div></body></html>"
```

# Usage

Require the namespace.

```clojure
(require '[dev.onionpancakes.chassis.core :as c])
```

## Elements

Use `html` function to generate HTML strings from vectors.

Vectors with **global keywords** in the head position are treated as normal HTML elements. The keyword's name is used as the element's tag name.

```clojure
(c/html [:div "foo"])

;; "<div>foo</div>"
```

Maps in the second position are treated as attributes. Use **global keywords** to name attribute keys.

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

The rest of the vector is treated as the element's content. They may be of any type, including other elements. Sequences are flattened into the parent element's content.

```clojure
(c/html [:div {:id "my-id"}
         "foo"
         (for [i (range 3)] i)
         "bar"])

;; "<div id=\"my-id\">foo012bar</div>"
```

## Id and Class Sugar

Like Hiccup, id and class attributes can be specified along with the tag name using the `#` and `.` syntax.

```clojure
(c/html [:div#my-id.my-class "foo"])

;; "<div id=\"my-id\" class=\"my-class\">foo</div>"
```

```clojure
;; Multiple '.' classes concatenates
(c/html [:div.my-class-1.my-class-2 "foo"])

;; "<div class=\"my-class-1 my-class-2\">foo</div>"
```

```clojure
;; '.' classes concatenates with :class keyword
(c/html [:div.my-class-1 {:class "my-class-2"} "foo"])

;; "<div class=\"my-class-1 my-class-2\">foo</div>"
```


```clojure
;; Extra '#' are uninterpreted.
(c/html [:div## "foo"])

;; "<div id=\"#\">foo</div>"

(c/html [:div#my-id.my-class-1#not-id "foo"])

;; "<div id=\"my-id\" class=\"my-class-1#not-id\">foo</div>"
```

However, there are differences from Hiccup.

```clojure
;; '#' id takes precedence over :id keyword
(c/html [:div#my-id {:id "not-my-id"} "foo"])

;; "<div id=\"my-id\">foo</div>"
```

```clojure
;; '#' id can be place anywhere
(c/html [:div.my-class-1#my-id "foo"])

;; "<div id=\"my-id\" class=\"my-class-1\">foo</div>"
```

```clojure
;; '#' id can be place in-between, but don't do this.
;; It will be slightly slower.
(c/html [:div.my-class-1#my-id.my-class-2 "foo"])

;; "<div id=\"my-id\" class=\"my-class-1 my-class-2\">foo</div>"
```

## Boolean Attributes

Use `true`/`false` to toggle boolean attributes.

```clojure
(c/html [:button {:disabled true} "Submit"])

;; "<button disabled>Submit</button>"

(c/html [:button {:disabled false} "Submit"])

;; "<button>Submit</button>"
```

## Composite Attribute Values

Collection of attribute values are concatenated as spaced strings.

```clojure
(c/html [:div {:class ["foo" "bar"]}])

;; "<div class=\"foo bar\"></div>"

(c/html [:div {:class #{:foo :bar}}])

;; "<div class=\"bar foo\"></div>"
```

Map of attribute values are concatenated as style strings.

```clojure
(c/html [:div {:style {:color  :red
                       :border "1px solid black"}}])

;; "<div style=\"color: red; border: 1px solid black;\"></div>"
```

They arbitrarily nest.

```clojure
(c/html [:div {:style {:color  :red
                       :border [:1px :solid :black]}}])

;; "<div style=\"color: red; border: 1px solid black;\"></div>"
```

## Write to Appendable

Avoid intermediate allocation by writing directly to [`java.lang.Appendable`](https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/Appendable.html) by using the `write-html` function.

However, [`java.lang.StringBuilder`](https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/StringBuilder.html) is highly optimized and it may be faster to write to it (and then write the string out) than to write to the Appendable directly. Performance testing is advised.

```clojure
(let [out (get-appendable-from-somewhere)]
  (c/write-html out [:div "foo"]))
```

## Escapes

By default, text and attribute values are escaped.

```clojure
(c/html [:div "& < >"])

;; "<div>&amp; &lt; &gt;</div>"

(c/html [:div {:foo "& < > \" '"}])

;; "<div foo=\"&amp; &lt; &gt; &quot; &apos;\"></div>"
```

Escapes can be disabled locally by wrapping string values with `raw`.

```clojure
(c/html [:div (c/raw "<p>foo</p>")])

;; "<div><p>foo</p></div>"
```

Escapes can be disabled globally by altering vars. Change `escape-text-fragment` and `escape-attribute-value-fragment` to
`identity` function to allow fragment values to pass through unescaped.

```clojure
(alter-var-root #'c/escape-text-fragment (constantly identity))
(alter-var-root #'c/escape-attribute-value-fragment (constantly identity))

(c/html [:div "<p>foo</p>"])

;; "<div><p>foo</p></div>"
```

### Vetted Unescaped Types

For performance, `java.lang.Number` and `java.util.UUID` bypass the default escapement.

### Tags and Attribute Keys are not escaped!

Element tags and attribute keys are not escaped. Be careful when placing dangerous text in these positions.

```clojure
;; uhoh
(c/html [:<> "This is bad!"])

;; "<<>>This is bad!</<>>"

(c/html [:div {:<> "This is bad!"}])

;; "<div <>=\"This is bad!\"></div>"
```

## Non-element Vectors

Only vectors beginning with keywords are interpreted as elements. A vector can set its metadata `::c/content` key to true to avoid being interpreted as a element, even if it begins with a keyword.

```clojure
;; Not elements
(c/html [0 1 2])                  ; => "123"
(c/html ["foo" "bar"])            ; => "foobar"
(c/html ^::c/content [:foo :bar]) ; => "foobar"

;; Use this to generate fragments of elements
(c/html [[:div "foo"]
         [:div "bar"]]) ; "<div>foo</div><div>bar</div>"
```

## Non-attribute Keys

Only **global keywords** and **strings** are interpreted as attribute keys. Everything else is ignored.

```clojure
(c/html [:div {:foo/bar "not here!"}])

;; "<div></div>"
```

## Alias Elements

Alias elements are user defined elements. They resolve to other elements through the `resolve-alias` multimethod. They must begin with a **namespaced keyword**.

Define an alias element by extending the `resolve-alias` multimethod with a namespaced keyword and a function implementation receiving 4 arguments: metadata map, tag keyword, attributes map, and content vector.

Since namespaced keywords are not interpreted as attributes, they can be used as arguments for alias elements.

Attribute map received (3rd arg) contains the merged attributes from the alias element, including `id` and `class` from the element tag. By placing the alias element's attribute map as the attribute of the resolved element, the attribute merges seamlessly.

Content subvector received (4th arg) contains the content of the alias element. It has its metadata `{::c/content true}` to avoid being interpreted as an element.

The metadata and tag (1st and 2nd arg) are not needed for normal use case, but is provided for advanced tinkering.

```clojure
(defmethod c/resolve-alias ::Layout
  [_ _ {:layout/keys [title] :as attrs} content]
  [:div.layout attrs ; Merge attributes
   [:h1 title]
   [:main content]
   [:footer "Some footer message."]])

(c/html [::Layout#blog.dark {:layout/title "My title!"}
         [:p "My content!"]])

;; "<div id=\"blog\" class=\"layout dark\"><h1>My title!</h1><main><p>My content!</p></main><footer>Some footer message.</footer></div>"
```

## Stateful Values

Instances of `clojure.lang.IDeref` and `clojure.lang.Fn` are automatically dereferenced at serialization. Functions are invoked on their zero argument arity.

Whether or not if this is a good idea is left to the user.

```clojure
(defn get-thing []
  "foobar")

(c/html [:div get-thing])

;; #'user/get-thing"<div>foobar</div>"
```

```clojure
(def delayed-thing
  (delay "delayed"))

(c/html [:div {:foo delayed-thing}])

;; "<div foo=\"delayed\"></div>"
```

## Token and HTML Serializers

Use `token-serializer` and `html-serializer` to access individual token and fragment instances. The underlying type, `TokenSerializer`, implements `clojure.lang.IReduceInit` and it is intended to be used in a reduce.

```clojure
(->> (c/token-serializer [:div "foo"])
     (eduction (map type))
     (vec))

;; [dev.onionpancakes.chassis.core.OpeningTag
;;  java.lang.String
;;  dev.onionpancakes.chassis.core.ClosingTag]
```

```clojure
(->> (c/html-serializer [:div "foo"])
     (vec))

;; ["<div>" "foo" "</div>"]
```

# Performance

At this time, benchmarks shows Chassis to be ~50% to +100% faster when compared to other Clojure HTML templating libraries. See bench results in the resource folder.

However, the dev benchmark example is contrived and benchmarking with real world data is recommended.

```clojure
$ clj -M:dev
Clojure 1.11.1
user=> (quick-bench (chassis-page data-mid))
Evaluation count : 2040 in 6 samples of 340 calls.
             Execution time mean : 296.440399 µs
    Execution time std-deviation : 18.138611 µs
   Execution time lower quantile : 280.674056 µs ( 2.5%)
   Execution time upper quantile : 319.907138 µs (97.5%)
                   Overhead used : 8.824566 ns
nil
user=> (quick-bench (hiccup-page data-mid))
Evaluation count : 1104 in 6 samples of 184 calls.
             Execution time mean : 594.344971 µs
    Execution time std-deviation : 37.178706 µs
   Execution time lower quantile : 562.081951 µs ( 2.5%)
   Execution time upper quantile : 636.998749 µs (97.5%)
                   Overhead used : 8.824566 ns
nil
```

## Element Vector Allocation is Negligible

Element vector allocation accounts for less than 0.1% of the runtime cost.

```clojure
user=> (quick-bench (page data-mid))
Evaluation count : 3926280 in 6 samples of 654380 calls.
             Execution time mean : 166.441170 ns
    Execution time std-deviation : 40.458902 ns
   Execution time lower quantile : 131.334184 ns ( 2.5%)
   Execution time upper quantile : 227.189879 ns (97.5%)
                   Overhead used : 8.824566 ns
```

The vast proportion of the runtime cost is the iteration of HTML data structure and fragment writes.

### It's All Interned

Keywords and Strings are interned objects. Therefore the cost of allocating HTML vectors is mostly the cost of allocation vectors, and allocating vectors is really fast.

# License

Released under the MIT License.