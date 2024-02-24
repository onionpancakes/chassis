# Chassis

Fast HTML5 serialization for Clojure.

Renders [Hiccup](https://github.com/weavejester/hiccup/) style HTML vectors to strings.

Highly optimized runtime serialization without macros. Even faster serialization when combined with compiling macros.

* See [Compiling Elements](#compiling-elements).
* See [Performance](#performance).

# Status

[![Run tests](https://github.com/onionpancakes/chassis/actions/workflows/run_tests.yml/badge.svg)](https://github.com/onionpancakes/chassis/actions/workflows/run_tests.yml)

Production released.

# Deps

Add one of these deployments to `deps.edn`.

### GitHub

```clojure
dev.onionpancakes/chassis {:git/url "https://github.com/onionpancakes/chassis"
                           :git/tag "v1.0.340" :git/sha "6babb84"}
```

### Clojars

```clojure
dev.onionpancakes/chassis {:mvn/version "1.0.340"}
```

# Example

### Runtime HTML Serialization

```clojure
(require '[dev.onionpancakes.chassis.core :as c])

(defn my-post
  [post]
  [:div {:id (:id post)}
   [:h2.title (:title post)]
   [:p.content (:content post)]])

(defn my-blog
  [data]
  [c/doctype-html5 ; Raw string for <!DOCTYPE html>
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

### Compiled HTML Serialization

```clojure
(require '[dev.onionpancakes.chassis.core :as c])
(require '[dev.onionpancakes.chassis.compiler :as cc])

(defn my-post-compiled
  [post]
  (cc/compile
    [:div {:id (:id post)}
     [:h2.title (:title post)]
     [:p.content (:content post)]]))

(defn my-blog-compiled
  [data]
  (cc/compile
    [c/doctype-html5 ; Raw string for <!DOCTYPE html>
     [:html
      [:head
       [:link {:href "/css/styles.css" :rel "stylesheet"}]
       [:title "My Blog"]]
      [:body
       [:h1 "My Blog"]
        (for [p (:posts data)]
          (my-post-compiled p))]]]))

(let [data {:posts [{:id "1" :title "foo" :content "bar"}]}]
  (c/html (my-blog-compiled data)))

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

The rest of the vector is treated as the element's content. They may be of any type, including other elements. Sequences, eductions, and [non-element vectors](#non-element-vectors) are logically flattened with the rest of the content.

```clojure
(c/html [:div {:id "my-id"}
         "foo"
         (for [i (range 3)] i)
         "bar"])

;; "<div id=\"my-id\">foo012bar</div>"
```

## Id and Class Sugar

Like Hiccup, id and class attributes can be specified along with the tag name using css style `#` and `.` syntax.

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

A collection of attribute values are concatenated as a spaced string.

```clojure
(c/html [:div {:class ["foo" "bar"]}])

;; "<div class=\"foo bar\"></div>"

(c/html [:div {:class #{:foo :bar}}])

;; "<div class=\"bar foo\"></div>"
```

A map of attribute values are concatenated as a style string.

```clojure
(c/html [:div {:style {:color  :red
                       :border "1px solid black"}}])

;; "<div style=\"color: red; border: 1px solid black;\"></div>"
```

Attribute collections and maps arbitrarily nest.

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

Only vectors beginning with keywords are interpreted as elements. A vector can set its metadata `::c/content` key to true to avoid being interpreted as an element, even if it begins with a keyword.

```clojure
;; Not elements
(c/html [0 1 2])                  ; => "012"
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

Alias elements are user defined elements. They resolve to other elements through the `resolve-alias` multimethod. They must begin with **namespaced keywords**.

Define an alias element by extending the `resolve-alias` multimethod with a namespaced keyword and a function implementation receiving 3 arguments: tag keyword, attributes map, and content vector.

Since namespaced keywords are not interpreted as attributes, they can be used as arguments for alias elements.

Attribute map received (2rd arg) contains the merged attributes from the alias element, including `id` and `class` from the element tag. By placing the alias element's attribute map as the attribute map of a resolved element, the attributes transfers seamlessly between the two.

Content subvector received (3rd arg) contains the content of the alias element. It has metadata `{::c/content true}` to avoid being interpreted as an element.

```clojure
;; Capitalized name optional, just to make it distinctive.
(defmethod c/resolve-alias ::Layout
  [_ {:layout/keys [title] :as attrs} content]
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
(defn current-year []
  (.getValue (java.time.Year/now)))

(c/html [:footer "My Company Inc " current-year])

;; "<footer>My Company Inc 2024</footer>"
```

```clojure
(def delayed-thing
  (delay "delayed"))

(c/html [:div {:foo delayed-thing}])

;; "<div foo=\"delayed\"></div>"
```

They can even deference into other elements.

```clojure
(defn get-children []
  [:p "Child element"])

(c/html [:div.parent get-children])

;; "<div class=\"parent\"><p>Child element</p></div>"
```

## Token and HTML Serializers

Use `token-serializer` and `html-serializer` to access individual tokens and fragment instances. The underlying type implements `clojure.lang.IReduceInit` and is intended to be used in a reduce.

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

## RawString Constants

### DOCTYPE

Use `c/doctype-html5`. It's just a RawString wrapping `<!DOCTYPE html>`. Because it's a RawString, it is safe to wrap in a vector to concatenate with the rest of the HTML document.

```clojure
(c/html [c/doctype-html5 [:html "..."]])

;; "<!DOCTYPE html><html>...</html>"
```

### &amp;nbsp;

Use the `c/nbsp` constant.

```clojure
(c/html [:div "foo" c/nbsp "bar"])

;; "<div>foo&nbsp;bar</div>"
```

# Compiling Elements

Require the namespace.

```clojure
(require '[dev.onionpancakes.chassis.compiler :as cc])
```

## Compile Examples

Slap a `cc/compile` wherever speed is needed! Then call `c/html` like normal to generate HTML.

```clojure
;; In defs
(def global-element
  (cc/compile [:div "foo"]))

;; In defns
(defn fn-element
  [arg]
  (cc/compile [:div "foo" arg "bar"]))

;; In aliases
(defmethod c/resolve-alias ::MyElement
  [_ attrs content]
  (cc/compile
    [:div
     [:p attrs content]]))

;; In fn args
(fn-element (cc/compile [:p "some content"]))

;; Then call c/html like normal to generate HTML.
(c/html (fn-element 123))

;; "<div>foo123bar</div>"
```

## Compile Usage

Chassis provides compiling macros `cc/compile` and `cc/compile*`. They take **one** argument, the root HTML tree, and they return compiled versions of the HTML tree. Use them to compile elements before passing them to `c/html`.

```clojure
(defn my-element []
  (cc/compile
    [:div [:p "foobar"]]))

(c/html (my-element))

;; "<div><p>foobar</p></div>"
```

Compiling **flattens** and **compacts** the HTML tree, making subsequent calls to `c/html` much faster.

```clojure
(macroexpand-1 '(cc/compile [:div [:p "foobar"]]))

;; Results in:
#object[dev.onionpancakes.chassis.core.RawString 0x11c2d9a2 "<div><p>foobar</p></div>"]

(let [body (identity "some-dynamic-content")]
  (pprint
    (macroexpand-1
      '(cc/compile
        [:div.deeply
          [:div.nested
            [:div.thing
              [:p "before" body "after"]]]]))))

;; Results in:
[#object[dev.onionpancakes.chassis.core.RawString 0x66fd28ce "<div class=\"deeply\"><div class=\"nested\"><div class=\"thing\"><p>before"]
 body
 #object[dev.onionpancakes.chassis.core.RawString 0xe9c5af6 "after</p></div></div></div>"]]
```

Use `cc/compile` for most purposes. For performance, the returned value may or may not be a vector. This is so that compiling small fragments of fully compacted HTML (like `<hr>`) is as efficient as possible when iterated over by `c/html`.

```clojure
;; <hr> is not wrapped as a 1-sized vector
(cc/compile [:hr])

;; #object[dev.onionpancakes.chassis.core.RawString 0x6ba58490 "<hr>"]

;; The end result is the same either way,
;; but the runtime serialization is faster this way.
(->> (range 10)
     (interpose (cc/compile [:hr]))
     (c/html))

;; "0<hr>1<hr>2<hr>3<hr>4<hr>5<hr>6<hr>7<hr>8<hr>9"
```

Use `cc/compile*` to ensure the return value is a vector. Otherwise, it is the same as `cc/compile`.

```clojure
;; <hr> is wrapped as a 1-sized vector
(cc/compile* [:hr])

;; [#object[dev.onionpancakes.chassis.core.RawString 0x24f1caeb "<hr>"]]
```

### Compiled Elements Must Have Literal Tags

A small but subtle difference between `cc/compile` and `c/html` is that `cc/compile` assumes elements are **literal** vectors with **literal** keyword tags. Vectors without literal tags, after [var resolution](#var-resolved-constants), are assumed to be content.

```clojure
;; Basically don't do this.
(let [footag :div]
  (c/html (cc/compile [footag "It's foobarred."])))

;; "divIt's foobarred."

;; Works at runtime.
(let [footag :div]
  (c/html [footag "It's foobarred."]))

;; "<div>It's foobarred.</div>"
```

## Ambiguous Attributes Produce Speed Bumps

Ambiguous objects in the second position forces the compiler to emit checks which examine the potential attributes map at runtime.

```clojure
(let [data {:body "foo"}]
  (pprint (macroexpand-1
     ;; Compiler can't see what (:body data) returns.
    '(cc/compile [:div (:body data)]))))

;; Results in:
[(let*
  [attrs13475 (:body data)]
  (if ;; Check if 2nd item is attrs map at runtime.
   (dev.onionpancakes.chassis.core/attrs? attrs13475)
   (dev.onionpancakes.chassis.core/->OpeningTag
    nil
    :div
    nil
    nil
    attrs13475)
   [#object[dev.onionpancakes.chassis.core.OpeningTag 0x34b8fe4b "<div>"]
    attrs13475]))
 #object[dev.onionpancakes.chassis.core.RawString 0x1b09ab08 "</div>"]]
```

### Resolving Ambiguity - Force Attributes Absence

Use `nil` in second position to force compile the element without attributes.

```clojure
(let [data {:body "foo"}]
  (pprint (macroexpand-1
    '(cc/compile [:div nil (:body data)]))))

;; Results in:
[#object[dev.onionpancakes.chassis.core.RawString 0x6e42ae2e "<div>"]
 (:body data)
 #object[dev.onionpancakes.chassis.core.RawString 0x588c9f7d "</div>"]]
```

### Resolving Ambiguity - Force Attributes Presence

Type hint the second position with either `java.util.Map` or `clojure.lang.IPersistentMap` to force compile elements with attributes.

```clojure
(let [data {:attrs {:foo "bar"}
            :body  "foo"}]
  (pprint (macroexpand-1
    '(cc/compile [:div ^java.util.Map (:attrs data) (:body data)]))))

;; Results in:
[(dev.onionpancakes.chassis.core/->OpeningTag
  nil
  :div
  nil
  nil
  (:attrs data))
 (:body data)
 #object[dev.onionpancakes.chassis.core.RawString 0x6314faa "</div>"]]
```

Type hinting the argument or bindings also works.
* Note: It doesn't show up correctly in a `macroexpand`, but it does works normally. This is because `cc/compile` examines the type hints from macro implied arg `&env`, and `macroexpand` for some reason doesn't capture `&env`.

```clojure
;; Should work!
(defmethod c/resolve-alias ::CompileWithAttrs
  [_ ^java.util.Map attrs content]
  (cc/compile [:div attrs content]))

(let [^java.util.Map attrs {:foo "bar"}]
  (cc/compile [:div attrs "foobar"]))
```

### Vetted Attributes Core Functions

Certain functions in `clojure.core` which returns maps are consider as attributes when called in the second position. Type hinting these invocations is not necessary. They include:

* `array-map`
* `hash-map`
* `sorted-map`
* `sorted-map-by`
* `assoc`
* `assoc-in`
* `merge`
* `select-keys`
* `update-keys`
* `update-vals`

```clojure
;; Useful in aliases when merging attrs.
(defmethod c/resolve-alias ::AliasWithAttrsMerge
  [_ attrs content]
  (cc/compile
    [:div (merge {:foo "bar"} attrs)
      content]))
```

### Warn on Ambiguous Attributes

Call `(cc/set-warn-on-ambig-attrs!)` to turn on warnings when compiling elements with ambiguous attributes. It will add a tap which prints out warning messages to `*err*` whenever ambiguous attributes are compiled.

Call `(cc/unset-warn-on-ambig-attrs!)` to disable.

## Compilation Barriers

### Function Calls

Functions calls, and generally any list values, block compilation traversal. Call `cc/compile` again to compile forms within.

```clojure
(defn comp-blocked
  []
  [:p "blocked"])

(cc/compile [:div "foo" (comp-blocked) "bar"])

;; Results in:
[#object[dev.onionpancakes.chassis.core.RawString 0x67574bda "<div>foo"] 
 [:p "blocked"]
 #object[dev.onionpancakes.chassis.core.RawString 0x565edf06 "bar</div>"]]
```

### Alias Elements

Alias elements are implemented as `c/resolve-alias` (via `c/resolve-alias-with-meta`) function calls. As a result, they also block compilation. However, the arguments pass to `c/resolve-alias` will be compiled.

```clojure
(defmethod c/resolve-alias ::CompileMyAlias
  [_ attrs content]
  [:div attrs content])

(pprint
 (clojure.walk/macroexpand-all
  '(cc/compile
    [::CompileMyAlias {:foo "bar"}
     [:p "content 1"]
     [:p "content 2"]])))

;; Results in:
(dev.onionpancakes.chassis.core/resolve-alias-with-meta
 nil
 :user/CompileMyAlias
 {:foo "bar"}
 [#object[dev.onionpancakes.chassis.core.RawString 0x34e3a7d6 "<p>content 1</p><p>content 2</p>"]])
```

### Macro Calls

Macros are expanded during compilation. Like function calls, those which expand into lists block compilation.

```clojure
(pprint
 (cc/compile
  [:ol
   (for [i (range 4)]
     [:li i])]))

;; Results in:
[[#object[dev.onionpancakes.chassis.core.OpeningTag 0x6e462cc4 "<ol>"]
  ([:li 0] [:li 1] [:li 2] [:li 3])]
 #object[dev.onionpancakes.chassis.core.RawString 0x27b55932 "</ol>"]]

;; Manually call compile in the inner form to reach inside.
(pprint
 (cc/compile
  [:ol
   (for [i (range 4)]
     (cc/compile [:li i]))]))
```

Macros which expand into non-lists can participate in compilation. Therefore, it is possible to use macros to abstract element components in a compile friendly way.

Whether or not if this is a good idea is left to the user.

```clojure
(defmacro NonBlockingElement
  [content]
  [:p nil content])

(cc/compile [:div (NonBlockingElement "not-blocked")])

;; Results in:
#object[dev.onionpancakes.chassis.core.RawString 0x31b2d0a8 "<div><p>not-blocked</p></div>"]
```

## Var Resolved Constants

Symbols referring to **constant** values are **var resolved** during compilation traversal, thereby allowing those constant values to participate in compilation. These include constant types such as `String` and any collection of constants, as well as `c/doctype-html5`, `c/nbsp`, and any `c/raw` string values. See `cc/constant?`.

```clojure
;; Fully compacted!
;; Even with a symbol splitting content in the middle.
(cc/compile [:div "foo" c/nbsp "bar"])

;; Results in:
#object[dev.onionpancakes.chassis.core.RawString 0x7fb21735 "<div>foo&nbsp;bar</div>"]
```

## Runtime Compilation

Chassis provides two analogous compile functions, `compile-node` and `compile-node*`, for compiling HTML tree at runtime. They are useful for compiling static HTML pages or components.

Because compiling happens at runtime, lists, function calls, and alias elements are no longer compilation barriers and ambiguous attributes are not possible.

Runtime compilation is similar to generating HTML with `c/html` but with key differences:

* The return values are `raw` strings, allowing the result to be embedded in other HTML components without the HTML tags being escaped.
* Stateful values, such as functions and derefs, are not realized.

```clojure
(defn current-time []
  (java.time.LocalTime/now))

(defmethod c/resolve-alias ::CurrentTime
  [_ _ _]
  [:p "Current time is: " current-time])

(def static-page
  (cc/compile-node
    [::CurrentTime]))

;; Results in:
[#object[dev.onionpancakes.chassis.core.RawString 0x7a702aaf "<p>Current time is: "]
 ;; Notice current-time function is not yet called.
 #object[user$current_time 0x584d9dc4 "user$current_time@584d9dc4"]
 #object[dev.onionpancakes.chassis.core.RawString 0x1c59c510 "</p>"]]

;; Stateful values realized on call to c/html
(c/html static-page)

;; "<p>Current time is: 13:48:14.228299269</p>"
```

# Performance

At this time, benchmarks shows Chassis to be 2x faster (and often more!) when compared to other Clojure HTML templating libraries on equivalent benchmark examples.

See bench results in the resource folder.

```clojure
$ clj -M:dev
Clojure 1.11.1

;; Chassis

user=> (quick-bench (chassis-page data-mid))
Evaluation count : 2712 in 6 samples of 452 calls.
             Execution time mean : 229.730870 µs
    Execution time std-deviation : 7.583674 µs
   Execution time lower quantile : 221.593639 µs ( 2.5%)
   Execution time upper quantile : 237.951723 µs (97.5%)
                   Overhead used : 8.800684 ns
nil
user=> (quick-bench (chassis-page-compiled data-mid))
Evaluation count : 4722 in 6 samples of 787 calls.
             Execution time mean : 131.554387 µs
    Execution time std-deviation : 4.400562 µs
   Execution time lower quantile : 127.024648 µs ( 2.5%)
   Execution time upper quantile : 137.206151 µs (97.5%)
                   Overhead used : 8.800684 ns
nil
user=> (quick-bench (chassis-page-compiled-unambig data-mid))
Evaluation count : 6186 in 6 samples of 1031 calls.
             Execution time mean : 100.309952 µs
    Execution time std-deviation : 3.392984 µs
   Execution time lower quantile : 98.074419 µs ( 2.5%)
   Execution time upper quantile : 105.031335 µs (97.5%)
                   Overhead used : 8.800684 ns
nil

;; Hiccup

user=> (quick-bench (hiccup-page data-mid))
Evaluation count : 990 in 6 samples of 165 calls.
             Execution time mean : 615.536499 µs
    Execution time std-deviation : 15.886454 µs
   Execution time lower quantile : 599.567903 µs ( 2.5%)
   Execution time upper quantile : 637.703394 µs (97.5%)
                   Overhead used : 8.800684 ns
nil
user=> (quick-bench (hiccup-page-compiled data-mid))
Evaluation count : 1044 in 6 samples of 174 calls.
             Execution time mean : 594.160734 µs
    Execution time std-deviation : 15.249740 µs
   Execution time lower quantile : 576.246477 µs ( 2.5%)
   Execution time upper quantile : 611.946104 µs (97.5%)
                   Overhead used : 8.800684 ns
nil
user=> (quick-bench (hiccup-page-compiled-unambig data-mid))
Evaluation count : 2544 in 6 samples of 424 calls.
             Execution time mean : 246.390352 µs
    Execution time std-deviation : 6.001164 µs
   Execution time lower quantile : 240.872342 µs ( 2.5%)
   Execution time upper quantile : 255.422063 µs (97.5%)
                   Overhead used : 8.800684 ns
nil

;; Selmer

user=> (quick-bench (selmer-page data-mid))
Evaluation count : 1428 in 6 samples of 238 calls.
             Execution time mean : 455.954085 µs
    Execution time std-deviation : 14.867158 µs
   Execution time lower quantile : 443.374807 µs ( 2.5%)
   Execution time upper quantile : 478.302764 µs (97.5%)
                   Overhead used : 8.800684 ns
nil

;; Enlive

user=> (quick-bench (enlive-page-item-html data-mid))
Evaluation count : 282 in 6 samples of 47 calls.
             Execution time mean : 2.254892 ms
    Execution time std-deviation : 83.779038 µs
   Execution time lower quantile : 2.156587 ms ( 2.5%)
   Execution time upper quantile : 2.341325 ms (97.5%)
                   Overhead used : 8.800684 ns
nil
```

## Element Vector Allocation is Small

Element vector allocation accounts for ~5% of the runtime cost.

```clojure
user=> (quick-bench (page-doall data-mid))
Evaluation count : 34752 in 6 samples of 5792 calls.
             Execution time mean : 18.073864 µs
    Execution time std-deviation : 623.107379 ns
   Execution time lower quantile : 17.421242 µs ( 2.5%)
   Execution time upper quantile : 18.715025 µs (97.5%)
                   Overhead used : 8.800684 ns
nil
```

The vast proportion of the runtime cost is the iteration of HTML data structure and fragment writes.

### It's All Interned

Keywords and Strings are interned objects. Therefore the cost of allocating HTML vectors is mostly the cost of allocation vectors, and allocating vectors is really fast.

# License

Released under the MIT License.