(ns bench.selmer
  (:require [selmer.parser :as selmer]))

(defn selmer-page
  [data]
  (selmer/render-file "selmer/page.html" data))
