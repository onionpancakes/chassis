(ns build
  (:require [cognitect.test-runner.api :as tr]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def basis
  (delay (b/create-basis)))

(def class-dir
  "target/classes")

(def lib
  'dev.onionpancakes/chassis)

(def version
  (format "1.0.%s" (b/git-count-revs nil)))

(def jar-file
  (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (clean nil)
  (b/write-pom {:basis     @basis
                :src-pom   "./build/pom.xml"
                :src-dirs  ["src"]
                :class-dir class-dir
                :lib       lib
                :version   version})
  (b/copy-dir {:src-dirs   ["src"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))

(defn install [_]
  (b/install {:basis      @basis
              :class-dir  class-dir
              :lib        lib
              :version    version
              :jar-file   jar-file}))

(defn deploy [_]
  (jar nil)
  (dd/deploy {:installer :remote
              :artifact  jar-file
              :pom-file  (b/pom-path {:class-dir class-dir :lib lib})}))
