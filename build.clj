(ns build
  (:require [clojure.tools.build.api :as b]))

(def build-folder "target")

(def jar-content (str build-folder "/classes"))

(def basis (b/create-basis {:project "deps.edn"}))
(def version (str "0.1." (b/git-count-revs nil)))
(def app-name "hundredrps")
(def lib-name 'com.github.abcdw/hundredrps)
(def uber-file-name
  (format "%s/%s-standalone.jar" build-folder app-name))

(defn clean [_]
  (b/delete {:path build-folder})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn uber [_]
  (clean nil)

  (b/copy-dir {:src-dirs   ["resources"]
               :target-dir jar-content})

  (b/compile-clj {:basis     basis
                  :src-dirs  ["src"]
                  :class-dir jar-content})

  (b/write-pom {:class-dir jar-content
                :lib       lib-name
                :version   version
                :basis     basis
                :src-dirs  ["src"]})

  (b/uber {:class-dir jar-content
           :uber-file uber-file-name
           :basis     basis
           :main      'hundredrps.core})                ; here we specify the entry point for uberjar

  (println (format "Uber file created: \"%s\"" uber-file-name)))
