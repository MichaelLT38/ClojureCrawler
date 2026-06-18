(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'michaellt38/clojure-crawler)
(def version "0.1.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir
                  :ns-compile ['game.main]})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'game.main}))