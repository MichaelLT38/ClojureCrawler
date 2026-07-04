(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'michaellt38/clojure-crawler)
(def version "0.1.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def gui-basis (b/create-basis {:project "deps.edn" :aliases [:gui]}))
(def uber-file (format "target/%s-%s-standalone.jar" (name lib) version))
(def gui-uber-file (format "target/%s-%s-gui-standalone.jar" (name lib) version))

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

(defn gui-uber
  "Build a standalone jar that launches the cljfx GUI. Bundles cljfx and the
  JavaFX libraries for the current platform, so the resulting jar is
  platform-specific (built on Windows -> runs on Windows)."
  [_]
  (clean nil)
  ;; Copy sources so game.gui (which is not AOT-compiled) can be required at
  ;; runtime from inside the jar.
  (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
  ;; Only AOT the tiny launcher; game.gui/cljfx stay as runtime-loaded sources.
  (b/compile-clj {:basis gui-basis
                  :src-dirs ["src"]
                  :class-dir class-dir
                  :ns-compile ['game.gui-main]})
  (b/uber {:class-dir class-dir
           :uber-file gui-uber-file
           :basis gui-basis
           :main 'game.gui-main}))