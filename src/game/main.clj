(ns game.main
  (:require [game.core :as core])
  (:gen-class))

(defn -main
  [& _args]
  (core/start-game))