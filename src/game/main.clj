(ns game.main
  (:require [game.core :as core])
  (:gen-class))

(defn show-result [lines]
  (doseq [line lines]
    (println line)))

(defn game-repl [state]
  (print "> ")
  (flush)
  (let [input (read-line)]
    (if (nil? input)
      (println "Goodbye!")
      (let [[new-state lines] (core/handle-command state input)]
        (show-result lines)
        (cond
          (or (:won new-state) (:lost new-state))
          ;; Leave the ending message on screen before the process exits.
          (Thread/sleep 20000)

          (not (:quit new-state))
          (recur new-state))))))

(defn -main
  [& _args]
  (println "Welcome to Clojure Crawler!")
  (println "Type 'help' for a list of commands.")
  (println)
  (let [state (core/new-game)]
    (show-result (core/look state))
    (game-repl state)))