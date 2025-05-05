
(ns game.core
  (:require [clojure.string :as str]))

;; Define global states using defs
(def ^:dynamic *nodes*
  {'living-room ["You are in the living-room. A wizard is snoring loudly on the couch."]
   'garden ["You are in a beautiful garden. There is a well in front of you."]
   'attic ["You are in the attic. There is a giant welding torch in the corner."]})

(def ^:dynamic *edges*
  {'living-room [['garden 'west 'door] ['attic 'upstairs 'ladder]]
   'garden [['living-room 'east 'door]]
   'attic [['living-room 'downstairs 'ladder]]})

(def ^:dynamic *objects* ['whiskey 'bucket 'frog 'chain])

(def ^:dynamic *object-locations*
  {'whiskey 'living-room
   'bucket 'living-room
   'chain 'garden
   'frog 'garden})

(def ^:dynamic *location* 'living-room)

;; Helper functions
(defn describe-location [location nodes]
  (get nodes location))

(defn describe-path [edge]
  (str "There is a " (nth edge 2) " going " (nth edge 1) " from here."))

(defn describe-paths [location edges]
  (map describe-path (get edges location)))

(defn objects-at [loc objs obj-locs]
  (filter (fn [obj] (= (get obj-locs obj) loc)) objs))

(defn describe-objects [loc objs obj-loc]
  (map (fn [obj] (str "You see a " obj " on the floor.")) (objects-at loc objs obj-loc)))

;; Core functions
(defn look []
  (concat (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defn walk [direction]
  (let [next (first (filter #(= (nth % 1) direction) (get *edges* *location*)))]
    (if next
      (do (alter-var-root #'*location* (fn [_] (first next)))
          (look))
      ["You cannot go that way."])))

(defn pickup [object]
  (if (some #(= % object) (objects-at *location* *objects* *object-locations*))
    (do (alter-var-root #'*object-locations* (fn [locs] (assoc locs object 'body)))
        (str "You are now carrying the " object "."))
    ["You cannot get that."]))

(defn inventory []
  (cons 'items- (objects-at 'body *objects* *object-locations*)))


;; Custom REPL function
(defn game-repl []
  (loop []
    ;; Read input from the user
    (print "> ")
    (flush)
    (let [input (read-line)
          [cmd & args] (str/split input #" ")]
      ;; Handle commands
      (case cmd
        "look" (println (look))
        "inventory" (println (inventory))
        "walk" (println (walk (keyword (first args))))
        "pickup" (println (pickup (keyword (first args))))
        "exit" (println "Goodbye!")
        (println "Unknown command."))
      ;; Continue loop unless 'exit' command is given
      (when (not= cmd "exit")
        (recur)))))

(defn start-game []
  (println "Starting the game...")
  (game-repl))