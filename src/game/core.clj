
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

(defn show-result [result]
  (when result
    (doseq [line result]
      (println line))))

(defn first-arg-symbol [args]
  (when-let [arg (first args)]
    (symbol arg)))

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
  (cond
    (nil? object) ["Usage: pickup <object>."]
    (some #(= % object) (objects-at *location* *objects* *object-locations*))
    (do (alter-var-root #'*object-locations* (fn [locs] (assoc locs object 'body)))
        [(str "You are now carrying the " object ".")])
    :else ["You cannot get that."]))

(defn inventory []
  (let [items (objects-at 'body *objects* *object-locations*)]
    (if (seq items)
      [(str "You are carrying: " (str/join ", " items))]
      ["You are not carrying anything."])))


;; Custom REPL function
(defn game-repl []
  (loop []
    ;; Read input from the user
    (print "> ")
    (flush)
    (let [input (read-line)
          [cmd & args] (when input (str/split (str/trim input) #"\s+"))]
      ;; Handle commands
      (if (nil? input)
        (println "Goodbye!")
        (case cmd
          "look" (show-result (look))
          "inventory" (show-result (inventory))
          "walk" (show-result (walk (first-arg-symbol args)))
          "pickup" (show-result (pickup (first-arg-symbol args)))
          "exit" (println "Goodbye!")
          (println "Unknown command.")))
      ;; Continue loop unless 'exit' command is given or input ended
      (when (and input (not= cmd "exit"))
        (recur)))))

(defn start-game []
  (println "Starting the game...")
  (game-repl))