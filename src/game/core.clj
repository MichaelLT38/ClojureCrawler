
(ns game.core
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; World data
;;
;; Each room is a single map entry holding its description, its exits, and the
;; items that start there. To add a room, add one entry here; to remove one,
;; delete its entry (and any exits pointing at it). Exits may carry a :locked
;; key naming the item required to open them.
;; ---------------------------------------------------------------------------

(def rooms
  {:living-room {:desc  "You are in the living-room. A wizard is snoring loudly on the couch."
                 :exits {:west     {:to :garden :via "door"}
                         :upstairs {:to :attic  :via "ladder"}}
                 :items #{:whiskey :bucket}}
   :garden      {:desc  "You are in a beautiful garden. There is a well in front of you."
                 :exits {:east {:to :living-room :via "door"}}
                 :items #{:chain :frog :rope}}
   :attic       {:desc  "You are in the attic. There is a giant welding torch in the corner."
                 :exits {:downstairs {:to :living-room :via "ladder"}
                         :north      {:to :portal-room :via "iron door" :locked :key}}
                 :items #{}}
   :portal-room {:desc  "A shimmering portal hums in the center of the room."
                 :exits {:south {:to :attic :via "iron door"}}
                 :items #{}}})

;; Object descriptions (used by `examine`). Add an entry per object.
(def objects
  {:whiskey {:desc "A bottle of fine whiskey."}
   :bucket  {:desc "A sturdy wooden bucket with a handle."}
   :frog    {:desc "A slippery green frog. It eyes you warily."}
   :chain   {:desc "A length of rusty chain."}
   :rope    {:desc "A long, sturdy coil of rope."}
   :key     {:desc "A small brass key."}})

;; ---------------------------------------------------------------------------
;; Command aliases / shorthands
;; ---------------------------------------------------------------------------

(def command-aliases
  {"l" "look" "i" "inventory" "x" "examine"
   "get" "pickup" "h" "help" "q" "exit" "quit" "exit"})

(def directions
  #{"north" "south" "east" "west" "up" "down" "upstairs" "downstairs"})

(def direction-aliases
  {"n" "north" "s" "south" "e" "east" "w" "west"
   "u" "upstairs" "d" "downstairs"})

;; ---------------------------------------------------------------------------
;; State construction
;; ---------------------------------------------------------------------------

(defn- initial-object-locations [rooms]
  (into {} (for [[room {:keys [items]}] rooms
                 item items]
             [item room])))

(defn new-game []
  {:location         :living-room
   :rooms            rooms
   :object-locations (initial-object-locations rooms)
   :flags            #{}
   :quit             false
   :won              false})

;; ---------------------------------------------------------------------------
;; Query helpers (pure)
;; ---------------------------------------------------------------------------

(defn item-accessible?
  "True when an item is in the current room or in the player's inventory."
  [state item]
  (let [loc (get-in state [:object-locations item])]
    (or (= loc :inventory) (= loc (:location state)))))

(defn- items-at [state where]
  (->> (:object-locations state)
       (filter (fn [[_ loc]] (= loc where)))
       (map key)
       (sort)))

;; ---------------------------------------------------------------------------
;; Description helpers (pure)
;; ---------------------------------------------------------------------------

(defn describe-location [state]
  [(get-in state [:rooms (:location state) :desc])])

(defn describe-path [dir exit]
  (str "There is a " (:via exit) " going " (name dir) " from here."
       (when (:locked exit) " It is locked.")))

(defn describe-paths [state]
  (let [exits (get-in state [:rooms (:location state) :exits])]
    (map (fn [[dir exit]] (describe-path dir exit)) exits)))

(defn describe-objects [state]
  (map #(str "You see a " (name %) " on the floor.")
       (items-at state (:location state))))

(defn look [state]
  (vec (concat (describe-location state)
               (describe-paths state)
               (describe-objects state))))

(defn inventory [state]
  (let [items (items-at state :inventory)]
    (if (seq items)
      [(str "You are carrying: " (str/join ", " (map name items)))]
      ["You are not carrying anything."])))

(defn examine [state item]
  (cond
    (nil? item)
    ["Usage: examine <object>."]

    (not (item-accessible? state item))
    ["You don't see that here."]

    (get-in objects [item :desc])
    [(get-in objects [item :desc])]

    :else
    [(str "You see nothing special about the " (name item) ".")]))

(defn help [_]
  ["Available commands:"
   "  look                 - Describe your surroundings."
   "  walk <direction>     - Move (e.g. walk west). Or just type a direction."
   "  pickup <object>      - Pick up an item."
   "  drop <object>        - Drop an item (or e.g. 'drop frog bucket')."
   "  use <item> <target>  - Use an item on something."
   "  examine <object>     - Look closely at an item."
   "  inventory            - List what you are carrying."
   "  help                 - Show this help."
   "  exit                 - Quit the game."
   "Shorthands: l=look, i=inventory, x=examine, get=pickup, h=help, q=exit,"
   "            n/s/e/w/u/d for directions."])

;; ---------------------------------------------------------------------------
;; Commands (pure: take state, return [new-state lines])
;; ---------------------------------------------------------------------------

(defn walk [state direction]
  (if (nil? direction)
    [state ["Usage: walk <direction>."]]
    (let [dir  (keyword (get direction-aliases direction direction))
          exit (get-in state [:rooms (:location state) :exits dir])]
      (cond
        (nil? exit)
        [state ["You cannot go that way."]]

        (:locked exit)
        [state [(str "The " (:via exit) " is locked.")]]

        :else
        (let [dest  (:to exit)
              moved (assoc state :location dest)]
          (if (= dest :portal-room)
            [(assoc moved :won true :quit true)
             (vec (concat (look moved)
                          [""
                           "You step into the shimmering portal..."
                           "Light engulfs you, and the dungeon fades away."
                           "*** YOU ESCAPED. YOU WIN! ***"]))]
            [moved (look moved)]))))))

(defn pickup [state item]
  (cond
    (nil? item)
    [state ["Usage: pickup <object>."]]

    (= (get-in state [:object-locations item]) (:location state))
    [(assoc-in state [:object-locations item] :inventory)
     [(str "You are now carrying the " (name item) ".")]]

    :else
    [state ["You cannot get that."]]))

(defn drop-item [state a b]
  (cond
    (nil? a)
    [state ["Usage: drop <object>."]]

    ;; Context-aware: put the frog into the bucket.
    (and (= a :frog) (= b :bucket))
    (if (and (item-accessible? state :frog) (item-accessible? state :bucket))
      [(-> state
           (update :flags conj :frog-in-bucket)
           (assoc-in [:object-locations :frog] :in-bucket))
       ["You plop the frog into the bucket. It croaks indignantly."]]
      [state ["You need both the frog and the bucket for that."]])

    ;; Unhandled two-object drop.
    b
    [state ["You can't do that."]]

    ;; Plain drop.
    (= (get-in state [:object-locations a]) :inventory)
    [(assoc-in state [:object-locations a] (:location state))
     [(str "You drop the " (name a) ".")]]

    :else
    [state ["You aren't carrying that."]]))

(defn use-item [state a b]
  (cond
    (or (nil? a) (nil? b))
    [state ["Usage: use <item> <target>."]]

    ;; Tie the rope to the bucket.
    (and (= a :rope) (= b :bucket))
    (if (and (item-accessible? state :rope) (item-accessible? state :bucket))
      [(-> state
           (update :flags conj :rope-on-bucket)
           (assoc-in [:object-locations :rope] :on-bucket))
       ["You tie the rope securely to the bucket's handle."]]
      [state ["You need the rope and the bucket for that."]])

    ;; Lower the bucket into the well.
    (and (= a :bucket) (= b :well))
    (cond
      (not= (:location state) :garden)
      [state ["There is no well here."]]

      (not (item-accessible? state :bucket))
      [state ["You don't have a bucket."]]

      (not (contains? (:flags state) :rope-on-bucket))
      [state ["The bucket has no rope; you can't lower it into the well."]]

      (not (contains? (:flags state) :frog-in-bucket))
      [state ["You lower the empty bucket, but nothing happens. Perhaps it needs bait."]]

      :else
      [(-> state
           (update :flags conj :bucket-lowered)
           (assoc-in [:object-locations :frog] :gone)
           (assoc-in [:object-locations :key] :garden))
       ["You lower the baited bucket into the well."
        "The frog croaks, leaps free, and vanishes into the dark water."
        "You haul the bucket back up - a brass key glints inside it!"]])

    ;; Unlock the attic door with the key.
    (and (= a :key) (= b :door))
    (cond
      (not= (:location state) :attic)
      [state ["There is no door to unlock here."]]

      (not (item-accessible? state :key))
      [state ["You don't have a key."]]

      :else
      [(-> state
           (update-in [:rooms :attic :exits :north] dissoc :locked)
           (assoc-in [:object-locations :key] :gone))
       ["You turn the brass key. The iron door unlocks with a heavy clunk."]])

    :else
    [state ["Nothing happens."]]))

;; ---------------------------------------------------------------------------
;; Command dispatch (pure) + REPL shell
;; ---------------------------------------------------------------------------

(defn- normalize
  "Expand aliases and bare directions into canonical [cmd & args] tokens."
  [tokens]
  (when (seq tokens)
    (let [[raw & args] tokens]
      (cond
        (contains? direction-aliases raw) ["walk" (direction-aliases raw)]
        (contains? directions raw)        ["walk" raw]
        (contains? command-aliases raw)   (into [(command-aliases raw)] args)
        :else                             tokens))))

(defn handle-command
  "Pure command dispatcher. Returns [new-state lines]."
  [state input]
  (let [trimmed (some-> input str/trim)
        tokens  (when (seq trimmed) (normalize (str/split trimmed #"\s+")))
        [cmd & args] tokens
        a (some-> (first args) keyword)
        b (some-> (second args) keyword)]
    (if (nil? tokens)
      [state []]
      (case cmd
        "look"      [state (look state)]
        "inventory" [state (inventory state)]
        "help"      [state (help state)]
        "walk"      (walk state (first args))
        "pickup"    (pickup state a)
        "drop"      (drop-item state a b)
        "use"       (use-item state a b)
        "examine"   [state (examine state a)]
        "exit"      [(assoc state :quit true) ["Goodbye!"]]
        [state ["Unknown command. Type 'help' for a list of commands."]]))))

(defn show-result [lines]
  (doseq [line lines]
    (println line)))

(defn game-repl [state]
  (print "> ")
  (flush)
  (let [input (read-line)]
    (if (nil? input)
      (println "Goodbye!")
      (let [[new-state lines] (handle-command state input)]
        (show-result lines)
        (when-not (:quit new-state)
          (recur new-state))))))

(defn start-game []
  (println "Welcome to Clojure Crawler!")
  (println "Type 'help' for a list of commands.")
  (println)
  (let [state (new-game)]
    (show-result (look state))
    (game-repl state)))