
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
   :key     {:desc "A small brass key."}
   :sword   {:desc "A crude iron sword, still warm from the forge."}
   :torch   {:desc "A giant welding torch bolted to the attic floor."}
   :wizard  {:desc "A wizard, snoring loudly on the couch."}})

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
   :won              false
   :lost             false})

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
  (let [loc (:location state)
        base (get-in state [:rooms loc :desc])]
    (cond
      (and (= loc :living-room) (contains? (:flags state) :wizard-dead))
      ["You are in the living-room. The wizard lies motionless on the couch."]

      :else
      [base])))

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

(defn- scenery-here?
  "True for fixed room features that can be examined or used as targets."
  [state item]
  (case item
    :torch  (= (:location state) :attic)
    :wizard (= (:location state) :living-room)
    :well   (= (:location state) :garden)
    :door   (#{:attic :living-room :garden :portal-room} (:location state))
    false))

(defn examine [state item]
  (cond
    (nil? item)
    ["Usage: examine <object>."]

    (and (= item :wizard) (contains? (:flags state) :wizard-dead)
         (= (:location state) :living-room))
    ["The wizard is dead. His snoring has stopped forever."]

    (or (item-accessible? state item) (scenery-here? state item))
    (if-let [desc (get-in objects [item :desc])]
      [desc]
      [(str "You see nothing special about the " (name item) ".")])

    :else
    ["You don't see that here."]))

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
            (let [look-lines (look moved)
                  cursed?    (contains? (:flags state) :wizard-dead)]
              (if cursed?
                [(assoc moved :lost true :quit true)
                 (vec (concat look-lines
                              [""
                               "You step into the shimmering portal..."
                               "The light twists. Something cold follows you through."
                               "A whisper threads the void - the wizard's dying curse:"
                               "  \"Wherever you flee, this prison follows.\""
                               "You tumble out into endless grey fog. No sky. No door back."
                               "Freedom, of a sort. And a sentence that never ends."
                               ""
                               "*** YOU ESCAPED... BUT THE CURSE CAME WITH YOU. ***"
                               "*** BAD ENDING ***"]))]
                [(assoc moved :won true :quit true)
                 (vec (concat look-lines
                              [""
                               "You step into the shimmering portal..."
                               "Light engulfs you, and the dungeon fades away."
                               "*** YOU ESCAPED. YOU WIN! ***"]))]))
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

    ;; Weld the chain into a sword with the attic torch (either word order).
    (or (and (= a :chain) (= b :torch))
        (and (= a :torch) (= b :chain)))
    (cond
      (not= (:location state) :attic)
      [state ["There is no welding torch here."]]

      (contains? (:flags state) :sword-forged)
      [state ["You've already forged a sword."]]

      (not (item-accessible? state :chain))
      [state ["You don't have a chain."]]

      :else
      [(-> state
           (update :flags conj :sword-forged)
           (assoc-in [:object-locations :chain] :gone)
           (assoc-in [:object-locations :sword] :inventory))
       ["You heat the rusty chain in the giant welding torch."
        "Sparks fly. When it cools, you hold a crude iron sword."]])

    ;; Slay the snoring wizard (either word order).
    ;; Does not end the game; sets :wizard-dead so the portal escape becomes a bad ending.
    (or (and (= a :sword) (= b :wizard))
        (and (= a :wizard) (= b :sword)))
    (cond
      (not= (:location state) :living-room)
      [state ["There is no wizard here."]]

      (not (item-accessible? state :sword))
      [state ["You don't have a sword."]]

      (contains? (:flags state) :wizard-dead)
      [state ["The wizard is already dead."]]

      :else
      [(update state :flags conj :wizard-dead)
       ["You raise the crude sword and strike."
        "The wizard's snoring stops forever."
        "As he dies, a green spark leaps from his lips and sinks into your chest."
        "Something cold settles behind your ribs. You feel watched."]])

    :else
    [state ["Nothing happens."]]))

;; ---------------------------------------------------------------------------
;; Command dispatch (pure)
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