(ns game.gui
  "A small terminal-style GUI front-end built with cljfx (JavaFX).

  It is a thin shell over game.core: the pure `handle-command` does all the
  work, and this namespace only renders state and forwards user input."
  (:require [clojure.string :as str]
            [cljfx.api :as fx]
            [game.core :as core]))

;; ---------------------------------------------------------------------------
;; Application state
;; ---------------------------------------------------------------------------

(defn initial-state []
  (let [game (core/new-game)]
    {:game  game
     :input ""
     :log   (into ["Welcome to Clojure Crawler!"
                   "Type a command below and press Enter (try 'help')."
                   ""]
                  (core/look game))}))

(defonce *state (atom (initial-state)))

(defn- append-log [log input lines]
  (into log (concat [(str "> " input)] lines)))

;; ---------------------------------------------------------------------------
;; Event handling (map events -> pure core calls)
;; ---------------------------------------------------------------------------

(defmulti handle-event :event/type)

(defmethod handle-event ::input-changed [{:keys [fx/event]}]
  (swap! *state assoc :input event))

(defmethod handle-event ::submit [_]
  (swap! *state
         (fn [{:keys [game input log] :as state}]
           (let [trimmed (str/trim (or input ""))]
             (if (empty? trimmed)
               state
               (let [[game' lines] (core/handle-command game trimmed)]
                 (assoc state
                        :game  game'
                        :input ""
                        :log   (append-log log trimmed lines))))))))

(defmethod handle-event ::new-game [_]
  (reset! *state (initial-state)))

(defmethod handle-event :default [_] nil)

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(defn- status-text [game]
  (if (:won game)
    "You escaped! Start a new game to play again."
    (str "Location: " (name (:location game)))))

(defn root-view [{:keys [log input game]}]
  {:fx/type :stage
   :showing true
   :title   "Clojure Crawler"
   :width   660
   :height  520
   :scene
   {:fx/type :scene
    :root
    {:fx/type :v-box
     :spacing 8
     :padding 12
     :children
     [{:fx/type :label
       :style   {:-fx-font-weight :bold}
       :text    (status-text game)}
      {:fx/type      :text-area
       :v-box/vgrow  :always
       :editable     false
       :wrap-text    true
       :focus-traversable false
       :style        {:-fx-font-family "monospace"}
       :text         (str/join "\n" log)}
      {:fx/type  :h-box
       :spacing  8
       :children
       [{:fx/type          :text-field
         :h-box/hgrow      :always
         :text             input
         :disable          (boolean (:won game))
         :prompt-text      "Type a command and press Enter..."
         :on-text-changed  {:event/type ::input-changed}
         :on-action        {:event/type ::submit}}
        {:fx/type   :button
         :text      "Send"
         :disable   (boolean (:won game))
         :on-action {:event/type ::submit}}
        {:fx/type   :button
         :text      "New game"
         :on-action {:event/type ::new-game}}]}]}}})

;; ---------------------------------------------------------------------------
;; Renderer / entry point
;; ---------------------------------------------------------------------------

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root-view)
    :opts {:fx.opt/map-event-handler handle-event}))

(defn -main [& _args]
  (fx/mount-renderer *state renderer))
