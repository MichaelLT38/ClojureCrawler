(ns game.gui
  "A small terminal-style GUI front-end built with cljfx (JavaFX).

  It is a thin shell over game.core: the pure `handle-command` does all the
  work, and this namespace only renders state and forwards user input.

  Styled as a green-phosphor CRT: green-on-black monospace text with a soft
  glow, scanline overlay, and a subtle barrel-distortion curve on the screen."
  (:require [clojure.string :as str]
            [cljfx.api :as fx]
            [game.core :as core])
  (:import [javafx.scene.effect FloatMap]))

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
;; CRT theme
;; ---------------------------------------------------------------------------

(def ^:private mono "'Consolas', 'Courier New', monospace")
(def ^:private phosphor "#3bff6a")        ; primary green text
(def ^:private phosphor-dim "#2aa74a")    ; dim green (prompts/borders)
(def ^:private phosphor-bright "#9dffb4") ; highlighted text
(def ^:private screen-bg "#04120a")       ; dark green-black glass
(def ^:private panel-bg "#0a170d")        ; bezel/panel background

(defn- barrel-float-map
  "Build a FloatMap encoding a subtle barrel (bulge) distortion. `strength`
  controls how much the edges curve; flip its sign for pincushion. The map is
  normalized, so it survives window resizing. Tunable knob for the CRT curve."
  ^FloatMap [size strength]
  (let [fm (FloatMap. size size)
        n  (double (dec size))]
    (dotimes [y size]
      (dotimes [x size]
        (let [u  (- (/ (* 2.0 x) n) 1.0)                 ; -1..1
              v  (- (/ (* 2.0 y) n) 1.0)                 ; -1..1
              r2 (/ (+ (* u u) (* v v)) 2.0)             ; 0..1
              dx (* (- strength) u r2)
              dy (* (- strength) v r2)]
          (.setSamples fm x y (float dx) (float dy)))))
    fm))

;; Built lazily so it is created on the JavaFX thread at first render.
(def ^:private barrel-map (delay (barrel-float-map 200 -0.03)))

(def ^:private scanlines
  {:fx/type :region
   :mouse-transparent true
   :style {:-fx-background-color
           (str "linear-gradient(from 0px 0px to 0px 3px, repeat, "
                "rgba(0,0,0,0.0) 0%, rgba(0,0,0,0.0) 50%, "
                "rgba(0,0,0,0.32) 51%, rgba(0,0,0,0.32) 100%)")}})

;; ---------------------------------------------------------------------------
;; Views
;; ---------------------------------------------------------------------------

(defn- crt-button [text event won?]
  {:fx/type   :button
   :text      text
   :disable   won?
   :style     {:-fx-background-color panel-bg
               :-fx-text-fill phosphor
               :-fx-border-color phosphor-dim
               :-fx-border-width 1
               :-fx-font-family mono
               :-fx-background-radius 3
               :-fx-border-radius 3}
   :on-action event})

(defn root-view [{:keys [log input game]}]
  (let [won? (boolean (:won game))]
    {:fx/type :stage
     :showing true
     :title   "Clojure Crawler"
     :width   660
     :height  520
     :scene
     {:fx/type :scene
      :fill    :black
      :root
      {:fx/type :v-box
       :spacing 8
       :padding 16
       :style   {:-fx-background-color panel-bg
                 :-fx-border-color "#123b1e"
                 :-fx-border-width 6
                 :-fx-background-radius 12
                 :-fx-border-radius 12}
       :children
       [;; The curved, scanlined "screen": text-area + scanline overlay,
        ;; warped together by the barrel DisplacementMap.
        {:fx/type     :stack-pane
         :v-box/vgrow :always
         :style       {:-fx-background-color screen-bg
                       :-fx-background-radius 8}
         :effect      {:fx/type   :displacement-map
                       :map-data  @barrel-map
                       :scale-x   1.0
                       :scale-y   1.0}
         :children
         [{:fx/type      :text-area
           :editable     false
           :wrap-text    true
           :focus-traversable false
           :text         (str/join "\n" log)
           :style        {:-fx-control-inner-background screen-bg
                          :-fx-background-color screen-bg
                          :-fx-text-fill phosphor
                          :-fx-font-family mono
                          :-fx-font-size 14
                          :-fx-highlight-fill phosphor-dim
                          :-fx-highlight-text-fill phosphor-bright
                          :-fx-focus-color "transparent"
                          :-fx-faint-focus-color "transparent"}
           :effect       {:fx/type :glow :level 0.55}}
          scanlines]}
        {:fx/type  :h-box
         :spacing  8
         :children
         [{:fx/type          :text-field
           :h-box/hgrow      :always
           :text             input
           :disable          won?
           :prompt-text      "Type a command and press Enter..."
           :style            {:-fx-control-inner-background screen-bg
                              :-fx-background-color screen-bg
                              :-fx-text-fill phosphor
                              :-fx-prompt-text-fill phosphor-dim
                              :-fx-font-family mono
                              :-fx-font-size 14
                              :-fx-highlight-fill phosphor-dim
                              :-fx-border-color phosphor-dim
                              :-fx-border-width 1
                              :-fx-background-radius 3
                              :-fx-border-radius 3
                              :-fx-focus-color phosphor-dim
                              :-fx-faint-focus-color "transparent"}
           :on-text-changed  {:event/type ::input-changed}
           :on-action        {:event/type ::submit}}
          (crt-button "Send" {:event/type ::submit} won?)
          (crt-button "New game" {:event/type ::new-game} false)]}]}}}))

;; ---------------------------------------------------------------------------
;; Renderer / entry point
;; ---------------------------------------------------------------------------

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root-view)
    :opts {:fx.opt/map-event-handler handle-event}))

(defn -main [& _args]
  (fx/mount-renderer *state renderer))
