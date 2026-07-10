(ns game.gui
  "A small terminal-style GUI front-end built with cljfx (JavaFX).

  It is a thin shell over game.core: the pure `handle-command` does all the
  work, and this namespace only renders state and forwards user input.

  Styled as a green-phosphor CRT: green-on-black monospace text with a soft
  glow, scanline overlay, and a subtle barrel-distortion curve on the screen."
  (:require [clojure.string :as str]
            [cljfx.api :as fx]
            [game.core :as core])
  (:import [javafx.application Platform]
           [javafx.beans.value ChangeListener]
           [javafx.scene.control TextArea]
           [javafx.scene.effect FloatMap]))

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
  (let [{:keys [game input log] :as state} @*state
        trimmed (str/trim (or input ""))]
    (when (seq trimmed)
      (let [[game' lines] (core/handle-command game trimmed)]
        (reset! *state (assoc state
                              :game  game'
                              :input ""
                              :log   (append-log log trimmed lines)))
        (when (:quit game')
          ;; Give the player time to read the final message, then close.
          ;; Win text is longer, so wait ~20s; a plain quit stays short.
          (let [ms (if (or (:won game') (:lost game')) 20000 3000)]
            (future
              (Thread/sleep ms)
              (Platform/runLater #(Platform/exit)))))))))

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

;; Stylesheet (as a self-contained data URI) that hides the text-area's
;; scroll bars, since the log auto-scrolls and never needs manual scrolling.
(def ^:private crt-css
  (let [css (str ".text-area .scroll-bar:vertical,"
                 ".text-area .scroll-bar:horizontal {"
                 "-fx-pref-width:0; -fx-pref-height:0;"
                 "-fx-max-width:0; -fx-max-height:0;"
                 "-fx-opacity:0; -fx-padding:0; }"
                 ".text-area .corner { -fx-opacity:0; -fx-padding:0; }")]
    (str "data:text/css;base64,"
         (.encodeToString (java.util.Base64/getEncoder)
                          (.getBytes css "UTF-8")))))

(defn- auto-scroll
  "Wrap a text-area description so it scrolls to the bottom whenever its text
  changes. When cljfx replaces the text, JavaFX relayouts and scrolls back to
  the caret (the top). We defer with a nested runLater so setScrollTop runs on
  the pulse *after* that relayout, landing on the newest line."
  [desc]
  {:fx/type    fx/ext-on-instance-lifecycle
   :on-created (fn [^TextArea ta]
                 (.addListener (.textProperty ta)
                               (reify ChangeListener
                                 (changed [_ _ _ _]
                                   (Platform/runLater
                                     (fn []
                                       (.positionCaret ta (.getLength ta))
                                       (Platform/runLater
                                         #(.setScrollTop ta Double/MAX_VALUE))))))))
   :desc       desc})

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
  (let [won?  (boolean (:won game))
        done? (or won? (boolean (:quit game)))]
    {:fx/type :stage
     :showing true
     :title   "Clojure Crawler"
     :width   660
     :height  520
     :scene
     {:fx/type     :scene
      :fill        :black
      :stylesheets [crt-css]
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
         [(auto-scroll
            {:fx/type      :text-area
             :editable     false
             :wrap-text    true
             :focus-traversable false
             :text         (str/join "\n" log)
             :style        {:-fx-control-inner-background screen-bg
                            :-fx-background-color screen-bg
                            :-fx-text-fill phosphor
                            :-fx-font-family mono
                            :-fx-font-size 36
                            :-fx-highlight-fill phosphor-dim
                            :-fx-highlight-text-fill phosphor-bright
                            :-fx-focus-color "transparent"
                            :-fx-faint-focus-color "transparent"}
             :effect       {:fx/type :glow :level 0.55}})
          scanlines]}
        {:fx/type  :h-box
         :spacing  8
         :children
         [{:fx/type          :text-field
           :h-box/hgrow      :always
           :text             input
           :disable          done?
           :prompt-text      "Type a command and press Enter..."
           :style            {:-fx-control-inner-background screen-bg
                              :-fx-background-color screen-bg
                              :-fx-text-fill phosphor
                              :-fx-prompt-text-fill phosphor-dim
                              :-fx-font-family mono
                              :-fx-font-size 36
                              :-fx-highlight-fill phosphor-dim
                              :-fx-border-color phosphor-dim
                              :-fx-border-width 1
                              :-fx-background-radius 3
                              :-fx-border-radius 3
                              :-fx-focus-color phosphor-dim
                              :-fx-faint-focus-color "transparent"}
           :on-text-changed  {:event/type ::input-changed}
           :on-action        {:event/type ::submit}}
          (crt-button "Send" {:event/type ::submit} done?)
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
