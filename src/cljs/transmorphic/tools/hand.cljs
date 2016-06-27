(ns transmorphic.tools.hand
  (:require-macros [transmorphic.core :refer [defcomponent]])
  (:require 
   [om.next :as om]
   [transmorphic.event :refer [get-client-pos get-cursor-pos hand-focus]]
   [transmorphic.symbolic :refer [ellipse?]]
   [transmorphic.morph :refer [$morph morph-under-me
                               behavior is-submorph?
                               position-in-world
                               eval-reactive-prop local-offset]]
   [transmorphic.core :refer [rectangle text universe
                              rerender! set-prop! move-morph!
                              move-component!
                              IRender IInitialize IRefresh]]
   [transmorphic.utils :refer [add-points eucl-distance delta]]))

; the hand is a mechanism to interface with imperative actions to the UI
; without direct usage of the halo. The halo comes with a very invasive
; approach, and will override any attribute according to the users specification.
; The hand is more subtle, in that it will only communicate with the imperative
; layer, as specified by the code (grabbable, draggable).
; The hand can either grab or drag:
; dragging -> for the (delta based) scrubbing of arbitrary values
; grabbing -> a direct transmission of data between components

(defn local-hand-name [] "localtoast-hand")

; CUSTOM MUTATION API

(defn grab-component!
  [component]
  (let [hand ($morph (local-hand-name))
        relative-pos (delta (position-in-world component)
                            (position-in-world hand))]
    (move-component! component hand)
    ; by default, the root-morph is provided with the props
    ; passed to the component. In case the component definition
    ; specifies this, properties of the root may of course be overridden. 
    ; this approach prevents tedious default passing of external
    ; properties, which is especially common in a morphic environment
    (set-prop! component :position relative-pos)
    (set-prop! component :drop-shadow? true)))

(defn drop-component!
  [component]
  (let [new-parent (morph-under-me ($morph (local-hand-name)))
        relative-pos (add-points 
                      (delta (position-in-world component)
                             (position-in-world new-parent))
                      (delta {:x 0 :y 0} 
                             (local-offset new-parent)))]
    (move-component! component new-parent)
    (set-prop! component :drop-shadow? false)
    (set-prop! component :position relative-pos)))

(defn grab-morph! 
  "Moves the morph (if not moved already) referenced 
   by morph-id to the hand of the
   system. This bypasses the usual grabbing mechanism!"
  [morph]
  (let [relative-pos (delta (position-in-world morph)
                            (position-in-world ($morph (local-hand-name))))]
    (move-morph! morph ($morph (local-hand-name)))
    (set-prop! morph :position relative-pos)
    (set-prop! morph :drop-shadow? true)))

(defn drop-morph! 
  "Moves the morph currently residing in the hand morph
   onto the one directly beneath it."
  [morph]
  (let [new-parent (morph-under-me ($morph (local-hand-name)))
        relative-pos (add-points 
                      (delta (position-in-world morph)
                             (position-in-world new-parent))
                      (delta {:x 0 :y 0} 
                             (local-offset new-parent)))]
    (move-morph! morph new-parent)
    (set-prop! morph :drop-shadow? false)
    (set-prop! morph :position relative-pos)))

; BEHAVIOR

(defn handle-grab-or-drag [hand hand-position]
  (let [{:keys [morph-id start-pos prev-pos]} @hand-focus
        focused-morph ($morph morph-id)
        {:keys [dragged-morph grabbed-morph]} (-> hand :local-state)]
    ; since the callback funcions are re-bound in every render cycle,
    ; we need to re-fetch them in every handling iteration
    (let [{:keys [draggable? on-drag-start on-drag on-drag-stop position]} 
          (or 
           (:props ($morph dragged-morph))
           (:props focused-morph))]
      ; on drag start
      (when (and (and focused-morph (not dragged-morph))
                 draggable?
                 (< 10 (eucl-distance hand-position start-pos)))
        (rerender! hand {:dragged-morph morph-id})
        (swap! hand-focus assoc :prev-pos hand-position)
        (when on-drag-start (on-drag-start position)))
      ; on drag
      (when (and focused-morph dragged-morph) 
        (let [{dx :x dy :y} (delta prev-pos hand-position)]
          (when (not= 0 (+ dx dy))
            (swap! hand-focus assoc :prev-pos hand-position)
            ; (rerender! hand {:prev-pos hand-position})
            (when on-drag (on-drag {:x (- dx) :y (- dy)})))))
      ; on drag stop
      (when (and (not focused-morph) dragged-morph) 
        (prn "Stop")
        (when on-drag-stop (on-drag-stop position))
        (rerender! hand {:dragged-morph nil})
        (swap! hand-focus dissoc :prev-pos)))

    ; grabbing
    (let [{:keys [grabbable? on-grab on-drop]} 
          (or 
           (:props ($morph grabbed-morph))
           (:props focused-morph))]
      (when (and focused-morph (not grabbed-morph) grabbable?
                 (< 10 (eucl-distance start-pos hand-position)))
        ; if no data is to be transferred, we do not drag at all through the hand 
        (grab-morph! focused-morph)
        (rerender! hand {:grabbed-morph focused-morph}))
      ; if there is no morph below us, that accepts a dropped morph
      ; we snap the morph back into the place where it used to be
      (when (and (not focused-morph) grabbed-morph)
        (drop-morph! grabbed-morph)))))

(declare grabbable)

(defn in-hand? [morph]
  (is-submorph? ($morph (local-hand-name)) morph))

(defcomponent hand-morph
  IRender
  (render [self props submorphs]
          (rectangle {:id (local-hand-name) 
                      :fill "red"
                      :position (add-points
                                 {:x 4 :y 4}
                                 (:position props))
                      :extent {:x 2 :y 2}}
                     submorphs)))