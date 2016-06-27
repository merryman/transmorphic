(ns transmorphic.tools.world
  (:require-macros [transmorphic.core :refer [defcomponent]])
  (:require
   [om.next :as om]
   [transmorphic.event :refer [meta-focus get-cursor-pos hand-focus meta-click?
                               drop-meta-focus! drop-hand-focus!]]
   [transmorphic.core :refer [rectangle text rerender! ellipse history-count
                              universe update-dynamic-props! refresh-scene!
                              reverted-entities get-ref revert-history!]]
   [transmorphic.morph :refer [$morph behavior $parent $props
                               position-in-world]]
   [transmorphic.tools.halo :refer [halo]]
   [transmorphic.tools.hand :refer [hand-morph local-hand-name handle-grab-or-drag]]
   [transmorphic.tools.function-editor :refer [component-editor]]
   [transmorphic.tools.window :refer [window]]
   [transmorphic.utils :refer [add-points eucl-distance]]))

; this is used to render morphs that are special to the
; local user. When collaboration happens, these objects
; are rendered per user. e.g. hand morph
(defn get-local-hkey [] "localtoast")

(defcomponent
  slider
  transmorphic.core/IRender
  (render
    [{:keys [local-state] :as self}
     {:keys [width on-change position title value]}
     submorphs]
    (window
     {:title title
      :extent {:x (+ 20 width) :y 60}
      :on-mouse-down (fn [e]
                        (.stopPropagation e))}
     (rectangle
      {:id "slider",
       :position {:x 10 :y 10},
       :extent {:x width :y 3},
       :fill "grey",
       :pivot-point {:x 0, :y 0}}
      (ellipse
        {:drop-shadow? false,
         :fill "silver",
         :pivot-point {:x 0, :y 0},
         :draggable? true,
         :on-drag
         (fn [{:keys [x]}]
           (let [{:keys [current-pos current-ext value]} local-state
                 new-pos (add-points current-pos {:x x, :y 0})
                 new-pos (if (or
                               (< (:x current-ext) (new-pos :x))
                               (> 0 (new-pos :x)))
                           current-pos
                           new-pos)
                 new-value (:x new-pos)]
             (rerender! self {:current-pos new-pos})
             (when on-change (on-change self (/ new-value width))))),
         :on-drag-start
         (fn [{:keys [x y]}]
           ; stop stepping
           (rerender!
             self
             {:current-pos {:x x, :y y},
              :current-ext {:x width, :y 19}})),
         :on-drag-stop
         (fn []
           ; start stepping again
           )
         :id "knob",
         :extent {:x 20, :y 20},
         :wants-hand-focus? true,
         :position {:x (* width (or value 1)), :y -10}})))))

(defcomponent world
  transmorphic.core/IRender
  (render [{:keys [local-state] :as self} props submorphs]
          (rectangle
           {:id "root-morph"
            :position {:x 0 :y 0}
            :did-mount (fn [_]
                            (rerender! self {:hand-position {:x 10 :y 10}
                                             :edited-morphs #{}
                                             :edited-components #{}}))
            :on-mouse-down (fn [e]
                             (when (and (not (meta-click? e))
                                        @meta-focus)
                               (drop-meta-focus!))
                             (rerender! self {}))
            :on-mouse-up (fn [e]
                           (handle-grab-or-drag self (get-cursor-pos e))
                           (drop-hand-focus!))
            :on-mouse-move (fn [e]
                             (handle-grab-or-drag self (get-cursor-pos e))
                             (rerender! self {:hand-position (get-cursor-pos e)}))}           
           (rectangle {:id "world-morph"
                      :extent (props :extent)
                      :position {:x 0 :y 0}
                      :fill (or (props :fill) "white")}
                      submorphs)
           (for [ec (:edited-morphs local-state)]
             (component-editor
              {:id (str "editor-on-" ec)
               :on-close (fn [_]
                           (rerender!
                            self
                            #(update-in %
                                        [:edited-morphs]
                                        disj ec)))
               :target-ref ec
               :position {:x 700 :y 100}
               :extent {:x 400 :y 600}}))
          ; (when (:morph-id @meta-focus)
          ;   (slider {:position {:x 700 :y 700}
          ;             :width 200
          ;             :value (/ (inc (or (get @reverted-entities (:morph-id @meta-focus))
          ;                               (history-count)))
          ;                       (inc (history-count)))
          ;             :title (str "History of " (-> @meta-focus :morph-id $morph $props :id))
          ;             :on-change (fn [self value]
          ;                         (let[i (.. js/Math (floor (* value (history-count))))]
          ;                           (revert-history! (-> @meta-focus :morph-id $morph) i)))}))
           (hand-morph {:id (str (get-local-hkey) "-hand")
                        :position (-> self :local-state :hand-position)})
           (halo {:id "halo"
                  :start-editing (fn [target]
                                   (rerender!
                                    self
                                    #(update-in %
                                                [:edited-morphs]
                                                conj (:morph-id target))))
                  :target (:morph-id @meta-focus)
                  :idle true}))))
