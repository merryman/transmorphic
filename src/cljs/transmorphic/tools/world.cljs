(ns transmorphic.tools.world
  (:require-macros [transmorphic.core :refer [defcomponent]])
  (:require 
   [om.next :as om]
   [transmorphic.event :refer [meta-focus get-cursor-pos hand-focus meta-click?
                               drop-meta-focus! drop-hand-focus!]]
   [transmorphic.core :refer [rectangle text rerender! universe update-dynamic-props! refresh-scene!]]
   [transmorphic.morph :refer [$morph behavior
                               position-in-world]]
   [transmorphic.tools.halo :refer [halo]]
   [transmorphic.tools.hand :refer [hand-morph local-hand-name handle-grab-or-drag]]
   [transmorphic.tools.function-inspector 
    :refer [scrubber-pane inspect-function! inspected-morphs]]
   [transmorphic.tools.function-editor :refer [edited-morphs edit-morph! edit-component!]]
   [transmorphic.tools.window :refer [window]]
   [transmorphic.utils :refer [add-points eucl-distance]]))

; this is used to render morphs that are special to the
; local user. When collaboration happens, these objects
; are rendered per user. e.g. hand morph
(defn get-local-hkey [] "localtoast")

(defcomponent world
  transmorphic.core/IInitialize
  (initialize [self]
              {:hand-position {:x 10 :y 10}
               :edited-morphs #{}
               :edited-components #{}})
  transmorphic.core/IRender
  (render [{:keys [local-state] :as self} props submorphs]
          (rectangle 
           {:id "root-morph"
            :position {:x 0 :y 0}
            :on-mouse-down (fn [e]
                             (when (and (not (meta-click? e)) 
                                        @meta-focus)
                               (drop-meta-focus!))
                             (rerender! self {})
                             (refresh-scene!))
            :on-mouse-up (fn [e]
                           (handle-grab-or-drag self (get-cursor-pos e))
                           (drop-hand-focus!)
                           (refresh-scene!))
            :on-mouse-move (fn [e]
                             (handle-grab-or-drag self (get-cursor-pos e))
                             (rerender! self {:hand-position (get-cursor-pos e)})
                             (refresh-scene!))}
           (rectangle {:id "world-morph" 
                       :extent (props :extent) 
                       :position {:x 0 :y 0} 
                       :fill "darkgrey"}
                      submorphs)
           (for [ec (local-state :edited-components)]
             (edit-component! ec))
           (for [em (local-state :edited-morphs)]
             (edit-morph! em))
           ; (scrubber-pane) 
           (hand-morph {:id (str (get-local-hkey) "-hand")
                        :position (-> self :local-state :hand-position)})
           (halo {:id "halo"
                  :target (:morph-id @meta-focus)
                  :idle true}))))