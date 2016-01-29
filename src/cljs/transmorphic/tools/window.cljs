(ns transmorphic.tools.window
  (:require-macros [transmorphic.core :refer [defcomponent]])
  (:require [transmorphic.core :refer [rectangle image ellipse text rerender! 
                                       IRender IRefresh IInitialize html]]
            [transmorphic.morph :refer [position-in-world
                                        $morph behavior $parent]]
            [om.dom :as dom]
            [transmorphic.utils :refer [add-points delta]]
            [transmorphic.tools.hand]
            [transmorphic.event]))

(declare window)

(def control-height 14)

(defcomponent close-box
 IRender 
  (render [self model _]
  (ellipse
   {:position {:x 10 :y 10}
    :fill "#ff6052"
    :on-mouse-down (fn [e]
                   (when-let [cb! (model :on-close)]
                     (cb!)))
    :extent {:x control-height :y control-height}} 
   (text {:position {:x -2 :y -6}
          :text-string "×"
          :font-size 10
          :allow-input false
                    :visible (model :hover?)}))))

(defcomponent min-box 
  IRender
  (render [self model submorphs]
  (ellipse 
   {:position {:x 30 :y 10}
    :extent {:x control-height :y control-height}
    :fill "#ffbe06"
    :on-mouse-down (fn [e]
                   (rerender! self (update-in model [:collapsed?] not)))}
   (text {:position {:x -2 :y -6}
          :text-string "−"
          :font-size 10
          :allow-input false
                    :visible (model :hover?)}))))

(defcomponent window-resizer 
  IRender
  (render [self {:keys [window position collapsed?]} _]
          (image {:id "resizer"
                  :visible? (not collapsed?)
                  :url "http://localhost:3449/resize.png" 
                  :opacity 0.3
                  :position (add-points position {:x -25 :y -25})
                  :extent {:x 25 :y 25} 
                  :css {"cursor" "nwse-resize"}
                  :draggable? true
                  :on-drag (fn [delta]
                             (rerender! window {:extent (add-points position delta)}))})))

(defcomponent window-controls
 IRender 
  (render [self props _]
  (rectangle
   {:id "control-wrapper"
    :extent ($parent :extent #(hash-map :x (:x %) :y 0))}
   (text
    {:position {:x 100 :y 5}
     :text-string (props :title)
     :text-color "grey"
     :font-size 12
     :font-family "Chrono Medium Italic"})
   (html
    {:visible (:spinner? props)
     :fill "#f0f0f0"
     :position ($parent :extent #(hash-map :x (- (:x %) 80) :y -38))
     :extent {:x 60 :y 20} ; ($parent :extent #(hash-map :x 70 :y (:y %)))
     :html (let [style {:backgroundColor "#C3C3C3"
                        :width "15px"
                        :height "15px"}]
             (dom/div (clj->js {:className "sk-three-bounce"
                                :style {:padding "5px"}})
               (dom/div (clj->js {:className "sk-child sk-bounce1" 
                                  :style style}))
               (dom/div (clj->js {:className "sk-child sk-bounce2" 
                                  :style style}))
               (dom/div (clj->js {:className "sk-child sk-bounce3" 
                                  :style style}))))})
   (close-box props)
   (min-box props))))

(defcomponent window 
  IInitialize
  (initialize [self]
              {:extent nil
              :position nil})
  IRender
  (render [{:keys [local-state] :as self} 
           {:keys [title scrollable? on-close spinner?] :as props} 
           submorphs]
          (let [{:keys [extent position]} local-state]
            (rectangle 
             {:position (or position (:position props)) 
              :extent (or extent (:extent props))
              :border-color "grey"
              :fill "linear-gradient(to bottom, #f0f0f0, #e9e9e9)"
              :border-radius 10
              :border-width 1
              :drop-shadow? true
              :draggable? true
              :on-drag (fn [delta]
                         (rerender! 
                          self 
                          {:position (add-points (or position (:position props)) delta)}))}
             (rectangle
              {:id "window-wrapper"
               :css {"WebkitClipPath" "inset(-10px 0px 0px 3px round 10px 10px)"}
               :position {:x 0 :y 30}
               :scrollable? scrollable?
               :extent (add-points (or extent (:extent props)) {:x 0 :y -30})}
              submorphs)
             (window-controls {:window self
                               :on-close on-close
                               :title title
                               :spinner? spinner?})
             (window-resizer 
              {:position (or extent (:extent props))
               :window self})))))