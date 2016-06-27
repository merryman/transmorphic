(ns examples.playground
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [transmorphic.core :refer [defcomponent]])
  (:require [transmorphic.symbolic :refer [format-code]]
            [om.next :as om]
            [goog.dom :as gdom]
            [transmorphic.repl :refer [init-compiler morph-defn morph-eval]]
            [transmorphic.utils :refer [add-points delta]]
            [transmorphic.morph :refer [$morph behavior]]
            [transmorphic.core :refer [listmorph ellipse rectangle rerender!
                                       image io text universe IRender IInitialize
                                       set-prop! set-root! refresh-scene! ace
                                       history-cache checkbox]]
            [transmorphic.manipulation :refer [changes-on-morph]]
            [transmorphic.event :refer [get-current-time meta-focus]]

            [transmorphic.tools.world :refer [world]]
            [transmorphic.tools.hand :refer [hand-morph]]
            [transmorphic.tools.window :refer [window]]
            [transmorphic.tools.function-editor :refer [component-editor]]
            [transmorphic.tools.ace]))

(cljs.core/declare digital-clock)


(defcomponent tree
  IRender
  (render [self {:keys [stem-pos]} _]
          (rectangle
           {:fill "brown",
            :inspectable? true,
            :grabbable? true,
            :id "stub",
            :extent {:y 62, :x 18},
            :position stem-pos}
           (ellipse
            {:fill "green",
             :inspectable? true,
             :grabbable? true,
             :id "cone",
             :extent {:y 100, :x 100},
             :position {:y -80, :x -40}}))))

(defcomponent scenery
  IRender
  (render [self props _ ]
          (rectangle
           {:fill "blue",
            :extent {:y 179, :x 274},
            :position (props :position)}
           (tree {:stem-pos {:y 95, :x 64}})
           (rectangle
            {:fill "green",
             :extent {:y 36, :x 274},
             :position {:y 143, :x 0}})
           (ellipse
            {:fill "yellow",
             :extent {:y 34, :x 37},
             :position {:y 25, :x 205}})
           (image
            {:extent {:y 101, :x 128},
             :url "http://pngimg.com/upload/cloud_PNG16.png",
             :position {:x 120, :y 9}}))))

(defcomponent namespace-viewer
  IRender
  (render [self props submorphs]
          (window 
           {:position (:position props)
            :extent (:extent props)
            :title "demo.clocks"})))

(def PI js/Math.PI)

(defn angle-for-hour [hour]
  (* (+ -0.25 (/ hour 12)) PI 2))

(defcomponent second-pointer
  IRender
  (render [self props _ ]
          (rectangle
           {:id "SecondPointer"
            :position {:x 0 :y -1.5}
            :fill "red"
            :rotation (* (+ -0.25 (/ (props :seconds) 60)) 2 PI)
            :stroke-width 2
            :extent {:x (* 0.85 (props :radius)) :y 3}})))

(defcomponent minute-pointer
  IRender
  (render [self props _]
          (rectangle {:id "MinutePointer"
                      :position {:x 0 :y -2}
                      :fill "darkblue"
                      :rotation (* (+ -0.25 (/ (props :minutes) 60)) 2 PI)
                      :stroke-width 2
                      :extent {:x (* .7 (props :radius)) :y 4}})))

(defcomponent hour-pointer
  IRender
  (render [self props _]
          (rectangle
           {:id "HourPointer"
            :position {:x 0 :y -2.5}
            :rotation (* (+ -0.25 (/ (props :hours) 12)) PI 2)
            :fill "darkblue"
            :stroke-width 2
            :extent {:x (* .5 (props :radius)) :y 5}})))

(defn point-from-polar [radius angle]
  {:x (* radius (.cos js/Math angle)) :y (* radius (.sin js/Math angle))})

(defcomponent hour-label
  IRender
  (render [self props _]
          (text
           {:id (str (props :label) "h")
            :position (add-points
                       (props :position)
                       (point-from-polar
                        (* (props :radius) .8)
                        (angle-for-hour (props :hour))))
            :text-string (props :label)
            :font-family "Arial"
            :allow-input false})))

(defcomponent
  clock
  IRender
  (render
    [{:as self, :keys [local-state]} {:keys [id extent position]} _]
    (let [{:keys [time]} local-state
          radius (/ (extent :x) 2)
          {:keys [x y]} extent
          offset {:x 0
                  :y 0}
          ext (if (> x y) {:y x, :x x} {:y y, :x y})
          direction (if (-> local-state :clockwise?) -1 1)]
      (ellipse
       {:id id,
        :position position,
        :extent extent,
        :fill "lightgrey"
        ; :step
        ; (fn [_]
        ;   (rerender! self {:time (get-current-time)})
        ;   (refresh-scene!))
        }
       (text {:position {:x (* 0.1 x) :y (* 0.1 y)}
                 :extent {:x 200 :y 40}
                 :text-string "backwards?"
                 :font-size 12
                 :text-color "black"
                 :font-family "Chrono Medium Italic"})
       (checkbox {:on-change (fn [_]
                               (rerender! self {:clockwise? 
                                                (-> local-state :clockwise? not)}))
                  :checked? (-> local-state :clockwise?)
                  :position {:x (* 0.0,5 x) :y (* 0.1 y)}})
       (map
        (fn [hour]
          (hour-label
           {:id (str hour "h"),
            :label hour,
            :hour hour,
            :position offset
            :radius radius,
            :extent {:x 30 :y 30}
            :font-size 12}))
        (range 1 13))
       (rectangle {:id "HourPointer"
                   :position offset
                   :rotation (* direction (+ -0.25 (/ (-> time :hours) 12)) PI 2)
                   :fill "darkblue"
                   :stroke-width 2
                   :extent {:x (* .5 radius) :y 5}})
       (rectangle {:id "MinutePointer"
                   :position offset
                   :fill "darkblue"
                   :rotation (* direction (+ -0.25 (/ (-> time :minutes) 60)) 2 PI)
                   :stroke-width 2
                   :extent {:x (* .7 radius) :y 4}})
       (rectangle {:id "SecondPointer"
                   :position offset
                   :fill "red"
                   :rotation (* direction (+ -0.25 (/ (-> time :seconds) 60)) 2 PI)
                   :stroke-width 2
                   :extent {:x (* 0.85 radius) :y 3}})))))


(defcomponent interaction-watcher
  IRender
  (render [self model _]
          (window {:extent (model :extent)
                   :position (model :position)
                   :title "User Interactions"
                   :scrollable? true
                   :on-resize (fn [new-extent] (rerender! self {:extent new-extent}))
                   :on-move (fn [new-pos] (rerender! self {:position new-pos}))
                   :submorphs
                   (when-let [txs (and @meta-focus (changes-on-morph @meta-focus))]
                     (let [hl (fn [v]
                                (text {:text-string v
                                       :font-size 12
                                       :text-color "black"
                                       :font-family "Chrono Medium Italic"}))
                           entry (fn [v]
                                   (text
                                    {:font-size 12,
                                     :text-color "grey",
                                     :text-string v,
                                     :font-family "Chrono Medium Italic"}))]
                       ; basically any visual property can be encapsueld by a widget
                       ; or part, since it can always be computet from the local state
                       ; or the props being passed. The one exception, where we actually need
                       ; to communicate visual properties between morphs, is in the case
                       ; of layouting. This is unfortunate, as it clutters the models with
                       ; unnessecary details about how morphs are supposed to be aligned.
                       ; it would be desireable to be able to ommit this, by specifying layout
                       ; properties in an entirely declarative fashion.
                       ; for example, the editor can default to always fitting its parent
                       ; and then the extent of the window part will influence the extent
                       ; of the ace editor accordingly.
                       ; We suggest a very basic variant of layouting by introducing relative props.
                       ; Relative props, are props that are evaluated with respect to the values of
                       ; other props in the immediate neighbourhood of a morph. (usually the owner)
                       [(ace {:position {:x 0 :y 0}
                              :id "model-viewer"
                              :value (format-code (-> ($morph :meta-focus)
                                                    :model :value))
                              :line-numbers? false
                              :edited? (atom false)
                              :extent {:x 400 :y 200}}
                             :model-viewer)
                        (listmorph {:id "structure"
                                    :position {:x 10 :y 210}}
                                   (hl "Property Changes:")
                                   (map (fn [[prop value]]
                                          (entry (str prop " " value)))
                                        (:prop-changes txs))
                                   (hl "Structural Changes:")
                                   (map (fn [tx]
                                          (entry (str (tx :op) " "
                                                      (tx :idx))))
                                        (:structure txs)))]))})))

(defcomponent scrubber
  IRender
  (render [self m _]
          (let [k (or (m :key) :extent)
                position (m :pos)
                model (m :model)
                transform (m :transform)]
            (ellipse
             {:fill "black"
              :id (m :id)
              :opacity 0.3
              :on-drag-start (fn [start-pos]
                               (rerender! self {:prev-cursor start-pos}))
              :on-drag (fn [new-cursor]
                         (let [{dx :x dy :y} (delta (model :prev-cursor) new-cursor)]
                           (rerender! self {:prev-cursor new-cursor
                                            k (or
                                               (when transform (transform delta (get model k)))
                                               (add-points (get model k)
                                                           {:x (- dx) :y (- dy)}))})))
              :position (add-points position {:x -25 :y -25})
              :extent {:x 25 :y 25}
              :css {"cursor" "nwse-resize"}}))))

(defcomponent kitchen
  transmorphic.core/IRender
  (render [_ _ _]
          (rectangle
           {:id "kitchen-morph"
            :position {:x 50 :y 50}}
           (ellipse
            {:id "ellipse"
             :wants-hand-focus? true
             :position {:x 100 :y 100}
             :extent {:x 100 :y 100}
             :fill "dodgerblue"
             :border-color "blue"})
           (rectangle
            {:id "rectangle"
             :wants-hand-focus? true
             :position {:x 300 :y 100}
             :extent {:x 200 :y 200}
             :border-color "lightgrey"
             :drop-shadow? true
             :fill "white"}
            (rectangle {:position {:x 0 :y 0}
                        :extent {:x 190 :y 35}
                        :fill "dodgerblue"})
            (text {:position {:x 10 :y 10}
                  :extent {:x 200 :y 40}
                 :text-string "commit changes..."
                 :font-size 12
                 :text-color "white"
                 :font-family "Chrono Medium Italic"})
          (text {:position {:x 10 :y 40}
                 :extent {:x 200 :y 40}
                 :text-string "publish as new..."
                 :font-size 12
                 :text-color "grey"
                 :font-family "Chrono Medium Italic"})
            (text {:position {:x 10 :y 70}
                  :extent {:x 200 :y 40}
                 :text-string "pull changes..."
                 :font-size 12
                 :text-color "grey"
                 :font-family "Chrono Medium Italic"}))
           (image {:position {:x 50 :y 100}
                   :url "http://localhost:9001/core/media/halos/morphmenu.svg"})
           (text
            {:id "text"
             :position {:x 700 :y 100}
             :text-string "Text"
             :font-family "Chrono Medium Italic"
             :allow-input false
             :font-size 17
             :extent {:x 50 :y 20}})
           (checkbox {:on-change (fn [_]
                                   (swap! transmorphic.event/stepping? not))
                      :checked? @transmorphic.event/stepping?
                      :position {:x 50 :y 50}})
            (clock {:id "Clock"
                    :extent {:x 300 :y 300}
                    :position {:x 300 :y 300}})
           (image {:url "https://scontent-fra3-1.cdninstagram.com/t51.2885-15/s640x640/sh0.08/e35/12816774_1978817152343619_1541514348_n.jpg?ig_cache_key=MTE5OTkxOTI4NDUzNzkxOTIyOA%3D%3D.2"
                   :extent {:x 300 :y 500}
                   :wants-hand-focus? true
                   :position {:x 300 :y 300}
                   :inspectable? true
                   :rotation 0.5
                   :id "coffee"}))))

(defcomponent
  digital-clock
  IRender
  (render [self props submorphs]
          (image
           {:extent {:x 200 :y 200}
            :position {:x 42 :y 42}
            :url "kermit.png"
            :on-mouse-enter (fn [e]
          	                   (prn "Mouse entered!"))}
           (ellipse 
            {:position {:x 0 :y 0}
             :fill "green"
             :extent {:x 100 :y 100}})
           (map (fn [i]
                  (text {:id (str i)
                         :value "Hello World!"
                         :position {:x (* i 10)
                                    :y (* i 10)}}))
                (range 10))
           (clock {:id "Clock"
                    :extent {:x 300 :y 300}
                    :position {:x 300 :y 300}}))))

(defcomponent my-world
  IRender
  (render [_ _ _]
        (world
          {:id "world"
          :fill "white"
          :extent {:x 2000 :y 2000}
          :mouse-move {:x 20 :y 20}}
   		     (namespace-viewer {:extent {:x 100 :y 100}
                             :position {:x 200 :y 200}})
          (digital-clock {:position {:x 100 :y 400}
                          :extent {:x 80 :y 120}})
         (kitchen {:id "kitchen"}))))

(defonce application
  (set-root!
    universe
    my-world
    {:id "foo"}
    (gdom/getElement "app")))

(def slider-state (atom {}))


; (set-root! slider-state
;   (history-slider {:width (count @history-cache) :position {:x 700 :y 200}
;                   :on-change (fn [self value]
;                                 (revert-to-state!
;                                 value
;                                 [:component/by-id
;                                   (:comoponent-id self)]))})
;   (gdom/getElement "history"))









