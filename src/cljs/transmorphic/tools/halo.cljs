(ns transmorphic.tools.halo
  (:require-macros [transmorphic.core :refer [defcomponent]])
  (:require [transmorphic.morph :refer [position-in-world $morph 
                                        orphaned? $parent $props $local-state
                                        morph-under-me $component
                                        abstraction-root $owner
                                        eval-reactive-props Root]]
            [transmorphic.core :refer [rectangle image ellipse text get-ref
                                       rerender! move-morph! copy-morph! orphanize!
                                       remove-morph! set-prop! set-props! refresh-scene!
                                       detach-cache! get-root universe]]
            [transmorphic.tools.function-inspector :refer [inspected-morphs]]
            [transmorphic.tools.hand :refer [grab-morph! drop-morph! local-hand-name
                                             grab-component! drop-component!]]
            [transmorphic.event :refer [get-cursor-pos meta-focus arrow? shift?]]
            [transmorphic.utils :refer [add-points delta]]))

(declare resize-button inspect-button close-button grab-button drag-button edit-button
         copy-button styling-button rotate-button name-tag viewer pivot-cursor scaling-button style-editor)

(defn abs [v]
  (.abs js/Math v))

(defn rotate [v angle]
  {:x (* (v :x) (.cos js/Math angle)) 
   :y (* (v :y) (.cos js/Math angle))})

(defn dot [v u]
  (let [{vx :x vy :y} v
        {ux :x uy :y} u]
    (+ (* ux vx) (* uy vy))))

(defn norm [v]
  (let [{:keys [x y]} v
        l (.sqrt js/Math (dot v v))]
    {:x (/ x l) :y (/ y l)}))

(defn cross [v u]
  (let [{vx :x vy :y} v
        {ux :x uy :y} u]
    (- (* ux vy) (* uy vx))))

(defn compute-bounding-box [{:keys [extent scale rotation pivot-point]}]
  ; accurately compute a bounding box, that
  ; takes into account the extent, scale and rotation
  ; of the morph being inspected
  ; x2 = x0+(x-x0)*cos(theta)+(y-y0)*sin(theta)
  ; y2 = y0-(x-x0)*sin(theta)+(y-y0)*cos(theta)
  (let [{:keys [x y]} extent
        {px :x py :y} pivot-point
        s (or scale 1)
        t (- (or rotation 0))
        bounds [{:x 0 :y 0}
                {:x 0 :y (* s y)}
                {:x (* s x) :y 0}
                {:x (* s x) :y (* s y)}]
        xs (map (fn [{:keys [x y]}] 
                  (+ px 
                     (* (- x px) (.cos js/Math t)) 
                     (+ (* (- y py) (.sin js/Math t)))))
                bounds)
        ys (map (fn [{:keys [x y]}] 
                  (+ py 
                     (- (* (- x px) (.sin js/Math t))) 
                     (* (- y py) (.cos js/Math t))))
                bounds)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    {:pos {:x min-x :y min-y}
     :ext (let [x (+ (.abs js/Math min-x) 
                     (.abs js/Math max-x))
                y (+ (.abs js/Math min-y) 
                     (.abs js/Math max-y))
                x (if (> 50 x) 50 x)
                y (if (> 50 y) 50 y)]
            {:x x :y y})}))

(defn transformed-props [model]
  (let [morph ($morph (model :target-id))
        props (morph :props)
        abstraction-root? (instance? morph Root)]
    (assoc 
     (if (model :transform) 
       (merge props (model :transform))
       props)
     :offset (if (= :ellipse (model :type))
               (let [{:keys [x y]} (props :extent)]
                 {:x (/ x 2) :y (/ y 2)})
               {:x 0 :y 0})
     :abstraction-root? abstraction-root?)))

(defn manage-meta [{:keys [target]}]
  (fn [e]
    (.stopPropagation e)
    (if (.-altKey e)
      (let [promoted-meta (if (:root? target)
                              ($owner target)
                              ($parent target))]
        (reset! meta-focus 
             			(or promoted-meta
                    (morph-under-me 
                    ($morph (local-hand-name)))
                  )
                 ))
      (reset! meta-focus nil))))

(defcomponent halo
  transmorphic.core/IInitialize
  (initialize [self]
              {:updated-prop nil
               :scaling-mode false
               :copy-handle nil})
  transmorphic.core/IRender
  (render [{:keys [local-state] :as self} props _]
           (when (-> props :target)
             (let [copied-morph (-> local-state :updated-prop :copying)
                   morph (or copied-morph ($morph (props :target)))
                   component ($component (props :target))
                   target (or copied-morph
                              (when (-> component :reconciler :active?)
                                (get-in @universe (get-root @universe component)))
                              component
                              morph)
                   halo-position (position-in-world target)
                   bbx (compute-bounding-box ($props target))
                   params {:start-editing (:start-editing props)
                           :start-updating #(do
                                              (detach-cache! target)
                                              (rerender! self {:updated-prop %}))
                           :multiple-update (fn [props->values]
                                              (set-props! target props->values)) 
                           :update (fn [prop value]
                                     (set-prop! target prop value))
                           :stop-updating #(rerender! self {:updated-prop nil})
                           :updated-prop (-> self :local-state :updated-prop)
                           :update-target #(rerender! self {:target %})
                           :component component
                           :morph morph
                           :bbx bbx
                           :target target}]
               (rectangle {:id (str "halo-on-" ($props target :id))
                           :extent (bbx :ext)
                           :position halo-position 
                           :on-mouse-down (manage-meta params)
                           :on-key-up (fn [e]
                                        (when 
                                          (shift? e) (rerender! self {:scaling-mode false})))
                           :on-key-down (fn [e]
                                          (.stopPropagation e)
                                          (cond 
                                            (shift? e) (rerender! self {:scaling-mode true})
                                            (arrow? e) (let [old-pos ($props target :position)
                                                             new-pos (add-points old-pos
                                                                                 (case (arrow? e)
                                                                                   :up {:x 0 :y 1}
                                                                                   :left {:x -1 :y 0}
                                                                                   :right {:x 1 :y 0}
                                                                                   :down {:x 0 :y -1}))]
                                                         (set-prop! target :position new-pos))))}
                          
                          (rectangle {:id "visible-bounding-box"
                                      :extent (bbx :ext)
                                      :position (bbx :pos)
                                      :border-color (if component
                                                      "blue"
                                                      "red") 
                                      :border-width 1}
                                     (map
                                      (fn [button]
                                        (button params))
                                      [name-tag
                                       copy-button 
                                       styling-button
                                       edit-button
                                       resize-button
                                       drag-button
                                       grab-button
                                       close-button
                                       inspect-button])
                                     (viewer params))
                          (if (-> self :local-state :scaling-mode)
                            (scaling-button params) 
                            (rotate-button params))
                          (pivot-cursor params))))))

(defcomponent pivot-cursor 
  transmorphic.core/IRender
  (render [self props submorphs]
          (let [{:keys [start-updating target
                        stop-updating update]} props
                pivot (or (-> target :props :pivot-point) {:x 0 :y 0})]
            (ellipse
             {:extent {:x 10 :y 10}
              :visible (if-let [prop (props :updated-prop)]
                         (= :pivot-point (props :updated-prop))
                         true) 
              :fill "red"
              :id "pivot-cursor"
              :border-color "black"
              :opacity 0.5
              :on-mouse-down (fn [e]
                               (.stopPropagation e))
              :position pivot
              :draggable? true
              :on-drag-start 
              (fn [start-pos]
                (start-updating :pivot-point)
                (rerender! self {:pivot-point start-pos}))
              :on-drag 
              (fn [delta]
                (let [{:keys [pivot-point]} (-> self :local-state)
                      new-pivot (add-points pivot-point delta)]
                  (update :pivot-point new-pivot)
                  (rerender! self {:pivot-point new-pivot})))
              :on-drag-stop
              (fn [_]
                (stop-updating))}))))

(defcomponent viewer 
  transmorphic.core/IRender
  (render [self {:keys [updated-prop target]} _]
          (let [value (str (when updated-prop
                             (-> target :props updated-prop)))]
            (rectangle
             {:position {:x 0 :y -20}
              :visible (-> value nil? not)
              :fill "rgba(255,255,255,0.4)"
              :extent {:x (* 6 (count value)) :y 20}
              :border-radius 10}
             (text {:font-size 10,
                    :position {:x 2 :y 2}
                    :text-color "black"
                    :text-string value
                    :font-family "Chrono Web Light Italic"})))))

(defcomponent name-tag 
  transmorphic.core/IRender
  (render [self props _]
          (let [{:keys [target updated-prop]} props
                id (or (-> target :props :id) (when (:component-id target)
                                                    (-> target :abstraction :name)))
                width (* 10 (count (str id)))]
            (when id
              (rectangle
               {:position ($parent :extent 
                           #(hash-map :x (- (/ (% :x) 2) (/ width 2)) 
                                      :y (+ (% :y) 5)))
                :visible (not updated-prop)
                :fill "rgba(255,255,255,0.4)"
                :extent {:x width :y 20}
                :border-radius 10}
               (text {:position {:x 5 :y 2} 
                      :font-size 12,
                      :text-color "black"
                      :allow-input true
                      :text-string (str id)
                      :font-family "Chrono Medium Italic"}))))))

(defn start-rotating [self {:keys [target start-updating]}]
  (fn [start-pos]
    (let [pivot-point (or (-> target :props :pivot-point)
                          {:x 0 :y 0})
          rotation (or (-> target :props :rotation) 
                       (* 2 js/Math.PI))]
      (start-updating :rotation)
      (rerender! 
       self 
       {:init-polar (norm (delta start-pos pivot-point))
        :pivot-point pivot-point
        :rotation-point start-pos
        :init-rotation rotation}))))

(defn update-rotation [self {:keys [update]}]
  (fn [delta]
    (let [{:keys [rotation-point pivot-point
                  init-rotation init-polar]} (-> self :local-state)
          new-pos (add-points rotation-point delta)
          curr-polar (norm (delta new-pos rotation-point))
          angle (- init-rotation 
                   (* (.sign js/Math (cross init-polar curr-polar))
                      (.acos js/Math (dot init-polar curr-polar))))]
      (update :rotation angle)
      (rerender! self {:rotation-point new-pos}))))

(defn stop-rotating [self {:keys [stop-updating]}]
  (fn [_]
    (stop-updating)
    (rerender! self #(dissoc % 
                            :init-polar
                            :pivot-point
                            :rotation-point
                            :init-rotation))))

(defcomponent rotate-button
  transmorphic.core/IRender 
  (render [{:keys [local-state] :as self } 
           {:keys [bbx updated-prop scaling-mode] :as props} _]
          (ellipse {:fill "rgba(255,255,255,0.4)"
                    :extent {:x 25 :y 25}
                    :visible (if updated-prop
                               (= updated-prop :rotation)
                               true)
                    :on-mouse-down (fn [e]
                                     (.stopPropagation e))
                    :wants-hand-focus? true
                    :position (or
                                (-> local-state :rotation-point) 
                                (add-points (-> bbx :pos) 
                                 {:x -25 :y (- (-> bbx :ext :y) 25)})) 
                    :draggable? true
                    :on-drag-start (start-rotating self props)
                    :on-drag (update-rotation self props)
                    :on-drag-stop (stop-rotating self props)}
                   (image {:position {:x -5 :y -5} 
                           :url (if scaling-mode
                                  "/media/halos/scale.svg"
                                  "/media/halos/rotate.svg")
                           :extent {:x 15 :y 15}}))))

(defcomponent styling-button
  transmorphic.core/IRender 
  (render [self {:keys [updated-prop]} _]
          (ellipse {:fill "rgba(255,255,255,0.4)"
                    :extent {:x 25 :y 25}
                    :opacity .2
                    :visible (if updated-prop
                               (= updated-prop :styling)
                               true)
                    :on-mouse-down (fn [e]
                                     (.stopPropagation e))
                    :position ($parent :extent #(hash-map :x -25 :y (- (* (% :y) .66) 25)))}
                   (image {:position {:x -5 :y -5} 
                           :url "/media/halos/styleedit.svg" 
                           :extent {:x 15 :y 15}}))))

(defcomponent edit-button 
  transmorphic.core/IRender
  (render [self props _]
          (let [{:keys [morph updated-prop start-editing]} props
                orphan? (not (:root? morph))]
            (ellipse {:fill "rgba(255,255,255,0.4)"
                      :extent {:x 25 :y 25}
                      :visible (not updated-prop)
                      :position ($parent :extent 
                                 #(hash-map :x (% :x) :y (- (* (% :y) .33) 25)))
                      :on-mouse-down (fn [e]
                                       (start-editing morph))}
                     (image {:position {:x -5 :y -5} 
                             :url (if orphan?
                                    "/media/halos/scriptedit.svg"
                                    "/media/halos/scriptedit_script.svg") 
                             :extent {:x 15 :y 15}})))))

(defcomponent copy-button 
  transmorphic.core/IRender
  (render [self
           {:keys [updated-prop start-updating
                        stop-updating target]} _]
          (ellipse {:fill "rgba(255,255,255,0.4)"
                    :extent {:x 25 :y 25}
                    :visible (if updated-prop
                               (= :copying updated-prop)
                               true)
                    :draggable? true
                    :on-mouse-down (fn [e]
                                     (.stopPropagation e))
                    :on-drag-start (fn [start-pos]
                                     (let [copy (-> target
                                                  copy-morph!
                                                  orphanize!)]
                                       (start-updating {:copying copy})
                                       (grab-morph! copy)
                                       (rerender! self {:copied-morph copy})))
                    :on-drag-stop (fn [_]
                                    (let [{:keys [copied-morph]} 
                                          (-> self $local-state)]
                                      (stop-updating)
                                      (drop-morph! copied-morph)
                                      (reset! meta-focus copied-morph)))
                    :position ($parent :extent #(hash-map :x -25 :y (- (* (% :y) .33) 25)))}
                   (image {:position {:x -5 :y -5} 
                           :url "/media/halos/copy.svg" 
                           :extent {:x 15 :y 15}}))))

(defcomponent grab-button 
  transmorphic.core/IRender
  (render [self {:keys [updated-prop target component update-target
                        start-updating stop-updating] :as props} _]
          (ellipse {:fill "rgba(255,255,255,0.4)"
                    :id "grab-button"
                    :extent {:x 25 :y 25}
                    :visible (if updated-prop
                               (= updated-prop :grabbing)
                               true)
                    :on-mouse-down (fn [e]
                                     (.stopPropagation e))
                    :position ($parent :extent #(hash-map :x (* (% :x) .33) :y -25))
                    :draggable? true
                    :on-drag-start (fn [start-pos]
                                     (start-updating :grabbing)
                                     (if component
                                       (grab-component! component)
                                       (grab-morph! target)))
                    :on-drag-stop (fn [_] 
                                    (stop-updating)
                                    (if component
                                      (drop-component! component)
                                      (drop-morph! target)))}
                   (image {:position {:x -5 :y -5} 
                           :url "/media/halos/grabbinghand.svg" 
                           :extent {:x 15 :y 15}}))))

(defcomponent drag-button
  transmorphic.core/IRender 
  (render [self {:keys [updated-prop start-updating
                        update stop-updating target] :as props} _]
          (ellipse {:fill "rgba(255,255,255,0.4)"
                    :extent {:x 25 :y 25}
                    :id "drag-button"
                    :visible (if updated-prop
                               (= :position updated-prop)
                               true)
                    :on-mouse-down (fn [e]
                                     (.stopPropagation e))
                    :position ($parent :extent #(hash-map :x (* (% :x) .66) :y -25))
                    :draggable? true
                    :on-drag-start (fn [start-pos]
                                     (start-updating :position)
                                     (rerender! self {:prev-position (-> target :props :position)}))
                    :on-drag (fn [delta]
                               (let [{:keys [prev-position]} (-> self :local-state)
                                     new-pos (add-points prev-position delta)]
                                 (update :position new-pos)
                                 (rerender! self {:prev-position new-pos})))
                    :on-drag-stop (fn [_]
                                    (stop-updating)
                                    (rerender! self #(dissoc % :prev-position)))}
                   (image {:position {:x -5 :y -5} 
                           :url "/media/halos/move.svg" 
                           :extent {:x 15 :y 15}}))))

(defn start-resizing [self {:keys [target update
                                   start-updating]}]
  (fn [start-pos]
    (let [{:keys [pivot-point extent]} (-> target :props)]
      (start-updating :extent)
      (when-not pivot-point
        (update :pivot-point {:x 0 :y 0}))
      (rerender! self {:extent extent
                       :pivot-point (or pivot-point {:x 0 :y 0})}))))

(defn resize [self {:keys [target multiple-update morph]}]
  (fn [delta]
    (let [{:keys [extent pivot-point]} (-> self :local-state)
          {old-x :x :old-y :y} extent
          {px :x py :y} pivot-point
          new-ext (add-points extent delta)
          new-pivot {:x (* (/ px old-x) (new-ext :x))
                     :y (* (/ py old-y) (new-ext :y))}
          new-props (merge 
                     {:extent new-ext
                      :pivot-point new-pivot}
                     (when (= :text (:type morph))
                        {:font-size (* .8 (new-ext :y))}))]
      (multiple-update new-props)
      (rerender! self {:pivot-point new-pivot
                       :extent new-ext}))))

(defn stop-resizing [{:keys [stop-updating]}]
  (fn [_]
    (stop-updating)))

(defcomponent resize-button
  transmorphic.core/IRender 
  (render [self {:keys [updated-prop] :as props} _]
          (ellipse {:fill "rgba(255,255,255,0.4)"
                    :extent {:x 25 :y 25}
                    :id "resize-button"
                    :on-mouse-down (fn [e]
                                     (.stopPropagation e))
                    :visible (if updated-prop
                               (= :extent updated-prop)
                               true)
                    :position ($parent :extent 
                               #(add-points % {:x 0 :y -25}))
                    :draggable? true
                    :on-drag-start (start-resizing self props)
                    :on-drag (resize self props)
                    :on-drag-stop (stop-resizing props)}
                   (image {:position {:x -5 :y -5} 
                           :url "/media/halos/resize.svg" 
                           :extent {:x 15 :y 15}}))))

(defcomponent close-button 
  transmorphic.core/IRender
  (render [self {:keys [updated-prop target
                        stop-updating]} _]
          (ellipse {:fill "rgba(255,255,255,0.4)"
                    :extent {:x 25 :y 25}
                    :visible (not updated-prop)
                    :position ($parent :extent (fn [ext] {:x (ext :x) :y -25}))
                    :on-mouse-down (fn [e]
                                     (.stopPropagation e)
                                     (remove-morph! target)
                                     (reset! meta-focus nil)
                                     (stop-updating))}
                   (image {:id "closeImage"
                           :position {:x -5 :y -5} 
                           :url "/media/halos/close.svg" 
                           :extent {:x 15 :y 15}}))))

(defcomponent inspect-button 
  transmorphic.core/IRender
  (render [self {:keys [updated-prop target]} _]
          (ellipse {:fill "rgba(255,255,255,0.4)"
                    :extent {:x 25 :y 25}
                    :opacity 0.2
                    :on-mouse-down (fn [e]
                                     (.stopPropagation e)
                                     (swap! inspected-morphs conj (get-ref target)))
                    :visible (not updated-prop)
                    :position ($parent :extent (fn [v] {:x (v :x) :y (- (* (v :y) .66) 25) }))}
                   (image {:id "infoImage"
                           :position {:x -5 :y -5} 
                           :url "/media/halos/info.svg" 
                           :extent {:x 15 :y 15}}))))