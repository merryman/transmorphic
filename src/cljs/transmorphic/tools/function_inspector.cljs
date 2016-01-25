(ns transmorphic.tools.function-inspector
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [transmorphic.core :refer [defcomponent]])
  (:require [transmorphic.morph :refer [$morph position-in-world 
                                        parent centered-in]]
            [transmorphic.symbolic :refer [format-code]]
            [transmorphic.manipulation :refer [changes-on-morph track-changes! revert-tx!
                                               add-evolvement!]]
            [transmorphic.core :refer [io ellipse rectangle text image listmorph
                                       rerender! IRender]]
            [transmorphic.event :refer [get-client-pos get-cursor-pos]]
            [transmorphic.utils :refer [add-points]]
            [transmorphic.tools.window :refer [window window-resizer]]
            [transmorphic.tools.ace :refer [paredit set-token-selection! set-token-value!
                                            get-numeric-token-at save-handler]]
            [transmorphic.repl :refer [morph-eval morph-eval-str read-objs 
                                       load-and-callback!]]
            [clojure.string :refer [join split-lines]]
            [cljs.core.async :as async :refer [>! <! put! chan timeout onto-chan]]
            [cljs.pprint :refer [pprint code-dispatch *print-right-margin*]]))

(def inspected-morphs (atom #{}))

; SYMBOLIC -> SPACIAL MAPPING

(def scrubber-pane-state (atom {:active? false
                                  :scrub-pos nil
                                  :callback nil
                                  :scrub-value nil}))

(defn activate-scrubber-pane [scrub-pos scrub-value callback]
  (reset! scrubber-pane-state {:active? true
                               :scrub-pos scrub-pos
                               :scrub-value scrub-value
                               :callback callback}))

(defcomponent scrubber-pane 
  IRender
  (render [self props submorphs]
          (let [{:keys [scrub-pos scrub-value active? callback]} props] 
            (rectangle {:visible active?
                        :fill "blue" :opacity 0.1 :extent (-> "world" $morph :props :extent)
                        :on-mouse-up (fn [e]
                                       (reset! scrubber-pane-state {active? false}))
                        :on-mouse-move (fn [e]
                                         (let [cursor-pos (get-cursor-pos e)
                                               client-pos (get-client-pos e)
                                               scrubbed-value (+ (int scrub-value) 
                                                                 (*
                                                                  (let [dy (- (:y scrub-pos) (:y scrub-pos))]
                                                                    (cond
                                                                      (> -100 dy) .1
                                                                      (> dy 100) 10
                                                                      :default 1)) 
                                                                  (- (:x cursor-pos) (:x scrub-pos))))]
                                           (callback scrubbed-value)))}))))

; EDITOR MORPH

(defn prepare-scrubbing [model ace-instance client-pos]
  (let [token (get-numeric-token-at ace-instance client-pos)]
    (set-token-selection! ace-instance token)
    (activate-scrubber-pane  
     (-> model :scrubbing :start) 
     (-> model :token :value)
     #(do
        (set-token-selection! ace-instance token) ; select the are
        (set-token-value! ace-instance token %) ; set the editor value
        ; (save-handler ace-instance (model :ace)) ; flush for immediate effect
        ; (set-value! ace-instance %)
        ))
    (assoc model :scrubbing {:start client-pos})))

; INSPECTING

(declare inspecting inspection-active)

(defcomponent loop-mapping-button 
  IRender
  (render [self model position]
  (let [coloring (if (-> model :deep-changes) "lightgreen" "grey")] 
    (rectangle {:inspectable? true
                :border-color coloring
                :border-width 3
                :position position
                :extent {:x 100 :y 20}
                :border-radius 5
                :on-mouse-down (fn [e]
                                 (.stopPropagation e)
                                 (rerender! self (update-in model [:deep-changes] not)))}
               (text {:id "map-button-label"
                      :position {:x 5 :y 5}
                      :text-color coloring
                      :font-size 10
                      :font-family "Chrono Medium Italic"
                                            :text-string "Loop Mapping"})))))

(defcomponent morph-mapping-button 
  IRender
  (render [self model position]
          (let [coloring (if (-> model :source-mapping) "orange" "grey")]
            (rectangle {:inspectable? true
                        :border-color coloring
                        :border-width 3
                        :position position
                        :extent {:x 100 :y 20}
                        :border-radius 5
                        :on-mouse-down (fn [e]
                                         (.stopPropagation e)
                                         (rerender! self (update-in model [:source-mapping] not)))}
                       (text {:id "morph-button-label"
                              :position {:x 5 :y 5}
                              :text-color coloring
                              :font-size 10
                              :font-family "Chrono Medium Italic"
                              :text-string "Morph Mapping"})))))

(defcomponent function-inspector 
  IRender
  (render [self model submorphs]
          (window {:scrollable? true
                   :on-close (fn [] 
                               (swap! inspected-morphs disj (model :target-ref)))
                   :on-resize (fn [new-extent])
                   :submorphs 
                   (let [txs (changes-on-morph (model :target-ref))
                         model-atom (-> model :target-ref $morph 
                                      :model meta :model-atom) 
                         hl (fn [v]
                              (text {:text-string v
                                     :font-size 12
                                     :text-color "black"
                                     :font-family "Chrono Medium Italic"
                                     }))
                         entry (fn [tx tx-ref]
                                 (text
                                  {:font-size 12,
                                   :text-color "grey",
                                   :text-string tx, 
                                   :font-family "Chrono Medium Italic"
                                   :on-mouse-down (fn [e]
                                                    (let [tx-path (concat (model :target-ref) tx-ref)]
                                                      (revert-tx! tx-ref)))}))]
                     [(paredit (assoc (model :ace)
                                  :position {:x 0 :y 0}
                                  :id (str "model-view-" (model :target-id))
                                  :value (if model-atom
                                           (format-code @model-atom)
                                           "Orphaned Morph!")
                                  :line-numbers? false
                                  :edited? (atom false)
                                  :extent {:x 400 :y 200}
                                  :on-save (fn [new-model]
                                             (swap! model-atom #(with-meta (morph-eval-str new-model) 
                                                                  (meta %)))))
                           "ace-inspector")
                      (listmorph {:id "structure"
                                  :position {:x 10 :y 210}}
                                 (hl "Property Changes:")
                                 (map (fn [[prop value]]
                                        (entry (str prop " " value) [:prop-changes prop]))
                                      (:prop-changes txs))
                                 (hl "Structural Changes:")
                                 (map (fn [i tx]
                                        (entry (str (tx :op) " " (tx :idx)) [:structure i]))
                                      (range) (:structure txs)))])})))

(defn inspect-function! [lens-ref]
  (function-inspector 
    {:title "Function Inspector"
     :position (centered-in ($morph "world") {:x 600 :y 600})
     :extent {:x 400 :y 400}
     :target-ref lens-ref
     :target-id (-> lens-ref $morph :morph-id)
     :ace {}
     } (str "inspector-on-" lens-ref)))