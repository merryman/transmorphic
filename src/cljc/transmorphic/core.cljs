(ns transmorphic.core
  (:require-macros [transmorphic.core :refer [defmorph defcomponent]]
                   [cljs.analyzer.macros :refer [no-warn]])
  (:require [om.core]
            [goog.dom :as gdom]
            [om.dom :as dom]
            [om.core :as om]
            [transmorphic.repl :refer [update-ns-source! morph-eval-str]]
            [transmorphic.event :as event :refer [extract-event-handlers
                                                  clean-handlers!]]
            [transmorphic.symbolic :refer [get-description]]
            [clojure.string :refer [join split]]
            [clojure.set :refer [union]]
            [devtools.core :as devtools]))

(enable-console-print!)

(devtools/enable-feature! :sanity-hints :dirac)
(devtools/install!)

(def meta-focus (atom #{}))

(declare rerender! insert-component!)

(def component-migration-queue (atom {}))
(def morph-migration-queue (atom {}))

(defprotocol IMorph?
  (morph? [x]))

(defprotocol IComponent?
  (component? [x]))

(defn reload-hook [args]
  (doseq [[morph {:keys [editor new-component-name]}] @morph-migration-queue]
    (no-warn ; find a way for the programmer to control the current props passed to the component
            (let [comp-fn (morph-eval-str new-component-name)
                  component (comp-fn {})]
              ; update the owner of the morph
              ; replace the morph with the component
              (insert-component! morph component)))
    (prn "Morph -> Component " (:morph-id morph))
    (rerender! editor {:compiling? false
                       :locked? false
                       :ns-source nil}))
  (reset! morph-migration-queue {})
  
  (doseq [[component editor-component] @component-migration-queue]
    (rerender! editor-component {:compiling? false
                                 :locked? false
                                 :ns-source nil})
    (prn "Wake up editor " (:component-id component)))
  (reset! component-migration-queue {})) 

(declare morphic-read morphic-mutate universe render-morph)

(defn morphic-uuid [morph]
  (swap! universe update-in [:meta-data :uuid-state (morph :type)] inc)
  (let [uuid-state (get-in @universe [:meta-data :uuid-state])] 
    (case (morph :type)
      :rectangle (str "rectangle-" (uuid-state :rectangle))
      :ellipse (str "ellipse-" (uuid-state :ellipse))
      :text (str "text-" (uuid-state :text))
      :polygon (str "polygon-" (uuid-state :polygon))
      :io (str "io-" (uuid-state :io))
      :image (str "image-" (uuid-state :image)))))

(defn filepath->ns [path]
  ; still not plattform independent
  (when path
    (let [file (-> path
                 (clojure.string/replace "/Users/robin/Development/cljs-morphic/" "")
                 (clojure.string/replace "src/cljs/" "")
                 (clojure.string/replace "test/" ""))
          lib-name (subs (clojure.string/replace file "/" ".")
                         0 (- (count file) 5))]
      (symbol (demunge lib-name)))))

(defn abstraction->ns [abstraction & {:keys [ns path]}]
  (or (get-in @universe [:meta-data :abstractions abstraction])
      (or ns (filepath->ns path))))

(defn get-abstraction-info [fn-name]
  (get-in @universe [:meta-data :abstractions fn-name]))

; DOM MAPPING

(def pi js/Math.PI)

(defn morph-class [props]
  (or (:css-class props) "Morph"))

(defn morph-style [props]
  (apply merge 
      (map (fn [[prop value]]               
             (case prop 
                :scrollable? {:overflow "scroll"} 
               :position {:position "absolute" 
                          :left (:x value)
                          :top (:y value)}
                ;; more to come... 
                nil)) props)))

(defn html-attributes [props ident]
  (clj->js 
    (merge
     (extract-event-handlers ident props) 
     {:key (:id props)
      :style (assoc (morph-style props) :padding 0)
      :class-name (morph-class props)})))

(def html5TransformProperty :WebkitTransform)
(def html5TransformOriginProperty :WebkitTransformOrigin)

(defn shape-style [props]
  (apply merge 
    (map (fn [[prop value]]               
           (case prop
             (:scale :rotation :pivot-point)
             (let [{rotation :rotation, scale :scale, pivot :pivot-point, 
                    :or {rotation 0, scale 1, pivot {:x 0, :y 0}}} props]
               {html5TransformProperty (str "rotate(" 
                                            (mod (/ (* rotation 180) pi) 360) 
                                            "deg) scale(" scale "," scale ")")
                html5TransformOriginProperty (str (:x pivot) "px " (:y pivot) "px")})
             :visible {:visibility (if value "visible" "hidden")}
             :extent {:height (:y value)
                      :width (:x value)}
             :border-width {:borderWidth value
                            :borderStyle "solid"}
             :border-color {:borderColor value
                            :borderStyle "solid"}
             :fill {:background value}
             :drop-shadow?
             (when value {:boxShadow "0 18px 40px 10px rgba(0, 0, 0, 0.36)" 
                          :WebkitTransition "box-shadow 0.5s"})
             :css value
             :border-radius {:borderRadius value}
             :opacity {:opacity value}
             :border-style {:borderStyle value}
             nil)) props)))

; RENDERING

(defprotocol IInitialize
  (initialize [self]))

(defprotocol IFinialize
  (finalize [self]))

(defprotocol IRefresh
  (refresh [self props]))

(defprotocol IDidMount
  (did-mount [self]))

(defprotocol IWillReceiveProps
  (will-receive-props [self next-props]))

(defprotocol IRender
  (render [self props submorphs]))

(defn eval-suspended-props [world-state morph]
  (assoc morph :props
         (into {} 
               (map (fn [[p v]]
                      (cond 
                        (-> v :dynamic?)
                        [p (v :value)]
                        (-> v :relative?)
                        [p ((v :eval) world-state morph)]
                        :default [p v])))
               (-> morph :props))))

(defn render-morph [morph]
  (reify
    om/IDidMount
    (did-mount [self]
               (when-let [did-mount (-> morph :props :did-mount)]
                 (did-mount self)))
    om/IWillReceiveProps
    (will-receive-props [self next-props]
               (when-let [recv-props (-> morph :props :will-receive-props)]
                 (recv-props self)))
    om/IRender
    (render [_]
          (if morph
            ((-> morph :morph->html) (eval-suspended-props @universe morph))
            (dom/div #js {:id "dummy-root"})))))

(defmorph div 
  [{:keys [type props submorphs]}]
  (apply dom/div #js {:style (shape-style props)}
    (om/build-all render-morph submorphs)))

(defmorph rectangle 
  [{:keys [morph-id type props submorphs]}]
  (dom/div (html-attributes props morph-id)
    (apply dom/div (clj->js {:style (shape-style props)})
      (om/build-all render-morph submorphs))))

(defmorph ellipse 
  [{:keys [morph-id type props submorphs]}]
  (dom/div (html-attributes props morph-id)
    (let [offset (props :extent)
          x-offset (str (/ (:x offset) 2) "px")
          y-offset (str (/ (:y offset) 2) "px")
          style (shape-style props)]
      ; shape
      (dom/div (clj->js {:style (assoc style 
                                        :borderRadius 
                                        (str (-> props :extent :x) "px /" 
                                             (-> props :extent :y) "px")) })
        (apply dom/div  (clj->js {:style {:position "absolute",
                                          :top y-offset,
                                          :left x-offset,
                                          :marginTop "-2px",
                                          :marginLeft "-2px"}})
          (om/build-all render-morph submorphs))))))

(defmorph image
  [{:keys [morph-id type props submorphs]}]
  (apply dom/div (html-attributes props morph-id) 
    (dom/div nil
      (dom/img  (clj->js {:style (shape-style props)
                          :src (props :url)})))
        (om/build-all render-morph submorphs)))

(defn text-style [props]
  (clj->js 
    (apply merge
      (map (fn [[prop value]]               
             (if (= prop :allow-input)
               (when (false? value) {:WebkitUserSelect "none"})
               (case prop 
                  :font-family {:fontFamily value}
                  :font-size {:fontSize (str value "pt")}
                  :max-text-height {:maxHeight (str value "px")}
                  :max-text-width {:maxWidth (str value "px")}
                  :min-text-height {:minHeight (str value "px")}
                  :min-text-width {:minWidth (str value "px")}
                  :text-color {:color value}
                  nil))) props))))

(defmorph text
  [{:keys [morph-id type props submorphs]}]
  (dom/div (html-attributes props morph-id)
    (apply dom/div 
      (clj->js {:style (assoc (shape-style props) :cursor "default")
                :className "Morph Text"}) 
      (dom/span  (clj->js {:className "visible Selection"
                           ; :content-editable (:allow-input props)
                           :style (text-style props)
                           :ref "myInput"})
        (:text-string props))    
      (om/build-all render-morph submorphs))))

(defn render-io 
  [{:keys [morph-id props submorphs]} owner]
  (reify
    om.core/IDidMount
    (did-mount [this]
               ((:did-mount props) this))
    om.core/IRender
    (render [this] ((:render props)))))

(defmorph io
  [args]
  (om/build render-io args))

(defmorph html
  [{:keys [morph-id props submorphs]} owner]
  (dom/div (html-attributes props morph-id)
    (apply dom/div (clj->js {:style (shape-style props)})
      (:html props)
    (om/build-all render-morph submorphs))))

(defmorph ace
  [{:keys [morph-id props submorphs]} owner]
  (dom/div (html-attributes props morph-id)
    (apply dom/div (clj->js {:style (shape-style props)})
      (dom/div
        (clj->js {:id (props :ace-id) 
                  :style {:height "100%" 
                          :width "100%"} 
                  :className "ace"}))
      (om/build-all render-morph submorphs))))

(defmorph listmorph
  [{:keys [morph-id type props submorphs]}]
  (dom/ul (html-attributes props morph-id)
    (for [[idx submorph] submorphs]
      (dom/li #js {:key idx
                   :style {:listStyleType "none"}}
        (om/build render-morph submorph)))))

(defmorph checkbox
  [{:keys [morph-id type props submorphs]}]
  (dom/div (html-attributes props morph-id)
    (apply dom/div 
      (clj->js {:style (shape-style props)}) 
      (dom/input
        #js {:className "toggle" :type "checkbox"
             :checked (:checked? props) 
             :onChange (:on-change props)})    
      (om/build-all render-morph submorphs))))

; (defmorph polygon) TBD

(def history-cache (atom []))

(defn update-dynamic-props! [morph cursor-pos]
  (doseq [[k v] (-> morph :props)]
    (when (-> v :dynamic?)
      ((v :update) morph k cursor-pos))))

(declare render-component* get-morph-tree store-morph!)

(defn expand-component [state component]
  (update-in component 
             [:submorphs]
             (fn [submorphs]
               (mapv #(get-morph-tree state %) submorphs))))

(defn- refresh-root [state]
  (clean-handlers!)
  (let [component (get state :root-component)
        ; {:keys [abstraction source-location parent owner props]} component
        state-tracker (atom state) ; TODO: only preserve the morphs, that are structurally altered
        morph-tree (render-component* component state-tracker nil)
        new-state (assoc @state-tracker :morph-tree morph-tree)]
    (swap! history-cache conj new-state)
    new-state))

(defn wrap-as-derived [morph]
  (fn [new-parent _ new-idx state]
    (let [local-morph-id (str (second new-parent) "." 
                              (or (-> morph :props :id) new-idx))
          morph (assoc morph 
                       :morph-id local-morph-id
                       :parent new-parent
                       :owner (if (= :component/by-id (-> morph :owner first))
                                (get-in @state (:owner morph))
                                (:owner morph))
                       :prototype [:morph/by-id (-> morph :morph-id)]
                       :submorphs (mapv (fn [idx wrap-fn]
                                          (wrap-fn [:morph/by-id local-morph-id] nil idx state))
                                        (range) (into [] 
                                                      (comp
                                                       (map #(if (= :morph/by-id (-> % first))
                                                               (get-in @state %)
                                                               %))
                                                       (map wrap-as-derived))
                                                      (-> morph :submorphs))))]
      (store-morph! state morph)
      morph)))

(declare wrap-component)

(defn submorphs->flatten 
  ([submorphs]
   (submorphs->flatten submorphs nil))
  ([submorphs idx?]
   (letfn [(reduce-morphs [morphs [idx x]]
                          (let [idx (if (and idx? idx)
                                      (str idx? "." idx)
                                      idx)]
                            (cond
                              (or (seq? x) (vector? x)) 
                              (into [] (concat morphs (submorphs->flatten x idx)))
                              (fn? x) 
                              (conj morphs (fn [parent owner _ state]
                                             (x parent owner idx state)))
                              (satisfies? IComponent? x)
                              (conj morphs (fn [parent owner _ state]
                                             ((wrap-component x nil) parent owner idx state)))
                              :default morphs)))]
     (into [] (reduce reduce-morphs [] (zipmap (range) submorphs))))))

(defn unwrap-submorphs 
  ([submorphs & {:keys [parent owner state idx]}]
   (into []
         (comp
          (map (fn [morph-fn]
                 (morph-fn parent owner nil state)))
          (remove nil?))
         (submorphs->flatten submorphs idx))))

(defn store-morph! [state morph redefined?]
  (swap! state assoc-in [:morph/by-id (-> morph :morph-id)] 
         (-> morph
           (update-in 
            [:owner] 
            (fn [c]
              (when c [:component/by-id (-> c :component-id)])))
           (update-in 
            [:submorphs]
            #(mapv (fn [s]
                     (when s [:morph/by-id (-> s :morph-id)])) %))))
  morph)

(defn store-component! [state component redefined?]
  ; rescue local state at any rate!
  (let [id (-> component :component-id)
        {:keys [local-state reconciler]} (get-in @state [:component/by-id id])
        component (assoc component 
                         :local-state (or local-state (:local-state component))
                         :reconciler (or (and (not redefined?) reconciler) 
                                         (:reconciler component)))]
    ; (when redefined?
    ;   (prn "redef taken into account..")
    ;   ; (swap! transmorphic.repl/component-migration-queue dissoc id)
    ;   )
    (swap! state assoc-in [:component/by-id (-> component :component-id)] 
           (-> component
             (update-in 
              [:owner] 
              (fn [c]
                (when c [:component/by-id (-> c :component-id)])))
             (update-in 
              [:submorphs]
              #(mapv (fn [s]
                       (when s [:morph/by-id (-> s :morph-id)])) %))))
    component))

(defn get-morph-tree [state morph-ref]
  ; this is needed, since we may have components down
  ; the hierarchy which need to be re-expanded
  (let [morph (get-in @state morph-ref)]
    (update-in morph 
               [:submorphs]
               (fn [submorphs]
                 (mapv (fn [m]
                         (get-morph-tree state m))
                        submorphs)))))

(defn align-with-stored [x stored owner state]
  (let [{:keys [txs source-location parent]} stored
        {:keys [reconciler]} owner
        {:keys [props added removed]} txs
        align (fn [x props added removed]
                (let [align-submorph (fn [submorphs morph-id]
                                       (if (-> removed (contains? morph-id) not) 
                                         (conj submorphs (get-morph-tree state [:morph/by-id morph-id]))
                                         submorphs))
                      aligned-props (merge-with (fn [inline stored]
                                                  (if (-> inline :dynamic?)
                                                    (merge inline (dissoc stored :update))
                                                    stored)) 
                                                (:props x) props)
                      aligned-submorphs (reduce align-submorph (or (:submorphs x) []) added)]
                  (assoc x
                         :parent parent 
                         :txs txs
                         :props aligned-props
                         :submorphs aligned-submorphs)))]
    (if (:active? reconciler)
      (let [{:keys [props added removed]} 
            (get-in reconciler [source-location :txs])]
        (align x props added removed))
      (align x props added removed))))

(defn consolidate [x store-fn key prefix state]
  (let [id (-> x key)
        stored (get-in @state [prefix id]) 
        owner (if stored 
                (get-in @state (:owner stored))
                (:owner x))
        redefined? (contains? @component-migration-queue id)
        removed? (and stored (not= (:parent x) (:parent stored)))
        x (when-not removed?
            (assoc x :submorphs
                   (unwrap-submorphs (:submorphs x) 
                                     :parent [prefix id]
                                     :owner owner
                                     :state state)))]
    (when x
      (if (and stored (not redefined?))
        (when-not removed? 
          (let [x (align-with-stored x stored owner state)]
            (store-fn state x redefined?)))
        (store-fn state x redefined?)))))

(defn consolidate-component [state component]
  (consolidate component store-component! 
               :component-id :component/by-id state))

(defn consolidate-morph [state morph]
  (consolidate morph store-morph! 
               :morph-id :morph/by-id state))

(defn render-component* [self state idx]
  (let [{:keys [local-state props submorphs parent component-id]} self
        _ (when (and (satisfies? IRefresh self) 
                     (:local-state (get-in @state [:component/by-id component-id])))
            (refresh self props))
        self (if-let [init! (and (not local-state) 
                                 (satisfies? IInitialize self))]
               (assoc self :local-state (initialize self))
               self)
        mfn (transmorphic.core/render self props (map wrap-as-derived submorphs))
        morph (when mfn (mfn parent self idx state :root? true))]
    morph))

(defn wrap-component [component loc]
  (fn [parent owner idx state & {:keys [root?]}]
    (let [component-id (->> [(second parent) (or (-> component :props :id) idx)]
                         (remove nil?)
                         (join "."))
          component (consolidate-component
                     state 
                     (assoc component
                            :component-id component-id
                            :parent parent
                            :owner owner
                            :txs {:props {}
                                  :removed #{}
                                  :added []}
                            :source-location loc))]
      (when component
        (render-component* component state idx)))))

(defn wrap-morph [morph loc]
  (fn [parent owner idx state & {:keys [root?]}]
    (let [morph-id (->> [(second parent) (or (-> morph :props :id) idx)]
                     (remove nil?)
                     (join "."))
          morph (consolidate-morph
                 state
                 (assoc morph 
                        :morph-id morph-id 
                        :parent parent
                        :owner owner
                        :txs {:props {}
                              :added []
                              :removed #{}}
                        :root? root?
                        :source-location loc))]
      morph)))

(defn update-reconciler 
  [state ref-to-changed]
  (let [{:keys [source-location owner txs]} (get-in state ref-to-changed) 
        reconciler (-> state
                     (get-in owner)
                     :reconciler
                     (update-in [source-location :txs] merge txs))]
    (if owner
      (update-in state owner assoc :reconciler reconciler)
      state)))

(defn ensure 
  "Ensures that the reference is
   actually valid, replacing it
   in case a redirect is referenced."
  [state ref]
  (loop [ref ref]
    (when-let [x (get-in state ref)] 
      (if-let [redirect (:redirect x)]
        (recur redirect)
        ref))))

(defn become 
  "Inserts a redirect into the
   identity lookup table at ref-a, that
   immediately dispatches to the
   identity ref-b"
  [state ref-a ref-b]
  (assoc-in state ref-a {:redirect ref-b}))

(defn set-props [state {:keys [ref props->values]}]
  (let [ref (ensure state ref)
        x (get-in state ref)
        prototype (-> x :prototype) 
        state (if prototype
                (set-props state {:ref prototype
                                  :props->values props->values})
                (-> state
                  (update-in ref update-in 
                             [:txs :props] merge props->values)
                  (update-in ref update-in 
                             [:props] merge props->values)))]
    (update-reconciler state ref)))

(defn set-prop [state {:keys [ref prop value]}]
  (set-props state {:ref ref :props->values {prop value}}))

(defn remove-morph [state {:keys [ref]}]
  (let [ref (ensure state ref)
        remove #(-> %
                  (update-in [:txs :removed] 
                             conj (second ref))
                  (update-in [:txs :added] 
                             (fn [added]
                               (into []
                                     (remove (fn [x] 
                                               (= x (second ref))) 
                                             added))))
                  (update-in [:submorphs] 
                             (fn [sub-refs]
                               (into []
                                     (remove (fn [x] (= x ref)) 
                                             sub-refs)))))
        to-be-removed (get-in state ref)
        prototype (-> to-be-removed :prototype)]
    (if prototype
      (-> (remove-morph state {:ref prototype})
        (become ref prototype))
      (-> state
        (update-in (-> to-be-removed :parent) remove)
        (update-in ref merge {:parent nil
                              :owner (when (:root? (get-in state ref))
                                       (:owner (get-in state ref)))})
        (update-reconciler (-> to-be-removed :parent))))))

(defn add-morph [state {:keys [ref new-parent-ref]}]
  (let [ref (ensure state ref)
        new-parent-ref (ensure state new-parent-ref)
        add #(-> %
               (update-in [:txs :added] conj (second ref))
               (update-in [:txs :removed] disj (second ref))
               (update-in [:submorphs] conj ref))
        change-parent #(assoc % :parent new-parent-ref)
        parent-prototype (-> state 
                           (get-in new-parent-ref)
                           :prototype)]
    (if parent-prototype
      (add-morph state {:new-parent-ref parent-prototype
                        :ref ref})
      (-> state
        (update-in new-parent-ref add)
        (update-in ref change-parent)
        (update-reconciler new-parent-ref)))))

(defn orphanize [state {:keys [ref]}]
  (update-in state ref 
             (fn [morph]
               (let [morph (dissoc morph :owner :prototype)
                     props (reduce-kv 
                            (fn [morph prop value]
                              (if (fn? value)
                                (dissoc morph prop)
                                morph))
                            morph (:props morph))]
                 (assoc morph :props props)))))

(defn- copy-morph [state {:keys [ref new-morph-id new-id]}]
  (assoc-in state [:morph/by-id new-morph-id] 
            (-> (get-in state ref)
              (assoc :morph-id new-morph-id)
              (assoc-in [:props :id] new-id))))

(defn- copy-component [state {:keys [ref new-component-id new-id]}]
  (assoc-in state [:component/by-id new-component-id] 
            (-> (get-in state ref)
              (assoc :component-id new-component-id)
              (assoc-in [:props :id] new-id))))

(defn- move-morph [state {:keys [ref new-parent-ref]}]
  (let [ref (ensure state ref)
        new-parent-ref (ensure state new-parent-ref)]
    (-> state
      (remove-morph {:ref ref})
      (add-morph {:new-parent-ref new-parent-ref 
                  :ref ref}))))

(defn set-root 
  [universe root-component]
  (let [state (atom universe)
        morph-tree ((wrap-component root-component nil) nil nil nil state)]
    (assoc @state 
           :morph-tree morph-tree
           :root-component (loop [owner (-> morph-tree :owner)]
                             (if (:owner owner)
                               (recur (:owner owner))
                               owner)))))

(defn update-abstraction! [editor component {:keys [ns name new-source]}]
  (swap! component-migration-queue
         assoc (:component-id component) editor)
  (update-ns-source! ns new-source
   (fn [_] (prn "Updated: " ns "/" name))))

(defn create-abstraction! [editor morph {:keys [ns name new-source]}]
  ; if a new abstraction is created, we spawn and attach a dummy
  ; component as the new owner to the current morph
  ; the morph is then replaced by a component, that
  ; is now being evaluated each time the initial
  ; morph would have been rendered.
  (swap! morph-migration-queue assoc (:morph-id morph) 
         {:editor editor
          :new-component-name (str ns "/" name)})
  (update-ns-source! ns new-source
   (fn [_] (prn "Created: " ns "/" name))))

(defn update-component [state {:keys [component-id new-local-state]}]
  (assoc-in state [:component/by-id component-id :local-state] new-local-state))

; PUBLIC TRANSFORMATION API

(defn get-ref [x]
  (ensure @universe 
          (cond
            (:morph-id x) [:morph/by-id (:morph-id x)]
            (:component-id x) [:component/by-id (:component-id x)]
            :else (throw (str "Can not transform " x)))))

(defn update-component! [component-id new-state]
  (swap! universe
         update-component
         {:component-id component-id
          :new-local-state new-state}))

(defn remove-component! [component-id]
  (om/transact! universe :component/by-id 
                #(dissoc % component-id)))

(defn rerender! [self val]
  (let [id (-> self :component-id)
        state (get-in @universe 
                      [:component/by-id id :local-state])]
    (update-component! 
     id 
     (if (fn? val)
       (val state)
       (merge state val)))))

(defn set-prop! [x prop-name prop-value]
  (swap! 
   universe
   (comp refresh-root set-prop) 
   {:ref (get-ref x)
    :prop prop-name 
    :value prop-value})
  (get-in @universe (get-ref x)))

(defn set-props! [x props->values]
  (swap! 
   universe
   (comp refresh-root set-props) 
   {:ref (get-ref x)
    :props->values props->values})
  (get-in @universe (get-ref x)))

(defn refresh-scene! []
  (swap! universe refresh-root)
  true)

(defn revert-to-state! [state-idx ref-to-preserve]
  (when-let [entry (get @history-cache state-idx)]
    (reset! transmorphic.event/step-cbs {})
    (reset! universe 
            (assoc-in entry ref-to-preserve 
                      (get-in @universe ref-to-preserve)))))

; moves morphs as well as components!
; parents can be either morphs or components.
; note, that a morph or component, whos parent is
; a component is always a prototype and will therefore
; never be rendered directly in the scene.
(defn move-morph! [x new-parent]
  (swap! 
     universe
     (comp refresh-root move-morph)
     {:ref (get-ref x)
      :new-parent-ref (get-ref new-parent)})
  (get-in @universe (get-ref x)))

(defn orphanize! [morph]
  (swap! universe orphanize {:ref (get-ref morph)})
  (get-in @universe (get-ref morph)))

(defn remove-morph! [x]
  (swap! 
   universe 
   (comp refresh-root remove-morph)
   {:ref (get-ref x)}))

(defn uuid-from 
  [morph-id]
  (let [splitted-morph-id (split morph-id #"\.")
        id-prop (str (-> splitted-morph-id last) "-copy")]
    {:uuid (join "." (concat (butlast splitted-morph-id) (list id-prop)))
     :id id-prop}))

(defn copy-morph! [morph]
  (let [morph-id (-> morph :morph-id)
        {:keys [uuid id]} (uuid-from morph-id)]
    (swap! 
     universe 
     copy-morph
     {:ref (get-ref morph)
      :new-morph-id uuid
      :new-id id})
    (get-in @universe [:morph/by-id uuid])))

(defn insert-component! 
  "1. Insert a component into the universe.
   2. Replace the morph with the component.
  This is easy, if the morph was added previously.
  What happens if an innate morph is replaced?
  Remoing and adding would alter the positon of the morph.
  But a replace tx does not exist. Can be built by removing all morphs
  and adding them again in the order they appeard in initially.
  
   When the component is unwrapped and re-evaluated, the
  morph is replaced by the newly rendered root-morph of
  the insered component. The component, needs to transfer
  the source location from the replaced morph and also its owner.
  This is to guarantee that the reconciliation of the owner
  will now display the call to the component instead
  of the morph that used to be there previously.
  3. The new morph hierarchy, that the component yields,
  will be equivalent but not identical. Therefore, 
  tools such as a component editor, need to re-focus their
  target to the yielded root-morph."
  [morph component]
  (swap! universe update-in (get-ref morph) assoc :submorphs map ))

(defn toggle-reconciler! [component]
  (let [component-id (:component-id component)]
    (swap! universe update-in 
           [:component/by-id component-id :reconciler :active?] not)))

(def current-namespace 'cljs.user)

(defn set-root! [app-state component target]
  (om/root  
   (fn [data _]
     (reify 
       om/IRender
       (render [self]
               (if-let [tree (not-empty (:morph-tree data))]
                 (om/build render-morph tree {:key :morph-id})
                 (dom/div #js {:id "dummy-root"})))))
   app-state {:target target})
  (swap! app-state set-root component))

(defonce universe (atom {:morph-tree {}
                         :abstraction/by-name {} ; ns.name -> abstraction data
                         :morph/by-id {}
                         :component/by-id {}}))