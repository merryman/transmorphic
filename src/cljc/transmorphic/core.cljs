(ns transmorphic.core
  (:require-macros [transmorphic.core :refer [defmorph defcomponent]])
  (:require [om.core]
            [goog.dom :as gdom]
            [om.dom :as dom]
            [om.core :as om]
            [transmorphic.event :as event :refer [extract-event-handlers]]
            [transmorphic.symbolic :refer [get-description]]
            [clojure.string :refer [join split]]
            [clojure.set :refer [union]]
            [devtools.core :as devtools]))

(enable-console-print!)

(devtools/enable-feature! :sanity-hints :dirac) ; enables additional features, :custom-formatters is enabled by default
(devtools/install!)

; the local state, that stores which
; of the morphs are focused through the
; meta toolchain (halo and hand)
; it is explicitly out of the om next db,
; since its information is never shared
; with other users but always relative to the
; local user, even when real time collaboraton
; is happening
(def meta-focus (atom #{}))

; the universe stores all information that will ever
; be important for morphic.
; This will also be the information that is required to
; reliably synchronize state among participating clients

; Future work would be to experiment with distributed dbs
; to not rely on a central server to 

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
  (let [component (get state :root-component)
        ; {:keys [abstraction source-location parent owner props]} component
        state-tracker (atom state) ; TODO: only preserve the morphs, that are structurally altered
        morph-tree (render-component* component state-tracker nil)]
    (assoc @state-tracker :morph-tree morph-tree)))

(defn wrap-as-derived [morph]
  (fn [new-parent _ new-idx state]
    (let [local-morph-id (str (second new-parent) "." 
                              (or (-> morph :props :id) new-idx))
          morph (assoc morph 
                       :morph-id local-morph-id
                       :parent new-parent
                       :prototype [:morph/by-id (-> morph :morph-id)]
                       :submorphs (mapv (fn [idx wrap-fn]
                                          (wrap-fn [:morph/by-id local-morph-id] nil idx state))
                                        (range) (map wrap-as-derived (-> morph :submorphs))))]
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
                              (seq? x) 
                              (into [] (concat morphs (submorphs->flatten x idx)))
                              (fn? x) 
                              (conj morphs (fn [parent owner _ state]
                                             (x parent owner idx state)))
                              (contains? x :component-id)
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

(defn store-morph! [state morph]
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

(defn store-component! [state component]
  ; rescue local state at any rate!
  (let [id (-> component :component-id)
        {:keys [local-state reconciler]} (get-in @state [:component/by-id id])
        component (assoc component 
                         :local-state (or local-state (:local-state component))
                         :reconciler (or reconciler (:reconciler component)))]
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
        removed? (and stored (not= (:parent x) (:parent stored)))
        x (when-not removed?
            (assoc x :submorphs
                   (unwrap-submorphs (:submorphs x) 
                                     :parent [prefix id]
                                     :owner owner
                                     :state state)))]
    (when x
      (if stored
        (when-not removed?
          (let [x (align-with-stored x stored owner state)]
            (store-fn state x)))
        (store-fn state x)))))

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

(defn get-morph-and-owner [state morph-id]
  (let [m-ref [:morph/by-id morph-id]
        m (get-in state m-ref)
        c-ref (:owner m)
        c (get-in state c-ref)]
    {:morph m
     :morph-ref m-ref
     :owner c
     :owner-ref c-ref}))

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
  (set-props state {:ref ref :props->values {prop value}})
  ; (let [ref (ensure state ref)
  ;       x (get-in state ref)
  ;       prototype (-> x :prototype) 
  ;       state (if prototype
  ;               (set-prop state {:ref prototype
  ;                               :prop prop
  ;                               :value value})
  ;               (-> state
  ;                 (update-in ref assoc-in 
  ;                           [:txs :props prop] value)
  ;                 (update-in ref assoc-in 
  ;                           [:props prop] value)))]
  ;   (update-reconciler state ref))
  )

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
                                             added)))))
        to-be-removed (get-in state ref)
        prototype (-> to-be-removed :prototype)]
    (if prototype
      (-> (remove-morph state {:ref prototype})
        (become ref prototype))
      (-> state
        (update-in (-> to-be-removed :parent) remove)
        (update-in ref merge {:parent nil
                              :owner nil})
        (update-reconciler (-> to-be-removed :parent))))))

(defn add-morph [state {:keys [ref new-parent-ref]}]
  (let [ref (ensure state ref)
        new-parent-ref (ensure state new-parent-ref)
        add #(-> %
               (update-in [:txs :added] conj (second ref))
               (update-in [:txs :removed] disj (second ref)))
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

; interesting: We could just use the ref to the ident
; of the morph twice, but this is not the semantics behind
; the halo copy in lively
; instead, we need to introduce a new morph entry in our
; universe

(defn- copy-morph [state {:keys [morph-id new-morph-id new-id]}]
  (assoc-in state [:morph/by-id new-morph-id] 
            (-> state 
              (get-in [:morph/by-id morph-id])
              (assoc :morph-id new-id))))

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

(defn update-abstraction [state {:keys [ns name new-source]}]
  ; trigger a figwheel recompile by altering the ns source file
  ; and also altering core.clj, such that possible new definitions
  ; are reprocessed and all abstractions are reloaded instrumented
  )

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
   set-prop 
   {:ref (get-ref x)
    :prop prop-name 
    :value prop-value}))

(defn set-props! [x props->values]
  (swap! 
   universe
   set-props 
   {:ref (get-ref x)
    :props->values props->values}))

(defn refresh-scene! []
  (swap! universe refresh-root))

(defn move-morph! [x new-parent]
  (swap! 
     universe
     (comp refresh-root move-morph)
     {:ref (get-ref x)
      :new-parent-ref (get-ref new-parent)}))

(defn remove-morph! [x]
  (swap! 
   universe 
   (comp refresh-root remove-morph)
   {:ref (get-ref x)}))

(defn uuid-from 
  [morph-id]
  (let [splitted-morph-id (split morph-id #"\.")
        id-prop (str (-> splitted-morph-id last) "-copy")]
    {:uuid (join "." (conj (apply vec (butlast splitted-morph-id)) 
                           id-prop))
     :id id-prop}))

(defn copy-morph! [morph]
  (let [morph-id (-> morph :morph-id)
        {:keys [uuid id]} (uuid-from morph-id)]
    (swap! 
     universe 
     copy-morph
     {:ref morph-id
      :new-uuid uuid
      :new-id id})
    [:morph/by-id uuid]))

(def current-namespace 'cljs.user)

(defn set-root! [component]
  (swap! universe set-root component))

(def universe (atom {:morph-tree {}
                     :abstraction/by-name {} ; ns.name -> abstraction data
                     :morph/by-id {}
                     :component/by-id {}}))

(om/root  
 (fn [data _]
   (reify 
     om/IRender
     (render [self]
             (if-let [tree (not-empty (:morph-tree data))]
               (om/build render-morph tree {:key :morph-id})
               (dom/div #js {:id "dummy-root"})))))
 universe {:target (gdom/getElement "app")})