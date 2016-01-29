(ns transmorphic.morph
  (:require-macros [om.next :refer [defui]])
  (:require
   [om.next :as om]
   [goog.dom :as gdom]
   [transmorphic.core :refer [universe eval-suspended-props ensure get-ref
                              morph? component?]]
   [transmorphic.utils :refer [contains-rect? add-points]]
   [transmorphic.manipulation]
   [transmorphic.symbolic :refer [instrument-body! analyze-body! ellipse?]]))

(enable-console-print!)

(declare orphaned? Root Orphan Part Coll RootView MorphView CollView)

; MORPH REFERENCING

(defn orphaned? [morph]
  (not (-> morph :owner)))

(defn is-submorph? [parent morph]
  (some #(= (second %) (-> morph :morph-id)) 
        (-> parent :submorphs)))

(defn $props 
  ([morph]
   (-> @universe 
     (get-in (get-ref morph))
     :props))
  ([morph prop]
   (let [v (get ($props morph) prop)]
     (or (:value v) v))))

(defn $submorphs 
  "Get a vector of all current submorphs of
   a component or morph."
  [x]
  (mapv #(get-in @universe %) (:submorphs x)))

(defn $morph
  "Get the morph that corresponding to the id.
   We first assume id, is a global identifier.
   If this fails, fall back to searching for
   the next morph that carries a matching
   :id property."
  [id]
  (when id
    (if-let [ref (ensure @universe [:morph/by-id id])]
      (get-in @universe ref)
      (some
       (fn [[_ morph]]
         (when (= id (-> morph :props :id))
           morph)) 
       (get @universe :morph/by-id)))))

(defn $component 
  "Get the component that corresponding to the id.
   We first assume id, is a global identifier.
   If this fails, fall back to searching for
   the next component that carries a matching
   :id property."
  [id]
  (when id
    (if-let [ref (ensure @universe [:component/by-id id])]
      (get-in @universe ref)
      (some
        (fn [[_ {:keys [props] :as c}]]
          (when (= id (props :id))
            c)) 
        (get @universe :component/by-id)))))

(defn $local-state 
  "Retrieve the current local state of
   the component."
  [component]
  (:local-state (get-in @universe (get-ref component))))

(defmulti $parent 
  "Get the parent morph of a component or morph or 
   evaluate to the property of a parent morph."
  (fn [x & args]
    (cond 
      (keyword? x) :property
      (or (component? x) (morph? x)) :morph
      :else (throw (str "Can not determine parent of: " x)))))

(defmethod $parent :morph
  [x]
  (let [ref (get-ref x)
             parent-ref (-> @universe (get-in ref) :parent)]
    (and parent-ref (get-in @universe parent-ref))))

(defn $owner 
  "Get the component, that passed the props to the
   given owner or morph"
  [x]
  (let [ref (get-ref x)
             owner-ref (-> @universe (get-in ref) :owner)]
    (and owner-ref (get-in @universe owner-ref))))

(defn centered-in [reference-morph extent]
  (let [{ref-x :x ref-y :y} (-> reference-morph $morph :extent)
        centered-x (- (/ ref-x 2) (/ (extent :x) 2))
        centered-y (- (/ ref-y 2) (/ (extent :y) 2))]
    {:x centered-x :y centered-y}))

(defn abstraction-root 
  "Returns the path to the structure that represents
  the root of the abstraction. Unlike owner-path, this can
  either be a morph or a collection of morphs, since
  abstractions can yield either of the two."
  ([morph]
   (abstraction-root morph (morph :abstraction-name)))
  ([morph abstraction-name]
   (cond
     (and (instance? morph Root) (= abstraction-name (morph :abstraction-name))) morph
     (= abstraction-name (morph :abstraction-name)) (abstraction-root ($parent morph)  
                                                                      abstraction-name))))


; COMPUTED PROPS

(defmethod $parent :property
  ([property]
   ($parent property identity))
  ([property transform]
   {:relative? true
    :eval (fn [state morph]
            (let [morph (get-in state (:parent morph))
                  parent-prop (-> morph :props property)]
              (if (:relative? parent-prop)
                (transform ((:eval parent-prop) state morph))
                (transform parent-prop))))}))

(defn vary 
  "Vary a value of a propery over time, given a transformation
   of the value with respect to the current signals value.
  
  ex: (vary 42 #(* %1 (sin %2)) (current-time))"
  [value transform signal]
  )

(defn behavior 
  ([{:keys [init-value signals on-change global?]}]
   (->
     (fn [world path] 
       (let [values (map 
                     (fn [beh] 
                       (let [v @beh]
                         (if (or global? (:global? v))
                           (v :value)
                           (when (= path (:origin v))
                             (:value v)))))
                     signals)]
         (if (every? nil? values)
           init-value 
           (apply
             on-change
             values))))
     (with-meta {:reactive true
                 :tangible false}))))

(defn animate [value easing]
  "Set property to value, but animate the change stylized
  by the provided easing function. Note that like all
  reactive property defs, also animated props are retriggered
  when =>, or set-prop defines a new value for that prop.
  Only explicit redefinitions can get 'rid' of 
  the animation behavior.")

; WORLD OPERATIONS

(defn local-offset [morph]
  (if (ellipse? morph) 
    (let [{:keys [x y]} ($props morph :extent)]
      {:x (/ x 2) :y (/ y 2)})
    {:x 0 :y 0}))

(defn position-in-world
  [morph]
  (if morph
    (add-points
     (let [owner-morph ($parent morph)]
       (add-points (local-offset owner-morph) 
                   (position-in-world owner-morph)))
     (or ($props morph :position)
         {:x 0 :y 0}))
    {:x 0 :y 0}))

(defn global-bounds [morph]
  [(position-in-world morph) 
   ($props morph :extent)])

(defn contains-morph? [morph-a morph-b]
  (let [bounds-a (global-bounds morph-a)
        bounds-b (global-bounds morph-b)
        contains (contains-rect? bounds-a bounds-b)]
    contains))

(defn morph-under-me 
  ([self]
   (morph-under-me self ($parent self) #{(-> self :morph-id)} true))
  ([self {:keys [morph-id] :as current} ignoring ask-owner?]
   (when current
     (let [match (some (fn [{:keys [morph-id] :as submorph}]
                         (when (not (contains? ignoring morph-id))
                           (morph-under-me self submorph 
                                           (conj ignoring morph-id) 
                                           false)))
                       ($submorphs current))]
       (or match
           (when (contains-morph? current self)
             current)
           (and ask-owner?
                current
                (morph-under-me
                 self
                 ($parent current) 
                 (conj ignoring morph-id)
                 true)))))))

(defn get-description* 
  ([integrator]
   (get-description* integrator 0))
  ([integrator i]
   (when-let [entry (-> integrator (get i))]
     (let [r (-> entry :reification)
           subs (-> entry :submorphs deref)]
       (apply r (remove nil? (map #(get-description* integrator %) subs)))))))

(declare set-prop add-morph remove-morph get-query get-props)

; a Morph that has been removed from
; an abstraction. Colls can not be orphaned,
; so this again just applies to 
; Orphans are never encountered, it is merely an operation
; that we can apply to parts, when they are relocated to a different
; view. Remove this record, and just emply the orphanization call
; to strip away the old, and introduce the new context for a morph
; orphans are simply parts, where the incorporation has refused
; to add a corresponding view (i.e. by the world or the hand)
; These can then be used, to create a new view definition
; from scratch

(defn eval-reactive-prop [v world lens]
  (if (-> v meta :reactive)
      (eval-reactive-prop (v world lens) world lens)
      v))

(defn eval-reactive-props [{:keys [id], :as compiled-props}
                           lens world]
  "Scan the compiled-props for reactively
  defined properties and evaluate them
  in the context of the world."
  (reduce
   merge
   (map 
    (fn [prop]
      (let [v (prop compiled-props)]
        {prop (eval-reactive-prop v world lens)})) 
    (keys compiled-props))))