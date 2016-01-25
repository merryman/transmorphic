(ns transmorphic.morph
  (:require-macros [om.next :refer [defui]])
  (:require
   [om.next :as om]
   [goog.dom :as gdom]
   [transmorphic.core :refer [universe eval-suspended-props ensure]]
   [transmorphic.utils :refer [contains-rect? add-points]]
   [transmorphic.manipulation]
   [transmorphic.symbolic :refer [instrument-body! analyze-body!
                                  ellipse?]]))

(enable-console-print!)

(declare orphaned? Root Orphan Part Coll RootView MorphView CollView)

; MORPH REFERENCING

(defn orphaned? [morph]
  (not (-> morph :owner)))

(defn is-submorph? [parent morph]
  (some #(= (second %) (-> morph :morph-id)) 
        (-> parent :submorphs)))

(defn $morph 
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
  [id]
  (when id
    (if-let [ref (ensure @universe [:component/by-id id])]
      (get-in @universe ref)
      (some
        (fn [[_ {:keys [props] :as c}]]
          (when (= id (props :id))
            c)) 
        (get @universe :component/by-id)))))

(defn $parent [morph]
  (when-let [parent-ref (-> morph :parent)]
    (get-in @universe parent-ref)))

(defn $owner [morph]
  (when-let [owner-ref (-> morph :owner)]
    (get-in @universe owner-ref)))

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

(defn parent
  "Reactive property that is dependent on a parent's property value
   subject to changes in the given transform."
  ([property]
   (parent property identity))
  ([property transform]
   {:relative? true
    :eval (fn [state morph]
            (-> (get-in state (:parent morph)) 
              :props property transform))}))

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
  (if-let [{:keys [x y]} (and (ellipse? morph)
                              (-> morph :props :extent))]
    {:x (/ x 2) :y (/ y 2)}
    {:x 0 :y 0}))

(defn position-in-world
  [morph]
  (if morph
    (add-points
     (let [owner-morph ($parent morph)]
       (add-points (local-offset owner-morph) 
                   (position-in-world owner-morph)))
     (or (-> morph :props :position :value)
         (-> morph :props :position) 
         {:x 0 :y 0}))
    {:x 0 :y 0}))

(defn global-bounds [morph]
  [(position-in-world morph) 
   (-> morph 
     :props
     :extent)])

(defn contains-morph? [morph-a morph-b]
  (let [bounds-a (global-bounds morph-a)
        bounds-b (global-bounds morph-b)
        contains (contains-rect? bounds-a bounds-b)]
    contains))

(defn morph-under-me 
  ([self]
   (morph-under-me self ($parent self) #{(-> self :morph-id)} true))
  ([self current ignoring ask-owner?]
   (let [match (some (fn [[_ ref]]
                       (when (not (contains? ignoring ref))
                         (morph-under-me self ($morph ref) (conj ignoring ref) false)))
                     (-> current :submorphs))]
     (or match
         (when (contains-morph? current self)
           current)
         (and ask-owner?
              current
              (morph-under-me
               self
               ($parent current) 
               (conj ignoring (-> current :morph-id))
               true))))))

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