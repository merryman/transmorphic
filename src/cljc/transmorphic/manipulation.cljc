(ns transmorphic.manipulation)

; HALO INTERACTION

(defprotocol IManipulation
  "The set of operations that can be enacted on a morphic
  entity through the halo."
  (add-morph [self morph] "Add a morph to a morph")
  (remove-morph [self id] "Remove morph with given id")
  (set-prop [self prop value] "Set prop of morph to value"))

; "Alters the reconciliation strategy of the given morph.
;   If a abstraction root is orphanized, it is turned into a
;   call with the current value of the model. This can be used
;   to drag and drop calls to the abstraction into code.
;   If an abstraction part is orphanized it is turned into
;   a plain call to the morph primitive. All submorphs
;   loose their abstraction too, since they are possibly
;   bound to variables declared in different scopes.
;   The exception are sub evolvements, which are now
;   explicitly parametrized with the current model.
;   They have the same behavior as orphaned abstraction roots."

(defmulti orphanize
  (fn [morph]
    (morph :role)))

; (defn orphanize 
;   "Strips all meta informations about
;   original abstraction and change integration that the morph
;   stems from. Converts it into a functional
;   building block, that can be included into
;   any abstraction, OR a starting point for
;   a new abstraction."
;   ([morph]
;   (let [id-stack (atom [])
;         morph (assoc-in morph [:props :id] (morphic-uuid morph))]
;     (orphanize morph 
;                   (let [change-integrator (atom {})
;                         morph-idx (atom -1)]
;                     (analyze-morph! ((morph :description) morph) 
;                                     change-integrator morph-idx id-stack)
;                     change-integrator) 
;                   id-stack)))
;   ([morph ci id-stack]
;   (let [morph (-> morph
;                 (dissoc :abstraction-name :model :change-integrator)
;                 (assoc :orphan? true
;                         :change-integrator ci
;                         :source-location (first @id-stack)))]
;     (swap! id-stack rest)
;     (update-in morph [:submorphs] (fn [subs]
;                                     (get-submorphs 
;                                       (map #(orphanize (val %) ci id-stack) 
;                                           (remove (comp nil? val) subs)) false))))))

(defmethod orphanize :part
  [morph]
  (-> morph
    (dissoc :model :abstraction-name :source-location)
    (assoc 
     :role :orphan
     :submorphs (map orphanize (self :submorphs))
     :reconciler (primitive-reconciler self))))

(defmethod orphanize :root
  [root]
  (-> root
    (assoc :role :orphan
           :reconciler (model-call-reconciler self))))

(defmethod orphanize :orphan
  [orphan]
  orphan)

(defmulti incorporate (fn [self orphan]
                        (self :role)))

(defmethod incorporate :orphan
  [orphan other-orphan])

(defmethod incorporate :part
  [part orphan])

(defmethod incorporate :root
  [root orphan])

(defn changes-on-morph [path]
  (get-in @tx-index path))

(defn empty-node [morph]
  (let [submorph-nodes (into {} (map (fn [s]
                                       [(key s) (when (val s)
                                                  (empty-node (val s)))]) 
                                     (morph :submorphs)))]
    (case (morph :node-type) 
      :coll {:structure []  
             :submorphs submorph-nodes}
      :morph {:prop-changes {}
              :structure []
              :submorphs submorph-nodes
              :source-location (get morph :source-location)
              :change-integrator (get morph :change-integrator)}
      :expr {}
      (throw (str "Encountered unknown structure at " morph)))))

(defn track-changes! [morph path]
  (when-not (get-in @tx-index path)
    (swap! tx-index assoc-in path (empty-node morph))))

(defn ensure-node [node path]
  (merge (empty-node (get-in @world-state path)) node))

(defn ensure-root-node [path]
  (let [root-path (abstraction-root @world-state path)]
    (swap! tx-index update-in root-path 
           (fn [root]
             (ensure-node root root-path)))))

(defn clear-txs-for! [path]
  (prn "clearing interactions with " path)
  (swap! tx-index assoc-in path nil))

(defn revert-tx! 
  "Undo the transaction given by the tx-id.
  tx-id is of the form [[:structure | :prop-changes][idx | prop-name]]"
  [tx-id])

(defn move-morph! [from-lens to-lens]
  ; without! + add-morph! but migrating changes!
  )

(defn set-prop! [path prop value]
  ; in case we are operating on a morph, that is 
  ; part of an abstraction we need to first
  ; make sure, that the tx index, is expanded such 
  ; that all change integrators are synchronized accordingly.
  ; (swap! tx-index update-in (abstraction-root @world-state path)
  ;       (fn [root-node]
  ;         (ensure-node )))
  (swap! tx-index update-in path
         (fn [self]
           (let [self (ensure-node self path)]
             (set-prop-change self prop value) ; propagate to change-integrator
             (-> self (assoc-in [:prop-changes prop] value))))))

(defn without! [path]
  (let [id (last path)
        path (drop-last 2 path)]
    (swap! tx-index update-in path
           (fn [self] 
             (let [self (ensure-node self path)
                   to-be-removed (get-in self [:submorphs id])]
               (when (-> @world-state (get-in path) :node-type (not= :coll)) 
                 (remove-morph-change self (get-in self [:submorphs id])))
               (-> self
                 (update-in [:structure] 
                            conj {:op :without
                                  :idx id
                                  :backup to-be-removed})
                 (update-in [:submorphs] (fn [subs] (dissoc subs id))))))))
  (swap! trigger-rerender! inc))

(defn ad-hoc-id [submorphs]
  (inc (apply max -1 (map #(if (number? %) 
                            %
                            (get @(-> submorphs meta :ordering) %)) 
                         (keys submorphs)))))

(defn add-morph! 
  ([self morph]
   (add-morph! self morph nil))
  ([self morph changes]
   (if-let [add-morph-cb! (-> self :props :on-morph-add)]
     (add-morph-cb! morph) ; no change, since part of domain
     (let [morph* (if (fn? morph) (morph) morph)
           changes (merge (empty-node morph*) changes) ; in case we are passed a nil value
           morph-id (-> morph* :props :id)]
       (when-not morph-id
         (throw (str "I can not grab a morph with no assigned id!")))
       (swap! tx-index update-in (-> self :path)
                            (fn [n] 
                              (let [n (ensure-node n path)
                                    alias-loc (add-morph-change n changes)]
                                (-> n
                                  (update-in [:structure] 
                                             conj {:op :add-morph
                                                   :morph morph
                                                   :idx morph-id
                                                   :backup changes})
                                  (assoc-in [:submorphs morph-id :alias] alias-loc)))))
       (swap! trigger-rerender! inc)
       (concat path [:submorphs morph-id])))))


(defn set-id! [path new-id]
  (let [idx (last path)
        path (drop-last 2 path)]
    (swap! tx-index update-in path
           (fn [self] 
             (let [self (ensure-node self path)]
               (set-id-change self idx new-id)
               (-> self
                 (update-in [:structure] 
                            conj {:op :set-id
                                  :idx idx
                                  :new-id new-id})))))))

(defn add-evolvement! [path evolve-fn]
  (swap! trigger-rerender! inc)
  (add-morph! path evolve-fn))

(defn add-morph 
  ([parent morph morph-id]
  "Add single morph to the morph with id"
  (let [morph (if (fn? morph) (morph) morph)
        submorphs (-> parent :submorphs)
        ordering (-> submorphs meta :ordering)] 
    (swap! ordering assoc morph-id (ad-hoc-id submorphs))
    (assoc-in parent [:submorphs morph-id] morph))))

(defn set-id [parent idx new-id]
  (update-in parent [:submorphs]
             (fn [submorphs]
               (-> submorphs
                 (dissoc idx)
                 (assoc new-id (assoc-in (get submorphs idx) [:props :id] new-id))))))

(defn apply-tx [tx morph]
  (case (tx :op)
    :set-id (set-id morph (tx :idx) (tx :new-id))
    :add-morph (add-morph morph (tx :morph) (tx :idx))
    :without (remove-morph morph [:submorphs (tx :idx)])
    morph))

(defn apply-interactions 
  ([world]
   (apply-interactions world @tx-index))
  ([morph txs]
   (let [ci (get txs :change-integrator)
         morph (if-let [structure-changes (get txs :structure)]
                 (loop [changes structure-changes
                        m morph]
                   (if-let [change (first changes)]
                     (recur (rest changes) (apply-tx change m))
                     m))
                 morph)
         morph (if-let [new-props (get txs :prop-changes)] 
                 (-> morph
                   (update-in [:props] #(merge % new-props))
                   (assoc :overridden-props 
                          (when-let [props (and ci
                                                (-> @ci 
                                                  (get (morph :source-location))
                                                  :props-atom))]
                            (select-keys
                             @props
                             (-> @ci 
                               (get (morph :source-location))
                               :overridden)))))
                 morph)
         morph (if ci
                 (assoc morph 
                        :consolidated-abstraction 
                        (get-description @ci))
                 morph)]
     (reduce-kv 
      (fn [morph id txs]
        (update-in morph [:submorphs id]
                   #(when % (apply-interactions % txs))))
      morph (select-keys (get txs :submorphs) (-> morph :submorphs keys))))))