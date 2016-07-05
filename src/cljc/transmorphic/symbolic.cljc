(ns transmorphic.symbolic
  #?(:cljs (:require-macros [cljs.pprint :refer [with-pprint-dispatch]]))
  (:require #?(:cljs [cljs.pprint :refer [pprint code-dispatch 
                                          *print-right-margin*]])
            #?(:cljs [cljs.tools.reader :refer [read-string]]
               :clj  [clojure.tools.reader :refer [read-string]])
            [cljs.analyzer]
            [transmorphic.globals]
            [clojure.core.reducers :as r]
            [clojure.string :refer [replace-first split-lines join]]
            [clojure.zip :as z]))

(defn orphan? [morph]
  (-> morph :owner nil?))

; we need to traverse the owner chain upwards,
; and make sure, the component is completely
; separate from this abstraction...
(defn foreign? 
  "A morph is foreign to another one, if
  he is an orphan or the owner are always
  different regardless how high we walk up
  the owner chain"
  [state a b]
  (or (-> b :owner not) (not= (:owner a) (:owner b))))

(defn fetch-ns-source [ns])

(defn update-ns-source! [ns])

#?(:cljs (defn format-code [form]
           "TODO: Find a faster way to do this."
           (with-out-str (binding [*print-right-margin* 72]
                           (with-pprint-dispatch code-dispatch 
                             (pprint form))))))

(def ctx-param-name `render-ctx#)

; SOURCE MAPPING

; this is a cljs/clj independent type dispatch, and is needed since 
; bootstrapped cljs can NOT YET work with the fully fledged clojure library
; at all

(def ^:dynamic *resolve*)

(def ^:dynamic ^:private *gensyms*)

(defn- n-resolve [sym]
  (let [ns (namespace sym)
         n (name sym)]
    (if (and (not ns) (= (last n) \#))
      (if-let [gs (@*gensyms* sym)]
        gs
        (let [gs (gensym (str (subs n 0 (dec (count n))) "__auto__"))]
          (swap! *gensyms* assoc sym gs)
          gs))
      (*resolve* sym))))

(defn unquote? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote)))

(defn unquote-splicing? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote-splicing)))

(defn- quote-fn* [form]
  (cond
    (symbol? form) `'~(n-resolve form)
    (unquote? form) (second form)
    (unquote-splicing? form) (throw "splice not in list")
    (record? form) `'~form
    (coll? form)
    (let [xs (if (map? form) (apply concat form) form)
          parts (for [x xs]
                  (if (unquote-splicing? x)
                    (second x)
                    [(quote-fn* x)]))
          cat (doall `(concat ~@parts))]
      (cond
        (vector? form) `(vec ~cat)
        (map? form) `(apply hash-map ~cat)
        (set? form) `(set ~cat)
        (seq? form) `(apply list ~cat)
        :else (throw "Unknown collection type")))
    :else `'~form))

(defn quote-fn [resolver form]
  (binding [*resolve* resolver
            *gensyms* (atom {})]
    (quote-fn* form)))

(defmacro template [form] 
  (quote-fn identity form))

(defn template-fn [form]
  (quote-fn identity form))

(defn get-class [node]
  (cond
    (vector? node) :vector
    (list? node) :list
    #?(:clj (= clojure.lang.Cons (type node))
            :cljs false) :list
    #?(:clj (= clojure.lang.ArraySeq (type node))
            :cljs (array? node)) :array
    :default false))

(defmulti tree-branch? get-class)
(defmethod tree-branch? :default [_] false)
(defmethod tree-branch? :vector [v] (not-empty v))
(defmethod tree-branch? :hash-map [m] (not-empty m))
(defmethod tree-branch? :list [l] true)
(defmethod tree-branch? :array [a] true)

(defmulti tree-children get-class)
(defmethod tree-children :vector [v] v)
(defmethod tree-children :hash-map [m] (->> m seq (apply concat)))
(defmethod tree-children :list [l] l)
(defmethod tree-children :array [a] a)

(defmulti tree-make-node (fn [node children] (get-class node)))
(defmethod tree-make-node :vector [v children]
  (vec children))
(defmethod tree-make-node :hash-map [m children]
  (apply hash-map children))
(defmethod tree-make-node :list [_ children]
  (apply list children))
#?(:clj (defmethod tree-make-node :array [_ children]
          (apply list children)))
#?(:cljs (defmethod tree-make-node :array [_ children]
           (prn "create array seq")
          (apply array-seq children)))

(defn tree-zipper [node]
  (z/zipper tree-branch? tree-children tree-make-node node))

(defn morph? [m]
  (and (seq? m) 
       (contains? @transmorphic.globals/morph-defs (first m))))

(defn component? [c]
  (and (seq? c) 
       (contains? @transmorphic.globals/component-defs (first c))))

(defn render-def? [form]
  (and (seq? form) (= 'render (first form))))

(defn ellipse? [m]
  (= :ellipse (-> m :type)))

(defn each-morph-call-out [form transform & args]
  "Traverse the given form bottom up, applying the
  code transformation to every morph definition
  that is encountered. Code transformations can
  not influence the traversal but may still override
  each other in case of nested morph definitions."
  (let [root (tree-zipper form)
        unrolled (take-while (comp not clojure.zip/end?) (iterate clojure.zip/next root))]
    (loop [i (count unrolled)
           z (last unrolled)]
      (if
        (let [x (clojure.zip/node z)]
          (or (morph? x) (component? x)))
        (let [z (clojure.zip/edit z #(apply transform % i args))]
          (if-let [rz (clojure.zip/prev z)]
            (recur (dec i) rz)
            (clojure.zip/root z)))
        (if (clojure.zip/prev z)
          (recur (dec i) (clojure.zip/prev z))
          (clojure.zip/root z))))))

(defn each-morph-call-in [form transform & args]
  "Traverse the given form top down, applying the
   code transformation to every morph definition
   that is encountered on the go.
   Notice, that since this is a top down traversal,
   the transform may influence the traversal
   by removing other morph definitions further down
   the form."
  (let [root (tree-zipper form)]
    (loop [i 0
           z root]
      (if
        (let [x (clojure.zip/node z)]
          (or (morph? x) (component? x)))
        (let [z (clojure.zip/edit z #(apply transform % i args))]
          (if-let [rz (clojure.zip/next z)]
            (recur (inc i) rz)
            (clojure.zip/root z)))
        (if (clojure.zip/end? z)
          (clojure.zip/root z)
          (recur (inc i) (clojure.zip/next z)))))))

; overriding only works for morphs currently. make it work for parts aswell! this is crucial.

(defn instrument-call 
  "Alter all occuring calls to morphs, parts or widgets
   such thath they are aware, they are being used in the
   context of the current abstraction.
   It also wraps the morph into a function call
   that closes over all the context related information
   needed to inspect the morph hierarchy through tooling."
  [call {:keys [edit-session
                 id-stack] :as env}]
  (let [loc (first @id-stack)]
    (swap! id-stack rest)
    (if (= 1 loc)
      (if (morph? call)
        `(transmorphic.core/wrap-morph ~call (assoc ~ctx-param-name :source-location ~loc))
        `(transmorphic.core/wrap-component ~call (assoc ~ctx-param-name :source-location ~loc)))
      `(assoc ~call :source-location ~loc))))

(defn inject-ctx-parameter
  "Find that place where render definition resides, and
   add an additional render-context variable"
  [form]
  (let [root (tree-zipper form)]
    (loop [i 0
           z root]
      (if (render-def? (clojure.zip/node z))
        (let [z (clojure.zip/edit z 
                                  (fn [render-def]
                                    (apply list 
                                      (first render-def) 
                                      (conj (second render-def) ctx-param-name) 
                                      (drop 2 render-def))))]
          
          (clojure.zip/root z))
        (if (clojure.zip/end? z)
          (clojure.zip/root z)
          (recur (inc i) (clojure.zip/next z)))))))

(defn instrument-body! [component-body env]
  (-> component-body
    (inject-ctx-parameter)
    (each-morph-call-out 
     (fn [call _]
       (instrument-call call env)))))

; RECONCILER

(defn bound-props [props]
  (binding [cljs.analyzer/*cljs-warnings*
            (zipmap (keys cljs.analyzer/*cljs-warnings*) (repeat false))]
    (let [props (-> (cljs.analyzer/empty-env)
                  (cljs.analyzer/analyze props) 
                  :children)]
      (map #(-> % first :form) 
           (filter (fn [v]
                     (or 
                      (= :var (-> v second :op))
                      (some #(= :var (:op %))
                            (-> v second :children)))) 
                   (partition 2 props))))))

(defn analyze-expr! [expr change-integrator morph-idx id-stack]
  (if 
    (or (component? expr) (morph? expr))
    (let [current-morph-idx (swap! morph-idx inc)
          _ (swap! id-stack conj current-morph-idx)
          [name props & submorphs] expr
          submorphs (for [m (vec submorphs)]
                            (analyze-expr! m change-integrator morph-idx id-stack))
          integration `{:type ~(if (morph? expr)
                                 :morph
                                 :component)
                        :txs {:props {}
                              :added []
                              :removed #{}}
                        :submorph-locations [~@submorphs]
                        :reification (fn [~'self ~'props-txs ~'submorphs]
                                       (apply list '~name 
                                         (merge '~props ~'props-txs) 
                                         ~'submorphs))}]
      (swap! change-integrator
             assoc current-morph-idx
             integration)
      current-morph-idx)
    
    (let [current-morph-idx (swap! morph-idx inc)
          submorphs (atom [])
          reification-params (atom []) 
          templatized-expr (each-morph-call-in 
                            expr 
                            (fn [morph-def _]
                              (let [m-idx (analyze-expr! morph-def 
                                                         change-integrator
                                                         morph-idx
                                                         id-stack)]
                                (swap! reification-params conj (symbol (str "m_" m-idx)))
                                (swap! submorphs conj m-idx)
                                (symbol (str "~m_" m-idx)))))
          integration `{:type :expr
                        :submorph-locations ~(deref submorphs)
                        :reification (fn [{:keys [~@(deref reification-params)]}]
                                       ~(template-fn (read-string (str templatized-expr))))}]
      (swap! change-integrator
             assoc current-morph-idx
             integration)
      current-morph-idx)))


(defn analyze-body! [defmorph-body id-stack]
  "This function traverses all different morph definitions
  inside the body, and finally yields a change integrator
  that can be used to incorporate structural and
  property changes into the code."
  (let [change-integrator (atom {})
        morph-idx (atom -1)]
    (analyze-expr! defmorph-body change-integrator morph-idx id-stack)
    @change-integrator))

(defn get-external-reconciliation 
  "Returns the symbolic description of a morph
  hierarchy outside of a symbolic functional scope.
  This is essentially a composition of morph calls,
  with custom behavior stripped away and and other functional
  abstractions removed. The only other kinds of abstractions,
  that an external reconciliation maintains are calls to other
  components, which function as 'abstractional strongholds'."
  [state morph]
  (let [c (or 
           (and (:root? morph) (:owner morph) (get-in state (:owner morph)))
           (and (:abstraction morph) morph))] 
    (apply list 
      (or (-> c :abstraction :name)
          (symbol (name (-> morph :type)))) 
      (-> (or c morph) :props)
      (into []
            (comp
             (map #(get-in state %))
             (map #(get-external-reconciliation state %)))
            (-> (or c morph) :submorphs)))))

(defn get-description [state morph reconciler]
  (let [loc (-> morph :source-location)
        {:keys [reification submorph-locations type txs]} (get reconciler loc)
        ; _ (prn (-> morph :component-id) (-> morph :txs)  txs)
        ; the following automatically considers the expr reifications
        get-sub-descriptions 
        (fn [submorph-refs]
          (let [submorphs (map #(get-in state %) submorph-refs)
                loc->submorph-descriptions 
                (into {}
                      (comp 
                       (map #(loop [x' %]
                               (if (and (:owner x') (foreign? state morph x'))
                                 (recur (get-in state (:owner x')))
                                 [(:source-location x')
                                  (get-description state x' reconciler)])))
                       (remove nil?))
                      submorphs)
                own-descriptions 
                (map (fn [loc]
                       (let [{:keys [reification submorph-locations type]} 
                             (get reconciler loc)]
                         (case type
                           :morph (get loc->submorph-descriptions loc)
                           :component (get loc->submorph-descriptions loc)
                           :expr (let [args (into {}
                                                  (map (fn [entry]
                                                         [(keyword (str "m_" (key entry)))
                                                          (val entry)]))
                                                  loc->submorph-descriptions)]
                                   (reification args))))) 
                     submorph-locations)
                added-submorphs (map #(get-in state %) (:added txs))
                added-descriptions
                (into [] 
                      (comp
                       (map (fn [m]
                              (when (foreign? state morph m)
                                (get-external-reconciliation state m))))
                       (remove nil?))
                      added-submorphs)]
            (concat own-descriptions added-descriptions)))]
    (case type
      :morph (reification morph (txs :props) (get-sub-descriptions (:submorphs morph)))
      :component (let [c (get-in state (:owner morph))]
                   (reification c (txs :props) (get-sub-descriptions (:submorphs c))))
      nil)))

(defn get-component-def [state root-morph reconciler]
  (let [{:keys [reification submorph-locations type txs]} (get reconciler 0)
        root-reconciliation (get-description state root-morph reconciler)]
    (if (= :expr type)
      (reification {:m_1 root-reconciliation})
      root-reconciliation)))

(defn get-internal-reconciliation
  "Given a morph, return its symbolic consolidation
  within the context of the abstraction it is the root of.
  If morph is not a root of a component,
  this falls back to an external reconciliation if the"
  [state morph]
  (get-description state morph 
                   (-> state
                     (get-in (:owner morph))
                     :reconciler)))