(ns transmorphic.core
  (:require
   [clojure.walk :refer [macroexpand-all]]
   [clojure.pprint :refer [pprint]] 
   [transmorphic.symbolic :refer [analyze-body! instrument-body!]]))

; morphs are atoms, and are therefore not composed at all
; morphs have a single default reconciler policy: (type props (submorph reconciliation))
(defmacro defmorph 
  [morph-name & stuff]
  (swap! transmorphic.symbolic/morph-defs assoc morph-name {:ns (-> &env :ns :name)})
  (let [has-doc (string? (first stuff))
        doc-string (if has-doc (first stuff))
        [args & body] (if has-doc (rest stuff) stuff)
        morph-record (symbol (str morph-name "*"))
        morph-constructor (symbol (str "map->" morph-name "*"))] 
    `(do
       (defrecord ~morph-record 
         [~'morph-id ~'type ~'props ~'submorphs ~'morph->html ~(symbol "root?")])
       (defn ~morph-name [~'props & ~'submorphs]
         (~morph-constructor {:type (keyword '~morph-name)
                              :props ~'props 
                              :submorphs ~'submorphs
                              :morph->html (fn ~args
                                              ~@body)})))))

; components are functional projections, yielding morph hierarchies
; components may or may not carry local state.
; future work is to experiment with different ways to distribute data
; to components. Currently this is only happening through either props
; (update ...) or if one component talks to another one through $component
; and sends a message.
; $morph is a meta inspection utility, only used by tools such as the halo
; to perform alterations on the rendered hierarchy that are preserved
; whenever projections are re-rendered

(defmacro defcomponent [comp-name & stuff]
  (swap! transmorphic.symbolic/component-defs 
         assoc comp-name {:ns (-> &env :ns :name)})
  (let [has-doc (string? (first stuff))
        doc-string (if has-doc (first stuff))
        body (if has-doc (rest stuff) stuff)
        source-locations (atom (list))
        record-name (symbol (str comp-name "*"))
        component-constructor (symbol (str "map->" record-name))
        internal-reconciler (assoc (analyze-body! body source-locations) :active? true)
        body' (instrument-body! body {:reconciler internal-reconciler
                                      :edit-session {:abstraction-name comp-name
                                                     :reconciler internal-reconciler}
                                      :id-stack source-locations})] 
    `(do
       (declare ~comp-name)
       (defrecord ~record-name
         [~'component-id 
          ~'local-state
          ~'parent 
          ~'props
          ~'txs
          ~'reconciler 
          ~'abstraction 
          ~'submorphs
          ~'source-location]
         ~@body')
       (defn ~comp-name {:doc ~doc-string} [props# & submorphs#]
         (~component-constructor
           {:props props# 
            :submorphs submorphs#
            :reconciler ~internal-reconciler
            :abstraction ~comp-name})))))