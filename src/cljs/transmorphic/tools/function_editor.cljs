(ns transmorphic.tools.function-editor
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]] 
   [transmorphic.core :refer [defcomponent]])
  (:require [transmorphic.morph :refer [$morph orphaned?
                                        parent $component
                                        centered-in abstraction-root]]
            [transmorphic.manipulation :refer [without! clear-txs-for! add-morph!]]
            [transmorphic.core :refer [io rectangle image listmorph text rerender! IRender
                                       universe checkbox ace IDidMount IRefresh]]
            [transmorphic.symbolic :refer [format-code get-internal-reconciliation]]
            [transmorphic.event :refer [get-client-pos get-cursor-pos]]
            [transmorphic.utils :refer [add-points]]
            [transmorphic.tools.window :refer [window window-resizer]]
            [transmorphic.tools.ace :refer [paredit setup-ace! set-value!]]
            [transmorphic.repl :refer [morph-eval morph-defn read-objs 
                                            load-and-callback! morph-eval-str]]
            
            [cljs.core.async :as async :refer [<!]]
            [cljs.reader :refer [read-string]]
            [clojure.string :refer [join split-lines]]
            [cljs.pprint :refer [pprint code-dispatch *print-right-margin*]]))

; EDITING

(def edited-morphs (atom #{}))

; this just stores the sources of all the previously
; accessed sources, to prevent fetching them from the server
(def ns->source (atom {}))

; this stores a mapping of [ns abstraction-name] pairs
; which caches information about the sources being edited from
; the perspective of different editors / abstractions
(def ns->altered (atom {}))

(defn get-consolidated-abstraction [old-form consolidated-abstraction]
  (if consolidated-abstraction 
    (let [form (butlast old-form)]
         `(~@form ~@consolidated-abstraction))
    old-form))

(defn get-info-idx [abstraction-name ns-info]
  (some (fn [[i info]]
          (when (-> info :name str (= (str abstraction-name))) 
            i)) (zipmap (range) ns-info)))

(defn patch-ns! [ns+abstraction read-ns consolidated-abstraction]
  (if (and (read-ns :prev-consolidation)
       (= consolidated-abstraction
                    (read-ns :prev-consolidation)))
      read-ns
      (let [abstraction-form (get read-ns :abstraction-form)
            updated-abstraction (get-consolidated-abstraction 
                                 abstraction-form
                                 consolidated-abstraction)
            updated-source (format-code updated-abstraction)
            line-count (count (re-seq #"\n" updated-source))
            patch {:value updated-source
                   :range [(read-ns :line) (dec (read-ns :end-line))]}
            read-ns* (-> read-ns
                       (assoc :patch patch
                              :abstraction-form updated-abstraction
                              :end-line (+ (read-ns :line) line-count)
                              :ns-source (str (-> read-ns :partitioned-source :before) "\n\n" 
                                              updated-source "\n\n\n" 
                                              (-> read-ns :partitioned-source :after)))
                       (with-meta {:prev-consolidation consolidated-abstraction}))]
        (swap! ns->altered assoc-in ns+abstraction read-ns*)
        read-ns*)))

(defn parse-ns [ns-source abstraction-name consolidated-abstraction]
  (let [read-ns (read-objs ns-source)
        info-idx (get-info-idx abstraction-name read-ns)
        line-before (get-in read-ns [(dec info-idx) :end-line])
        line-after (get-in read-ns [(inc info-idx) :line])
        lines (split-lines ns-source)
        source-before (join "\n" (take line-before lines))
        source-after (if line-after (join "\n" (drop (dec line-after) lines)) "")
        abstraction-form (get-consolidated-abstraction 
                            (get-in read-ns [info-idx :form])
                            consolidated-abstraction)
        abstraction-source (format-code abstraction-form)
        line-count (count (re-seq #"\n" abstraction-source))
        line (get-in read-ns [info-idx :line])]
    {:prev-consolidation consolidated-abstraction
     :info-idx info-idx
     :ns-info read-ns
     :abstraction-form abstraction-form
     :partitioned-source {:before source-before
                          :after source-after}
     :line line
     :end-line (+ line line-count)
     :ns-source (str source-before (apply str (repeat (- line line-before) "\n")) 
                     abstraction-source (apply str (repeat (- line-after (get-in read-ns [info-idx :end-line])) "\n")) 
                     source-after)}))

(defn get-editable-source [ns consolidated-abstraction abstraction-name]
  "Returns a hashmap of line-number intervals to morph defintions."
  (if-let [s (get-in @ns->altered [ns abstraction-name])]
    {:cached (patch-ns! [ns abstraction-name] s consolidated-abstraction)}
    (or
     (when-let [ns-source (get @ns->source ns)]
       (swap! ns->altered assoc-in 
              [ns abstraction-name] 
              (parse-ns ns-source 
                        abstraction-name
                        consolidated-abstraction))
       {:cached (get-in @ns->altered [ns abstraction-name])})
     (do
      (load-and-callback! ns ".cljs" #(let[ns-source (:source %)
                                           parsed-ns (parse-ns ns-source 
                                                               abstraction-name
                                                               consolidated-abstraction)]
                                        (swap! ns->source assoc ns ns-source)
                                        (swap! ns->altered assoc-in [ns abstraction-name] parsed-ns)))
             {:loading true}))))

(defn read-namespace [morph]
  (let [ns (-> morph :namespace)
         abstraction-name (-> morph :abstraction-name)
         consolidated-abstraction (morph :consolidated-abstraction)]
    (get-editable-source ns consolidated-abstraction abstraction-name)))

(defn update-source! [ns abstraction-name new-source new-form]
  (swap! ns->source assoc ns new-source)
  (swap! ns->altered dissoc ns))

(defn empty-morph-ns [ns-name]
  (format-code
   (read-string 
    (str "(ns " ns-name
         "(:require-macros [cljs-morphic.macros 
         :refer [evolve! defview
         io ellipse rectangle image 
         listmorph text]])
        ; (:require [cljs_morphic.helper :refer 
        ; [*abstraction-name* *call-depth* *change-integrator* 
        ;   *source-location* lookup-model-atom-for-morph]])
         )"))))

(defn get-local-namespace [new-ns]
  (or
   (get @ns->source new-ns)
   (let [new-source (empty-morph-ns new-ns)]
     (swap! ns->source assoc new-ns new-source)
     new-source)))

(defn redefine-ns! [ns new-source]
  (swap! ns->source assoc ns new-source)
  ; TODO: clear the altered ns as well  
  (let [read-ns (read-objs new-source)
        head (-> read-ns first :source)
        body (->> read-ns rest (map :source) doall)
        errors (atom [])]
    (doseq [body-part body]
      (when-let [res (morph-eval-str (str head body-part) ns)]
        (when-let [err (:error res)] (swap! errors conj err))))
    (when-not (empty? @errors)
      {:error @errors})))

; (defcomponent morph-editor 
;   IRender
;   (render [self {:keys [position extent target-id] :as props} _]
;           (let [morph ($morph target-id)]
;             (window
;             {:position position
;               :extent extent}
;             ; ace always will fill its parent morph
;             (ace {:theme "ace/theme/github"
;                   :line line
;                   :value (get-internal-reconciliation @universe morph)
;                   :refresh patch
;                   :on-save (fn [new-source]
;                               ; prompt to create a new component for the unfinished description
;                               (redefine-ns! (:abstraction morph) new-source))})))))

(defcomponent component-editor
  IRefresh
  (refresh [self props]
           (let [root-morph ($morph (props :target-ref))
                 component (get-in @universe (:owner root-morph))]
             (set-value! (.edit js/ace (props :id)) 
               (format-code (get-internal-reconciliation @universe root-morph)))))
  IRender
  (render [self {:keys [position extent target-ref id] :as props} _]
          (let [root-morph ($morph target-ref)
                component (get-in @universe (:owner root-morph))]
            (window
             {:position position
              :extent extent}
             (rectangle {}
              (text {:position {:x (- (:x extent) 30) 
                                :y -20}
                     :text-string "Reconcile Changes "
                     :font-family "Chrono Medium Italic"
                     :text-color "black"
                     :font-size 12})
              (checkbox {:on-change (fn [_] )
                         :position {:x (- (:x extent) 30) :y -20}}))
             (ace {:ace-id id
                   :did-mount (fn [_] (setup-ace! self props))})
            ; (paredit 
            ;   {:id (str "editor-on-" target-ref)
            ;   :theme "ace/theme/github"
            ;   ; this will make ace jump to the definition
            ;   ; of the passed symbol after a save and upon init
            ;   :focus (:abstraction-name component)
            ;   ; this value is set intially, after the initial set
            ;   ; all alterations to the editor can only be done
            ;   ; through a refresh
            ;   :value (format-code (get-internal-reconciliation @universe root-morph))
            ;   :patch {(:abstraction-name component) 
            ;           (format-code (get-internal-reconciliation @universe root-morph))}
            ;   ; allows to replace particular spexps inside the source
            ;   ; to insert edjustments from outside.
            ;   ; 
            ;   ; :refresh {sexp-idx (get-internal-reconciliation @universe root-morph)}
            ;   :on-save (fn [new-source]
            ;               ; prompt to create a new component for the unfinished description
            ;               (redefine-ns! (:abstraction component) new-source))} "ace-editor")
             ))))

(defcomponent text-prompt
  IRender 
  (render [self model submorphs]
          (let [ok-button (fn [cb]
                            (rectangle
                             {:position {:x 120 :y 50}
                              :extent {:x 100 :y 30}
                              :fill "grey"
                              :on-mouse-down cb}
                             (text {:position {:x 45 :y 8}
                                    :text-string "OK"
                                    :font-family "Chrono Medium Italic"
                                    :text-color "white"
                                    :font-size 15})))]
            (window
             {:title (model :question)
              :extent {:x 300 :y 150}
              :position (centered-in ($morph "world") ; centered in parent?
                                     {:x 300 :y 100})
              :on-close (fn [] 
                          (swap! edited-morphs disj (model :target-id)))
              :submorphs 
              [(paredit (model :input) "ace-editor")
               (ok-button (fn []
                            (let [value (.getValue (-> model :input :ace-state deref :ace-instance))]
                              ((model :on-finish) value))))]}))))

(defn prompt-user [question on-finish morph-id]
  (text-prompt
   {:on-finish on-finish
    :target-id morph-id
    :input {:id (str "promp-for-" question)
            :theme "ace/theme/github"
            :extent {:x 280 :y 40}
            :line-numbers? false
            :position {:x 30 :y 10}}}))


; calling this function will gather the
; existing symbolic description of the component
; and provide symbolic consolidation of the
; interaction changes, to be merged into
; the code
(defn edit-component! [component-id])

; calling this function, will render the
; external reconciliation of the morph
; hierarchy, starting at the morph with morph-id.
; provides the ability, to create a new component definition
; based of the hierarchy.
(defn edit-morph! [morph-id]
  (let [world-ref ($morph "world")
        morph ($morph morph-id)]
    (if (orphaned? morph)
      (prompt-user "Please Enter Name for Function:"
                   (fn [function-name]
                     ; get the cljs.user source
                     (let [source (get-local-namespace 'cljs.user)
                           ; create a new defmorph in the cljs.user namespace and
                           ; append the cljs.user form with the corresponding source
                           empty-defn (read-string 
                                       (str "(defview " function-name  "[model]"
                                            ((morph :description) morph) ")"))
                           source (str source "\n\n\n" (format-code empty-defn) "\n")]
                       (redefine-ns! 'cljs.user source)
                       ; compiled the template, 
                       
                       ;and replace the orphaned morph with a call
                       ; to the new definition
                       ; this needs to happen, once figwheel returns
                       (without! morph-id)
                       (add-morph! (morph :owner) 
                                   (let [current-morph morph
                                           m (morph-eval-str
                                              (str "(evolve! (cljs.user/" function-name
                                                   " {}) :" (or (-> morph :props :id) function-name) ")")
                                              'cljs.user)]
                                       
                                       (if-let [err (:error m)]
                                         (assoc current-morph :error err)
                                         m)))))
                   morph-id)
      (window
       {:on-close (fn [] 
                    (swap! edited-morphs disj morph-id))
        :submorphs (component-editor 
                    {:title "Function Editor"
                     :position (centered-in world-ref {:x 600 :y 600})
                     :extent {:x 600 :y 600}
                     :target-id morph-id}
                    (str "editor-on-" morph-id))}))))