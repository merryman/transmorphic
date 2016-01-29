(ns transmorphic.tools.ace
  (:require-macros [transmorphic.core :refer [defcomponent]])
  (:require [transmorphic.tools.window :refer [window]]
            [transmorphic.utils :refer [add-points]]
            [transmorphic.morph :refer [$morph]]
            [transmorphic.core :refer [ellipse rectangle text image ace rerender!
                                       IRender IRefresh IDidMount]]
            [transmorphic.symbolic :refer [format-code]]
            [transmorphic.repl :refer [morph-eval-str morph-eval morph-defn]]
            [om.dom :as dom]
            [cljs.reader :refer [read-string]]))

; TODO : start interfacing with paredit.js, to replace ast nodes
; and stop doing the index shuffeling. Also extent paredit.js, such
; that dirty nodes are recognized, and we can continue updating the
; source on a structural level even when the user is interfering
; with the code, without saving the chsanges

(defn set-value! 
  ([{:keys [local-state] :as owner} ace-instance value]
   (let [session (.. ace-instance -session)
         pos (.. ace-instance -selection getCursor)
         scroll (.. ace-instance -session getScrollTop)
        ; value (reduce-kv (fn [value [start end] changed]
        ;                     (str (subs value 0 start) changed (subs value (dec end))))
        ;                   value (:dirty-nodes local-state))
         ]
     (.. session -doc (setValue value))
     (.. ace-instance -selection (moveToPosition pos))
     (.. ace-instance -session (setScrollTop scroll))
     (.. ace-instance (resize true)))))

(defn save-handler [{:keys [local-state] :as owner} ace-state]
  (let [{:keys [on-save ace-instance]} @ace-state
        v (.getValue ace-instance)]
    (swap! ace-state assoc :edited? false :focused? false :value v)
    (on-save v)
    (rerender! owner {:dirty-nodes nil})))

(defn token-at [ace-instance pos]
  (.. ace-instance
      -session
      (getTokenAt (.-row pos) (.-column pos))))

(defn highlight-interval! [ace-instance interval]
  "Highlights the given interval within the editor
  and return a reference to the corresponding marker."
  (.. ace-instance 
      getSession 
      (addMarker interval 
                 "ace-code-highlight" "text")))

(defn clear-marker! [ace-instance marker-ref]
  (.. ace-instance getSession (removeMarker marker-ref)))

(defn get-token-bounds [ace-instance token]
  (let [{page-x :pageX page-y :pageY} (js->clj 
                                        (.. ace-instance -renderer (textToScreenCoordinates (:row token) (:start token))) 
                                        :keywordize-keys true)]
    {:position {:x page-x :y page-y}
     :extent {:y 12 :x (* 7 (count (:value token)))}}))

(defn set-token-selection! [ace-instance token]
  (let [sel (.. ace-instance getSelection)
        r (.. sel getRange)]
    (.. r (setStart (:row token) (:start token)))
    (.. r (setEnd (:row token) (+ (count (:value token)) (:start token))))
    (.. sel (setSelectionRange r))))

(defn set-token-value! [ace-instance token value]
  (let [session (.. ace-instance getSession)
        r (.. ace-instance getSelectionRange)]
    (.. session (remove r))
    (.. session (insert (.. r -start) (str value)))))

(defn get-numeric-token-at [ace-instance global-pos]
  (let [ace-pos1 (.. ace-instance -renderer (pixelToScreenCoordinates (:x global-pos) (:y global-pos)))
        ace-pos2 #js {:column (dec (.-column ace-pos1)) :row (.-row ace-pos1)}
        ace-pos3 #js {:column (inc (.-column ace-pos1)) :row (.-row ace-pos1)}
        tokens [(merge (some-> ace-instance (token-at ace-pos1) (js->clj :keywordize-keys true)) 
                       (js->clj ace-pos1 :keywordize-keys true)) 
                (merge (some-> ace-instance (token-at ace-pos2) (js->clj :keywordize-keys true))
                       (js->clj ace-pos2 :keywordize-keys true)) 
                (merge (some-> ace-instance (token-at ace-pos3) (js->clj :keywordize-keys true))
                       (js->clj ace-pos3 :keywordize-keys true))]]
    (some #(when (= (get % :type) "constant.numeric") %) tokens)))

(declare get-morph-node-at)

; (defn get-morph-node-at [source-map ace-instance client-pos]
;   (let [ace-pos (.. ace-instance -renderer (pixelToScreenCoordinates (:x client-pos) (:y client-pos)))
;         idx (.. ace-instance (posToIdx ace-pos))
;         {:keys [range ref]} (idx->range&ref idx source-map)]
;     (when range 
;       (let [[start end] range
;             idx->pos #(.. ace-instance getSession -doc (indexToPosition %))
;             start-pos (idx->pos start)
;             end-pos (idx->pos end)
;             r ( .. ace-instance getSelectionRange)
;         (.. r (setStart start-pos))
;         (.. r (setEnd end-pos))
;         {:range r :ref ref}))))

(defn restore-ace! [model]
  (prn "Restoring!")
  (let [ace-instance (.edit js/ace (model :id))]
    (.setValue ace-instance (@(model :ace-state) :value))))

(defn set-save-handler! [ace-instance on-save]
  (.. ace-instance
      -commands
      (addCommand  (clj->js {:name "save"
                             :bindKey {:win "Ctrl-S" :mac "Ctrl-S" :sender "editor|cli"}
                             :exec #(on-save (.getValue ace-instance))}))))

(defn set-change-handler! [ace-instance on-change]
  (.. ace-instance (on "change" on-change)))

(defn setup-ace! [self model]
  (let [ace-instance (.edit js/ace (model :id))
        clojure-mode (-> js/ace
                       (.require "ace/mode/clojure")
                       .-Mode)
        model (assoc model :ace-state (atom {:value (model :value)
                                             :ace-instance ace-instance}))]
    (.. ace-instance
        getSession
        (setMode (clojure-mode.)))
    (.. ace-instance 
        (setTheme (or (model :theme) "ace/theme/github")))
    (.. ace-instance
        -commands
        (addCommand (-> js/ace 
                      (.require "ace/commands/occur_commands")
                      .-occurStartCommand)))
    (.. ace-instance
        -commands
        (addCommands (.. js/ace -ext -lang -astCommands)))
    (.. ace-instance
        -renderer
        (setShowGutter (if (contains? model :line-numbers?)
                         (model :line-numbers?)
                         true)))
    (.. ace-instance
        -commands
        (addCommand  (clj->js {:name "save"
                               :bindKey {:win "Ctrl-S" :mac "Ctrl-S" :sender "editor|cli"}
                               :exec #(save-handler self ace-instance)})))
    (when (model :value) (set-value! self ace-instance (model :value)))
    (.gotoLine ace-instance (model :line) 1 true)
    (rerender! self {:edited-value (model :value)})))