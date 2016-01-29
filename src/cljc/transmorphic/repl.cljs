(ns transmorphic.repl
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.reader :refer [read-string]]
            [cljs.pprint :refer [pprint]]
            [cljs.tools.reader.reader-types :as trt]
            [cljs.tools.reader :as tr]
            [cljs.js]
            [cljs.core.async :as async :refer [>! <! put! chan timeout onto-chan close!]]
            [clojure.string :as s]
            [goog.string :refer [StringBuffer]]
            [goog.net.XhrIo :as xhr])
  (:import [goog.net XhrIo]
           [goog.events EventType]
           [goog.string.StringBuffer]))

(def current-namespace (atom nil))

(def *closure-index-mem* (atom {}))

(def morph-libs
    {'cljs-morphic.morph :cljs
     'cljs-morphic.morph.window :cljs
     'cljs-morphic.morph.editor :cljs
     'cljs-morphic.test :cljs
     'cljs-morphic.helper :cljc
     'cljs-morphic.macros :clj
     'cljs-morphic.init-macro :clj
     'cljs.analyzer :cljc
     'goog.net.XhrIo :js
     'goog.events :js})

(defn get-file [url]
  (let [ch (chan 1)]
    (xhr/send url
              (fn [event]
                (if (.. event -target isSuccess)
                  (let [res (-> event .-target .getResponseText)]
                    (go (>! ch res)
                        (close! ch)))
                  (go (>! ch :error)
                        (close! ch)
                  ))))
    ch))

(defn write-file [url, new-source]
  (let [ch (chan 1)]
    (xhr/send url
              (fn [event]
                (if (.. event -target isSuccess)
                  (let [res (-> event .-target .getResponseText)]
                    (go (>! ch res)
                        (close! ch)))
                  (go (>! ch :error)
                        (close! ch)
                  )))
              "POST"
              new-source)
    ch))

(defn get-ns-source [ns-name cb]
  (go
   (cb (<! (get-file (str (cljs.js/ns->relpath ns-name) ".cljs"))))))

(defn update-ns-source! [ns-name new-source cb]
  (go
   (cb (<! (write-file (str (cljs.js/ns->relpath ns-name) ".cljs") new-source)))))

(def file-cache (atom {}))

(defn extension->lang
  [extension]
  (if (= ".js" extension)
    :js
    :clj))

(defn load-and-callback!
  [name lang cb]
  (go
   (let [lang (if (contains? #{'cljs.analyzer} name) ".js" lang)
         path  (str (cljs.js/ns->relpath name) lang)
         path  (case lang
                 ".cljs" path
                 ".cljc" (str "bootstrap-safe/" path)
                 ".js" (str "cloxp-cljs-build/out/" path)
                 ".clj" (cond 
                          (re-matches #"^cljs_morphic/.*" path) (str "src/clj/" path)
                          ; (re-matches #"^reagent/.*" path) (str "reagent/src/" path)
                          :else (str "clojurescript/src/main/clojure/" path)))
         file (or (get @file-cache path)
                  (do
                    (swap! file-cache assoc path (<! (get-file path)))
                    (get @file-cache path)))]
     (if (= :error file)
       :error
       (cb {:lang (extension->lang lang) :source file})))))

(defn- skip-load?
  [{:keys [name macros]}]
  (cond
    (= name 'cljs.core) true
    (and (= name 'cljs.pprint) macros) true
    :else false))

(defn load-existing [cb {:keys [name macros]}]
  (cond
    (= name 'cljs.core) (go (cb {:lang :js
                                 :source (<! (get-file 
                                              "cloxp-cljs-build/out/cljs/core.js"))}))
    (and (= name 'cljs.pprint) macros) true
    :else false))

(defn- do-load-file
  [file cb]
  (when-not (load-and-callback! file :clj cb)
    (cb nil)))

(defn closure-index
  []
  (go
   (let [paths-to-provides
        (map (fn [[_ path provides]]
               [path (map second
                       (re-seq #"'(.*?)'" provides))])
          (re-seq #"\ngoog\.addDependency\('(.*)', \[(.*?)\].*"
                  (<! (get-file "cloxp-cljs-build/out/goog/deps.js"))))]
    (into {}
      (for [[path provides] paths-to-provides
            provide provides]
                [(symbol provide) (str "goog/" (second (re-find #"(.*)\.js$" path)))])))))

(defn- do-load-goog
  [name cb]
  (if-let [goog-path (get @*closure-index-mem* name)]
    (go 
     (let [file (<! (get-file 
                     (str "cloxp-cljs-build/out/" goog-path ".js")))]
       (if (= :error file)
         (cb nil)
         (cb {:lang :js :source file}))))
    (cb nil)))



(defn- do-load-other
  [name macros cb]
  (go-loop [extensions (if macros
                      [".clj" ".cljc"]
                      [".cljs" ".cljc" ".js"])]
    (if extensions
      (when (= :error (<! (load-and-callback!
                           name
                           (first extensions)
                           cb)))
        (recur (do
                 (prn "failed loading" name "retrying with" (first (next extensions)))
                 (next extensions))))
      (cb nil))))

(def foreign-libs 
  {"cljsjs/react" "react.inc.js"
   "cljsjs/react/dom" "react-dom.inc.js"
   "cljsjs/react/dom/server" "react-dom-server.inc.js"})

(defn load-foreign-lib [path cb]
  (if
    (contains? foreign-libs path) 
    (go (cb {:lang :js
             :source (<! (get-file (str "cloxp-cljs-build/out/" (get foreign-libs path))))}))
    (cb nil)))

(defn load-for-macro [path cb]
  (prn "loading for macro: " path)
  (go 
   (let [source (<! (get-file (str "bootstrap-safe/" path ".clj")))
         source (if (= source :error) 
                  (<! (get-file (str "bootstrap-safe/" path ".cljc")))
                  source)]
     (if source 
       (cb {:lang :clj
            :source source})
       (cb nil)))))

(defn is-foreign? [path]
  (re-matches #"^cljsjs/.*" path))

; file here is an alternate parameter denoting a filesystem path
(defn load
  [{:keys [name macros path file] :as full} cb]
  (prn "LOADING " path " macros? " macros)
  (cond
    macros (load-for-macro path cb)
    (is-foreign? path) (load-foreign-lib path cb)
    (skip-load? full) (load-existing cb full)
    file (do-load-file file cb)
    (re-matches #"^goog/.*" path) (do-load-goog name cb)
    :else (do-load-other name macros cb)))

(defn load-cache
  ([cstate s] (load-cache cstate s {}))
  ([cstate s opts]
   (let [ext (or (:ext opts) :cljs)]
     (go
      (let [path (str "cloxp-cljs-build/out/" (cljs.js/ns->relpath s) "." (name ext) ".cache.edn")
            cache-edn (<! (get-file path))
            cache (read-string cache-edn)]
        (cljs.js/load-analysis-cache! cstate s cache)
        cache)))))

; stolen from rksm.cloxp-source-reader

(defn- line-column-access
  [string]
  (let [lines (s/split-lines string)]
    (fn line-column-access-for-string
      [{start-line :line start-column :column :as s} {end-line :line end-column :column :as e}]
      (if (or (> start-line end-line) (and (= start-line end-line) (> start-column end-column)))
        (line-column-access-for-string e s)
        (let [start  (nth lines (dec start-line))
              start (.substring start (dec start-column))
              end  (if (= start-line end-line)
                     start
                     (nth lines (min (dec (count lines)) (dec end-line))))
              end (.substring end 0 (if (= start-line end-line) (- (dec end-column) (dec start-column)) (dec end-column)))
              inbetween (->> lines (drop start-line) (take (dec (- end-line start-line))))]
          (s/join "\n"
                  (if (= start-line end-line)
                    [end]
                    (concat [start] inbetween [end]))))))))

(defn make-reader
  "Currently using direct instantiation of reader b/c there seems to be a bug
  in tools reader regarding line number access, see comment"
  [source & [file-name]]
  (trt/source-logging-push-back-reader source 1))

(defn name-of-def
  [form]
  (first (drop 1 (filter symbol? form))))

(defn defmethod?
  [form]
  (and (seq? form)
       (= 'defmethod (first form))))

(defn defmulti?
  [form]
  (and (seq? form)
       (= 'defmulti (first form))))

(defn defmorph?
  [form]
  (and (seq? form)
       (= 'defmorph (first form))))

(defn purge-string!
  [rdr]
  (let [buf (-> rdr .-frames deref :buffer)
        str (.toString buf)]
    (.clear buf)
    str))

(defn read-with-source-logger
  "reads a single next obj from *current-code* :source"
  [src]
  (let [rdr (make-reader src)]
    (tr/read {:read-cond :allow} rdr)))

(defn read-objs
  "Reads sexps from source and returns them as a {:form :source :line
  :column} map. Note: this is more that the typical reader gives us."
  [source & [{:keys [features line-offset column-offset] :as opts}]]
  ; FIXME this is hacked...
  (let [source (if-not (.endsWith source "\n") (str source "\n") source)
        get-src-fn (line-column-access source)
        rdr (make-reader source)
        line-offset (or line-offset 0)
        column-offset (or column-offset 0)
        reader-opts (if features
                      {:eof nil :read-cond :allow :features (set features)}
                      {:eof nil :read-cond :preserve})]
    (loop [result []]
      (let [start-line (trt/get-line-number rdr)
            start-column (trt/get-column-number rdr)]
        (if-let [o (tr/read reader-opts rdr)]
          (let [; get the string from the reader:
                raw-str (purge-string! rdr)
                lines (s/split-lines raw-str)
                ; trim surrounding whitespace and offset line / column accordingly
                ws-lines (take-while #(re-find #"^\s*(;.*)?$" %) lines)
                src-lines (drop (count ws-lines) lines)
                [_ leading-ws first-line-content] (re-matches #"^(\s*)(.*)" (first src-lines))
                src-lines (assoc (vec src-lines) 0 first-line-content)
                src (s/join "\n" src-lines)
                line (+ start-line (count ws-lines))
                column (+ (if (> (count ws-lines) 0) 1 start-column) (count leading-ws))
                meta (meta o)
                defmorph? (defmorph? o)
                name (if defmorph? (name-of-def o))]
            (when (= \newline (trt/peek-char rdr))
              (trt/read-char rdr)
              (purge-string! rdr))
            (let [start {:line line :column column}
                  start-for-meta {:line (+ line line-offset)
                                  :column (if (= 1 start-line) (+ column column-offset) column)}
                  end-line (trt/get-line-number rdr)
                  end-column (trt/get-column-number rdr)
                  end {:line end-line :column end-column}
                  end-for-meta {:end-line (+ end-line line-offset)
                                :end-column (if (= 1 start-line) (+ end-column column-offset) end-column)}
                  source (get-src-fn start end)]
              (recur (conj result
                           (merge start-for-meta
                                  end-for-meta
                                  {:form o, :source source}
                                  (if defmorph?
                                    {:form (with-meta o (assoc meta :source src)),
                                     :name name}))))))
          result)))))

(defn get-morph-essay-info [name cb]
  (load-and-callback! 
   name 
   ".cljs" 
   #(cb (read-objs (:source %)))))

(def st (cljs.js/empty-state))

(defn morph-eval [m]
  (cljs.js/eval 
  st
  m
  {:eval cljs.js/js-eval
   :ns 'cljs-morphic.playground
   :context :expr
   :verbose true}
  (fn [c]
    ;(pprint c)
    c)))

(defn morph-eval-str 
  ([code]
   (morph-eval-str code 'cljs-morphic.playground))
  ([code ns]
   (reset! current-namespace ns)
   (cljs.js/eval-str 
    st
    code
    ns
    {:eval cljs.js/js-eval
     :context :expr
     :load load}
    (fn [c]
      (if-let [err (:error c)]
        c 
        (:value c))))))

(defn morph-defn [form ns]
  (reset! current-namespace ns)
  (cljs.js/eval 
     st
     form
     {:eval cljs.js/js-eval
      :ns ns
      :context :expr
      :verbose true
      :def-emits-var true}
     (fn [c]
       c)))

(defn init-compiler [cb]
  (set! (.-isProvided_ js/goog) (fn [name] false))
  (let [finished (atom false)]
    (go
     (reset! *closure-index-mem* (<! (closure-index)))
     (cljs.js/eval
     st
     '(ns cljs.user
        (:require-macros [cljs-morphic.core]))
     {:eval cljs.js/js-eval
      :load load
      :ns 'cljs.user
      :context :expr}
     (fn [c] 
       (go
        (<! (load-cache st 'transmorphic.playground))
        (<! (load-cache st 'cljs.analyzer {:ext :cljc}))
        (<! (load-cache st 'transmorphic.core {:ext :cljc}))
        (reset! finished true)
        (cb c)))))
    finished))