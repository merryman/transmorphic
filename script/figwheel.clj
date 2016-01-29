(require '[figwheel-sidecar.repl :as r]
         '[figwheel-sidecar.repl-api :as ra]
         '[clojure.pprint :refer [pprint]]
         '[clojure.java.io :refer [writer file]])

; this stores the app state that should
; be kept in sync among all connected clients
; enables a simplified collaborative environment
(def global-app-state (atom {}))

(defn handler [{:keys [uri body request-method]}]
  (when (= :post request-method)
    ; if the app state is updated, incorporate and
    ; push to all connected clients
    (let [new-file-contents (slurp body)]
      (with-open [wrtr (writer (str "./resources/public/" uri))]
          (.write wrtr new-file-contents)))))

(ra/start-figwheel!
  {:figwheel-options {:http-server-root "public"
                      :ring-handler handler}
   :build-ids ["dev"]
   :all-builds
   [{:id "dev"
     :figwheel {:on-jsload "transmorphic.core/reload-hook"}
     :source-paths ["src/cljs" "src/cljc" "src/clj"]
     :compiler {:main 'examples.playground
                :optimizations :none
                :cache-analysis true 
                :asset-path "js"
                :output-to "resources/public/js/transmorphic.js"
                :output-dir "resources/public/js"
                :verbose true}}]})

(ra/cljs-repl)