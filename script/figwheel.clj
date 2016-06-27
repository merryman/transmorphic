(use 'org.httpkit.server)
(require '[figwheel-sidecar.repl :as r]
         '[figwheel-sidecar.repl-api :as ra]
         '[ring.middleware.edn]
         '[clojure.pprint :refer [pprint]]
         '[clojure.java.io :refer [writer file]])

; this stores the app state that should
; be kept in sync among all connected clients
; enables a simplified collaborative environment
(def global-app-state (atom {}))

; maybe re-user the sessions from figwheel, to recieve and propagate changes
; among all open sessions?

(def sessions (atom #{}))

(defn generate-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/edn"}
   :body (pr-str data)})

(defn apply-txs [app-state txs]
  (reduce (fn [state [ref {:keys [added removed props]}]]
            (update-in state ref 
                       update-in [:txs] ))
          txs app-state))

(defn set-app-state [params origin]
  (prn origin)
  (swap! global-app-state (apply-txs (:new-txs params)))
  (doseq [client (keys @sessions)]
      (when (not= client origin) (send! client (pr-str params))))
  (generate-response {:status :ok}))

(defn updater [req]
  (with-channel req up-chan
    (swap! sessions conj up-chan)
    (on-receive up-chan (fn [req] 
                          (prn "websocket request: " req)
                          (set-app-state (read-string req) up-chan)))
    (on-close up-chan (fn [status]
                        (swap! sessions disj up-chan)
                        (prn up-chan "closed, status" status)))))

(defn update-app-state [{:keys [ref->txs]} sender]
  ; apply the transactions onto the global state, and then
  ; broadcast these txs to all other sessions
  (prn "Received operations " (prn ref->txs) " from " sender)
  ; (reset! global-app-state (apply new-state ref->txs))
  ; broadcast state to all other sessions
  )

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