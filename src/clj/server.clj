(ns transmorphic.server)

(defn handler [request]
  (prn request)
  {:status  200
   :headers {"Content-Type" "text/plain"}
   :body    "Hello World"})