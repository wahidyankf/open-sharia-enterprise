(ns demo-be-cjpd.handlers.health
  "Health check handler.")

(defn health-handler
  "Handler for GET /health."
  [_request]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    "{\"status\":\"UP\"}"})
