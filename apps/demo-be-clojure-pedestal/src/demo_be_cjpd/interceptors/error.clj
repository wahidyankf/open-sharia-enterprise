(ns demo-be-cjpd.interceptors.error
  "Global error handling interceptor."
  (:require [cheshire.core :as json])
  (:refer-clojure :exclude [error-handler]))

(def error-handler
  "Interceptor that catches uncaught exceptions and returns JSON error responses."
  {:name  ::error-handler
   :error (fn [ctx ex]
            (let [data    (ex-data ex)
                  status  (or (:status data) 500)
                  message (or (:message data) (.getMessage ex) "Internal server error")]
              (assoc ctx :response
                     {:status  status
                      :headers {"Content-Type" "application/json"}
                      :body    (json/generate-string {:message message})})))})
