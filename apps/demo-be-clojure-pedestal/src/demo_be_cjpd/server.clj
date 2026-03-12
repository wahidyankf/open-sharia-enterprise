(ns demo-be-cjpd.server
  "Pedestal server creation and lifecycle."
  (:require [io.pedestal.http :as http]
            [demo-be-cjpd.routes :as routes]))

(defn create-server
  "Create a configured Pedestal HTTP server."
  [config ds]
  (http/create-server
    {::http/routes         (routes/make-routes config ds)
     ::http/type           :jetty
     ::http/port           (:port config)
     ::http/host           "0.0.0.0"
     ::http/join?          false
     ;; Use linear-search so static routes (/summary) match before
     ;; parameterised routes (/:id) regardless of iteration order.
     ::http/router         :linear-search
     ::http/container-options
     {:context-configurator
      (fn [ctx]
        (.setMaxFormContentSize ctx (* 20 1024 1024))
        ctx)}}))

(defn start!
  "Start the Pedestal server. Returns the started server."
  [server]
  (http/start server))

(defn stop!
  "Stop the Pedestal server."
  [server]
  (http/stop server))
