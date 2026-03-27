(ns demo-be-cjpd.server
  "Pedestal server creation and lifecycle."
  (:require [io.pedestal.http :as http]
            [demo-be-cjpd.db.jdbc-user-repo :as jdbc-user-repo]
            [demo-be-cjpd.db.jdbc-expense-repo :as jdbc-expense-repo]
            [demo-be-cjpd.db.jdbc-attachment-repo :as jdbc-attachment-repo]
            [demo-be-cjpd.db.jdbc-token-repo :as jdbc-token-repo]
            [demo-be-cjpd.routes :as routes]))

(defn create-server
  "Create a configured Pedestal HTTP server."
  [config ds]
  (let [user-repo       (jdbc-user-repo/create ds)
        expense-repo    (jdbc-expense-repo/create ds)
        attachment-repo (jdbc-attachment-repo/create ds)
        token-repo      (jdbc-token-repo/create ds)]
    (http/create-server
      {::http/routes         (routes/make-routes config ds user-repo expense-repo attachment-repo token-repo)
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
          ctx)}})))

(defn start!
  "Start the Pedestal server. Returns the started server."
  [server]
  (http/start server))

(defn stop!
  "Stop the Pedestal server."
  [server]
  (http/stop server))
