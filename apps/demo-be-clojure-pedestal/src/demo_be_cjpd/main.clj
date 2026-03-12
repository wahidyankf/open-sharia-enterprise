(ns demo-be-cjpd.main
  "Application entry point."
  (:require [demo-be-cjpd.config :as config]
            [demo-be-cjpd.db.core :as db]
            [demo-be-cjpd.db.schema :as schema]
            [demo-be-cjpd.server :as server])
  (:gen-class))

(defn -main
  "Start the demo-be-cjpd Pedestal application."
  [& _args]
  (let [cfg (config/load-config)
        ds  (db/create-datasource (:database-url cfg))]
    (schema/create-schema! ds (:database-url cfg))
    (let [srv (server/create-server cfg ds)]
      (server/start! srv)
      (println (str "Server started on port " (:port cfg)))
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. (fn [] (server/stop! srv)))))))
