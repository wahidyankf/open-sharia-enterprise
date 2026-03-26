(ns demo-be-cjpd.main
  "Application entry point."
  (:require [demo-be-cjpd.config :as config]
            [demo-be-cjpd.db.core :as db]
            [demo-be-cjpd.server :as server]
            [migratus.core :as migratus])
  (:gen-class))

(defn- migratus-config
  "Build Migratus configuration from the database URL."
  [database-url]
  {:store         :database
   :migration-dir "migrations/"
   :db            {:connection-uri database-url
                   :user           (or (System/getenv "DB_USER") "demo_be_cjpd")
                   :password       (or (System/getenv "DB_PASSWORD") "demo_be_cjpd")}})

(defn -main
  "Start the demo-be-cjpd Pedestal application."
  [& _args]
  (let [cfg (config/load-config)
        ds  (db/create-datasource (:database-url cfg))]
    (migratus/migrate (migratus-config (:database-url cfg)))
    (let [srv (server/create-server cfg ds)]
      (server/start! srv)
      (println (str "Server started on port " (:port cfg)))
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. (fn [] (server/stop! srv)))))))
