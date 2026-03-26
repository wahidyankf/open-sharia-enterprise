(ns demo-be-cjpd.main
  "Application entry point."
  (:require [demo-be-cjpd.config :as config]
            [demo-be-cjpd.db.core :as db]
            [demo-be-cjpd.server :as server]
            [migratus.core :as migratus])
  (:gen-class))

(defn- migratus-config
  "Build Migratus configuration from the database URL.
   Embeds user and password as query parameters because Migratus passes
   the :db map directly to next.jdbc which requires credentials in the URI."
  [database-url]
  (let [user     (or (System/getenv "DB_USER") "demo_be_cjpd")
        password (or (System/getenv "DB_PASSWORD") "demo_be_cjpd")
        sep      (if (.contains database-url "?") "&" "?")
        uri      (str database-url sep "user=" user "&password=" password)]
    {:store         :database
     :migration-dir "migrations"
     :db            {:connection-uri uri}}))

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
