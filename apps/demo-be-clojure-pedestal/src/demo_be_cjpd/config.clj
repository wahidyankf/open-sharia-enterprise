(ns demo-be-cjpd.config
  "Load configuration from environment variables.")

(defn load-config
  "Return application configuration from environment variables."
  []
  {:port         (Integer/parseInt (or (System/getenv "PORT") "8201"))
   :database-url (or (System/getenv "DATABASE_URL") "jdbc:sqlite::memory:")
   :jwt-secret   (or (System/getenv "APP_JWT_SECRET") "default-dev-secret-change-in-production")})
