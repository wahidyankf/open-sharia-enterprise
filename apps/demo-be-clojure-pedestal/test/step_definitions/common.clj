(ns step-definitions.common
  "Shared utilities for step definitions."
  (:require [clj-http.client :as http]
            [cheshire.core :as json]
            [demo-be-cjpd.config :as config]
            [demo-be-cjpd.db.core :as db]
            [demo-be-cjpd.db.schema :as schema]
            [demo-be-cjpd.server :as server])
  (:import (java.net ServerSocket)
           (java.util UUID)))

(defn find-free-port []
  (with-open [ss (ServerSocket. 0)]
    (.getLocalPort ss)))

(defn start-test-server! []
  (let [port    (find-free-port)
        ;; Use a unique named in-memory database with shared cache so that
        ;; all connections in the HikariCP pool see the same data.
        db-name (str "testdb_" (str (UUID/randomUUID)))
        db-url  (str "jdbc:sqlite:file:" db-name "?mode=memory&cache=shared")
        cfg     (assoc (config/load-config) :port port :database-url db-url)
        ds      (db/create-datasource db-url)
        _       (schema/create-schema! ds db-url)
        srv     (server/create-server cfg ds)
        started (server/start! srv)]
    {:server  started
     :port    port
     :ds      ds
     :config  cfg
     :base-url (str "http://localhost:" port)}))

(defn stop-test-server! [{:keys [server ds]}]
  (server/stop! server)
  (.close ds))

(defn base-url [{:keys [base-url]}]
  base-url)

(defn parse-json [body]
  (try
    (json/parse-string body true)
    (catch Exception _
      {})))

(defn register-user! [ctx username & {:keys [email password display-name]
                                       :or   {email    (str username "@example.com")
                                              password "Str0ng#Pass1"}}]
  (let [resp (http/post (str (base-url ctx) "/api/v1/auth/register")
                        {:body             (json/generate-string
                                             {:username    username
                                              :email       email
                                              :password    password
                                              :displayName (or display-name username)})
                         :content-type     :json
                         :throw-exceptions false})
        body (parse-json (:body resp))]
    (assoc ctx
           (keyword (str username "-user")) body
           (keyword (str username "-id")) (:id body)
           ;; Store the registered password so login-user! can use it
           (keyword (str username "-password")) password)))

(defn login-user! [ctx username & {:keys [password]}]
  (let [pw   (or password
                 (get ctx (keyword (str username "-password")))
                 "Str0ng#Pass1")
        resp (http/post (str (base-url ctx) "/api/v1/auth/login")
                        {:body             (json/generate-string {:username username
                                                                  :password pw})
                         :content-type     :json
                         :throw-exceptions false})
        body (parse-json (:body resp))]
    (assoc ctx
           (keyword (str username "-access-token"))  (:access_token body)
           (keyword (str username "-refresh-token")) (:refresh_token body)
           (keyword (str username "-login-response")) body)))

(defn auth-header [ctx username]
  {"Authorization" (str "Bearer " (get ctx (keyword (str username "-access-token"))))})

(defn get-user-id [ctx username]
  (get ctx (keyword (str username "-id"))))

(defn get-access-token [ctx username]
  (get ctx (keyword (str username "-access-token"))))

(defn get-refresh-token [ctx username]
  (get ctx (keyword (str username "-refresh-token"))))
