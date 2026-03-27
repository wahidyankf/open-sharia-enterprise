(ns demo-be-cjpd.handlers.test-api
  "Test-only API handlers gated by ENABLE_TEST_API=true env var.
   These endpoints are used by FE E2E tests to reset state between runs."
  (:require [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [demo-be-cjpd.db.protocols :as proto]))

(defn- json-response [status body]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string body)})

(defn- error-response [status message]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string {:message message})})

(defn reset-db-handler!
  "POST /api/v1/test/reset-db — Delete all data in dependency order.
   Deletion order: attachments → expenses → revoked_tokens → users.
   Takes a raw datasource for direct truncation (test infra only)."
  [ds]
  (fn [_request]
    (jdbc/execute! ds ["DELETE FROM attachments"])
    (jdbc/execute! ds ["DELETE FROM expenses"])
    (jdbc/execute! ds ["DELETE FROM revoked_tokens"])
    (jdbc/execute! ds ["DELETE FROM users"])
    (json-response 200 {:message "Database reset successful"})))

(defn promote-admin-handler!
  "POST /api/v1/test/promote-admin — Set a user's role to ADMIN by username.
   Body: {\"username\": \"...\"}
   Returns 200 with message or 404 if user not found."
  [ds user-repo]
  (fn [request]
    (let [params   (or (:json-params request) {})
          username (:username params)]
      (if-not username
        (error-response 400 "Missing required field: username")
        (let [user (proto/find-user-by-username user-repo username)]
          (if-not user
            (error-response 404 (str "User not found: " username))
            (do
              (jdbc/execute! ds
                             ["UPDATE users SET role = 'ADMIN' WHERE username = ?"
                              username])
              (json-response 200 {:message (str "User " username " promoted to ADMIN")}))))))))
