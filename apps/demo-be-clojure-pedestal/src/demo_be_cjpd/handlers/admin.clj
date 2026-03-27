(ns demo-be-cjpd.handlers.admin
  "Admin handlers for user management."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [demo-be-cjpd.auth.jwt :as jwt]
            [demo-be-cjpd.db.protocols :as proto]))

(defn- kebab->camel [s]
  (let [parts (str/split s #"-")]
    (str (first parts)
         (apply str (map str/capitalize (rest parts))))))

(defn- json-response [status body]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string body {:key-fn #(kebab->camel (name %))})})

(defn- error-response [status message]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string {:message message})})

(defn- user->public [user]
  (dissoc user :password-hash :failed-login-attempts))

(defn list-users-handler
  "GET /api/v1/admin/users — List all users with pagination and optional search."
  [user-repo]
  (fn [request]
    (let [params  (:query-params request)
          search  (or (:search params) (:email params)
                      (get params "search") (get params "email"))
          page    (Integer/parseInt (or (some-> params :page str) (get params "page") "1"))
          size    (Integer/parseInt (or (some-> params :size str) (get params "size") "20"))
          result  (proto/list-users user-repo {:search search :page page :size size})]
      (json-response 200 {:content        (mapv user->public (:data result))
                          :total-elements (:total result)
                          :page           (:page result)
                          :size           (:size result)}))))

(defn disable-user-handler
  "POST /api/v1/admin/users/:id/disable — Disable a user account."
  [user-repo token-repo]
  (fn [request]
    (let [user-id (get-in request [:path-params :id])
          user    (proto/find-user-by-id user-repo user-id)]
      (if-not user
        (error-response 404 "User not found")
        (let [updated (proto/update-status! user-repo user-id "DISABLED")]
          (proto/revoke-all-for-user! token-repo user-id)
          (json-response 200 (user->public updated)))))))

(defn enable-user-handler
  "POST /api/v1/admin/users/:id/enable — Enable a disabled user account."
  [user-repo]
  (fn [request]
    (let [user-id (get-in request [:path-params :id])
          user    (proto/find-user-by-id user-repo user-id)]
      (if-not user
        (error-response 404 "User not found")
        (let [updated (proto/update-status! user-repo user-id "ACTIVE")]
          (json-response 200 (user->public updated)))))))

(defn unlock-user-handler
  "POST /api/v1/admin/users/:id/unlock — Unlock a locked user account."
  [user-repo]
  (fn [request]
    (let [user-id (get-in request [:path-params :id])
          user    (proto/find-user-by-id user-repo user-id)]
      (if-not user
        (error-response 404 "User not found")
        (do
          (proto/update-status! user-repo user-id "ACTIVE")
          (proto/reset-failed-attempts! user-repo user-id)
          (let [updated (proto/find-user-by-id user-repo user-id)]
            (json-response 200 (user->public updated))))))))

(defn force-password-reset-handler
  "POST /api/v1/admin/users/:id/force-password-reset — Generate reset token."
  [config user-repo]
  (fn [request]
    (let [user-id (get-in request [:path-params :id])
          user    (proto/find-user-by-id user-repo user-id)]
      (if-not user
        (error-response 404 "User not found")
        (let [reset-token (jwt/sign-access-token (:jwt-secret config)
                                                  user-id
                                                  (:username user)
                                                  "RESET")]
          (json-response 200 {:token   reset-token
                              :user-id user-id}))))))
