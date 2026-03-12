(ns demo-be-cjpd.handlers.user
  "User profile handlers."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [demo-be-cjpd.auth.password :as password]
            [demo-be-cjpd.db.user-repo :as user-repo]))

(defn- json-response [status body]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string body {:key-fn #(str/replace (name %) #"-" "_")})})

(defn- error-response [status message]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string {:error message})})

(defn- user->public [user]
  (dissoc user :password-hash :failed-login-attempts))

(defn get-profile-handler
  "GET /api/v1/users/me — Return the authenticated user's profile."
  [ds]
  (fn [request]
    (let [user-id (:user-id (:identity request))
          user    (user-repo/find-by-id ds user-id)]
      (if user
        (json-response 200 (user->public user))
        (error-response 404 "User not found")))))

(defn update-profile-handler
  "PATCH /api/v1/users/me — Update display name."
  [ds]
  (fn [request]
    (let [user-id      (:user-id (:identity request))
          params       (:json-params request)
          display-name (or (:display_name params) (:display-name params))
          updated      (user-repo/update-display-name! ds user-id display-name)]
      (json-response 200 (user->public updated)))))

(defn change-password-handler
  "POST /api/v1/users/me/password — Change the authenticated user's password."
  [ds]
  (fn [request]
    (let [user-id  (:user-id (:identity request))
          params   (:json-params request)
          old-pw   (or (:old_password params) (:old-password params))
          new-pw   (or (:new_password params) (:new-password params))
          user     (user-repo/find-by-id ds user-id)]
      (cond
        (not (password/verify-password old-pw (:password-hash user)))
        (error-response 401 "Invalid credentials")

        :else
        (do
          (user-repo/update-password! ds user-id (password/hash-password new-pw))
          {:status  200
           :headers {"Content-Type" "application/json"}
           :body    "{\"message\":\"Password changed successfully\"}"})))))

(defn deactivate-handler
  "POST /api/v1/users/me/deactivate — Self-deactivate account."
  [ds]
  (fn [request]
    (let [user-id (:user-id (:identity request))]
      (user-repo/update-status! ds user-id "INACTIVE")
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    "{\"message\":\"Account deactivated successfully\"}"})))
