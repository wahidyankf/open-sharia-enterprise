(ns demo-be-cjpd.handlers.user
  "User profile handlers."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [demo-be-cjpd.auth.password :as password]
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

(defn get-profile-handler
  "GET /api/v1/users/me — Return the authenticated user's profile."
  [user-repo]
  (fn [request]
    (let [user-id (:user-id (:identity request))
          user    (proto/find-user-by-id user-repo user-id)]
      (if user
        (json-response 200 (user->public user))
        (error-response 404 "User not found")))))

(defn update-profile-handler
  "PATCH /api/v1/users/me — Update display name."
  [user-repo]
  (fn [request]
    (let [user-id      (:user-id (:identity request))
          params       (:json-params request)
          display-name (or (:displayName params) (:display_name params) (:display-name params))
          updated      (proto/update-display-name! user-repo user-id display-name)]
      (json-response 200 (user->public updated)))))

(defn change-password-handler
  "POST /api/v1/users/me/password — Change the authenticated user's password."
  [user-repo]
  (fn [request]
    (let [user-id  (:user-id (:identity request))
          params   (:json-params request)
          old-pw   (or (:oldPassword params) (:old_password params) (:old-password params))
          new-pw   (or (:newPassword params) (:new_password params) (:new-password params))
          user     (proto/find-user-by-id user-repo user-id)]
      (cond
        (not (password/verify-password old-pw (:password-hash user)))
        (error-response 401 "Invalid credentials")

        :else
        (do
          (proto/update-password! user-repo user-id (password/hash-password new-pw))
          {:status  200
           :headers {"Content-Type" "application/json"}
           :body    "{\"message\":\"Password changed successfully\"}"})))))

(defn deactivate-handler
  "POST /api/v1/users/me/deactivate — Self-deactivate account."
  [user-repo]
  (fn [request]
    (let [user-id (:user-id (:identity request))]
      (proto/update-status! user-repo user-id "INACTIVE")
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    "{\"message\":\"Account deactivated successfully\"}"})))
