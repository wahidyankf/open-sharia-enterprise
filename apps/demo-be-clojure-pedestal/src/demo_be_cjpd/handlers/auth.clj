(ns demo-be-cjpd.handlers.auth
  "Authentication handlers: register, login, refresh, logout, logout-all.
   Request/response schemas: see domain.schemas (RegisterRequest, LoginRequest,
   RefreshRequest, TokenResponse)."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [demo-be-cjpd.auth.jwt :as jwt]
            [demo-be-cjpd.auth.password :as password]
            [demo-be-cjpd.db.protocols :as proto]
            [demo-be-cjpd.domain.user :as user-domain]))

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

(defn register-handler
  "POST /api/v1/auth/register — Register a new user."
  [_config user-repo]
  (fn [request]
    (let [params   (:json-params request)
          username (:username params)
          email    (:email params)
          password (:password params)]
      (cond
        (not (user-domain/valid-username? username))
        (error-response 400 "Invalid username format")

        (not (user-domain/valid-email? email))
        {:status  400
         :headers {"Content-Type" "application/json"}
         :body    (json/generate-string {:message "Invalid email format" :field "email"})}

        :else
        (let [pw-error (user-domain/validate-password-strength password)]
          (if pw-error
            {:status  400
             :headers {"Content-Type" "application/json"}
             :body    (json/generate-string {:message (:message pw-error) :field "password"})}
            (do
              (when (proto/find-user-by-username user-repo username)
                (throw (ex-info "Username already exists"
                                {:status 409
                                 :message "Username already exists"})))
              (when (proto/find-user-by-email user-repo email)
                (throw (ex-info "Email already exists"
                                {:status 409
                                 :message "Email already registered"})))
              (let [count  (proto/count-users user-repo)
                    role   (if (zero? count) "ADMIN" "USER")
                    hash   (password/hash-password password)
                    user   (proto/create-user! user-repo
                                               {:username      username
                                                :email         email
                                                :password-hash hash
                                                :display-name  username
                                                :role          role
                                                :status        "ACTIVE"})]
                (json-response 201 (user->public user))))))))))

(defn login-handler
  "POST /api/v1/auth/login — Authenticate a user and return tokens."
  [config user-repo]
  (fn [request]
    (let [params   (:json-params request)
          username (:username params)
          pw       (:password params)
          user     (proto/find-user-by-username user-repo username)]
      (cond
        (nil? user)
        (error-response 401 "Invalid credentials")

        (= "INACTIVE" (:status user))
        (error-response 401 "User account is deactivated")

        (= "DISABLED" (:status user))
        (error-response 403 "User account is disabled")

        (= "LOCKED" (:status user))
        (error-response 401 "Account is locked due to too many failed login attempts")

        (not (password/verify-password pw (:password-hash user)))
        (let [updated (proto/increment-failed-attempts! user-repo (:id user))]
          (if (= "LOCKED" (:status updated))
            (error-response 401 "Account is locked due to too many failed login attempts")
            (error-response 401 "Invalid credentials")))

        :else
        (do
          (proto/reset-failed-attempts! user-repo (:id user))
          (let [access-token  (jwt/sign-access-token (:jwt-secret config)
                                                     (:id user)
                                                     (:username user)
                                                     (:role user))
                refresh-token (jwt/sign-refresh-token (:jwt-secret config) (:id user))]
            (json-response 200 {:access-token  access-token
                                :refresh-token refresh-token
                                "tokenType"    "Bearer"
                                :user          (user->public user)})))))))

(defn refresh-handler
  "POST /api/v1/auth/refresh — Refresh tokens using a refresh token."
  [config user-repo token-repo]
  (fn [request]
    (let [params        (:json-params request)
          refresh-token (or (:refreshToken params) (:refresh_token params) (:refresh-token params))
          claims        (jwt/verify-token (:jwt-secret config) refresh-token)]
      (if-not claims
        (error-response 401 "Token has expired or is invalid")
        (let [jti     (:jti claims)
              user-id (:sub claims)
              type    (:type claims)]
          (if (not= "refresh" type)
            (error-response 401 "Invalid token type")
            (if (proto/token-revoked? token-repo jti)
              (error-response 401 "Token is invalid or already used")
              (if (proto/all-revoked-for-user? token-repo user-id (or (:iat claims) 0))
                (error-response 401 "Token has been revoked")
                (let [user (proto/find-user-by-id user-repo user-id)]
                  (if-not user
                    (error-response 401 "User not found")
                    (cond
                      (= "INACTIVE" (:status user))
                      (error-response 401 "User account is deactivated")

                      (= "DISABLED" (:status user))
                      (error-response 401 "User account is disabled")

                      (= "LOCKED" (:status user))
                      (error-response 401 "Account is locked")

                      :else
                      (do
                        (proto/revoke-token! token-repo jti user-id)
                        (let [new-access  (jwt/sign-access-token (:jwt-secret config)
                                                                 user-id
                                                                 (:username user)
                                                                 (:role user))
                              new-refresh (jwt/sign-refresh-token (:jwt-secret config) user-id)]
                          (json-response 200 {:access-token  new-access
                                              :refresh-token new-refresh
                                              "tokenType"    "Bearer"}))))))))))))))

(defn logout-handler
  "POST /api/v1/auth/logout — Revoke the provided access token."
  [config token-repo]
  (fn [request]
    (let [params     (:json-params request)
          auth-token (or (some-> (get-in request [:headers "authorization"])
                                 (str/replace #"^Bearer " ""))
                         (:access-token params)
                         (:accessToken params))
          claims     (when auth-token (jwt/verify-token (:jwt-secret config) auth-token))]
      (when claims
        (proto/revoke-token! token-repo (:jti claims) (:sub claims)))
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    "{\"message\":\"Logged out successfully\"}"})))

(defn logout-all-handler
  "POST /api/v1/auth/logout-all — Revoke all tokens for the authenticated user."
  [_config token-repo]
  (fn [request]
    (let [identity (:identity request)
          user-id  (:user-id identity)
          jti      (:jti identity)]
      (proto/revoke-all-for-user! token-repo user-id)
      (proto/revoke-token! token-repo jti user-id)
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    "{\"message\":\"Logged out from all devices\"}"})))
