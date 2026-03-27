(ns demo-be-cjpd.interceptors.auth
  "Authentication interceptors for Pedestal.
   Identity schema: see domain.schemas/Identity."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [demo-be-cjpd.auth.jwt :as jwt]
            [demo-be-cjpd.db.protocols :as proto]
            [demo-be-cjpd.domain.schemas :as schemas]))

(defn- extract-bearer-token [request]
  (let [auth-header (get-in request [:headers "authorization"] "")]
    (when (str/starts-with? auth-header "Bearer ")
      (subs auth-header 7))))

(defn- auth-error-response
  "Return a 401 response with the given error message."
  [ctx message]
  (assoc ctx :response
         {:status  401
          :headers {"Content-Type" "application/json"}
          :body    (str "{\"error\":\"" message "\"")}))

(defn- check-token-revocation
  "Check if the token's jti or the user's tokens have been revoked.
   Returns true if revoked, false otherwise. Treats DB errors as revoked
   (fail-closed) to prevent 500s from reaching the client."
  [token-repo jti user-id iat]
  (try
    (or (proto/token-revoked? token-repo jti)
        (proto/all-revoked-for-user? token-repo user-id (long iat)))
    (catch Exception _
      true)))

(defn require-auth
  "Interceptor factory that validates JWT and attaches identity to request.
   The identity map conforms to schemas/Identity.
   Takes app config, user-repo and token-repo."
  [config user-repo token-repo]
  {:name  ::require-auth
   :enter (fn [ctx]
            (let [request (:request ctx)
                  token   (extract-bearer-token request)]
              (if-not token
                (auth-error-response ctx "Missing or invalid authorization token")
                (let [claims (jwt/verify-token (:jwt-secret config) token)]
                  (if-not claims
                    (auth-error-response ctx "Invalid or expired token")
                    (let [jti     (:jti claims)
                          user-id (:sub claims)
                          iat     (or (:iat claims) 0)]
                      (if (check-token-revocation token-repo jti user-id iat)
                        (auth-error-response ctx "Token has been revoked")
                        (let [user (proto/find-user-by-id user-repo user-id)]
                          (if-not user
                            (auth-error-response ctx "User not found")
                            (if (not= "ACTIVE" (:status user))
                              (auth-error-response ctx "User account is not active")
                              (let [identity {:user-id  user-id
                                              :username (:username claims)
                                              :role     (:role claims)
                                              :jti      jti
                                              :iat      iat}]
                                (assert (m/validate schemas/Identity identity)
                                        (str "Invalid identity: " (pr-str (m/explain schemas/Identity identity))))
                                (assoc-in ctx [:request :identity] identity))))))))))))})
