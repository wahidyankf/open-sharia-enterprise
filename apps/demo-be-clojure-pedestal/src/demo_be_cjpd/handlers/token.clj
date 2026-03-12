(ns demo-be-cjpd.handlers.token
  "Token introspection handler."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [demo-be-cjpd.auth.jwt :as jwt]))

(defn token-claims-handler
  "GET /api/v1/tokens/claims — Return decoded JWT claims."
  [config]
  (fn [request]
    (let [auth   (get-in request [:headers "authorization"] "")
          token  (when (str/starts-with? auth "Bearer ")
                   (subs auth 7))
          claims (when token (jwt/verify-token (:jwt-secret config) token))]
      (if claims
        {:status  200
         :headers {"Content-Type" "application/json"}
         :body    (json/generate-string claims)}
        {:status  401
         :headers {"Content-Type" "application/json"}
         :body    "{\"error\":\"Invalid or expired token\"}"}))))
