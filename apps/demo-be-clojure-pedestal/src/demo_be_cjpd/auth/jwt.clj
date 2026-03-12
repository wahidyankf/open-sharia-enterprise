(ns demo-be-cjpd.auth.jwt
  "JWT token signing and verification using buddy-sign."
  (:require [buddy.sign.jwt :as jwt]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import (java.util UUID Base64)))

(def ^:private issuer "demo-be-cjpd")

(defn- now-epoch []
  (quot (System/currentTimeMillis) 1000))

(defn- secret->bytes [^String secret]
  (.getBytes secret "UTF-8"))

(defn sign-access-token
  "Sign an access token with 15-minute expiry."
  [secret user-id username role]
  (let [now    (now-epoch)
        exp    (+ now (* 15 60))
        jti    (str (UUID/randomUUID))
        claims {:sub      (str user-id)
                :username username
                :role     (str role)
                :jti      jti
                :iss      issuer
                :iat      now
                :exp      exp}]
    (jwt/sign claims (secret->bytes secret) {:alg :hs256})))

(defn sign-refresh-token
  "Sign a refresh token with 7-day expiry."
  [secret user-id]
  (let [now    (now-epoch)
        exp    (+ now (* 7 24 60 60))
        jti    (str (UUID/randomUUID))
        claims {:sub  (str user-id)
                :jti  jti
                :type "refresh"
                :iss  issuer
                :iat  now
                :exp  exp}]
    (jwt/sign claims (secret->bytes secret) {:alg :hs256})))

(defn verify-token
  "Verify and decode a JWT token. Returns claims map or nil on failure."
  [secret token]
  (try
    (jwt/unsign token (secret->bytes secret) {:alg :hs256})
    (catch Exception _
      nil)))

(defn decode-claims
  "Decode JWT claims without verification (for introspection)."
  [token]
  (try
    (let [parts   (str/split token #"\.")
          payload (second parts)
          decoded (-> (Base64/getUrlDecoder)
                      (.decode ^String payload)
                      (String. "UTF-8"))]
      (json/parse-string decoded true))
    (catch Exception _
      nil)))
