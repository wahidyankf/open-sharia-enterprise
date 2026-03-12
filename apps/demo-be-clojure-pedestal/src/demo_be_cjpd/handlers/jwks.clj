(ns demo-be-cjpd.handlers.jwks
  "JWKS endpoint handler."
  (:require [cheshire.core :as json])
  (:import (java.util Base64)))

(defn jwks-handler
  "GET /.well-known/jwks.json — Return JWKS document."
  [config]
  (fn [_request]
    (let [secret-bytes (.getBytes ^String (:jwt-secret config) "UTF-8")
          encoded      (.encodeToString (Base64/getUrlEncoder) secret-bytes)]
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string
                  {:keys [{:kty "oct"
                           :use "sig"
                           :alg "HS256"
                           :k   encoded}]})})))
