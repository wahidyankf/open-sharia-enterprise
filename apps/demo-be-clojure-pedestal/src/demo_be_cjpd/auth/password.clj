(ns demo-be-cjpd.auth.password
  "Password hashing and verification using buddy-hashers."
  (:require [buddy.hashers :as hashers]))

(defn hash-password
  "Hash a plaintext password using bcrypt+sha512."
  [password]
  (hashers/derive password {:alg :bcrypt+sha512}))

(defn verify-password
  "Verify a plaintext password against a hash. Returns true if valid."
  [password hash]
  (let [result (hashers/verify password hash)]
    (:valid result)))
