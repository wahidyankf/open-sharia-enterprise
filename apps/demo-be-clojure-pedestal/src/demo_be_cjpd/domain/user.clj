(ns demo-be-cjpd.domain.user
  "User domain model, validation, and business rules."
  (:require [clojure.string :as str]))

(def max-failed-attempts 5)

(def statuses #{:ACTIVE :INACTIVE :DISABLED :LOCKED})
(def roles #{:USER :ADMIN})

(defn valid-email?
  "Return true if the email address is valid."
  [email]
  (boolean (re-matches #"^[a-zA-Z0-9._%+\-]+@[a-zA-Z0-9.\-]+\.[a-zA-Z]{2,}$" (or email ""))))

(defn valid-username?
  "Return true if the username is 3-50 chars of letters, numbers, underscores, hyphens."
  [username]
  (boolean (re-matches #"^[a-zA-Z0-9_\-]{3,50}$" (or username ""))))

(defn validate-password-strength
  "Validate password complexity rules. Returns nil on success or error map on failure."
  [password]
  (cond
    (or (nil? password) (str/blank? password))
    {:field "password" :message "password is required"}

    (< (count password) 12)
    {:field "password" :message "password must be at least 12 characters"}

    (not (re-find #"[A-Z]" password))
    {:field "password" :message "password must contain at least one uppercase letter"}

    (not (re-find #"[a-z]" password))
    {:field "password" :message "password must contain at least one lowercase letter"}

    (not (re-find #"[0-9]" password))
    {:field "password" :message "password must contain at least one digit"}

    (not (re-find #"[^a-zA-Z0-9]" password))
    {:field "password" :message "password must contain at least one special character"}

    :else nil))

(defn should-lock?
  "Return true if the failed attempt count exceeds the threshold."
  [failed-attempts]
  (>= failed-attempts max-failed-attempts))
