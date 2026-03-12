(ns demo-be-cjpd.db.user-repo
  "User repository operations using next.jdbc."
  (:require [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import (java.util UUID)))

(defn- now-str []
  (.format java.time.format.DateTimeFormatter/ISO_INSTANT
           (java.time.Instant/now)))

(defn- row->user [row]
  (when row
    {:id                   (or (:users/id row) (:id row))
     :username             (or (:users/username row) (:username row))
     :email                (or (:users/email row) (:email row))
     :password-hash        (or (:users/password_hash row) (:password_hash row))
     :display-name         (or (:users/display_name row) (:display_name row) "")
     :role                 (or (:users/role row) (:role row) "USER")
     :status               (or (:users/status row) (:status row) "ACTIVE")
     :failed-login-attempts (or (:users/failed_login_attempts row) (:failed_login_attempts row) 0)
     :created-at           (or (:users/created_at row) (:created_at row))
     :updated-at           (or (:users/updated_at row) (:updated_at row))}))

(defn count-users
  "Return total count of users in the database."
  [ds]
  (let [row (jdbc/execute-one! ds ["SELECT COUNT(*) AS cnt FROM users"]
                               {:builder-fn rs/as-unqualified-maps})]
    (or (:cnt row) 0)))

(defn find-by-id
  "Find a user by their UUID string. Returns user map or nil."
  [ds id]
  (row->user (jdbc/execute-one! ds ["SELECT * FROM users WHERE id = ?" id]
                                {:builder-fn rs/as-unqualified-maps})))

(defn find-by-username
  "Find a user by username. Returns user map or nil."
  [ds username]
  (row->user (jdbc/execute-one! ds ["SELECT * FROM users WHERE username = ?" username]
                                {:builder-fn rs/as-unqualified-maps})))

(defn find-by-email
  "Find a user by email. Returns user map or nil."
  [ds email]
  (row->user (jdbc/execute-one! ds ["SELECT * FROM users WHERE email = ?" email]
                                {:builder-fn rs/as-unqualified-maps})))

(defn create-user!
  "Insert a new user into the database. Returns the created user map."
  [ds {:keys [username email password-hash display-name role status]}]
  (let [id  (str (UUID/randomUUID))
        now (now-str)]
    (jdbc/execute! ds
                   ["INSERT INTO users (id, username, email, password_hash, display_name, role, status, failed_login_attempts, created_at, updated_at)
                     VALUES (?, ?, ?, ?, ?, ?, ?, 0, ?, ?)"
                    id username email password-hash
                    (or display-name username)
                    (or role "USER")
                    (or status "ACTIVE")
                    now now])
    (find-by-id ds id)))

(defn update-display-name!
  "Update a user's display name. Returns updated user map."
  [ds user-id display-name]
  (let [now (now-str)]
    (jdbc/execute! ds
                   ["UPDATE users SET display_name = ?, updated_at = ? WHERE id = ?"
                    display-name now user-id])
    (find-by-id ds user-id)))

(defn update-password!
  "Update a user's password hash. Returns updated user map."
  [ds user-id password-hash]
  (let [now (now-str)]
    (jdbc/execute! ds
                   ["UPDATE users SET password_hash = ?, updated_at = ? WHERE id = ?"
                    password-hash now user-id])
    (find-by-id ds user-id)))

(defn update-status!
  "Update a user's status. Returns updated user map."
  [ds user-id status]
  (let [now (now-str)]
    (jdbc/execute! ds
                   ["UPDATE users SET status = ?, updated_at = ? WHERE id = ?"
                    status now user-id])
    (find-by-id ds user-id)))

(defn increment-failed-attempts!
  "Increment failed login attempts and potentially lock the account."
  [ds user-id]
  (let [now  (now-str)
        user (find-by-id ds user-id)
        new-count (inc (or (:failed-login-attempts user) 0))
        status    (if (>= new-count 5) "LOCKED" (:status user))]
    (jdbc/execute! ds
                   ["UPDATE users SET failed_login_attempts = ?, status = ?, updated_at = ? WHERE id = ?"
                    new-count status now user-id])
    (find-by-id ds user-id)))

(defn reset-failed-attempts!
  "Reset failed login attempts to zero."
  [ds user-id]
  (let [now (now-str)]
    (jdbc/execute! ds
                   ["UPDATE users SET failed_login_attempts = 0, updated_at = ? WHERE id = ?"
                    now user-id])
    (find-by-id ds user-id)))

(defn list-users
  "Return paginated list of users with optional search filter."
  [ds {:keys [search page size]
       :or   {page 1 size 20}}]
  (let [page   (max 1 page)
        offset (* (dec page) size)]
    (if (and search (not (str/blank? search)))
      (let [pattern (str "%" search "%")]
        {:data  (mapv row->user
                      (jdbc/execute! ds
                                     ["SELECT * FROM users WHERE username LIKE ? OR email LIKE ? ORDER BY created_at DESC LIMIT ? OFFSET ?"
                                      pattern pattern size offset]
                                     {:builder-fn rs/as-unqualified-maps}))
         :total (:cnt (jdbc/execute-one! ds
                                         ["SELECT COUNT(*) AS cnt FROM users WHERE username LIKE ? OR email LIKE ?"
                                          pattern pattern]
                                         {:builder-fn rs/as-unqualified-maps}))
         :page  page
         :size  size})
      {:data  (mapv row->user
                    (jdbc/execute! ds
                                   ["SELECT * FROM users ORDER BY created_at DESC LIMIT ? OFFSET ?"
                                    size offset]
                                   {:builder-fn rs/as-unqualified-maps}))
       :total (count-users ds)
       :page  page
       :size  size})))
