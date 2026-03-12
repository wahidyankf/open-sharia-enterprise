(ns demo-be-cjpd.db.schema
  "Database schema creation and migration."
  (:require [next.jdbc :as jdbc]))

(def create-users-sql
  "CREATE TABLE IF NOT EXISTS users (
     id TEXT PRIMARY KEY,
     username TEXT NOT NULL UNIQUE,
     email TEXT NOT NULL UNIQUE,
     password_hash TEXT NOT NULL,
     display_name TEXT NOT NULL DEFAULT '',
     role TEXT NOT NULL DEFAULT 'USER',
     status TEXT NOT NULL DEFAULT 'ACTIVE',
     failed_login_attempts INTEGER NOT NULL DEFAULT 0,
     created_at TEXT NOT NULL,
     updated_at TEXT NOT NULL
   )")

(def create-revoked-tokens-sql
  "CREATE TABLE IF NOT EXISTS revoked_tokens (
     id TEXT PRIMARY KEY,
     jti TEXT NOT NULL UNIQUE,
     user_id TEXT NOT NULL,
     revoked_at TEXT NOT NULL
   )")

(def create-expenses-sql
  "CREATE TABLE IF NOT EXISTS expenses (
     id TEXT PRIMARY KEY,
     user_id TEXT NOT NULL,
     type TEXT NOT NULL,
     amount TEXT NOT NULL,
     currency TEXT NOT NULL,
     description TEXT NOT NULL DEFAULT '',
     category TEXT NOT NULL DEFAULT '',
     unit TEXT,
     quantity TEXT,
     date TEXT NOT NULL,
     created_at TEXT NOT NULL,
     updated_at TEXT NOT NULL
   )")

(def create-attachments-sql
  "CREATE TABLE IF NOT EXISTS attachments (
     id TEXT PRIMARY KEY,
     expense_id TEXT NOT NULL,
     user_id TEXT NOT NULL,
     filename TEXT NOT NULL,
     content_type TEXT NOT NULL,
     size INTEGER NOT NULL,
     data BLOB NOT NULL,
     created_at TEXT NOT NULL
   )")

(defn create-schema!
  "Create all database tables if they do not exist."
  [ds]
  (jdbc/execute! ds [create-users-sql])
  (jdbc/execute! ds [create-revoked-tokens-sql])
  (jdbc/execute! ds [create-expenses-sql])
  (jdbc/execute! ds [create-attachments-sql]))
