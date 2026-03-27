(ns demo-be-cjpd.db.jdbc-attachment-repo
  "JDBC implementation of AttachmentRepo backed by next.jdbc."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [demo-be-cjpd.db.protocols :as proto])
  (:import (java.util UUID)))

(defn- now-str []
  (.format java.time.format.DateTimeFormatter/ISO_INSTANT
           (java.time.Instant/now)))

(defn- row->attachment [row]
  (when row
    {:id           (or (:attachments/id row) (:id row))
     :expense-id   (or (:attachments/expense_id row) (:expense_id row))
     :user-id      (or (:attachments/user_id row) (:user_id row))
     :filename     (or (:attachments/filename row) (:filename row))
     :content-type (or (:attachments/content_type row) (:content_type row))
     :size         (or (:attachments/size row) (:size row))
     :created-at   (or (:attachments/created_at row) (:created_at row))}))

(defrecord JdbcAttachmentRepo [ds]
  proto/AttachmentRepo

  (create-attachment! [_ {:keys [expense-id user-id filename content-type size data]}]
    (let [id  (str (UUID/randomUUID))
          now (now-str)]
      (jdbc/execute! ds
                     ["INSERT INTO attachments (id, expense_id, user_id, filename, content_type, size, data, created_at)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
                      id expense-id user-id filename content-type size data now])
      (row->attachment (jdbc/execute-one! ds
                                          ["SELECT id, expense_id, user_id, filename, content_type, size, created_at FROM attachments WHERE id = ?" id]
                                          {:builder-fn rs/as-unqualified-maps}))))

  (find-attachment-by-id [_ id]
    (row->attachment (jdbc/execute-one! ds
                                        ["SELECT id, expense_id, user_id, filename, content_type, size, created_at FROM attachments WHERE id = ?" id]
                                        {:builder-fn rs/as-unqualified-maps})))

  (list-attachments-by-expense [_ expense-id]
    (mapv row->attachment
          (jdbc/execute! ds
                         ["SELECT id, expense_id, user_id, filename, content_type, size, created_at FROM attachments WHERE expense_id = ? ORDER BY created_at ASC"
                          expense-id]
                         {:builder-fn rs/as-unqualified-maps})))

  (delete-attachment! [_ id]
    (jdbc/execute! ds ["DELETE FROM attachments WHERE id = ?" id]))

  (get-attachment-data [_ id]
    (let [row (jdbc/execute-one! ds
                                 ["SELECT data FROM attachments WHERE id = ?" id]
                                 {:builder-fn rs/as-unqualified-maps})]
      (or (:attachments/data row) (:data row)))))

(defn create
  "Create a JdbcAttachmentRepo backed by the given datasource."
  [ds]
  (->JdbcAttachmentRepo ds))
