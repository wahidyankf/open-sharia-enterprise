(ns demo-be-cjpd.db.jdbc-token-repo
  "JDBC implementation of TokenRepo backed by next.jdbc."
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [demo-be-cjpd.db.protocols :as proto])
  (:import (java.util UUID)))

(defn- now-str []
  (.format java.time.format.DateTimeFormatter/ISO_INSTANT
           (java.time.Instant/now)))

(defrecord JdbcTokenRepo [ds]
  proto/TokenRepo

  (revoke-token! [_ jti user-id]
    (let [id  (str (UUID/randomUUID))
          now (now-str)]
      (jdbc/execute! ds
                     ["INSERT INTO revoked_tokens (id, jti, user_id, revoked_at)
                       VALUES (?, ?, ?, ?)
                       ON CONFLICT(jti) DO NOTHING"
                      id jti user-id now])))

  (token-revoked? [_ jti]
    (let [row (jdbc/execute-one! ds
                                 ["SELECT id FROM revoked_tokens WHERE jti = ?" jti]
                                 {:builder-fn rs/as-unqualified-maps})]
      (some? row)))

  (revoke-all-for-user! [_ user-id]
    (let [id           (str (UUID/randomUUID))
          now          (now-str)
          sentinel-jti (str "ALL:" user-id ":" now)]
      (jdbc/execute! ds
                     ["INSERT INTO revoked_tokens (id, jti, user_id, revoked_at)
                       VALUES (?, ?, ?, ?)"
                      id sentinel-jti user-id now])))

  (all-revoked-for-user? [_ user-id iat]
    (let [rows (jdbc/execute! ds
                              ["SELECT revoked_at FROM revoked_tokens WHERE user_id = ? AND jti LIKE 'ALL:%'"
                               user-id]
                              {:builder-fn rs/as-unqualified-maps})]
      (boolean
       (some (fn [row]
               (let [revoked-epoch (-> (:revoked_at row)
                                       java.time.Instant/parse
                                       .getEpochSecond)]
                 (>= revoked-epoch iat)))
             rows)))))

(defn create
  "Create a JdbcTokenRepo backed by the given datasource."
  [ds]
  (->JdbcTokenRepo ds))
