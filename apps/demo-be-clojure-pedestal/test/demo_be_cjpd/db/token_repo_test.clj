(ns demo-be-cjpd.db.token-repo-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [demo-be-cjpd.db.core :as db]
            [demo-be-cjpd.db.schema :as schema]
            [demo-be-cjpd.db.token-repo :as sut]))

(def ^:private test-ds (atom nil))

(defn- setup-db [test-fn]
  (let [db-url "jdbc:sqlite::memory:"
        ds (db/create-datasource db-url)]
    (schema/create-schema! ds db-url)
    (reset! test-ds ds)
    (test-fn)
    (.close ds)))

(use-fixtures :each setup-db)

(deftest revoke-token-test
  (testing "revoke and check token"
    (let [jti "test-jti-123"]
      (is (false? (sut/revoked? @test-ds jti)))
      (sut/revoke-token! @test-ds jti "user-1")
      (is (true? (sut/revoked? @test-ds jti)))))

  (testing "revoking same JTI twice does not throw"
    (let [jti "duplicate-jti"]
      (sut/revoke-token! @test-ds jti "user-1")
      (sut/revoke-token! @test-ds jti "user-1")
      (is (true? (sut/revoked? @test-ds jti))))))

(deftest revoke-all-for-user-test
  (testing "tokens issued before revoke-all are detected as all-revoked"
    (let [user-id "user-abc"
          ;; Simulate a token issued 10 seconds ago
          past-iat (- (quot (System/currentTimeMillis) 1000) 10)]
      (is (false? (sut/all-revoked-for-user? @test-ds user-id past-iat)))
      (sut/revoke-all-for-user! @test-ds user-id)
      (is (true? (sut/all-revoked-for-user? @test-ds user-id past-iat)))))

  (testing "tokens issued after revoke-all are not affected"
    (let [user-id "user-xyz"
          future-iat (+ (quot (System/currentTimeMillis) 1000) 1000)]
      (sut/revoke-all-for-user! @test-ds user-id)
      (is (false? (sut/all-revoked-for-user? @test-ds user-id future-iat))))))
