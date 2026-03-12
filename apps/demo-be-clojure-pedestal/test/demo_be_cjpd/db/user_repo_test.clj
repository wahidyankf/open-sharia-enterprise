(ns demo-be-cjpd.db.user-repo-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [demo-be-cjpd.db.core :as db]
            [demo-be-cjpd.db.schema :as schema]
            [demo-be-cjpd.db.user-repo :as sut]))

(def ^:private test-ds (atom nil))

(defn- setup-db [test-fn]
  (let [ds (db/create-datasource "jdbc:sqlite::memory:")]
    (schema/create-schema! ds)
    (reset! test-ds ds)
    (test-fn)
    (.close ds)))

(use-fixtures :each setup-db)

(deftest create-and-find-user-test
  (testing "creates a user and retrieves it by id"
    (let [user (sut/create-user! @test-ds
                                 {:username      "alice"
                                  :email         "alice@example.com"
                                  :password-hash "hash123"
                                  :display-name  "Alice"
                                  :role          "USER"
                                  :status        "ACTIVE"})]
      (is (some? (:id user)))
      (is (= "alice" (:username user)))
      (is (= "alice@example.com" (:email user)))
      (is (= "USER" (:role user)))
      (is (= "ACTIVE" (:status user)))))

  (testing "find by username"
    (sut/create-user! @test-ds
                      {:username      "bob"
                       :email         "bob@example.com"
                       :password-hash "hash456"
                       :display-name  "Bob"
                       :role          "USER"
                       :status        "ACTIVE"})
    (let [found (sut/find-by-username @test-ds "bob")]
      (is (= "bob" (:username found)))))

  (testing "find by email"
    (sut/create-user! @test-ds
                      {:username      "carol"
                       :email         "carol@example.com"
                       :password-hash "hash789"
                       :display-name  "Carol"
                       :role          "USER"
                       :status        "ACTIVE"})
    (let [found (sut/find-by-email @test-ds "carol@example.com")]
      (is (= "carol" (:username found))))))

(deftest count-users-test
  (testing "count-users returns zero on empty database"
    (is (zero? (sut/count-users @test-ds))))

  (testing "count-users increments after creating users"
    (sut/create-user! @test-ds
                      {:username      "first"
                       :email         "first@example.com"
                       :password-hash "hash"
                       :display-name  "First"
                       :role          "ADMIN"
                       :status        "ACTIVE"})
    (is (= 1 (sut/count-users @test-ds)))))

(deftest update-operations-test
  (testing "update display name"
    (let [user    (sut/create-user! @test-ds
                                    {:username      "alice"
                                     :email         "alice@example.com"
                                     :password-hash "hash123"
                                     :display-name  "Alice"
                                     :role          "USER"
                                     :status        "ACTIVE"})
          updated (sut/update-display-name! @test-ds (:id user) "Alice Smith")]
      (is (= "Alice Smith" (:display-name updated)))))

  (testing "update status"
    (let [user    (sut/create-user! @test-ds
                                    {:username      "dave"
                                     :email         "dave@example.com"
                                     :password-hash "hash"
                                     :display-name  "Dave"
                                     :role          "USER"
                                     :status        "ACTIVE"})
          updated (sut/update-status! @test-ds (:id user) "INACTIVE")]
      (is (= "INACTIVE" (:status updated))))))

(deftest failed-attempts-test
  (testing "increments failed attempts"
    (let [user (sut/create-user! @test-ds
                                 {:username      "testuser"
                                  :email         "test@example.com"
                                  :password-hash "hash"
                                  :display-name  "Test"
                                  :role          "USER"
                                  :status        "ACTIVE"})]
      (sut/increment-failed-attempts! @test-ds (:id user))
      (sut/increment-failed-attempts! @test-ds (:id user))
      (let [updated (sut/find-by-id @test-ds (:id user))]
        (is (= 2 (:failed-login-attempts updated))))))

  (testing "locks account after 5 failed attempts"
    (let [user (sut/create-user! @test-ds
                                 {:username      "lockme"
                                  :email         "lockme@example.com"
                                  :password-hash "hash"
                                  :display-name  "Lock Me"
                                  :role          "USER"
                                  :status        "ACTIVE"})]
      (dotimes [_ 5]
        (sut/increment-failed-attempts! @test-ds (:id user)))
      (let [locked (sut/find-by-id @test-ds (:id user))]
        (is (= "LOCKED" (:status locked)))))))
