(ns demo-be-cjpd.auth.jwt-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [demo-be-cjpd.auth.jwt :as sut]))

(def ^:private test-secret "test-secret-that-is-long-enough-for-hs256")

(deftest sign-access-token-test
  (testing "returns a non-nil JWT string"
    (let [token (sut/sign-access-token test-secret "user-123" "alice" "USER")]
      (is (string? token))
      (is (= 3 (count (str/split token #"\."))))))

  (testing "token verifies correctly"
    (let [token  (sut/sign-access-token test-secret "user-123" "alice" "USER")
          claims (sut/verify-token test-secret token)]
      (is (some? claims))
      (is (= "user-123" (:sub claims)))
      (is (= "alice" (:username claims)))
      (is (= "USER" (:role claims)))
      (is (string? (:jti claims))))))

(deftest sign-refresh-token-test
  (testing "refresh token has type=refresh claim"
    (let [token  (sut/sign-refresh-token test-secret "user-123")
          claims (sut/verify-token test-secret token)]
      (is (some? claims))
      (is (= "refresh" (:type claims)))
      (is (= "user-123" (:sub claims))))))

(deftest verify-token-test
  (testing "returns nil for invalid token"
    (is (nil? (sut/verify-token test-secret "not.a.valid.token"))))

  (testing "returns nil for wrong secret"
    (let [token (sut/sign-access-token test-secret "user-123" "alice" "USER")]
      (is (nil? (sut/verify-token "different-secret-abc" token))))))
