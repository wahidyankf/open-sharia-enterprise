(ns demo-be-cjpd.auth.password-test
  (:require [clojure.test :refer [deftest testing is]]
            [demo-be-cjpd.auth.password :as sut]))

(deftest hash-and-verify-test
  (testing "hashed password verifies correctly"
    (let [pw   "Str0ng#Pass1"
          hash (sut/hash-password pw)]
      (is (string? hash))
      (is (true? (sut/verify-password pw hash)))))

  (testing "wrong password fails verification"
    (let [hash (sut/hash-password "Str0ng#Pass1")]
      (is (false? (sut/verify-password "WrongPassword1!" hash)))))

  (testing "hashes are different for same password"
    (let [pw "Str0ng#Pass1"
          h1 (sut/hash-password pw)
          h2 (sut/hash-password pw)]
      (is (not= h1 h2)))))
