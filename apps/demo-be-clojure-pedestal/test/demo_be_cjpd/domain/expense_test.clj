(ns demo-be-cjpd.domain.expense-test
  (:require [clojure.test :refer [deftest testing is]]
            [demo-be-cjpd.domain.expense :as sut]))

(deftest valid-currency-test
  (testing "supported currencies"
    (is (true? (sut/valid-currency? "USD")))
    (is (true? (sut/valid-currency? "IDR")))
    (is (true? (sut/valid-currency? "usd"))))

  (testing "unsupported currencies"
    (is (false? (sut/valid-currency? "EUR")))
    (is (false? (sut/valid-currency? "GBP")))
    (is (false? (sut/valid-currency? "US")))
    (is (false? (sut/valid-currency? "USDD")))))

(deftest valid-unit-test
  (testing "supported units"
    (is (true? (sut/valid-unit? "liter")))
    (is (true? (sut/valid-unit? "kg")))
    (is (true? (sut/valid-unit? "gallon")))
    (is (true? (sut/valid-unit? "hour"))))

  (testing "nil/empty unit is valid"
    (is (true? (sut/valid-unit? nil)))
    (is (true? (sut/valid-unit? ""))))

  (testing "unsupported units"
    (is (false? (sut/valid-unit? "fathom")))
    (is (false? (sut/valid-unit? "cups")))))

(deftest parse-amount-test
  (testing "valid amounts"
    (is (= 10.50M (sut/parse-amount "10.50")))
    (is (= 150000M (sut/parse-amount "150000"))))

  (testing "invalid amounts"
    (is (nil? (sut/parse-amount "not-a-number")))
    (is (nil? (sut/parse-amount nil)))))

(deftest validate-amount-test
  (testing "valid USD amounts"
    (is (nil? (sut/validate-amount "USD" "10.50")))
    (is (nil? (sut/validate-amount "USD" "0.00"))))

  (testing "negative amount"
    (is (some? (sut/validate-amount "USD" "-10.00"))))

  (testing "USD exceeds 2 decimal places"
    (is (some? (sut/validate-amount "USD" "10.555"))))

  (testing "IDR must be whole number"
    (is (nil? (sut/validate-amount "IDR" "150000")))
    (is (some? (sut/validate-amount "IDR" "150000.50")))))

(deftest format-amount-test
  (testing "USD formats to 2 decimal places"
    (is (= "10.50" (sut/format-amount "USD" "10.50")))
    (is (= "10.00" (sut/format-amount "USD" "10"))))

  (testing "IDR formats to 0 decimal places"
    (is (= "150000" (sut/format-amount "IDR" "150000")))))
