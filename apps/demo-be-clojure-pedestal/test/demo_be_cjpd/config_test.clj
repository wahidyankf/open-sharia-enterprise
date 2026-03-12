(ns demo-be-cjpd.config-test
  (:require [clojure.test :refer [deftest testing is]]
            [demo-be-cjpd.config :as sut]))

(deftest load-config-test
  (testing "returns default port 8201"
    (let [cfg (sut/load-config)]
      (is (integer? (:port cfg)))
      (is (pos? (:port cfg)))))

  (testing "returns default database URL"
    (let [cfg (sut/load-config)]
      (is (string? (:database-url cfg)))))

  (testing "returns default JWT secret"
    (let [cfg (sut/load-config)]
      (is (string? (:jwt-secret cfg)))
      (is (pos? (count (:jwt-secret cfg)))))))
