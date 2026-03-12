(ns demo-be-cjpd.domain.attachment-test
  (:require [clojure.test :refer [deftest testing is]]
            [demo-be-cjpd.domain.attachment :as sut]))

(deftest valid-content-type-test
  (testing "allowed content types"
    (is (true? (sut/valid-content-type? "image/jpeg")))
    (is (true? (sut/valid-content-type? "image/png")))
    (is (true? (sut/valid-content-type? "application/pdf"))))

  (testing "disallowed content types"
    (is (false? (sut/valid-content-type? "application/octet-stream")))
    (is (false? (sut/valid-content-type? "text/plain")))
    (is (false? (sut/valid-content-type? "video/mp4")))))

(deftest valid-file-size-test
  (testing "within limit"
    (is (true? (sut/valid-file-size? 0)))
    (is (true? (sut/valid-file-size? 1024)))
    (is (true? (sut/valid-file-size? (* 10 1024 1024)))))

  (testing "exceeds limit"
    (is (false? (sut/valid-file-size? (inc (* 10 1024 1024)))))))
