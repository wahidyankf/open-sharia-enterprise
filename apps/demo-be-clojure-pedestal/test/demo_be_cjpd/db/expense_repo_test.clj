(ns demo-be-cjpd.db.expense-repo-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [demo-be-cjpd.db.core :as db]
            [demo-be-cjpd.db.schema :as schema]
            [demo-be-cjpd.db.expense-repo :as sut]))

(def ^:private test-ds (atom nil))
(def ^:private user-id "test-user-123")

(defn- setup-db [test-fn]
  (let [ds (db/create-datasource "jdbc:sqlite::memory:")]
    (schema/create-schema! ds)
    (reset! test-ds ds)
    (test-fn)
    (.close ds)))

(use-fixtures :each setup-db)

(deftest create-and-find-expense-test
  (testing "creates an expense and retrieves it by id"
    (let [expense (sut/create-expense! @test-ds
                                       {:user-id     user-id
                                        :type        "expense"
                                        :amount      "10.50"
                                        :currency    "USD"
                                        :description "Test expense"
                                        :category    "food"
                                        :date        "2025-01-15"})]
      (is (some? (:id expense)))
      (is (= user-id (:user-id expense)))
      (is (= "expense" (:type expense)))
      (is (= "10.50" (:amount expense)))
      (is (= "USD" (:currency expense)))))

  (testing "find by id and user"
    (let [expense (sut/create-expense! @test-ds
                                       {:user-id     user-id
                                        :type        "income"
                                        :amount      "3000.00"
                                        :currency    "USD"
                                        :description "Salary"
                                        :category    "salary"
                                        :date        "2025-01-31"})
          found   (sut/find-by-id-and-user @test-ds (:id expense) user-id)]
      (is (= "3000.00" (:amount found)))
      (is (= "income" (:type found)))))

  (testing "find by id and wrong user returns nil"
    (let [expense (sut/create-expense! @test-ds
                                       {:user-id     user-id
                                        :type        "expense"
                                        :amount      "5.00"
                                        :currency    "USD"
                                        :description "Coffee"
                                        :category    "food"
                                        :date        "2025-01-15"})]
      (is (nil? (sut/find-by-id-and-user @test-ds (:id expense) "other-user"))))))

(deftest list-expenses-test
  (testing "lists expenses for a user paginated"
    (sut/create-expense! @test-ds {:user-id user-id :type "expense" :amount "10.00"
                                   :currency "USD" :description "A" :category "food"
                                   :date "2025-01-01"})
    (sut/create-expense! @test-ds {:user-id user-id :type "expense" :amount "20.00"
                                   :currency "USD" :description "B" :category "food"
                                   :date "2025-01-02"})
    (let [result (sut/list-by-user @test-ds user-id {:page 1 :size 10})]
      (is (= 2 (:total result)))
      (is (= 2 (count (:data result)))))))

(deftest update-expense-test
  (testing "updates an expense"
    (let [expense (sut/create-expense! @test-ds
                                       {:user-id     user-id
                                        :type        "expense"
                                        :amount      "10.00"
                                        :currency    "USD"
                                        :description "Original"
                                        :category    "food"
                                        :date        "2025-01-15"})
          updated (sut/update-expense! @test-ds (:id expense)
                                       {:type        "expense"
                                        :amount      "15.00"
                                        :currency    "USD"
                                        :description "Updated"
                                        :category    "food"
                                        :date        "2025-01-15"})]
      (is (= "15.00" (:amount updated)))
      (is (= "Updated" (:description updated))))))

(deftest delete-expense-test
  (testing "deletes an expense"
    (let [expense (sut/create-expense! @test-ds
                                       {:user-id     user-id
                                        :type        "expense"
                                        :amount      "5.00"
                                        :currency    "USD"
                                        :description "Delete me"
                                        :category    "misc"
                                        :date        "2025-01-01"})]
      (sut/delete-expense! @test-ds (:id expense))
      (is (nil? (sut/find-by-id @test-ds (:id expense)))))))

(deftest pl-report-test
  (testing "returns correct P&L totals"
    (sut/create-expense! @test-ds {:user-id user-id :type "income" :amount "5000.00"
                                   :currency "USD" :description "Salary" :category "salary"
                                   :date "2025-01-15"})
    (sut/create-expense! @test-ds {:user-id user-id :type "expense" :amount "150.00"
                                   :currency "USD" :description "Food" :category "food"
                                   :date "2025-01-20"})
    (let [report (sut/pl-report @test-ds user-id "2025-01-01" "2025-01-31" "USD")]
      (is (= "5000.00" (:income-total report)))
      (is (= "150.00" (:expense-total report)))
      (is (= "4850.00" (:net report))))))
