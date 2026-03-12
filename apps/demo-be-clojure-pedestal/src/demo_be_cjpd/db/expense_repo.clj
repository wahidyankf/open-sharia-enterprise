(ns demo-be-cjpd.db.expense-repo
  "Expense repository operations using next.jdbc."
  (:require [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [demo-be-cjpd.domain.expense :as expense-domain])
  (:import (java.util UUID)))

(defn- now-str []
  (.format java.time.format.DateTimeFormatter/ISO_INSTANT
           (java.time.Instant/now)))

(defn- row->expense [row]
  (when row
    (let [base {:id          (or (:expenses/id row) (:id row))
                :user-id     (or (:expenses/user_id row) (:user_id row))
                :type        (or (:expenses/type row) (:type row))
                :amount      (or (:expenses/amount row) (:amount row))
                :currency    (or (:expenses/currency row) (:currency row))
                :description (or (:expenses/description row) (:description row) "")
                :category    (or (:expenses/category row) (:category row) "")
                :date        (or (:expenses/date row) (:date row))
                :created-at  (or (:expenses/created_at row) (:created_at row))
                :updated-at  (or (:expenses/updated_at row) (:updated_at row))}
          unit     (or (:expenses/unit row) (:unit row))
          quantity (or (:expenses/quantity row) (:quantity row))]
      (cond-> base
        (and unit (not= "" unit))     (assoc :unit unit)
        (and quantity (not= "" quantity)) (assoc :quantity (Double/parseDouble (str quantity)))))))

(defn create-expense!
  "Insert a new expense. Returns the created expense map."
  [ds {:keys [user-id type amount currency description category unit quantity date]}]
  (let [id  (str (UUID/randomUUID))
        now (now-str)]
    (jdbc/execute! ds
                   ["INSERT INTO expenses (id, user_id, type, amount, currency, description, category, unit, quantity, date, created_at, updated_at)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    id user-id type
                    (expense-domain/format-amount currency amount)
                    (str/upper-case currency)
                    (or description "")
                    (or category "")
                    (or unit nil)
                    (when quantity (str quantity))
                    date now now])
    (row->expense (jdbc/execute-one! ds
                                     ["SELECT * FROM expenses WHERE id = ?" id]
                                     {:builder-fn rs/as-unqualified-maps}))))

(defn find-by-id
  "Find an expense by ID. Returns expense map or nil."
  [ds id]
  (row->expense (jdbc/execute-one! ds
                                   ["SELECT * FROM expenses WHERE id = ?" id]
                                   {:builder-fn rs/as-unqualified-maps})))

(defn find-by-id-and-user
  "Find an expense by ID and user ID. Returns expense map or nil."
  [ds id user-id]
  (row->expense (jdbc/execute-one! ds
                                   ["SELECT * FROM expenses WHERE id = ? AND user_id = ?" id user-id]
                                   {:builder-fn rs/as-unqualified-maps})))

(defn list-by-user
  "Return paginated list of expenses for a user."
  [ds user-id {:keys [page size] :or {page 1 size 20}}]
  (let [page   (max 1 page)
        offset (* (dec page) size)
        rows   (jdbc/execute! ds
                              ["SELECT * FROM expenses WHERE user_id = ? ORDER BY date DESC, created_at DESC LIMIT ? OFFSET ?"
                               user-id size offset]
                              {:builder-fn rs/as-unqualified-maps})
        total  (:cnt (jdbc/execute-one! ds
                                        ["SELECT COUNT(*) AS cnt FROM expenses WHERE user_id = ?" user-id]
                                        {:builder-fn rs/as-unqualified-maps}))]
    {:data  (mapv row->expense rows)
     :total (or total 0)
     :page  page
     :size  size}))

(defn update-expense!
  "Update an expense. Returns updated expense map."
  [ds id {:keys [type amount currency description category unit quantity date]}]
  (let [now (now-str)]
    (jdbc/execute! ds
                   ["UPDATE expenses SET type = ?, amount = ?, currency = ?, description = ?, category = ?, unit = ?, quantity = ?, date = ?, updated_at = ?
                     WHERE id = ?"
                    type
                    (expense-domain/format-amount currency amount)
                    (str/upper-case currency)
                    (or description "")
                    (or category "")
                    (or unit nil)
                    (when quantity (str quantity))
                    date now id])
    (find-by-id ds id)))

(defn delete-expense!
  "Delete an expense by ID."
  [ds id]
  (jdbc/execute! ds ["DELETE FROM expenses WHERE id = ?" id]))

(defn summary-by-user
  "Return expense totals grouped by currency for a user."
  [ds user-id]
  (let [rows (jdbc/execute! ds
                            ["SELECT currency, type, amount FROM expenses WHERE user_id = ?"
                             user-id]
                            {:builder-fn rs/as-unqualified-maps})]
    (reduce (fn [acc row]
              (let [currency (or (:currency row) "USD")
                    type-str (or (:type row) "expense")
                    amount   (bigdec (or (:amount row) "0"))]
                (update-in acc [currency type-str]
                           (fn [old] (.add (or old 0M) amount)))))
            {}
            rows)))

(defn pl-report
  "Return P&L report for a user filtered by date range and currency."
  [ds user-id from-date to-date currency]
  (let [upper   (str/upper-case currency)
        rows    (jdbc/execute! ds
                               ["SELECT type, amount, category FROM expenses
                                 WHERE user_id = ? AND currency = ? AND date >= ? AND date <= ?"
                                user-id upper from-date to-date]
                               {:builder-fn rs/as-unqualified-maps})
        result  (reduce (fn [acc row]
                          (let [t      (or (:type row) "expense")
                                cat    (or (:category row) "")
                                amount (bigdec (or (:amount row) "0"))]
                            (-> acc
                                (update-in [t :total] (fn [old] (.add (or old 0M) amount)))
                                (update-in [t :breakdown cat]
                                           (fn [old] (.add (or old 0M) amount))))))
                        {}
                        rows)
        income  (get-in result ["income" :total] 0M)
        expense (get-in result ["expense" :total] 0M)]
    {:income-total     (.toPlainString (.setScale income 2 java.math.RoundingMode/HALF_UP))
     :expense-total    (.toPlainString (.setScale expense 2 java.math.RoundingMode/HALF_UP))
     :net              (.toPlainString (.setScale (.subtract income expense) 2 java.math.RoundingMode/HALF_UP))
     :income-breakdown  (into {}
                              (map (fn [[k v]] [k (.toPlainString (.setScale v 2 java.math.RoundingMode/HALF_UP))])
                                   (get-in result ["income" :breakdown] {})))
     :expense-breakdown (into {}
                              (map (fn [[k v]] [k (.toPlainString (.setScale v 2 java.math.RoundingMode/HALF_UP))])
                                   (get-in result ["expense" :breakdown] {})))}))
