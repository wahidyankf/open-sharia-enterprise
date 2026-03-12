(ns demo-be-cjpd.handlers.report
  "P&L report handler."
  (:require [cheshire.core :as json]
            [demo-be-cjpd.db.expense-repo :as expense-repo]))

(defn pl-report-handler
  "GET /api/v1/reports/pl — Return profit and loss report."
  [ds]
  (fn [request]
    (let [user-id  (:user-id (:identity request))
          params   (:query-params request)
          ;; Pedestal populates :query-params with keyword keys; support both
          from     (or (get params :from) (get params "from"))
          to       (or (get params :to) (get params "to"))
          currency (or (get params :currency) (get params "currency") "USD")
          report   (expense-repo/pl-report ds user-id from to currency)]
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string {:income_total      (:income-total report)
                                       :expense_total     (:expense-total report)
                                       :net               (:net report)
                                       :income_breakdown  (:income-breakdown report)
                                       :expense_breakdown (:expense-breakdown report)})})))
