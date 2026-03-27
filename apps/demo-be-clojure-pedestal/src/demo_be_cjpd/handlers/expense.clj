(ns demo-be-cjpd.handlers.expense
  "Expense CRUD handlers."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [demo-be-cjpd.db.protocols :as proto]
            [demo-be-cjpd.domain.expense :as expense-domain]))

(defn- kebab->camel [s]
  (let [parts (str/split s #"-")]
    (str (first parts)
         (apply str (map str/capitalize (rest parts))))))

(defn- json-response [status body]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string body {:key-fn #(kebab->camel (name %))})})

(defn- error-response [status message]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string {:message message})})

(defn- expense->response [expense]
  (cond-> {:id          (:id expense)
           :user-id     (:user-id expense)
           :type        (:type expense)
           :amount      (:amount expense)
           :currency    (:currency expense)
           :description (:description expense)
           :category    (:category expense)
           :date        (:date expense)
           :created-at  (:created-at expense)
           :updated-at  (:updated-at expense)}
    (:unit expense)     (assoc :unit (:unit expense))
    (:quantity expense) (assoc :quantity (:quantity expense))))

(defn- validate-expense-params
  "Validate expense creation/update parameters. Returns error map or nil."
  [{:keys [type amount currency unit]}]
  (cond
    (and type (not (expense-domain/valid-type? type)))
    {:status 400 :field "type" :message "type must be income or expense"}

    (and currency (not (expense-domain/valid-currency? currency)))
    {:status 400 :field "currency" :message (str "Unsupported or invalid currency: " currency)}

    (and amount currency (expense-domain/validate-amount currency amount))
    (merge {:status 400} (expense-domain/validate-amount currency amount))

    (and unit (not (expense-domain/valid-unit? unit)))
    {:status 400 :field "unit" :message (str "Unsupported unit: " unit)}

    :else nil))

(defn create-expense-handler
  "POST /api/v1/expenses — Create a new expense entry."
  [expense-repo]
  (fn [request]
    (let [user-id  (:user-id (:identity request))
          params   (:json-params request)
          type     (str/lower-case (or (:type params) "expense"))
          amount   (str (:amount params))
          currency (str (:currency params))
          desc     (:description params)
          category (or (:category params) "")
          unit     (:unit params)
          quantity (:quantity params)
          date     (str (:date params))
          error    (validate-expense-params {:type type :amount amount :currency currency :unit unit})]
      (if error
        {:status  (:status error)
         :headers {"Content-Type" "application/json"}
         :body    (json/generate-string {:message (:message error) :field (:field error)})}
        (let [expense (proto/create-expense! expense-repo
                                             {:user-id     user-id
                                              :type        type
                                              :amount      amount
                                              :currency    currency
                                              :description (or desc "")
                                              :category    (or category "")
                                              :unit        unit
                                              :quantity    quantity
                                              :date        date})]
          (json-response 201 (expense->response expense)))))))

(defn get-expense-handler
  "GET /api/v1/expenses/:id — Get a single expense."
  [expense-repo]
  (fn [request]
    (let [user-id    (:user-id (:identity request))
          expense-id (get-in request [:path-params :id])
          expense    (proto/find-expense-by-id-and-user expense-repo expense-id user-id)]
      (if expense
        (json-response 200 (expense->response expense))
        (error-response 404 "Expense not found")))))

(defn list-expenses-handler
  "GET /api/v1/expenses — List expenses for the authenticated user."
  [expense-repo]
  (fn [request]
    (let [user-id  (:user-id (:identity request))
          params   (:query-params request)
          page     (Integer/parseInt (or (some-> params :page str) (get params "page") "1"))
          size     (Integer/parseInt (or (some-> params :size str) (get params "size") "20"))
          result   (proto/list-expenses-by-user expense-repo user-id {:page page :size size})]
      (json-response 200 {:content        (mapv expense->response (:data result))
                          :total-elements (:total result)
                          :page           (:page result)
                          :size           (:size result)}))))

(defn update-expense-handler
  "PUT /api/v1/expenses/:id — Update an existing expense."
  [expense-repo]
  (fn [request]
    (let [user-id    (:user-id (:identity request))
          expense-id (get-in request [:path-params :id])
          existing   (proto/find-expense-by-id-and-user expense-repo expense-id user-id)]
      (if-not existing
        (error-response 404 "Expense not found")
        (let [params   (:json-params request)
              type     (str/lower-case (or (:type params) (:type existing)))
              amount   (str (or (:amount params) (:amount existing)))
              currency (str (or (:currency params) (:currency existing)))
              desc     (or (:description params) (:description existing))
              category (or (:category params) (:category existing))
              unit     (or (:unit params) (:unit existing))
              quantity (or (:quantity params) (:quantity existing))
              date     (str (or (:date params) (:date existing)))
              error    (validate-expense-params {:type type :amount amount :currency currency :unit unit})]
          (if error
            {:status  (:status error)
             :headers {"Content-Type" "application/json"}
             :body    (json/generate-string {:message (:message error) :field (:field error)})}
            (let [updated (proto/update-expense! expense-repo expense-id
                                                 {:type        type
                                                  :amount      amount
                                                  :currency    currency
                                                  :description desc
                                                  :category    category
                                                  :unit        unit
                                                  :quantity    quantity
                                                  :date        date})]
              (json-response 200 (expense->response updated)))))))))

(defn delete-expense-handler
  "DELETE /api/v1/expenses/:id — Delete an expense."
  [expense-repo]
  (fn [request]
    (let [user-id    (:user-id (:identity request))
          expense-id (get-in request [:path-params :id])
          existing   (proto/find-expense-by-id-and-user expense-repo expense-id user-id)]
      (if-not existing
        (error-response 404 "Expense not found")
        (do
          (proto/delete-expense! expense-repo expense-id)
          {:status  204
           :headers {"Content-Type" "application/json"}
           :body    ""})))))

(defn summary-handler
  "GET /api/v1/expenses/summary — Return expense totals grouped by currency (expenses only)."
  [expense-repo]
  (fn [request]
    (let [user-id  (:user-id (:identity request))
          summary  (proto/summary-by-user expense-repo user-id)
          ;; Only sum expense entries (not income), matching Go/Java implementations
          result   (into {}
                         (keep (fn [[currency by-type]]
                                 (let [expense-total (or (get by-type "expense") 0M)
                                       decimals      (if (= "IDR" currency) 0 2)
                                       formatted     (.toPlainString
                                                       (.setScale expense-total decimals
                                                                  java.math.RoundingMode/HALF_UP))]
                                   (when (pos? (.compareTo expense-total 0M))
                                     [currency formatted])))
                               summary))]
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body    (json/generate-string result)})))
