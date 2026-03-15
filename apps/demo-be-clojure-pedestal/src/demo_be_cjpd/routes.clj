(ns demo-be-cjpd.routes
  "Pedestal route table definition."
  (:require [io.pedestal.http.route.definition.table :as table]
            [demo-be-cjpd.interceptors.json :as json-int]
            [demo-be-cjpd.interceptors.auth :as auth-int]
            [demo-be-cjpd.interceptors.admin :as admin-int]
            [demo-be-cjpd.interceptors.error :as error-int]
            [demo-be-cjpd.interceptors.multipart :as multipart-int]
            [demo-be-cjpd.handlers.health :as health]
            [demo-be-cjpd.handlers.auth :as auth-h]
            [demo-be-cjpd.handlers.user :as user-h]
            [demo-be-cjpd.handlers.admin :as admin-h]
            [demo-be-cjpd.handlers.expense :as expense-h]
            [demo-be-cjpd.handlers.attachment :as attachment-h]
            [demo-be-cjpd.handlers.report :as report-h]
            [demo-be-cjpd.handlers.token :as token-h]
            [demo-be-cjpd.handlers.jwks :as jwks-h]
            [demo-be-cjpd.handlers.test-api :as test-api-h]))

(defn- handler->interceptor [handler-fn handler-name]
  {:name  handler-name
   :enter (fn [ctx]
            (assoc ctx :response (handler-fn (:request ctx))))})

(defn- test-api-enabled? []
  (= (System/getenv "ENABLE_TEST_API") "true"))

(defn make-routes
  "Build Pedestal route table with all handlers wired up."
  [config ds]
  (let [auth     (auth-int/require-auth config ds)
        admin    admin-int/require-admin
        err      error-int/error-handler
        json-in  json-int/json-body
        json-out json-int/json-response
        mp       multipart-int/multipart-params

        ;; Common interceptor chains
        public-chain  [err json-in json-out]
        auth-chain    [err json-in json-out auth]
        admin-chain   [err json-in json-out auth admin]
        upload-chain  [err mp json-out auth]

        ;; Helpers to wrap handler functions as pedestal interceptors
        h (fn [name-kw fn-val]
            (handler->interceptor fn-val name-kw))

        base-routes
        [["/health"
          :get
          (conj public-chain (h ::health health/health-handler))
          :route-name ::health]

         ["/.well-known/jwks.json"
          :get
          (conj public-chain (h ::jwks (jwks-h/jwks-handler config)))
          :route-name ::jwks]

         ;; Auth
         ["/api/v1/auth/register"
          :post
          (conj public-chain (h ::register (auth-h/register-handler config ds)))
          :route-name ::register]

         ["/api/v1/auth/login"
          :post
          (conj public-chain (h ::login (auth-h/login-handler config ds)))
          :route-name ::login]

         ["/api/v1/auth/refresh"
          :post
          (conj public-chain (h ::refresh (auth-h/refresh-handler config ds)))
          :route-name ::refresh]

         ["/api/v1/auth/logout"
          :post
          (conj public-chain (h ::logout (auth-h/logout-handler config ds)))
          :route-name ::logout]

         ["/api/v1/auth/logout-all"
          :post
          (conj auth-chain (h ::logout-all (auth-h/logout-all-handler config ds)))
          :route-name ::logout-all]

         ;; User profile
         ["/api/v1/users/me"
          :get
          (conj auth-chain (h ::get-profile (user-h/get-profile-handler ds)))
          :route-name ::get-profile]

         ["/api/v1/users/me"
          :patch
          (conj auth-chain (h ::update-profile (user-h/update-profile-handler ds)))
          :route-name ::update-profile]

         ["/api/v1/users/me/password"
          :post
          (conj auth-chain (h ::change-password (user-h/change-password-handler ds)))
          :route-name ::change-password]

         ["/api/v1/users/me/deactivate"
          :post
          (conj auth-chain (h ::deactivate (user-h/deactivate-handler ds)))
          :route-name ::deactivate]

         ;; Admin — list (static) before parameterized paths
         ["/api/v1/admin/users"
          :get
          (conj admin-chain (h ::list-users (admin-h/list-users-handler ds)))
          :route-name ::list-users]

         ["/api/v1/admin/users/:id/disable"
          :post
          (conj admin-chain (h ::disable-user (admin-h/disable-user-handler ds)))
          :route-name ::disable-user]

         ["/api/v1/admin/users/:id/enable"
          :post
          (conj admin-chain (h ::enable-user (admin-h/enable-user-handler ds)))
          :route-name ::enable-user]

         ["/api/v1/admin/users/:id/unlock"
          :post
          (conj admin-chain (h ::unlock-user (admin-h/unlock-user-handler ds)))
          :route-name ::unlock-user]

         ["/api/v1/admin/users/:id/force-password-reset"
          :post
          (conj admin-chain (h ::force-password-reset (admin-h/force-password-reset-handler config ds)))
          :route-name ::force-password-reset]

         ;; Expenses — static routes BEFORE parameterized :id routes
         ["/api/v1/expenses"
          :post
          (conj auth-chain (h ::create-expense (expense-h/create-expense-handler ds)))
          :route-name ::create-expense]

         ["/api/v1/expenses"
          :get
          (conj auth-chain (h ::list-expenses (expense-h/list-expenses-handler ds)))
          :route-name ::list-expenses]

         ;; /expenses/summary must be registered BEFORE /expenses/:id
         ["/api/v1/expenses/summary"
          :get
          (conj auth-chain (h ::expense-summary (expense-h/summary-handler ds)))
          :route-name ::expense-summary]

         ["/api/v1/expenses/:id"
          :get
          (conj auth-chain (h ::get-expense (expense-h/get-expense-handler ds)))
          :route-name ::get-expense]

         ["/api/v1/expenses/:id"
          :put
          (conj auth-chain (h ::update-expense (expense-h/update-expense-handler ds)))
          :route-name ::update-expense]

         ["/api/v1/expenses/:id"
          :delete
          (conj auth-chain (h ::delete-expense (expense-h/delete-expense-handler ds)))
          :route-name ::delete-expense]

         ;; Attachments
         ["/api/v1/expenses/:id/attachments"
          :post
          (conj upload-chain (h ::upload-attachment (attachment-h/upload-attachment-handler ds)))
          :route-name ::upload-attachment]

         ["/api/v1/expenses/:id/attachments"
          :get
          (conj auth-chain (h ::list-attachments (attachment-h/list-attachments-handler ds)))
          :route-name ::list-attachments]

         ["/api/v1/expenses/:id/attachments/:attachment-id"
          :delete
          (conj auth-chain (h ::delete-attachment (attachment-h/delete-attachment-handler ds)))
          :route-name ::delete-attachment]

         ;; Reports
         ["/api/v1/reports/pl"
          :get
          (conj auth-chain (h ::pl-report (report-h/pl-report-handler ds)))
          :route-name ::pl-report]

         ;; Token management
         ["/api/v1/tokens/claims"
          :get
          (conj public-chain (h ::token-claims (token-h/token-claims-handler config)))
          :route-name ::token-claims]]

        ;; Test-only routes — only appended when ENABLE_TEST_API=true
        test-routes (when (test-api-enabled?)
                      [["/api/v1/test/reset-db"
                        :post
                        (conj public-chain (h ::reset-db (test-api-h/reset-db-handler! ds)))
                        :route-name ::reset-db]

                       ["/api/v1/test/promote-admin"
                        :post
                        (conj public-chain (h ::promote-admin (test-api-h/promote-admin-handler! ds)))
                        :route-name ::promote-admin]])]

    ;; Use table/table-routes with a vector to guarantee ordering:
    ;; static routes must appear before parameterized routes so that
    ;; the linear-search router matches them first.
    (table/table-routes
      (into base-routes test-routes))))
