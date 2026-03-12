(ns step-definitions.steps
  "Cucumber step definitions for all feature files."
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [lambdaisland.cucumber.dsl :refer [Given When Then]]
            [step-definitions.common :as common]))

;; ============================================================
;; Server lifecycle
;; ============================================================

(Given "the API is running" [state]
  (if (:server state)
    state
    (merge state (common/start-test-server!))))

;; ============================================================
;; User setup steps
;; ============================================================

(Given "a user {string} is registered with password {string}" [state username password]
  (common/register-user! state username :password password))

(Given "a user {string} is registered with email {string} and password {string}"
  [state username email password]
  (common/register-user! state username :email email :password password))

(Given "a user {string} is registered" [state username]
  (common/register-user! state username))

(Given "users {string}, {string}, and {string} are registered" [state u1 u2 u3]
  (-> state
      (common/register-user! u1)
      (common/register-user! u2)
      (common/register-user! u3)))

(Given "an admin user {string} is registered and logged in" [state username]
  (let [new-state (-> state
                      (common/register-user! username)
                      (common/login-user! username))
        ds        (:ds new-state)
        user-id   (common/get-user-id new-state username)]
    ;; Ensure the user has ADMIN role regardless of registration order
    (when (and ds user-id)
      (jdbc/execute! ds
                     ["UPDATE users SET role = 'ADMIN' WHERE id = ?" user-id]))
    ;; Re-login so the JWT reflects the updated ADMIN role
    (common/login-user! new-state username)))

(Given "{string} has logged in and stored the access token" [state username]
  (common/login-user! state username))

(Given "{string} has logged in and stored the access token and refresh token" [state username]
  (common/login-user! state username))

(Given "a user {string} is registered and deactivated" [state username]
  (let [new-state (common/register-user! state username)
        login-resp (http/post (str (common/base-url new-state) "/api/v1/auth/login")
                              {:body         (json/generate-string {:username username
                                                                    :password "Str0ng#Pass1"})
                               :content-type :json
                               :throw-exceptions false})
        token (-> login-resp :body (json/parse-string true) :access_token)]
    (http/post (str (common/base-url new-state) "/api/v1/users/me/deactivate")
               {:headers      {"Authorization" (str "Bearer " token)}
                :content-type :json
                :throw-exceptions false})
    new-state))

(Given "a user {string} is registered and locked after too many failed logins" [state username]
  (let [new-state (common/register-user! state username)]
    (dotimes [_ 5]
      (http/post (str (common/base-url new-state) "/api/v1/auth/login")
                 {:body         (json/generate-string {:username username
                                                       :password "WrongP@ss!"})
                  :content-type :json
                  :throw-exceptions false}))
    new-state))

(Given "an admin has unlocked alice's account" [state]
  (let [admin-state (-> state
                        (common/register-user! "superadmin-unlock")
                        (common/login-user! "superadmin-unlock"))
        ds          (:ds admin-state)
        admin-id    (common/get-user-id admin-state "superadmin-unlock")
        _           (when (and ds admin-id)
                      (jdbc/execute! ds
                                     ["UPDATE users SET role = 'ADMIN' WHERE id = ?" admin-id]))
        ;; Re-login to get JWT with ADMIN role
        admin-state (common/login-user! admin-state "superadmin-unlock")
        alice-id    (common/get-user-id state "alice")]
    (http/post (str (common/base-url state)
                    "/api/v1/admin/users/" alice-id "/unlock")
               {:headers      (common/auth-header admin-state "superadmin-unlock")
                :content-type :json
                :throw-exceptions false})
    state))

(Given "{string} has had the maximum number of failed login attempts" [state username]
  (dotimes [_ 5]
    (http/post (str (common/base-url state) "/api/v1/auth/login")
               {:body         (json/generate-string {:username username
                                                     :password "WrongP@ss!"})
                :content-type :json
                :throw-exceptions false}))
  state)

;; ============================================================
;; Token lifecycle setup
;; ============================================================

(Given "alice's refresh token has expired" [state]
  (assoc state :alice-refresh-token "invalid.expired.token"))

(Given "alice has used her refresh token to get a new token pair" [state]
  (let [refresh-token (common/get-refresh-token state "alice")]
    (http/post (str (common/base-url state) "/api/v1/auth/refresh")
               {:body             (json/generate-string {:refresh_token refresh-token})
                :content-type     :json
                :throw-exceptions false})
    (assoc state :alice-original-refresh-token refresh-token)))

(Given "the user {string} has been deactivated" [state username]
  (let [new-state (common/login-user! state username)
        token     (common/get-access-token new-state username)]
    (http/post (str (common/base-url new-state) "/api/v1/users/me/deactivate")
               {:headers      {"Authorization" (str "Bearer " token)}
                :content-type :json
                :throw-exceptions false})
    new-state))

(Given "alice has already logged out once" [state]
  (let [token (common/get-access-token state "alice")]
    (http/post (str (common/base-url state) "/api/v1/auth/logout")
               {:headers      {"Authorization" (str "Bearer " token)}
                :content-type :json
                :throw-exceptions false}))
  state)

(Given "alice has logged out and her access token is blacklisted" [state]
  (let [token (common/get-access-token state "alice")]
    (http/post (str (common/base-url state) "/api/v1/auth/logout")
               {:headers      {"Authorization" (str "Bearer " token)}
                :content-type :json
                :throw-exceptions false}))
  state)

;; ============================================================
;; Admin setup
;; ============================================================

(Given "alice's account has been disabled by the admin" [state]
  (let [alice-id (common/get-user-id state "alice")]
    (http/post (str (common/base-url state)
                    "/api/v1/admin/users/" alice-id "/disable")
               {:headers      (common/auth-header state "superadmin")
                :body         (json/generate-string {:reason "Test"})
                :content-type :json
                :throw-exceptions false}))
  state)

(Given "alice's account has been disabled" [state]
  (let [admin-state (-> state
                        (common/register-user! "superadmin-disable")
                        (common/login-user! "superadmin-disable"))
        ds          (:ds admin-state)
        admin-id    (common/get-user-id admin-state "superadmin-disable")
        _           (when (and ds admin-id)
                      (jdbc/execute! ds
                                     ["UPDATE users SET role = 'ADMIN' WHERE id = ?" admin-id]))
        admin-state (common/login-user! admin-state "superadmin-disable")
        alice-id    (common/get-user-id state "alice")]
    (http/post (str (common/base-url state)
                    "/api/v1/admin/users/" alice-id "/disable")
               {:headers      (common/auth-header admin-state "superadmin-disable")
                :body         (json/generate-string {:reason "Test"})
                :content-type :json
                :throw-exceptions false}))
  state)

(Given "^the admin has disabled alice's account via POST /api/v1/admin/users/\\{alice_id\\}/disable$"
  [state]
  (let [alice-id (common/get-user-id state "alice")]
    (http/post (str (common/base-url state)
                    "/api/v1/admin/users/" alice-id "/disable")
               {:headers      (common/auth-header state "superadmin")
                :body         (json/generate-string {:reason "Test"})
                :content-type :json
                :throw-exceptions false}))
  state)

;; ============================================================
;; Expense setup — unquoted body, so regex patterns required
;; ============================================================

(Given "^alice has created an entry with body (.+)$" [state body-str]
  (let [token (common/get-access-token state "alice")
        resp  (http/post (str (common/base-url state) "/api/v1/expenses")
                         {:headers      {"Authorization" (str "Bearer " token)}
                          :body         body-str
                          :content-type :json
                          :throw-exceptions false})
        body  (common/parse-json (:body resp))]
    (assoc state :alice-expense-id (:id body)
           :alice-last-expense body)))

(Given "^alice has created an expense with body (.+)$" [state body-str]
  (let [token (common/get-access-token state "alice")
        resp  (http/post (str (common/base-url state) "/api/v1/expenses")
                         {:headers      {"Authorization" (str "Bearer " token)}
                          :body         body-str
                          :content-type :json
                          :throw-exceptions false})
        body  (common/parse-json (:body resp))]
    (assoc state :alice-expense-id (:id body)
           :alice-last-expense body)))

(Given "alice has created 3 entries" [state]
  (let [token (common/get-access-token state "alice")]
    (dotimes [i 3]
      (http/post (str (common/base-url state) "/api/v1/expenses")
                 {:headers      {"Authorization" (str "Bearer " token)}
                  :body         (json/generate-string
                                  {:amount      "10.00"
                                   :currency    "USD"
                                   :category    "food"
                                   :description (str "Entry " i)
                                   :date        "2025-01-15"
                                   :type        "expense"})
                  :content-type :json
                  :throw-exceptions false})))
  state)

(Given "^bob has created an entry with body (.+)$" [state body-str]
  (let [new-state (common/login-user! state "bob")
        token     (common/get-access-token new-state "bob")
        resp      (http/post (str (common/base-url new-state) "/api/v1/expenses")
                             {:headers      {"Authorization" (str "Bearer " token)}
                              :body         body-str
                              :content-type :json
                              :throw-exceptions false})
        body      (common/parse-json (:body resp))]
    (assoc new-state :bob-expense-id (:id body)
           :bob-last-expense body)))

;; ============================================================
;; Attachment setup
;; ============================================================

(Given "alice has uploaded file {string} with content type {string} to the entry" [state filename content-type]
  (let [token      (common/get-access-token state "alice")
        expense-id (:alice-expense-id state)
        data       (byte-array [0xFF 0xD8 0xFF 0xE0])
        resp       (http/post (str (common/base-url state)
                                   "/api/v1/expenses/" expense-id "/attachments")
                              {:headers      {"Authorization" (str "Bearer " token)}
                               ;; clj-http byte-array multipart: :part-name = form field name,
                               ;; :name = filename in Content-Disposition header
                               :multipart    [{:part-name "file"
                                               :name      filename
                                               :content   data
                                               :mime-type content-type}]
                               :throw-exceptions false})
        body       (common/parse-json (:body resp))]
    (assoc state :alice-attachment-id (:id body)
           :alice-last-attachment body)))

;; ============================================================
;; Self-deactivation setup
;; ============================================================

(Given "^alice has deactivated her own account via POST /api/v1/users/me/deactivate$" [state]
  (let [token (common/get-access-token state "alice")]
    (http/post (str (common/base-url state) "/api/v1/users/me/deactivate")
               {:headers      {"Authorization" (str "Bearer " token)}
                :content-type :json
                :throw-exceptions false}))
  state)

;; ============================================================
;; Public HTTP actions — paths are UNQUOTED in Gherkin, so regex required
;; ============================================================

(When "^the client sends POST (.+?) with body (.+)$" [state path body-str]
  (let [resp (http/post (str (common/base-url state) path)
                        {:body             body-str
                         :content-type     :json
                         :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

;; More specific pattern (with trailing context) must be defined before the generic GET
(When "^the client sends GET (.+?) with alice's access token$" [state path]
  (let [token (common/get-access-token state "alice")
        resp  (http/get (str (common/base-url state) path)
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^the client sends GET (.+)$" [state path]
  (let [resp (http/get (str (common/base-url state) path)
                       {:throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^an operations engineer sends GET (.+)$" [state path]
  (let [resp (http/get (str (common/base-url state) path)
                       {:throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^an unauthenticated engineer sends GET (.+)$" [state path]
  (let [resp (http/get (str (common/base-url state) path)
                       {:throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

;; ============================================================
;; Authenticated user actions (alice) — fixed paths, no args
;; ============================================================

(When "^alice sends GET /api/v1/users/me$" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/get (str (common/base-url state) "/api/v1/users/me")
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends GET /api/v1/expenses$" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/get (str (common/base-url state) "/api/v1/expenses")
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends GET /api/v1/expenses/\\{expenseId\\}$" [state]
  (let [token      (common/get-access-token state "alice")
        expense-id (:alice-expense-id state)
        resp       (http/get (str (common/base-url state) "/api/v1/expenses/" expense-id)
                             {:headers      {"Authorization" (str "Bearer " token)}
                              :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends GET /api/v1/expenses/summary$" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/get (str (common/base-url state) "/api/v1/expenses/summary")
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends GET /api/v1/reports/pl\\?(.+)$" [state query-string]
  (let [token (common/get-access-token state "alice")
        resp  (http/get (str (common/base-url state) "/api/v1/reports/pl?" query-string)
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends POST /api/v1/users/me/deactivate$" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/post (str (common/base-url state) "/api/v1/users/me/deactivate")
                         {:headers      {"Authorization" (str "Bearer " token)}
                          :content-type :json
                          :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends POST /api/v1/users/me/password with body (.+)$" [state body-str]
  (let [token (common/get-access-token state "alice")
        resp  (http/post (str (common/base-url state) "/api/v1/users/me/password")
                         {:headers      {"Authorization" (str "Bearer " token)}
                          :body         body-str
                          :content-type :json
                          :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends POST /api/v1/expenses with body (.+)$" [state body-str]
  (let [token (common/get-access-token state "alice")
        resp  (http/post (str (common/base-url state) "/api/v1/expenses")
                         {:headers      {"Authorization" (str "Bearer " token)}
                          :body         body-str
                          :content-type :json
                          :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends PATCH /api/v1/users/me with body (.+)$" [state body-str]
  (let [token (common/get-access-token state "alice")
        resp  (http/patch (str (common/base-url state) "/api/v1/users/me")
                          {:headers      {"Authorization" (str "Bearer " token)}
                           :body         body-str
                           :content-type :json
                           :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends PUT /api/v1/expenses/\\{expenseId\\} with body (.+)$" [state body-str]
  (let [token      (common/get-access-token state "alice")
        expense-id (:alice-expense-id state)
        resp       (http/put (str (common/base-url state) "/api/v1/expenses/" expense-id)
                             {:headers      {"Authorization" (str "Bearer " token)}
                              :body         body-str
                              :content-type :json
                              :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends DELETE /api/v1/expenses/\\{expenseId\\}$" [state]
  (let [token      (common/get-access-token state "alice")
        expense-id (:alice-expense-id state)
        resp       (http/delete (str (common/base-url state) "/api/v1/expenses/" expense-id)
                                {:headers      {"Authorization" (str "Bearer " token)}
                                 :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

;; ============================================================
;; Auth token actions — fixed paths, no captures needed
;; ============================================================

(When "^alice sends POST /api/v1/auth/refresh with her refresh token$" [state]
  (let [refresh-token (or (:alice-refresh-token state)
                          (common/get-refresh-token state "alice"))
        resp          (http/post (str (common/base-url state) "/api/v1/auth/refresh")
                                 {:body             (json/generate-string {:refresh_token refresh-token})
                                  :content-type     :json
                                  :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends POST /api/v1/auth/refresh with her original refresh token$" [state]
  (let [refresh-token (or (:alice-original-refresh-token state)
                          (common/get-refresh-token state "alice"))
        resp          (http/post (str (common/base-url state) "/api/v1/auth/refresh")
                                 {:body             (json/generate-string {:refresh_token refresh-token})
                                  :content-type     :json
                                  :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends POST /api/v1/auth/logout with her access token$" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/post (str (common/base-url state) "/api/v1/auth/logout")
                         {:headers      {"Authorization" (str "Bearer " token)}
                          :content-type :json
                          :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends POST /api/v1/auth/logout-all with her access token$" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/post (str (common/base-url state) "/api/v1/auth/logout-all")
                         {:headers      {"Authorization" (str "Bearer " token)}
                          :content-type :json
                          :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

;; ============================================================
;; Admin actions — paths are UNQUOTED in Gherkin, regex required
;; ============================================================

(When "^the admin sends GET (.+)$" [state path]
  (let [token (common/get-access-token state "superadmin")
        resp  (http/get (str (common/base-url state) path)
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/disable with body (.+)$"
  [state body-str]
  (let [token    (common/get-access-token state "superadmin")
        alice-id (common/get-user-id state "alice")
        resp     (http/post (str (common/base-url state)
                                 "/api/v1/admin/users/" alice-id "/disable")
                            {:headers      {"Authorization" (str "Bearer " token)}
                             :body         body-str
                             :content-type :json
                             :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/enable$" [state]
  (let [token    (common/get-access-token state "superadmin")
        alice-id (common/get-user-id state "alice")
        resp     (http/post (str (common/base-url state)
                                 "/api/v1/admin/users/" alice-id "/enable")
                            {:headers      {"Authorization" (str "Bearer " token)}
                             :content-type :json
                             :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/force-password-reset$" [state]
  (let [token    (common/get-access-token state "superadmin")
        alice-id (common/get-user-id state "alice")
        resp     (http/post (str (common/base-url state)
                                 "/api/v1/admin/users/" alice-id "/force-password-reset")
                            {:headers      {"Authorization" (str "Bearer " token)}
                             :content-type :json
                             :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/unlock$" [state]
  (let [token    (common/get-access-token state "superadmin")
        alice-id (common/get-user-id state "alice")
        resp     (http/post (str (common/base-url state)
                                 "/api/v1/admin/users/" alice-id "/unlock")
                            {:headers      {"Authorization" (str "Bearer " token)}
                             :content-type :json
                             :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

;; ============================================================
;; Token introspection actions
;; ============================================================

(When "^alice decodes her access token payload$" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/get (str (common/base-url state) "/api/v1/tokens/claims")
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp))
           :token-claims (common/parse-json (:body resp)))))

;; ============================================================
;; Attachment actions
;; ============================================================

(When "^alice uploads file \"([^\"]*)\" with content type \"([^\"]*)\" to POST /api/v1/expenses/\\{expenseId\\}/attachments$"
  [state filename content-type]
  (let [token      (common/get-access-token state "alice")
        expense-id (:alice-expense-id state)
        data       (byte-array [0xFF 0xD8 0xFF 0xE0])
        resp       (http/post (str (common/base-url state)
                                   "/api/v1/expenses/" expense-id "/attachments")
                              {:headers      {"Authorization" (str "Bearer " token)}
                               :multipart    [{:part-name "file"
                                               :name      filename
                                               :content   data
                                               :mime-type content-type}]
                               :throw-exceptions false})
        body       (common/parse-json (:body resp))]
    (assoc state :last-response resp
           :last-body body
           :alice-attachment-id (:id body))))

(When "^alice uploads file \"([^\"]*)\" with content type \"([^\"]*)\" to POST /api/v1/expenses/\\{bobExpenseId\\}/attachments$"
  [state filename content-type]
  (let [token      (common/get-access-token state "alice")
        expense-id (:bob-expense-id state)
        data       (byte-array [0xFF 0xD8 0xFF 0xE0])
        resp       (http/post (str (common/base-url state)
                                   "/api/v1/expenses/" expense-id "/attachments")
                              {:headers      {"Authorization" (str "Bearer " token)}
                               :multipart    [{:part-name "file"
                                               :name      filename
                                               :content   data
                                               :mime-type content-type}]
                               :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice uploads an oversized file to POST /api/v1/expenses/\\{expenseId\\}/attachments$" [state]
  (let [token      (common/get-access-token state "alice")
        expense-id (:alice-expense-id state)
        big-data   (byte-array (+ (* 10 1024 1024) 1))
        resp       (http/post (str (common/base-url state)
                                   "/api/v1/expenses/" expense-id "/attachments")
                              {:headers      {"Authorization" (str "Bearer " token)}
                               :multipart    [{:part-name "file"
                                               :name      "big.jpg"
                                               :content   big-data
                                               :mime-type "image/jpeg"}]
                               :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends GET /api/v1/expenses/\\{expenseId\\}/attachments$" [state]
  (let [token      (common/get-access-token state "alice")
        expense-id (:alice-expense-id state)
        resp       (http/get (str (common/base-url state)
                                  "/api/v1/expenses/" expense-id "/attachments")
                             {:headers      {"Authorization" (str "Bearer " token)}
                              :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends GET /api/v1/expenses/\\{bobExpenseId\\}/attachments$" [state]
  (let [token      (common/get-access-token state "alice")
        expense-id (:bob-expense-id state)
        resp       (http/get (str (common/base-url state)
                                  "/api/v1/expenses/" expense-id "/attachments")
                             {:headers      {"Authorization" (str "Bearer " token)}
                              :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{attachmentId\\}$" [state]
  (let [token         (common/get-access-token state "alice")
        expense-id    (:alice-expense-id state)
        attachment-id (:alice-attachment-id state)
        resp          (http/delete (str (common/base-url state)
                                        "/api/v1/expenses/" expense-id
                                        "/attachments/" attachment-id)
                                   {:headers      {"Authorization" (str "Bearer " token)}
                                    :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends DELETE /api/v1/expenses/\\{bobExpenseId\\}/attachments/\\{attachmentId\\}$" [state]
  (let [token         (common/get-access-token state "alice")
        expense-id    (:bob-expense-id state)
        attachment-id (:alice-attachment-id state)
        resp          (http/delete (str (common/base-url state)
                                        "/api/v1/expenses/" expense-id
                                        "/attachments/" attachment-id)
                                   {:headers      {"Authorization" (str "Bearer " token)}
                                    :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

(When "^alice sends DELETE /api/v1/expenses/\\{expenseId\\}/attachments/\\{randomAttachmentId\\}$" [state]
  (let [token      (common/get-access-token state "alice")
        expense-id (:alice-expense-id state)
        rand-id    (str (java.util.UUID/randomUUID))
        resp       (http/delete (str (common/base-url state)
                                     "/api/v1/expenses/" expense-id
                                     "/attachments/" rand-id)
                                {:headers      {"Authorization" (str "Bearer " token)}
                                 :throw-exceptions false})]
    (assoc state :last-response resp
           :last-body (common/parse-json (:body resp)))))

;; ============================================================
;; Assertion steps
;; ============================================================

(Then "the response status code should be {int}" [state status-code]
  (is (= status-code (:status (:last-response state)))
      (str "Expected " status-code " but got " (:status (:last-response state))
           "\nBody: " (:body (:last-response state))))
  state)

(Then "the health status should be {string}" [state expected-status]
  (is (= expected-status (:status (:last-body state))))
  state)

(Then "the response should not include detailed component health information" [state]
  (let [body (:last-body state)]
    (is (nil? (:components body)))
    (is (nil? (:details body))))
  state)

(Then "the response body should contain {string} equal to {string}" [state field value]
  (let [k    (keyword (str/replace field #"_" "-"))
        k2   (keyword field)
        body (:last-body state)]
    (is (or (= value (get body k))
            (= value (get body k2))
            (= value (str (get body k)))
            (= value (str (get body k2))))
        (str "Expected field " field " = " value " in " body)))
  state)

(Then "the response body should contain {string} equal to {double}" [state field value]
  (let [k2     (keyword field)
        body   (:last-body state)
        actual (or (get body (keyword (str/replace field #"_" "-"))) (get body k2))]
    (is (= value (double actual))
        (str "Expected " field " = " value " in " body)))
  state)

(Then "the response body should contain {string} equal to {int}" [state field value]
  (let [k2     (keyword field)
        body   (:last-body state)
        actual (or (get body (keyword (str/replace field #"_" "-"))) (get body k2))]
    (is (= (double value) (double actual))
        (str "Expected " field " = " value " in " body)))
  state)

(Then "the response body should contain a non-null {string} field" [state field]
  (let [k    (keyword (str/replace field #"_" "-"))
        k2   (keyword field)
        body (:last-body state)]
    (is (or (some? (get body k)) (some? (get body k2)))
        (str "Expected non-null " field " in " body)))
  state)

(Then "the response body should not contain a {string} field" [state field]
  (let [k    (keyword (str/replace field #"_" "-"))
        k2   (keyword field)
        body (:last-body state)]
    (is (and (nil? (get body k)) (nil? (get body k2)))
        (str "Expected " field " to be absent in " body)))
  state)

(Then "the response body should contain an error message about invalid credentials" [state]
  (is (or (some? (:error (:last-body state)))
          (some? (:message (:last-body state)))))
  state)

(Then "the response body should contain an error message about account deactivation" [state]
  (is (or (some? (:error (:last-body state)))
          (some? (:message (:last-body state)))))
  state)

(Then "the response body should contain an error message about token expiration" [state]
  (is (or (some? (:error (:last-body state)))
          (some? (:message (:last-body state)))))
  state)

(Then "the response body should contain an error message about invalid token" [state]
  (is (or (some? (:error (:last-body state)))
          (some? (:message (:last-body state)))))
  state)

(Then "the response body should contain a validation error for {string}" [state field]
  (let [body (:last-body state)]
    (is (or (= field (:field body))
            (some? (:error body))
            (some? (:message body)))
        (str "Expected validation error for " field " in " body)))
  state)

(Then "the response body should contain {string} total equal to {string}" [state currency total]
  (let [body   (:last-body state)
        actual (or (get body (keyword currency))
                   (get body currency))]
    (is (= total (str actual))
        (str "Expected " currency " = " total " in " body)))
  state)

(Then "the response body should contain at least one user with {string} equal to {string}"
  [state field value]
  (let [data (get (:last-body state) :data)
        k    (keyword field)]
    (is (some #(= value (get % k)) data)
        (str "Expected user with " field " = " value)))
  state)

(Then "the response body should contain at least one key in the {string} array" [state field]
  (let [arr (get (:last-body state) (keyword field))]
    (is (and (some? arr) (pos? (count arr)))))
  state)

(Then "alice's account status should be {string}" [state expected-status]
  (let [alice-id    (common/get-user-id state "alice")
        admin-token (common/get-access-token state "superadmin")]
    (if admin-token
      ;; Admin is available — use admin users endpoint
      (let [resp  (http/get (str (common/base-url state) "/api/v1/admin/users")
                            {:headers      {"Authorization" (str "Bearer " admin-token)}
                             :throw-exceptions false})
            users (-> resp :body (json/parse-string true) :data)
            alice (first (filter #(= alice-id (:id %)) users))]
        (is (= (clojure.string/lower-case expected-status)
               (clojure.string/lower-case (or (:status alice) "")))))
      ;; No admin — verify via attempted login behaviour
      (let [expected-lower (clojure.string/lower-case expected-status)]
        (cond
          (= "locked" expected-lower)
          (let [resp (http/post (str (common/base-url state) "/api/v1/auth/login")
                                {:body             (json/generate-string {:username "alice"
                                                                          :password "Str0ng#Pass1"})
                                 :content-type     :json
                                 :throw-exceptions false})]
            (is (= 401 (:status resp))
                "Locked account should return 401"))
          :else
          (is false (str "Cannot verify status " expected-status " without admin token"))))))
  state)

(Then "alice's access token should be invalidated" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/get (str (common/base-url state) "/api/v1/users/me")
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (is (= 401 (:status resp))))
  state)

(Then "alice's access token should be recorded as revoked" [state]
  (let [token (common/get-access-token state "alice")
        resp  (http/get (str (common/base-url state) "/api/v1/users/me")
                        {:headers      {"Authorization" (str "Bearer " token)}
                         :throw-exceptions false})]
    (is (= 401 (:status resp))))
  state)

(Then "the token should contain a non-null {string} claim" [state claim]
  (let [claims (:token-claims state)]
    (is (some? (get claims (keyword claim)))
        (str "Expected non-null claim " claim " in " claims)))
  state)

(Then "the response body should contain {int} items in the {string} array" [state cnt field]
  (let [arr (get (:last-body state) (keyword field))]
    (is (= cnt (count arr))
        (str "Expected " cnt " items in " field " but got " (count arr))))
  state)

(Then "the response body should contain an attachment with {string} equal to {string}"
  [state field value]
  (let [attachments (:attachments (:last-body state))
        k           (keyword field)]
    (is (some #(= value (get % k)) attachments)
        (str "Expected attachment with " field " = " value)))
  state)

(Then "the income breakdown should contain {string} with amount {string}" [state category amount]
  (let [body      (:last-body state)
        breakdown (or (:income_breakdown body) (:income-breakdown body))
        actual    (get breakdown (keyword category))]
    (is (= amount (str actual))
        (str "Expected income breakdown " category " = " amount " in " breakdown)))
  state)

(Then "the expense breakdown should contain {string} with amount {string}" [state category amount]
  (let [body      (:last-body state)
        breakdown (or (:expense_breakdown body) (:expense-breakdown body))
        actual    (get breakdown (keyword category))]
    (is (= amount (str actual))
        (str "Expected expense breakdown " category " = " amount " in " breakdown)))
  state)

(Then "the response body should contain an error message about duplicate username" [state]
  (is (or (some? (:error (:last-body state)))
          (some? (:message (:last-body state)))))
  state)

(Then "the response body should contain an error message about file size" [state]
  (is (or (some? (:error (:last-body state)))
          (some? (:message (:last-body state)))))
  state)
