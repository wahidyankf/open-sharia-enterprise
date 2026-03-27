(ns step-definitions.common
  "Shared utilities for step definitions.
   Calls service handler functions directly — no HTTP client, no embedded server."
  (:require [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [demo-be-cjpd.auth.jwt :as jwt]
            [demo-be-cjpd.config :as config]
            [demo-be-cjpd.db.core :as db]
            [demo-be-cjpd.db.jdbc-attachment-repo :as jdbc-attachment-repo]
            [demo-be-cjpd.db.jdbc-expense-repo :as jdbc-expense-repo]
            [demo-be-cjpd.db.jdbc-token-repo :as jdbc-token-repo]
            [demo-be-cjpd.db.jdbc-user-repo :as jdbc-user-repo]
            [demo-be-cjpd.db.protocols :as proto]
            [demo-be-cjpd.db.schema :as schema]
            [demo-be-cjpd.in-memory-repos :as mem]
            [demo-be-cjpd.handlers.admin :as admin-handler]
            [demo-be-cjpd.handlers.attachment :as attachment-handler]
            [demo-be-cjpd.handlers.auth :as auth-handler]
            [demo-be-cjpd.handlers.expense :as expense-handler]
            [demo-be-cjpd.handlers.health :as health-handler]
            [demo-be-cjpd.handlers.jwks :as jwks-handler]
            [demo-be-cjpd.handlers.report :as report-handler]
            [demo-be-cjpd.handlers.token :as token-handler]
            [demo-be-cjpd.handlers.user :as user-handler]))

;; ============================================================
;; Test context bootstrap
;; ============================================================

(defn- integration-mode? []
  (some? (System/getenv "DATABASE_URL")))

;; Shared datasource: created once for the entire test suite run.
;; Prevents exhausting PostgreSQL's max_connections across 70+ scenarios.
(defonce ^:private shared-test-context
  (delay
    (if (integration-mode?)
      ;; Integration mode: real PostgreSQL via DATABASE_URL
      (let [database-url (System/getenv "DATABASE_URL")
            cfg          (assoc (config/load-config) :database-url database-url)
            ds           (db/create-datasource database-url)
            _            (schema/create-schema! ds database-url)
            user-repo       (jdbc-user-repo/create ds)
            expense-repo    (jdbc-expense-repo/create ds)
            attachment-repo (jdbc-attachment-repo/create ds)
            token-repo      (jdbc-token-repo/create ds)]
        {:ds             ds
         :config         cfg
         :database-url   database-url
         :server         :direct
         :base-url       "http://localhost:0"
         :user-repo      user-repo
         :expense-repo   expense-repo
         :attachment-repo attachment-repo
         :token-repo     token-repo})
      ;; Unit mode: in-memory repos — no real DB connection
      (let [cfg (config/load-config)]
        {:ds              nil
         :config          cfg
         :database-url    nil
         :server          :direct
         :base-url        "http://localhost:0"
         :in-memory?      true
         :user-repo       (mem/make-user-repo)
         :expense-repo    (mem/make-expense-repo)
         :attachment-repo (mem/make-attachment-repo)
         :token-repo      (mem/make-token-repo)}))))

(defn- truncate-in-memory-repos!
  "Reset all in-memory repo stores for test isolation."
  [ctx]
  (when-let [user-repo (:user-repo ctx)]
    (reset! (:store user-repo) {}))
  (when-let [expense-repo (:expense-repo ctx)]
    (reset! (:store expense-repo) {}))
  (when-let [attachment-repo (:attachment-repo ctx)]
    (reset! (:store attachment-repo) {})
    (reset! (:data-store attachment-repo) {}))
  (when-let [token-repo (:token-repo ctx)]
    (reset! (:store token-repo) {})))

(defn- truncate-db-tables!
  "Delete all rows from every table (used in integration mode)."
  [ds]
  (when ds
    (jdbc/execute! ds ["DELETE FROM attachments"])
    (jdbc/execute! ds ["DELETE FROM expenses"])
    (jdbc/execute! ds ["DELETE FROM revoked_tokens"])
    (jdbc/execute! ds ["DELETE FROM users"])))

(defn start-test-server!
  "Return the shared test context, resetting all data for test isolation.
   In unit mode: resets in-memory stores.
   In integration mode: truncates DB tables."
  []
  (let [ctx @shared-test-context]
    (if (integration-mode?)
      (truncate-db-tables! (:ds ctx))
      (truncate-in-memory-repos! ctx))
    ctx))

(defn stop-test-server! [{:keys [ds]}]
  (when ds (.close ds)))

;; ============================================================
;; Request / response helpers
;; ============================================================

(defn parse-json [body]
  (try
    (cond
      (string? body)  (json/parse-string body true)
      (map? body)     body
      :else           {})
    (catch Exception _
      {})))

(defn- response->result
  "Normalise a Ring response map to {:status N :body parsed-map}.
   Handlers return {:status N :headers {} :body json-string}."
  [resp]
  {:status (:status resp)
   :body   (parse-json (:body resp))})

;; ============================================================
;; Auth interceptor logic (replicated for direct calls)
;; ============================================================

(defn- resolve-identity
  "Replicates the require-auth interceptor logic.
   Returns identity map if the token is valid and the user is ACTIVE,
   or throws an ex-info whose :status and :message describe the error."
  [config user-repo token-repo token]
  (when-not token
    (throw (ex-info "Missing or invalid authorization token"
                    {:status 401 :message "Missing or invalid authorization token"})))
  (let [claims (jwt/verify-token (:jwt-secret config) token)]
    (when-not claims
      (throw (ex-info "Invalid or expired token"
                      {:status 401 :message "Invalid or expired token"})))
    (let [jti     (:jti claims)
          user-id (:sub claims)
          iat     (or (:iat claims) 0)]
      (when (proto/token-revoked? token-repo jti)
        (throw (ex-info "Token has been revoked"
                        {:status 401 :message "Token has been revoked"})))
      (when (proto/all-revoked-for-user? token-repo user-id iat)
        (throw (ex-info "Token has been revoked"
                        {:status 401 :message "Token has been revoked"})))
      (let [user (proto/find-user-by-id user-repo user-id)]
        (when-not user
          (throw (ex-info "User not found"
                          {:status 401 :message "User not found"})))
        (when (not= "ACTIVE" (:status user))
          (throw (ex-info "User account is not active"
                          {:status 401 :message "User account is not active"})))
        {:user-id  user-id
         :username (:username claims)
         :role     (:role claims)
         :jti      jti
         :iat      iat}))))

(defn- require-admin-identity
  "Asserts that the identity has ADMIN role."
  [identity]
  (when (not= "ADMIN" (:role identity))
    (throw (ex-info "Forbidden: admin access required"
                    {:status 403 :message "Forbidden: admin access required"}))))

;; ============================================================
;; Request map builders
;; ============================================================

(defn- public-request
  "Build a Ring-like request map for unauthenticated endpoints."
  ([json-body]
   {:json-params (parse-json json-body)
    :headers     {}
    :query-params {}
    :path-params  {}})
  ([]
   (public-request nil)))

(defn- keywordize-map
  "Convert string keys in a map to keywords."
  [m]
  (into {} (map (fn [[k v]] [(if (string? k) (keyword k) k) v]) m)))

(defn- auth-request
  "Build a Ring-like request map with :identity for authenticated endpoints."
  [identity & {:keys [json-body query-params path-params headers]
               :or   {json-body nil query-params {} path-params {} headers {}}}]
  {:identity     identity
   :json-params  (if json-body (parse-json json-body) {})
   :headers      (merge {"host" "localhost"} headers)
   :query-params query-params
   :path-params  (keywordize-map path-params)})

;; ============================================================
;; Error handling wrapper
;; ============================================================

(defn- call-handler
  "Invoke a handler function, catching ex-info thrown by domain logic.
   Returns {:status N :body parsed-map}."
  [handler request]
  (try
    (response->result (handler request))
    (catch clojure.lang.ExceptionInfo e
      (let [data   (ex-data e)
            status (or (:status data) 500)
            msg    (or (:message data) (.getMessage e))]
        {:status status
         :body   {:message msg}}))
    (catch Exception e
      {:status 500
       :body   {:message (.getMessage e)}})))

(defn- call-protected-handler
  "Resolve identity from token, enforce auth, then call handler.
   Returns {:status N :body parsed-map}."
  [config user-repo token-repo token handler request-fn]
  (try
    (let [identity (resolve-identity config user-repo token-repo token)
          request  (request-fn identity)]
      (call-handler handler request))
    (catch clojure.lang.ExceptionInfo e
      (let [data   (ex-data e)
            status (or (:status data) 401)
            msg    (or (:message data) (.getMessage e))]
        {:status status
         :body   {:message msg}}))))

(defn- call-admin-handler
  "Resolve identity, enforce ADMIN role, then call handler.
   Returns {:status N :body parsed-map}."
  [config user-repo token-repo token handler request-fn]
  (try
    (let [identity (resolve-identity config user-repo token-repo token)
          _        (require-admin-identity identity)
          request  (request-fn identity)]
      (call-handler handler request))
    (catch clojure.lang.ExceptionInfo e
      (let [data   (ex-data e)
            status (or (:status data) 401)
            msg    (or (:message data) (.getMessage e))]
        {:status status
         :body   {:message msg}}))))

;; ============================================================
;; Public context accessors
;; ============================================================

(defn get-user-id [ctx username]
  (get ctx (keyword (str username "-id"))))

(defn get-access-token [ctx username]
  (get ctx (keyword (str username "-access-token"))))

(defn get-refresh-token [ctx username]
  (get ctx (keyword (str username "-refresh-token"))))

(defn auth-header [ctx username]
  {"Authorization" (str "Bearer " (get-access-token ctx username))})

;; base-url kept for assertion compatibility — not used to make HTTP calls
(defn base-url [_ctx]
  "http://localhost:0")

;; ============================================================
;; Admin promotion helper
;; ============================================================

(defn promote-to-admin!
  "Promote the user with the given ID to ADMIN role.
   Works for both in-memory repos (unit mode) and JDBC repos (integration mode)."
  [ctx user-id]
  (let [user-repo (:user-repo ctx)]
    (when (and user-repo user-id)
      (if (:in-memory? ctx)
        ;; Unit mode: update the atom store directly
        (swap! (:store user-repo) update user-id assoc :role "ADMIN")
        ;; Integration mode: direct SQL (role is not in the UserRepo protocol)
        (when-let [ds (:ds ctx)]
          (jdbc/execute! ds ["UPDATE users SET role = 'ADMIN' WHERE id = ?" user-id])))))
  ctx)

;; ============================================================
;; Core scenario helpers
;; ============================================================

(defn register-user!
  "Register a user by calling the register handler directly.
   Returns updated context with user id and password stored."
  [ctx username & {:keys [email password display-name]
                   :or   {email    (str username "@example.com")
                          password "Str0ng#Pass1"}}]
  (let [{:keys [config user-repo]} ctx
        handler (auth-handler/register-handler config user-repo)
        request (public-request (json/generate-string
                                  {:username    username
                                   :email       email
                                   :password    password
                                   :displayName (or display-name username)}))
        result  (call-handler handler request)
        body    (:body result)]
    (assoc ctx
           (keyword (str username "-user")) body
           (keyword (str username "-id")) (:id body)
           (keyword (str username "-password")) password)))

(defn login-user!
  "Authenticate a user by calling the login handler directly.
   Returns updated context with access and refresh tokens stored."
  [ctx username & {:keys [password]}]
  (let [{:keys [config user-repo]} ctx
        pw      (or password
                    (get ctx (keyword (str username "-password")))
                    "Str0ng#Pass1")
        handler (auth-handler/login-handler config user-repo)
        request (public-request (json/generate-string {:username username :password pw}))
        result  (call-handler handler request)
        body    (:body result)]
    (assoc ctx
           (keyword (str username "-access-token"))  (or (:accessToken body) (:access_token body))
           (keyword (str username "-refresh-token")) (or (:refreshToken body) (:refresh_token body))
           (keyword (str username "-login-response")) body)))

;; ============================================================
;; Direct service calls — public endpoints
;; ============================================================

(defn call-register
  "POST /api/v1/auth/register — direct handler call."
  [ctx body-str]
  (let [{:keys [config user-repo]} ctx
        handler (auth-handler/register-handler config user-repo)
        result  (call-handler handler (public-request body-str))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-login
  "POST /api/v1/auth/login — direct handler call."
  [ctx body-str]
  (let [{:keys [config user-repo]} ctx
        handler (auth-handler/login-handler config user-repo)
        result  (call-handler handler (public-request body-str))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-refresh
  "POST /api/v1/auth/refresh — direct handler call."
  [ctx body-str]
  (let [{:keys [config user-repo token-repo]} ctx
        handler (auth-handler/refresh-handler config user-repo token-repo)
        result  (call-handler handler (public-request body-str))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-health
  "GET /health — direct handler call (no auth)."
  [ctx]
  (let [result (response->result (health-handler/health-handler {}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-jwks
  "GET /.well-known/jwks.json — direct handler call (no auth)."
  [ctx]
  (let [{:keys [config]} ctx
        handler (jwks-handler/jwks-handler config)
        result  (response->result (handler {}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-get
  "Generic GET with no auth — returns updated ctx."
  [ctx path]
  (cond
    (= "/health" path)
    (call-health ctx)

    (= "/.well-known/jwks.json" path)
    (call-jwks ctx)

    :else
    (assoc ctx
           :last-response {:status 404 :body {:message "Not found"}}
           :last-body {:message "Not found"})))

;; ============================================================
;; Direct service calls — authenticated endpoints
;; ============================================================

(defn call-get-profile
  "GET /api/v1/users/me — direct handler call with auth."
  [ctx token]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (user-handler/get-profile-handler user-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request %))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-update-profile
  "PATCH /api/v1/users/me — direct handler call with auth."
  [ctx token body-str]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (user-handler/update-profile-handler user-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request % :json-body body-str))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-change-password
  "POST /api/v1/users/me/password — direct handler call with auth."
  [ctx token body-str]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (user-handler/change-password-handler user-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request % :json-body body-str))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-deactivate
  "POST /api/v1/users/me/deactivate — direct handler call with auth."
  [ctx token]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (user-handler/deactivate-handler user-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request %))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-logout
  "POST /api/v1/auth/logout — direct handler call with auth.
   Passes the token via the Authorization header in the request map."
  [ctx token]
  (let [{:keys [config token-repo]} ctx
        handler (auth-handler/logout-handler config token-repo)
        request {:headers      {"authorization" (str "Bearer " token)}
                 :json-params  {}
                 :query-params {}
                 :path-params  {}}
        result  (call-handler handler request)]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-logout-all
  "POST /api/v1/auth/logout-all — direct handler call with auth."
  [ctx token]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (auth-handler/logout-all-handler config token-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request %))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-token-claims
  "GET /api/v1/tokens/claims — direct handler call with auth."
  [ctx token]
  (let [{:keys [config]} ctx
        handler (token-handler/token-claims-handler config)
        request {:headers      {"authorization" (str "Bearer " token)}
                 :query-params {}
                 :path-params  {}}
        result  (call-handler handler request)]
    (assoc ctx
           :last-response result
           :last-body (:body result)
           :token-claims (:body result))))

;; ============================================================
;; Direct service calls — expenses
;; ============================================================

(defn call-create-expense
  "POST /api/v1/expenses — direct handler call with auth."
  [ctx token body-str]
  (let [{:keys [config user-repo expense-repo token-repo]} ctx
        handler  (expense-handler/create-expense-handler expense-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request % :json-body body-str))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-get-expense
  "GET /api/v1/expenses/:id — direct handler call with auth."
  [ctx token expense-id]
  (let [{:keys [config user-repo expense-repo token-repo]} ctx
        handler  (expense-handler/get-expense-handler expense-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request % :path-params {"id" expense-id}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-list-expenses
  "GET /api/v1/expenses — direct handler call with auth."
  [ctx token & {:keys [query-params] :or {query-params {}}}]
  (let [{:keys [config user-repo expense-repo token-repo]} ctx
        handler  (expense-handler/list-expenses-handler expense-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request % :query-params query-params))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-update-expense
  "PUT /api/v1/expenses/:id — direct handler call with auth."
  [ctx token expense-id body-str]
  (let [{:keys [config user-repo expense-repo token-repo]} ctx
        handler  (expense-handler/update-expense-handler expense-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request % :json-body body-str
                                                        :path-params {"id" expense-id}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-delete-expense
  "DELETE /api/v1/expenses/:id — direct handler call with auth."
  [ctx token expense-id]
  (let [{:keys [config user-repo expense-repo token-repo]} ctx
        handler  (expense-handler/delete-expense-handler expense-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request % :path-params {"id" expense-id}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-expense-summary
  "GET /api/v1/expenses/summary — direct handler call with auth."
  [ctx token]
  (let [{:keys [config user-repo expense-repo token-repo]} ctx
        handler  (expense-handler/summary-handler expense-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request %))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-pl-report
  "GET /api/v1/reports/pl?from=...&to=...&currency=... — direct handler call with auth."
  [ctx token query-params]
  (let [{:keys [config user-repo expense-repo token-repo]} ctx
        handler  (report-handler/pl-report-handler expense-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request % :query-params query-params))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

;; ============================================================
;; Direct service calls — attachments
;; ============================================================

(defn call-upload-attachment
  "POST /api/v1/expenses/:id/attachments — direct handler call with auth.
   Builds a multipart-params map mimicking Ring's byte-array-store format."
  [ctx token expense-id filename content-type file-bytes]
  (let [{:keys [config user-repo expense-repo attachment-repo token-repo]} ctx
        handler  (attachment-handler/upload-attachment-handler expense-repo attachment-repo)
        file-map {:filename     filename
                  :content-type content-type
                  :bytes        file-bytes}
        result   (try
                   (let [identity (resolve-identity config user-repo token-repo token)
                         request  (assoc (auth-request identity
                                                        :path-params {"id" expense-id}
                                                        :headers {"host" "localhost"})
                                         :multipart-params {"file" file-map})]
                     (call-handler handler request))
                   (catch clojure.lang.ExceptionInfo e
                     (let [data   (ex-data e)
                           status (or (:status data) 401)
                           msg    (or (:message data) (.getMessage e))]
                       {:status status :body {:message msg}})))]
    (assoc ctx
           :last-response result
           :last-body (:body result)
           :alice-attachment-id (:id (:body result)))))

(defn call-list-attachments
  "GET /api/v1/expenses/:id/attachments — direct handler call with auth."
  [ctx token expense-id]
  (let [{:keys [config user-repo expense-repo attachment-repo token-repo]} ctx
        handler  (attachment-handler/list-attachments-handler expense-repo attachment-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request %
                                                        :path-params {"id" expense-id}
                                                        :headers {"host" "localhost"}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-delete-attachment
  "DELETE /api/v1/expenses/:id/attachments/:attachment-id — direct handler call with auth."
  [ctx token expense-id attachment-id]
  (let [{:keys [config user-repo expense-repo attachment-repo token-repo]} ctx
        handler  (attachment-handler/delete-attachment-handler expense-repo attachment-repo)
        result   (call-protected-handler config user-repo token-repo token handler
                                         #(auth-request %
                                                        :path-params {"id"            expense-id
                                                                      "attachment-id" attachment-id}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

;; ============================================================
;; Direct service calls — admin
;; ============================================================

(defn call-admin-list-users
  "GET /api/v1/admin/users — direct admin handler call."
  [ctx token & {:keys [query-params] :or {query-params {}}}]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (admin-handler/list-users-handler user-repo)
        result   (call-admin-handler config user-repo token-repo token handler
                                     #(auth-request % :query-params query-params))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-admin-disable-user
  "POST /api/v1/admin/users/:id/disable — direct admin handler call."
  [ctx token user-id body-str]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (admin-handler/disable-user-handler user-repo token-repo)
        result   (call-admin-handler config user-repo token-repo token handler
                                     #(auth-request %
                                                    :path-params {"id" user-id}
                                                    :json-body body-str))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-admin-enable-user
  "POST /api/v1/admin/users/:id/enable — direct admin handler call."
  [ctx token user-id]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (admin-handler/enable-user-handler user-repo)
        result   (call-admin-handler config user-repo token-repo token handler
                                     #(auth-request %
                                                    :path-params {"id" user-id}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-admin-unlock-user
  "POST /api/v1/admin/users/:id/unlock — direct admin handler call."
  [ctx token user-id]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (admin-handler/unlock-user-handler user-repo)
        result   (call-admin-handler config user-repo token-repo token handler
                                     #(auth-request %
                                                    :path-params {"id" user-id}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

(defn call-admin-force-password-reset
  "POST /api/v1/admin/users/:id/force-password-reset — direct admin handler call."
  [ctx token user-id]
  (let [{:keys [config user-repo token-repo]} ctx
        handler  (admin-handler/force-password-reset-handler config user-repo)
        result   (call-admin-handler config user-repo token-repo token handler
                                     #(auth-request %
                                                    :path-params {"id" user-id}))]
    (assoc ctx
           :last-response result
           :last-body (:body result))))

;; ============================================================
;; Verify-token-is-invalid helper (used in assertion steps)
;; ============================================================

(defn token-valid?
  "Returns true if the token can still be used to authenticate."
  [ctx token]
  (let [{:keys [config user-repo token-repo]} ctx]
    (try
      (resolve-identity config user-repo token-repo token)
      true
      (catch Exception _
        false))))
