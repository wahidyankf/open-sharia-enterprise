(ns demo-be-cjpd.interceptors.admin
  "Admin role enforcement interceptor.")

(def require-admin
  "Interceptor that rejects non-ADMIN users with 403."
  {:name  ::require-admin
   :enter (fn [ctx]
            (let [identity (get-in ctx [:request :identity])
                  role     (:role identity)]
              (if (= "ADMIN" role)
                ctx
                (assoc ctx :response
                       {:status  403
                        :headers {"Content-Type" "application/json"}
                        :body    "{\"error\":\"Forbidden: admin access required\"}"}))))})
