(ns demo-be-cjpd.interceptors.multipart
  "Multipart form-data parsing interceptor for file uploads.
   Uses ring.middleware.multipart-params with byte-array storage so that
   the file bytes are available in memory without requiring the Servlet
   multipart API (which is unavailable in some Pedestal/Jetty setups)."
  (:require [clojure.string :as str]
            [ring.middleware.multipart-params :as mp]
            [ring.middleware.multipart-params.byte-array :as ba]))

(def ^:private byte-array-store (ba/byte-array-store))

(def multipart-params
  "Interceptor that parses multipart/form-data and attaches :multipart-params to request.
   Each file entry has :filename, :content-type, and :bytes keys."
  {:name  ::multipart-params
   :enter (fn [ctx]
            (let [request      (:request ctx)
                  content-type (get-in request [:headers "content-type"] "")]
              (if (str/includes? content-type "multipart/form-data")
                (let [updated-req (mp/multipart-params-request
                                    request
                                    {:store byte-array-store})]
                  (assoc-in ctx [:request :multipart-params]
                            (:multipart-params updated-req)))
                ctx)))})
