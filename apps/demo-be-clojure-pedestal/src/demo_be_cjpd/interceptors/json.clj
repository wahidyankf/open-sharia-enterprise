(ns demo-be-cjpd.interceptors.json
  "JSON request/response interceptors for Pedestal."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn- parse-body [request]
  (let [content-type (get-in request [:headers "content-type"] "")
        body         (:body request)]
    (if (and body (str/includes? content-type "application/json"))
      (try
        (with-open [rdr (java.io.InputStreamReader. body "UTF-8")]
          (json/parse-stream rdr true))
        (catch Exception _
          {}))
      {})))

(def json-body
  "Interceptor that parses JSON request body and adds json-params to request."
  {:name  ::json-body
   :enter (fn [ctx]
            (let [params (parse-body (:request ctx))]
              (assoc-in ctx [:request :json-params] params)))})

(defn- snake-case->kebab-case [s]
  (str/replace s #"_" "-"))

(defn- kebab->camel [s]
  (let [parts (str/split s #"-")]
    (str (first parts)
         (apply str (map str/capitalize (rest parts))))))

(defn- keywordize-keys-kebab [m]
  (into {}
        (map (fn [[k v]]
               [(keyword (snake-case->kebab-case (name k))) v]))
        m))

(def json-body-kebab
  "Interceptor that parses JSON body with kebab-case keyword keys."
  {:name  ::json-body-kebab
   :enter (fn [ctx]
            (let [params (parse-body (:request ctx))
                  kebab  (keywordize-keys-kebab params)]
              (assoc-in ctx [:request :json-params] kebab)))})

(def json-response
  "Interceptor that serializes response body to JSON using camelCase keys."
  {:name  ::json-response
   :leave (fn [ctx]
            (let [response (:response ctx)
                  body     (:body response)]
              (if (map? body)
                (-> ctx
                    (assoc-in [:response :body]
                               (json/generate-string body {:key-fn #(kebab->camel (name %))}))
                    (assoc-in [:response :headers "Content-Type"] "application/json"))
                ctx)))})
