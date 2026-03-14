(ns demo-be-cjpd.handlers.attachment
  "Attachment upload, list, and delete handlers."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [demo-be-cjpd.db.attachment-repo :as attachment-repo]
            [demo-be-cjpd.db.expense-repo :as expense-repo]
            [demo-be-cjpd.domain.attachment :as attachment-domain]))

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

(defn- attachment->response [attachment base-url]
  {:id           (:id attachment)
   :expense-id   (:expense-id attachment)
   :filename     (:filename attachment)
   :content-type (:content-type attachment)
   :size         (:size attachment)
   :url          (str base-url "/api/v1/expenses/"
                      (:expense-id attachment)
                      "/attachments/"
                      (:id attachment)
                      "/data")
   :created-at   (:created-at attachment)})

(defn upload-attachment-handler
  "POST /api/v1/expenses/:id/attachments — Upload an attachment."
  [ds]
  (fn [request]
    (let [user-id    (:user-id (:identity request))
          expense-id (get-in request [:path-params :id])
          expense    (expense-repo/find-by-id ds expense-id)]
      (cond
        (nil? expense)
        (error-response 404 "Expense not found")

        (not= user-id (:user-id expense))
        (error-response 403 "Forbidden")

        :else
        (let [multipart  (:multipart-params request)
              ;; Ring's byte-array-store uses string keys; fall back to first value
              file-part  (or (get multipart "file") (first (vals multipart)))]
          (if (or (nil? file-part) (not (map? file-part)))
            (error-response 400 "No file uploaded")
            (let [content-type (or (:content-type file-part) "application/octet-stream")
                  ;; Ring byte-array-store uses :bytes; compute size from the array
                  file-bytes   (or (:bytes file-part) (byte-array 0))
                  size         (alength ^bytes file-bytes)
                  filename     (or (:filename file-part) "upload")]
              (cond
                (not (attachment-domain/valid-content-type? content-type))
                {:status  415
                 :headers {"Content-Type" "application/json"}
                 :body    (json/generate-string {:message "Unsupported file type" :field "file"})}

                (not (attachment-domain/valid-file-size? size))
                {:status  413
                 :headers {"Content-Type" "application/json"}
                 :body    (json/generate-string {:message "File size exceeds the 10MB limit"})}

                :else
                (let [attachment (attachment-repo/create-attachment!
                                   ds
                                   {:expense-id   expense-id
                                    :user-id      user-id
                                    :filename     filename
                                    :content-type content-type
                                    :size         size
                                    :data         file-bytes})
                      scheme     (or (get-in request [:headers "x-forwarded-proto"]) "http")
                      host       (or (get-in request [:headers "host"]) "localhost")]
                  (json-response 201
                                 (attachment->response attachment
                                                       (str scheme "://" host))))))))))))

(defn list-attachments-handler
  "GET /api/v1/expenses/:id/attachments — List attachments for an expense."
  [ds]
  (fn [request]
    (let [user-id    (:user-id (:identity request))
          expense-id (get-in request [:path-params :id])
          expense    (expense-repo/find-by-id ds expense-id)]
      (cond
        (nil? expense)
        (error-response 404 "Expense not found")

        (not= user-id (:user-id expense))
        (error-response 403 "Forbidden")

        :else
        (let [attachments (attachment-repo/list-by-expense ds expense-id)
              scheme      (or (get-in request [:headers "x-forwarded-proto"]) "http")
              host        (or (get-in request [:headers "host"]) "localhost")
              base-url    (str scheme "://" host)]
          (json-response 200
                         {:attachments (mapv #(attachment->response % base-url) attachments)}))))))

(defn delete-attachment-handler
  "DELETE /api/v1/expenses/:id/attachments/:attachment-id — Delete an attachment."
  [ds]
  (fn [request]
    (let [user-id       (:user-id (:identity request))
          expense-id    (get-in request [:path-params :id])
          attachment-id (get-in request [:path-params :attachment-id])
          expense       (expense-repo/find-by-id ds expense-id)]
      (cond
        (nil? expense)
        (error-response 404 "Expense not found")

        (not= user-id (:user-id expense))
        (error-response 403 "Forbidden")

        :else
        (let [attachment (attachment-repo/find-by-id ds attachment-id)]
          (cond
            (nil? attachment)
            (error-response 404 "Attachment not found")

            (not= expense-id (:expense-id attachment))
            (error-response 404 "Attachment not found")

            :else
            (do
              (attachment-repo/delete-attachment! ds attachment-id)
              {:status  204
               :headers {"Content-Type" "application/json"}
               :body    ""})))))))
