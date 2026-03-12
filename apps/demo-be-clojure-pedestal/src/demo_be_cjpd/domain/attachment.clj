(ns demo-be-cjpd.domain.attachment
  "Attachment domain validation rules.")

(def allowed-content-types
  "Set of allowed MIME types for attachments."
  #{"image/jpeg" "image/png" "application/pdf"})

(def max-file-size-bytes
  "Maximum allowed file size in bytes (10 MB)."
  (* 10 1024 1024))

(defn valid-content-type?
  "Return true if the content type is allowed."
  [content-type]
  (contains? allowed-content-types content-type))

(defn valid-file-size?
  "Return true if the file size is within the allowed limit."
  [size-bytes]
  (<= size-bytes max-file-size-bytes))
