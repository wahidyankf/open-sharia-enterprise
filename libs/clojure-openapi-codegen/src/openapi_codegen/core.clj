(ns openapi-codegen.core
  "Entry point: read an OpenAPI bundled YAML spec and generate Malli schema files."
  (:require [openapi-codegen.parser :as parser]
            [openapi-codegen.generator :as generator]))

(defn generate
  "Read the OpenAPI YAML at spec-path and write Malli schema .clj files to output-dir.

  Returns a sequence of written file paths.

  Example:
    (generate \"specs/.../openapi-bundled.yaml\" \"generated/schemas\")"
  [spec-path output-dir]
  (let [schemas (parser/parse-schemas spec-path)]
    (generator/generate-schema-files schemas output-dir)))
