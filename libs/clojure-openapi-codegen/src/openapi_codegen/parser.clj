(ns openapi-codegen.parser
  "Parse an OpenAPI 3.x bundled YAML spec and extract schema definitions."
  (:require [clojure.java.io :as io])
  (:import (org.yaml.snakeyaml Yaml)))

(defn- load-spec
  "Read and parse a YAML file at path. Returns parsed data structure (nested maps/lists/scalars)."
  [spec-path]
  (let [yaml (Yaml.)]
    (with-open [reader (io/reader spec-path)]
      (.load yaml reader))))

(defn- extract-required-set
  "Return the set of required property names for a schema definition, or #{} if none."
  [schema-def]
  (into #{} (get schema-def "required" [])))

(defn- extract-property
  "Given a schema-name, property-name, property-definition map, and required-set,
  return a map describing the parsed property.

  Keys:
    :name       - property name (string)
    :type       - OpenAPI type string (e.g. \"string\", \"integer\")
    :format     - optional format string (e.g. \"date-time\", \"email\")
    :required?  - boolean: true when the property is in the required list
    :items      - (for array type) the items sub-schema map (may be nil)
    :ref        - $ref value when property is a schema reference (may be nil)"
  [_schema-name prop-name prop-def required-set]
  (let [ref-val (get prop-def "$ref")]
    {:name      prop-name
     :type      (if ref-val "object" (get prop-def "type" "string"))
     :format    (get prop-def "format")
     :required? (contains? required-set prop-name)
     :items     (get prop-def "items")
     :ref       ref-val}))

(defn- extract-schema
  "Given schema-name and schema-definition map, return a parsed schema map.

  Keys:
    :name        - schema name (string)
    :type        - top-level type (defaults to \"object\")
    :description - optional description
    :properties  - sequence of parsed property maps (may be empty)
    :required    - set of required property name strings"
  [schema-name schema-def]
  (let [required-set (extract-required-set schema-def)
        properties   (get schema-def "properties" {})]
    {:name        schema-name
     :type        (get schema-def "type" "object")
     :description (get schema-def "description")
     :properties  (mapv (fn [[prop-name prop-def]]
                          (extract-property schema-name prop-name prop-def required-set))
                        properties)
     :required    required-set}))

(defn parse-schemas
  "Load the OpenAPI bundled YAML at spec-path and return a sequence of parsed schema maps.

  Each schema map has keys: :name, :type, :description, :properties, :required."
  [spec-path]
  (let [spec    (load-spec spec-path)
        schemas (get-in spec ["components" "schemas"] {})]
    (mapv (fn [[schema-name schema-def]]
            (extract-schema schema-name schema-def))
          schemas)))
