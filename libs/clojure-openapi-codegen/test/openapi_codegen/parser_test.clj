(ns openapi-codegen.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [openapi-codegen.parser :as sut]))

(def sample-yaml
  "openapi: 3.1.0
info:
  title: Test API
  version: 1.0.0
components:
  schemas:
    LoginRequest:
      type: object
      description: Login credentials
      required:
        - username
        - password
      properties:
        username:
          type: string
        password:
          type: string
    UserProfile:
      type: object
      description: User profile details
      required:
        - id
        - email
      properties:
        id:
          type: string
        email:
          type: string
          format: email
        age:
          type: integer
        score:
          type: number
        active:
          type: boolean
        tags:
          type: array
          items:
            type: string
    EmptySchema:
      type: object
      description: Schema with no properties
")

(defn- write-temp-yaml
  "Write yaml-content to a temp file and return its path string."
  [yaml-content]
  (let [f (java.io.File/createTempFile "test-spec" ".yaml")]
    (.deleteOnExit f)
    (spit f yaml-content)
    (.getAbsolutePath f)))

(deftest parse-schemas-returns-sequence-test
  (testing "returns a non-empty sequence for a spec with schemas"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)]
      (is (seq schemas))
      (is (= 3 (count schemas))))))

(deftest parse-schemas-schema-names-test
  (testing "each parsed schema has a :name key"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          names     (set (map :name schemas))]
      (is (contains? names "LoginRequest"))
      (is (contains? names "UserProfile"))
      (is (contains? names "EmptySchema")))))

(deftest parse-schemas-required-fields-test
  (testing "required properties are marked with :required? true"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          login     (first (filter #(= "LoginRequest" (:name %)) schemas))
          props     (:properties login)
          username  (first (filter #(= "username" (:name %)) props))
          password  (first (filter #(= "password" (:name %)) props))]
      (is (true? (:required? username)))
      (is (true? (:required? password))))))

(deftest parse-schemas-optional-fields-test
  (testing "non-required properties are marked with :required? false"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          profile   (first (filter #(= "UserProfile" (:name %)) schemas))
          props     (:properties profile)
          age       (first (filter #(= "age" (:name %)) props))]
      (is (false? (:required? age))))))

(deftest parse-schemas-property-types-test
  (testing "property types are extracted correctly"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          profile   (first (filter #(= "UserProfile" (:name %)) schemas))
          props     (:properties profile)
          find-prop (fn [n] (first (filter #(= n (:name %)) props)))]
      (is (= "string"  (:type (find-prop "id"))))
      (is (= "integer" (:type (find-prop "age"))))
      (is (= "number"  (:type (find-prop "score"))))
      (is (= "boolean" (:type (find-prop "active"))))
      (is (= "array"   (:type (find-prop "tags")))))))

(deftest parse-schemas-array-items-test
  (testing "array properties carry :items sub-schema"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          profile   (first (filter #(= "UserProfile" (:name %)) schemas))
          tags      (first (filter #(= "tags" (:name %)) (:properties profile)))]
      (is (some? (:items tags)))
      (is (= "string" (get (:items tags) "type"))))))

(deftest parse-schemas-description-test
  (testing "description is extracted from schema definition"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          login     (first (filter #(= "LoginRequest" (:name %)) schemas))]
      (is (= "Login credentials" (:description login))))))

(deftest parse-schemas-empty-properties-test
  (testing "schema with no properties yields empty :properties sequence"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          empty-s   (first (filter #(= "EmptySchema" (:name %)) schemas))]
      (is (empty? (:properties empty-s))))))

(deftest parse-schemas-required-set-test
  (testing ":required key is a set of required property name strings"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          login     (first (filter #(= "LoginRequest" (:name %)) schemas))]
      (is (set? (:required login)))
      (is (= #{"username" "password"} (:required login))))))

(deftest parse-schemas-no-schemas-test
  (testing "returns empty sequence when components.schemas is absent"
    (let [empty-yaml "openapi: 3.1.0\ninfo:\n  title: Empty\n  version: 1.0.0\n"
          spec-path  (write-temp-yaml empty-yaml)
          schemas    (sut/parse-schemas spec-path)]
      (is (empty? schemas)))))

(deftest parse-schemas-format-preserved-test
  (testing "property format (e.g. email, date-time) is extracted"
    (let [spec-path (write-temp-yaml sample-yaml)
          schemas   (sut/parse-schemas spec-path)
          profile   (first (filter #(= "UserProfile" (:name %)) schemas))
          email     (first (filter #(= "email" (:name %)) (:properties profile)))]
      (is (= "email" (:format email))))))
