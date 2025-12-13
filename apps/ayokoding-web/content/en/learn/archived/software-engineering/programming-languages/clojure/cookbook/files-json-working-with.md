---
title: "Files: JSON - Working With"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/blob/main/contents/clojure-cookbook/clojure-cookbook-primary/src/files_json/core.clj).

</aside>

## Introduction

Working with JSON files in Clojure allows storing and exchanging data in a simple and readable format. JSON stands for "JavaScript Object Notation" and is a popular data format for representing structured data. It provides a way to represent complex data structures using arrays and objects. You can easily save and load data with JSON files, making it convenient for configuration files, serialization, and interchanging data between different systems.

To work with JSON files in Clojure, you can use the `clojure.data.json` library. This library provides functions like `slurp` to read JSON data from a file and `json/read-str` to parse JSON data from a string. Similarly, you can use `spit` and `json/write-str` to write JSON data to a file. These functions make reading and writing data in JSON format straightforward, allowing you to work efficiently with structured data in your Clojure programs.

## Working with JSON Files

### Code

`simple_example_read.edn`

```json
{
  "name": "John Doe",
  "age": 30,
  "email": "john.doe@example.com",
  "address": {
    "street": "123 Main St",
    "city": "New York",
    "state": "NY",
    "zip": "10001"
  },
  "interests": ["programming", "reading", "hiking"]
}
```

`files_json/core.clj`

```clojure
(ns files-json.core
  (:require [clojure.data.json :as json]))

;; Reading JSON Files
(def json-file (slurp "data_set/example_read.json"))

json-file
;; => "{\n  \"name\": \"John Doe\",\n  \"age\": 30,\n  \"email\": \"john.doe@example.com\",\n  \"address\": {\n    \"street\": \"123 Main St\",\n    \"city\": \"New York\",\n    \"state\": \"NY\",\n    \"zip\": \"10001\"\n  },\n  \"interests\": [\n    \"programming\",\n    \"reading\",\n    \"hiking\"\n  ]\n}\n"

(def non-kw-json-data (json/read-str json-file))
non-kw-json-data
;; => {"name" "John Doe",
;;     "age" 30,
;;     "email" "john.doe@example.com",
;;     "address" {"street" "123 Main St", "city" "New York", "state" "NY", "zip" "10001"},
;;     "interests" ["programming" "reading" "hiking"]}
(class non-kw-json-data)
;; => clojure.lang.PersistentArrayMap
(type non-kw-json-data)
;; => clojure.lang.PersistentArrayMap
(select-keys non-kw-json-data ["name" "age" "email" "interests"])
;; => {"name" "John Doe", "age" 30, "email" "john.doe@example.com", "interests" ["programming" "reading" "hiking"]}
(non-kw-json-data "address")
;; => {"street" "123 Main St", "city" "New York", "state" "NY", "zip" "10001"}
(get-in non-kw-json-data ["address" "street"])
;; => "123 Main St"

(def kw-json-data (json/read-str json-file :key-fn keyword))
kw-json-data
;; => {:name "John Doe",
;;     :age 30,
;;     :email "john.doe@example.com",
;;     :address {:street "123 Main St", :city "New York", :state "NY", :zip "10001"},
;;     :interests ["programming" "reading" "hiking"]}
(class kw-json-data)
;; => clojure.lang.PersistentArrayMap
(type kw-json-data)
;; => clojure.lang.PersistentArrayMap
(select-keys kw-json-data [:name :age :email :interests])
;; => {:name "John Doe", :age 30, :email "john.doe@example.com", :interests ["programming" "reading" "hiking"]}
(get-in kw-json-data [:address :street])
;; => "123 Main St"

;; Writing JSON Files

(spit "data_set/example_write_non_kw.json" (json/write-str non-kw-json-data))
(slurp "data_set/example_write_non_kw.json")
;; => "{\"name\":\"John Doe\",\"age\":30,\"email\":\"john.doe@example.com\",\"address\":{\"street\":\"123 Main St\",\"city\":\"New York\",\"state\":\"NY\",\"zip\":\"10001\"},\"interests\":[\"programming\",\"reading\",\"hiking\"]}"

;; the written file's parsed json data is the same as the original file's parsed json data
(= (json/read-str (slurp "data_set/example_write_non_kw.json")) non-kw-json-data)
;; => true

(spit "data_set/example_write_kw.json" (json/write-str kw-json-data))
(slurp "data_set/example_write_kw.json")
;; => "{\"name\":\"John Doe\",\"age\":30,\"email\":\"john.doe@example.com\",\"address\":{\"street\":\"123 Main St\",\"city\":\"New York\",\"state\":\"NY\",\"zip\":\"10001\"},\"interests\":[\"programming\",\"reading\",\"hiking\"]}"

;; the written file's parsed json data is the same as the original file's parsed json data
(= (json/read-str (slurp "data_set/example_write_kw.json") :key-fn keyword) kw-json-data)
;; => true

;; the written files are the same for non-keyword and keyword json data
(= (slurp "data_set/example_write_non_kw.json")
   (slurp "data_set/example_write_kw.json"))
;; => true

;; pretty printing when writing JSON files
(spit "data_set/example_write_pretty.json" (with-out-str (clojure.data.json/pprint kw-json-data)))
```

### Explanation

This Clojure code is about reading and writing JSON files. Here's a more detailed breakdown of what each part does:

1. **Namespace and Library Requirement**: The code defines a namespace `files-json.core`. This is a common practice in Clojure to organize code into logical units. The `:require` keyword is used to import the `clojure.data.json` library, which provides functions for reading and writing JSON data.
2. **Reading JSON Files**: The `slurp` function is used to read the content of a JSON file into a string. This function reads the entire file into memory, which is fine for small files but could be problematic for large ones.
3. **Parsing JSON Data**: The `json/read-str` function parse the JSON string into a Clojure data structure. This function takes a string of JSON data and returns a Clojure data structure that represents the same data. This is done twice, once without any options (resulting in a map with string keys), and once with the `:key-fn keyword` option (resulting in a map with keyword keys). The `:key-fn keyword` option tells the function to convert all JSON keys into Clojure keywords, which can be more convenient to work with in Clojure.
4. **Accessing Parsed Data**: The parsed data is accessed using various functions. The `select-keys` function is used to get a subset of the data by specifying a list of keys. The `get-in` function is used to access nested data by specifying a path of keys. Direct key access is also used, which returns the value associated with a given key in the map.
5. **Writing JSON Files**: The `spit` function returns the JSON string to a file. This function takes a filename and a data string and writes the data to the file. The `json/write-str` function converts the Clojure data structure into a JSON string. This function takes a Clojure data structure and returns a JSON string representing the same data.
6. **Verifying Written Data**: The written JSON data is read back in and compared to the original data to verify that the write operation was successful. This is done by reading the written file with `slurp`, parsing it with `json/read-str`, and then comparing the resulting data structure to the original data structure with the `=` function.
7. **Pretty Printing JSON Data**: The `clojure.data.json/pprint` function is used to pretty-print the JSON data, making it easier to read. This function takes a Clojure data structure and returns a string of JSON data formatted with indentation and line breaks to make it easier to read. This pretty-printed data is then written to a file with the `spit` function.
