---
title: "Files: YAML - Working With"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/blob/main/contents/clojure-cookbook/clojure-cookbook-primary/src/files_yaml/core.clj).

</aside>

## Introduction

Working with YAML files in Clojure allows storing and exchanging data in a human-readable and easily editable format. YAML stands for "YAML Ain't Markup Language" and is a popular data format for representing structured data. It provides a way to represent complex data structures using maps, lists, and scalars. You can easily save and load data with YAML files, making it convenient for configuration files, serialization, and interchanging data between different systems.

To work with YAML files in Clojure, you can use the https://github.com/clj-commons/clj-yaml library. This library provides functions like `slurp` to read YAML data from a file and `yaml/parse-string` to parse YAML data from a string. Similarly, you can use `spit` and `yaml/generate-string` to write YAML data to a file. These functions make reading and writing data in YAML format straightforward, allowing you to work efficiently with structured data in your Clojure programs.

## Working with YAML Files

### Code

`example_read.yaml`

```yaml
name: John Doe
age: 30
email: john.doe@example.com
address:
  street: 123 Main St
  city: New York
  state: NY
  zip: "10001"
interests:
  - programming
  - reading
  - hiking
```

`files_yaml/core.clj`

```clojure
(ns files-yaml.core
  (:require [clj-yaml.core :as yaml]))

;; Reading YAML Files
(def yaml-file (slurp "data_set/example_read.yaml"))

yaml-file
;; => "name: John Doe\nage: 30\nemail: john.doe@example.com\naddress:\n  street: 123 Main St\n  city: New York\n  state: NY\n  zip: \"10001\"\ninterests:\n  - programming\n  - reading\n  - hiking\n"

(def kw-yaml-data (yaml/parse-string yaml-file))
kw-yaml-data
;; => {:name "John Doe",
;;     :age 30,
;;     :email "john.doe@example.com",
;;     :address {:street "123 Main St", :city "New York", :state "NY", :zip "10001"},
;;     :interests ("programming" "reading" "hiking")}

(class kw-yaml-data)
;; => flatland.ordered.map.OrderedMap
(type kw-yaml-data)
;; => flatland.ordered.map.OrderedMap

(select-keys kw-yaml-data [:name :age :email :interests])
;; => {:name "John Doe", :age 30, :email "john.doe@example.com", :interests ("programming" "reading" "hiking")}
(get-in kw-yaml-data [:address :street])
;; => "123 Main St"

(def non-kw-yaml-data (yaml/parse-string yaml-file :keywords false))
non-kw-yaml-data
;; => {"name" "John Doe",
;;     "age" 30,
;;     "email" "john.doe@example.com",
;;     "address" {"street" "123 Main St", "city" "New York", "state" "NY", "zip" "10001"},
;;     "interests" ("programming" "reading" "hiking")}

(class non-kw-yaml-data)
;; => clojure.lang.PersistentArrayMap
(type non-kw-yaml-data)
;; => clojure.lang.PersistentArrayMap

(select-keys non-kw-yaml-data ["name" "age" "email" "interests"])
;; => {"name" "John Doe", "age" 30, "email" "john.doe@example.com", "interests" ("programming" "reading" "hiking")}
(get-in non-kw-yaml-data ["address" "street"])
;; => "123 Main St"

;; Writing YAML Files
(spit "data_set/example_write_kw.yaml" (yaml/generate-string kw-yaml-data))
(slurp "data_set/example_write_kw.yaml")
;; => "name: John Doe\nage: 30\nemail: john.doe@example.com\naddress: {street: 123 Main St, city: New York, state: NY, zip: '10001'}\ninterests: [programming, reading, hiking]\n"
;; Note: the list of interests is rendered in different style than the original file, but the data is the same

;; the written file's parsed yaml data is the same as the original kw file's parsed yaml data
(= (yaml/parse-string (slurp "data_set/example_write_kw.yaml")) kw-yaml-data)
;; => true

(spit "data_set/example_write_non_kw.yaml" (yaml/generate-string non-kw-yaml-data))
(slurp "data_set/example_write_non_kw.yaml")
;; => "name: John Doe\nage: 30\nemail: john.doe@example.com\naddress: {street: 123 Main St, city: New York, state: NY, zip: '10001'}\ninterests: [programming, reading, hiking]\n"
;; Note: the list of interests is rendered in different style than the original file, but the data is the same

;; the written file's parsed yaml data is the same as the original non-kw file's parsed yaml data
(= (yaml/parse-string (slurp "data_set/example_write_kw.yaml") :keywords false) non-kw-yaml-data)
;; => true

;; the written files are the same for non-keyword and keyword yaml data
(= (slurp "data_set/example_write_non_kw.yaml")
   (slurp "data_set/example_write_kw.yaml"))
;; => true

;; pretty printing when writing yaml files
(spit "data_set/example_write_pretty.yaml" (yaml/generate-string kw-yaml-data :dumper-options {:flow-style :block
                                                                                               :indicator-indent 2
                                                                                               :indent 6}))
```

`example_write_kw.yaml`

```yaml
name: John Doe
age: 30
email: john.doe@example.com
address: { street: 123 Main St, city: New York, state: NY, zip: "10001" }
interests: [programming, reading, hiking]
```

`example_write_non_kw.yaml`

```yaml
name: John Doe
age: 30
email: john.doe@example.com
address: { street: 123 Main St, city: New York, state: NY, zip: "10001" }
interests: [programming, reading, hiking]
```

`example_write_pretty.yaml`

```yaml
name: John Doe
age: 30
email: john.doe@example.com
address:
  street: 123 Main St
  city: New York
  state: NY
  zip: "10001"
interests:
  - programming
  - reading
  - hiking
```

### Explanation

This Clojure code is about reading and writing YAML files. Here's a more detailed breakdown of what each part does:

1. **Namespace and Library Requirement**: The file defines the namespace `files-yaml.core`. This is a common practice in Clojure to organize code into logical groups. It also requires the `clj-yaml.core` library as `yaml`. This library, [clj-yaml](https://github.com/clj-commons/clj-yaml), is a Clojure library that provides functions for parsing and generating YAML, a human-readable data serialization format.
2. **Reading YAML Files**: The `slurp` function is used to read the contents of the `example_read.yaml` file into the `yaml-file` variable. `slurp` is a built-in Clojure function that reads a file into a string.
3. **Parsing YAML Data**: The `parse-string` function from the `clj-yaml` library is used to parse the YAML data from the `yaml-file` variable into a Clojure data structure. This is done twice, once with keywords (`kw-yaml-data`) and once without (`non-kw-yaml-data`). Keywords in Clojure are symbols used as identifiers, similar to strings, but with some performance benefits.
4. **Inspecting the Data**: The `class` and `type` functions are used to inspect the type of the parsed data. These functions help debug and understand the structure of the data. The `select-keys` function selects specific keys from the data, and the `get-in` function is used to access nested data. These functions are part of Clojure's rich set of data manipulation functions.
5. **Writing YAML Files**: The `spit` function writes the parsed data into a new YAML file. `spit` is a built-in Clojure function that writes a string to a file. This is done twice, once with the keyword data (`example_write_kw.yaml`) and once with the non-keyword data (`example_write_non_kw.yaml`).
6. **Verifying the Written Data**: The written files are read and parsed to verify that the written data matches the original data. This is done using the `slurp`, `parse-string` functions, and the equality (`=`) operator. This step is important to ensure the write operation succeeded and didn't alter the data.
7. **Pretty Printing**: The `generate-string` function from the `clj-yaml` library is used with the `:dumper-options` option to print the YAML data pretty when writing it to a file (`example_write_pretty.yaml`). The options specify the style of the output and the indentation levels. Pretty printing makes the output more human-readable, especially for complex data structures.
