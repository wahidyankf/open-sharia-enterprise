---
title: "Files: EDN - Working With"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/files_edn).

</aside>

## Introduction

Working with .edn files in Clojure allows storing and exchanging data in a simple and readable format. .edn stands for "extensible data notation" and is similar to JSON or XML but designed explicitly for Clojure. It provides a way to represent complex data structures using lists, vectors, maps, and sets. With .edn files, you can easily save and load data, making it convenient for configuration files, serialization, and interchanging data between different systems.

To work with .edn files in Clojure, you can use the language's built-in functions. Clojure provides functions like `slurp` to read .edn data from a file and `edn/read-string` to read .edn data from a string. Similarly, you can use `spit` to write .edn data to a file. These functions make reading and writing data in .edn format straightforward, allowing you to work efficiently with structured data in your Clojure programs.

## Simple EDN File

### Code

`simple_example_read.edn`

```clojure
;; Example EDN file

^{:author "John" :created-at "2022-01-01"}
{:name "John Doe"
 :age 30
 :email "john.doe@example.com"
 :address {:street "123 Main St"
           :city "New York"
           :state "NY"
           :zip "10001"}
 :interests ["programming" "reading" "hiking"]}
```

`files_edn/core.clj`

```clojure
(ns files-edn.core
  (:require
   [clojure.edn :as edn]))

;; Reading Simple EDN Files

(def edn_simple_sample_data (edn/read-string (slurp "data_set/simple_example_read.edn")))

edn_simple_sample_data
;; => {:name "John Doe", :age 30, :email "john.doe@example.com", :address {:street "123 Main St", :city "New York", :state "NY", :zip "10001"}, :interests ["programming" "reading" "hiking"]}

(class edn_simple_sample_data)
;; => clojure.lang.PersistentArrayMap

(:name edn_simple_sample_data)
;; => "John Doe"
(:age edn_simple_sample_data)
;; => 30
(:email edn_simple_sample_data)
; => "john.doe@example.com"
(:address edn_simple_sample_data)
;; => {:street "123 Main St", :city "New York", :state "NY", :zip "10001"}
(:street (:address edn_simple_sample_data))
;; => "123 Main St"
(:interests edn_simple_sample_data)
;; => ["programming" "reading" "hiking"]

(def edn_simple_sample_meta_data (meta edn_simple_sample_data))

edn_simple_sample_meta_data
;; => {:author "John", :created-at "2022-01-01"}

(:author edn_simple_sample_meta_data)
;; => "John"
(:created-at edn_simple_sample_meta_data)
;; => "2022-01-01"

;; Writing Simple EDN Files
(spit "data_set/simple_example_write.edn" edn_simple_sample_data)
```

### Explanation

The provided Clojure code reads data from an EDN (Extensible Data Notation) file, processes the data, and then writes the data back to another EDN file. Here's the explanation in itemized format:

1. Namespace Declaration: `(ns files-edn.core (:require [clojure.edn :as edn]))`
   - This code declares a namespace named `files-edn.core` and imports the `clojure.edn` library, aliasing it as `edn` for later use.
2. Reading EDN Files:
   - `(def edn_simple_sample_data (edn/read-string (slurp "data_set/simple_example_read.edn")))`
     - Here, an EDN (Extensible Data Notation, a subset of Clojure used for data transfer) file is being read from the file system. The `slurp` function is used to read the contents of the file as a string. `edn/read-string` parses this string into a Clojure data structure. The parsed data is assigned to the `edn_simple_sample_data` var.
3. Inspecting the Data:
   - `edn_simple_sample_data`
     - This line is simply outputting the contents of `edn_simple_sample_data`, which is an example of a map with nested map and vector.
   - `(class edn_simple_sample_data)`
     - This line returns the class of `edn_simple_sample_data` which is `clojure.lang.PersistentArrayMap`, indicating that this is an instance of an immutable Clojure map.
   - `(:name edn_simple_sample_data)`, `(:age edn_simple_sample_data)`, etc.
     - These are examples of using keywords as functions to get the value of a map for a given key. For instance, `(:name edn_simple_sample_data)` returns "John Doe".
4. Meta Data Inspection:
   - `(def edn_simple_sample_meta_data (meta edn_simple_sample_data))`
     - This line retrieves metadata associated with the `edn_simple_sample_data` var and assigns it to `edn_simple_sample_meta_data`.
   - `edn_simple_sample_meta_data`
     - This line outputs the content of the metadata.
   - `(:author edn_simple_sample_meta_data)`, `(:created-at edn_simple_sample_meta_data)`
     - These lines retrieve the value associated with the keywords in the metadata map.
5. Writing EDN Files:
   - `(spit "data_set/simple_example_write.edn" edn_simple_sample_data)`
     - This writes the `edn_simple_sample_data` back to an EDN file using the `spit` function, which writes data to a file. The `edn_simple_sample_data` is automatically converted back to a string before writing.

## EDN with Custom Reader

### Code

`custom_example_read.edn`

```clojure
;; Example EDN file

^{:author "John" :created-at "2022-01-01"}
{:name "John Doe"
 :age 30
 :email "john.doe@example.com"
 :address {:street "123 Main St"
           :city "New York"
           :state "NY"
           :zip "10001"}
 :interests ["programming" "reading" "hiking"]

 :wishes
 [#myapp/wishes "Learn 100 languages"
  #myapp/wishes "Teleport to office"]}
```

`files_edn/core.clj`

```clojure
(ns files-edn.core
  (:require
   [clojure.edn :as edn]))

;; Read Custom EDN Files

(defn wishes-reader [data]
  (println "Parsing myapp/wishes tag with data:" data)
  (str data " >>|<< " data))

(def edn_custom_sample_data (edn/read-string {:readers {'myapp/wishes wishes-reader}} (slurp "data_set/custom_example_read.edn")))

edn_custom_sample_data
;; => {:name "John Doe", :age 30, :email "john.doe@example.com", :address {:street "123 Main St", :city "New York", :state "NY", :zip "10001"}, :interests ["programming" "reading" "hiking"], :wishes ["Learn 100 languages >>|<< Learn 100 languages" "Teleport to office >>|<< Teleport to office"]}

(:wishes edn_custom_sample_data)
;; => ["Learn 100 languages >>|<< Learn 100 languages" "Teleport to office >>|<< Teleport to office"]

(:age edn_custom_sample_data)
;; => 30

(:author (meta edn_custom_sample_data))
;; => "John"
```

### Explanation

This Clojure code snippet is focused on reading and manipulating EDN (Extensible Data Notation) files using custom tags. Here's what's happening:

1. **Namespace Definition**: `(ns files-edn.core (:require [clojure.edn :as edn]))` is defining a namespace for this Clojure code and importing the Clojure EDN module.
2. **Custom Tag Reader Definition**: `defn` is being used to define a function `wishes-reader` that takes a data argument. This function logs a message and then creates a string that consists of the input data, the string `>>|<<`, and the input data again.
3. **Reading an EDN file with a Custom Tag**: `(edn/read-string {:readers {'myapp/wishes wishes-reader}} (slurp "data_set/custom_example_read.edn"))` reads the content of an EDN file and parses it using the custom reader for the tag 'myapp/wishes'.
4. **Custom EDN File Parsing**: The `edn/read-string` function parses the EDN string with a map as a first argument. The map `{:readers {'myapp/wishes wishes-reader}}` indicates that whenever the parser encounters 'myapp/wishes' as a tag, it should use the `wishes-reader` function to interpret the data.
5. **Storing the Parsed EDN Data**: `def` is used to bind the result of the `edn/read-string` function to the symbol `edn_custom_sample_data`.
6. **Accessing Data from the EDN**: Keywords like `(:wishes edn_custom_sample_data)`, `(:age edn_custom_sample_data)`, and `(:author (meta edn_custom_sample_data))` are used to access specific data within the parsed EDN data structure.
7. **Keyword Access**: The first two expressions are straightforward: they access the 'wishes' and 'age' fields of the map.
8. Lastly, `(:author (meta edn_custom_sample_data))` is used to fetch metadata attached to the `edn_custom_sample_data` map. The `meta` function is used to fetch the metadata of a collection. In this case, it's fetching the author of the data, which is "John".
