---
title: "Files: General"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

## Introduction

Clojure, a functional JVM language, provides seamless interoperability with Java. This makes working with files in Clojure efficient and straightforward, as it can leverage the underlying Java libraries. The standard way to handle file operations in Clojure is through the `clojure.java.io` namespace, which provides several functions to manipulate files and streams. This namespace's utility functions help in performing operations like reading from files (`slurp`), writing to files (`spit`), opening files for reading and writing (`reader`, `writer`), and dealing with streams, all while managing resources responsibly.

Beyond the basics, the `clojure.java.io` namespace also allows for manipulating file metadata and permissions, including checking if a file exists, whether it can be read, written, or executed, and even modifying these attributes. Furthermore, Clojure's ability to call shell commands using the `clojure.java.shell` namespace extends its file manipulation capabilities, making it possible to execute shell commands for operations such as changing file permissions or listing file details. These interop capabilities make Clojure a powerful tool for working with files, whether for simple tasks like reading and writing data or more complex operations such as permissions management and shell interactions.

## Working with Text File

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/files_general).

</aside>

### Code

`data_set/file_example_input.txt`

```clojure
Hi there!
Hello world!
Hola!
Halo!
```

`files_general/core.clj`

```clojure
(ns files-general.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]))

;; Whole file

(def input-file "data_set/file_example_input.txt")
(def output-file "data_set/file_example_output.txt")

(slurp input-file)
;; => "Hi there!\nHello world!\nHola!\nHalo!"

(spit output-file "Hello, world!")
;; => nil
(slurp output-file)
;; => "Hello, world!"

(spit output-file "\nHi there!" :append true)
;; => nil
(slurp output-file)
;; => "Hello, world!\nHi there!"

;; Line by line

(with-open [reader (io/reader input-file)]
  (doall (mapv str (line-seq reader))))
;; => ["Hi there!" "Hello world!" "Hola!" "Halo!"]

(with-open [writer (io/writer output-file)]
  (doseq [line ["Hi there!" "Hello world!" "Hola!" "Halo!"]]
    (.write writer (str line "\n"))))
;; => nil
(slurp output-file)
;; => "Hi there!\nHello world!\nHola!\nHalo!\n"

;; File Properties

(.exists (io/file input-file))
;; => true

(.setReadable (io/file input-file) false)
;; => true
(.canRead (io/file input-file))
;; => false
(.setReadable (io/file input-file) true)
;; => true
(.canRead (io/file input-file))
;; => true

(.setWritable (io/file input-file) false)
;; => true
(.canWrite (io/file input-file))
;; => false
(.setWritable (io/file input-file) true)
;; => true
(.canWrite (io/file input-file))
;; => true

(sh "chmod" "+x" input-file)
;; => {:exit 0, :out "", :err ""}
(.canExecute (io/file input-file))
;; => true
(sh "chmod" "-x" input-file)
;; => {:exit 0, :out "", :err ""}
(.canExecute (io/file input-file))
;; => false

(sh "chmod" "0644" input-file)
;; => {:exit 0, :out "", :err ""}
(sh "ls" "-l" input-file)
;; => {:exit 0, :out "-rw-r--r--@ 1 abcd  programmer  34 Jun 27 06:53 data_set/file_example_input.txt\n", :err ""}
```

### Explanation

Let’s break this code:

1. **Namespace declaration**: The `(ns files-general.core ...)` statement defines the namespace of the current file, `files-general.core`.

   Clojure uses namespaces to group related functionality and avoid name clashes.

2. **Library requirements**: The `(:require [clojure.java.shell :refer [sh]] [clojure.java.io :as io])` inside the namespace declaration tells Clojure to include the `clojure.java.shell` and `clojure.java.io` libraries.
   - `clojure.java.shell` is included with `sh` being referred to directly, allowing it to be used without a namespace prefix.
   - `clojure.java.io` is included as `io`, allowing functions in this library to be called with the `io/` prefix.
3. **Defining variables**: `(def input-file "data_set/file_example_input.txt")` and `(def output-file "data_set/file_example_output.txt")` define two strings representing file paths and store them in the variables `input-file` and `output-file` respectively. `def` is used to define a global var.
4. **Reading files**: `(slurp input-file)` reads the file's entire contents at `input-file` into memory as a single string. The returned string is displayed in the comments (`;; => ...`) for clarity.
5. **Writing files**: `(spit output-file "Hello, world!")` writes the string `"Hello, world!"` to the file at `output-file`. If the file already exists, it is overwritten.
6. **Appending to files**: `(spit output-file "\\nHi there!" :append true)` appends the string `"\\nHi there!"` to the end of the file at `output-file`. `:append true` is an optional argument that changes the behavior of `spit` from overwriting to appending.
7. **Reading files line by line**: The `(with-open [reader (io/reader input-file)] ...)` block opens a file at `input-file` for reading, and ensures that the file is closed when done. Inside this block, `(doall (mapv str (line-seq reader)))` reads the file line by line, converting each line into a string, and returns a vector of these strings.
8. **Writing files line by line**: The `(with-open [writer (io/writer output-file)] ...)` block opens a file at `output-file` for writing, and ensures that the file is closed when done. Inside this block, `(doseq [line ["Hi there!" "Hello world!" "Hola!" "Halo!"]] (.write writer (str line "\\n")))` writes each line in the provided vector to the file, followed by a newline.
9. **File properties and operations**: The `io/file` function is used to create a File object for the file at `input-file`. File properties such as existence, readability, writability, and executability can be checked with `.exists`, `.canRead`, `.canWrite`, and `.canExecute` respectively. These properties can also be set with `.setReadable`, `.setWritable`, and by running shell commands with the `sh` function.
10. **Shell operations**: The `sh` function is used to run shell commands. For example, `(sh "chmod" "+x" input-file)` runs the shell command `chmod +x` on `input-file`, making it executable. The return value is a map containing the shell command's exit status, stdout, and stderr.
11. **Interacting with the file system**: The `sh` function is also used to change file permissions and to list file properties in the shell with `chmod` and `ls -l` commands, respectively. For example, `(sh "chmod" "0644" input-file)` sets the file permissions to `-rw-r--r--`. `(sh "ls" "-l" input-file)` lists the detailed properties of `input-file`.
