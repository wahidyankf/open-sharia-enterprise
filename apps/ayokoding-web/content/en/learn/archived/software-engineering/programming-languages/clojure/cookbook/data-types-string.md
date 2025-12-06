---
title: "Data Types: String"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the source code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/data_types_string).

</aside>

## Overview

Clojure strings are sequences of characters enclosed in double quotes. They are immutable, meaning that once a string is created, it cannot be modified. Under the hood, Clojure strings are implemented as Java strings, themselves implemented as arrays of characters. This means many Java string functions are also available in Clojure.

Because Clojure strings are immutable, they are safe to use in concurrent programs without locks or other synchronization mechanisms. This can make them a good choice for working with text data in multi-threaded applications.

Overall, Clojure strings provide a powerful and flexible set of tools for working with text data. In contrast, their underlying implementation as Java strings ensures they are efficient and well-suited to various use cases.

## Basic String Functions

### Code

```clojure
(ns data-types-string.core)

;; ---
;; Basic String Functions
;; ---

"abcdef123456"
;; => "abcdef123456"

(class "abcdef123456")
;; => java.lang.String

(string? "abc")
;; => true

(str "abc" "def")
;; => "abcdef"
(str "abc" "def" 123 "456")
;; => "abcdef123456"

(subs "abcdef123456" 0 3)
;; => "abc"
(subs "abcdef123456" 3 7)
;; => "def1"
(subs "abcdef123456" 0)
;; => "abcdef123456"
(try (subs "abcdef123456" 13)
     (catch Exception _e "StringIndexOutOfBoundsException"))
;; => "StringIndexOutOfBoundsException"
(try #_{:clj-kondo/ignore [:type-mismatch]}
 (subs "abcdef123456" -1 3)
     (catch Exception _e "StringIndexOutOfBoundsException"))

(.contains "abcdef123456" "def")

(count "abcdef123456")
;; => 12

(format "Hello %s" "World")
;; => "Hello World"
```

### Explanation

Sure, here is a list explaining each portion of the code:

1. `(ns data-types-string.core)`: This declares the namespace of the current file. Namespaces in Clojure are similar to packages in Java, and are used to organize and manage your code.
2. `"abcdef123456"`: This is a literal string in Clojure. It represents a sequence of characters.
3. `(class "abcdef123456")`: The `class` function returns the Java class that the argument belongs to. In this case, it returns `java.lang.String`, because "abcdef123456" is a string.
4. `(string? "abc")`: The `string?` function checks if the argument is a string. In this case, it returns `true`, because "abc" is a string.
5. `(str "abc" "def")` and `(str "abc" "def" 123 "456")`: The `str` function concatenates its arguments into a string. The first expression returns "abcdef", and the second returns "abcdef123456".
6. `(subs "abcdef123456" 0 3)`, `(subs "abcdef123456" 3 7)`, and `(subs "abcdef123456" 0)`: The `subs` function returns a substring of the original string. The first argument is the original string, the second argument is the start index (inclusive), and the optional third argument is the end index (exclusive). If the end index is omitted, it returns the substring from the start index to the end of the string.
7. `(try (subs "abcdef123456" 13) (catch Exception _e "StringIndexOutOfBoundsException"))` and `(try (subs "abcdef123456" -1 3) (catch Exception _e "StringIndexOutOfBoundsException"))`: These are try-catch blocks that attempt to get a substring from the string "abcdef123456". If an `Exception` is caught, it returns the string "StringIndexOutOfBoundsException". This happens when the substring start index or end index is out of the range of the original string's indices.
8. `(.contains "abcdef123456" "def")`: This is a Java interop form that calls the `.contains` method of the String class, which checks if the string contains a certain sequence of characters. In this case, it checks if "abcdef123456" contains "def".
9. `(count "abcdef123456")`: The `count` function returns the number of characters in the string. In this case, it returns 12, because "abcdef123456" has 12 characters.
10. `(format "Hello %s" "World")`: The `format` function formats the string by replacing the format specifiers (like `%s`) with the corresponding arguments. In this case, it returns "Hello World", because `%s` is replaced with "World".

## clojure.string Module

`clojure.string` is a Clojure library that provides functions for working with strings. It is included in the core Clojure library and provides a wide range of string manipulation functions, including concatenation, substring extraction, case conversion, and regular expression matching.

### Code

```clojure
(ns data-types-string.core
  (:require [clojure.string :as str]))

;; ---
;; clojure.string functions
;; ---

(str/join ["abc" "def" "123456"])
;; => "abcdef123456"
(str/split "abcdef123456" #"def")
;; => ["abc" "123456"]
(str/replace "Hello World" "World" "Clojure")
;; => "Hello Clojure"

(str/trim "  abc  ")
;; => "abc"
(str/triml "  abc  ")
;; => "abc  "
(str/trimr "  abc  ")
;; => "  abc"

(str/upper-case "hello world")
;; => "HELLO WORLD"
(str/lower-case "HELLO WORLD")
;; => "hello world"
(str/capitalize "hello world")
;; => "Hello world"

(str/starts-with? "abcdef123456" "abc")
;; => true
(str/ends-with? "abcdef123456" "456")
;; => true

(str/blank? "")
;; => true
(str/blank? nil)
;; => true
(str/blank? "abcdef123456")
;; => false
```

### Explanation

Let's break down each section of this Clojure code:

1. **Namespace declaration and library import**: The `(ns data-types-string.core (:require [clojure.string :as str]))` line declares the namespace for the current file as `data-types-string.core` and then imports the `clojure.string` library, aliasing it as `str` for convenience.
2. **Join function**: `(str/join ["abc" "def" "123456"])` joins together the strings in the array into a single string, producing `"abcdef123456"`.
3. **Split function**: `(str/split "abcdef123456" #"def")` splits the input string `"abcdef123456"` at the substring `"def"`, resulting in a sequence of strings: `["abc" "123456"]`.
4. **Replace function**: `(str/replace "Hello World" "World" "Clojure")` replaces the occurrence of the string "World" in "Hello World" with "Clojure", yielding `"Hello Clojure"`.
5. **Trim functions**:
   - `(str/trim " abc ")` trims whitespace from both ends of the string, resulting in `"abc"`.
   - `(str/triml " abc ")` trims whitespace from the left (start) of the string, resulting in `"abc "`.
   - `(str/trimr " abc ")` trims whitespace from the right (end) of the string, yielding `" abc"`.
6. **Case functions**:
   - `(str/upper-case "hello world")` converts all the characters in the string to uppercase, producing `"HELLO WORLD"`.
   - `(str/lower-case "HELLO WORLD")` converts all the characters in the string to lowercase, resulting in `"hello world"`.
   - `(str/capitalize "hello world")` capitalizes the first character of the string, yielding `"Hello world"`.
7. **Starts with and Ends with functions**:
   - `(str/starts-with? "abcdef123456" "abc")` checks if the string "abcdef123456" starts with "abc", returning `true`.
   - `(str/ends-with? "abcdef123456" "456")` checks if the string "abcdef123456" ends with "456", also returning `true`.
8. **Blank function**: The `str/blank?` function checks whether a string is blank (empty or contains only whitespace) or `nil`.
   - `(str/blank? "")` checks if an empty string is blank, returning `true`.
   - `(str/blank? nil)` checks if `nil` is blank, returning `true`.
   - `(str/blank? "abcdef123456")` checks if "abcdef123456" is blank, returning `false` since it's not empty or whitespace.
