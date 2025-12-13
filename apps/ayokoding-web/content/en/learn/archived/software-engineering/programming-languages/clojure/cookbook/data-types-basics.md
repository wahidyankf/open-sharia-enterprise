---
title: "Data Types: Basics"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/data_types_basics).

</aside>

## Basic Data Types

### Code

```clojure
(ns data-types-basic.core)

;; ---
;; Basic data types
;; ---

;; Integers
42
;; => 42
;; Float
3.14
;; => 3.14
36786883868216818816N
;; => 36786883868216818816N
3.14159265358M
;; => 3.14159265358M
22/7
;; => 22/7

;; Booleans
true
;; => true
false
;; => false

;; Character
\a
;; => \a
\b
;; => \b
\
;; => \space

;; Strings
"abcdef123456"
;; => "abcdef123456"

;; Keywords
:my-keyword
;; => :my-keyword

nil
;; => nil
```

### Explanation

Explanation of the Clojure code:

1. Integers:
   - The code `42` represents an integer with the value 42.
   - The `=> 42` comment indicates that the result of evaluating the code is 42.
2. Float:
   - The code `3.14` represents a floating-point number with the value 3.14.
   - The `=> 3.14` comment indicates that the result of evaluating the code is 3.14.
3. BigInt:
   - The code `36786883868216818816N` represents a BigInt, which is a way to represent arbitrarily large integers in Clojure.
   - The `=> 36786883868216818816N` comment indicates that the result of evaluating the code is 36786883868216818816N.
4. BigDecimal:
   - The code `3.14159265358M` represents a BigDecimal, which is a way to represent arbitrary-precision decimal numbers in Clojure.
   - The `=> 3.14159265358M` comment indicates that the result of evaluating the code is 3.14159265358M.
5. Ratios:
   - The code `22/7` represents a ratio, which is a way to represent fractions in Clojure.
   - The `=> 22/7` comment indicates that the result of evaluating the code is 22/7.
6. Booleans:
   - The code `true` represents the boolean value true.
   - The `=> true` comment indicates that the result of evaluating the code is true.
   - The code `false` represents the boolean value false.
   - The `=> false` comment indicates that the result of evaluating the code is false.
7. Character:
   - The code `\\a` represents the character 'a'.
   - The `=> \\a` comment indicates that the result of evaluating the code is the character 'a'.
   - The code `\\b` represents the character 'b'.
   - The `=> \\b` comment indicates that the result of evaluating the code is the character 'b'.
   - The code `\\\\` represents the character '\'.
   - The `=> \\space` comment indicates that the result of evaluating the code is the character '\space'.
8. Strings:
   - The code `"abcdef123456"` represents a string with the value "abcdef123456".
   - The `=> "abcdef123456"` comment indicates that the result of evaluating the code is the string "abcdef123456".
9. Keywords:
   - The code `:my-keyword` represents a keyword, which is a unique identifier in Clojure.
   - The `=> :my-keyword` comment indicates that the result of evaluating the code is the keyword :my-keyword.
10. nil:
    - The code `nil` represents the absence of a value or the null value in Clojure.
    - The `=> nil` comment indicates that the result of evaluating the code is nil.

---

This code demonstrates the basic data types in Clojure, including integers, floats, BigInts, BigDecimals, ratios, booleans, characters, strings, keywords, and nil. A specific syntax represents each data type, and the comments indicate the result of evaluating the code. Understanding these data types is essential for working with data in Clojure.

## Underlying Class Implementation

### Code

```clojure
(ns data-types-basic.core)

;; ---
;; Underlying Class
;; ---

(class 42)
;; => java.lang.Long

(class 3.14)
;; => java.lang.Double

(class 36786883868216818816N)
;; => clojure.lang.BigInt

(class 3.14159265358M)
;; => java.math.BigDecimal

(class 22/7)
;; => clojure.lang.Ratio

(class true)
;; => java.lang.Boolean

(class false)
;; => java.lang.Boolean

(class \a)
;; => java.lang.Character

(class "abcdef123456")
;; => java.lang.String

(class :my-keyword)
;; => clojure.lang.Keyword

(class nil)
;; => nil
```

### Explanation

Explanation of the Clojure code:

1. The code `(class 42)` returns the underlying class of the value 42.
2. The `=> java.lang.Long` comment indicates that the result of evaluating the code is the class `java.lang.Long`.
3. The code `(class 3.14)` returns the underlying class of the value 3.14.
4. The `=> java.lang.Double` comment indicates that the result of evaluating the code is the class `java.lang.Double`.
5. The code `(class 36786883868216818816N)` returns the underlying class of the value 36786883868216818816N.
6. The `=> clojure.lang.BigInt` comment indicates that the result of evaluating the code is the class `clojure.lang.BigInt`.
7. The code `(class 3.14159265358M)` returns the underlying class of the value 3.14159265358M.
8. The `=> java.math.BigDecimal` comment indicates that the result of evaluating the code is the class `java.math.BigDecimal`.
9. The code `(class 22/7)` returns the underlying class of the value 22/7.
10. The `=> clojure.lang.Ratio` comment indicates that the result of evaluating the code is the class `clojure.lang.Ratio`.
11. The code `(class true)` returns the underlying class of the value true.
12. The `=> java.lang.Boolean` comment indicates that the result of evaluating the code is the class `java.lang.Boolean`.
13. The code `(class false)` returns the underlying class of the value false.
14. The `=> java.lang.Boolean` comment indicates that the result of evaluating the code is the class `java.lang.Boolean`.
15. The code `(class \\a)` returns the value 'a' underlying class.
16. The `=> java.lang.Character` comment indicates that the result of evaluating the code is the class `java.lang.Character`.
17. The code `(class "abcdef123456")` returns the underlying class of the value "abcdef123456".
18. The `=> java.lang.String` comment indicates that the result of evaluating the code is the class `java.lang.String`.
19. The code `(class :my-keyword)` returns the underlying class of the value :my-keyword.
20. The `=> clojure.lang.Keyword` comment indicates that the result of evaluating the code is the class `clojure.lang.Keyword`.
21. The code `(class nil)` returns the underlying class of the value nil.
22. The `=> nil` comment indicates that the result of evaluating the code is nil.

---

This code demonstrates how to determine the underlying class of values in Clojure. The `class` function is used to retrieve the class of a value. The comments indicate the result of evaluating the code, which is the class of the corresponding value. Understanding the underlying class of values can be helpful when working with Java interop or polymorphic behavior in Clojure.
