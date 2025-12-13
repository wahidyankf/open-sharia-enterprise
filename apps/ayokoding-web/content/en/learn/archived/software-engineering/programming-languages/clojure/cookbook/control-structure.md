---
title: "Control Structure"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

Clojure, a modern functional language on the Java platform, has a distinct approach to control structures, primarily influenced by its Lisp roots. It encourages a functional programming style, leaning heavily on recursion, immutability, and higher-order functions for managing program flow. Despite this, it also provides a suite of control structures familiar to those from imperative or procedural languages. Fundamental control structures in Clojure include **`if`**, **`when`**, **`cond`**, **`case`**, and **`loop`** coupled with **`recur`** for more efficient looping. Macros like **`if-let`** and **`when-let`** provide conditional bindings and help reduce verbosity in handling potential **`nil`** values.

## Code

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/control_structure).

</aside>

```clojure
(ns control-structure.core)

(defn demo-if [x]
  (if (> x 0)
    (str "x is positive")
    (str "x is non-positive")))
(demo-if -1)
;; => "x is non-positive"
(demo-if 0)
;; => "x is non-positive"
(demo-if 1)
;; => "x is positive"

(defn demo-if-let [m]
  (if-let [val (m :key)]
    (str "Value found:" val)
    (str "No value found")))
(demo-if-let {:key "A value"})
;; => "Value found:A value"
(demo-if-let {})
;; => "No value found"

(defn demo-when [x]
  (when (= x 0)
    (str "x is zero")))
(demo-when 0)
;; => "x is zero"
(demo-when 1)
;; => nil

(defn demo-when-let [m]
  (when-let [val (m :key)]
    (str "Value found:" val)))
(demo-when-let {:key "Another value"})
;; => "Value found:Another value"
(demo-when-let {})
;; => nil

(defn demo-cond [x]
  (cond
    (< x 0) (str "x is negative")
    (= x 0) (str "x is zero")
    :else   (str "x is positive")))
(demo-cond -1)
;; => "x is negative"
(demo-cond 0)
;; => "x is zero"
(demo-cond 1)
;; => "x is positive"

(defn demo-case [x]
  (case x
    0 (str "x is zero")
    1 (str "x is one")
    (str "x is neither zero nor one")))
(demo-case 0)
;; => "x is zero"
(demo-case 1)
;; => "x is one"
(demo-case 2)
;; => "x is neither zero nor one"

(defn demo-for [x]
  (for [i (range x)]
    (str "i:" i)))
(demo-for 3)
;; => ("i:0" "i:1" "i:2")

(defn demo-loop [x]
  (loop [i 0]
    (when (< i x)
      (println "i:" i)
      (recur (inc i)))))
(demo-loop 3)
;; => nil
```

Let's break this down into separate parts:

1. `(ns control-structure.core)` - This sets the namespace for the current file, sort of like using `package` in Java. The `ns` macro is used to declare a namespace, which in this case is `control-structure.core`.
2. `(defn demo-if [x]...)` - This is a function definition named `demo-if` that takes a single argument `x`. The `if` function then checks whether `x` is greater than zero. If it is, the function returns the string "x is positive", otherwise it returns "x is non-positive".
3. `(defn demo-if-let [m]...)` - This is a function definition named `demo-if-let` that takes a map `m` as its argument. The `if-let` form is used to bind `val` to the value of `:key` in `m` if it exists, otherwise it proceeds to the else part. If `val` is not `nil`, it will return "Value found:val", otherwise it will return "No value found".
4. `(defn demo-when [x]...)` - This function, named `demo-when`, takes a single argument `x`. It uses the `when` macro, which is similar to `if` but only has one branch. If the condition is true (in this case, if `x` equals 0), it executes the body and returns the result; otherwise, it returns `nil`.
5. `(defn demo-when-let [m]...)` - Similar to `demo-if-let`, but it uses `when-let`. If the value at `:key` in the map `m` is non-nil, it binds the value to `val` and returns "Value found:val". If it's `nil`, it does nothing and returns `nil`.
6. `(defn demo-cond [x]...)` - This is a function named `demo-cond` which uses the `cond` macro for multi-way conditionals. If `x` is less than zero it returns "x is negative", if `x` equals zero it returns "x is zero", otherwise it returns "x is positive".
7. `(defn demo-case [x]...)` - The function `demo-case` uses the `case` macro, which is similar to `switch` in other languages. Depending on the value of `x`, it returns "x is zero", "x is one", or "x is neither zero nor one".
8. `(defn demo-for [x]...)` - This function, named `demo-for`, uses the `for` macro to create a sequence of strings for every number in the range from 0 to `x` (exclusive). The range function generates a sequence of numbers.
9. `(defn demo-loop [x]...)` - This function named `demo-loop` demonstrates the `loop` and `recur` macros for looping in Clojure. The `loop` macro sets up some bindings, here `i` is bound to `0`. If `i` is less than `x`, it prints "i:i" and recurs with an incremented `i`. The `recur` function jumps back to the nearest enclosing `loop` or function call. The `demo-loop` function doesn't return a value, hence it returns `nil`.
