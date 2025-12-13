---
title: "Functions: Basics"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the source code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/functions_basics).

</aside>

## Code

```clojure
(ns functions-basic.core)

(defn print-hello-world []
  (println "Hello world"))
(print-hello-world)
;; => nil

(defn get-hello [who]
  (str "Hello " who "!"))
(get-hello "world")
;; => "Hello world!"

(defn add [x y]
  (+ x y))
(add 1 2)
;; => 3

(defn print-hello [who]
  (println (get-hello who)))
(print-hello "world")
;; => nil
```

## Explanation

Here is an explanation of the given Clojure code:

Here is a comprehensive explanation of the Clojure code:

1. **Namespace definition**: `(ns functions-basic.core)`. This defines the namespace for the current file. `ns` stands for namespace, and `functions-basic.core` is the name of the namespace. A namespace is a container that allows you to group related functions and variables together, and it helps to avoid name clashes in large projects.
2. **Function definition**: `(defn print-hello-world [] (println "Hello world"))`. This declares a function named `print-hello-world` with `defn`. The function takes no arguments, as indicated by the empty vector `[]`, and prints the "Hello world" string to the console.
3. **Function call**: `(print-hello-world)`. This is a call to the `print-hello-world` function. It will print "Hello world" to the console. The comment `;; => nil` indicates that the function returns `nil` - Clojure functions by default return the value of their last statement, and the `println` function returns `nil`.
4. **String concatenation function**: `(defn get-hello [who] (str "Hello " who "!"))`. This defines a function named `get-hello` that takes a single argument `who` and returns a string that starts with "Hello ", appends the value of `who`, and then adds "!" at the end. The `str` function is used to concatenate strings.
5. **Function call with parameter**: `(get-hello "world")`. This is a call to the `get-hello` function with the argument "world". The function returns the string "Hello world!".
6. **Addition function**: `(defn add [x y] (+ x y))`. This defines a function named `add` that takes two arguments `x` and `y`, and returns their sum. The `+` operator is used to add numbers in Clojure.
7. **Function call with parameters**: `(add 1 2)`. This is a call to the `add` function with the arguments 1 and 2. The function returns the sum, which is 3.
8. **Function definition with function call**: `(defn print-hello [who] (println (get-hello who)))`. This defines a function named `print-hello` that takes a single argument `who`. Inside this function, it first calls the `get-hello` function with the `who` argument and then prints the returned string.
9. **Nested function call**: `(print-hello "world")`. This is a call to the `print-hello` function with the argument "world". This function calls the `get-hello` function with "world" as an argument, concatenates the return with "Hello " and "!", then prints the result. Again, the comment `;; => nil` shows the `println` function's return value is `nil`.

This code provides examples of defining and calling functions in Clojure, including functions with no arguments, functions with arguments, and functions that call other functions. It also demonstrates string concatenation and addition.
