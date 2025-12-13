---
title: "Hello World"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/hello_world).

</aside>

## Code

```clojure
(ns hello-world.core)

(println "Hello world")
;; => nil
```

### Explanation

Here's a step-by-step explanation of the Clojure code snippet:

1. `ns`: The `ns` macro is used to define a namespace in Clojure. A namespace in Clojure (and in other programming languages) is a container that allows the developer to group related functions, macros, and data together. This aims to avoid naming conflicts between different parts of a program. The provided code snippet defines the namespace as `hello-world.core`.
2. `hello-world.core`: This is the name of the namespace declared. `hello-world` is usually the name of the project or application, and `core` is a conventional name for a main namespace in a Clojure application. However, these are just names and can be anything the programmer wishes.
3. `println`: The `println` function is a built-in function in Clojure for outputting text to the console. The text to be printed is passed as an argument to the `println` function.
4. `"Hello world"`: This is a string argument passed to the `println` function. The `println` function will print the text "Hello world" to the console when the program is run.
5. `;; => nil`: This is a comment in Clojure. Anything that follows `;;` on the same line is a comment that the Clojure interpreter ignores. The `=> nil` indicates the expected return value of the `println` function call. In Clojure, the `println` function returns `nil` after it prints to the console. It's important to note that `nil` in Clojure is equivalent to `null` in other languages, which signifies no value or absence of value.
6. Clojure Syntax: Clojure uses a prefix notation which means that the function name comes before the arguments (e.g., `(println "Hello world")`). This is a characteristic of Lisp-like languages, which Clojure is part of. Code is organized in lists that are delimited by parentheses. The first element of a list is usually a function or macro to be invoked, followed by its arguments.
7. S-Expressions: Clojure, as a dialect of Lisp, follows the idea of "code as data" and uses s-expressions (symbolic expressions) to represent both code and data. An s-expression can be an atom or a list of s-expressions, as you see with the `(println "Hello world")`.
