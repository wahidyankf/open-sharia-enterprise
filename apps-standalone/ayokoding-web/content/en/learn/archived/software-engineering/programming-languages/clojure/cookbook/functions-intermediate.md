---
title: "Functions: Intermediate"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/functions_intermediate).

</aside>

## Overview

Clojure provides a variety of utilities for working with functions. These utilities enable powerful functional programming techniques and allow developers to write expressive and concise code. Here are some key utilities that Clojure offers:

1. Higher-order functions: Clojure treats functions as first-class citizens, allowing them to be assigned to variables, passed as arguments to other functions, and returned as results. This enables the use of higher-order functions, which operate on functions themselves. Clojure provides functions like `comp`, `partial`, and `juxt` that help compose, partially apply, and combine functions. These utilities allow for code reuse, abstraction, and expressive function composition.
2. Anonymous functions: Clojure supports creating anonymous functions using the `fn` or `#()` syntax. This enables the creation of functions on-the-fly without explicitly naming them. Anonymous functions are often combined with higher-order functions like `map`, `filter`, and `reduce` to perform transformations and computations on sequences.
3. Function composition: Clojure provides the `comp` function, which allows you to compose multiple functions into one function. Composing functions allows for a clean and declarative style of coding, where the output of one function becomes the input of the next. This facilitates the chaining of transformations and makes code more readable and expressive.
4. Partial application: Clojure offers the `partial` function, which allows you to create new functions by fixing a subset of arguments of an existing function. This technique is useful when you want to create specialized versions of functions or when you need to supply arguments in multiple steps. Partial application helps with code reuse and makes functions more flexible and composable.
5. Currying: Clojure supports currying, which is the transformation of a function that takes multiple arguments into a sequence of functions, each taking a single argument. This can be achieved using the `curry` function from the `clojure.core` namespace. Currying allows for easy partial application and can be useful in creating reusable function components.
6. Function introspection: Clojure provides various utilities for inspecting and manipulating functions. The `doc` function displays documentation for a given function, helping understand its purpose and usage. The `meta` function retrieves metadata associated with a function, such as docstrings or custom annotations. This introspection capability aids in understanding and working with functions effectively.
7. Function combinators: Clojure offers combinators, which are higher-order functions that combine multiple functions to create new functions. For example, the `juxt` function takes several functions as arguments and returns a new function that applies each of them to its arguments and returns a vector of their results. Combinators provide potent abstractions for function composition and can simplify complex operations.

These utilities in Clojure provide developers with a rich set of tools for working with functions in a functional programming style. They enable code reuse, abstraction, composition, and flexibility, allowing for the creation of concise, expressive, and reusable code.

## Playing with Functions

### Code

```clojure
(ns functions-intermediate.core)

;; ---
;; Playing with Function
;; ---

;; Anonymous function

((fn [x] (* x 2)) 5)
;; => 10

(#(* % 2) 5)
;; => 10

;; Recursion

(defn factorial [n]
  (if (<= n 1)
    1
    (* n (factorial (dec n)))))
(factorial 5)
;; => 120

(defn fibonacci [n]
  (if (<= n 2)
    1
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
(fibonacci 10)
;; => 55

;; Higher-order function

(defn apply-twice [f x]
  (f (f x)))
(defn square [x]
  (* x x))

(apply-twice square 2)
;; => 16

;; Function composition

(defn add-one [x]
  (+ x 1))
(defn triple [x]
  (* x 3))
(defn cubic [x]
  (* x x x))
((comp add-one) 3)
;; => 4
((comp add-one triple) 3)
;; => 10
((comp add-one triple cubic) 3)
;; => 82

;; juxt

(def combined-function (juxt add-one triple cubic))
(combined-function 5)
;; => [6 15 125]

;; Partial application and currying

((partial * 3) 4)
;; => 12

(defn add [x y]
  (+ x y))
(def curried-add (partial add 5))
(curried-add 3)
;; => 8
```

### Explanation

Let's break down this Clojure code:

1. **Namespace Declaration**
   The `(ns functions-intermediate.core)` command sets the namespace for the following code. This is similar to a 'package' in other languages like Java or Python.
2. **Anonymous Functions**
   - `((fn (* x 2)) 5)`: This code defines an anonymous function that takes one argument, `x`, and returns `x` multiplied by 2. The outer parentheses call this function with the argument 5, returning 10.
   - `(#(* % 2) 5)`: This is a shorter way to write the same anonymous function using Clojure's shorthand syntax. The `%` symbol is a placeholder for the argument, which is 5.
3. **Recursion**
   - `factorial`: This function calculates the factorial of a number `n` using recursion. If `n` is less than or equal to 1, it returns 1. Otherwise, it multiplies `n` by the factorial of `n-1`.
   - `fibonacci`: This function calculates the `n`th number in the Fibonacci sequence using recursion. If `n` is less than or equal to 2, it returns 1. Otherwise, it returns the sum of the `n-1`th and `n-2`th Fibonacci numbers.
4. **Higher-order function**
   - `apply-twice`: This function takes a function `f` and an argument `x`, and applies `f` to `x` twice. For instance, given the `square` function, which squares its argument, `(apply-twice square 2)` returns 16 (i.e., `(2^2)^2 = 16`).
5. **Function Composition**
   - The `comp` function is used to compose functions together. `((comp add-one triple) 3)` would add one to the result of tripling 3 (i.e., `(1 + (3 * 3) = 10)`), and `((comp add-one triple cubic) 3)` would add one to the result of tripling the cube of 3 (i.e., `(1 + (3 * (3^3)) = 82)`).
6. **Juxt**
   - `juxt` creates a function that calls all its input functions with its input arguments and returns a vector of the results. In the example, `(combined-function 5)` will return a vector where the first element is the result of `add-one 5`, the second element is the result of `triple 5`, and the third element is the result of `cubic 5` (i.e., `[6 15 125]`).
7. **Partial Application and Currying**
   - `partial` creates a function that fixes some number of arguments to a function. For example, `((partial * 3) 4)` creates a function that multiplies its argument by 3, and then applies this function to 4 to get 12.
   - `curried-add`: This function uses `partial` to create a function that adds 5 to its argument. For instance, `(curried-add 3)` will return 8 (i.e., `5 + 3 = 8`).

## Multiple Arities and Variadic Functions

### Code

```clojure
(ns functions-intermediate.core)

;; ---
;; Multiple arities and variadic functions
;; ---

(defn greeting
  ([] "Hello, World!")
  ([name] (str "Hello, " name "!"))
  ([name salutation] (str salutation ", " name "!")))
(greeting)
;; => "Hello, World!"
(greeting "John")
;; => "Hello, John!"
(greeting "John" "Hi")
;; => "Hi, John!"

(defn greeting-v2
  ([] (greeting-v2 "World" "Hello"))
  ([name] (greeting-v2 name "Hello"))
  ([name salutation] (str salutation ", " name "!")))
(greeting-v2)
;; => "Hello, World!"
(greeting-v2 "John")
;; => "Hello, John!"
(greeting-v2 "John" "Hi")
;; => "Hi, John!"

(defn sum [& nums]
  (apply + nums))
(sum 1 2 3 4)
;; => 10
```

### Explanation

This Clojure code defines several functions and tests them. Here's what's happening:

1. The namespace `functions-intermediate.core` is declared using the `ns` form.
2. A function named `greeting` is defined using `defn`. This function is an example of a function with multiple arities, meaning it can be called with a different number of arguments:
   - When called without arguments, it returns the "Hello, World!".
   - When called with one argument, it expects the argument to be a string representing a name and returns a greeting to that name.
   - When called with two arguments, it expects the first argument to be a name and the second to be a salutation, and it constructs a greeting using these inputs.
3. The function `greeting` is then called three times, each with different numbers of arguments, demonstrating the behaviors based on the arity.
4. A similar function, `greeting-v2`, is then defined. This function has the same arities and behaviors as `greeting`, but it is implemented recursively: the zero- and one-argument versions call the two-argument version.
5. The function `greeting-v2` is then also called three times, similarly demonstrating the different behaviors based on arity.
6. A function named `sum` is defined using `defn`. This function demonstrates a variadic function, which can be called with many arguments. The `&` in the argument list gathers all provided arguments into a single collection, bound to the `nums` parameter. The function's body applies the `+` function to this collection of numbers, effectively summing all provided arguments.
7. The `sum` function is then called with four arguments, demonstrating its behavior.

## More on Recursion and Tail Recursion

### Code

```clojure
(ns functions-intermediate.core)

;; ---
;; More on recursion and tail recursion
;; ---

(defn recursive-function [n]
  (if (zero? n)
    0
    (inc (recursive-function (dec n)))))
(recursive-function 100000)
;; => this will most-likely cause stack overflow.
;; You might need to restart your REPL

(defn recursive-function-with-recur [n]
  (loop [n n]
    (if (zero? n)
      0
      (recur (dec n)))))
(recursive-function-with-recur 10000)
;; => 0

(defn factorial-with-recur [n]
  (loop [acc 1 n n]
    (if (<= n 1)
      acc
      (recur (* acc n) (dec n)))))
(factorial-with-recur 5)
;; => 120

(defn fibonacci-with-recur [n]
  (loop [a 1 b 1 n n]
    (if (<= n 2)
      b
      (recur b (+ a b) (dec n)))))
(fibonacci-with-recur 10)
;; => 55
```

### Explanation

Here's an explanation of this Clojure code broken down into points:

1. The code begins by defining the namespace `functions-intermediate.core`.
2. The comment is indicating that the following section of code will demonstrate examples of recursion and tail recursion.
3. The function `recursive-function` is a simple recursive function that increments a counter by one each time it is called until it hits zero, at which point it returns zero. However, the problem with this function is that it's not tail recursive, meaning that it builds up a call stack for every recursive call it makes. This will result in a stack overflow error when the input is large (in this case, `recursive-function` with an argument of `100000`).
4. The function `recursive-function-with-recur` demonstrates how to avoid stack overflow in recursion by using Clojure's `recur` special form inside a `loop` form, which allows for tail-call optimization. Instead of calling the function again (and hence adding to the call stack), the function essentially 'loops' with a new value for `n`. When `n` hits zero, it returns zero. This function will not cause a stack overflow even with large inputs, as demonstrated with the example `(recursive-function-with-recur 10000)`.
5. The function `factorial-with-recur` calculates the factorial of `n` using tail recursion. It uses an accumulator (`acc`) to hold the product of the numbers from `n` down to 1. The `loop` and `recur` forms are used here to ensure tail-call optimization, preventing stack overflow. For example, `(factorial-with-recur 5)` returns `120`.
6. The function `fibonacci-with-recur` calculates the `n`th Fibonacci number using tail recursion. The `loop` form and `recur` special form are used to ensure tail-call optimization. The function uses two variables `a` and `b` to hold the last two numbers in the sequence. For instance, `(fibonacci-with-recur 10)` returns `55`, the 10th number in the Fibonacci sequence.

## Utilities

### Code

```clojure
(ns functions-intermediate.core
  (:require [clojure.repl :refer [doc]]
            [clojure.string :as string]))

;; ---
;; Other utilities
;; ---

;; Documentation

(doc partial)
;; => print:
;; (doc partial)
;; -------------------------
;; clojure.core/partial
;; ([f] [f arg1] [f arg1 arg2] [f arg1 arg2 arg3] [f arg1 arg2 arg3 & more])
;; Takes a function f and fewer than the normal arguments to f, and
;; returns a fn that takes a variable number of additional args. When
;; called, the returned function calls f with args + additional args.

(defn triple-2
  "Calculates the triple of a number."
  [x]
  (* x 3))

(doc triple-2)
;; => print:
;; -------------------------
;; functions-intermediate.core/triple-2
;; ([x])
;; Calculates the tripe of a number.

;; Metadata

(defn ^{:author "John Doe" :date "2023-06-11"} my-function [x]
  (str "Executing my-function with argument: " x))

(:author (meta #'my-function))
;; => "John Doe"
(:date (meta #'my-function))
;; => "2023-06-11"

;; Threading

(-> "   clojure  "
    (string/lower-case)
    (string/trim)
    (string/replace " " "-"))
;; => "clojure"
(clojure.string/replace (clojure.string/trim (clojure.string/lower-case "   clojure  ")) " " "-")
;; => "clojure"

(->> [1 2 3 4 5]
     (map #(* % 2))
     (filter odd?)
     (reduce +))
;; => 0
(reduce + (filter odd? (map #(* % 2) [1 2 3 4 5])))
;; => 0
```

### Explanation

Here's an explanation of this Clojure code broken down into points:

1. `(ns functions-intermediate.core...)` - This sets the namespace for the code that follows. `clojure.repl` and `clojure.string` are required for the functions used later in the code.
2. `(doc partial)` - This is a function from `clojure.repl` that prints the documentation for the specified function. In this case, it is used to print the documentation for `partial`, a function from `clojure.core`. `partial` takes a function and a variable number of arguments and returns a new function that when called, calls the original function with the specified arguments and any additional ones passed to it.
3. `defn triple-2...` - This is a function definition for `triple-2`, a function that multiplies its argument by 3.
4. `(doc triple-2)` - This prints the documentation for the `triple-2` function, including its parameters and description.
5. `defn ^{:author "John Doe" :date "2023-06-11"} my-function...` - This is a function definition for `my-function` that includes metadata. The metadata includes the `:author` and `:date` attributes.
6. `(:author (meta #'my-function))` and `(:date (meta #'my-function))` - These expressions retrieve the `:author` and `:date` metadata attributes from the `my-function` function definition.
7. The `>` macro (threading macro) - The threading macro takes an expression (in this case, the string " clojure ") and passes it as the first argument to the next function, then takes the result and passes it as the first argument to the next function, and so on. The code does the following:
   - Transforms the input string to lower-case.
   - Trims leading and trailing spaces.
   - Replaces spaces with dashes.
8. The `>>` macro (thread-last macro) - Similar to the `>` threading macro, but passes the result as the last argument to the next function. The code does the following:
   - Doubles every number in the vector [1, 2, 3, 4, 5].
   - Filters out the even numbers.
   - Sums the remaining numbers.

Note: The `->` and `->>` macros can significantly improve the readability of your code when you are composing several functions. The last two expressions (7th and 8th) are equivalent, but use different ways of expressing the function composition.
