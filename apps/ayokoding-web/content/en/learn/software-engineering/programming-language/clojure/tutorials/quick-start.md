---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn core Clojure syntax and functional programming patterns"
tags:
  - clojure
  - quick-start
  - functional-programming
---

Learn core Clojure syntax and functional programming patterns to start reading and writing Clojure code. This Quick Start teaches the essential concepts you need to explore Clojure independently.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- Clojure's Lisp syntax and data structures
- Functional programming with immutable data
- Working with sequences and collections
- Core functions and higher-order patterns

## 📋 Prerequisites

- Clojure and Leiningen installed (see [Initial Setup](/en/learn/software-engineering/programming-language/clojure/tutorials/initial-setup))
- Basic programming knowledge in any language

## 🔤 Basic Syntax

Clojure uses prefix notation (Polish notation):

```clojure
;; Addition
(+ 1 2 3)  ;; => 6

;; Subtraction
(- 10 3)  ;; => 7

;; Multiplication
(* 2 3 4)  ;; => 24

;; Division
(/ 10 2)  ;; => 5

;; Nested expressions
(+ (* 2 3) (- 10 4))  ;; => 12
```

Everything in Clojure is an expression that returns a value.

## 📦 Data Structures

Clojure has four core immutable data structures:

### Vectors (Indexed Collections)

```clojure
[1 2 3 4 5]
["alice" "bob" "charlie"]

;; Access by index
(get [1 2 3] 0)  ;; => 1
(nth [1 2 3] 1)  ;; => 2

;; Add element
(conj [1 2 3] 4)  ;; => [1 2 3 4]

;; First and rest
(first [1 2 3])  ;; => 1
(rest [1 2 3])   ;; => (2 3)
```

### Lists (Sequential Collections)

```clojure
'(1 2 3 4 5)
(list 1 2 3)

;; Add to front
(conj '(2 3) 1)  ;; => (1 2 3)

;; First and rest
(first '(1 2 3))  ;; => 1
(rest '(1 2 3))   ;; => (2 3)
```

### Maps (Key-Value Pairs)

```clojure
{:name "Alice" :age 30 :city "Jakarta"}

;; Access values
(get {:name "Alice"} :name)  ;; => "Alice"
(:name {:name "Alice"})      ;; => "Alice"

;; Add/update
(assoc {:name "Alice"} :age 30)  ;; => {:name "Alice" :age 30}

;; Remove
(dissoc {:name "Alice" :age 30} :age)  ;; => {:name "Alice"}
```

### Sets (Unique Collections)

```clojure
#{1 2 3 4 5}

;; Add element
(conj #{1 2 3} 4)  ;; => #{1 2 3 4}

;; Check membership
(contains? #{1 2 3} 2)  ;; => true
(#{1 2 3} 2)            ;; => 2
```

## 🔧 Defining Values and Functions

### Values (Immutable)

```clojure
(def x 42)
(def name "Alice")
(def numbers [1 2 3 4 5])
```

### Functions

```clojure
;; Named function
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alice")  ;; => "Hello, Alice!"

;; Multiple parameters
(defn add [a b]
  (+ a b))

(add 5 3)  ;; => 8

;; Anonymous function
(fn [x] (* x x))
((fn [x] (* x x)) 5)  ;; => 25

;; Short syntax for anonymous functions
#(* % %)
(#(* % %) 5)  ;; => 25

;; Multiple parameters in short syntax
#(+ %1 %2)
(#(+ %1 %2) 3 4)  ;; => 7
```

## 🔄 Conditional Logic

```clojure
;; if
(if true
  "yes"
  "no")  ;; => "yes"

(if (> 5 3)
  "greater"
  "not greater")  ;; => "greater"

;; when (no else clause)
(when (> 5 3)
  (println "5 is greater")
  "done")

;; cond (multiple conditions)
(cond
  (< 5 3) "less"
  (> 5 3) "greater"
  :else "equal")  ;; => "greater"

;; case (pattern matching)
(case 2
  1 "one"
  2 "two"
  3 "three"
  "other")  ;; => "two"
```

## 📋 Working with Sequences

Clojure excels at sequence processing:

```clojure
;; map - transform each element
(map inc [1 2 3 4 5])  ;; => (2 3 4 5 6)
(map #(* % %) [1 2 3 4 5])  ;; => (1 4 9 16 25)

;; filter - keep elements matching predicate
(filter even? [1 2 3 4 5 6])  ;; => (2 4 6)
(filter #(> % 3) [1 2 3 4 5])  ;; => (4 5)

;; reduce - combine elements
(reduce + [1 2 3 4 5])  ;; => 15
(reduce * [1 2 3 4 5])  ;; => 120

;; take and drop
(take 3 [1 2 3 4 5])  ;; => (1 2 3)
(drop 2 [1 2 3 4 5])  ;; => (3 4 5)

;; partition
(partition 2 [1 2 3 4 5 6])  ;; => ((1 2) (3 4) (5 6))
```

## 🚀 Function Composition

Combine functions for powerful transformations:

```clojure
;; Threading macro (->>)
(->> [1 2 3 4 5]
     (map #(* % %))
     (filter even?)
     (reduce +))  ;; => 20

;; Equivalent to:
(reduce + (filter even? (map #(* % %) [1 2 3 4 5])))

;; Threading macro (->)
(-> {:name "Alice"}
    (assoc :age 30)
    (assoc :city "Jakarta"))  ;; => {:name "Alice" :age 30 :city "Jakarta"}
```

## 💡 Immutability

All data structures are immutable by default:

```clojure
(def numbers [1 2 3])
(conj numbers 4)  ;; => [1 2 3 4]
numbers           ;; => [1 2 3] (unchanged)

(def person {:name "Alice" :age 30})
(assoc person :city "Jakarta")  ;; => {:name "Alice" :age 30 :city "Jakarta"}
person                          ;; => {:name "Alice" :age 30} (unchanged)
```

## 🧪 REPL-Driven Development

The REPL is central to Clojure development:

```clojure
;; Start REPL
;; lein repl

;; Load namespace
(require '[clojure.string :as str])

;; Use functions
(str/upper-case "hello")  ;; => "HELLO"
(str/split "a,b,c" #",")  ;; => ["a" "b" "c"]

;; Reload changed code
(require 'my-namespace :reload)
```

## 📚 Common Standard Library Functions

```clojure
;; Strings
(str "Hello" " " "World")  ;; => "Hello World"
(clojure.string/upper-case "hello")  ;; => "HELLO"

;; Collections
(count [1 2 3])  ;; => 3
(empty? [])      ;; => true
(reverse [1 2 3])  ;; => (3 2 1)
(sort [3 1 2])   ;; => (1 2 3)

;; Predicates
(even? 4)   ;; => true
(odd? 3)    ;; => true
(nil? nil)  ;; => true
(empty? []) ;; => true
```

## ✅ Next Steps

You now understand Clojure's core concepts! To deepen your knowledge:

1. **Try the examples**: Run each code snippet in the REPL
2. **Explore By Example**: [Clojure By Example](/en/learn/software-engineering/programming-language/clojure/tutorials/by-example) - 80 annotated examples

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Understand Clojure's prefix notation syntax
- [ ] Work with vectors, lists, maps, and sets
- [ ] Define functions with `defn`
- [ ] Use `map`, `filter`, and `reduce` for sequence processing
- [ ] Understand immutability and its benefits
- [ ] Use the REPL for interactive development
