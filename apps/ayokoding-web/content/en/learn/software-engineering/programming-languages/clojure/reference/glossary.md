---
title: "Glossary"
date: 2025-12-30T14:13:06+07:00
draft: false
weight: 1000040
description: Clojure-specific terminology and definitions for functional programmers and learners
tags: ["clojure", "glossary", "reference", "terminology", "functional-programming", "lisp"]
---

**Comprehensive glossary** of Clojure-specific terms, concepts, and functional programming features. Organized alphabetically for quick reference.

## A

### Agent

**Definition**: Reference type for managing asynchronous, independent state changes. Updates happen in thread pool, not immediately.

**Example**:

```clojure
(def logger (agent []))

(send logger conj "Log 1")
(send logger conj "Log 2")

(await logger)
@logger  ; ["Log 1" "Log 2"]
```

**See Also**: Atom, Ref, State Management, Asynchronous

### Arity

**Definition**: Number of arguments a function accepts. Clojure functions can have multiple arities.

**Example**:

```clojure
(defn greet
  ([] (greet "World"))           ; 0-arity
  ([name] (str "Hello, " name))) ; 1-arity

(greet)        ; "Hello, World"
(greet "Alice") ; "Hello, Alice"
```

**See Also**: Function, Variadic, Overloading

### Atom

**Definition**: Reference type for managing synchronous, independent state changes. Updates are atomic and immediate.

**Example**:

```clojure
(def counter (atom 0))

(swap! counter inc)  ; 1
(reset! counter 10)  ; 10
@counter            ; 10
```

**See Also**: State Management, Ref, Agent, Swap, Reset

## B

### Binding

**Definition**: Association between a name and a value. Can be lexical (let) or dynamic (binding with ^:dynamic vars).

**Example**:

```clojure
;; Lexical binding
(let [x 10
      y 20]
  (+ x y))  ; 30

;; Dynamic binding
(def ^:dynamic *config* {:env "dev"})

(binding [*config* {:env "prod"}]
  (:env *config*))  ; "prod"
```

**See Also**: Let, Var, Dynamic Var, Scope

## C

### Closure

**Definition**: Function that captures and retains access to variables from its enclosing scope.

**Example**:

```clojure
(defn make-adder [n]
  (fn [x] (+ x n)))  ; Captures n

(def add5 (make-adder 5))
(add5 10)  ; 15
```

**See Also**: Function, Lexical Scope, Higher-Order Function

### Cons

**Definition**: Fundamental operation for prepending an element to a sequence. Creates new list with element as head.

**Example**:

```clojure
(cons 1 [2 3 4])     ; (1 2 3 4)
(cons :a '(:b :c))   ; (:a :b :c)

;; cons always returns a seq, not original type
(type (cons 1 [2 3]))  ; clojure.lang.Cons
```

**See Also**: Conj, First, Rest, Sequence

### Conj

**Definition**: Add element to collection in most efficient way. For vectors, adds to end; for lists, adds to front.

**Example**:

```clojure
(conj [1 2 3] 4)     ; [1 2 3 4] - vector adds to end
(conj '(1 2 3) 0)    ; (0 1 2 3) - list adds to front
(conj #{1 2 3} 4)    ; #{1 2 3 4} - set
(conj {:a 1} [:b 2]) ; {:a 1, :b 2} - map
```

**See Also**: Cons, Into, Assoc, Collection

## D

### Def

**Definition**: Creates or re-defines a global var in current namespace. Root binding for the var.

**Example**:

```clojure
(def pi 3.14159)
(def greeting "Hello, Clojure!")

(def calculate-area
  (fn [radius] (* pi radius radius)))
```

**See Also**: Defn, Var, Namespace, Let

### Defmacro

**Definition**: Defines a macro - code that generates code at compile time. Macros receive unevaluated forms and return forms to be evaluated.

**Example**:

```clojure
(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

(unless false
  (println "This runs")
  :done)
```

**See Also**: Macro, Quote, Unquote, Syntax Quote

### Defn

**Definition**: Defines a named function. Syntactic sugar over def with fn.

**Example**:

```clojure
(defn add [a b]
  (+ a b))

;; Equivalent to:
(def add
  (fn [a b] (+ a b)))

;; With metadata
(defn ^:private helper [x]
  (* x x))
```

**See Also**: Def, Function, Fn, Arity

### Deref

**Definition**: Dereferences a reference type (atom, ref, agent, future, delay) to get its current value. Can use `@` reader macro.

**Example**:

```clojure
(def counter (atom 5))

(deref counter)  ; 5
@counter        ; 5 (reader macro)

(def result (future (Thread/sleep 1000) 42))
@result         ; 42 (blocks until complete)
```

**See Also**: Atom, Ref, Agent, Reference Types

### Destructuring

**Definition**: Pattern matching syntax to extract values from data structures in let bindings and function parameters.

**Example**:

```clojure
;; Sequential destructuring
(let [[a b c] [1 2 3]]
  (+ a b c))  ; 6

;; Map destructuring
(let [{:keys [name age]} {:name "Alice" :age 30}]
  (str name " is " age))  ; "Alice is 30"

;; Nested destructuring
(defn process-user [{:keys [name address]
                     {:keys [city]} :address}]
  (str name " lives in " city))
```

**See Also**: Let, Function Parameters, Pattern Matching

### Dynamic Var

**Definition**: Var marked with `^:dynamic` metadata that can be thread-locally rebound using `binding`.

**Example**:

```clojure
(def ^:dynamic *db-connection* nil)

(defn query []
  (println "Using:" *db-connection*))

(binding [*db-connection* "postgresql://..."]
  (query))  ; "Using: postgresql://..."

(query)     ; "Using: nil" (restored)
```

**See Also**: Var, Binding, Thread-Local, Metadata

## F

### First

**Definition**: Returns the first element of a sequence. Returns nil for empty sequences.

**Example**:

```clojure
(first [1 2 3])      ; 1
(first '(:a :b :c))  ; :a
(first [])           ; nil
```

**See Also**: Rest, Last, Nth, Sequence

### Fn

**Definition**: Creates an anonymous function. Can be named for recursion.

**Example**:

```clojure
;; Anonymous
(fn [x] (* x x))

;; Multiple arity
(fn
  ([x] (* x x))
  ([x y] (* x y)))

;; Named for recursion
(fn factorial [n]
  (if (<= n 1)
    1
    (* n (factorial (dec n)))))

;; Short form
#(* % %)
```

**See Also**: Defn, Lambda, Closure, Higher-Order Function

### Future

**Definition**: Executes expression in another thread. Dereferencing blocks until computation completes.

**Example**:

```clojure
(def result (future
              (Thread/sleep 2000)
              (+ 1 2 3)))

;; Do other work...

@result  ; 6 (blocks if not done)

;; Check if done
(future-done? result)  ; true/false
```

**See Also**: Promise, Delay, Deref, Concurrency

## H

### Higher-Order Function

**Definition**: Function that takes other functions as arguments or returns functions as results.

**Example**:

```clojure
;; Takes function as argument
(map inc [1 2 3])  ; (2 3 4)
(filter even? [1 2 3 4])  ; (2 4)

;; Returns function
(defn make-multiplier [n]
  (fn [x] (* x n)))

(def times3 (make-multiplier 3))
(times3 5)  ; 15
```

**See Also**: Function, Map, Filter, Reduce, Comp

### Homoiconicity

**Definition**: Property where code and data have the same structure. Clojure code is made of Clojure data structures (lists, vectors, etc.).

**Example**:

```clojure
;; This code:
(+ 1 2 3)

;; Is a list data structure:
'(+ 1 2 3)

;; Can be manipulated as data:
(first '(+ 1 2 3))   ; +
(rest '(+ 1 2 3))    ; (1 2 3)
```

**See Also**: Macro, Lisp, Code as Data, S-expression

## I

### Immutability

**Definition**: Property where data structures cannot be modified after creation. Operations return new structures with changes.

**Example**:

```clojure
(def v [1 2 3])
(def v2 (conj v 4))  ; [1 2 3 4]

v   ; [1 2 3] - original unchanged
v2  ; [1 2 3 4] - new vector

;; Structural sharing - efficient memory use
(identical? (pop [1 2 3]) [1 2])  ; false (different objects)
```

**See Also**: Persistent Data Structure, Structural Sharing, Pure Function

## K

### Keyword

**Definition**: Self-evaluating identifier starting with `:`. Often used as map keys. Can be namespaced.

**Example**:

```clojure
:name                    ; Simple keyword
:user/id                 ; Namespaced keyword

;; As map keys
{:name "Alice" :age 30}

;; As functions (get value from map)
(:name {:name "Alice"})  ; "Alice"

;; In sets
#{:read :write :execute}
```

**See Also**: Symbol, Map, Namespace

## L

### Lazy Sequence

**Definition**: Sequence where elements are computed on-demand, not immediately. Enables infinite sequences and performance optimization.

**Example**:

```clojure
;; Infinite sequence (doesn't hang)
(def natural-numbers (range))

(take 5 natural-numbers)  ; (0 1 2 3 4)

;; Lazy map - not computed until needed
(def squares (map #(* % %) (range 1000000)))

(take 3 squares)  ; (0 1 4) - only 3 computed

;; Custom lazy sequence
(defn fib-seq
  ([] (fib-seq 0 1))
  ([a b] (lazy-seq (cons a (fib-seq b (+ a b))))))

(take 10 (fib-seq))  ; (0 1 1 2 3 5 8 13 21 34)
```

**See Also**: Sequence, Range, Map, Lazy-seq, Realize

### Let

**Definition**: Creates lexical bindings for local scope. Bindings available in body but not outside.

**Example**:

```clojure
(let [x 10
      y 20
      sum (+ x y)]
  (* sum 2))  ; 60

;; Bindings not available here
;; x is undefined

;; Destructuring in let
(let [[a b] [1 2]
      {:keys [name]} {:name "Alice"}]
  (str name ": " (+ a b)))
```

**See Also**: Binding, Destructuring, Scope, Def

### Lisp

**Definition**: Family of programming languages based on s-expressions. Clojure is a modern Lisp dialect on the JVM.

**Example**:

```clojure
;; S-expressions (symbolic expressions)
(+ 1 2 3)           ; Prefix notation
(if true :yes :no)  ; Everything is an expression

;; Code as data (homoiconicity)
'(defn add [a b] (+ a b))  ; This is a list
```

**See Also**: Homoiconicity, S-expression, Macro, Functional Programming

## M

### Macro

**Definition**: Code transformation mechanism that operates at compile time. Receives unevaluated code, returns code to be evaluated.

**Example**:

```clojure
(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

;; Usage
(unless false
  (println "This runs"))

;; Expands to:
(if (not false)
  (do (println "This runs")))

;; Check expansion
(macroexpand '(unless false (println "hi")))
```

**See Also**: Defmacro, Quote, Syntax Quote, Homoiconicity

### Map (Function)

**Definition**: Higher-order function that applies a function to each element of a collection, returning lazy sequence.

**Example**:

```clojure
(map inc [1 2 3])           ; (2 3 4)
(map + [1 2 3] [10 20 30])  ; (11 22 33)

;; Lazy - only computes when needed
(take 5 (map #(* % %) (range)))  ; (0 1 4 9 16)

;; With anonymous function
(map #(str "Hello, " %) ["Alice" "Bob"])
;; ("Hello, Alice" "Hello, Bob")
```

**See Also**: Filter, Reduce, Mapv, Higher-Order Function, Lazy Sequence

### Map (Data Structure)

**Definition**: Associative data structure with key-value pairs. Keys are typically keywords. Efficient lookup.

**Example**:

```clojure
;; Map literal
{:name "Alice" :age 30 :email "alice@example.com"}

;; Nested map
{:user {:name "Alice" :address {:city "NYC"}}}

;; Map operations
(get {:a 1 :b 2} :a)         ; 1
(:a {:a 1 :b 2})             ; 1 (keyword as fn)
({:a 1 :b 2} :a)             ; 1 (map as fn)

(assoc {:a 1} :b 2)          ; {:a 1, :b 2}
(dissoc {:a 1 :b 2} :b)      ; {:a 1}
(merge {:a 1} {:b 2})        ; {:a 1, :b 2}
```

**See Also**: Hash Map, Keyword, Assoc, Dissoc, Get

### Metadata

**Definition**: Data about data attached to symbols, collections, functions. Not part of value equality.

**Example**:

```clojure
;; Attach metadata
(def ^{:doc "A counter"} counter (atom 0))
(def ^:private helper [x] (* x x))

;; Read metadata
(meta #'counter)  ; {:doc "A counter", :line 1, ...}

;; with-meta
(def v (with-meta [1 2 3] {:source "api"}))
(meta v)  ; {:source "api"}

;; Metadata doesn't affect equality
(= [1 2 3] (with-meta [1 2 3] {:x 1}))  ; true
```

**See Also**: Symbol, Var, With-Meta, Meta

## N

### Namespace

**Definition**: Context for organizing and isolating vars. Prevents naming conflicts. Corresponds to file paths.

**Example**:

```clojure
;; Define namespace
(ns myapp.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; Require in REPL
(require '[clojure.string :as str])

;; Use qualified name
(clojure.string/upper-case "hello")

;; Use alias
(str/upper-case "hello")

;; Current namespace
*ns*  ; #namespace[user]
```

**See Also**: Require, Use, Import, Var, ns

### Nil

**Definition**: Represents absence of value or null. Logically false in conditionals. Only nil and false are falsy.

**Example**:

```clojure
nil                    ; nil

(if nil :yes :no)      ; :no (nil is falsy)
(nil? nil)             ; true
(nil? false)           ; false

(get {:a 1} :b)        ; nil (key not found)
(first [])             ; nil (empty sequence)
```

**See Also**: Boolean, Falsy, Truthy, Nothing

### Nth

**Definition**: Returns element at index n in indexed collection. Throws exception if out of bounds (or returns default).

**Example**:

```clojure
(nth [1 2 3] 0)        ; 1
(nth [1 2 3] 2)        ; 3

;; With default for out of bounds
(nth [1 2 3] 10 :not-found)  ; :not-found

;; Works on sequences too
(nth (range) 100)      ; 100
```

**See Also**: Get, First, Last, Indexed Collection

## P

### Persistent Data Structure

**Definition**: Immutable data structure that preserves previous versions when modified. Uses structural sharing for efficiency.

**Example**:

```clojure
(def v1 [1 2 3])
(def v2 (conj v1 4))   ; [1 2 3 4]

;; Both versions exist
v1  ; [1 2 3]
v2  ; [1 2 3 4]

;; Efficient - shares structure
;; Not copying entire vector
```

**See Also**: Immutability, Structural Sharing, Vector, Map, Set

### Predicate

**Definition**: Function that returns boolean (true/false). By convention, names end with `?`.

**Example**:

```clojure
(even? 4)              ; true
(odd? 5)               ; true
(nil? nil)             ; true
(empty? [])            ; true
(contains? #{1 2 3} 2) ; true

;; Custom predicate
(defn positive? [n]
  (> n 0))

(filter positive? [-1 0 1 2])  ; (1 2)
```

**See Also**: Boolean, Filter, Test, Conditional

### Promise

**Definition**: One-time container for value to be delivered later. Blocks on deref until value delivered.

**Example**:

```clojure
(def p (promise))

;; In another thread
(future (Thread/sleep 2000)
        (deliver p 42))

;; Blocks until delivered
@p  ; 42 (waits 2 seconds)
```

**See Also**: Future, Delay, Deref, Deliver, Concurrency

### Pure Function

**Definition**: Function with no side effects - same input always produces same output. Doesn't mutate state or interact with outside world.

**Example**:

```clojure
;; Pure - deterministic, no side effects
(defn add [a b]
  (+ a b))

(defn square [x]
  (* x x))

;; Not pure - side effect (I/O)
(defn log-and-add [a b]
  (println "Adding" a "and" b)  ; Side effect
  (+ a b))

;; Not pure - depends on external state
(defn add-to-counter [n]
  (+ n @counter))  ; Depends on atom
```

**See Also**: Side Effect, Referential Transparency, Immutability

## R

### Reader

**Definition**: Component that parses text into Clojure data structures. Can be extended with reader macros.

**Example**:

```clojure
;; Reader macros
#"pattern"             ; Regex
#(* % %)               ; Anonymous function
#{1 2 3}               ; Set
@atom-var              ; Deref
^{:key val}            ; Metadata

;; Reading strings
(read-string "(+ 1 2)")  ; (+ 1 2) - a list
```

**See Also**: Reader Macro, Syntax Quote, Eval

### Reduce

**Definition**: Accumulates result by applying function to accumulator and each element. Foundation for many operations.

**Example**:

```clojure
(reduce + [1 2 3 4])           ; 10
(reduce + 100 [1 2 3 4])       ; 110 (initial value)

;; Build a map
(reduce (fn [acc x] (assoc acc x (* x x)))
        {}
        [1 2 3 4])
;; {1 1, 2 4, 3 9, 4 16}

;; Find maximum
(reduce max [3 1 4 1 5 9])     ; 9
```

**See Also**: Map, Filter, Fold, Accumulator

### Ref

**Definition**: Reference type for coordinated, synchronous state changes using Software Transactional Memory (STM). Changes happen in transactions.

**Example**:

```clojure
(def account1 (ref 1000))
(def account2 (ref 500))

;; Coordinated update (atomic)
(dosync
  (alter account1 - 100)
  (alter account2 + 100))

@account1  ; 900
@account2  ; 600
```

**See Also**: Atom, Agent, STM, Dosync, Alter

### REPL

**Definition**: Read-Eval-Print Loop - interactive programming environment. Core workflow for Clojure development.

**Example**:

```clojure
user=> (+ 1 2 3)
6

user=> (def x 10)
#'user/x

user=> (* x 5)
50

user=> (doc map)
; Documentation for map function
```

**See Also**: Interactive Development, Eval, Read, Print

### Rest

**Definition**: Returns sequence of all elements except first. Returns empty sequence if 0 or 1 elements.

**Example**:

```clojure
(rest [1 2 3 4])       ; (2 3 4)
(rest [1])             ; ()
(rest [])              ; ()

;; With destructuring
(let [[first & rest] [1 2 3 4 5]]
  rest)                ; (2 3 4 5)
```

**See Also**: First, Next, Cons, Sequence

## S

### S-expression

**Definition**: Symbolic expression - nested list structure representing both code and data in Lisp languages.

**Example**:

```clojure
(+ 1 2 3)              ; S-expression
(if (> x 10) :big :small)

;; Nested s-expressions
(map (fn [x] (* x x))
     (filter even? [1 2 3 4 5]))

;; As data
'(+ 1 2 3)  ; A list
```

**See Also**: Lisp, Homoiconicity, Code as Data

### Sequence

**Definition**: Abstraction for sequential collections. Supports first/rest operations. Core to many Clojure functions.

**Example**:

```clojure
;; Many types are seqs
(seq [1 2 3])          ; (1 2 3)
(seq {:a 1 :b 2})      ; ([:a 1] [:b 2])
(seq "hello")          ; (\h \e \l \l \o)

;; Seq operations
(first (range 100))    ; 0
(rest (range 5))       ; (1 2 3 4)
(cons 0 (range 1 5))   ; (0 1 2 3 4)

;; Empty seq is nil
(seq [])               ; nil
```

**See Also**: First, Rest, Cons, Lazy Sequence, Collection

### Set

**Definition**: Unordered collection of unique values. Efficient membership testing. Can be used as function to test membership.

**Example**:

```clojure
#{1 2 3 4}             ; Set literal
(hash-set 1 2 3 4)     ; Create set

;; Duplicates ignored
#{1 2 2 3 3}           ; #{1 2 3}

;; Set as function
(#{:read :write} :read)  ; :read
(#{:read :write} :delete)  ; nil

;; Operations
(conj #{1 2} 3)        ; #{1 2 3}
(disj #{1 2 3} 2)      ; #{1 3}
(contains? #{1 2 3} 2) ; true
```

**See Also**: Hash Set, Sorted Set, Collection, Contains

### Side Effect

**Definition**: Observable interaction with world outside function - I/O, mutation, exceptions. Avoided in pure functions.

**Example**:

```clojure
;; Side effects
(println "Hello")      ; I/O
(swap! atom inc)       ; Mutation
(throw ex)             ; Exception

;; Pure (no side effects)
(+ 1 2)
(map inc [1 2 3])
(filter even? [1 2 3 4])
```

**See Also**: Pure Function, Immutability, Referential Transparency

### STM

**Definition**: Software Transactional Memory - coordination mechanism for refs. Guarantees atomic, consistent, isolated updates.

**Example**:

```clojure
(def account1 (ref 1000))
(def account2 (ref 500))

;; Transaction - all or nothing
(dosync
  (when (>= @account1 100)
    (alter account1 - 100)
    (alter account2 + 100)))

;; If transaction retried, all changes retried
```

**See Also**: Ref, Dosync, Alter, Concurrency, Transaction

### Structural Sharing

**Definition**: Optimization where modified data structures share structure with original. Enables efficient immutability.

**Example**:

```clojure
(def v1 [1 2 3 4 5 6 7 8])
(def v2 (conj v1 9))

;; v2 shares most structure with v1
;; Only new tail node created
;; Not copying entire vector

;; Appears as separate values
v1  ; [1 2 3 4 5 6 7 8]
v2  ; [1 2 3 4 5 6 7 8 9]
```

**See Also**: Persistent Data Structure, Immutability, Efficiency

### Swap

**Definition**: Updates atom by applying function to current value. Retries if value changed during update (compare-and-swap).

**Example**:

```clojure
(def counter (atom 0))

(swap! counter inc)           ; 1
(swap! counter + 10)          ; 11
(swap! counter (fn [n] (* n 2)))  ; 22

;; With extra args
(swap! counter - 5 2)         ; 15 (22 - 5 - 2)
```

**See Also**: Atom, Reset, Compare-and-Swap, State Management

### Symbol

**Definition**: Identifier used to refer to vars, functions, or as data. Evaluated to value it refers to.

**Example**:

```clojure
;; Symbol evaluation
(def x 10)
x                      ; 10 (symbol evaluates to value)

;; Quoted symbol (not evaluated)
'x                     ; x (symbol itself)

;; Namespaced symbol
'clojure.core/map      ; clojure.core/map

;; Gensym (generated unique symbol)
(gensym)               ; G__1234
(gensym "temp")        ; temp1235
```

**See Also**: Var, Keyword, Quote, Namespace

### Syntax Quote

**Definition**: Backtick `` ` `` - quotes form but allows selective evaluation with unquote. Namespace-qualifies symbols. Used in macros.

**Example**:

```clojure
`(+ 1 2)               ; (clojure.core/+ 1 2)

;; Unquote with ~
(let [x 5]
  `(+ 1 ~x))           ; (clojure.core/+ 1 5)

;; Unquote-splicing with ~@
`(list ~@[1 2 3])      ; (clojure.core/list 1 2 3)
```

**See Also**: Quote, Unquote, Macro, Reader

## T

### Threading Macro

**Definition**: Macro for linear data transformation pipelines. `->` threads as first arg, `->>` as last arg.

**Example**:

```clojure
;; -> (thread-first)
(-> 5
    (+ 3)
    (* 2)
    (- 1))             ; 15

;; ->> (thread-last)
(->> [1 2 3 4 5]
     (map inc)
     (filter even?)
     (reduce +))       ; 12

;; as-> (named threading)
(as-> [1 2 3] $
  (map inc $)
  (reduce + $))        ; 9
```

**See Also**: Comp, Pipeline, Macro

### Transducer

**Definition**: Composable transformation independent of input/output context. Efficient composition without intermediate collections.

**Example**:

```clojure
;; Regular composition (creates intermediates)
(->> (range 100)
     (map inc)
     (filter even?)
     (take 5))

;; Transducer (no intermediates)
(def xf (comp
          (map inc)
          (filter even?)
          (take 5)))

(into [] xf (range 100))    ; [2 4 6 8 10]
(sequence xf (range 100))   ; (2 4 6 8 10)
```

**See Also**: Map, Filter, Comp, Sequence

## V

### Var

**Definition**: Mutable reference to immutable value. Created with def. Provides namespace and name.

**Example**:

```clojure
(def x 10)             ; Creates var #'user/x

;; Var metadata
(meta #'x)

;; Alter root binding
(alter-var-root #'x inc)

;; Dynamic vars
(def ^:dynamic *config* {:env "dev"})
```

**See Also**: Def, Symbol, Namespace, Dynamic Var

### Vector

**Definition**: Indexed, immutable sequential collection. Fast random access and append. Literal syntax `[...]`.

**Example**:

```clojure
[1 2 3 4]              ; Vector literal
(vector 1 2 3 4)       ; Constructor

;; Efficient operations
(nth [1 2 3] 1)        ; 2 (fast index access)
(conj [1 2 3] 4)       ; [1 2 3 4] (fast append)

;; Vector as function (index lookup)
([1 2 3] 1)            ; 2

;; Nesting
[[1 2] [3 4] [5 6]]
```

**See Also**: List, Sequence, Nth, Conj, Collection

## Learn More

**Comprehensive Documentation**:

- [Quick Start](/en/learn/software-engineering/programming-languages/clojure/tutorials/quick-start) - Overview of key features
- [Beginner Tutorial](/en/learn/software-engineering/programming-languages/clojure/tutorials/beginner) - Detailed explanations
- [How-To Guides](/en/learn/software-engineering/programming-languages/clojure/how-to) - Practical usage examples
- [Cheat Sheet](/en/learn/software-engineering/programming-languages/clojure/reference/cheat-sheet) - Quick syntax reference

**Official Resources**:

- [Clojure Official Documentation](https://clojure.org/) - Complete language reference
- [ClojureDocs](https://clojuredocs.org/) - Community-powered examples
- [Clojure Cheatsheet](https://clojure.org/api/cheatsheet) - Interactive reference
