---
title: "Cheat Sheet"
date: 2025-12-30T14:13:06+07:00
draft: false
weight: 1000030
description: Quick reference for Clojure syntax, core functions, macros, and functional patterns
---

**Quick reference guide** for essential Clojure syntax and patterns. Copy-paste ready snippets for daily functional programming.

## Basic Syntax

### Values and Literals

```clojure
;; Numbers
42                              ; Integer
3.14                            ; Double
22/7                            ; Ratio
0x2A                            ; Hex (42)
2r101010                        ; Binary (42)

;; Strings
"Hello, Clojure!"               ; String
\a                              ; Character
\newline                        ; Special character

;; Booleans and nil
true
false
nil                             ; Null/nothing

;; Keywords (self-evaluating identifiers)
:name                           ; Keyword
:user/id                        ; Namespaced keyword

;; Symbols
'name                           ; Symbol (quoted)
```

### Comments

```clojure
;; Single-line comment

(comment
  ; Code that won't execute
  ; Useful for documentation and experiments
  (+ 1 2 3))

#_ (ignored-expression)         ; Reader macro - ignores next form
```

### Data Structures

```clojure
;; Lists (linked list, evaluated as function calls)
'(1 2 3 4)                      ; Quoted list
(list 1 2 3 4)                  ; Using list function

;; Vectors (indexed collection)
[1 2 3 4]                       ; Vector literal
(vector 1 2 3 4)                ; Using vector function

;; Maps (key-value pairs)
{:name "Alice" :age 30}         ; Map literal
(hash-map :name "Alice" :age 30)

;; Sets (unique values)
#{1 2 3 4}                      ; Set literal
(hash-set 1 2 3 4)

;; Nested structures
{:user {:name "Alice"
        :contacts ["bob@example.com" "charlie@example.com"]}
 :roles #{:admin :user}}
```

## Vars and Bindings

### Def and Local Bindings

```clojure
;; Define global var
(def x 42)
(def greeting "Hello")

;; Define function
(defn add [a b]
  (+ a b))

;; Local bindings with let
(let [x 10
      y 20
      z (+ x y)]
  (* z 2))                      ; 60

;; Destructuring in let
(let [[a b c] [1 2 3]]
  (+ a b c))                    ; 6

(let [{:keys [name age]} {:name "Alice" :age 30}]
  (str name " is " age))        ; "Alice is 30"
```

### Dynamic Binding

```clojure
;; Dynamic var (can be thread-locally rebound)
(def ^:dynamic *config* {:env "dev"})

;; Temporarily rebind
(binding [*config* {:env "prod"}]
  (println (:env *config*)))    ; "prod"

(println (:env *config*))       ; "dev" (restored)
```

## Functions

### Function Definition

```clojure
;; Named function
(defn greet [name]
  (str "Hello, " name "!"))

;; Multiple arity
(defn greet
  ([] (greet "World"))
  ([name] (str "Hello, " name "!")))

;; Variadic function (rest args)
(defn sum [& numbers]
  (reduce + numbers))

(sum 1 2 3 4 5)                 ; 15

;; Anonymous function
(fn [x] (* x x))                ; Long form
#(* % %)                        ; Short form
#(* %1 %2)                      ; Multiple args

;; Using anonymous functions
(map #(* % %) [1 2 3 4])        ; [1 4 9 16]
```

### Function Composition

```clojure
;; comp - right to left composition
(def parse-int-and-inc
  (comp inc #(Integer/parseInt %)))

(parse-int-and-inc "42")        ; 43

;; partial - partial application
(def add10 (partial + 10))
(add10 5)                       ; 15

;; juxt - apply multiple functions, return vector of results
(def stats (juxt count #(reduce + %) #(reduce * %)))
(stats [1 2 3 4])               ; [4 10 24]
```

## Core Functions

### Collections

```clojure
;; Sequence operations
(first [1 2 3])                 ; 1
(rest [1 2 3])                  ; (2 3)
(last [1 2 3])                  ; 3
(butlast [1 2 3])               ; (1 2)

(cons 0 [1 2 3])                ; (0 1 2 3)
(conj [1 2 3] 4)                ; [1 2 3 4] - adds to end for vectors
(conj '(1 2 3) 4)               ; (4 1 2 3) - adds to front for lists

;; Vector-specific
(nth [1 2 3] 1)                 ; 2 (index access)
(get [1 2 3] 1)                 ; 2
(assoc [1 2 3] 1 99)            ; [1 99 3]

;; Map operations
(get {:name "Alice"} :name)     ; "Alice"
(:name {:name "Alice"})         ; "Alice" (keywords as functions)
({:name "Alice"} :name)         ; "Alice" (maps as functions)

(assoc {:a 1} :b 2)             ; {:a 1, :b 2}
(dissoc {:a 1 :b 2} :b)         ; {:a 1}
(merge {:a 1} {:b 2})           ; {:a 1, :b 2}

(keys {:a 1 :b 2})              ; (:a :b)
(vals {:a 1 :b 2})              ; (1 2)

;; Set operations
(conj #{1 2 3} 4)               ; #{1 2 3 4}
(disj #{1 2 3 4} 3)             ; #{1 2 4}
(contains? #{1 2 3} 2)          ; true
```

### Transformations

```clojure
;; map - transform each element
(map inc [1 2 3])               ; (2 3 4)
(map + [1 2 3] [10 20 30])      ; (11 22 33)

;; filter - keep elements matching predicate
(filter even? [1 2 3 4 5])      ; (2 4)
(filter #(> % 10) [5 10 15 20]) ; (15 20)

;; remove - opposite of filter
(remove even? [1 2 3 4 5])      ; (1 3 5)

;; reduce - accumulate over collection
(reduce + [1 2 3 4])            ; 10
(reduce + 100 [1 2 3 4])        ; 110 (with initial value)

(reduce (fn [acc x] (+ acc (* x x)))
        0
        [1 2 3 4])              ; 30 (sum of squares)

;; take and drop
(take 3 [1 2 3 4 5])            ; (1 2 3)
(drop 2 [1 2 3 4 5])            ; (3 4 5)
(take-while #(< % 5) [1 2 3 4 5 6]) ; (1 2 3 4)

;; partition
(partition 2 [1 2 3 4 5 6])     ; ((1 2) (3 4) (5 6))
(partition-all 2 [1 2 3 4 5])   ; ((1 2) (3 4) (5))
```

### Higher-Order Functions

```clojure
;; apply - apply function to collection as args
(apply + [1 2 3 4])             ; 10
(apply str ["Hello" " " "World"]) ; "Hello World"

;; mapv - eager map returning vector
(mapv inc [1 2 3])              ; [2 3 4]

;; keep - map and remove nils
(keep #(when (even? %) %) [1 2 3 4 5]) ; (2 4)

;; group-by
(group-by even? [1 2 3 4 5])    ; {false [1 3 5], true [2 4]}

;; sort and sort-by
(sort [3 1 4 1 5])              ; (1 1 3 4 5)
(sort-by :age [{:name "Alice" :age 30}
               {:name "Bob" :age 25}]) ; Bob first
```

## Threading Macros

### Thread-First and Thread-Last

```clojure
;; -> (thread-first) - thread as first argument
(-> 5
    (+ 3)
    (* 2)
    (- 1))                      ; 15
;; Equivalent: (- (* (+ 5 3) 2) 1)

;; ->> (thread-last) - thread as last argument
(->> [1 2 3 4 5]
     (map inc)
     (filter even?)
     (reduce +))                ; 12

;; as-> - thread with custom position
(as-> [1 2 3] $
  (map inc $)
  (filter even? $)
  (reduce + $))                 ; 6

;; cond-> - conditional threading
(cond-> []
  true (conj 1)
  (even? 2) (conj 2)
  false (conj 3))               ; [1 2]
```

### Useful Threading Patterns

```clojure
;; Data transformation pipeline
(->> (range 1 11)
     (filter odd?)
     (map #(* % %))
     (reduce +))                ; 165 (sum of squares of odds 1-9)

;; Building data structure
(-> {}
    (assoc :name "Alice")
    (assoc :age 30)
    (update :age inc))          ; {:name "Alice", :age 31}
```

## Conditionals

### Basic Conditionals

```clojure
;; if
(if (even? 4)
  "Even"
  "Odd")                        ; "Even"

;; when (no else, implicit do)
(when (pos? 5)
  (println "Positive")
  :positive)

;; if-let (bind and test)
(if-let [x (get {:a 1} :a)]
  (str "Found: " x)
  "Not found")                  ; "Found: 1"

;; when-let
(when-let [x (get {:a 1} :a)]
  (println "Value:" x)
  (* x 2))                      ; 2

;; cond (multiple conditions)
(cond
  (< x 0) "Negative"
  (= x 0) "Zero"
  (> x 0) "Positive"
  :else "Unknown")

;; case (constant matching, efficient)
(case x
  1 "One"
  2 "Two"
  3 "Three"
  "Other")

;; condp (compare with predicate)
(condp = x
  1 "One"
  2 "Two"
  3 "Three"
  "Other")
```

## Destructuring

### Sequential Destructuring

```clojure
;; Vector destructuring
(let [[a b c] [1 2 3]]
  (+ a b c))                    ; 6

;; Rest args
(let [[first & rest] [1 2 3 4 5]]
  {:first first :rest rest})    ; {:first 1, :rest (2 3 4 5)}

;; :as to keep original
(let [[a b :as all] [1 2 3]]
  {:a a :b b :all all})         ; {:a 1, :b 2, :all [1 2 3]}
```

### Associative Destructuring

```clojure
;; Map destructuring with :keys
(let [{:keys [name age]} {:name "Alice" :age 30}]
  (str name " is " age))        ; "Alice is 30"

;; With :or for defaults
(let [{:keys [name age] :or {age 0}} {:name "Bob"}]
  {:name name :age age})        ; {:name "Bob", :age 0}

;; Nested destructuring
(let [{:keys [user]
       {:keys [name email]} :user}
      {:user {:name "Alice" :email "alice@example.com"}}]
  (str name ": " email))

;; String keys with :strs
(let [{:strs [name age]} {"name" "Alice" "age" "30"}]
  (str name " is " age))

;; Symbol keys with :syms
(let [{:syms [x y]} {'x 10 'y 20}]
  (+ x y))                      ; 30
```

## Sequences and Laziness

### Lazy Sequences

```clojure
;; range - lazy sequence
(range 10)                      ; (0 1 2 3 4 5 6 7 8 9)
(range 5 10)                    ; (5 6 7 8 9)
(range 0 10 2)                  ; (0 2 4 6 8)

;; repeat - infinite repetition
(take 5 (repeat :x))            ; (:x :x :x :x :x)
(repeat 3 :x)                   ; (:x :x :x)

;; repeatedly - infinite evaluation
(take 3 (repeatedly #(rand-int 10))) ; (7 2 9) - random

;; iterate - infinite sequence by repeated application
(take 5 (iterate inc 0))        ; (0 1 2 3 4)
(take 4 (iterate #(* 2 %) 1))   ; (1 2 4 8)

;; cycle - infinite repetition of collection
(take 7 (cycle [1 2 3]))        ; (1 2 3 1 2 3 1)

;; lazy-seq - create custom lazy sequence
(defn fib-seq
  ([] (fib-seq 0 1))
  ([a b] (lazy-seq (cons a (fib-seq b (+ a b))))))

(take 10 (fib-seq))             ; (0 1 1 2 3 5 8 13 21 34)
```

### Realizing Sequences

```clojure
;; doall - realize entire sequence, return it
(doall (map println [1 2 3]))

;; dorun - realize for side effects, return nil
(dorun (map println [1 2 3]))

;; Force realization with vec or into
(vec (map inc [1 2 3]))         ; [2 3 4]
(into [] (map inc [1 2 3]))     ; [2 3 4]
```

## State Management

### Atoms (Synchronous, Independent)

```clojure
;; Create atom
(def counter (atom 0))

;; Deref (get value)
@counter                        ; 0
(deref counter)                 ; 0

;; swap! - update with function
(swap! counter inc)             ; 1
(swap! counter + 10)            ; 11

;; reset! - set value directly
(reset! counter 0)              ; 0

;; Complex state
(def user (atom {:name "Alice" :age 30}))
(swap! user assoc :email "alice@example.com")
(swap! user update :age inc)
```

### Refs (Synchronous, Coordinated - STM)

```clojure
;; Create refs
(def account1 (ref 1000))
(def account2 (ref 500))

;; dosync - transaction
(dosync
  (alter account1 - 100)
  (alter account2 + 100))

@account1                       ; 900
@account2                       ; 600

;; ref-set - set value in transaction
(dosync
  (ref-set account1 1000))
```

### Agents (Asynchronous, Independent)

```clojure
;; Create agent
(def logger (agent []))

;; send - asynchronous update
(send logger conj "Log entry 1")
(send logger conj "Log entry 2")

;; Wait for completion
(await logger)
@logger                         ; ["Log entry 1" "Log entry 2"]

;; send-off - for blocking operations
(send-off logger (fn [logs]
                   (Thread/sleep 1000)
                   (conj logs "Delayed")))
```

### Vars (Thread-Local, Dynamic)

```clojure
;; def creates root binding
(def ^:dynamic *db-connection* nil)

;; binding creates thread-local binding
(binding [*db-connection* "postgresql://..."]
  (println *db-connection*))    ; "postgresql://..."
```

## Namespaces

### Basic Namespace Operations

```clojure
;; Define namespace
(ns myapp.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:import [java.util Date UUID]))

;; Require in REPL
(require '[clojure.string :as str])
(str/upper-case "hello")        ; "HELLO"

;; Use (import into current namespace)
(use 'clojure.string)
(upper-case "hello")            ; "HELLO" (not recommended)

;; Import Java classes
(import 'java.util.Date)
(Date.)

;; Refer specific vars
(require '[clojure.string :refer [upper-case lower-case]])
(upper-case "hello")            ; "HELLO"
```

### Private Vars

```clojure
;; Private function (not accessible from other namespaces)
(defn- private-helper [x]
  (* x x))

;; Public function using private
(defn square-sum [a b]
  (+ (private-helper a)
     (private-helper b)))
```

## Macros

### Common Macros

```clojure
;; defmacro - define macro
(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

(unless false
  (println "This runs")
  :done)                        ; prints and returns :done

;; Quote and unquote
`(+ 1 2)                        ; (clojure.core/+ 1 2) - syntax quote
'(+ 1 2)                        ; (+ 1 2) - regular quote
`(+ 1 ~(+ 1 1))                 ; (clojure.core/+ 1 2) - unquote

;; Unquote-splicing
`(list ~@[1 2 3])               ; (clojure.core/list 1 2 3)

;; time - measure execution time
(time
  (reduce + (range 1000000)))   ; Prints elapsed time, returns result

;; assert - runtime assertion
(assert (pos? 5) "Must be positive")

;; -> and ->> are macros (threading)
(macroexpand '(-> 5 inc (* 2))) ; (* (inc 5) 2)
```

## Java Interop

### Calling Java Methods

```clojure
;; Create instance
(new String "Hello")            ; "Hello"
(String. "Hello")               ; "Hello" (preferred)

;; Instance method
(.toUpperCase "hello")          ; "HELLO"
(.length "hello")               ; 5

;; Static method
(Math/pow 2 8)                  ; 256.0
(System/currentTimeMillis)      ; 1735542786000

;; Chained methods
(.. "hello"
    (toUpperCase)
    (substring 0 3))            ; "HEL"

;; doto - call multiple methods
(doto (java.util.ArrayList.)
  (.add "one")
  (.add "two")
  (.add "three"))               ; ArrayList with 3 elements

;; Access field
(.-x (java.awt.Point. 10 20))   ; 10

;; Set field
(set! (.-x point) 30)
```

### Java Collections Interop

```clojure
;; Convert Clojure → Java
(into-array String ["a" "b" "c"])
(java.util.ArrayList. [1 2 3])

;; Convert Java → Clojure
(vec (java.util.ArrayList. [1 2 3])) ; [1 2 3]
(into {} (java.util.HashMap. {"a" 1})) ; {"a" 1}
```

## Error Handling

### Try-Catch-Finally

```clojure
;; Basic try-catch
(try
  (/ 10 0)
  (catch ArithmeticException e
    (println "Cannot divide by zero")
    :error))

;; Multiple catch clauses
(try
  (Integer/parseInt "not-a-number")
  (catch NumberFormatException e
    (println "Parse error"))
  (catch Exception e
    (println "General error"))
  (finally
    (println "Cleanup")))

;; Throw exception
(throw (Exception. "Something went wrong"))

;; Custom exception
(defn validate-positive [x]
  (when-not (pos? x)
    (throw (IllegalArgumentException. "Must be positive")))
  x)
```

## String Operations

```clojure
(require '[clojure.string :as str])

;; String functions
(str/upper-case "hello")        ; "HELLO"
(str/lower-case "HELLO")        ; "hello"
(str/capitalize "hello")        ; "Hello"

(str/trim "  hello  ")          ; "hello"
(str/triml "  hello")           ; "hello"
(str/trimr "hello  ")           ; "hello"

(str/split "a,b,c" #",")        ; ["a" "b" "c"]
(str/join ", " ["a" "b" "c"])   ; "a, b, c"

(str/replace "hello" #"l" "L")  ; "heLLo"
(str/replace-first "hello" #"l" "L") ; "heLlo"

(str/starts-with? "hello" "he") ; true
(str/ends-with? "hello" "lo")   ; true
(str/includes? "hello" "ell")   ; true

(str/blank? "")                 ; true
(str/blank? "  ")               ; true
```

## Regular Expressions

```clojure
;; Regex literal
#"pattern"

;; re-find - first match
(re-find #"\d+" "abc123def")    ; "123"

;; re-seq - all matches
(re-seq #"\d+" "a1b2c3")        ; ("1" "2" "3")

;; re-matches - full string match
(re-matches #"\d+" "123")       ; "123"
(re-matches #"\d+" "a123")      ; nil

;; re-pattern - create regex from string
(re-pattern "\\d+")             ; #"\d+"

;; Groups
(re-find #"(\w+)@(\w+)" "user@example")
;; ["user@example" "user" "example"]
```

## Learn More

**Comprehensive Documentation**:

- [Quick Start](/en/learn/software-engineering/programming-languages/clojure/tutorials/quick-start) - 12 Clojure touchpoints
- [Beginner Tutorial](/en/learn/software-engineering/programming-languages/clojure/tutorials/beginner) - Comprehensive fundamentals
- [Cookbook](/en/learn/software-engineering/programming-languages/clojure/how-to/cookbook) - Practical recipes
- [How-To Guides](/en/learn/software-engineering/programming-languages/clojure/how-to) - Problem-solving guides

**Official Resources**:

- [Clojure Official Documentation](https://clojure.org/) - Language reference and guides
- [ClojureDocs](https://clojuredocs.org/) - Community-powered documentation with examples
- [Clojure Cheatsheet](https://clojure.org/api/cheatsheet) - Interactive reference
