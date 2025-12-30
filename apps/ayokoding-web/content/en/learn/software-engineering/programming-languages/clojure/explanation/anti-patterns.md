---
title: "Anti Patterns"
date: 2025-12-30T14:05:50+07:00
draft: false
weight: 1000040
description: "Recognizing and avoiding common Clojure anti-patterns that violate functional programming principles"
tags: ["clojure", "anti-patterns", "functional-programming", "refactoring"]
categories: ["learn"]
---

## Overview

Anti-patterns in Clojure often stem from bringing object-oriented habits into a functional language. Recognizing these patterns helps you write idiomatic Clojure that leverages immutability, simplicity, and the power of functional composition.

## State Management Anti-Patterns

### Atom Overuse

Using atoms everywhere instead of pure functions and immutable data.

**Why it's bad:**

- Breaks referential transparency
- Makes testing difficult (shared mutable state)
- Hides data flow
- Prevents composition
- Loses time-travel debugging

**Example:**

```clojure
;; ❌ Atom overuse - stateful when unnecessary
(def user-count (atom 0))
(def total-revenue (atom 0.0))
(def processed-orders (atom []))

(defn process-order [order]
  (swap! user-count inc)
  (swap! total-revenue + (:amount order))
  (swap! processed-orders conj order)
  order)

;; Cannot test without global state
;; Cannot parallelize
;; Order of execution matters
```

**Solution:**

```clojure
;; ✅ Pure functions - stateless and composable
(defn calculate-metrics [orders]
  {:user-count (count (distinct (map :user-id orders)))
   :total-revenue (reduce + (map :amount orders))
   :processed-orders orders})

;; Easy to test
(calculate-metrics [{:user-id 1 :amount 100}
                    {:user-id 2 :amount 200}])
;; => {:user-count 2, :total-revenue 300, :processed-orders [...]}

;; State management at system boundary only
(defonce system-state (atom {:orders []}))

(defn update-system! [order]
  (swap! system-state update :orders conj order))

;; Core logic remains pure
(defn process-order [order]
  (-> order
      validate-order
      calculate-total
      apply-discounts))
```

**When atoms are appropriate:**

- Application state (web server, database connections)
- Cache management
- Coordinating system lifecycle
- UI state in frontend applications

### Global Mutable State

Scattered def forms creating hidden global state.

**Why it's bad:**

- Hidden dependencies
- Impossible to isolate for testing
- Race conditions in concurrent code
- Violates functional principles
- Makes code unpredictable

**Example:**

```clojure
;; ❌ Global mutable state - hidden dependencies
(def current-user nil)
(def session-token nil)

(defn authenticate! [username password]
  (let [user (db/find-user username password)
        token (generate-token user)]
    (def current-user user)
    (def session-token token)
    user))

(defn get-user-orders []
  ;; Hidden dependency on global state
  (db/query-orders (:id current-user)))

;; Cannot test without side effects
;; Cannot run concurrent requests
;; Race conditions guaranteed
```

**Solution:**

```clojure
;; ✅ Explicit state passing
(defn authenticate [username password]
  (when-let [user (db/find-user username password)]
    {:user user
     :token (generate-token user)}))

(defn get-user-orders [user-id]
  (db/query-orders user-id))

;; System state management
(defrecord AppState [users sessions])

(defn create-session [state username password]
  (when-let [auth (authenticate username password)]
    (update state :sessions assoc (:token auth) auth)))

(defn get-orders-for-session [state session-token]
  (when-let [session (get-in state [:sessions session-token])]
    (get-user-orders (get-in session [:user :id]))))

;; State at boundaries
(defonce system (atom (AppState. {} {})))

(defn handle-login! [username password]
  (swap! system create-session username password))
```

## Object-Oriented Patterns in Functional Code

### Excessive Use of defrecord

Using defrecord for everything instead of plain maps.

**Why it's bad:**

- Premature optimization
- Less flexible than maps
- Cannot add keys dynamically
- Couples code to specific structure
- Breaks REPL-driven development

**Example:**

```clojure
;; ❌ defrecord everywhere - unnecessary rigidity
(defrecord User [id name email created-at])
(defrecord Order [id user-id items total created-at])
(defrecord Product [id name price inventory])

;; Cannot extend dynamically
(def user (User. 1 "Alice" "alice@example.com" (java.util.Date.)))

;; Want to add temporary field? Can't
(assoc user :temporary-token "abc123")
;; => User has extra fields that don't print

;; Protocols everywhere
(defprotocol Processable
  (process [this]))

(extend-type Order
  Processable
  (process [order]
    ;; ...
    ))
```

**Solution:**

```clojure
;; ✅ Plain maps - flexible and simple
(defn user [id name email]
  {:user/id id
   :user/name name
   :user/email email
   :user/created-at (java.time.Instant/now)})

(defn order [user-id items]
  {:order/id (random-uuid)
   :order/user-id user-id
   :order/items items
   :order/total (calculate-total items)
   :order/created-at (java.time.Instant/now)})

;; Extend dynamically
(-> (user 1 "Alice" "alice@example.com")
    (assoc :temporary-token "abc123"))

;; Multimethods for polymorphism
(defmulti process :type)

(defmethod process :order [entity]
  ;; Process order
  )

(defmethod process :payment [entity]
  ;; Process payment
  )

;; Use defrecord only when:
;; - Performance critical (profiling proves it)
;; - Protocol implementation needed
;; - Type dispatch required
```

### Deep Nesting Instead of Threading

Nested function calls that obscure data flow.

**Why it's bad:**

- Hard to read (inside-out)
- Difficult to debug
- Cannot easily reorder steps
- Violates visual flow

**Example:**

```clojure
;; ❌ Deeply nested - reads inside-out
(defn process-text [text]
  (clojure.string/join " "
    (map clojure.string/capitalize
      (filter #(> (count %) 2)
        (map clojure.string/trim
          (clojure.string/split text #","))))))

;; ❌ Intermediate bindings - verbose
(defn process-text [text]
  (let [parts (clojure.string/split text #",")
        trimmed (map clojure.string/trim parts)
        filtered (filter #(> (count %) 2) trimmed)
        capitalized (map clojure.string/capitalize filtered)
        result (clojure.string/join " " capitalized)]
    result))
```

**Solution:**

```clojure
;; ✅ Threading macros - reads top-to-bottom
(defn process-text [text]
  (->> (clojure.string/split text #",")
       (map clojure.string/trim)
       (filter #(> (count %) 2))
       (map clojure.string/capitalize)
       (clojure.string/join " ")))

;; ✅ Thread-first for transformations
(defn update-user [user changes]
  (-> user
      (merge changes)
      (assoc :updated-at (java.time.Instant/now))
      (update :version inc)))

;; ✅ Conditional threading
(defn prepare-response [data request]
  (cond-> {:status 200 :body data}
    (:format request) (assoc :format (:format request))
    (:compress request) (update :body compress)))
```

## Lazy Sequence Pitfalls

### Lazy I/O Side Effects

Performing I/O operations in lazy sequences.

**Why it's bad:**

- Resource leaks (files left open)
- Unpredictable execution timing
- Errors far from source
- Cannot control when side effects happen

**Example:**

```clojure
;; ❌ Lazy I/O - resource leak waiting to happen
(defn read-user-files []
  (for [filename (list-user-files)]
    (slurp filename))) ;; File opened lazily, never closed

;; Caller might not realize all files
(def user-data (read-user-files))

;; Files still open! Leak!
(take 5 user-data) ;; Only 5 files read, others leaked

;; ❌ Side effects in lazy seq
(defn process-files [filenames]
  (map (fn [filename]
         (println "Processing" filename) ;; When does this execute?
         (-> filename
             slurp
             parse-data))
       filenames))

;; Side effect doesn't happen yet
(def results (process-files ["a.txt" "b.txt"]))

;; Side effect happens... sometime later
(first results) ;; Prints "Processing a.txt"
```

**Solution:**

```clojure
;; ✅ Eager resource management
(defn read-user-files []
  (mapv slurp (list-user-files))) ;; mapv forces evaluation

;; ✅ Explicit resource handling
(defn process-files [filenames]
  (into []
        (map (fn [filename]
               (with-open [rdr (clojure.java.io/reader filename)]
                 (parse-data (slurp rdr)))))
        filenames))

;; ✅ Separate pure logic from I/O
(defn parse-file [content]
  ;; Pure function
  (parse-data content))

(defn read-and-parse [filenames]
  (for [filename filenames]
    (with-open [rdr (clojure.java.io/reader filename)]
      (parse-file (slurp rdr)))))

;; Force evaluation with doall/dorun
(defn process-with-side-effects! [items]
  (doall (map process-item! items))) ;; Forces evaluation
```

### Holding Onto Head

Retaining reference to start of lazy sequence while processing it.

**Why it's bad:**

- Memory leak (entire sequence held in memory)
- Defeats laziness benefits
- OutOfMemoryError on large data
- Slow performance

**Example:**

```clojure
;; ❌ Holding head - memory leak
(defn process-large-file [filename]
  (let [lines (line-seq (clojure.java.io/reader filename))]
    ;; Head of sequence retained
    (println "First line:" (first lines))
    ;; All lines kept in memory
    (->> lines
         (filter important?)
         (map process-line)
         doall)))

;; ❌ Returning lazy seq with closed resource
(defn read-file-lines [filename]
  (let [rdr (clojure.java.io/reader filename)]
    (line-seq rdr))) ;; Returns lazy seq, but rdr gets closed!

;; Caller gets error
(first (read-file-lines "data.txt"))
;; => IOException: Stream closed
```

**Solution:**

```clojure
;; ✅ Don't hold head
(defn process-large-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (let [first-line (-> rdr line-seq first)]
      (println "First line:" first-line))
    ;; Don't retain first-line reference
    (->> (line-seq rdr)
         (filter important?)
         (map process-line)
         doall))) ;; Force in scope of with-open

;; ✅ Return realized collection
(defn read-file-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr)))) ;; Force before closing

;; ✅ Process with transducers
(defn process-large-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (into []
          (comp (filter important?)
                (map process-line))
          (line-seq rdr))))
```

## Error Handling Anti-Patterns

### Swallowing Exceptions

Catching exceptions without handling or logging.

**Why it's bad:**

- Errors disappear silently
- Debugging becomes impossible
- Corrupted state continues
- Users get no feedback

**Example:**

```clojure
;; ❌ Swallowed exception
(defn load-config [filename]
  (try
    (-> filename
        slurp
        edn/read-string)
    (catch Exception e
      {}))) ;; Returns empty map, error lost

;; ❌ Generic exception catching
(defn process-payment [payment]
  (try
    (charge-credit-card payment)
    (send-receipt payment)
    (update-inventory payment)
    (catch Exception e
      nil))) ;; All errors treated same
```

**Solution:**

```clojure
;; ✅ Specific error handling
(defn load-config [filename]
  (try
    (-> filename
        slurp
        edn/read-string)
    (catch java.io.FileNotFoundException e
      (log/warn "Config file not found, using defaults" filename)
      default-config)
    (catch Exception e
      (log/error e "Failed to load config" filename)
      (throw (ex-info "Configuration error"
                      {:filename filename
                       :cause :parse-failure}
                      e)))))

;; ✅ Context-rich exceptions
(defn process-payment [payment]
  (try
    (charge-credit-card payment)
    (send-receipt payment)
    (update-inventory payment)
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (case (:type data)
          :insufficient-funds
          {:error "Payment declined" :reason "Insufficient funds"}

          :invalid-card
          {:error "Payment declined" :reason "Invalid card"}

          ;; Rethrow unknown
          (throw e))))))
```

### String-Based Error Messages

Using plain strings instead of structured error data.

**Why it's bad:**

- Cannot handle programmatically
- No context for debugging
- Difficult to test
- Poor error recovery

**Example:**

```clojure
;; ❌ String exceptions - no context
(defn create-user [username email]
  (cond
    (clojure.string/blank? username)
    (throw (Exception. "Username required"))

    (not (valid-email? email))
    (throw (Exception. "Invalid email"))

    (user-exists? username)
    (throw (Exception. "User already exists"))))

;; Caller must parse strings
(try
  (create-user "" "invalid")
  (catch Exception e
    (if (= "Username required" (.getMessage e))
      ;; Fragile string matching
      (handle-username-error)
      (handle-other-error))))
```

**Solution:**

```clojure
;; ✅ Structured errors with ex-info
(defn create-user [username email]
  (cond
    (clojure.string/blank? username)
    (throw (ex-info "Validation failed"
                    {:type :validation-error
                     :field :username
                     :reason :required}))

    (not (valid-email? email))
    (throw (ex-info "Validation failed"
                    {:type :validation-error
                     :field :email
                     :reason :invalid-format
                     :value email}))

    (user-exists? username)
    (throw (ex-info "User creation failed"
                    {:type :duplicate-user
                     :username username}))))

;; Programmatic handling
(try
  (create-user username email)
  (catch clojure.lang.ExceptionInfo e
    (let [data (ex-data e)]
      (case (:type data)
        :validation-error
        {:error "Validation failed"
         :field (:field data)
         :reason (:reason data)}

        :duplicate-user
        {:error "Username taken"
         :username (:username data)}

        (throw e)))))

;; ✅ Return Either/Result type
(defn create-user [username email]
  (cond
    (clojure.string/blank? username)
    {:error {:type :validation :field :username}}

    (not (valid-email? email))
    {:error {:type :validation :field :email}}

    (user-exists? username)
    {:error {:type :duplicate :username username}}

    :else
    {:ok (save-user username email)}))

;; Pattern matching on result
(let [result (create-user "alice" "alice@example.com")]
  (if (:error result)
    (handle-error (:error result))
    (handle-success (:ok result))))
```

## Performance Anti-Patterns

### Premature Transients

Using transients everywhere without profiling.

**Why it's bad:**

- Premature optimization
- More complex code
- Marginal performance gains
- Breaks immutability benefits

**Example:**

```clojure
;; ❌ Transients everywhere - premature optimization
(defn process-items [items]
  (persistent!
   (reduce (fn [acc item]
             (assoc! acc (:id item) (process-item item)))
           (transient {})
           items)))

;; For small collections, immutable is fine
(process-items [{:id 1} {:id 2} {:id 3}])
```

**Solution:**

```clojure
;; ✅ Start with immutable
(defn process-items [items]
  (reduce (fn [acc item]
            (assoc acc (:id item) (process-item item)))
          {}
          items))

;; ✅ Use transients ONLY after profiling shows bottleneck
(defn process-large-batch [items]
  ;; Only when processing 10,000+ items AND profiling shows benefit
  (persistent!
   (reduce (fn [acc item]
             (assoc! acc (:id item) (process-item item)))
           (transient {})
           items)))
```

### Inefficient Collection Operations

Using wrong collection or operation for access pattern.

**Why it's bad:**

- Unnecessary O(n) operations
- Poor performance on large data
- Wastes memory

**Example:**

```clojure
;; ❌ Linear search on list
(def users (list {:id 1 :name "Alice"}
                 {:id 2 :name "Bob"}))

(defn find-user [id]
  (first (filter #(= id (:id %)) users))) ;; O(n)

;; ❌ Repeated concatenation
(defn build-list [items]
  (loop [result []
         items items]
    (if (empty? items)
      result
      (recur (concat result [(first items)])  ;; O(n) each iteration
             (rest items)))))

;; ❌ Contains? on vector
(def ids [1 2 3 4 5])
(contains? ids 3) ;; => false! (checks index, not value)
```

**Solution:**

```clojure
;; ✅ Hash map for lookups
(def users {1 {:id 1 :name "Alice"}
            2 {:id 2 :name "Bob"}})

(defn find-user [id]
  (get users id)) ;; O(log32 n) ≈ O(1)

;; ✅ Conj for building collections
(defn build-list [items]
  (reduce (fn [result item]
            (conj result item)) ;; O(1)
          []
          items))

;; Even simpler
(defn build-list [items]
  (vec items))

;; ✅ Sets for membership
(def ids #{1 2 3 4 5})
(contains? ids 3) ;; => true, O(log32 n) ≈ O(1)

;; ✅ Use into with transducers
(def result
  (into []
        (comp (filter even?)
              (map #(* % %)))
        (range 100)))
```

## Code Organization Anti-Patterns

### God Namespace

Single namespace with hundreds of functions.

**Why it's bad:**

- Hard to navigate
- Circular dependency risks
- Merge conflicts
- Unclear responsibilities

**Example:**

```clojure
;; ❌ God namespace - everything in one place
(ns myapp.core
  (:require [clojure.java.jdbc :as jdbc]
            [ring.adapter.jetty :as jetty]
            [hiccup.core :as hiccup]))

;; Database functions
(defn db-config [] ...)
(defn query-users [] ...)
(defn insert-user [] ...)
(defn update-user [] ...)

;; HTTP handlers
(defn home-page [] ...)
(defn user-page [] ...)
(defn login-page [] ...)

;; Business logic
(defn validate-user [] ...)
(defn calculate-discount [] ...)
(defn process-order [] ...)

;; Utilities
(defn format-date [] ...)
(defn send-email [] ...)

;; Server
(defn start-server [] ...)

;; ...300 more functions
```

**Solution:**

```clojure
;; ✅ Focused namespaces
;; myapp.db - Database access
(ns myapp.db
  (:require [clojure.java.jdbc :as jdbc]))

(defn config [] ...)
(defn query-users [] ...)
(defn insert-user! [user] ...)

;; myapp.domain.user - User business logic
(ns myapp.domain.user)

(defn validate-user [user] ...)
(defn active? [user] ...)

;; myapp.handlers - HTTP handlers
(ns myapp.handlers
  (:require [myapp.db :as db]
            [myapp.domain.user :as user]
            [myapp.views :as views]))

(defn home-page [request] ...)
(defn user-page [request] ...)

;; myapp.views - HTML rendering
(ns myapp.views
  (:require [hiccup.core :as hiccup]))

(defn user-list-page [users] ...)

;; myapp.server - Server lifecycle
(ns myapp.server
  (:require [ring.adapter.jetty :as jetty]
            [myapp.handlers :as handlers]))

(defn start [] (jetty/run-jetty handlers/app {...}))
```

## Summary

Clojure anti-patterns often emerge from forcing object-oriented or imperative patterns into a functional language. State management anti-patterns like atom overuse and global mutable state break the functional programming model, making code difficult to test and reason about. Use atoms sparingly at system boundaries, keeping core logic pure and stateless.

Object-oriented thinking manifests as excessive defrecord usage and protocol abuse. Plain maps with namespaced keywords provide the flexibility and simplicity that make Clojure powerful. Save defrecord for proven performance bottlenecks and explicit protocol implementations.

Lazy sequence pitfalls create subtle bugs through lazy I/O side effects and holding onto heads. Force evaluation when performing I/O operations, use with-open for resource management, and be mindful of retaining references to sequence heads that prevent garbage collection.

Error handling anti-patterns like swallowing exceptions and string-based errors make debugging impossible and prevent programmatic error recovery. Use ex-info to create structured exceptions with contextual data that callers can handle intelligently.

Performance anti-patterns waste effort on premature optimization while missing actual bottlenecks. Use immutable data structures by default, only reaching for transients after profiling proves their necessity. Choose appropriate collection types based on access patterns - maps for lookups, sets for membership tests, vectors for indexed access.

Code organization anti-patterns create god namespaces that become unmaintainable as they grow. Break code into focused namespaces with clear responsibilities. Each namespace should have a single, well-defined purpose that makes finding and understanding functionality straightforward.

Recognition and early intervention prevent these anti-patterns from becoming entrenched. Write idiomatic Clojure by embracing immutability, functional composition, and the REPL-driven development workflow. Question patterns that feel awkward - they often signal an anti-pattern trying to emerge.

## Related Content

- [Clojure Best Practices and Functional Programming Principles](/en/learn/software-engineering/programming-languages/clojure/explanation/best-practices)
- [Clojure Beginner Tutorial](/en/learn/software-engineering/programming-languages/clojure/tutorials/beginner)
- [Clojure Intermediate Tutorial](/en/learn/software-engineering/programming-languages/clojure/tutorials/intermediate)
- [Clojure Advanced Tutorial](/en/learn/software-engineering/programming-languages/clojure/tutorials/advanced)
