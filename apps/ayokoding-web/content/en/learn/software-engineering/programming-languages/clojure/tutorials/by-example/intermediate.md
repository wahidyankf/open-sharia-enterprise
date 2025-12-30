---
title: "Intermediate"
weight: 10000002
date: 2025-12-30T03:00:00+07:00
draft: false
description: "Examples 28-54: Multimethods, protocols, macros, STM, core.async, spec (40-75% coverage)"
---

This section covers production Clojure patterns from examples 28-54, achieving 40-75% topic coverage.

## Example 28: Multimethods

Multimethods enable polymorphism based on arbitrary dispatch functions.

```mermaid
%% Multimethod dispatch flow
graph TD
    A[area shape] --> B{Dispatch on :shape}
    B -->|:circle| C[Circle Method]
    B -->|:rectangle| D[Rectangle Method]
    B -->|:triangle| E[:default Method]
    C --> F[π × r²]
    D --> G[width × height]
    E --> H[Exception]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
    style E fill:#029E73,color:#fff
    style F fill:#CC78BC,color:#000
    style G fill:#CC78BC,color:#000
    style H fill:#CA9161,color:#000
```

```clojure
(defmulti area :shape)                       ;; => Dispatch on :shape key

(defmethod area :circle [shape]
  (* Math/PI (:radius shape) (:radius shape))) ;; => Circle area formula

(defmethod area :rectangle [shape]
  (* (:width shape) (:height shape)))        ;; => Rectangle area formula

(defmethod area :default [shape]
  (throw (ex-info "Unknown shape" {:shape shape})))

(area {:shape :circle :radius 5})            ;; => 78.53981633974483
(area {:shape :rectangle :width 4 :height 3}) ;; => 12
```

**Key Takeaway**: Multimethods dispatch on computed values enabling flexible polymorphism beyond type hierarchies.

## Example 29: Protocols

Protocols define interfaces for polymorphic functions with type-based dispatch.

```mermaid
%% Protocol implementation flow
graph TD
    A[IDrawable Protocol] --> B[draw method]
    A --> C[resize method]
    B --> D[Circle Implementation]
    B --> E[Square Implementation]
    C --> F[Circle resize]
    C --> G[Square resize]
    D --> H[String Output]
    E --> I[String Output]
    F --> J[New Circle Instance]
    G --> K[New Square Instance]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#DE8F05,color:#000
    style D fill:#029E73,color:#fff
    style E fill:#029E73,color:#fff
    style F fill:#029E73,color:#fff
    style G fill:#029E73,color:#fff
    style H fill:#CC78BC,color:#000
    style I fill:#CC78BC,color:#000
    style J fill:#CC78BC,color:#000
    style K fill:#CC78BC,color:#000
```

```clojure
(defprotocol IDrawable
  (draw [this])                              ;; => Protocol method
  (resize [this factor]))

(defrecord Circle [radius]
  IDrawable
  (draw [this]
    (str "Drawing circle with radius " radius))
  (resize [this factor]
    (Circle. (* radius factor))))            ;; => Create new instance

(defrecord Square [side]
  IDrawable
  (draw [this]
    (str "Drawing square with side " side))
  (resize [this factor]
    (Square. (* side factor))))

(let [c (Circle. 5)
      s (Square. 10)]
  (println (draw c))                         ;; => Drawing circle with radius 5
  (println (draw (resize s 2))))             ;; => Drawing square with side 20
```

**Key Takeaway**: Protocols provide interface-based polymorphism with better performance than multimethods for type dispatch.

## Example 30: Records and Types

Records provide efficient map-like data structures with type identity.

```clojure
(defrecord User [id name email])

(def user (->User 1 "Alice" "alice@example.com")) ;; => Positional constructor
(def user2 (map->User {:id 2 :name "Bob" :email "bob@example.com"})) ;; => Map constructor

;; Records are maps
(println (:name user))                       ;; => Alice
(println (assoc user :role "admin"))         ;; => Adds :role key
(println (keys user))                        ;; => (:id :name :email)

;; But have type identity
(println (instance? User user))              ;; => true
(println (= user {:id 1 :name "Alice" :email "alice@example.com"})) ;; => true

;; deftype for lower-level control (no map behavior)
(deftype Point [x y]
  Object
  (toString [this]
    (str "Point(" x ", " y ")")))

(let [p (Point. 3 4)]
  (println (.toString p)))                   ;; => Point(3, 4)
```

**Key Takeaway**: Records combine map convenience with type identity and performance; types offer full control.

## Example 31: Basic Macros

Macros transform code at compile time enabling custom syntax.

```clojure
(defmacro unless [test then-form]
  `(if (not ~test) ~then-form))              ;; => Syntax quote and unquote

(unless false (println "Executed"))          ;; => Executed
(unless true (println "Not executed"))       ;; => (nothing)

;; Macro with multiple forms
(defmacro with-timing [expr]
  `(let [start# (System/nanoTime)            ;; => Auto-gensym with #
         result# ~expr
         end# (System/nanoTime)]
     (println "Elapsed:" (/ (- end# start#) 1e6) "ms")
     result#))

(with-timing (reduce + (range 1000000)))     ;; => Elapsed: X ms, then sum
```

**Key Takeaway**: Macros enable metaprogramming by manipulating code as data before evaluation.

## Example 32: Macro Hygiene and Gensym

Avoid variable capture in macros using gensym or auto-gensym.

```clojure
;; Bad: variable capture
(defmacro bad-twice [expr]
  `(let [x# ~expr]                           ;; => x# is gensym
     (+ x# x#)))

;; If user code has x, no conflict
(let [x 10]
  (bad-twice 5))                             ;; => 10 (not 20!)

;; Good: explicit gensym
(defmacro good-twice [expr]
  (let [result (gensym "result")]
    `(let [~result ~expr]
       (+ ~result ~result))))

(good-twice 5)                               ;; => 10

;; Auto-gensym with # suffix
(defmacro auto-twice [expr]
  `(let [result# ~expr]                      ;; => result# unique symbol
     (+ result# result#)))

(auto-twice 5)                               ;; => 10
```

**Key Takeaway**: Use auto-gensym (#) or explicit gensym to prevent variable capture in macros.

## Example 33: Atoms for Synchronous State

Atoms provide thread-safe synchronous mutable references.

```mermaid
%% Atom state transitions
stateDiagram-v2
    [*] --> atom_0: Create atom 0
    atom_0 --> atom_1: swap! inc
    atom_1 --> atom_6: swap! + 5
    atom_6 --> atom_0: reset! 0
    atom_0 --> complex: Create {:users [] :count 0}
    complex --> updated: swap! update :count inc
    updated --> final: swap! update :users conj

    note right of atom_0: Deref with @
    note right of atom_1: Atomic increment
    note right of atom_6: Atomic addition
    note right of atom_0: Direct set
    note right of final: Nested state updates
```

```clojure
(def counter (atom 0))                       ;; => Create atom

(println @counter)                           ;; => 0 (dereference)

(swap! counter inc)                          ;; => Atomically increment
(println @counter)                           ;; => 1

(swap! counter + 5)                          ;; => Apply function with args
(println @counter)                           ;; => 6

(reset! counter 0)                           ;; => Set to value directly
(println @counter)                           ;; => 0

;; Atoms with complex state
(def app-state (atom {:users [] :count 0}))

(swap! app-state update :count inc)
(swap! app-state update :users conj {:name "Alice"})

(println @app-state)                         ;; => {:users [{:name "Alice"}], :count 1}
```

**Key Takeaway**: Atoms provide lock-free atomic updates for independent synchronous state changes.

## Example 34: Refs and Software Transactional Memory

Refs enable coordinated synchronous updates across multiple references.

```mermaid
%% STM transaction flow
sequenceDiagram
    participant T as Transaction
    participant A as account-a (ref)
    participant B as account-b (ref)

    Note over T,B: Initial: A=1000, B=500
    T->>A: dosync start
    T->>A: alter - 200
    A-->>T: 800 (tentative)
    T->>B: alter + 200
    B-->>T: 700 (tentative)
    T->>T: Commit transaction
    T-->>A: Committed: 800
    T-->>B: Committed: 700
    Note over T,B: Final: A=800, B=700

    Note over T,B: Transaction Failure
    T->>A: dosync start
    T->>A: alter - 100
    T->>T: Exception thrown
    T->>T: Rollback
    Note over A,B: State unchanged
```

```clojure
(def account-a (ref 1000))                   ;; => Bank account A
(def account-b (ref 500))                    ;; => Bank account B

(defn transfer [from to amount]
  (dosync                                    ;; => Transaction boundary
    (alter from - amount)                    ;; => Deduct from source
    (alter to + amount)))                    ;; => Add to destination

(println @account-a @account-b)              ;; => 1000 500

(transfer account-a account-b 200)           ;; => Transfer $200
(println @account-a @account-b)              ;; => 800 700

;; Transactions are atomic - all succeed or all fail
(try
  (dosync
    (alter account-a - 100)
    (throw (Exception. "Error!"))            ;; => Transaction aborted
    (alter account-b + 100))                 ;; => Never executed
  (catch Exception e
    (println "Transaction failed")))

(println @account-a @account-b)              ;; => 800 700 (unchanged)
```

**Key Takeaway**: Refs with STM provide coordinated atomic transactions across multiple state changes.

## Example 35: Agents for Asynchronous State

Agents handle asynchronous state changes with guaranteed sequential processing.

```mermaid
%% Agent asynchronous processing
graph LR
    A[send action 1] --> Q[Agent Queue]
    B[send action 2] --> Q
    C[send action 3] --> Q
    Q --> T[Thread Pool]
    T --> S1[Sequential Execution]
    S1 --> S2[Action 1]
    S2 --> S3[Action 2]
    S3 --> S4[Action 3]
    S4 --> R[Agent State Updated]

    style A fill:#0173B2,color:#fff
    style B fill:#0173B2,color:#fff
    style C fill:#0173B2,color:#fff
    style Q fill:#DE8F05,color:#000
    style T fill:#029E73,color:#fff
    style S1 fill:#CC78BC,color:#000
    style R fill:#CA9161,color:#000
```

```clojure
(def logger (agent []))                      ;; => Create agent

(send logger conj "First log")               ;; => Async update (thread pool)
(send logger conj "Second log")

(Thread/sleep 100)                           ;; => Wait for async updates
(println @logger)                            ;; => ["First log" "Second log"]

;; send-off for blocking operations
(def file-writer (agent nil))

(send-off file-writer
  (fn [_]
    (Thread/sleep 1000)                      ;; => Blocking operation
    (println "File written")
    :done))

;; Error handling
(def error-agent (agent 0))

(send error-agent / 0)                       ;; => Division by zero
(Thread/sleep 100)
(println (agent-error error-agent))          ;; => Exception object

(restart-agent error-agent 0)                ;; => Restart after error
(send error-agent inc)
```

**Key Takeaway**: Agents provide asynchronous state updates with sequential processing guarantees.

## Example 36: core.async Channels

Channels enable CSP-style communication between async processes.

```mermaid
%% Channel communication pattern
graph TD
    P1[Producer go block] -->|>! put| C[Channel]
    P2[Producer go block] -->|>! put| C
    C -->|<! take| C1[Consumer go block 1]
    C -->|<! take| C2[Consumer go block 2]
    C1 --> O1[Process Value]
    C2 --> O2[Process Value]

    B[Buffered Channel] -->|Size 10| BUF[Buffer]
    BUF -->|Full?| PARK[Park Producer]

    style P1 fill:#0173B2,color:#fff
    style P2 fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style C1 fill:#029E73,color:#fff
    style C2 fill:#029E73,color:#fff
    style B fill:#CC78BC,color:#000
    style BUF fill:#CA9161,color:#000
```

```clojure
(require '[clojure.core.async :refer [chan go >! <! >!! <!! timeout close!]])

(def ch (chan))                              ;; => Create channel

;; Producer (in go block)
(go
  (>! ch "Hello")                            ;; => Put value (parking)
  (>! ch "World"))

;; Consumer (in go block)
(go
  (println (<! ch))                          ;; => Take value (parking)
  (println (<! ch)))                         ;; => Hello, World

;; Blocking put/take (outside go blocks)
(def ch2 (chan))

(future (>!! ch2 "Data"))                    ;; => Blocking put
(println (<!! ch2))                          ;; => Blocking take: Data

;; Buffered channels
(def buffered (chan 10))                     ;; => Buffer size 10
(>!! buffered 1)
(>!! buffered 2)
(println (<!! buffered))                     ;; => 1
```

**Key Takeaway**: Channels decouple producers from consumers enabling async coordination.

## Example 37: core.async go Blocks

go blocks execute code asynchronously with automatic channel parking.

```mermaid
%% go block execution timeline
sequenceDiagram
    participant M as Main Thread
    participant G1 as go block 1
    participant G2 as go block 2
    participant CH as Channel

    M->>G1: Launch go block
    M->>G2: Launch go block
    Note over M: Main continues...

    G1->>G1: println "Starting..."
    G1->>G1: timeout 1000ms (park)
    Note over G1: Thread released
    G2->>G2: println "Waiting..."
    G2->>CH: <! (park, waiting)
    Note over G2: Thread released

    Note over G1: 1000ms elapsed
    G1->>CH: >! "Task complete"
    CH->>G2: Value delivered
    G2->>G2: println "Result"
```

```clojure
(require '[clojure.core.async :refer [go chan >! <! timeout]])

(def ch (chan))

(go
  (println "Starting task...")
  (<! (timeout 1000))                        ;; => Park for 1 second
  (>! ch "Task complete")
  (println "Task done"))

(go
  (println "Waiting for result...")
  (println "Result:" (<! ch)))               ;; => Wait for value

;; Multiple parallel tasks
(defn async-task [id delay-ms]
  (go
    (println "Task" id "starting")
    (<! (timeout delay-ms))
    (println "Task" id "completed")
    id))

(go
  (let [results (doall (map #(async-task % (* % 100)) (range 5)))]
    (doseq [result results]
      (println "Completed:" (<! result)))))
```

**Key Takeaway**: go blocks enable lightweight async computation with automatic channel coordination.

## Example 38: clojure.spec Validation

spec defines data shape specifications for validation and generation.

```clojure
(require '[clojure.spec.alpha :as s])

;; Simple predicates
(s/def ::age pos-int?)                       ;; => Positive integer
(s/def ::name string?)

(s/valid? ::age 25)                          ;; => true
(s/valid? ::age -5)                          ;; => false

;; Composite specs
(s/def ::user (s/keys :req [::name ::age]))  ;; => Required keys

(s/valid? ::user {::name "Alice" ::age 30})  ;; => true
(s/valid? ::user {::name "Bob"})             ;; => false (missing ::age)

;; explain for validation errors
(s/explain ::user {::name "Bob"})
;; => val: {:user/name "Bob"} fails spec: :user/user at: [:user/age] predicate: pos-int?

;; conform transforms data
(s/def ::number-string (s/conformer #(Integer/parseInt %)))
(s/conform ::number-string "42")             ;; => 42
```

**Key Takeaway**: spec provides declarative validation with rich error messages and data transformation.

## Example 39: Function Specs

Define specs for function arguments and return values.

```clojure
(require '[clojure.spec.alpha :as s])
(require '[clojure.spec.test.alpha :as stest])

(s/def ::x number?)
(s/def ::y number?)

(defn add [x y]
  (+ x y))

(s/fdef add
  :args (s/cat :x ::x :y ::y)                ;; => Arguments spec
  :ret number?)                              ;; => Return value spec

;; Instrument for runtime checking
(stest/instrument `add)

(add 2 3)                                    ;; => 5
;; (add "2" 3)                               ;; => Error: invalid arguments

;; Generative testing
(stest/check `add)                           ;; => Run property-based tests
```

**Key Takeaway**: Function specs enable runtime validation and generative testing of function contracts.

## Example 40: Transducers

Transducers compose transformations without creating intermediate collections.

```mermaid
%% Transducer vs regular sequence operations
graph TD
    subgraph Regular Sequences
    R1[range 10] --> R2[map inc]
    R2 --> R3[Lazy Seq 1]
    R3 --> R4[filter even?]
    R4 --> R5[Lazy Seq 2]
    R5 --> R6[Final Result]
    end

    subgraph Transducers
    T1[range 10] --> TX[Transducer comp]
    TX --> T2[map inc + filter even?]
    T2 --> T3[Single Pass]
    T3 --> T4[Final Result]
    end

    style R3 fill:#CA9161,color:#000
    style R5 fill:#CA9161,color:#000
    style TX fill:#029E73,color:#fff
    style T2 fill:#0173B2,color:#fff
    style T3 fill:#DE8F05,color:#000
```

```clojure
;; Regular sequence operations (create intermediates)
(->> (range 1000000)
     (map inc)                               ;; => Lazy seq
     (filter even?)                          ;; => Another lazy seq
     (take 10))

;; Transducer (no intermediate sequences)
(def xf
  (comp
    (map inc)                                ;; => Composable transformation
    (filter even?)))

(into [] xf (range 10))                      ;; => [2 4 6 8 10]
(sequence xf (range 10))                     ;; => (2 4 6 8 10) lazy

;; With transduce (reduce-like)
(transduce xf + (range 10))                  ;; => 30

;; Custom transducer
(defn take-while-xf [pred]
  (fn [rf]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (if (pred input)
         (rf result input)
         (reduced result))))))               ;; => Early termination

(into [] (take-while-xf #(< % 5)) (range 10)) ;; => [0 1 2 3 4]
```

**Key Takeaway**: Transducers eliminate intermediate collections providing efficient composable transformations.

## Example 41: Exception Handling

Handle errors with try/catch and ex-info for custom exceptions.

```clojure
(defn divide [a b]
  (if (zero? b)
    (throw (ex-info "Division by zero"      ;; => Exception with data
                    {:a a :b b}))
    (/ a b)))

(try
  (divide 10 0)
  (catch Exception e
    (println "Error:" (.getMessage e))       ;; => Error: Division by zero
    (println "Data:" (ex-data e))))          ;; => Data: {:a 10, :b 0}

;; Multiple catch blocks
(try
  (Integer/parseInt "not-a-number")
  (catch NumberFormatException e
    (println "Invalid number"))
  (catch Exception e
    (println "Other error")))

;; finally
(try
  (println "Opening resource")
  (/ 1 0)
  (catch Exception e
    (println "Error occurred"))
  (finally
    (println "Cleanup")))                    ;; => Always executed
```

**Key Takeaway**: ex-info attaches structured data to exceptions for rich error context.

## Example 42: Lazy Sequences

Lazy sequences compute elements on demand enabling infinite sequences.

```mermaid
%% Lazy sequence evaluation
graph LR
    A[Infinite Sequence] --> B[naturals iterate inc]
    B --> C[Not Computed]
    C --> D{take 5}
    D -->|Demand| E[Compute 0]
    E --> F[Compute 1]
    F --> G[Compute 2]
    G --> H[Compute 3]
    H --> I[Compute 4]
    I --> J[Return 0 1 2 3 4]
    C -.->|Rest| K[Still Lazy]

    style A fill:#0173B2,color:#fff
    style C fill:#CA9161,color:#000
    style D fill:#DE8F05,color:#000
    style E fill:#029E73,color:#fff
    style F fill:#029E73,color:#fff
    style G fill:#029E73,color:#fff
    style H fill:#029E73,color:#fff
    style I fill:#029E73,color:#fff
    style J fill:#CC78BC,color:#000
    style K fill:#CA9161,color:#000
```

```clojure
;; Infinite sequence
(def naturals (iterate inc 0))               ;; => 0, 1, 2, 3, ...

(take 5 naturals)                            ;; => (0 1 2 3 4)

;; Lazy map/filter
(def evens (filter even? naturals))          ;; => Lazy, not computed yet

(take 5 evens)                               ;; => (0 2 4 6 8) computed on demand

;; Custom lazy sequence
(defn fib-seq
  ([] (fib-seq 0 1))
  ([a b]
   (lazy-seq                                 ;; => Lazy constructor
     (cons a (fib-seq b (+ a b))))))

(take 10 (fib-seq))                          ;; => (0 1 1 2 3 5 8 13 21 34)

;; Force realization
(def realized (doall (take 5 naturals)))     ;; => Force computation
```

**Key Takeaway**: Lazy sequences enable memory-efficient processing of large or infinite data.

## Example 43: Testing with clojure.test

Write unit tests using clojure.test framework.

```clojure
(ns myapp.core-test
  (:require [clojure.test :refer :all]
            [myapp.core :refer :all]))

(deftest addition-test
  (testing "Addition of positive numbers"
    (is (= 5 (+ 2 3)))                       ;; => Assertion
    (is (= 10 (+ 4 6)))))

(deftest division-test
  (testing "Division"
    (is (= 2 (/ 10 5)))
    (is (thrown? ArithmeticException (/ 1 0))))) ;; => Exception assertion

;; Fixtures (setup/teardown)
(defn database-fixture [f]
  (println "Setup database")
  (f)                                        ;; => Run test
  (println "Teardown database"))

(use-fixtures :each database-fixture)        ;; => Run for each test

;; Run tests
(run-tests)                                  ;; => Execute all tests
```

**Key Takeaway**: clojure.test provides simple assertion-based testing with fixtures.

## Example 44: deps.edn Dependencies

Modern dependency management with deps.edn instead of leiningen.

```clojure
;; deps.edn
{:paths ["src" "resources"]
 :deps {org.clojure/clojure {:mvn/version "1.11.1"}
        http-kit/http-kit {:mvn/version "2.7.0"}
        compojure/compojure {:mvn/version "1.7.0"}}

 :aliases
 {:dev {:extra-paths ["dev"]
        :extra-deps {org.clojure/tools.namespace {:mvn/version "1.4.4"}}}

  :test {:extra-paths ["test"]
         :extra-deps {org.clojure/test.check {:mvn/version "1.1.1"}}}

  :uberjar {:replace-deps {com.github.seancorfield/depstar {:mvn/version "2.1.303"}}
            :exec-fn hf.depstar/uberjar
            :exec-args {:jar "app.jar"}}}}

;; Run REPL with dev alias
;; clj -A:dev

;; Run tests
;; clj -A:test -M -m clojure.test

;; Build uberjar
;; clj -X:uberjar
```

**Key Takeaway**: deps.edn provides declarative dependency management with aliases for environments.

## Example 45: Namespace Organization

Organize code into namespaces with proper require/refer patterns.

```clojure
(ns myapp.user
  (:require [clojure.string :as str]         ;; => Alias
            [clojure.set :refer [union intersection]] ;; => Refer specific
            [myapp.database :as db]))        ;; => Project namespace

(defn create-user [name email]
  {:id (random-uuid)
   :name (str/upper-case name)               ;; => Use alias
   :email email})

(defn find-common-roles [user1 user2]
  (intersection (:roles user1) (:roles user2))) ;; => Referred function

;; Private functions
(defn- validate-email [email]                ;; => Private with defn-
  (str/includes? email "@"))

;; Load from file
;; (load "myapp/utils")                      ;; => Load relative file
```

**Key Takeaway**: Namespaces organize code with explicit dependencies via require/refer.

## Example 46: Metadata

Attach metadata to values for documentation and tools.

```clojure
;; Function metadata
(defn greet
  "Greets a person by name"                  ;; => Docstring
  {:added "1.0" :author "Alice"}             ;; => Metadata map
  [name]
  (str "Hello, " name))

(meta #'greet)                               ;; => {:doc "Greets...", :added "1.0", ...}

;; Value metadata
(def config ^{:private true} {:host "localhost"})

(meta #'config)                              ;; => {:private true}

;; Type hints for performance
(defn fast-add ^long [^long a ^long b]      ;; => Return type and arg types
  (+ a b))

;; Reader metadata
(def data ^:dynamic *db-conn*)               ;; => Dynamic var marker

(with-meta {:name "Alice"} {:timestamp 123})
;; => {:name "Alice"} with metadata {:timestamp 123}
```

**Key Takeaway**: Metadata provides out-of-band information without affecting value equality.

## Example 47: Destructuring Advanced

Advanced destructuring patterns for complex data extraction.

```clojure
;; Nested destructuring
(let [{:keys [name address]} {:name "Alice" :address {:city "NYC" :zip 10001}}
      {city :city} address]                  ;; => Nested destructuring
  (println name city))                       ;; => Alice NYC

;; :as for original value
(let [[a b :as all] [1 2 3 4]]
  (println "a:" a "b:" b "all:" all))        ;; => a: 1 b: 2 all: [1 2 3 4]

;; :or for defaults
(let [{:keys [x y]
       :or {x 0 y 0}} {:y 5}]                ;; => x defaults to 0
  (println "x:" x "y:" y))                   ;; => x: 0 y: 5

;; String keys
(let [{:strs [name age]} {"name" "Bob" "age" 25}]
  (println name age))                        ;; => Bob 25

;; Symbol keys
(let [{:syms [x y]} {'x 10 'y 20}]
  (println x y))                             ;; => 10 20

;; Function argument destructuring
(defn print-point [{:keys [x y] :or {x 0 y 0}}]
  (println "Point:" x y))

(print-point {:x 3 y 4})                     ;; => Point: 3 4
(print-point {})                             ;; => Point: 0 0
```

**Key Takeaway**: Advanced destructuring enables concise extraction from nested complex structures.

## Example 48: Java Interop Advanced

Advanced Java interoperability patterns.

```clojure
;; Static methods and fields
(Math/pow 2 3)                               ;; => 8.0
Math/PI                                      ;; => 3.141592653589793

;; Instance methods
(.toUpperCase "hello")                       ;; => "HELLO"
(.length "world")                            ;; => 5

;; Constructor
(java.util.Date.)                            ;; => Current date

;; Chaining (..)
(.. (java.util.Date.) (toString) (toUpperCase)) ;; => "WED DEC 30 ..."

;; doto for mutation
(doto (java.util.ArrayList.)
  (.add "first")
  (.add "second")
  (.add "third"))                            ;; => ["first" "second" "third"]

;; Type hints for performance
(defn fast-array-sum ^long [^"[J" arr]      ;; => long[], returns long
  (aget arr 0))                              ;; => Array access

;; Implementing interfaces
(defn create-runnable [f]
  (reify java.lang.Runnable
    (run [this]
      (f))))

(let [task (create-runnable #(println "Running!"))]
  (.start (Thread. task)))                   ;; => Running!
```

**Key Takeaway**: Clojure provides seamless Java interop with concise syntax for methods, fields, and interfaces.

## Example 49: Reducers

Reducers enable parallel fold operations on collections.

```clojure
(require '[clojure.core.reducers :as r])

;; Sequential reduce
(reduce + (range 1000000))                   ;; => Sequential sum

;; Parallel fold
(r/fold + (vec (range 1000000)))             ;; => Parallel sum (fork-join)

;; Reducer transformations (like transducers)
(->> (range 1000000)
     vec
     (r/map inc)                             ;; => Parallel map
     (r/filter even?)                        ;; => Parallel filter
     (r/fold +))                             ;; => Parallel reduction

;; Custom folder
(defn parallel-count [coll]
  (r/fold
    (fn ([] 0) ([a b] (+ a b)))              ;; => Combine function
    (fn ([acc _] (inc acc)))                 ;; => Reduce function
    coll))

(parallel-count (vec (range 1000000)))       ;; => 1000000
```

**Key Takeaway**: Reducers enable parallel processing with fork-join for performance-critical reductions.

## Example 50: Futures and Promises

Futures run async computations; promises coordinate async results.

```mermaid
%% Futures and Promises coordination
sequenceDiagram
    participant M as Main Thread
    participant F as Future Thread
    participant P as Promise

    M->>F: Create future
    Note over F: Computing...
    M->>M: println "Computing..."
    M->>F: Deref @ (blocks)
    Note over M: Waiting...
    F->>F: Thread/sleep 1000
    F->>F: (+ 1 2)
    F-->>M: Return 3
    M->>M: println 3

    M->>P: Create promise
    M->>F: Launch future
    M->>M: println "Waiting..."
    M->>P: Deref @ (blocks)
    F->>F: Thread/sleep 1000
    F->>P: deliver "Async result"
    P-->>M: Return value
    M->>M: println result

    style M fill:#0173B2,color:#fff
    style F fill:#029E73,color:#fff
    style P fill:#DE8F05,color:#000
```

```clojure
;; Future (async computation)
(def result (future
              (Thread/sleep 1000)
              (+ 1 2)))                      ;; => Runs in thread pool

(println "Computing...")
(println @result)                            ;; => 3 (blocks until done)

(println (realized? result))                 ;; => true (already computed)

;; Promise (synchronization point)
(def p (promise))

(future
  (Thread/sleep 1000)
  (deliver p "Async result"))                ;; => Deliver value

(println "Waiting...")
(println @p)                                 ;; => Blocks until delivered: "Async result"

;; Combining futures and promises
(defn async-add [a b]
  (let [p (promise)]
    (future
      (Thread/sleep 500)
      (deliver p (+ a b)))
    p))

(println @(async-add 10 20))                 ;; => 30
```

**Key Takeaway**: Futures enable fire-and-forget async; promises enable result coordination.

## Example 51: Delays

Delays defer computation until first dereference with caching.

```clojure
(def expensive (delay
                 (println "Computing...")
                 (Thread/sleep 1000)
                 42))                        ;; => Not computed yet

(println "Created delay")

(println @expensive)                         ;; => Computing..., 42
(println @expensive)                         ;; => 42 (cached, no recomputation)

(println (realized? expensive))              ;; => true

;; Useful for optional expensive initializations
(defn load-config []
  (delay
    (println "Loading config...")
    {:db "localhost" :port 5432}))

(def config (load-config))                   ;; => Not loaded yet

;; Config loaded only if needed
(when-let [db (:db @config)]
  (println "Connecting to" db))              ;; => Loading config..., Connecting to localhost
```

**Key Takeaway**: Delays provide lazy initialization with memoization for expensive one-time computations.

## Example 52: File I/O

Read and write files using clojure.java.io.

```clojure
(require '[clojure.java.io :as io])

;; Write to file
(spit "data.txt" "Hello, World!")            ;; => Write string

;; Append to file
(spit "data.txt" "\nNew line" :append true)

;; Read from file
(println (slurp "data.txt"))                 ;; => Read entire file

;; Line-by-line reading
(with-open [rdr (io/reader "data.txt")]
  (doseq [line (line-seq rdr)]               ;; => Lazy line sequence
    (println "Line:" line)))

;; Write lines
(with-open [wtr (io/writer "output.txt")]
  (binding [*out* wtr]
    (println "Line 1")
    (println "Line 2")))

;; Resources
(slurp (io/resource "config.edn"))           ;; => Read from classpath
```

**Key Takeaway**: spit/slurp for simple I/O; with-open ensures resource cleanup.

## Example 53: Atoms Advanced Patterns

Advanced atom usage patterns for complex state management.

```clojure
;; Atom with validators
(def positive-count
  (atom 0
    :validator pos-int?))                    ;; => Validation function

(reset! positive-count 5)                    ;; => OK
;; (reset! positive-count -1)                ;; => Throws (validation fails)

;; Atom with watchers
(def watched-atom (atom 0))

(add-watch watched-atom :logger
  (fn [key ref old-val new-val]
    (println "Changed from" old-val "to" new-val)))

(reset! watched-atom 10)                     ;; => Changed from 0 to 10
(swap! watched-atom inc)                     ;; => Changed from 10 to 11

(remove-watch watched-atom :logger)          ;; => Remove watcher

;; Compare and set
(def counter (atom 0))

(compare-and-set! counter 0 1)               ;; => true (old=0, set to 1)
(compare-and-set! counter 0 2)               ;; => false (old≠0)
```

**Key Takeaway**: Validators enforce constraints; watchers enable reactive updates; compare-and-set provides CAS semantics.

## Example 54: Set Operations

Clojure sets support relational algebra operations.

```clojure
(require '[clojure.set :as set])

(def set1 #{1 2 3 4})
(def set2 #{3 4 5 6})

;; Union
(set/union set1 set2)                        ;; => #{1 2 3 4 5 6}

;; Intersection
(set/intersection set1 set2)                 ;; => #{3 4}

;; Difference
(set/difference set1 set2)                   ;; => #{1 2}

;; Relational operations
(def users #{{:id 1 :name "Alice" :dept "IT"}
             {:id 2 :name "Bob" :dept "HR"}
             {:id 3 :name "Charlie" :dept "IT"}})

(def depts #{{:dept "IT" :budget 100000}
             {:dept "HR" :budget 50000}})

;; Join
(set/join users depts)                       ;; => Natural join on :dept

;; Select (filter)
(set/select #(= (:dept %) "IT") users)       ;; => IT department users

;; Project (column selection)
(set/project users [:name :dept])            ;; => #{:name :dept} only

;; Rename
(set/rename users {:name :full-name})        ;; => Rename key
```

**Key Takeaway**: Set operations provide relational algebra for in-memory data manipulation.

## Summary

Intermediate Clojure (examples 28-54) covers production patterns for real-world development: multimethods and protocols for polymorphism, macros for metaprogramming, STM and async coordination, core.async for CSP-style concurrency, spec for validation, transducers for efficient transformations, comprehensive error handling, lazy sequences, testing, dependency management, and advanced Java interop. Master these patterns to write maintainable, concurrent Clojure applications operating at 75% language coverage.
