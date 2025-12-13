---
title: "Testing: Basics"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the source code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/testing_basics).

</aside>

## Testing Functions

### Code

```clojure
(ns testing-basics.core
  (:require [clojure.test :refer [are deftest is run-all-tests run-tests
                                  testing]]))

;; ---
;; Testing functions
;; ---

;; Basic functions

(defn sum [a b]
  (+ a b))
(deftest sum-test
  (is (= 3 (sum 1 2)))
  (is (= 0 (sum 1 -1))))

(deftest sum-test-with-setup
  (let [a 1]
    (is (= 3 (sum a 2)))
    (is (= 0 (sum a -1)))))

(defn subtract [a b]
  (- a b))
(deftest subtract-test
  (are [x y] (= x y)
    0 (subtract 1 1)
    -1 (subtract 1 2)))

;; Function with multiple arities

(defn greeting
  ([] (greeting "World" "Hello"))
  ([name] (greeting name "Hello"))
  ([name salutation] (str salutation ", " name "!")))
(deftest greeting-test
  (is (= "Hello, World!"
         (greeting)))
  (is (= "Hello, John!"
         (greeting "John")))
  (is (= "Good morning, John!"
         (greeting "John" "Good morning"))))

;; Function that throws an exception

(defn throws-exception []
  (throw (Exception. "This function throws an exception")))
(deftest throws-exception-test
  (is (thrown?  Exception
                (throws-exception))))

(run-tests 'testing-basics.core)
;; => {:test 6, :pass 13, :fail 0, :error 0, :type :summary}

(run-all-tests)
;; => {:test 6, :pass 13, :fail 0, :error 0, :type :summary}
```

### Explanation

This Clojure code demonstrates basic unit testing practices in the language. Here's a breakdown of what the code does:

1. **Namespace declaration:** The script starts with the `(ns ...)` form which stands for 'namespace'. This specifies the namespace `testing-basics.core` where the following code will reside.
2. **Require statement:** The `:require` keyword within the namespace declaration is used to import necessary dependencies, in this case, various testing functionalities provided by the `clojure.test` library.
3. **Function definition (sum):** `(defn sum [a b] (+ a b))` defines a function `sum` that takes two arguments, `a` and `b`, and returns their sum.
4. **Test definition for sum:** `(deftest sum-test ...)` defines a test called `sum-test` which verifies that the `sum` function works as expected. It uses the `is` function to assert that the result of the `sum` function is equal to the expected value for two different sets of inputs.
5. **Test definition with setup for sum:** `sum-test-with-setup` is another test for the `sum` function. It utilizes `let` to bind a value to `a` before the assertions. This is useful for providing setup for a test.
6. **Function definition (subtract):** `(defn subtract [a b] ...)` defines a function `subtract` that takes two arguments, `a` and `b`, and returns the result of subtracting `b` from `a`.
7. **Test definition for subtract:** `subtract-test` is a test for the `subtract` function. It uses the `are` macro, which allows for more concise repetitive tests by defining a template for the test, which is then repeated for each group of inputs.
8. **Function definition with multiple arities (greeting):** `greeting` is a function with multiple arities, meaning it can take different numbers of arguments. This function returns a greeting based on the number and type of arguments given.
9. **Test definition for greeting:** `greeting-test` defines a test for the `greeting` function, asserting the expected output for three different sets of inputs.
10. **Function definition (throws-exception):** `throws-exception` is a function that simply throws an exception when called.
11. **Test definition for throws-exception:** `throws-exception-test` is a test that checks if the `throws-exception` function indeed throws an exception as expected. The `thrown?` macro is used to check if an exception of a specific type is thrown.
12. **Running the tests:** `(run-tests 'testing-basics.core)` runs all tests in the `testing-basics.core` namespace. Similarly, `(run-all-tests)` runs all tests in all namespaces. The results (summary statistics) of the tests are returned.

This Clojure code defines several functions and their corresponding tests, then runs them. It demonstrates the basic usage of the `clojure.test` library for testing Clojure code.

## Organizing the Tests

### Code

```clojure
(ns testing-basics.core
  (:require [clojure.test :refer [are deftest is run-all-tests run-tests
                                  testing]]))

;; ---
;; Organizing the tests
;; ---

(defn divide [a b]
  (if (zero? b)
    (throw (IllegalArgumentException. "Cannot divide by zero"))
    (/ a b)))
(deftest divide-test
  (testing "Positive case"
    (is (= 2 (divide 4 2)))
    (is (= 1/2 (divide 1 2))))
  (testing "Negative case"
    (is (thrown?  IllegalArgumentException
                  (divide 4 0)))))
```

### Explanation

Let's break down the Clojure code snippet:

1. `(ns testing-basics.core (:require [clojure.test :refer [are deftest is run-all-tests run-tests testing]]))` - This is the namespace declaration for the module. It defines a new namespace `testing-basics.core` and requires a set of testing-related functions from the `clojure.test` library. The functions that are referred for use in this namespace are `are`, `deftest`, `is`, `run-all-tests`, `run-tests`, and `testing`.
2. `(defn divide [a b] (if (zero? b) (throw (IllegalArgumentException. "Cannot divide by zero")) (/ a b)))` - This function `divide` takes two arguments, `a` and `b`. It checks if `b` is zero, if it is, then an `IllegalArgumentException` is thrown with a message "Cannot divide by zero". If `b` is not zero, then `a` is divided by `b` and the result is returned.
3. `deftest` - This is a macro provided by the `clojure.test` library for defining a test.
4. `(deftest divide-test (testing "Positive case" (is (= 2 (divide 4 2))) (is (= 1/2 (divide 1 2)))) (testing "Negative case" (is (thrown? IllegalArgumentException (divide 4 0)))))` - This defines a test suite named `divide-test` with two test scenarios.
   1. `(testing "Positive case" (is (= 2 (divide 4 2))) (is (= 1/2 (divide 1 2))))` - This is the first testing scenario, labeled as "Positive case". It checks two conditions:
      - The first condition `(is (= 2 (divide 4 2)))` checks if the function `divide` returns `2` when arguments `4` and `2` are passed.
      - The second condition `(is (= 1/2 (divide 1 2)))` checks if the function `divide` returns `1/2` when arguments `1` and `2` are passed.
   2. `(testing "Negative case" (is (thrown? IllegalArgumentException (divide 4 0))))` - This is the second testing scenario, labeled as "Negative case". It checks if an `IllegalArgumentException` is thrown when `4` and `0` are passed to the `divide` function, which is expected because you cannot divide by zero.

These tests can then be run using the `run-tests` function from `clojure.test`, which will provide a report detailing the success or failure of each test case.
