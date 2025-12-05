---
title: "Interop: Using Java in Clojure"
date: 2025-03-16T07:20:00+07:00
draft: false
---

---

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/tree/main/contents/clojure-cookbook/clojure-cookbook-primary/src/interop_java_in_clojure).

</aside>

## Interop Basics

### Code

```clojure
(ns interop-using-java-in-clojure.core
  (:require
   [clojure.reflect :as reflect]))

;; ---
;; Interop basics
;; ---

;; Call instance method

(import 'java.util.ArrayList)

(def my-list (ArrayList.))
my-list
;; => []
(.add my-list "Hello")
;; => true
my-list
;; => ["Hello"]
(.add my-list "World")
;; => true
my-list
;; => ["Hello" "World"]
(.size my-list)
;; => 2

;; Call fields

(import 'java.awt.Dimension)

(def dim (Dimension. 10 20))
dim
;; => #object[java.awt.Dimension 0x48d8fc0 "java.awt.Dimension[width=10,height=20]"]
(.width dim)
;; => 10
(.height dim)
;; => 20

;; Call static method

(import 'java.util.UUID)
(UUID/randomUUID)
;; => #uuid "467c1c23-5a96-4e09-bcbe-497abcb6d73f"
```

### Explanation

Here's the explanation for the provided Clojure code:

1. `(ns interop-java-in-clojure.core (:require [clojure.reflect :as reflect]))`
   This statement is defining a namespace called "interop-java-in-clojure.core". It's also requiring a library called "clojure.reflect" and aliasing it as "reflect".
2. `(import 'java.util.ArrayList)`
   This statement is importing the `ArrayList` class from the `java.util` package.
3. `(def my-list (ArrayList.))`
   Here we are defining a variable `my-list` which is an instance of the `ArrayList` class.
4. `(.add my-list "Hello")`
   This statement is calling the instance method `add` on `my-list` and passing the string "Hello" to it. This will add "Hello" to the ArrayList.
5. `(.add my-list "World")`
   This is similar to the previous `add` call but it's adding the string "World" to the ArrayList.
6. `(.size my-list)`
   Here we are calling the `size` method on `my-list` which will return the number of elements in the ArrayList.
7. `(import 'java.awt.Dimension)`
   This statement is importing the `Dimension` class from the `java.awt` package.
8. `(def dim (Dimension. 10 20))`
   This defines a variable `dim` which is an instance of the `Dimension` class initialized with width 10 and height 20.
9. `(.width dim)` and `(.height dim)`
   These are calling the `width` and `height` fields of the `dim` object which will return the width and height of the dimension, respectively.
10. `(import 'java.util.UUID)`
    This statement is importing the `UUID` class from the `java.util` package.
11. `(UUID/randomUUID)`
    Here we're calling the static method `randomUUID` of the `UUID` class which generates a random UUID and returns it.

## Java Data Types

### Code

```clojure
(ns interop-using-java-in-clojure.core
  (:require
   [clojure.reflect :as reflect]))

;; ---
;; Java data types
;; ---

(int 42)
;; => 42
(double 3.14)
;; => 3.14
(+ (int 42) (double 3.14))
;; => 45.14

(def java-array (to-array [1 2 3]))
(class java-array)
;; => [Ljava.lang.Object;
(seq java-array)
;; => (1 2 3)

(def java-string (String. "Hello"))
;; => #'interop-java-in-clojure.core/java-string
(class java-string)
;; => java.lang.String
(.length java-string)
;; => 5
(str java-string " World")
;; => "Hello World"
```

### Explanation

This Clojure code is demonstrating how to work with Java data types and objects in Clojure. Clojure is a hosted language that can interoperate with its host platform, such as JVM (Java Virtual Machine). This allows Clojure to interact with Java libraries and utilize its functionality. Here's a breakdown of the code:

1. `(ns interop-java-in-clojure.core (:require [clojure.reflect :as reflect]))`
   - This line is defining a new namespace named `interop-java-in-clojure.core` and importing the `clojure.reflect` library.
2. `(int 42)`
   - This function converts the number 42 into an integer. The result is `42`.
3. `(double 3.14)`
   - This function converts the number `3.14` into a double-precision floating-point number. The result is `3.14`.
4. `(+ (int 42) (double 3.14))`
   - This line adds together an integer (`42`) and a double (`3.14`). Clojure will automatically coerce the integer to a double before performing the addition, so the result is `45.14`.
5. `(def java-array (to-array [1 2 3]))`
   - This line defines a variable `java-array`, converting the Clojure vector `[1 2 3]` into a Java array.
6. `(class java-array)`
   - This function gets the class of the `java-array` object. The result, `[Ljava.lang.Object;`, is the internal notation used by the JVM to represent an array of objects.
7. `(seq java-array)`
   - This function converts the Java array `java-array` back into a Clojure sequence `(1 2 3)`.
8. `(def java-string (String. "Hello"))`
   - This line defines a variable `java-string` and initializes it to a new instance of the Java `String` class with the value `"Hello"`.
9. `(class java-string)`
   - This function gets the class of the `java-string` object. The result is `java.lang.String`.
10. `(.length java-string)`
    - This line calls the `length` method on the `java-string` object. The result is `5`, which is the length of the string `"Hello"`.
11. `(str java-string " World")`
    - This function concatenates the `java-string` object with the string `" World"`. The result is `"Hello World"`.

## Java Exception Handling

### Code

```clojure
(ns interop-java-in-clojure.core
  (:require
   [clojure.reflect :as reflect]))

;; ---
;; Java exception handling
;; ---

(try (/ (int 42) (int 0))
     (catch ArithmeticException e
       (str "Exception name: " e " >>> Message: "  (.getMessage e))))
;; => "Exception name: java.lang.ArithmeticException: Divide by zero >>> Message: Divide by zero"

```

### Explanation

This Clojure code is demonstrating how to interoperate with Java, specifically dealing with exception handling. Let's go through it in more detail:

1. **Namespace Definition (`ns`)**: `(ns interop-java-in-clojure.core (:require [clojure.reflect :as reflect]))`
   This line defines the namespace `interop-java-in-clojure.core` and includes the `clojure.reflect` library (with the alias `reflect`). The `clojure.reflect` library is used for accessing metadata of Clojure data structures and Java objects, though in this snippet it is not actually used.
2. **Try/Catch block**:
   This block tries to execute a division operation (`(/ (int 42) (int 0))`), which attempts to divide 42 by 0. This is an undefined operation and it will throw an `ArithmeticException` in Java.
3. **Exception Handling (`catch`)**: `(catch ArithmeticException e (str "Exception name: " e " >>> Message: " (.getMessage e)))`
   When the `ArithmeticException` is thrown, the `catch` block catches it. In this block, the exception `e` is caught and the name of the exception along with its message is concatenated into a string. The `.getMessage` function is a Java method that retrieves the detail message string of the exception.
4. **Output**: `"Exception name: java.lang.ArithmeticException: Divide by zero >>> Message: Divide by zero"`
   This is the output from the `catch` block when the exception is caught. It includes the type of the exception (`java.lang.ArithmeticException`), the default message from the exception (`Divide by zero`), and it's printed out in the string format specified in the `catch` block.

In summary, this Clojure code demonstrates how to handle Java exceptions within Clojure, specifically the `ArithmeticException` that is thrown when attempting to divide by zero.

## Java External Libraries

### Code

```clojure
(ns interop-java-in-clojure.core
  (:require
   [clojure.reflect :as reflect]))

;; ---
;; Java external libraries
;; ---

(import 'com.google.common.base.Strings)
(Strings/repeat "Hello" 3)
;; => "HelloHelloHello"
(Strings/titleCase "hello world")

(import 'com.google.common.base.Preconditions)

(defn divide [numerator denominator]
  (Preconditions/checkArgument (not= denominator 0) "Denominator must be non-zero")
  (/ numerator denominator))

(try
  (divide 42 0)
  (catch IllegalArgumentException e
    (str "Exception name: " e " >>> Message: "  (.getMessage e))))
;; => "Exception name: java.lang.IllegalArgumentException: Denominator must be non-zero >>> Message: Denominator must be non-zero"

(keys (reflect/reflect Strings))
;; => (:bases :flags :members)
```

### Explanation

Here's the explanation for the provided Clojure code:

1. `(ns interop-java-in-clojure.core (:require [clojure.reflect :as reflect]))`
   - This is the namespace declaration. It's declaring a namespace called "interop-java-in-clojure.core" and it's also requiring the `clojure.reflect` library and aliasing it as `reflect`.
2. `(import 'com.google.common.base.Strings)`
   - This line is importing the `Strings` class from the Google Guava library.
3. `(Strings/repeat "Hello" 3)`
   - It uses the `repeat` method from the `Strings` class to repeat the string "Hello" three times. It will return "HelloHelloHello".
4. `(Strings/titleCase "hello world")`
   - It converts the string "hello world" to title case, using the `titleCase` method from the `Strings` class.
5. `(import 'com.google.common.base.Preconditions)`
   - This line is importing the `Preconditions` class from the Google Guava library.
6. `(defn divide [numerator denominator] (Preconditions/checkArgument (not= denominator 0) "Denominator must be non-zero") (/ numerator denominator))`
   - This function takes two parameters, `numerator` and `denominator`. It checks the precondition that the denominator is not zero, using the `checkArgument` method from the `Preconditions` class. If the precondition fails, it will throw an `IllegalArgumentException` with the message "Denominator must be non-zero". If the precondition passes, it performs the division and returns the result.
7. `(try (divide 42 0) (catch IllegalArgumentException e (str "Exception name: " e " >>> Message: " (.getMessage e))))`
   - This block of code is trying to call the `divide` function with 42 as the numerator and 0 as the denominator. Since dividing by zero is not allowed by the `divide` function, it will throw an `IllegalArgumentException`. The `catch` block will then catch this exception, and return a string that contains the name of the exception and its message.
8. `(keys (reflect/reflect Strings))`
   - This line uses the `reflect` function from the `clojure.reflect` library to reflect on the `Strings` class, i.e., to examine its structure and metadata at runtime. It then uses the `keys` function to retrieve the keys of the map returned by the `reflect` function. These keys represent the top-level categories of information available via reflection for the `Strings` class.

## Local Java File

### Code

`Calculator.java`

```java
package interop_java_in_clojure;

public class Calculator {
  public int superAdd(int a, int b) {
    return a + b;
  }
}
```

`core.clj`

```clojure
(ns interop-java-in-clojure.core
  (:require
   [clojure.reflect :as reflect]))

;; ---
;; Local Java file
;; ---

;; Assume that Calculator.class is in the correct folder/package
;; and has a superAdd method that adds two numbers
;; See the repository

(import 'interop_java_in_clojure.Calculator)

(def calculator (Calculator.))
;; => #'interop-java-in-clojure.core/calculator
(class calculator)
;; => interop_java_in_clojure.Calculator
(.superAdd calculator 1 2)
;; => 3
```

### Explanation

This Clojure code is doing the following:

1. The `(ns interop-java-in-clojure.core (:require [clojure.reflect :as reflect]))` line is declaring a namespace named `interop-java-in-clojure.core`. It also imports the `clojure.reflect` module under the alias `reflect`.
2. The `(import 'interop_java_in_clojure.Calculator)` line is importing a Java class named `Calculator` from the `interop_java_in_clojure` package.
3. The `(def calculator (Calculator.))` line is defining a new instance of the `Calculator` class and binding it to the symbol `calculator`.
4. The `(class calculator)` line retrieves the class of the `calculator` instance, which is `interop_java_in_clojure.Calculator`.
5. The `(.superAdd calculator 1 2)` line is calling the `superAdd` method of the `calculator` instance, passing `1` and `2` as arguments. This function assumes that `Calculator` has a `superAdd` method that adds two numbers. The expected result of this call is `3`.

## Further Readings

- Official Clojure’s Java Interop Reference: [https://clojure.org/reference/java_interop](https://clojure.org/reference/java_interop)
