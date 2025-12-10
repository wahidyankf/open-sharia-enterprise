---
title: "Typing: Result"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Introduction

The `Result` type, known as `Result` in Rust, OCaml, and F#, and `Either` in Haskell, is a powerful programming construct that handles the outcome of an operation, representing either success or error. It provides a structured and type-safe way to handle different outcomes or states. The `Result` class in the Dart code includes methods like `isOk()` and `isError()` to determine if the result is a success or an error. This eliminates the need for manual type checks or null checks, making the code more robust and less error-prone.

The `Result` type promotes safer error handling compared to traditional approaches. Explicitly defining success and error cases ensures that all possible outcomes are accounted for. This reduces the risk of unexpected errors or unhandled exceptions. Additionally, the `Result` type encourages developers to handle errors gracefully by providing methods like `getOrElse()` and `getErrorOrElse()` to provide default values in case of errors. This prevents unexpected crashes or undefined behavior.

Using the `Result` type also eliminates the need for null references. Instead of relying on null checks, the `Result` class provides methods like `getOk()` and `getError()` that return an `Option` type. This enforces the safe handling of nullable values and eliminates the possibility of null pointer exceptions. The `Option` type ensures that developers explicitly handle the absence of a value, promoting more reliable and predictable code.

The `Result` type's methods, such as `map()`, `mapError()`, `flatmap()`, and `flatmapError()`, allow for seamless composition of operations. These methods enable developers to combine the value inside the result or chain results, ensuring type safety throughout the process. This reduces the likelihood of type-related errors and promotes code readability and maintainability.

The `Result` type enhances code reliability and maintainability by providing a structured and type-safe approach to handling different outcomes. It encourages developers to handle errors explicitly, eliminates null references, and promotes a functional programming style. These benefits make the `Result` type, known as `Result` in Rust, OCaml, and F#, and `Either` in Haskell, a safer alternative to traditional error handling approaches, reducing the risk of errors and improving the overall quality of the codebase.

## Implementation

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/blob/main/contents/dart-cookbook/dart-cookbook-primary/src/typing/result.dart). And if you are interested in the test file for this code, you can see it [here](https://github.com/organiclever/ayokoding/blob/main/contents/dart-cookbook/dart-cookbook-primary/test/typing_utils/result_test.dart). Further note, [the Relude library](https://github.com/reazen/relude) (ReasonML) inspires the code in this section.

</aside>

```dart
import 'option.dart';

sealed class Result<T, U> {
  bool isError() {
    switch (this) {
      case (Ok _):
        return false;
      case (Error _):
        return true;
    }
  }

  bool isOk() {
    switch (this) {
      case (Ok _):
        return true;
      case (Error _):
        return false;
    }
  }

  bool isEqual(Result<T, U> other) {
    switch ((this, other)) {
      case (Ok ok, Ok other):
        return ok.value == other.value;
      case (Error error, Error other):
        return error.value == other.value;
      default:
        return false;
    }
  }

  String toString() {
    switch (this) {
      case (Ok ok):
        return "Ok(${ok.value})";
      case (Error error):
        return "Error(${error.value})";
    }
  }

  Option<T> getOk() {
    switch (this) {
      case (Ok ok):
        return Some(ok.value);
      case (Error _):
        return None();
    }
  }

  Option<U> getError() {
    switch (this) {
      case (Ok _):
        return None();
      case (Error error):
        return Some(error.value);
    }
  }

  T getOrElse(T defVal) {
    switch (this) {
      case (Ok ok):
        return ok.value;
      case (Error _):
        return defVal;
    }
  }

  U getErrorOrElse(U defVal) {
    switch (this) {
      case (Ok _):
        return defVal;
      case (Error error):
        return error.value;
    }
  }

  Result<T1, U> map<T1>(T1 Function(T) f) {
    switch (this) {
      case (Ok ok):
        return Ok(f(ok.value));
      case (Error error):
        return Error(error.value);
    }
  }

  Result<T, U1> mapError<U1>(U1 Function(U) f) {
    switch (this) {
      case (Ok ok):
        return Ok(ok.value);
      case (Error error):
        return Error(f(error.value));
    }
  }

  Result<T1, U> flatmap<T1>(Result<T1, U> Function(T) f) {
    switch (this) {
      case (Ok ok):
        return f(ok.value);
      case (Error error):
        return Error(error.value);
    }
  }

  Result<T, U1> flatmapError<U1>(Result<T, U1> Function(U) f) {
    switch (this) {
      case (Ok ok):
        return Ok(ok.value);
      case (Error error):
        return f(error.value);
    }
  }

  Result<T1, U1> bimap<T1, U1>(T1 Function(T) f, U1 Function(U) g) {
    switch (this) {
      case (Ok ok):
        return Ok(f(ok.value));
      case (Error error):
        return Error(g(error.value));
    }
  }

  Result<T, U> tap(void Function(T) f) {
    switch (this) {
      case (Ok ok):
        f(ok.value);
        return Ok(ok.value);
      case (Error error):
        return Error(error.value);
    }
  }

  Result<T, U> tapError(void Function(U) f) {
    switch (this) {
      case (Ok ok):
        return Ok(ok.value);
      case (Error error):
        f(error.value);
        return Error(error.value);
    }
  }

  Result<T, U> bitap(void Function(T) f, void Function(U) g) {
    switch (this) {
      case (Ok ok):
        f(ok.value);
        return Ok(ok.value);
      case (Error error):
        g(error.value);
        return Error(error.value);
    }
  }
}

class Ok<T, U> extends Result<T, U> {
  final T value;

  Ok(this.value);
}

class Error<T, U> extends Result<T, U> {
  final U value;

  Error(this.value);
}
```

Explanation of the Dart code:

1. The code defines a sealed class `Result<T, U>` that represents a result that can either be an `Ok` value of type `T` or an `Error` value of type `U`. This sealed class ensures that all possible result types are explicitly defined and cannot be extended or modified outside of the file. It provides a structured and type-safe way to handle different outcomes or states of an operation.
2. The `Result` class provides several utility methods to work with the result. By examining its type, the `isError()` method checks if the result is an error. The `isOk()` method does the opposite, checking if the result succeeds. These methods allow developers to easily determine the outcome of an operation without resorting to manual type checks or error-prone null checks.
3. The `isEqual()` method compares two results for equality. It checks if both results are the same type (`Ok` or `Error`) and their values are equal. This method is proper when comparing the results of two operations to determine if they produced the same outcome.
4. The `toString()` method provides a string representation of the result. It returns a formatted string that includes the type of the result (`Ok` or `Error`) and the value it holds. This method is helpful for debugging and logging purposes, allowing developers to inspect the contents of a result quickly.
5. The `getOk()` and `getError()` methods retrieve the value wrapped inside the result. The `getOk()` method returns an `Option` type that represents the success value, while the `getError()` method returns an `Option` type that represents the error value. These methods allow developers to safely access the value without the risk of null pointer exceptions.
6. The `getOrElse()` and `getErrorOrElse()` methods retrieve the value from the result or provide a default value if it is an error. The `getOrElse()` method returns the success value if the result is `Ok`, or the provided default value if it is an `Error`. Similarly, the `getErrorOrElse()` method returns the error value if the result is `Error`, or the provided default value if it is `Ok`. These methods are helpful when handling errors and providing fallback values.
7. The `map()` and `mapError()` methods allow mapping the value inside the result to a new value. The `map()` method takes a function that transforms the success value and returns a new `Result` with the transformed value. The `mapError()` method does the same for the error value. These methods enable developers to perform operations on the result value while preserving the result type.
8. The `flatmap()` and `flatmapError()` methods allow chaining results together. They take a function that produces a new `Result` based on the value inside the current result. If the current result is `Ok`, the function is applied to the success value and returns a new `Result`. If the current result is `Error`, the function is not applied, and the current `Error` is returned. These methods help compose operations that depend on the outcome of previous operations.
9. The `bimap()` method allows simultaneous success and error values mapping. It takes two functions, one for transforming the success value and another for transforming the error value. It returns a new `Result` with the transformed values. This method is proper when the success and error values must be modified simultaneously.
10. The `tap()`, `tapError()`, and `bitap()` methods allow performing side effects on the value inside the result without modifying it. The `tap()` method takes a function that performs a side effect on the success value, while the `tapError()` method does the same for the error value. The `bitap()` method allows side effects on success and error values. These methods are helpful when additional actions need to be taken based on the result value without altering the result itself.
11. The `Ok` and `Error` classes are subclasses of `Result` and represent the success and error cases, respectively. They hold the actual values of types `T` and `U`. These classes provide a structured way to wrap and access the success and error values within the `Result` type.

This code provides a flexible and type-safe way to handle results that can be either successful or erroneous, allowing developers to handle different scenarios effectively.

For more information and examples, you can refer to the [Typing: Option](/en/learn/archived/software-engineering/programming-languages/dart/cookbook/typing-option) resource.

```dart
void main() {
  Result<String, ({int status, String message})> okRes = Ok("hello world!");
  Result<String, ({int status, String message})> errorRes =
      Error((status: 404, message: "URL not found"));

  print(okRes); // Output: Ok(hello world!)
  print(errorRes); // Output: Error((message: URL not found, status: 404)))

  print(okRes.isOk()); // Output: true
  print(okRes.isError()); // Output: false
  print(errorRes.isOk()); // Output: false
  print(errorRes.isError()); // Output: true

  print(okRes.isEqual(Ok("hello world!"))); // Output: true
  print(errorRes
      .isEqual(Error((status: 404, message: "URL not found")))); // Output: true

  print(okRes.getOk()); // Output: Some(hello world!)
  print(okRes.getError()); // Output: None()
  print(errorRes.getOk()); // Output: None()
  print(errorRes
      .getError()); // Output: Some((message: URL not found, status: 404))

  print(okRes.getOrElse("default value")); // Output: hello world!
  print(errorRes.getOrElse("default value")); // Output: default value

  print(okRes.getErrorOrElse((
    status: 500,
    message: "Internal server error",
  ))); // Output: (message: Internal server error, status: 500)
  print(errorRes.getErrorOrElse((
    status: 500,
    message: "Internal server error",
  ))); // Output: (message: URL not found, status: 404)

  print(okRes.map((value) => value.toUpperCase())); // Output: Ok(HELLO WORLD!)
  print(errorRes.map((value) => value
      .toUpperCase())); // Output: Error((message: URL not found, status: 404))

  print(okRes.mapError((err) => (
        status: err.status,
        message: err.message.toUpperCase()
      ))); // Output: Ok(hello world!)
  print(errorRes.mapError((err) => (
        status: err.status,
        message: err.message.toUpperCase()
      ))); // Output: Error((message: URL NOT FOUND, status: 404))

  print(okRes
      .flatmap((value) => Ok(value.toUpperCase()))); // Output: Ok(HELLO WORLD!)
  print(errorRes.flatmap((value) => Ok(value
      .toUpperCase()))); // Output: Error((message: URL not found, status: 404))

  print(okRes.flatmapError((err) => Error((
        status: 500,
        message: err.message.toUpperCase()
      )))); // Output: Ok(hello world!)
  print(errorRes.flatmapError((err) => Error((
        status: err.status,
        message: err.message.toUpperCase()
      )))); // Output: Error((message: URL NOT FOUND, status: 404))

  print(okRes.bimap(
      (value) => value.toUpperCase(),
      (err) => (
            status: err.status,
            message: err.message.toUpperCase()
          ))); // Output: Ok(HELLO WORLD!)
  print(errorRes.bimap(
      (value) => value.toUpperCase(),
      (err) => (
            status: err.status,
            message: err.message.toUpperCase()
          ))); // Output: Error((message: URL NOT FOUND, status: 404))

  print(okRes.tap((value) =>
      print(value))); // Output: print hello world! then Ok(hello world!)
  print(errorRes.tap((value) =>
      print(value))); // Output: Error((message: URL not found, status: 404))

  print(
      okRes.tapError((err) => print(err.message))); // Output: Ok(hello world!)
  print(errorRes.tapError((err) => print(err
      .message))); // Output: print URL not found then Error((message: URL not found, status: 404))

  print(okRes.bitap(
      (value) => print(value),
      (err) => print(
          err.message))); // Output: print hello world! then Ok(hello world!)
  print(errorRes.bitap(
      (value) => print(value),
      (err) => print(err
          .message))); // Output: print URL not found then Error((message: URL not found, status: 404)
}
```

Let's break down the `main` function step-by-step:

1. The code demonstrates the usage of the `Result` class and its methods. It creates two instances of `Result` - `okRes` and `errorRes`. `okRes` represents a successful result with the value "hello world!", while `errorRes` represents an error result with the value `{status: 404, message: "URL not found"}`.
2. The `print()` statements are used to output the results of various operations on the `Result` instances.
3. The `isOk()` and `isError()` methods are used to check if a result is a success or an error, respectively. The output of these methods indicates whether the result is an `Ok` or an `Error`.
4. The `isEqual()` method is used to compare two results for equality. It checks if the values and types of the results match.
5. The `getOk()` and `getError()` methods retrieve the success and error values from the `Result` instances, respectively. These methods return an `Option` type representing the value, allowing for safe handling of nullable values.
6. The `getOrElse()` and `getErrorOrElse()` methods retrieve the value from the result or provide a default value if it is an error. These methods return the value if the result is `Ok`, or the provided default value if it is an `Error`.
7. The `map()`, `mapError()`, `flatmap()`, and `flatmapError()` methods are used to transform the value inside the result or chain results together. These methods apply functions to the value or error value and return a new `Result` instance with the transformed value.
8. The `bimap()` method allows simultaneous success and error values mapping. It applies functions to both values and returns a new `Result` instance with the transformed values.
9. The `tap()`, `tapError()`, and `bitap()` methods are used to perform side effects on the value inside the result without modifying it. These methods execute functions with the value or error value and return the original `Result` instance.

The output of each operation is shown as comments in the code. This code demonstrates how the `Result` class can handle different outcomes or states of an operation in a structured and type-safe manner.
