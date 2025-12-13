---
title: "Typing: tryCatch"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Introduction

The `tryCatch()` function is a useful utility that allows developers to execute code that may potentially throw exceptions safely. It provides a structured and type-safe way to handle exceptions by wrapping the result in a `Result` type (more on `Result` type: [Typing: Result](/en/learn/archived/software-engineering/programming-languages/dart/cookbook/typing-result)).

Exception handling is an essential aspect of writing robust and reliable code. However, if not appropriately handled, traditional exception-handling mechanisms, such as `try-catch` blocks, can sometimes lead to unhandled exceptions or unexpected crashes. The `tryCatch()` function addresses this issue by encapsulating the execution of code within a controlled environment.

By using the `tryCatch()` function, developers can execute code that may throw exceptions without the risk of unhandled exceptions propagating up the call stack. Instead of allowing exceptions to crash the program, the function catches any thrown exceptions and wraps them in an `Error` instance of the `Result` class. This ensures that exceptions are captured and can be handled in a controlled manner.

The `tryCatch()` function is particularly useful when developers want to handle exceptions gracefully and provide alternative behavior or error-handling mechanisms. By returning a `Result` type, the function allows the caller to quickly determine whether the operation was successful or resulted in an error. This promotes a more structured and predictable control flow, making the code more robust and easier to reason.

Additionally, the `tryCatch()` function provides access to the caught exception and stack trace through the `Error` instance. This allows developers to inspect and analyze the exception, enabling them to log relevant information, perform error recovery, or take appropriate actions based on the exception type.

Overall, the `tryCatch()` function enhances the error-handling capabilities of Dart by providing a structured and type-safe approach to handle exceptions. It promotes code reliability, maintainability, and robustness by encapsulating exception-prone code and allowing for controlled error handling and alternative behavior.

## Implementation

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/blob/main/contents/dart-cookbook/dart-cookbook-primary/src/typing/exception_utils.dart). And if you are interested in the test file for this code, you can see it [here](https://github.com/organiclever/ayokoding/blob/main/contents/dart-cookbook/dart-cookbook-primary/test/typing_utils/exception_utils_test.dart).

</aside>

```dart
import 'result.dart';

Result<T, ({Object ex, StackTrace st})> tryCatch<T>(T Function() operation) {
  try {
    T result = operation();
    return Ok(result);
  } catch (e, s) {
    return Error((ex: e, st: s));
  }
}

void main() {
  // this will wrap exception in Result type
  var res = tryCatch<bool>(() {
    dynamic foo = true;
    return foo++;
  });

  print(res.isError()); // Output: true
  print(res.isOk()); // Output: false

  res.tapError((err) {
    print(err.ex.runtimeType); // Output: NoSuchMethodError
    print(err.st.runtimeType); // Output: _StackTrace
  });
}
```

Explanation of the code:

1. The code begins by importing the `Result` class from a file named `result.dart`. This suggests that the `Result` class is defined in a separate file and is being used in this code.
2. The `tryCatch` function is defined, which takes a parameter `operation` representing a function that returns a value of type `T`. This function attempts to execute the `operation` and returns a `Result` instance. The result is wrapped in an Ok value if the `operation` executes successfully. If an exception occurs, the exception and the associated stack trace are wrapped in an `Error` value.
3. The `main` function is defined, which serves as the program's entry point.
4. The `tryCatch` function is called to wrap an exception in a `Result` type. In this case, the `operation` is an anonymous function that attempts to increment a variable `foo` of type `dynamic`, which will result in a `NoSuchMethodError` exception.
5. The `isError()` and `isOk()` methods are called on the `resError` instance to check if it represents an error or success. The output indicates that the result is an error.
6. The `tapError()` method is called on the `resError` instance to perform a side effect on the error value. Inside the callback function, the `ex` property of the error is accessed using the `err` parameter, and its runtime type is printed. Similarly, the runtime type of the stack trace (`st`) is also printed. The output shows that the exception is of type `NoSuchMethodError` and the stack trace is of type `_StackTrace`.
7. The `mapError()` method is called on the `resError` instance to transform the error value. In this case, the error value is mapped to the `ex` property of the error. The `tapError()` method is then called to perform a side effect on the transformed error value. The runtime type of the transformed error value is printed, which matches the original exception type (`NoSuchMethodError`).
8. The `tryCatch` function is called again to wrap a successful operation in a `Result` type. In this case, the `operation` is an anonymous function that converts the string "Hello" to lowercase.
9. The `map()` method is called multiple times on the `resOk` instance to transform the success value. Each `map()` call applies a different transformation to the value. The `tap()` method is then called to perform a side effect on the final transformed value. The transformed value is printed, which is "HELLOHELLO!".

This code demonstrates the usage of the `tryCatch` function to wrap exceptions in a `Result` type. It showcases various methods available on the `Result` instances, such as `isError()`, `isOk()`, `tapError()`, `mapError()`, `map()`, and `tap()`. These methods allow for safe and expressive handling of success and error values, enabling developers to handle exceptions and transform values in a structured and type-safe manner.
