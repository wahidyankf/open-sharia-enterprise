---
title: "Typing: Option"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Introduction

The `Option` type is a valuable programming construct designed to represent an optional value. It is usually used in functional programming languages and libraries to handle situations where a value may or may not be present. This type is called by different names based on the programming language; for example, `Option` in Rust, OCaml, F#, or `Maybe` in Haskell.

The `Option` type can encapsulate that a value can exist (`Some`) or be absent (`None`). This provides a way to explicitly handle the absence of a value without resorting to null references or checks.

In languages like Dart, where null safety is enforced, the `Option` type can be viewed as an extra layer of abstraction that promotes immutability, safety, and expressiveness when working with optional values.

The `Option` type usually comes with methods and operations that allow for manipulating the underlying value, such as mapping, filtering, and extracting the value if it exists. These methods permit the safe and concise handling of optional values without explicit null checks.

Using the `Option` type, developers can write more robust, easier-to-understand code less susceptible to null-related errors. It encourages a more functional programming style by providing a consistent and type-safe way to handle optional values throughout the codebase. Additionally, utilizing the `Option` type can create more readable code that is less error-prone and more maintainable in the long run.

## Implementation

<aside>
🗒️ You can find the code [here](https://github.com/organiclever/ayokoding/blob/main/contents/dart-cookbook/dart-cookbook-primary/src/typing/option.dart). And if you are interested in the test file for this code, you can see it [here](https://github.com/organiclever/ayokoding/blob/main/contents/dart-cookbook/dart-cookbook-primary/test/typing_utils/option_test.dart). Further note, [the Relude library](https://github.com/reazen/relude) (ReasonML) inspires the code in this section.

</aside>

```dart
sealed class Option<T> {
  bool isSome() {
    switch (this) {
      case (Some _):
        return true;
      case (None _):
        return false;
    }
  }

  bool isNone() {
    switch (this) {
      case (Some _):
        return false;
      case (None _):
        return true;
    }
  }

  bool isEqual(Option<T> other) {
    switch ((this, other)) {
      case (Some some, Some other):
        return some.value == other.value;
      case (None _, None _):
        return true;
      case _:
        return false;
    }
  }

  T getOrElse(T defVal) {
    switch (this) {
      case (Some some):
        return some.value;
      case (None _):
        return defVal;
    }
  }

  Option<T1> map<T1>(T1 Function(T) f) {
    switch (this) {
      case (Some some):
        return Some(f(some.value));
      case (None _):
        return None();
    }
  }

  Option<T1> flatmap<T1>(Option<T1> Function(T) f) {
    switch (this) {
      case (Some some):
        return f(some.value);
      case (None _):
        return None();
    }
  }

  Option<T> tap(void Function(T) f) {
    switch (this) {
      case (Some some):
        f(some.value);
        return Some(some.value);
      case (None _):
        return None();
    }
  }

  String toString() {
    switch (this) {
      case (Some some):
        return "Some(${some.value})";
      case (None _):
        return "None";
    }
  }
}

class Some<T> extends Option<T> {
  final T value;

  Some(this.value);
}

class None<T> extends Option<T> {
  final value = Null;
}
```

Explanation of the Dart code:

1. The code defines a sealed class `Option<T>` which represents an optional value that can either be `Some` (containing a value) or `None` (empty).
2. The `Option<T>` class has several methods:
   - `isSome()` checks if the option is `Some` and returns `true` if it is, `false` otherwise.
   - `isNone()` checks if the option is `None` and returns `true` if it is, `false` otherwise.
   - `isEqual(Option<T> other)` compares two options for equality. It returns `true` if both options are `Some` and their values are equal, or if both options are `None`. Otherwise, it returns `false`.
   - `getOrElse(T defVal)` returns the option's value if it is `Some`, or the provided default value `defVal` if it is `None`.
   - `map<T1>(T1 Function(T) f)` applies the function `f` to the value of the option if it is `Some`, and returns a new `Option<T1>` with the result. If the option is `None`, it returns a new `None`.
   - `flatmap<T1>(Option<T1> Function(T) f)` applies the function `f` to the value of the option if it is `Some`, and returns the result. If the option is `None`, it returns a new `None`.
   - `tap(void Function(T) f)` applies the function `f` to the option's value if it is `Some`, and returns the same option. If the option is `None`, it returns a new `None`. The `tap` method is used to perform side effects, such as logging or updating external state, without modifying the value of the option.
   - `toString()` returns a string representation of the option. If it is `Some`, it returns "Some(value)", where `value` is the option's value. If it is `None`, it returns "None".
3. The code also defines two subclasses of `Option<T>`:
   - `Some<T>` represents a `Some` option with a non-null value of type `T`. It has a constructor that takes a value.
   - `None<T>` represents a `None` option with no value. It has a `value` field set to `Null`.

This code provides a way to work with optional values in Dart using the `Option` class. It emphasizes immutability and safely handling values by encapsulating them within the `Option` type. This ensures the value is either present (`Some`) or absent (`None`), eliminating the need for null checks and reducing the risk of null pointer exceptions.

The `Option` class provides methods for safely accessing and manipulating the value, such as `getOrElse`, `map`, `flatmap`, and `tap`. The `tap` method is specifically designed to perform side effects, allowing you to execute code that has an effect outside of the `Option` instance. This can be useful for logging, updating external state, or triggering other actions without modifying the option's value.

Using the `tap` method, you can perform side effects in a controlled and predictable manner while maintaining the immutability and safety of the `Option` instance. This promotes a functional programming style where side effects are isolated and explicit, making the code easier to reason about and test.

```dart
void main() {
  var someNumber = Some(1);
  Option<int> noneNumber = None();
  var someString = Some("hello");
  Option<String> noneString = None();

  print(someNumber); // Output: Some(1)
  print(noneNumber); // Output: None
  print(someString); // Output: Some(hello)
  print(noneString); // Output: None

  print(someNumber.isSome()); // Output: true
  print(someNumber.isNone()); // Output: false

  print(someNumber.isEqual(Some(1))); // Output: true
  print(someNumber.isEqual(None())); // Output: false

  print(someNumber.getOrElse(2)); // Output: 1
  print(noneNumber.getOrElse(2)); // Output: 2

  print(someNumber.map((x) => x + 1)); // Output: Some(2)
  print(someString.map((x) => x + " world")); // Output: Some(hello world)
  print(noneNumber.map((x) => x + 1)); // Output: None
  print(someNumber.map((x) => x + 1).map((x) => x * 3)); // Output: Some(6)
  print(someNumber); // Output: Some(1)

  print(someNumber.flatmap((x) => Some(x + 1))); // Output: Some(2)
  print(someString
      .flatmap((x) => Some(x + " world"))); // Output: Some(hello world)
  print(noneNumber.flatmap((x) => Some(x + 1))); // Output: None
  print(someNumber
      .flatmap((x) => Some(x + 1))
      .flatmap((x) => Some(x * 3))); // Output: Some(6)

  print(someNumber.tap((p0) {
    print(p0);
  })); // Output: Some(1), but print 1 first
}
```

Let's break down the `main` function step-by-step:

1. `var someNumber = Some(1);`: Here we create a `Some` instance `someNumber` with the integer value `1`.
2. `Option<int> noneNumber = None();`: We create a `None` instance `noneNumber` representing no integer value.
3. `var someString = Some("hello");`: We create a `Some` instance `someString` with the string value "hello".
4. `Option<String> noneString = None();`: We create a `None` instance `noneString` representing no string value.
5. `print(someNumber);`: This line will print `Some(1)` because `someNumber` is an instance of `Some` containing value `1`.
6. `print(noneNumber);`: This line will print `None` as `noneNumber` is an instance of `None`.
7. `print(someString);`: This will print `Some(hello)` as `someString` is a `Some` instance containing the string "hello".
8. `print(noneString);`: This will print `None` because `noneString` is an instance of `None`.
9. `print(someNumber.isSome());`: This will print `true` as `someNumber` is an instance of `Some`.
10. `print(someNumber.isNone());`: This will print `false` as `someNumber` is not a `None` instance.
11. `print(someNumber.isEqual(Some(1)));`: This will print `true` because `someNumber` is equal to `Some(1)`.
12. `print(someNumber.isEqual(None()));`: This will print `false` as `someNumber` is not equal to `None`.
13. `print(someNumber.getOrElse(2));`: This will print `1` because `someNumber` is a `Some` instance and its value is `1`.
14. `print(noneNumber.getOrElse(2));`: This will print `2`, which is the default value, as `noneNumber` is a `None` instance.
15. `print(someNumber.map((x) => x + 1));`: This will print `Some(2)` because we are applying a function that increments the value inside `someNumber`.
16. `print(someString.map((x) => x + " world"));`: This will print `Some(hello world)` because we are applying a function that appends " world" to the value in `someString`.
17. `print(noneNumber.map((x) => x + 1));`: This will print `None` because `noneNumber` is a `None` instance and the `map` function won't change it.
18. `print(someNumber.map((x) => x + 1).map((x) => x * 3));`: This will print `Some(6)`. The value in `someNumber` is first incremented and then tripled.
19. `print(someNumber);`: This will print `Some(1)` because `someNumber` still holds the value `1`.
20. `print(someNumber.flatmap((x) => Some(x + 1)));`: This will print `Some(2)` because we apply a function that increments the value inside `someNumber` and wraps it in `Some`.
21. `print(someString.flatmap((x) => Some(x + " world")));`: This will print `Some(hello world)` because we are applying a function that appends " world" to the value in `someString` and wraps it in `Some`.
22. `print(noneNumber.flatmap((x) => Some(x + 1)));`: This will print `None` because `noneNumber` is a `None` instance and the `flatmap` function won't change it.
23. `print(someNumber.flatmap((x) => Some(x + 1)).flatmap((x) => Some(x * 3)));`: This will print `Some(6)`. The value in `someNumber` is first incremented, wrapped in `Some`, then tripled, and wrapped in `Some` again.
24. `print(someNumber.tap((p0) {print(p0);}));`: This will print `1` and then `Some(1)`. The `tap` function prints the value in `someNumber` and then prints `someNumber` itself.
