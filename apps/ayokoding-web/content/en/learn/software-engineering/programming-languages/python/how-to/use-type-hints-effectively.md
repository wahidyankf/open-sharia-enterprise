---
title: "Use Type Hints Effectively"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 1000005
description: "Apply modern Python typing with generics, protocols, and static analysis"
tags: ["python", "type-hints", "mypy", "typing", "static-analysis"]
categories: ["learn"]
---

## Problem

Python's dynamic typing provides flexibility but can hide type errors until runtime. Type hints (PEP 484) enable static analysis while preserving Python's dynamic nature.

This guide shows how to use type hints effectively for better code quality and tooling support.

## Basic Type Hints

Start with simple type annotations for function parameters and return values.

```python
def calculate_discount(price, rate, apply):
    if apply:
        return price * (1 - rate)
    return price

def calculate_discount(price: float, rate: float, apply: bool) -> float:
    if apply:
        return price * (1 - rate)
    return price

username: str = "alice"
age: int = 30
is_active: bool = True

names: list[str] = ["Alice", "Bob"]
scores: dict[str, int] = {"alice": 95, "bob": 87}
unique_ids: set[int] = {1, 2, 3}
```

## Optional and Union Types

Handle values that might be None or multiple types.

```python
from typing import Optional, Union

def find_user(user_id: int) -> Optional[dict]:
    """Returns user dict or None if not found."""
    user = database.query(user_id)
    return user if user else None

def parse_value(input: str) -> int | float | None:
    try:
        return int(input)
    except ValueError:
        try:
            return float(input)
        except ValueError:
            return None

def format_id(id: int | str) -> str:
    return str(id)

from typing import Literal

def set_log_level(level: Literal["DEBUG", "INFO", "WARNING", "ERROR"]):
    logger.setLevel(level)

set_log_level("INFO")  # OK
set_log_level("TRACE")  # Type checker error
```

## Generic Types

Create reusable type-safe functions and classes.

```python
from typing import TypeVar, Generic

T = TypeVar('T')

def first_element(items: list[T]) -> T | None:
    return items[0] if items else None

numbers = [1, 2, 3]
first_num: int | None = first_element(numbers)  # int inferred

strings = ["a", "b", "c"]
first_str: str | None = first_element(strings)  # str inferred

class Stack(Generic[T]):
    def __init__(self) -> None:
        self._items: list[T] = []

    def push(self, item: T) -> None:
        self._items.append(item)

    def pop(self) -> T:
        return self._items.pop()

int_stack: Stack[int] = Stack()
int_stack.push(42)
value: int = int_stack.pop()

K = TypeVar('K')
V = TypeVar('V')

class Cache(Generic[K, V]):
    def __init__(self) -> None:
        self._data: dict[K, V] = {}

    def get(self, key: K) -> V | None:
        return self._data.get(key)

    def set(self, key: K, value: V) -> None:
        self._data[key] = value
```

## Protocols for Structural Typing

Define interfaces without inheritance (duck typing with type checking).

```python
from typing import Protocol

class Drawable(Protocol):
    def draw(self) -> str:
        ...

class Circle:
    def __init__(self, radius: float):
        self.radius = radius

    def draw(self) -> str:
        return f"Circle(r={self.radius})"

class Square:
    def __init__(self, side: float):
        self.side = side

    def draw(self) -> str:
        return f"Square(s={self.side})"

def render(shape: Drawable) -> None:
    print(shape.draw())

render(Circle(5.0))
render(Square(10.0))

from typing import runtime_checkable

@runtime_checkable
class Saveable(Protocol):
    def save(self) -> None:
        ...

def save_data(obj: Saveable) -> None:
    if isinstance(obj, Saveable):  # Runtime check
        obj.save()
```

## TypedDict for Structured Dictionaries

Type dictionaries with known structure.

```python
from typing import TypedDict

class UserDict(TypedDict):
    id: int
    name: str
    email: str
    age: int

def create_user(user_data: UserDict) -> None:
    # Type checker knows the structure
    print(f"User: {user_data['name']}")
    # print(user_data['invalid'])  # Type checker error

user: UserDict = {
    "id": 1,
    "name": "Alice",
    "email": "alice@example.com",
    "age": 30
}

class UserDictOptional(TypedDict, total=False):
    id: int
    name: str
    email: str
    age: int
    phone: str

from dataclasses import dataclass

@dataclass
class User:
    id: int
    name: str
    email: str
    age: int = 0  # Optional with default

```

## Callable Types

Type functions and callbacks.

```python
from typing import Callable

def apply_operation(
    value: int,
    operation: Callable[[int], int]
) -> int:
    return operation(value)

def double(x: int) -> int:
    return x * 2

result = apply_operation(5, double)  # 10

def process_items(
    items: list[str],
    processor: Callable[[str, int], str]
) -> list[str]:
    return [processor(item, i) for i, item in enumerate(items)]

def add_index(item: str, index: int) -> str:
    return f"{index}: {item}"

from typing import Any

def register_callback(
    callback: Callable[..., None]
) -> None:
    callback()  # Can be called with any arguments
```

## Type Aliases

Create readable names for complex types.

```python
from typing import TypeAlias

UserId: TypeAlias = int
UserName: TypeAlias = str
UserData: TypeAlias = dict[str, int | str]

def get_user(user_id: UserId) -> UserData:
    return {
        "id": user_id,
        "name": "Alice",
        "age": 30
    }

from typing import Callable

Handler: TypeAlias = Callable[[str], None]
Middleware: TypeAlias = Callable[[Handler], Handler]

def create_middleware(handler: Handler) -> Middleware:
    def middleware(next_handler: Handler) -> Handler:
        def wrapper(request: str) -> None:
            print("Before")
            next_handler(request)
            print("After")
        return wrapper
    return middleware
```

## Static Type Checking with mypy

Use mypy to verify type correctness.

```bash
pip install mypy

mypy your_script.py

mypy --strict your_script.py
```

```python
[mypy]
python_version = 3.10
warn_return_any = True
warn_unused_configs = True
disallow_untyped_defs = True
disallow_any_generics = True
check_untyped_defs = True
```

## Gradual Typing

Add type hints incrementally without breaking existing code.

```python
def calculate_total(items: list[dict]) -> float:
    return sum(item['price'] * item['quantity'] for item in items)

def validate_email(email: str) -> bool:
    return '@' in email and '.' in email

from typing import Any

def process_legacy_data(data: Any) -> Any:
    # TODO: Add proper types later
    return transform(data)

result = complex_legacy_function()  # type: ignore
```

## Type Checking Best Practices

```python
def example() -> None:
    value = [1, 2, 3]
    reveal_type(value)  # Revealed type is "builtins.list[builtins.int]"

from typing import assert_type

def get_number() -> int:
    return 42

value = get_number()
assert_type(value, int)  # Passes type check

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from complex_module import ComplexType

def process(data: 'ComplexType') -> None:  # String annotation avoids runtime import
    ...
```

## Summary

Type hints provide documentation and enable static analysis while preserving Python's dynamic nature. Start with basic annotations for function parameters and return values - simple types like str, int, and bool clarify contracts without adding complexity. Use Optional for nullable values and Union (or | in Python 3.10+) for parameters that accept multiple types.

Generic types create reusable, type-safe components. TypeVar defines type parameters that preserve relationships between inputs and outputs, letting type checkers infer specific types from usage. Generic classes like Stack[T] provide compile-time type safety for containers and data structures.

Protocols enable structural typing - define interfaces through methods rather than inheritance. Objects that implement the required methods satisfy the protocol without explicit inheritance, combining duck typing's flexibility with type checking's safety. Runtime checkable protocols allow isinstance checks when needed.

TypedDict types dictionaries with known structure, though dataclasses usually provide better IDE support and runtime validation. Callable types describe functions and callbacks, specifying parameter and return types for higher-order functions. Type aliases create readable names for complex type expressions.

Static type checking with mypy verifies type correctness without runtime overhead. Configure mypy's strictness through command-line flags or configuration files, balancing strictness with pragmatism. Gradual typing lets you add hints incrementally - start with critical functions and new code, using Any for complex legacy types that will be refined later.

Type hints improve code quality through better IDE support, earlier error detection, and clearer interfaces. They serve as executable documentation that type checkers verify automatically. The investment in type hints pays dividends through reduced bugs and improved maintainability.

## Related Content

- [Python Best Practices](/en/learn/software-engineering/programming-languages/python/explanation/best-practices)
- [How to Write Pythonic Code](/en/learn/software-engineering/programming-languages/python/how-to/write-pythonic-code)
- [How to Use Collections Effectively](/en/learn/software-engineering/programming-languages/python/how-to/use-collections-effectively)
