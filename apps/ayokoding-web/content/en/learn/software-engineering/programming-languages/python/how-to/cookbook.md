---
title: "Cookbook"
date: 2025-12-17T00:00:00+07:00
draft: false
description: Practical recipes and patterns for idiomatic Python programming
weight: 1000001
---

**Ready to level up your Python skills?** This cookbook provides practical, battle-tested recipes for solving real-world problems with idiomatic Python code. Whether you're building web services, data pipelines, or automation tools, you'll find proven patterns and techniques used in production by companies like Google, Spotify, and Instagram.

## 🎯 What You'll Learn

By working through this cookbook, you will be able to:

1. **Master Pythonic Patterns** - Write clean, idiomatic code using comprehensions, generators, and decorators
2. **Use Type Hints Effectively** - Add static type checking to catch bugs before runtime
3. **Handle Concurrency** - Implement async/await patterns for I/O-bound operations
4. **Work with Context Managers** - Manage resources safely with proper cleanup
5. **Process Collections** - Use powerful list/dict/set comprehensions and functional operations
6. **Manage Configuration** - Load and validate application settings securely
7. **Write Robust Tests** - Create comprehensive test suites with pytest
8. **Build CLIs** - Create user-friendly command-line applications

## 📋 Prerequisites

Before using this cookbook, you should:

- ✅ Complete the [Python Beginner tutorial](/en/learn/software-engineering/programming-languages/python/tutorials/beginner) - or have equivalent experience with Python fundamentals
- ✅ Understand Python syntax, types, and control flow
- ✅ Know how to work with lists, dictionaries, and sets
- ✅ Understand functions, classes, and modules
- ✅ Be familiar with pip and virtual environments
- ✅ Have Python 3.9+ installed (3.11+ recommended for best performance)

## 🎯 What's in This Cookbook

- **Collection Operations** - Comprehensions, filtering, grouping
- **Type Hints** - Static typing, generics, protocols
- **Decorators** - Caching, timing, validation
- **Context Managers** - Resource management patterns
- **Async/Await** - Concurrent I/O operations
- **Error Handling** - Exception patterns and best practices
- **File Operations** - Reading, writing, path handling
- **Testing Patterns** - pytest fixtures and parametrization
- **Configuration** - Loading and validating settings
- **CLI Development** - Building command-line tools

---

## 🔷 Collection Operations

Python's collection operations are powerful and expressive. Master these patterns for cleaner code.

### Recipe 1: List Comprehensions

**Problem**: You need to transform or filter a list with minimal code.

**Solution**:

```python
numbers = [1, 2, 3, 4, 5]
squares = [n ** 2 for n in numbers]

evens = [n for n in numbers if n % 2 == 0]

matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flattened = [item for row in matrix for item in row]

positive_squares = [n ** 2 for n in [-2, -1, 0, 1, 2] if n > 0]

colors = ['red', 'blue']
sizes = ['S', 'M', 'L']
combinations = [(color, size) for color in colors for size in sizes]
```

**When to use**: When you need to create a new list from an existing iterable with transformation or filtering.

**See Also**:

- [Recipe 2: Dictionary Comprehensions](#recipe-2-dictionary-comprehensions) - Transform to dictionaries
- [Recipe 5: Generator Expressions](#recipe-5-generator-expressions) - Memory-efficient iteration

---

### Recipe 2: Dictionary Comprehensions

**Problem**: You need to build or transform dictionaries efficiently.

**Solution**:

```python
numbers = [1, 2, 3, 4, 5]
squares_dict = {n: n ** 2 for n in numbers}

keys = ['name', 'age', 'city']
values = ['Alice', 30, 'NYC']
person = {k: v for k, v in zip(keys, values)}

scores = {'Alice': 85, 'Bob': 92, 'Charlie': 78, 'Diana': 95}
high_scores = {name: score for name, score in scores.items() if score >= 90}

celsius = {'morning': 20, 'afternoon': 25, 'evening': 22}
fahrenheit = {time: (temp * 9/5) + 32 for time, temp in celsius.items()}

original = {'a': 1, 'b': 2, 'c': 3}
swapped = {v: k for k, v in original.items()}
```

**When to use**: When building or transforming dictionaries, especially for data processing.

---

### Recipe 3: Set Comprehensions and Operations

**Problem**: You need unique collections or set operations.

**Solution**:

```python
numbers = [1, 2, 2, 3, 3, 3, 4, 5]
unique_squares = {n ** 2 for n in numbers}

items = ['apple', 'banana', 'apple', 'cherry', 'banana']
unique_items = list(dict.fromkeys(items))

set_a = {1, 2, 3, 4, 5}
set_b = {4, 5, 6, 7, 8}

union = set_a | set_b

intersection = set_a & set_b

difference = set_a - set_b

sym_diff = set_a ^ set_b

is_subset = {1, 2} <= set_a  # True
is_superset = set_a >= {1, 2}  # True
```

**When to use**: When uniqueness matters or you need mathematical set operations.

---

### Recipe 4: defaultdict and Counter

**Problem**: You need dictionaries with default values or counting capabilities.

**Solution**:

```python
from collections import defaultdict, Counter

word_lists = defaultdict(list)
words = [('fruit', 'apple'), ('fruit', 'banana'), ('veggie', 'carrot')]
for category, item in words:
    word_lists[category].append(item)

word_counts = defaultdict(int)
sentence = "the quick brown fox jumps over the lazy dog"
for word in sentence.split():
    word_counts[word] += 1

text = "hello world"
letter_counts = Counter(text)

letter_counts.most_common(3)

c1 = Counter(['a', 'b', 'c', 'a'])
c2 = Counter(['b', 'c', 'd', 'b'])
combined = c1 + c2

words = ['apple', 'banana', 'apple', 'cherry', 'banana', 'apple']
fruit_counts = Counter(words)
```

**When to use**: When you need default values in dicts or counting frequencies.

---

### Recipe 5: Generator Expressions

**Problem**: You need memory-efficient iteration over large datasets.

**Solution**:

```python
numbers = range(1, 1000000)
squares_gen = (n ** 2 for n in numbers)  # No computation yet

for square in squares_gen:
    if square > 100:
        print(square)
        break  # Only computed up to this point

def fibonacci():
    """Generate fibonacci sequence infinitely"""
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b

fib = fibonacci()
first_ten = [next(fib) for _ in range(10)]

def read_large_file(filepath):
    """Memory-efficient file reading"""
    with open(filepath, 'r') as f:
        for line in f:
            yield line.strip()

for line in read_large_file('data.txt'):
    process(line)  # Only one line in memory at a time

def read_csv(filepath):
    with open(filepath) as f:
        for line in f:
            yield line.strip().split(',')

def filter_rows(rows, min_value):
    for row in rows:
        if int(row[1]) >= min_value:
            yield row

rows = read_csv('data.csv')
filtered = filter_rows(rows, 100)
for row in filtered:
    print(row)
```

**When to use**: When working with large datasets or infinite sequences where you don't need all values in memory.

---

### Recipe 6: Grouping with groupby and itertools

**Problem**: You need to group items by a key or perform complex iterations.

**Solution**:

```python
from itertools import groupby, chain, combinations, product

data = [
    {'name': 'Alice', 'dept': 'Engineering'},
    {'name': 'Bob', 'dept': 'Sales'},
    {'name': 'Charlie', 'dept': 'Engineering'},
    {'name': 'Diana', 'dept': 'Sales'}
]

from collections import defaultdict
grouped = defaultdict(list)
for person in data:
    grouped[person['dept']].append(person['name'])

data_sorted = sorted(data, key=lambda x: x['dept'])
for dept, group in groupby(data_sorted, key=lambda x: x['dept']):
    people = [person['name'] for person in group]
    print(f"{dept}: {people}")

nested = [[1, 2, 3], [4, 5], [6, 7, 8]]
flattened = list(chain.from_iterable(nested))

flattened = list(chain(*nested))

items = ['A', 'B', 'C']
pairs = list(combinations(items, 2))

colors = ['red', 'blue']
sizes = ['S', 'M']
variants = list(product(colors, sizes))
```

**When to use**: When grouping data, flattening lists, or generating combinations/permutations.

---

## 🔷 Type Hints and Type Safety

Python 3.5+ supports type hints for static type checking with tools like mypy.

### Recipe 7: Basic Type Hints

**Problem**: You want static type checking to catch bugs early.

**Solution**:

```python

def greet(name: str) -> str:
    return f"Hello, {name}"

def sum_numbers(numbers: list[int]) -> int:
    return sum(numbers)

def get_scores() -> dict[str, int]:
    return {'Alice': 95, 'Bob': 87}

def unique_items(items: list[str]) -> set[str]:
    return set(items)

def find_user(user_id: int) -> str | None:
    users = {1: 'Alice', 2: 'Bob'}
    return users.get(user_id)  # Returns str or None

def parse_value(value: int | str) -> int:
    if isinstance(value, str):
        return int(value)
    return value

def get_coordinates() -> tuple[float, float]:
    return (40.7128, -74.0060)

def divide(a: int, b: int) -> tuple[int, int]:
    return a // b, a % b  # quotient, remainder

def process_data() -> dict[str, list[tuple[int, str]]]:
    return {
        'users': [(1, 'Alice'), (2, 'Bob')],
        'admins': [(10, 'Admin')]
    }
```

**When to use**: Always! Type hints improve code clarity and catch bugs before runtime.

---

### Recipe 8: Advanced Type Hints

**Problem**: You need generics, protocols, or type variables for flexible typing.

**Solution**:

```python
from typing import TypeVar, Generic, Protocol, Callable, Any, NewType

T = TypeVar('T')

def first_element(items: list[T]) -> T | None:
    return items[0] if items else None

numbers = [1, 2, 3]
first_num = first_element(numbers)  # Type: int | None

strings = ['a', 'b', 'c']
first_str = first_element(strings)  # Type: str | None

class Stack(Generic[T]):
    def __init__(self) -> None:
        self.items: list[T] = []

    def push(self, item: T) -> None:
        self.items.append(item)

    def pop(self) -> T | None:
        return self.items.pop() if self.items else None

    def is_empty(self) -> bool:
        return len(self.items) == 0

int_stack: Stack[int] = Stack()
int_stack.push(1)
int_stack.push(2)
value: int | None = int_stack.pop()

class Drawable(Protocol):
    def draw(self) -> str:
        ...

class Circle:
    def draw(self) -> str:
        return "⭕"

class Square:
    def draw(self) -> str:
        return "⬜"

def render(shape: Drawable) -> None:
    print(shape.draw())

render(Circle())  # ✅ Works - has draw() method
render(Square())  # ✅ Works - has draw() method

def apply_operation(x: int, operation: Callable[[int], int]) -> int:
    return operation(x)

result = apply_operation(5, lambda x: x ** 2)  # 25

UserId = NewType('UserId', int)
OrderId = NewType('OrderId', int)

def get_user(user_id: UserId) -> str:
    return f"User {user_id}"

user_id = UserId(123)
order_id = OrderId(456)
get_user(user_id)  # ✅ OK
```

**When to use**: When building reusable generic code or defining structural types.

---

## 🔷 Decorators

Decorators are powerful Python features for modifying function behavior.

### Recipe 9: Timing Decorator

**Problem**: You want to measure function execution time.

**Solution**:

```python
import functools
import time
from typing import Callable, Any

def timer(func: Callable[..., Any]) -> Callable[..., Any]:
    """Measure and print function execution time"""
    @functools.wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        start = time.perf_counter()
        result = func(*args, **kwargs)
        end = time.perf_counter()
        print(f"{func.__name__} took {end - start:.4f} seconds")
        return result
    return wrapper

@timer
def slow_function():
    time.sleep(1)
    return "Done"

result = slow_function()

def retry(max_attempts: int = 3, delay: float = 1.0):
    """Retry function on exception"""
    def decorator(func: Callable[..., Any]) -> Callable[..., Any]:
        @functools.wraps(func)
        def wrapper(*args: Any, **kwargs: Any) -> Any:
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_attempts - 1:
                        raise
                    print(f"Attempt {attempt + 1} failed: {e}")
                    time.sleep(delay)
        return wrapper
    return decorator

@retry(max_attempts=3, delay=0.5)
def unreliable_network_call():
    # Might fail occasionally
    import random
    if random.random() < 0.7:
        raise ConnectionError("Network error")
    return "Success"
```

**When to use**: When you need to measure performance or add retry logic.

---

### Recipe 10: Caching Decorator

**Problem**: You want to cache expensive function results.

**Solution**:

```python
import functools
from typing import Any

@functools.lru_cache(maxsize=128)
def fibonacci(n: int) -> int:
    """Fibonacci with memoization"""
    if n < 2:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

result = fibonacci(100)

def memoize(func: Callable[..., Any]) -> Callable[..., Any]:
    """Simple memoization decorator"""
    cache: dict[str, Any] = {}

    @functools.wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        # Create cache key from arguments
        key = str(args) + str(kwargs)
        if key not in cache:
            cache[key] = func(*args, **kwargs)
        return cache[key]

    # Add cache control methods
    wrapper.cache = cache
    wrapper.clear_cache = lambda: cache.clear()
    return wrapper

@memoize
def expensive_computation(x: int, y: int) -> int:
    print(f"Computing {x} + {y}")
    time.sleep(1)
    return x + y

result1 = expensive_computation(5, 3)  # Prints "Computing 5 + 3"
result2 = expensive_computation(5, 3)  # Returns cached result
```

**When to use**: When function results are expensive to compute and arguments repeat.

---

### Recipe 11: Validation Decorator

**Problem**: You want to validate function arguments automatically.

**Solution**:

```python
import functools
from typing import Callable, Any

def validate_positive(func: Callable[..., Any]) -> Callable[..., Any]:
    """Ensure all numeric arguments are positive"""
    @functools.wraps(func)
    def wrapper(*args: Any, **kwargs: Any) -> Any:
        for arg in args:
            if isinstance(arg, (int, float)) and arg <= 0:
                raise ValueError(f"Argument must be positive: {arg}")
        for value in kwargs.values():
            if isinstance(value, (int, float)) and value <= 0:
                raise ValueError(f"Argument must be positive: {value}")
        return func(*args, **kwargs)
    return wrapper

@validate_positive
def calculate_area(width: float, height: float) -> float:
    return width * height

area = calculate_area(5, 10)  # ✅ OK: 50

def validate_types(**type_hints: type):
    """Validate argument types at runtime"""
    def decorator(func: Callable[..., Any]) -> Callable[..., Any]:
        @functools.wraps(func)
        def wrapper(*args: Any, **kwargs: Any) -> Any:
            # Get function signature
            import inspect
            sig = inspect.signature(func)
            bound = sig.bind(*args, **kwargs)

            # Validate types
            for name, value in bound.arguments.items():
                if name in type_hints:
                    expected_type = type_hints[name]
                    if not isinstance(value, expected_type):
                        raise TypeError(
                            f"{name} must be {expected_type.__name__}, "
                            f"got {type(value).__name__}"
                        )
            return func(*args, **kwargs)
        return wrapper
    return decorator

@validate_types(name=str, age=int)
def create_user(name: str, age: int) -> str:
    return f"{name} is {age} years old"

user = create_user("Alice", 30)  # ✅ OK
```

**When to use**: When you need runtime validation of function arguments.

---

## 🔷 Context Managers

Context managers ensure proper resource cleanup using `with` statements.

### Recipe 12: File Context Manager

**Problem**: You need to ensure files are properly closed even if errors occur.

**Solution**:

```python
with open('data.txt', 'r') as f:
    content = f.read()
    # File automatically closed, even if exception occurs

with open('input.txt', 'r') as infile, open('output.txt', 'w') as outfile:
    for line in infile:
        outfile.write(line.upper())

class DatabaseConnection:
    def __init__(self, connection_string: str):
        self.connection_string = connection_string
        self.connection = None

    def __enter__(self):
        print(f"Connecting to {self.connection_string}")
        self.connection = f"Connection({self.connection_string})"
        return self.connection

    def __exit__(self, exc_type, exc_val, exc_tb):
        print("Closing connection")
        self.connection = None
        # Return False to propagate exceptions
        return False

with DatabaseConnection("postgresql://localhost") as conn:
    print(f"Using {conn}")

from contextlib import contextmanager

@contextmanager
def temporary_value(obj, attr, new_value):
    """Temporarily change object attribute"""
    old_value = getattr(obj, attr)
    setattr(obj, attr, new_value)
    try:
        yield obj
    finally:
        setattr(obj, attr, old_value)

class Config:
    debug = False

config = Config()
print(config.debug)  # False
with temporary_value(config, 'debug', True):
    print(config.debug)  # True (temporarily)
print(config.debug)  # False (restored)
```

**When to use**: Whenever you need guaranteed resource cleanup (files, connections, locks).

---

### Recipe 13: Timer Context Manager

**Problem**: You want to measure code block execution time.

**Solution**:

```python
import time
from contextlib import contextmanager
from typing import Generator

@contextmanager
def timer(label: str = "Operation") -> Generator[None, None, None]:
    """Context manager to measure execution time"""
    start = time.perf_counter()
    try:
        yield
    finally:
        end = time.perf_counter()
        print(f"{label} took {end - start:.4f} seconds")

with timer("Database query"):
    time.sleep(0.5)
    result = "data"

class Timer:
    def __init__(self, label: str = "Operation"):
        self.label = label
        self.elapsed = 0.0

    def __enter__(self):
        self.start = time.perf_counter()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.end = time.perf_counter()
        self.elapsed = self.end - self.start
        print(f"{self.label} took {self.elapsed:.4f} seconds")
        return False

with Timer("Complex operation") as t:
    time.sleep(1)

print(f"Total time was {t.elapsed:.2f}s")
```

**When to use**: When profiling code sections or measuring performance.

---

## 🔷 Async/Await Patterns

Python's async/await enables efficient concurrent I/O operations.

### Recipe 14: Basic Async Operations

**Problem**: You need to perform multiple I/O operations concurrently.

**Solution**:

```python
import asyncio

async def fetch_data(url: str) -> str:
    """Simulate async data fetching"""
    print(f"Fetching {url}")
    await asyncio.sleep(1)  # Simulate network delay
    return f"Data from {url}"

async def main():
    result = await fetch_data("https://api.example.com")
    print(result)

asyncio.run(main())

async def fetch_all():
    urls = [
        "https://api.example.com/users",
        "https://api.example.com/posts",
        "https://api.example.com/comments"
    ]

    # Run all concurrently
    results = await asyncio.gather(*[fetch_data(url) for url in urls])
    return results

results = asyncio.run(fetch_all())

async def main_with_tasks():
    task1 = asyncio.create_task(fetch_data("url1"))
    task2 = asyncio.create_task(fetch_data("url2"))
    task3 = asyncio.create_task(fetch_data("url3"))

    # Wait for all tasks
    results = await asyncio.gather(task1, task2, task3)
    return results
```

**When to use**: When you have multiple I/O-bound operations that can run concurrently.

---

### Recipe 15: Async HTTP Requests

**Problem**: You need to make multiple HTTP requests efficiently.

**Solution**:

```python
import asyncio
import aiohttp
from typing import Any

async def fetch_url(session: aiohttp.ClientSession, url: str) -> str:
    """Fetch URL content asynchronously"""
    async with session.get(url) as response:
        return await response.text()

async def fetch_multiple_urls(urls: list[str]) -> list[str]:
    """Fetch multiple URLs concurrently"""
    async with aiohttp.ClientSession() as session:
        tasks = [fetch_url(session, url) for url in urls]
        results = await asyncio.gather(*tasks)
        return results

urls = [
    'https://httpbin.org/delay/1',
    'https://httpbin.org/delay/1',
    'https://httpbin.org/delay/1'
]
results = asyncio.run(fetch_multiple_urls(urls))

async def fetch_with_error_handling(url: str) -> dict[str, Any]:
    """Fetch URL with error handling"""
    try:
        async with aiohttp.ClientSession() as session:
            async with session.get(url, timeout=aiohttp.ClientTimeout(total=5)) as response:
                return {
                    'url': url,
                    'status': response.status,
                    'data': await response.text()
                }
    except asyncio.TimeoutError:
        return {'url': url, 'error': 'Timeout'}
    except Exception as e:
        return {'url': url, 'error': str(e)}

async def fetch_stream(url: str):
    """Stream large response line by line"""
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            async for line in response.content:
                yield line.decode('utf-8')
```

**When to use**: When making multiple HTTP requests or streaming large responses.

**See Also**:

- [Recipe 33-35: Web and API Operations](#-web-and-api-operations) - Synchronous HTTP requests
- [Recipe 14: Basic Async Operations](#recipe-14-basic-async-operations) - Async fundamentals

---

### Recipe 16: Async Queue Pattern

**Problem**: You need producer-consumer pattern with async workers.

**Solution**:

```python
import asyncio
from asyncio import Queue

async def producer(queue: Queue, num_items: int):
    """Produce items to queue"""
    for i in range(num_items):
        item = f"Item {i}"
        await queue.put(item)
        print(f"Produced: {item}")
        await asyncio.sleep(0.1)

    # Signal completion
    await queue.put(None)

async def consumer(queue: Queue, consumer_id: int):
    """Consume items from queue"""
    while True:
        item = await queue.get()

        if item is None:
            # Put None back for other consumers
            await queue.put(None)
            break

        print(f"Consumer {consumer_id} processing: {item}")
        await asyncio.sleep(0.5)  # Simulate work
        queue.task_done()

async def main():
    queue: Queue = Queue(maxsize=10)

    # Start producer and multiple consumers
    prod_task = asyncio.create_task(producer(queue, 10))
    consumer_tasks = [
        asyncio.create_task(consumer(queue, i))
        for i in range(3)
    ]

    # Wait for producer to finish
    await prod_task

    # Wait for all items to be processed
    await queue.join()

    # Wait for consumers to finish
    await asyncio.gather(*consumer_tasks)

asyncio.run(main())
```

**When to use**: When you need work distribution across multiple async workers.

---

## 🔷 Error Handling

Robust error handling is critical for production code.

### Recipe 17: Exception Patterns

**Problem**: You need to handle errors gracefully with proper cleanup.

**Solution**:

```python
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero")

try:
    value = int("not a number")
except (ValueError, TypeError) as e:
    print(f"Conversion error: {e}")

import logging

try:
    risky_operation()
except Exception as e:
    logging.error(f"Operation failed: {e}", exc_info=True)
    raise  # Re-raise after logging

try:
    file = open('data.txt', 'r')
    content = file.read()
except FileNotFoundError:
    print("File not found")
    content = ""
except IOError as e:
    print(f"I/O error: {e}")
    content = ""
else:
    # Runs if no exception occurred
    print("File read successfully")
finally:
    # Always runs, even if exception occurs
    if 'file' in locals():
        file.close()

class ValidationError(Exception):
    """Custom exception for validation failures"""
    def __init__(self, field: str, message: str):
        self.field = field
        self.message = message
        super().__init__(f"{field}: {message}")

class InvalidEmailError(ValidationError):
    """Specific validation error for emails"""
    pass

def validate_email(email: str):
    if '@' not in email:
        raise InvalidEmailError('email', 'Must contain @')

try:
    validate_email('invalid')
except InvalidEmailError as e:
    print(f"Validation failed - {e.field}: {e.message}")

from contextlib import contextmanager

@contextmanager
def handle_errors(error_handler):
    """Context manager for centralized error handling"""
    try:
        yield
    except Exception as e:
        error_handler(e)

def log_error(error: Exception):
    logging.error(f"Error occurred: {error}")

with handle_errors(log_error):
    risky_operation()
```

**When to use**: Always! Proper error handling prevents crashes and aids debugging.

**See Also**:

- [Recipe 18: Exception Chaining](#recipe-18-exception-chaining) - Preserve error context across layers
- [Recipe 37-46: Troubleshooting](#-troubleshooting) - Common Python errors and solutions
- [Recipe 12: File Context Manager](#recipe-12-file-context-manager) - Safe resource cleanup with context managers

---

### Recipe 18: Exception Chaining

**Problem**: You need to preserve error context when catching and re-raising exceptions.

**Solution**:

```python
def load_config(filename: str):
    try:
        with open(filename) as f:
            return json.load(f)
    except FileNotFoundError as e:
        raise ConfigError(f"Config file not found: {filename}") from e
    except json.JSONDecodeError as e:
        raise ConfigError(f"Invalid JSON in config: {filename}") from e

try:
    config = load_config("config.json")
except ConfigError as e:
    print(f"Error: {e}")
    print(f"Caused by: {e.__cause__}")
    # Output: Error: Config file not found: config.json
    #         Caused by: [Errno 2] No such file or directory: 'config.json'
```

**Advanced example - Exception context preservation**:

```python
import logging

class DatabaseError(Exception):
    """Custom exception with context preservation."""
    pass

class ConnectionError(DatabaseError):
    """Database connection failed."""
    pass

def connect_to_database(url: str):
    """Connect with detailed error chain."""
    try:
        # Simulate connection attempt
        if "invalid" in url:
            raise ValueError(f"Invalid URL format: {url}")

        # Simulate network error
        raise OSError("Connection refused")

    except ValueError as e:
        # Explicitly chain - shows both original and new error
        raise ConnectionError(f"Failed to parse database URL") from e
    except OSError as e:
        # Chain network errors
        raise ConnectionError(f"Failed to connect to database") from e

def initialize_app():
    """Initialize with full error context."""
    try:
        connect_to_database("invalid://localhost")
    except DatabaseError as e:
        # Full error chain is preserved
        logging.error("Application initialization failed", exc_info=True)

        # Access error chain
        print(f"Main error: {e}")
        if e.__cause__:
            print(f"Root cause: {e.__cause__}")
        # Output: Main error: Failed to parse database URL
        #         Root cause: Invalid URL format: invalid://localhost

def safe_conversion(value: str) -> int | None:
    """Convert string to int, hide implementation details."""
    try:
        return int(value)
    except ValueError:
        # Hide internal error, raise clean error
        raise TypeError(f"Cannot convert '{value}' to integer") from None

try:
    result = safe_conversion("abc")
except TypeError as e:
    # No __cause__, cleaner for user-facing errors
    print(f"Error: {e}")
    print(f"Cause: {e.__cause__}")  # None
```

**When to use**: Preserving error context across layers (e.g., database → business logic → API). Use `raise ... from e` to chain exceptions. Use `raise ... from None` to suppress internal errors for user-facing messages. Always preserve context in libraries and internal code.

**See Also**:

- [Recipe 17: Exception Patterns](#recipe-17-exception-patterns) - Basic exception handling patterns
- [Recipe 37-46: Troubleshooting](#-troubleshooting) - Common error solutions

---

## 🔷 File and Path Operations

Modern Python uses `pathlib` for path handling and provides robust file operations.

### Recipe 19: Path Operations

**Problem**: You need to work with file paths safely across platforms.

**Solution**:

```python
from pathlib import Path

path = Path('data/files/document.txt')

print(path.name)       # document.txt
print(path.stem)       # document
print(path.suffix)     # .txt
print(path.parent)     # data/files
print(path.parts)      # ('data', 'files', 'document.txt')

base = Path('data')
full_path = base / 'files' / 'document.txt'  # Pythonic!

if path.exists():
    print("File exists")

if path.is_file():
    print("It's a file")

if path.is_dir():
    print("It's a directory")

output_dir = Path('output/results')
output_dir.mkdir(parents=True, exist_ok=True)  # Create all parent dirs

data_dir = Path('data')
for item in data_dir.iterdir():
    print(item)

for txt_file in data_dir.glob('*.txt'):
    print(txt_file)

for py_file in data_dir.rglob('*.py'):
    print(py_file)

path = Path('config.txt')
path.write_text('key=value')
content = path.read_text()

data = b'\x00\x01\x02'
path.write_bytes(data)
binary_content = path.read_bytes()

absolute = path.resolve()

home = Path.home()
config_file = home / '.config' / 'app' / 'settings.ini'

import tempfile
with tempfile.TemporaryDirectory() as temp_dir:
    temp_path = Path(temp_dir)
    # Use temp_path
    # Automatically cleaned up after with block
```

**When to use**: Always prefer `pathlib` over `os.path` for modern Python code.

---

### Recipe 20: File Reading Patterns

**Problem**: You need to read files efficiently in various formats.

**Solution**:

```python
from pathlib import Path
from typing import Iterator

content = Path('data.txt').read_text()

def read_lines(filepath: Path) -> Iterator[str]:
    with filepath.open('r') as f:
        for line in f:
            yield line.strip()

for line in read_lines(Path('large_file.txt')):
    process(line)

lines: list[str] = Path('data.txt').read_text().splitlines()

import csv
from pathlib import Path

def read_csv(filepath: Path) -> list[dict[str, str]]:
    with filepath.open('r') as f:
        reader = csv.DictReader(f)
        return list(reader)

import json

def read_json(filepath: Path) -> dict:
    return json.loads(filepath.read_text())

def read_json_stream(filepath: Path) -> dict:
    with filepath.open('r') as f:
        return json.load(f)

import yaml

def read_yaml(filepath: Path) -> dict:
    return yaml.safe_load(filepath.read_text())

content = Path('data.txt').read_text(encoding='utf-8')

data = Path('image.png').read_bytes()

def process_file(filepath: Path):
    with filepath.open('r') as f:
        header = f.readline()
        for line in f:
            if line.startswith('#'):
                continue
            yield line.strip()
```

**When to use**: Choose the appropriate method based on file size and format.

---

## 🔷 String Operations

Python provides powerful string manipulation capabilities.

### Recipe 21: String Formatting

**Problem**: You need to format strings with variables.

**Solution**:

```python
name = "Alice"
age = 30
balance = 1234.5678

message = f"Hello, {name}! You are {age} years old."

status = f"{name} has ${balance:.2f} in account"

upper_name = f"Name: {name.upper()}"

pi = 3.14159
formatted = f"Pi is approximately {pi:.2f}"  # 3.14

num = 1234567
formatted_num = f"{num:,}"  # 1,234,567
formatted_num = f"{num:_}"  # 1_234_567

text = "hello"
print(f"|{text:>10}|")  # Right-align:  |     hello|
print(f"|{text:<10}|")  # Left-align:   |hello     |
print(f"|{text:^10}|")  # Center:       |  hello   |

from datetime import datetime
now = datetime.now()
formatted_date = f"{now:%Y-%m-%d %H:%M:%S}"

person = {
    'name': 'Alice',
    'age': 30,
    'city': 'NYC'
}
formatted = f"""
Name: {person['name']}
Age: {person['age']}
City: {person['city']}
"""

message = "Hello, {}! You are {} years old.".format(name, age)
message = "Hello, {name}! You are {age} years old.".format(name=name, age=age)

from string import Template
template = Template("Hello, $name! You are $age years old.")
result = template.substitute(name=name, age=age)
```

**When to use**: f-strings for most cases, Template for user-provided format strings.

---

### Recipe 22: String Manipulation

**Problem**: You need to parse, split, or transform strings.

**Solution**:

```python
text = "apple,banana,cherry"
fruits = text.split(',')  # ['apple', 'banana', 'cherry']
joined = ', '.join(fruits)  # 'apple, banana, cherry'

dirty = "  hello world  "
clean = dirty.strip()  # "hello world"
left_clean = dirty.lstrip()  # "hello world  "
right_clean = dirty.rstrip()  # "  hello world"

text = "Hello World"
replaced = text.replace("World", "Python")  # "Hello Python"
replaced_all = text.replace("l", "L")  # "HeLLo WorLd"

text = "Hello World"
print(text.lower())      # hello world
print(text.upper())      # HELLO WORLD
print(text.capitalize()) # Hello world
print(text.title())      # Hello World
print(text.swapcase())   # hELLO wORLD

text = "hello123"
print(text.isalnum())    # True (alphanumeric)
print(text.isalpha())    # False (has numbers)
print(text.isdigit())    # False (has letters)
print(text.islower())    # True
print(text.isupper())    # False

filename = "document.txt"
if filename.endswith('.txt'):
    print("Text file")

url = "https://example.com"
if url.startswith('https://'):
    print("Secure URL")

text = "hello world hello"
count = text.count('hello')  # 2

position = text.find('world')  # 6
position = text.rfind('hello')  # 12 (last occurrence)

text = "42"
padded = text.zfill(5)  # "00042"
padded = text.rjust(5, '0')  # "00042"
padded = text.ljust(5, '0')  # "42000"

text = "https://example.com"
without_protocol = text.removeprefix('https://')  # "example.com"

filename = "document.txt"
without_ext = filename.removesuffix('.txt')  # "document"
```

**When to use**: For text parsing, cleaning, and transformation operations.

---

### Recipe 23: Regular Expression Matching

**Problem**: You need powerful pattern matching and extraction from text.

**Solution**:

```python
import re

text = "My email is john@example.com"
pattern = r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b'

match = re.search(pattern, text)
if match:
    print(f"Found email: {match.group()}")
    # Output: Found email: john@example.com

text = "Contact us: info@company.com or support@company.com"
emails = re.findall(pattern, text)
print(f"Found {len(emails)} emails: {emails}")

phone_text = "Call me at (555) 123-4567"
phone_pattern = r'\((\d{3})\)\s*(\d{3})-(\d{4})'

match = re.search(phone_pattern, phone_text)
if match:
    area_code, prefix, number = match.groups()
    print(f"Area: {area_code}, Prefix: {prefix}, Number: {number}")
    # Output: Area: 555, Prefix: 123, Number: 4567
```

**Advanced example - URL parsing and log extraction**:

```python
import re

log_line = "2025-12-18 14:30:45 ERROR [UserService] Failed to authenticate user_id=123"

log_pattern = r'(?P<date>\d{4}-\d{2}-\d{2})\s+(?P<time>\d{2}:\d{2}:\d{2})\s+(?P<level>\w+)\s+\[(?P<component>\w+)\]\s+(?P<message>.+)'

match = re.match(log_pattern, log_line)
if match:
    log_data = match.groupdict()
    print(f"Level: {log_data['level']}, Component: {log_data['component']}")
    # Output: Level: ERROR, Component: UserService

text = "Price: $100, Discount: $20"
cleaned = re.sub(r'\$(\d+)', r'\1 USD', text)
print(cleaned)

version_text = "App version: 2.5.3-beta"
version_pattern = r'(\d+)\.(\d+)\.(\d+)(?:-(\w+))?'

match = re.search(version_pattern, version_text)
if match:
    major, minor, patch, tag = match.groups()
    print(f"Version: {major}.{minor}.{patch} ({tag or 'release'})")
    # Output: Version: 2.5.3 (beta)
```

**When to use**: Pattern matching, data extraction, text validation, log parsing, or complex find/replace. Use raw strings (r'...') for patterns. Use named groups for clarity. Compile patterns with re.compile() if reusing frequently.

**See Also**:

- [Recipe 24: String Validation Patterns](#recipe-24-string-validation-patterns) - Validation using regex
- [Recipe 22: String Manipulation](#recipe-22-string-manipulation) - Basic string operations

---

### Recipe 24: String Validation Patterns

**Problem**: You need to validate user input against common formats (email, phone, URL, etc.).

**Solution**:

```python
import re

def validate_email(email: str) -> tuple[bool, str]:
    """Validate email address format."""
    pattern = r'^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'

    if not email:
        return False, "Email cannot be empty"

    if not re.match(pattern, email):
        return False, "Invalid email format"

    if len(email) > 254:  # RFC 5321
        return False, "Email too long"

    return True, "Valid"

test_emails = [
    "user@example.com",
    "invalid.email",
    "user@domain.co.uk",
]

for email in test_emails:
    is_valid, message = validate_email(email)
    status = "✓" if is_valid else "✗"
    print(f"{status} {email}: {message}")
```

**Advanced example - Validation suite**:

```python
import re
from dataclasses import dataclass

@dataclass
class ValidationResult:
    is_valid: bool
    errors: list[str]

    def __bool__(self):
        return self.is_valid

class StringValidator:
    """Collection of string validation methods."""

    @staticmethod
    def validate_password(password: str, min_length: int = 8) -> ValidationResult:
        """Validate password strength."""
        errors = []

        if len(password) < min_length:
            errors.append(f"Must be at least {min_length} characters")

        if not re.search(r'[A-Z]', password):
            errors.append("Must contain uppercase letter")

        if not re.search(r'[a-z]', password):
            errors.append("Must contain lowercase letter")

        if not re.search(r'\d', password):
            errors.append("Must contain digit")

        if not re.search(r'[!@#$%^&*()]', password):
            errors.append("Must contain special character")

        return ValidationResult(len(errors) == 0, errors)

    @staticmethod
    def validate_username(username: str) -> ValidationResult:
        """Validate username (alphanumeric, underscore, 3-20 chars)."""
        errors = []

        if len(username) < 3 or len(username) > 20:
            errors.append("Must be 3-20 characters")

        if not re.match(r'^[a-zA-Z0-9_]+$', username):
            errors.append("Only letters, numbers, underscores allowed")

        if not re.match(r'^[a-zA-Z]', username):
            errors.append("Must start with a letter")

        return ValidationResult(len(errors) == 0, errors)

validator = StringValidator()

pwd_result = validator.validate_password("weak")
if not pwd_result:
    print("Password errors:")
    for error in pwd_result.errors:
        print(f"  - {error}")

strong_pwd = validator.validate_password("MyP@ssw0rd!")
print(f"Strong password valid: {strong_pwd.is_valid}")  # True

user_result = validator.validate_username("user_123")
print(f"Username valid: {user_result.is_valid}")  # True
```

**When to use**: User input validation, form processing, or data quality checks. Combine regex with business rules (length, character requirements). Return structured errors for user feedback. Use dataclasses for validation results.

**See Also**:

- [Recipe 23: Regular Expression Matching](#recipe-23-regular-expression-matching) - Pattern matching basics
- [Recipe 17: Exception Patterns](#recipe-17-exception-patterns) - Handling validation errors

---

## 🔷 Date and Time Operations

Python's `datetime` module provides comprehensive date/time handling.

### Recipe 25: Working with Dates

**Problem**: You need to parse, format, and manipulate dates.

**Solution**:

```python
from datetime import datetime, date, time, timedelta
from zoneinfo import ZoneInfo  # Python 3.9+

now = datetime.now()
today = date.today()
current_time = datetime.now().time()

specific = datetime(2025, 12, 17, 14, 30, 0)
specific_date = date(2025, 12, 17)

date_str = "2025-12-17"
parsed = datetime.strptime(date_str, "%Y-%m-%d")

time_str = "2025-12-17 14:30:45"
parsed_time = datetime.strptime(time_str, "%Y-%m-%d %H:%M:%S")

formatted = now.strftime("%Y-%m-%d %H:%M:%S")

formats = {
    'ISO': now.isoformat(),  # "2025-12-17T14:30:45.123456"
    'US': now.strftime("%m/%d/%Y"),  # "12/17/2025"
    'EU': now.strftime("%d/%m/%Y"),  # "17/12/2025"
    'Long': now.strftime("%B %d, %Y"),  # "December 17, 2025"
}

tomorrow = today + timedelta(days=1)
yesterday = today - timedelta(days=1)
next_week = today + timedelta(weeks=1)
three_hours_ago = now - timedelta(hours=3)

start = datetime(2025, 1, 1)
end = datetime(2025, 12, 31)
difference = end - start
print(f"Days: {difference.days}")
print(f"Total seconds: {difference.total_seconds()}")

utc_now = datetime.now(ZoneInfo("UTC"))
jakarta_now = datetime.now(ZoneInfo("Asia/Jakarta"))

utc_time = datetime.now(ZoneInfo("UTC"))
jakarta_time = utc_time.astimezone(ZoneInfo("Asia/Jakarta"))

if date.today() < date(2026, 1, 1):
    print("Still in 2025")

now = datetime.now()
print(f"Year: {now.year}")
print(f"Month: {now.month}")
print(f"Day: {now.day}")
print(f"Hour: {now.hour}")
print(f"Minute: {now.minute}")
print(f"Weekday: {now.weekday()}")  # Monday=0, Sunday=6
```

**When to use**: For any date/time operations, scheduling, or time-series data.

---

## 🔷 Configuration Management

Loading and validating configuration is essential for applications.

### Recipe 26: Configuration from Environment

**Problem**: You need to load configuration from environment variables.

**Solution**:

```python
import os

api_key = os.getenv('API_KEY')
port = os.getenv('PORT', '8000')  # Default value

port_int = int(os.getenv('PORT', '8000'))
debug_mode = os.getenv('DEBUG', 'false').lower() == 'true'

from decouple import config

api_key = config('API_KEY')
port = config('PORT', default=8000, cast=int)
debug = config('DEBUG', default=False, cast=bool)
database_url = config('DATABASE_URL')

from pydantic_settings import BaseSettings
from pydantic import Field

class Settings(BaseSettings):
    api_key: str
    port: int = 8000
    debug: bool = False
    database_url: str
    max_connections: int = Field(default=10, ge=1, le=100)

    class Config:
        env_file = '.env'
        env_file_encoding = 'utf-8'

settings = Settings()
print(f"API Key: {settings.api_key}")
print(f"Port: {settings.port}")

def connect_to_db(url: str):
    print(f"Connecting to {url}")

connect_to_db(settings.database_url)  # Type-checked!

class DevelopmentSettings(Settings):
    debug: bool = True
    log_level: str = "DEBUG"

class ProductionSettings(Settings):
    debug: bool = False
    log_level: str = "WARNING"

def get_settings() -> Settings:
    env = os.getenv('ENVIRONMENT', 'development')
    if env == 'production':
        return ProductionSettings()
    return DevelopmentSettings()

settings = get_settings()
```

**When to use**: Always! Don't hardcode configuration values.

---

### Recipe 27: Configuration from Files

**Problem**: You need to load configuration from JSON or YAML files.

**Solution**:

```python
import json
import yaml
from pathlib import Path
from typing import Any

def load_json_config(filepath: Path) -> dict[str, Any]:
    return json.loads(filepath.read_text())

config = load_json_config(Path('config.json'))

def load_yaml_config(filepath: Path) -> dict[str, Any]:
    return yaml.safe_load(filepath.read_text())

config = load_yaml_config(Path('config.yaml'))

class Config:
    def __init__(self, config_path: Path):
        self.config = self._load_config(config_path)

    def _load_config(self, path: Path) -> dict[str, Any]:
        if path.suffix == '.json':
            return json.loads(path.read_text())
        elif path.suffix in ('.yaml', '.yml'):
            return yaml.safe_load(path.read_text())
        else:
            raise ValueError(f"Unsupported config format: {path.suffix}")

    def get(self, key: str, default: Any = None) -> Any:
        return self.config.get(key, default)

    def __getitem__(self, key: str) -> Any:
        return self.config[key]

config = Config(Path('config.yaml'))
database_url = config.get('database_url', 'sqlite:///default.db')
api_key = config['api_key']

def load_layered_config(config_file: Path) -> dict[str, Any]:
    # Load from file
    config = load_yaml_config(config_file)

    # Override with environment variables
    for key in config.keys():
        env_value = os.getenv(key.upper())
        if env_value is not None:
            config[key] = env_value

    return config
```

**When to use**: For complex configuration or when you need separate dev/prod configs.

---

## 🔷 Testing with pytest

pytest is the de facto standard for Python testing.

### Recipe 28: Basic pytest Patterns

**Problem**: You need to write effective unit tests.

**Solution**:

```python
import pytest

def add(a: int, b: int) -> int:
    return a + b

def divide(a: int, b: int) -> float:
    if b == 0:
        raise ValueError("Cannot divide by zero")
    return a / b

def test_add():
    assert add(2, 3) == 5
    assert add(-1, 1) == 0
    assert add(0, 0) == 0

def test_add_multiple():
    assert add(1, 1) == 2
    assert add(2, 2) == 4
    assert add(3, 3) == 6

def test_divide_by_zero():
    with pytest.raises(ValueError, match="Cannot divide by zero"):
        divide(10, 0)

@pytest.mark.parametrize("a,b,expected", [
    (2, 3, 5),
    (-1, 1, 0),
    (0, 0, 0),
    (100, -50, 50),
])
def test_add_parametrized(a, b, expected):
    assert add(a, b) == expected

@pytest.fixture
def sample_data():
    """Provide sample data for tests"""
    return [1, 2, 3, 4, 5]

def test_with_fixture(sample_data):
    assert len(sample_data) == 5
    assert sum(sample_data) == 15

@pytest.fixture
def temp_file(tmp_path):
    """Create temporary file"""
    file_path = tmp_path / "test.txt"
    file_path.write_text("test content")
    yield file_path
    # Cleanup happens automatically with tmp_path

def test_file_operations(temp_file):
    content = temp_file.read_text()
    assert content == "test content"

class TestCalculator:
    def test_add(self):
        assert add(1, 1) == 2

    def test_divide(self):
        assert divide(10, 2) == 5.0

    def test_divide_by_zero(self):
        with pytest.raises(ValueError):
            divide(10, 0)

@pytest.mark.slow
def test_slow_operation():
    import time
    time.sleep(1)
    assert True

@pytest.mark.integration
def test_database_connection():
    # Integration test code
    pass

```

**When to use**: For all unit testing (pytest is preferred over unittest).

---

### Recipe 29: Advanced pytest Patterns

**Problem**: You need mocking, async tests, or complex fixtures.

**Solution**:

```python
import pytest
from unittest.mock import Mock, patch, MagicMock
import asyncio

def fetch_data(api_client):
    response = api_client.get('/users')
    return response.json()

def test_fetch_data_with_mock():
    # Create mock
    mock_client = Mock()
    mock_client.get.return_value.json.return_value = {'users': ['Alice', 'Bob']}

    # Test with mock
    result = fetch_data(mock_client)
    assert result == {'users': ['Alice', 'Bob']}
    mock_client.get.assert_called_once_with('/users')

@patch('requests.get')
def test_with_patch(mock_get):
    mock_get.return_value.status_code = 200
    mock_get.return_value.json.return_value = {'status': 'ok'}

    import requests
    response = requests.get('http://api.example.com')
    assert response.status_code == 200

@pytest.mark.asyncio
async def test_async_function():
    async def async_add(a, b):
        await asyncio.sleep(0.1)
        return a + b

    result = await async_add(2, 3)
    assert result == 5

@pytest.fixture
def database():
    """Database connection fixture"""
    db = {'users': []}
    yield db
    # Cleanup
    db.clear()

@pytest.fixture
def user_service(database):
    """Service using database fixture"""
    class UserService:
        def __init__(self, db):
            self.db = db

        def add_user(self, name):
            self.db['users'].append(name)

        def get_users(self):
            return self.db['users']

    return UserService(database)

def test_user_service(user_service):
    user_service.add_user('Alice')
    assert 'Alice' in user_service.get_users()

@pytest.fixture(scope="module")
def expensive_setup():
    """Setup once per module"""
    print("Expensive setup")
    yield "resource"
    print("Expensive teardown")

@pytest.fixture(params=[1, 2, 3])
def number(request):
    return request.param

def test_with_parametrized_fixture(number):
    assert number > 0
    # Test runs 3 times with numbers 1, 2, 3
```

**When to use**: For complex testing scenarios requiring mocks or async code.

---

## 🔷 CLI Development

Building command-line interfaces is common in Python.

### Recipe 30: argparse for CLI

**Problem**: You need to build a CLI tool with arguments and options.

**Solution**:

```python
import argparse
from pathlib import Path

def main():
    parser = argparse.ArgumentParser(
        description='Process data files',
        epilog='Example: python script.py input.txt -o output.txt'
    )

    # Positional argument
    parser.add_argument('input', type=str, help='Input file path')

    # Optional arguments
    parser.add_argument('-o', '--output', type=str, default='output.txt',
                        help='Output file path (default: output.txt)')

    parser.add_argument('-v', '--verbose', action='store_true',
                        help='Enable verbose output')

    parser.add_argument('-f', '--format', choices=['json', 'csv', 'xml'],
                        default='json', help='Output format')

    parser.add_argument('-n', '--number', type=int, default=10,
                        help='Number of items to process')

    # Parse arguments
    args = parser.parse_args()

    # Use arguments
    input_path = Path(args.input)
    output_path = Path(args.output)

    if args.verbose:
        print(f"Processing {input_path}")

    process_file(input_path, output_path, args.format, args.number)

def process_file(input_path, output_path, format, number):
    print(f"Processing {input_path} -> {output_path} ({format}, n={number})")

if __name__ == '__main__':
    main()

```

**When to use**: For building command-line tools with simple argument parsing.

---

### Recipe 31: Click for Advanced CLI

**Problem**: You need a more powerful CLI framework with subcommands.

**Solution**:

```python
import click
from pathlib import Path

@click.group()
@click.version_option(version='1.0.0')
def cli():
    """Data processing CLI tool"""
    pass

@cli.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.option('-o', '--output', type=click.Path(), default='output.txt',
              help='Output file path')
@click.option('-v', '--verbose', is_flag=True, help='Verbose output')
@click.option('-f', '--format', type=click.Choice(['json', 'csv', 'xml']),
              default='json', help='Output format')
def process(input_file, output, verbose, format):
    """Process input file"""
    if verbose:
        click.echo(f"Processing {input_file}")

    click.echo(f"Output: {output} ({format})")

@cli.command()
@click.option('-n', '--number', type=int, default=10,
              help='Number of items')
def generate(number):
    """Generate sample data"""
    click.echo(f"Generating {number} items...")
    with click.progressbar(range(number)) as bar:
        for i in bar:
            # Simulate work
            import time
            time.sleep(0.1)
    click.secho('Done!', fg='green', bold=True)

@cli.command()
@click.argument('files', nargs=-1, type=click.Path(exists=True))
def merge(files):
    """Merge multiple files"""
    click.echo(f"Merging {len(files)} files:")
    for file in files:
        click.echo(f"  - {file}")

if __name__ == '__main__':
    cli()

```

**When to use**: For complex CLIs with subcommands, progress bars, or interactive prompts.

---

## 🔷 Logging

Proper logging is essential for debugging and monitoring production applications.

### Recipe 32: Logging Setup

**Problem**: You need structured logging for your application.

**Solution**:

```python
import logging
from pathlib import Path

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

logger = logging.getLogger(__name__)

logger.debug("Debug message")      # Not shown (level is INFO)
logger.info("Info message")        # Shown
logger.warning("Warning message")  # Shown
logger.error("Error message")      # Shown
logger.critical("Critical message") # Shown

try:
    result = 10 / 0
except ZeroDivisionError:
    logger.exception("Division error occurred")  # Includes traceback

def setup_logging(log_file: Path, level: str = 'INFO'):
    """Configure logging with file and console handlers"""
    # Create logger
    logger = logging.getLogger()
    logger.setLevel(level)

    # File handler
    file_handler = logging.FileHandler(log_file)
    file_handler.setLevel(level)
    file_formatter = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    file_handler.setFormatter(file_formatter)

    # Console handler
    console_handler = logging.StreamHandler()
    console_handler.setLevel(level)
    console_formatter = logging.Formatter(
        '%(levelname)s - %(message)s'
    )
    console_handler.setFormatter(console_formatter)

    # Add handlers
    logger.addHandler(file_handler)
    logger.addHandler(console_handler)

setup_logging(Path('app.log'), level='DEBUG')
logger = logging.getLogger(__name__)
logger.info("Application started")

logger.info("User login", extra={
    'user_id': 123,
    'ip_address': '192.168.1.1'
})

logger = logging.getLogger(__name__)  # __name__ is 'module1'

logging.getLogger('module1').setLevel(logging.DEBUG)
logging.getLogger('module2').setLevel(logging.WARNING)
```

**When to use**: Always! Prefer logging over print() for anything beyond debugging.

---

## 🔷 Web and API Operations

Master HTTP requests and API interactions using the `requests` library.

### Recipe 33: Make HTTP GET Request

**Problem**: You need to fetch data from a REST API or web service.

**Solution**:

```python
import requests

def main():
    # Make GET request
    response = requests.get('https://api.github.com/users/octocat')

    # Check status code
    if response.status_code == 200:
        print("✓ Request successful")
        data = response.json()  # Parse JSON response
        print(f"User: {data['login']}")
        print(f"Name: {data['name']}")
        print(f"Followers: {data['followers']}")
        # Output: User: octocat
        #         Name: The Octocat
        #         Followers: 12345
    else:
        print(f"✗ Request failed: {response.status_code}")

    # Access response properties
    print(f"Status: {response.status_code}")
    print(f"Headers: {response.headers['content-type']}")
    print(f"URL: {response.url}")

if __name__ == '__main__':
    main()
```

requests.get() makes an HTTP GET request and returns a Response object. Use response.json() to parse JSON responses automatically. Check response.status_code to verify success (200 = OK). Access response.text for raw text or response.content for binary data.

```python
import requests
from typing import Any
import time

def fetch_user_repos(username: str, per_page: int = 30) -> list | None:
    """Fetch GitHub repos with proper error handling."""

    # URL and query parameters
    url = f'https://api.github.com/users/{username}/repos'
    params = {
        'per_page': per_page,
        'sort': 'updated',
        'direction': 'desc'
    }

    # Custom headers
    headers = {
        'Accept': 'application/vnd.github.v3+json',
        'User-Agent': 'Python-Requests-Tutorial'
    }

    # Timeout prevents hanging indefinitely
    try:
        response = requests.get(
            url,
            params=params,
            headers=headers,
            timeout=10  # 10 seconds timeout
        )

        # Raise exception for bad status codes
        response.raise_for_status()

        repos = response.json()
        print(f"✓ Fetched {len(repos)} repositories")

        for repo in repos[:5]:  # Show first 5
            print(f"  - {repo['name']}: {repo['stargazers_count']} ⭐")

        return repos

    except requests.exceptions.Timeout:
        print(f"✗ Request timed out after 10 seconds")
        return None
    except requests.exceptions.ConnectionError:
        print(f"✗ Connection error - check your internet")
        return None
    except requests.exceptions.HTTPError as e:
        print(f"✗ HTTP error: {e.response.status_code} - {e.response.reason}")
        return None
    except requests.exceptions.RequestException as e:
        print(f"✗ Request failed: {e}")
        return None

def fetch_with_retry(url: str, max_retries: int = 3) -> dict[Any, Any] | None:
    """Fetch with exponential backoff retry."""

    for attempt in range(max_retries):
        try:
            response = requests.get(url, timeout=10)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            if attempt < max_retries - 1:
                wait_time = 2 ** attempt  # Exponential backoff: 1s, 2s, 4s
                print(f"Retry {attempt + 1}/{max_retries} after {wait_time}s...")
                time.sleep(wait_time)
            else:
                print(f"✗ Failed after {max_retries} attempts: {e}")
                return None

def main():
    # Fetch with parameters and headers
    repos = fetch_user_repos('octocat', per_page=10)

    # Fetch with retry logic
    data = fetch_with_retry('https://api.github.com/users/torvalds')
    if data:
        print(f"\n✓ Fetched user: {data['name']}")

    # Multiple parallel requests (session for connection pooling)
    with requests.Session() as session:
        users = ['octocat', 'torvalds', 'gvanrossum']
        for user in users:
            response = session.get(f'https://api.github.com/users/{user}')
            if response.ok:
                data = response.json()
                print(f"{data['login']}: {data['public_repos']} repos")

if __name__ == '__main__':
    main()
```

**When to use**: Fetching data from REST APIs, calling web services, downloading files, or integrating with third-party services. Use params for query parameters. Use headers for authentication (API keys, tokens). Always set timeout to prevent hanging. Use raise_for_status() to catch HTTP errors. Use Session() for multiple requests to the same host (connection pooling).

### Recipe 34: Make HTTP POST Request

**Problem**: You need to send data to an API or web service.

**Solution**:

```python
import requests
import json

def main():
    # API endpoint
    url = 'https://httpbin.org/post'

    # Data to send (will be JSON-encoded)
    payload = {
        'name': 'Alice',
        'email': 'alice@example.com',
        'age': 30
    }

    # POST with JSON
    response = requests.post(url, json=payload)

    if response.status_code == 200:
        print("✓ POST successful")
        result = response.json()
        print(f"Sent data: {result['json']}")
        # Output: Sent data: {'name': 'Alice', 'email': 'alice@example.com', 'age': 30}
    else:
        print(f"✗ POST failed: {response.status_code}")

    # POST with form data
    form_data = {
        'username': 'alice',
        'password': 'secret123'
    }
    response = requests.post(url, data=form_data)
    print(f"Form POST status: {response.status_code}")

if __name__ == '__main__':
    main()
```

requests.post() sends data to a server. Use json=data to send JSON (sets Content-Type automatically). Use data=data for form-encoded data (application/x-www-form-urlencoded). The server response can be parsed with response.json() or response.text.

```python
import requests
from typing import Any
from pathlib import Path

def create_user(api_url: str, user_data: dict[str, Any], api_key: str) -> dict | None:
    """Create user via API with authentication."""

    headers = {
        'Authorization': f'Bearer {api_key}',
        'Content-Type': 'application/json'
    }

    try:
        response = requests.post(
            f'{api_url}/users',
            json=user_data,
            headers=headers,
            timeout=10
        )

        # Handle different status codes
        if response.status_code == 201:
            print("✓ User created successfully")
            return response.json()
        elif response.status_code == 400:
            print(f"✗ Bad request: {response.json()}")
            return None
        elif response.status_code == 401:
            print("✗ Unauthorized - check your API key")
            return None
        elif response.status_code == 409:
            print("✗ User already exists")
            return None
        else:
            response.raise_for_status()

    except requests.exceptions.RequestException as e:
        print(f"✗ Request failed: {e}")
        return None

def upload_file(url: str, file_path: Path) -> bool:
    """Upload file with multipart/form-data."""

    if not file_path.exists():
        print(f"✗ File not found: {file_path}")
        return False

    # Open file in binary mode
    with open(file_path, 'rb') as f:
        files = {'file': (file_path.name, f, 'application/octet-stream')}

        # Additional form fields
        data = {
            'description': 'Uploaded via Python',
            'category': 'documents'
        }

        try:
            response = requests.post(
                url,
                files=files,
                data=data,
                timeout=30  # Longer timeout for file uploads
            )

            if response.ok:
                print(f"✓ File uploaded: {file_path.name}")
                return True
            else:
                print(f"✗ Upload failed: {response.status_code}")
                return False

        except requests.exceptions.RequestException as e:
            print(f"✗ Upload error: {e}")
            return False

def batch_create(api_url: str, items: list, api_key: str) -> dict[str, int]:
    """Batch create multiple items with progress tracking."""

    results = {'success': 0, 'failed': 0}
    headers = {'Authorization': f'Bearer {api_key}'}

    for i, item in enumerate(items, 1):
        print(f"Processing {i}/{len(items)}...", end=' ')

        try:
            response = requests.post(
                f'{api_url}/items',
                json=item,
                headers=headers,
                timeout=10
            )

            if response.status_code == 201:
                print("✓")
                results['success'] += 1
            else:
                print(f"✗ {response.status_code}")
                results['failed'] += 1

        except requests.exceptions.RequestException as e:
            print(f"✗ Error: {e}")
            results['failed'] += 1

    print(f"\nResults: {results['success']} success, {results['failed']} failed")
    return results

def main():
    # Create user with authentication
    user_data = {
        'username': 'alice',
        'email': 'alice@example.com',
        'full_name': 'Alice Anderson'
    }

    user = create_user(
        'https://api.example.com',
        user_data,
        'your-api-key-here'
    )

    # Upload file
    file_path = Path('document.pdf')
    upload_file('https://httpbin.org/post', file_path)

    # Batch operations
    items = [
        {'name': 'Item 1', 'price': 10.00},
        {'name': 'Item 2', 'price': 20.00},
        {'name': 'Item 3', 'price': 15.00},
    ]

    results = batch_create('https://api.example.com', items, 'api-key')

    # Session for multiple requests
    with requests.Session() as session:
        session.headers.update({'Authorization': 'Bearer api-key'})

        # All requests in this session use the same headers
        response1 = session.post('https://api.example.com/endpoint1', json={'data': 1})
        response2 = session.post('https://api.example.com/endpoint2', json={'data': 2})

if __name__ == '__main__':
    main()
```

**When to use**: Creating resources via REST APIs, submitting forms, uploading files, authentication requests, or sending data to web services. Use json=data for JSON payloads. Use data=data for form submissions. Use files=files for file uploads (multipart/form-data). Always handle different status codes (201 Created, 400 Bad Request, 401 Unauthorized). Use Session() for authenticated requests to reuse headers.

### Recipe 35: Parse JSON API Responses

**Problem**: You need to safely parse and validate JSON data from APIs.

**Solution**:

```python
import requests
from typing import Any

def fetch_and_parse(url: str) -> dict[str, Any] | None:
    """Fetch JSON data with safe parsing."""

    try:
        response = requests.get(url, timeout=10)
        response.raise_for_status()

        # Parse JSON
        data = response.json()

        # Access nested data safely
        if 'user' in data:
            user = data['user']
            print(f"User: {user.get('name', 'Unknown')}")
            print(f"Email: {user.get('email', 'Not provided')}")

        return data

    except requests.exceptions.JSONDecodeError:
        print("✗ Invalid JSON response")
        return None
    except requests.exceptions.RequestException as e:
        print(f"✗ Request failed: {e}")
        return None

def main():
    # Fetch and parse GitHub user
    url = 'https://api.github.com/users/octocat'
    data = fetch_and_parse(url)

    if data:
        # Safe access with get()
        login = data.get('login', 'unknown')
        followers = data.get('followers', 0)
        repos = data.get('public_repos', 0)

        print(f"\n{login}: {followers} followers, {repos} repos")

        # Access nested fields
        company = data.get('company')
        if company:
            print(f"Company: {company}")

if __name__ == '__main__':
    main()
```

response.json() parses JSON automatically and returns a Python dict. Use .get() for safe access to optional fields. Handle JSONDecodeError for invalid JSON. Check for required fields before accessing to avoid KeyError.

```python
import requests
from typing import Any, TypedDict
from dataclasses import dataclass
from datetime import datetime

class UserData(TypedDict):
    """Type definition for user data."""
    id: int
    login: str
    name: str
    followers: int
    public_repos: int
    created_at: str

@dataclass
class User:
    """Validated user model."""
    id: int
    username: str
    name: str
    followers: int
    repos: int
    created_at: datetime

    @classmethod
    def from_api(cls, data: dict[str, Any]) -> 'User' | None:
        """Create User from API response with validation."""
        try:
            # Validate required fields
            required_fields = ['id', 'login', 'followers', 'public_repos', 'created_at']
            missing = [f for f in required_fields if f not in data]

            if missing:
                print(f"✗ Missing required fields: {missing}")
                return None

            # Parse datetime
            created_at = datetime.fromisoformat(data['created_at'].replace('Z', '+00:00'))

            return cls(
                id=int(data['id']),
                username=str(data['login']),
                name=str(data.get('name', '')),
                followers=int(data['followers']),
                repos=int(data['public_repos']),
                created_at=created_at
            )

        except (ValueError, TypeError) as e:
            print(f"✗ Validation error: {e}")
            return None

def fetch_users(usernames: list[str]) -> list[User]:
    """Fetch multiple users with validation."""

    users = []

    for username in usernames:
        url = f'https://api.github.com/users/{username}'

        try:
            response = requests.get(url, timeout=10)

            if response.status_code == 404:
                print(f"✗ User not found: {username}")
                continue

            response.raise_for_status()
            data = response.json()

            # Validate and parse
            user = User.from_api(data)
            if user:
                users.append(user)
                print(f"✓ Fetched: {user.username}")

        except requests.exceptions.RequestException as e:
            print(f"✗ Error fetching {username}: {e}")
            continue

    return users

def parse_paginated_response(url: str) -> list[dict[str, Any]]:
    """Parse paginated API responses."""

    all_items = []

    while url:
        try:
            response = requests.get(url, timeout=10)
            response.raise_for_status()

            # Parse current page
            data = response.json()

            # Handle different pagination formats
            if isinstance(data, list):
                # Simple list response
                all_items.extend(data)
                break
            elif 'items' in data:
                # Format: {"items": [...], "next_page": "url"}
                all_items.extend(data['items'])
                url = data.get('next_page')
            elif 'results' in data:
                # Format: {"results": [...], "next": "url"}
                all_items.extend(data['results'])
                url = data.get('next')
            else:
                break

            print(f"Fetched {len(all_items)} items so far...")

        except requests.exceptions.RequestException as e:
            print(f"✗ Pagination error: {e}")
            break

    return all_items

def extract_nested_data(data: dict[str, Any], path: str, default: Any = None) -> Any:
    """Safely extract nested data using dot notation."""

    keys = path.split('.')
    current = data

    for key in keys:
        if isinstance(current, dict) and key in current:
            current = current[key]
        else:
            return default

    return current

def main():
    # Fetch and validate multiple users
    usernames = ['octocat', 'torvalds', 'invalid-user-xyz']
    users = fetch_users(usernames)

    print(f"\n✓ Successfully fetched {len(users)} users:")
    for user in users:
        print(f"  {user.username}: {user.followers} followers, joined {user.created_at.year}")

    # Parse paginated responses
    repos = parse_paginated_response('https://api.github.com/users/octocat/repos')
    print(f"\n✓ Total repos: {len(repos)}")

    # Extract nested data
    sample_data = {
        'user': {
            'profile': {
                'contact': {
                    'email': 'user@example.com'
                }
            }
        }
    }

    email = extract_nested_data(sample_data, 'user.profile.contact.email', 'N/A')
    print(f"\nExtracted email: {email}")

if __name__ == '__main__':
    main()
```

**When to use**: Consuming REST APIs, processing JSON data, integrating with web services, or building API clients. Always validate required fields before accessing. Use dataclasses or TypedDict for type safety. Handle pagination for large result sets. Use .get() with defaults for optional fields. Parse dates/timestamps explicitly. Create validation functions for complex data structures.

---

## 🔷 Common Patterns and Best Practices

### Recipe 36: Enum for Constants

**Problem**: You need type-safe constants or enumerations.

**Solution**:

```python
from enum import Enum, auto

class Status(Enum):
    PENDING = 'pending'
    PROCESSING = 'processing'
    COMPLETED = 'completed'
    FAILED = 'failed'

current_status = Status.PENDING
print(current_status)  # Status.PENDING
print(current_status.value)  # 'pending'

if current_status == Status.PENDING:
    print("Waiting to start")

class Color(Enum):
    RED = auto()     # 1
    GREEN = auto()   # 2
    BLUE = auto()    # 3

for color in Color:
    print(f"{color.name}: {color.value}")

color = Color['RED']
color = Color(1)

from enum import IntEnum

class Priority(IntEnum):
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4

if Priority.HIGH >= 3:
    print("High priority or above")

from enum import Flag

class Permission(Flag):
    READ = auto()
    WRITE = auto()
    EXECUTE = auto()

user_permissions = Permission.READ | Permission.WRITE
if Permission.READ in user_permissions:
    print("Can read")
```

**When to use**: For any set of related constants or states.

---

## 🐛 Troubleshooting

Common Python errors and their solutions.

### Recipe 37: ImportError / ModuleNotFoundError

**Problem**: You encounter "ModuleNotFoundError: No module named 'xyz'" when running your code.

**Solution**:

**Basic Example - Install Missing Package**:

```python




try:
    import requests
    print(f"requests version: {requests.__version__}")
except ImportError as e:
    print(f"Import failed: {e}")
    print("Run: pip install requests")
```

**Common Causes**:

- Package not installed in current virtual environment
- Typo in package name
- Wrong Python interpreter (system vs virtual environment)
- Package installed in different Python version

**Advanced Example - Virtual Environment Setup**:

```python
import sys
print(f"Python executable: {sys.executable}")
print(f"Python version: {sys.version}")
print(f"Python path: {sys.path}")



import requests
print(f"requests location: {requests.__file__}")

if hasattr(sys, 'real_prefix') or (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix):
    print("Running in virtual environment")
else:
    print("NOT in virtual environment - create one!")
```

**Prevention Tips**:

- Always use virtual environments (`python -m venv venv`)
- Keep `requirements.txt` updated (`pip freeze > requirements.txt`)
- Document installation steps in README
- Use `pip list` to verify installed packages

**When to use**: Whenever you encounter import errors or module not found issues.

**See Also**:

- [Recipe 17: Exception Patterns](#recipe-17-exception-patterns) - Proper error handling
- [Recipe 26-27: Configuration](#-configuration-management) - Environment setup

---

### Recipe 38: AttributeError

**Problem**: You get "AttributeError: 'NoneType' object has no attribute 'xyz'" or similar attribute errors.

**Solution**:

**Basic Example - Check for None**:

```python
user_data = get_user(user_id)  # Might return None
name = user_data.name  # AttributeError if user_data is None

user_data = get_user(user_id)
if user_data is not None:
    name = user_data.name
else:
    name = "Unknown"

user_data = get_user(user_id)
name = getattr(user_data, 'name', 'Unknown')


def get_user_name(user: object | None) -> str:
    return getattr(user, 'name', 'Unknown')
```

**Common Causes**:

- Accessing attributes on `None` value
- Typo in attribute name
- Object doesn't have expected attribute
- Accessing private attributes incorrectly

**Advanced Example - Safe Attribute Access**:

```python
from typing import Any
from dataclasses import dataclass

@dataclass
class User:
    name: str
    email: str
    age: int | None = None

def safe_get_attr(obj: Any, attr_path: str, default: Any = None) -> Any:
    """
    Safely get nested attributes with dot notation.

    Example: safe_get_attr(user, 'profile.address.city', 'Unknown')
    """
    attrs = attr_path.split('.')
    current = obj

    for attr in attrs:
        if current is None:
            return default
        try:
            current = getattr(current, attr)
        except AttributeError:
            return default

    return current if current is not None else default

user = User(name="Alice", email="alice@example.com")

name = safe_get_attr(user, 'name', 'Unknown')  # "Alice"
age = safe_get_attr(user, 'age', 0)  # 0 (None → default)
city = safe_get_attr(user, 'profile.address.city', 'N/A')  # "N/A"

print(f"User: {name}, Age: {age}, City: {city}")

if hasattr(user, 'name'):
    print(f"Name exists: {user.name}")

required_attrs = ['name', 'email']
missing = [attr for attr in required_attrs if not hasattr(user, attr)]
if missing:
    print(f"Missing attributes: {missing}")
```

**Prevention Tips**:

- Use type hints to catch issues early
- Check for `None` before accessing attributes
- Use `hasattr()` to verify attribute existence
- Use `getattr()` with defaults for optional attributes

**When to use**: When working with objects that might be None or have optional attributes.

**See Also**:

- [Recipe 7-8: Type Hints](#-type-hints-and-type-safety) - Type safety with hints
- [Recipe 17: Exception Patterns](#recipe-17-exception-patterns) - Error handling patterns

---

### Recipe 39: KeyError

**Problem**: You get "KeyError: 'xyz'" when accessing dictionary keys.

**Solution**:

**Basic Example - Safe Dictionary Access**:

```python
config = {'host': 'localhost', 'port': 8080}
database = config['database']  # KeyError: 'database'

database = config.get('database', 'default.db')

if 'database' in config:
    database = config['database']
else:
    database = 'default.db'

database = config.setdefault('database', 'default.db')
print(config)  # Now includes 'database': 'default.db'
```

**Common Causes**:

- Key doesn't exist in dictionary
- Typo in key name
- Expecting data from API that wasn't returned
- Case sensitivity in keys

**Advanced Example - Nested Dictionary Access**:

```python
from typing import Any
from collections import defaultdict

def safe_get_nested(data: dict[str, Any], *keys: str, default: Any = None) -> Any:
    """
    Safely access nested dictionary values.

    Example: safe_get_nested(data, 'user', 'profile', 'email', default='')
    """
    current = data
    for key in keys:
        if not isinstance(current, dict):
            return default
        current = current.get(key)
        if current is None:
            return default
    return current

api_response = {
    'status': 'success',
    'data': {
        'user': {
            'name': 'Alice',
            'profile': {
                'email': 'alice@example.com'
            }
        }
    }
}

email = safe_get_nested(api_response, 'data', 'user', 'profile', 'email', default='no-email')
phone = safe_get_nested(api_response, 'data', 'user', 'profile', 'phone', default='no-phone')

print(f"Email: {email}")  # alice@example.com
print(f"Phone: {phone}")  # no-phone

from collections import defaultdict

counts = defaultdict(int)
counts['apples'] += 1  # No KeyError, starts at 0
counts['oranges'] += 2

nested = defaultdict(lambda: defaultdict(list))
nested['user1']['orders'].append('item1')  # No KeyError
nested['user2']['orders'].append('item2')
```

**Prevention Tips**:

- Use `.get()` method instead of direct key access
- Validate API responses before accessing nested keys
- Use `defaultdict` for counters and accumulations
- Check key existence with `in` operator

**When to use**: When working with dictionaries, especially from external sources like APIs or configuration files.

**See Also**:

- [Recipe 4: defaultdict and Counter](#recipe-4-defaultdict-and-counter) - Avoid KeyError with defaultdict
- [Recipe 2: Dictionary Comprehensions](#recipe-2-dictionary-comprehensions) - Dictionary operations

---

### Recipe 40: IndexError

**Problem**: You get "IndexError: list index out of range" when accessing list elements.

**Solution**:

**Basic Example - Safe List Access**:

```python
items = [1, 2, 3]
fourth = items[3]  # IndexError: list index out of range

if len(items) > 3:
    fourth = items[3]
else:
    fourth = None

try:
    fourth = items[3]
except IndexError:
    fourth = None

fourth_list = items[3:4]  # Returns [] if index 3 doesn't exist
fourth = fourth_list[0] if fourth_list else None
```

**Common Causes**:

- Accessing index beyond list length
- Off-by-one errors (forgetting lists are 0-indexed)
- Empty list access
- Negative indices on short lists

**Advanced Example - Robust List Operations**:

```python
from typing import TypeVar

T = TypeVar('T')

def safe_get_index(lst: list[T], index: int, default: T | None = None) -> T | None:
    """
    Safely get list element at index.

    Supports negative indices.
    """
    try:
        return lst[index]
    except IndexError:
        return default

def safe_slice(lst: list[T], start: int = 0, end: int | None = None,
               step: int = 1) -> list[T]:
    """
    Safe list slicing with bounds checking.
    """
    if not lst:
        return []

    length = len(lst)
    # Normalize negative indices
    start = max(0, start if start >= 0 else length + start)
    end = min(length, end if end is not None and end >= 0 else length + (end or 0))

    return lst[start:end:step]

numbers = [10, 20, 30, 40, 50]

first = safe_get_index(numbers, 0, default=0)  # 10
tenth = safe_get_index(numbers, 10, default=0)  # 0 (out of range)
last = safe_get_index(numbers, -1, default=0)  # 50
invalid = safe_get_index(numbers, -10, default=0)  # 0 (out of range)

print(f"First: {first}, Tenth: {tenth}, Last: {last}, Invalid: {invalid}")

empty_list: list[int] = []
slice1 = safe_slice(empty_list, 0, 5)  # [] (empty input)
slice2 = safe_slice(numbers, 2, 10)  # [30, 40, 50] (end beyond length)
slice3 = safe_slice(numbers, -2)  # [40, 50] (negative start)

for index, value in enumerate(numbers):
    next_value = safe_get_index(numbers, index + 1, default='End')
    print(f"Current: {value}, Next: {next_value}")
```

**Prevention Tips**:

- Always check list length before accessing indices
- Use `enumerate()` for index-based iteration
- Prefer slicing over direct index access when possible
- Use negative indices carefully (remember `-len(lst)` to `-1`)

**When to use**: When working with lists where length is uncertain or when accessing specific positions.

---

### Recipe 41: TypeError

**Problem**: You get "TypeError: unsupported operand type(s)" or "TypeError: 'X' object is not callable".

**Solution**:

**Basic Example - Type Mismatches**:

```python
age = "25"
next_year = age + 1  # TypeError: can only concatenate str (not "int") to str

age = "25"
next_year = int(age) + 1  # 26

def add_one(value):
    if isinstance(value, str):
        value = int(value)
    return value + 1

data = {'name': 'Alice'}
result = data()  # TypeError: 'dict' object is not callable

if callable(data):
    result = data()
else:
    result = data
```

**Common Causes**:

- Mixing strings and numbers in operations
- Calling variables that aren't functions
- Passing wrong number of arguments
- Mixing mutable and immutable types

**Advanced Example - Type-Safe Operations**:

```python
from typing import TypeVar, Callable, Any
from functools import wraps

T = TypeVar('T')

def ensure_type(expected_type: type, convert: bool = True):
    """
    Decorator to ensure argument types.

    Example:
        @ensure_type(int, convert=True)
        def add_one(x): return x + 1
    """
    def decorator(func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args, **kwargs):
            converted_args = []
            for arg in args:
                if not isinstance(arg, expected_type):
                    if convert:
                        try:
                            arg = expected_type(arg)
                        except (ValueError, TypeError) as e:
                            raise TypeError(f"Cannot convert {type(arg).__name__} to {expected_type.__name__}: {e}")
                    else:
                        raise TypeError(f"Expected {expected_type.__name__}, got {type(arg).__name__}")
                converted_args.append(arg)
            return func(*converted_args, **kwargs)
        return wrapper
    return decorator

@ensure_type(int, convert=True)
def add_one(x: int) -> int:
    return x + 1

result1 = add_one("42")  # 43
result2 = add_one(42)  # 43

print(f"Results: {result1}, {result2}")

def safe_convert(value: Any, target_type: type, default: Any = None) -> Any:
    """
    Safely convert value to target type with fallback.
    """
    if isinstance(value, target_type):
        return value

    try:
        return target_type(value)
    except (ValueError, TypeError):
        return default

num1 = safe_convert("123", int, default=0)  # 123
num2 = safe_convert("abc", int, default=0)  # 0
num3 = safe_convert([1, 2], str, default="")  # "[1, 2]"

def safe_call(obj: Any, *args, **kwargs) -> Any:
    """
    Safely call object if callable, otherwise return it.
    """
    if callable(obj):
        return obj(*args, **kwargs)
    return obj

func = lambda x: x * 2
value = 42

result1 = safe_call(func, 5)  # 10 (callable)
result2 = safe_call(value)  # 42 (not callable, returned as-is)
```

**Prevention Tips**:

- Use type hints and mypy for static type checking
- Convert types explicitly rather than relying on implicit conversion
- Check if objects are callable before calling
- Use `isinstance()` for runtime type checking

**When to use**: When working with mixed types or when type safety is critical.

---

### Recipe 42: ValueError

**Problem**: You get "ValueError: invalid literal for int()" or similar value conversion errors.

**Solution**:

**Basic Example - Input Validation**:

```python
user_input = "abc"
age = int(user_input)  # ValueError: invalid literal for int() with base 10: 'abc'

user_input = "abc"
try:
    age = int(user_input)
except ValueError:
    age = 0
    print("Invalid input, using default age")

user_input = "abc"
if user_input.isdigit():
    age = int(user_input)
else:
    age = 0

import re

def is_valid_age(value: str) -> bool:
    return bool(re.match(r'^\d+$', value))

if is_valid_age(user_input):
    age = int(user_input)
```

**Common Causes**:

- Invalid string to number conversion
- Unpacking wrong number of values
- Invalid format strings
- Out-of-range values

**Advanced Example - Robust Input Parsing**:

```python
import re

T = TypeVar('T')

def safe_parse(value: str, parser: Callable[[str], T],
               validator: Callable[[T], bool] | None = None,
               default: T | None = None) -> T | None:
    """
    Safely parse and validate input with custom parser.

    Example:
        age = safe_parse("25", int, lambda x: 0 <= x <= 120, default=0)
    """
    try:
        parsed = parser(value)
        if validator and not validator(parsed):
            return default
        return parsed
    except (ValueError, TypeError):
        return default

age = safe_parse("25", int, lambda x: 0 <= x <= 120, default=0)  # 25
invalid_age = safe_parse("200", int, lambda x: 0 <= x <= 120, default=0)  # 0
price = safe_parse("19.99", float, lambda x: x > 0, default=0.0)  # 19.99

print(f"Age: {age}, Invalid Age: {invalid_age}, Price: {price}")

def parse_phone(phone: str) -> str | None:
    """
    Parse phone number to standard format.

    Accepts: (123) 456-7890, 123-456-7890, 1234567890
    Returns: 1234567890 or None
    """
    # Remove all non-digits
    digits = re.sub(r'\D', '', phone)

    # Validate length
    if len(digits) != 10:
        return None

    return digits

phones = ["(123) 456-7890", "123-456-7890", "1234567890", "invalid"]
for phone in phones:
    parsed = parse_phone(phone)
    print(f"{phone} → {parsed}")

def safe_unpack(data: str, separator: str = ',', expected_count: int | None = None):
    """
    Safely unpack delimited string.
    """
    parts = [p.strip() for p in data.split(separator)]

    if expected_count and len(parts) != expected_count:
        raise ValueError(f"Expected {expected_count} parts, got {len(parts)}")

    return parts

try:
    name, age, city = safe_unpack("Alice,25,NYC", expected_count=3)
    print(f"Name: {name}, Age: {age}, City: {city}")
except ValueError as e:
    print(f"Unpacking error: {e}")
```

**Prevention Tips**:

- Validate input before conversion
- Use try-except for conversion operations
- Provide sensible defaults for invalid input
- Use regular expressions for format validation

**When to use**: When parsing user input, configuration files, or external data sources.

---

### Recipe 43: NameError

**Problem**: You get "NameError: name 'xyz' is not defined".

**Solution**:

**Basic Example - Undefined Variables**:

```python
print(message)  # NameError: name 'message' is not defined

message = "Hello, World!"
print(message)

greeting = "Hello"
print(greting)  # NameError: name 'greting' is not defined

greeting = "Hello"
print(greeting)  # Correct spelling

def my_function():
    local_var = 42

print(local_var)  # NameError: name 'local_var' is not defined

def my_function():
    local_var = 42
    return local_var

result = my_function()
print(result)
```

**Common Causes**:

- Typo in variable name
- Variable not defined before use
- Scope issues (accessing local variable outside function)
- Forgetting to import module

**Advanced Example - Scope Management**:

```python
from typing import Any

global_var = "I'm global"

def demonstrate_scope():
    local_var = "I'm local"

    # Access global variable
    print(global_var)  # Works

    # Modify global variable (requires global keyword)
    global global_var
    global_var = "Modified global"

    return local_var

result = demonstrate_scope()
print(global_var)  # "Modified global"

def safe_eval(expr: str, context: dict[str, Any]) -> Any | None:
    """
    Safely evaluate expression with given context.

    Example:
        result = safe_eval("x + y", {"x": 10, "y": 20})  # 30
    """
    try:
        return eval(expr, {"__builtins__": {}}, context)
    except NameError as e:
        print(f"Undefined variable in '{expr}': {e}")
        return None

context = {"x": 10, "y": 20, "z": 30}
result1 = safe_eval("x + y", context)  # 30
result2 = safe_eval("x + undefined", context)  # None (catches NameError)

print(f"Result 1: {result1}, Result 2: {result2}")

def is_defined(var_name: str, scope: dict[str, Any]) -> bool:
    """
    Check if variable is defined in given scope.
    """
    return var_name in scope

x = 42

print(is_defined('x', globals()))  # True
print(is_defined('y', globals()))  # False

def my_function():
    local_x = 100
    print(is_defined('local_x', locals()))  # True
    print(is_defined('x', globals()))  # True (global x)

my_function()

class Config:
    """Configuration with lazy initialization."""

    def __getattr__(self, name: str) -> Any:
        """
        Called when attribute doesn't exist.
        Prevents NameError for missing config values.
        """
        print(f"Config '{name}' not defined, using default")
        return None

config = Config()
config.database = "postgres"

print(config.database)  # postgres
print(config.cache)  # None (doesn't exist, __getattr__ called)
```

**Prevention Tips**:

- Use linters (pylint, flake8) to catch undefined variables
- Initialize all variables before use
- Be careful with variable scope (global vs local)
- Use `globals()` and `locals()` to check variable existence

**When to use**: When debugging scope issues or working with dynamic variable names.

---

### Recipe 44: IndentationError

**Problem**: You get "IndentationError: expected an indented block" or "IndentationError: unindent does not match any outer indentation level".

**Solution**:

**Basic Example - Consistent Indentation**:

```python
def my_function():
print("Hello")  # IndentationError: expected an indented block

def my_function():
    print("Hello")  # Correct: 4 spaces

def another_function():
    x = 1  # 4 spaces
	y = 2  # Tab (looks like spaces but isn't)
    # IndentationError: unindent does not match

def another_function():
    x = 1  # 4 spaces
    y = 2  # 4 spaces
```

**Common Causes**:

- Missing indentation after `def`, `if`, `for`, `while`, `class`
- Mixing tabs and spaces
- Incorrect unindentation
- Copy-paste from different sources with different indentation

**Advanced Example - Indentation Best Practices**:

```python
def process_data(items):
    """
    Process items with proper indentation.
    """
    results = []

    for item in items:
        if item > 0:
            # Nested indentation: 8 spaces (2 levels × 4 spaces)
            processed = item * 2
            results.append(processed)
        else:
            # Alternative branch at same level
            results.append(0)

    return results

def function_with_many_parameters(
        param1,
        param2,
        param3,
        param4):
    """Parameters aligned or indented."""
    return param1 + param2 + param3 + param4

result = function_with_many_parameters(
    param1=1,
    param2=2,
    param3=3,
    param4=4
)

filtered_and_processed = [
    item * 2
    for item in range(100)
    if item % 2 == 0
]

config = {
    'database': {
        'host': 'localhost',
        'port': 5432,
        'name': 'mydb'
    },
    'cache': {
        'enabled': True,
        'ttl': 300
    }
}

class DataProcessor:
    """Example class with correct indentation."""

    def __init__(self, name):
        """Constructor."""
        self.name = name
        self.data = []

    def process(self, items):
        """
        Process items.

        Multi-line docstring properly indented.
        """
        for item in items:
            if self._is_valid(item):
                self.data.append(item)

    def _is_valid(self, item):
        """Private method."""
        return item is not None

with open('file.txt', 'r') as f:
    for line in f:
        if line.strip():
            print(line)
```

**Prevention Tips**:

- **Always use 4 spaces for indentation** (PEP 8 standard)
- Configure editor to insert spaces when Tab is pressed
- Use `.editorconfig` file for consistent team settings
- Enable "show whitespace" in your editor
- Use code formatters (black, autopep8) to auto-fix indentation

**Editor Configuration Example** (`.editorconfig`):

```ini
[*.py]
indent_style = space
indent_size = 4
trim_trailing_whitespace = true
```

**When to use**: Configure your editor once and let tools enforce consistency.

---

### Recipe 45: FileNotFoundError

**Problem**: You get "FileNotFoundError: [Errno 2] No such file or directory: 'xyz.txt'".

**Solution**:

**Basic Example - Check File Existence**:

```python
import os
from pathlib import Path

with open('nonexistent.txt', 'r') as f:
    content = f.read()  # FileNotFoundError

if os.path.exists('config.txt'):
    with open('config.txt', 'r') as f:
        content = f.read()
else:
    print("File not found, using defaults")
    content = ""

config_path = Path('config.txt')
if config_path.exists():
    content = config_path.read_text()
else:
    content = ""

try:
    with open('config.txt', 'r') as f:
        content = f.read()
except FileNotFoundError:
    print("File not found, creating default")
    content = "default content"
    with open('config.txt', 'w') as f:
        f.write(content)
```

**Common Causes**:

- Typo in filename
- Wrong directory (relative vs absolute paths)
- File doesn't exist yet
- Insufficient permissions

**Advanced Example - Robust File Operations**:

```python
from pathlib import Path
import os

def safe_read_file(filepath: str | Path,
                   default: str = "",
                   create_if_missing: bool = False) -> str:
    """
    Safely read file with fallback options.

    Args:
        filepath: Path to file
        default: Default content if file doesn't exist
        create_if_missing: Create file with default content if True

    Returns:
        File content or default
    """
    path = Path(filepath)

    try:
        return path.read_text(encoding='utf-8')
    except FileNotFoundError:
        if create_if_missing:
            # Create parent directories if needed
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(default, encoding='utf-8')
            print(f"Created {path} with default content")
        return default
    except PermissionError:
        print(f"Permission denied: {path}")
        return default

config = safe_read_file('config/settings.txt', default='{}', create_if_missing=True)
print(f"Config: {config}")

def find_file(filename: str, search_paths: list[str]) -> Path | None:
    """
    Search for file in multiple directories.

    Returns first match or None.
    """
    for search_path in search_paths:
        path = Path(search_path) / filename
        if path.exists():
            return path
    return None

search_locations = [
    '.',
    './config',
    os.path.expanduser('~/.config/myapp'),
    '/etc/myapp'
]

config_file = find_file('settings.json', search_locations)
if config_file:
    print(f"Found config at: {config_file}")
    content = config_file.read_text()
else:
    print("Config not found in any location")

class SafeFileHandler:
    """Handle files with automatic fallback and logging."""

    def __init__(self, base_path: str | Path):
        self.base_path = Path(base_path)
        self.base_path.mkdir(parents=True, exist_ok=True)

    def read(self, filename: str, default: str = "") -> str:
        """Read file with fallback."""
        filepath = self.base_path / filename
        try:
            return filepath.read_text(encoding='utf-8')
        except FileNotFoundError:
            print(f"File not found: {filepath}")
            return default

    def write(self, filename: str, content: str) -> bool:
        """Write file, create directories if needed."""
        filepath = self.base_path / filename
        try:
            filepath.parent.mkdir(parents=True, exist_ok=True)
            filepath.write_text(content, encoding='utf-8')
            return True
        except (PermissionError, OSError) as e:
            print(f"Write failed: {e}")
            return False

    def exists(self, filename: str) -> bool:
        """Check if file exists."""
        return (self.base_path / filename).exists()

handler = SafeFileHandler('./data')
content = handler.read('users.json', default='[]')
success = handler.write('users.json', '["Alice", "Bob"]')
print(f"File exists: {handler.exists('users.json')}")
```

**Prevention Tips**:

- Use `Path.exists()` before file operations
- Use absolute paths instead of relative when possible
- Create parent directories with `mkdir(parents=True, exist_ok=True)`
- Handle `FileNotFoundError` with try-except
- Log file paths for debugging

**When to use**: When working with files, especially configuration or user-provided paths.

**See Also**:

- [Recipe 19: Path Operations](#recipe-19-path-operations) - Safe path handling
- [Recipe 20: File Reading Patterns](#recipe-20-file-reading-patterns) - File operations

---

### Recipe 46: ZeroDivisionError

**Problem**: You get "ZeroDivisionError: division by zero".

**Solution**:

**Basic Example - Check for Zero**:

```python
result = 10 / 0  # ZeroDivisionError

denominator = 0
if denominator != 0:
    result = 10 / denominator
else:
    result = float('inf')  # or None, or 0, depending on context

try:
    result = 10 / denominator
except ZeroDivisionError:
    result = None
    print("Cannot divide by zero")

denominator = 0
result = 10 / denominator if denominator != 0 else 0
```

**Common Causes**:

- User input of zero
- Calculation resulting in zero
- Empty dataset (average of no items)
- Iteration over empty collection

**Advanced Example - Safe Mathematical Operations**:

```python
import math

def safe_divide(numerator: float, denominator: float,
                default: float | None = None) -> float | None:
    """
    Safely divide with zero check.

    Returns default if denominator is zero.
    """
    if denominator == 0:
        return default
    return numerator / denominator

result1 = safe_divide(10, 2)  # 5.0
result2 = safe_divide(10, 0, default=0)  # 0
result3 = safe_divide(10, 0, default=float('inf'))  # inf

print(f"Results: {result1}, {result2}, {result3}")

def safe_average(numbers: list[float], default: float = 0.0) -> float:
    """
    Calculate average with empty list handling.
    """
    if not numbers:
        return default
    return sum(numbers) / len(numbers)

avg1 = safe_average([10, 20, 30])  # 20.0
avg2 = safe_average([])  # 0.0 (default)
avg3 = safe_average([], default=float('nan'))  # nan

def calculate_statistics(data: list[float]) -> dict:
    """
    Calculate statistics with zero-division protection.
    """
    if not data:
        return {
            'mean': 0.0,
            'variance': 0.0,
            'std_dev': 0.0,
            'coefficient_of_variation': None
        }

    n = len(data)
    mean = sum(data) / n

    variance = sum((x - mean) ** 2 for x in data) / n
    std_dev = math.sqrt(variance)

    # Coefficient of Variation (CV) can have division by zero
    cv = safe_divide(std_dev, mean) if mean != 0 else None

    return {
        'mean': mean,
        'variance': variance,
        'std_dev': std_dev,
        'coefficient_of_variation': cv
    }

data = [10, 20, 30, 40, 50]
stats = calculate_statistics(data)
print(f"Mean: {stats['mean']:.2f}")
print(f"Std Dev: {stats['std_dev']:.2f}")
print(f"CV: {stats['coefficient_of_variation']:.2f if stats['coefficient_of_variation'] else 'N/A'}")

def calculate_percentage(part: float, total: float,
                         decimal_places: int = 2) -> float | None:
    """
    Calculate percentage with zero total handling.

    Returns None if total is zero.
    """
    if total == 0:
        return None
    percentage = (part / total) * 100
    return round(percentage, decimal_places)

pct1 = calculate_percentage(25, 100)  # 25.0
pct2 = calculate_percentage(25, 0)  # None
print(f"Percentage 1: {pct1}%, Percentage 2: {pct2}")

def calculate_rate(items: int, duration_seconds: float,
                   default: float = 0.0) -> float:
    """
    Calculate rate with zero duration handling.
    """
    if duration_seconds <= 0:
        return default
    return items / duration_seconds

rate1 = calculate_rate(100, 10)  # 10.0 items/second
rate2 = calculate_rate(100, 0)  # 0.0 (default)
```

**Prevention Tips**:

- Always validate denominators before division
- Use safe wrapper functions for common calculations
- Return `None` or sentinel values for undefined results
- Consider using `float('inf')` or `float('nan')` for mathematical contexts
- Validate input data before calculations

**When to use**: When performing division, calculating averages, percentages, or rates.

---

## 🔷 Advanced Async Patterns

Modern Python applications leverage async/await for efficient I/O operations. These recipes show production-ready async patterns.

### Recipe: Concurrent API Requests with Rate Limiting

**Problem**: You need to fetch data from multiple API endpoints concurrently while respecting rate limits.

**Solution**:

```python
import asyncio
import aiohttp
from typing import List, Dict
from asyncio import Semaphore

class RateLimitedClient:
    """HTTP client with built-in rate limiting."""

    def __init__(self, max_concurrent: int = 5, requests_per_second: float = 10):
        self.semaphore = Semaphore(max_concurrent)
        self.delay = 1.0 / requests_per_second
        self.last_request = 0

    async def fetch(self, session: aiohttp.ClientSession, url: str) -> Dict:
        """Fetch URL with rate limiting."""
        async with self.semaphore:
            # Ensure minimum delay between requests
            now = asyncio.get_event_loop().time()
            time_since_last = now - self.last_request

            if time_since_last < self.delay:
                await asyncio.sleep(self.delay - time_since_last)

            self.last_request = asyncio.get_event_loop().time()

            try:
                async with session.get(url, timeout=aiohttp.ClientTimeout(total=10)) as response:
                    return {
                        "url": url,
                        "status": response.status,
                        "data": await response.json()
                    }
            except asyncio.TimeoutError:
                return {"url": url, "error": "timeout"}
            except Exception as e:
                return {"url": url, "error": str(e)}

async def fetch_multiple_apis(urls: List[str]) -> List[Dict]:
    """Fetch multiple URLs with rate limiting."""
    client = RateLimitedClient(max_concurrent=5, requests_per_second=10)

    async with aiohttp.ClientSession() as session:
        tasks = [client.fetch(session, url) for url in urls]
        return await asyncio.gather(*tasks)

urls = [
    "https://api.example.com/users/1",
    "https://api.example.com/users/2",
    "https://api.example.com/users/3",
    # ... more URLs
]

results = asyncio.run(fetch_multiple_apis(urls))
for result in results:
    if "error" in result:
        print(f"Failed {result['url']}: {result['error']}")
    else:
        print(f"Success {result['url']}: {result['status']}")
```

**When to use**: When making multiple concurrent HTTP requests, especially to rate-limited APIs.

**See Also**:

- [How to Use Advanced Async Patterns](/en/learn/software-engineering/programming-languages/python/how-to/async-patterns-advanced) - Complete async guide
- [Recipe: Async Context Managers](#recipe-2-context-managers-for-resources) - Resource management

---

### Recipe: Async Generator for Streaming Data

**Problem**: You need to process large datasets without loading everything into memory.

**Solution**:

```python
import asyncio
from typing import AsyncIterator, Dict, List

async def fetch_paginated_data(
    page_size: int = 100,
    max_pages: int = 10
) -> AsyncIterator[Dict]:
    """Stream paginated API results."""
    page = 1

    while page <= max_pages:
        # Simulate API call
        await asyncio.sleep(0.1)
        data = await fetch_page(page, page_size)

        if not data:
            break

        for item in data:
            yield item

        page += 1

async def fetch_page(page: int, size: int) -> List[Dict]:
    """Simulate fetching a page of data."""
    if page > 3:
        return []
    return [{"id": i + (page - 1) * size, "page": page} for i in range(size)]

async def process_large_dataset():
    """Process items one at a time without loading all into memory."""
    item_count = 0

    async for item in fetch_paginated_data(page_size=50):
        await process_item(item)
        item_count += 1

        if item_count % 50 == 0:
            print(f"Processed {item_count} items...")

async def process_item(item: Dict):
    """Process individual item."""
    await asyncio.sleep(0.01)  # Simulate processing
    print(f"Processing {item['id']}")

asyncio.run(process_large_dataset())
```

**When to use**: When working with large datasets, streaming data, or paginated APIs.

**Performance Impact**: Reduces memory usage from O(n) to O(1) for large datasets.

---

### Recipe: Background Task Management with Graceful Shutdown

**Problem**: You need to run background tasks and cleanly shut them down.

**Solution**:

```python
import asyncio
from typing import Set, Coroutine

class BackgroundTaskManager:
    """Manage background tasks with graceful shutdown."""

    def __init__(self):
        self.tasks: Set[asyncio.Task] = set()
        self._shutting_down = False

    def create_task(self, coro: Coroutine) -> asyncio.Task:
        """Create and track background task."""
        if self._shutting_down:
            raise RuntimeError("Cannot create tasks during shutdown")

        task = asyncio.create_task(coro)
        self.tasks.add(task)
        task.add_done_callback(self.tasks.discard)
        return task

    async def shutdown(self, timeout: float = 30.0):
        """Cancel all tasks and wait for completion."""
        self._shutting_down = True

        if not self.tasks:
            return

        # Cancel all tasks
        for task in self.tasks:
            task.cancel()

        # Wait for tasks with timeout
        try:
            await asyncio.wait_for(
                asyncio.gather(*self.tasks, return_exceptions=True),
                timeout=timeout
            )
        except asyncio.TimeoutError:
            print(f"Warning: {len(self.tasks)} tasks did not complete in {timeout}s")

async def background_worker(name: str, interval: float):
    """Long-running background task."""
    try:
        while True:
            print(f"{name} working...")
            await asyncio.sleep(interval)
    except asyncio.CancelledError:
        print(f"{name} shutting down gracefully")
        # Cleanup code here
        raise

async def main():
    manager = BackgroundTaskManager()

    # Start background workers
    manager.create_task(background_worker("Worker-1", 1.0))
    manager.create_task(background_worker("Worker-2", 2.0))
    manager.create_task(background_worker("Worker-3", 1.5))

    # Do main work
    await asyncio.sleep(5)

    # Graceful shutdown
    print("Initiating shutdown...")
    await manager.shutdown(timeout=5.0)
    print("Shutdown complete")

asyncio.run(main())
```

**When to use**: When building services with background tasks (queue processors, periodic jobs, monitoring).

**See Also**:

- [How to Use Advanced Async Patterns](/en/learn/software-engineering/programming-languages/python/how-to/async-patterns-advanced#6-background-task-management) - Complete background task guide

---

## 🔷 Database Patterns with SQLAlchemy

Production applications need robust database interactions. These recipes show SQLAlchemy best practices.

### Recipe: Repository Pattern for Clean Architecture

**Problem**: You want to separate database logic from business logic.

**Solution**:

```python
from abc import ABC, abstractmethod
from typing import Optional, List, Generic, TypeVar
from sqlalchemy.orm import Session
from sqlalchemy import select

T = TypeVar('T')

class Repository(ABC, Generic[T]):
    """Abstract repository for database operations."""

    @abstractmethod
    def get_by_id(self, id: int) -> Optional[T]:
        pass

    @abstractmethod
    def get_all(self, skip: int = 0, limit: int = 100) -> List[T]:
        pass

    @abstractmethod
    def create(self, entity: T) -> T:
        pass

    @abstractmethod
    def update(self, entity: T) -> T:
        pass

    @abstractmethod
    def delete(self, id: int) -> None:
        pass

class SQLAlchemyRepository(Repository[T]):
    """SQLAlchemy implementation of repository."""

    def __init__(self, session: Session, model_class: type):
        self.session = session
        self.model_class = model_class

    def get_by_id(self, id: int) -> Optional[T]:
        return self.session.query(self.model_class).filter_by(id=id).first()

    def get_all(self, skip: int = 0, limit: int = 100) -> List[T]:
        return self.session.query(self.model_class).offset(skip).limit(limit).all()

    def create(self, entity: T) -> T:
        self.session.add(entity)
        self.session.flush()
        self.session.refresh(entity)
        return entity

    def update(self, entity: T) -> T:
        merged = self.session.merge(entity)
        self.session.flush()
        return merged

    def delete(self, id: int) -> None:
        entity = self.get_by_id(id)
        if entity:
            self.session.delete(entity)
            self.session.flush()

from contextlib import contextmanager

@contextmanager
def get_session():
    """Provide transactional session."""
    session = Session(engine)
    try:
        yield session
        session.commit()
    except Exception:
        session.rollback()
        raise
    finally:
        session.close()

def create_user_service(username: str, email: str):
    """Business logic using repository."""
    with get_session() as session:
        user_repo = SQLAlchemyRepository(session, User)

        # Business logic here
        user = User(username=username, email=email)
        return user_repo.create(user)

class InMemoryRepository(Repository[T]):
    """In-memory repository for testing."""

    def __init__(self):
        self.store = {}
        self.next_id = 1

    def get_by_id(self, id: int) -> Optional[T]:
        return self.store.get(id)

    def create(self, entity: T) -> T:
        entity.id = self.next_id
        self.store[self.next_id] = entity
        self.next_id += 1
        return entity

    # ... implement other methods
```

**When to use**: When building applications with clean architecture, testable code, or multiple storage backends.

**Benefits**:

- Separation of concerns (business logic vs data access)
- Easier testing (mock repository)
- Flexible (swap implementations)

**See Also**:

- [How to Work with Databases](/en/learn/software-engineering/programming-languages/python/how-to/work-with-databases#repository-pattern-for-clean-architecture) - Complete database guide

---

### Recipe: Async SQLAlchemy with Connection Pooling

**Problem**: You need high-performance database access in async applications.

**Solution**:

```python
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession, async_sessionmaker
from sqlalchemy.orm import selectinload
from contextlib import asynccontextmanager
from typing import AsyncIterator

engine = create_async_engine(
    "postgresql+asyncpg://user:password@localhost/dbname",
    pool_size=20,           # Maintain 20 connections
    max_overflow=40,        # Allow 40 additional connections
    pool_pre_ping=True,     # Verify connections before use
    pool_recycle=3600,      # Recycle connections after 1 hour
    echo=True               # Log SQL (disable in production)
)

AsyncSessionLocal = async_sessionmaker(
    engine,
    class_=AsyncSession,
    expire_on_commit=False
)

@asynccontextmanager
async def get_db_session() -> AsyncIterator[AsyncSession]:
    """Provide async database session."""
    async with AsyncSessionLocal() as session:
        try:
            yield session
            await session.commit()
        except Exception:
            await session.rollback()
            raise

async def create_user(username: str, email: str):
    """Create user asynchronously."""
    async with get_db_session() as session:
        user = User(username=username, email=email)
        session.add(user)
        await session.flush()
        await session.refresh(user)
        return user

async def get_users_with_posts():
    """Fetch users with eager loading of posts (N+1 prevention)."""
    async with get_db_session() as session:
        stmt = select(User).options(selectinload(User.posts))
        result = await session.execute(stmt)
        return result.scalars().all()

async def bulk_create_users(users_data: List[Dict]):
    """Bulk insert for performance."""
    async with get_db_session() as session:
        users = [User(**data) for data in users_data]
        session.add_all(users)
        # Bulk insert is faster than individual inserts

import asyncio

async def main():
    # Create single user
    user = await create_user("john_doe", "john@example.com")
    print(f"Created user: {user.id}")

    # Fetch with relationships
    users = await get_users_with_posts()
    for user in users:
        print(f"{user.username} has {len(user.posts)} posts")

    # Bulk operations
    users_data = [
        {"username": f"user_{i}", "email": f"user_{i}@example.com"}
        for i in range(1000)
    ]
    await bulk_create_users(users_data)
    print("Bulk insert complete")

asyncio.run(main())
```

**When to use**: When building async web services (FastAPI, aiohttp) with database access.

**Performance Impact**: 10-20x throughput improvement for I/O-bound database operations.

---

## 🔷 FastAPI REST API Patterns

Building production-ready REST APIs requires proper validation, error handling, and dependency injection.

### Recipe: FastAPI with Pydantic Validation and Error Handling

**Problem**: You need type-safe REST API endpoints with comprehensive validation.

**Solution**:

```python
from fastapi import FastAPI, HTTPException, Depends, status
from fastapi.responses import JSONResponse
from fastapi.exceptions import RequestValidationError
from pydantic import BaseModel, EmailStr, Field, validator
from sqlalchemy.ext.asyncio import AsyncSession
from typing import List, Optional

app = FastAPI(title="User API", version="1.0.0")

class UserCreate(BaseModel):
    """User creation request."""
    username: str = Field(..., min_length=3, max_length=50, pattern=r'^[a-zA-Z0-9_]+$')
    email: EmailStr
    password: str = Field(..., min_length=8)
    age: Optional[int] = Field(None, ge=18, le=150)

    @validator('password')
    def password_strength(cls, v):
        if not any(char.isdigit() for char in v):
            raise ValueError('Password must contain at least one digit')
        if not any(char.isupper() for char in v):
            raise ValueError('Password must contain uppercase letter')
        return v

class UserResponse(BaseModel):
    """User response model."""
    id: int
    username: str
    email: str
    age: Optional[int]

    class Config:
        from_attributes = True

class ErrorResponse(BaseModel):
    """Standard error response."""
    detail: str
    error_code: str

@app.exception_handler(RequestValidationError)
async def validation_exception_handler(request, exc: RequestValidationError):
    """Handle validation errors."""
    errors = []
    for error in exc.errors():
        errors.append({
            "field": ".".join(str(loc) for loc in error["loc"]),
            "message": error["msg"],
            "type": error["type"]
        })

    return JSONResponse(
        status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
        content={"detail": errors}
    )

@app.exception_handler(Exception)
async def general_exception_handler(request, exc: Exception):
    """Handle unexpected errors."""
    return JSONResponse(
        status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
        content={"detail": "Internal server error", "error_code": "INTERNAL_ERROR"}
    )

async def get_db() -> AsyncSession:
    """Provide database session."""
    async with get_db_session() as session:
        yield session

@app.post(
    "/users",
    response_model=UserResponse,
    status_code=status.HTTP_201_CREATED,
    responses={
        422: {"model": ErrorResponse, "description": "Validation error"},
        500: {"model": ErrorResponse, "description": "Internal error"}
    }
)
async def create_user(
    user: UserCreate,
    db: AsyncSession = Depends(get_db)
):
    """Create new user with validation."""
    # Check for duplicate
    existing = await get_user_by_username(db, user.username)
    if existing:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Username already exists"
        )

    # Create user
    db_user = User(
        username=user.username,
        email=user.email,
        hashed_password=hash_password(user.password),
        age=user.age
    )
    db.add(db_user)
    await db.flush()
    await db.refresh(db_user)

    return db_user

@app.get("/users", response_model=List[UserResponse])
async def list_users(
    skip: int = 0,
    limit: int = 100,
    db: AsyncSession = Depends(get_db)
):
    """List users with pagination."""
    stmt = select(User).offset(skip).limit(limit)
    result = await db.execute(stmt)
    return result.scalars().all()
```

**When to use**: When building REST APIs with type safety and automatic documentation.

**Benefits**:

- Automatic OpenAPI documentation
- Request/response validation
- Type safety with Pydantic
- Dependency injection

**See Also**:

- [How to Build REST APIs](/en/learn/software-engineering/programming-languages/python/how-to/build-rest-apis) - Complete FastAPI guide
- [How to Implement Data Validation](/en/learn/software-engineering/programming-languages/python/how-to/data-validation-patterns) - Pydantic patterns

---

### Recipe: JWT Authentication with Dependency Injection

**Problem**: You need to protect endpoints with JWT authentication.

**Solution**:

```python
from fastapi import Depends, HTTPException, status
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from jose import JWTError, jwt
from passlib.context import CryptContext
from datetime import datetime, timedelta
from typing import Optional

SECRET_KEY = "your-secret-key-keep-it-secret"
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30

pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
security = HTTPBearer()

def verify_password(plain: str, hashed: str) -> bool:
    return pwd_context.verify(plain, hashed)

def hash_password(password: str) -> str:
    return pwd_context.hash(password)

def create_access_token(data: dict, expires_delta: Optional[timedelta] = None):
    to_encode = data.copy()
    expire = datetime.utcnow() + (expires_delta or timedelta(minutes=15))
    to_encode.update({"exp": expire})
    return jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)

async def get_current_user(
    credentials: HTTPAuthorizationCredentials = Depends(security),
    db: AsyncSession = Depends(get_db)
) -> User:
    """Verify JWT and return current user."""
    credentials_exception = HTTPException(
        status_code=status.HTTP_401_UNAUTHORIZED,
        detail="Could not validate credentials",
        headers={"WWW-Authenticate": "Bearer"},
    )

    try:
        token = credentials.credentials
        payload = jwt.decode(token, SECRET_KEY, algorithms=[ALGORITHM])
        username: str = payload.get("sub")
        if username is None:
            raise credentials_exception
    except JWTError:
        raise credentials_exception

    stmt = select(User).where(User.username == username)
    result = await db.execute(stmt)
    user = result.scalar_one_or_none()

    if user is None:
        raise credentials_exception

    return user

@app.post("/auth/login")
async def login(username: str, password: str, db: AsyncSession = Depends(get_db)):
    """Authenticate and return JWT token."""
    stmt = select(User).where(User.username == username)
    result = await db.execute(stmt)
    user = result.scalar_one_or_none()

    if not user or not verify_password(password, user.hashed_password):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Incorrect username or password"
        )

    access_token = create_access_token(
        data={"sub": user.username},
        expires_delta=timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    )

    return {"access_token": access_token, "token_type": "bearer"}

@app.get("/users/me", response_model=UserResponse)
async def get_current_user_info(current_user: User = Depends(get_current_user)):
    """Get current authenticated user."""
    return current_user

async def require_admin(current_user: User = Depends(get_current_user)) -> User:
    """Require admin role."""
    if not current_user.is_admin:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Admin access required"
        )
    return current_user

@app.delete("/users/{user_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_user(
    user_id: int,
    db: AsyncSession = Depends(get_db),
    admin: User = Depends(require_admin)
):
    """Delete user (admin only)."""
    user = await db.get(User, user_id)
    if not user:
        raise HTTPException(status_code=404, detail="User not found")

    await db.delete(user)
    await db.flush()
```

**When to use**: When building secured REST APIs with user authentication.

**Security Considerations**:

- Use strong secret keys (store in environment variables)
- Set appropriate token expiration
- Use HTTPS in production
- Consider refresh tokens for long-lived sessions

---

## 🔷 Security Patterns

Production applications must handle credentials and sensitive data securely.

### Recipe: Secure Credential Management with Environment Variables

**Problem**: You need to manage secrets without hardcoding them.

**Solution**:

```python
import os
from pathlib import Path
from typing import Optional
from pydantic import BaseSettings, Field, validator

class Settings(BaseSettings):
    """Application settings with validation."""

    # Database
    database_url: str = Field(..., env='DATABASE_URL')
    database_pool_size: int = Field(10, env='DB_POOL_SIZE')

    # Security
    secret_key: str = Field(..., env='SECRET_KEY')
    api_key: str = Field(..., env='API_KEY')

    # Application
    app_name: str = Field('MyApp', env='APP_NAME')
    debug_mode: bool = Field(False, env='DEBUG')
    log_level: str = Field('INFO', env='LOG_LEVEL')

    # Redis
    redis_url: Optional[str] = Field(None, env='REDIS_URL')

    @validator('secret_key')
    def validate_secret_key(cls, v):
        if len(v) < 32:
            raise ValueError('SECRET_KEY must be at least 32 characters')
        return v

    @validator('log_level')
    def validate_log_level(cls, v):
        valid_levels = ['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL']
        if v.upper() not in valid_levels:
            raise ValueError(f'Invalid log level: {v}')
        return v.upper()

    class Config:
        env_file = '.env'
        env_file_encoding = 'utf-8'
        case_sensitive = False

settings = Settings()

from sqlalchemy import create_engine

engine = create_engine(
    settings.database_url,
    pool_size=settings.database_pool_size
)

"""
DATABASE_URL=postgresql://user:password@localhost/dbname
DB_POOL_SIZE=20
SECRET_KEY=your-super-secret-key-at-least-32-characters-long
API_KEY=your-api-key-here
DEBUG=False
LOG_LEVEL=INFO
REDIS_URL=redis://localhost:6379/0
"""

"""
DATABASE_URL=postgresql://user:password@localhost/dbname
DB_POOL_SIZE=10
SECRET_KEY=change-me-to-secure-random-string
API_KEY=your-api-key
DEBUG=False
LOG_LEVEL=INFO
REDIS_URL=redis://localhost:6379/0
"""

"""
.env
.env.local
.env.*.local
"""
```

**When to use**: Always, for any secret or environment-specific configuration.

**Best Practices**:

- Never hardcode secrets
- Use different .env files for dev/staging/prod
- Validate all settings on startup
- Provide .env.example for documentation
- Use secret management services in production (AWS Secrets Manager, HashiCorp Vault)

**See Also**:

- [How to Implement Security Best Practices](/en/learn/software-engineering/programming-languages/python/how-to/security-best-practices) - Complete security guide

---

## 🎯 Practice Exercises

Apply the cookbook recipes with these hands-on challenges.

### Exercise 1: Build a File Processor (Intermediate)

Create a CLI tool that:

- Reads text files from a directory
- Counts word frequencies using Counter
- Exports results to JSON or CSV
- Uses type hints and logging
- Includes error handling

**Hint**: Combine Path operations, collections, and CLI recipes.

### Exercise 2: Async Web Scraper (Advanced)

Build a concurrent web scraper that:

- Fetches multiple URLs using aiohttp
- Extracts data using BeautifulSoup
- Saves results to database
- Implements retry logic with decorators
- Uses async queue for rate limiting

**Hint**: Combine async patterns, decorators, and error handling.

### Exercise 3: Configuration Manager (Intermediate)

Create a configuration system that:

- Loads from YAML/JSON files
- Overrides with environment variables
- Validates using Pydantic
- Supports multiple environments
- Includes type-safe access

**Hint**: Use configuration recipes and type hints.

### Exercise 4: Data Pipeline (Advanced)

Implement a data processing pipeline that:

- Reads CSV files using generators
- Transforms data with comprehensions
- Groups by keys using defaultdict
- Writes results with context managers
- Logs progress at each stage

**Hint**: Combine generators, collections, and file operations.

### Exercise 5: Testing Suite (Expert)

Write comprehensive tests for a calculator module:

- Parametrized tests for all operations
- Mock external dependencies
- Test async operations
- Measure coverage
- Use fixtures for test data

**Hint**: Apply pytest patterns and mocking recipes.

---

## 🚀 Next Steps

You've learned practical Python patterns! Continue your journey:

### Practice Projects

- **Build a REST API** - Use FastAPI or Flask
- **Create a data pipeline** - Use pandas or Polars
- **Develop a CLI tool** - Use Click or Typer
- **Build a web scraper** - Use aiohttp and BeautifulSoup
- **Create an async service** - Use asyncio and aiohttp

### Advanced Topics

- **Type system** - Master mypy and advanced type hints
- **Performance** - Profile with cProfile, optimize with Cython
- **Concurrency** - AsyncIO patterns, multiprocessing
- **Packaging** - Create distributable packages
- **Web frameworks** - FastAPI, Django, Flask patterns

### Resources

- **Official Documentation** - [docs.python.org](https://docs.python.org/)
- **Type Hints** - [mypy documentation](https://mypy.readthedocs.io/)
- **AsyncIO** - [asyncio documentation](https://docs.python.org/3/library/asyncio.html)
- **pytest** - [pytest documentation](https://docs.pytest.org/)

---

**Happy Cooking! 🍳**
