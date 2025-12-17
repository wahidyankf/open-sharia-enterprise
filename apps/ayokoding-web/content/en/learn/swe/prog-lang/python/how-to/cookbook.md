---
title: Cookbook
date: 2025-12-17T00:00:00+07:00
draft: false
description: Practical recipes and patterns for idiomatic Python programming
weight: 503
tags:
  - python
  - cookbook
  - patterns
  - best-practices
  - type-hints
  - async
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

- ✅ Complete the [Python Beginner tutorial](/en/learn/swe/prog-lang/python/tutorials/beginner) - or have equivalent experience with Python fundamentals
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
# Basic list comprehension
numbers = [1, 2, 3, 4, 5]
squares = [n ** 2 for n in numbers]
# Result: [1, 4, 9, 16, 25]

# With condition
evens = [n for n in numbers if n % 2 == 0]
# Result: [2, 4]

# Nested comprehension
matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
flattened = [item for row in matrix for item in row]
# Result: [1, 2, 3, 4, 5, 6, 7, 8, 9]

# Transform with condition
positive_squares = [n ** 2 for n in [-2, -1, 0, 1, 2] if n > 0]
# Result: [1, 4]

# Multiple lists (cartesian product)
colors = ['red', 'blue']
sizes = ['S', 'M', 'L']
combinations = [(color, size) for color in colors for size in sizes]
# Result: [('red', 'S'), ('red', 'M'), ('red', 'L'),
#          ('blue', 'S'), ('blue', 'M'), ('blue', 'L')]
```

**When to use**: When you need to create a new list from an existing iterable with transformation or filtering.

---

### Recipe 2: Dictionary Comprehensions

**Problem**: You need to build or transform dictionaries efficiently.

**Solution**:

```python
# Basic dict comprehension
numbers = [1, 2, 3, 4, 5]
squares_dict = {n: n ** 2 for n in numbers}
# Result: {1: 1, 2: 4, 3: 9, 4: 16, 5: 25}

# From two lists
keys = ['name', 'age', 'city']
values = ['Alice', 30, 'NYC']
person = {k: v for k, v in zip(keys, values)}
# Result: {'name': 'Alice', 'age': 30, 'city': 'NYC'}

# Filter dictionary
scores = {'Alice': 85, 'Bob': 92, 'Charlie': 78, 'Diana': 95}
high_scores = {name: score for name, score in scores.items() if score >= 90}
# Result: {'Bob': 92, 'Diana': 95}

# Transform dictionary values
celsius = {'morning': 20, 'afternoon': 25, 'evening': 22}
fahrenheit = {time: (temp * 9/5) + 32 for time, temp in celsius.items()}
# Result: {'morning': 68.0, 'afternoon': 77.0, 'evening': 71.6}

# Swap keys and values
original = {'a': 1, 'b': 2, 'c': 3}
swapped = {v: k for k, v in original.items()}
# Result: {1: 'a', 2: 'b', 3: 'c'}
```

**When to use**: When building or transforming dictionaries, especially for data processing.

---

### Recipe 3: Set Comprehensions and Operations

**Problem**: You need unique collections or set operations.

**Solution**:

```python
# Set comprehension
numbers = [1, 2, 2, 3, 3, 3, 4, 5]
unique_squares = {n ** 2 for n in numbers}
# Result: {1, 4, 9, 16, 25}

# Remove duplicates while preserving order (Python 3.7+)
items = ['apple', 'banana', 'apple', 'cherry', 'banana']
unique_items = list(dict.fromkeys(items))
# Result: ['apple', 'banana', 'cherry']

# Set operations
set_a = {1, 2, 3, 4, 5}
set_b = {4, 5, 6, 7, 8}

# Union (all items)
union = set_a | set_b
# Result: {1, 2, 3, 4, 5, 6, 7, 8}

# Intersection (common items)
intersection = set_a & set_b
# Result: {4, 5}

# Difference (in A but not B)
difference = set_a - set_b
# Result: {1, 2, 3}

# Symmetric difference (in A or B but not both)
sym_diff = set_a ^ set_b
# Result: {1, 2, 3, 6, 7, 8}

# Check membership
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

# defaultdict - automatic default values
word_lists = defaultdict(list)
words = [('fruit', 'apple'), ('fruit', 'banana'), ('veggie', 'carrot')]
for category, item in words:
    word_lists[category].append(item)
# Result: {'fruit': ['apple', 'banana'], 'veggie': ['carrot']}

# defaultdict with int (counting)
word_counts = defaultdict(int)
sentence = "the quick brown fox jumps over the lazy dog"
for word in sentence.split():
    word_counts[word] += 1
# Result: {'the': 2, 'quick': 1, 'brown': 1, ...}

# Counter - specialized for counting
text = "hello world"
letter_counts = Counter(text)
# Result: Counter({'l': 3, 'o': 2, 'h': 1, 'e': 1, ' ': 1, ...})

# Most common elements
letter_counts.most_common(3)
# Result: [('l', 3), ('o', 2), ('h', 1)]

# Combine counters
c1 = Counter(['a', 'b', 'c', 'a'])
c2 = Counter(['b', 'c', 'd', 'b'])
combined = c1 + c2
# Result: Counter({'b': 3, 'c': 2, 'a': 2, 'd': 1})

# Count from iterable
words = ['apple', 'banana', 'apple', 'cherry', 'banana', 'apple']
fruit_counts = Counter(words)
# Result: Counter({'apple': 3, 'banana': 2, 'cherry': 1})
```

**When to use**: When you need default values in dicts or counting frequencies.

---

### Recipe 5: Generator Expressions

**Problem**: You need memory-efficient iteration over large datasets.

**Solution**:

```python
# Generator expression (lazy evaluation)
numbers = range(1, 1000000)
squares_gen = (n ** 2 for n in numbers)  # No computation yet

# Iterate when needed
for square in squares_gen:
    if square > 100:
        print(square)
        break  # Only computed up to this point

# Generator function
def fibonacci():
    """Generate fibonacci sequence infinitely"""
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b

# Use generator
fib = fibonacci()
first_ten = [next(fib) for _ in range(10)]
# Result: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

# File reading generator
def read_large_file(filepath):
    """Memory-efficient file reading"""
    with open(filepath, 'r') as f:
        for line in f:
            yield line.strip()

# Process file line by line
for line in read_large_file('data.txt'):
    process(line)  # Only one line in memory at a time

# Generator pipeline
def read_csv(filepath):
    with open(filepath) as f:
        for line in f:
            yield line.strip().split(',')

def filter_rows(rows, min_value):
    for row in rows:
        if int(row[1]) >= min_value:
            yield row

# Chain generators
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

# Group by key
data = [
    {'name': 'Alice', 'dept': 'Engineering'},
    {'name': 'Bob', 'dept': 'Sales'},
    {'name': 'Charlie', 'dept': 'Engineering'},
    {'name': 'Diana', 'dept': 'Sales'}
]

# Using dict comprehension
from collections import defaultdict
grouped = defaultdict(list)
for person in data:
    grouped[person['dept']].append(person['name'])
# Result: {'Engineering': ['Alice', 'Charlie'], 'Sales': ['Bob', 'Diana']}

# Using itertools.groupby (requires sorted data)
data_sorted = sorted(data, key=lambda x: x['dept'])
for dept, group in groupby(data_sorted, key=lambda x: x['dept']):
    people = [person['name'] for person in group]
    print(f"{dept}: {people}")

# Flatten nested lists
nested = [[1, 2, 3], [4, 5], [6, 7, 8]]
flattened = list(chain.from_iterable(nested))
# Result: [1, 2, 3, 4, 5, 6, 7, 8]

# Or using chain(*nested)
flattened = list(chain(*nested))

# Combinations
items = ['A', 'B', 'C']
pairs = list(combinations(items, 2))
# Result: [('A', 'B'), ('A', 'C'), ('B', 'C')]

# Cartesian product
colors = ['red', 'blue']
sizes = ['S', 'M']
variants = list(product(colors, sizes))
# Result: [('red', 'S'), ('red', 'M'), ('blue', 'S'), ('blue', 'M')]
```

**When to use**: When grouping data, flattening lists, or generating combinations/permutations.

---

## 🔷 Type Hints and Type Safety

Python 3.5+ supports type hints for static type checking with tools like mypy.

### Recipe 7: Basic Type Hints

**Problem**: You want static type checking to catch bugs early.

**Solution**:

```python
from typing import List, Dict, Set, Optional, Union, Tuple

# Basic types
def greet(name: str) -> str:
    return f"Hello, {name}"

# Collection types
def sum_numbers(numbers: List[int]) -> int:
    return sum(numbers)

def get_scores() -> Dict[str, int]:
    return {'Alice': 95, 'Bob': 87}

def unique_items(items: List[str]) -> Set[str]:
    return set(items)

# Optional (can be None)
def find_user(user_id: int) -> Optional[str]:
    users = {1: 'Alice', 2: 'Bob'}
    return users.get(user_id)  # Returns str or None

# Union (multiple types)
def parse_value(value: Union[int, str]) -> int:
    if isinstance(value, str):
        return int(value)
    return value

# Tuple with specific types
def get_coordinates() -> Tuple[float, float]:
    return (40.7128, -74.0060)

# Multiple return values
def divide(a: int, b: int) -> Tuple[int, int]:
    return a // b, a % b  # quotient, remainder

# Complex nested types
def process_data() -> Dict[str, List[Tuple[int, str]]]:
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
from typing import TypeVar, Generic, Protocol, Callable, Any

# Type variables
T = TypeVar('T')

def first_element(items: List[T]) -> Optional[T]:
    return items[0] if items else None

# Works with any type
numbers = [1, 2, 3]
first_num = first_element(numbers)  # Type: Optional[int]

strings = ['a', 'b', 'c']
first_str = first_element(strings)  # Type: Optional[str]

# Generic class
class Stack(Generic[T]):
    def __init__(self) -> None:
        self.items: List[T] = []

    def push(self, item: T) -> None:
        self.items.append(item)

    def pop(self) -> Optional[T]:
        return self.items.pop() if self.items else None

    def is_empty(self) -> bool:
        return len(self.items) == 0

# Type-safe stack usage
int_stack: Stack[int] = Stack()
int_stack.push(1)
int_stack.push(2)
value: Optional[int] = int_stack.pop()

# Protocol (structural typing)
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

# Callable types
def apply_operation(x: int, operation: Callable[[int], int]) -> int:
    return operation(x)

result = apply_operation(5, lambda x: x ** 2)  # 25

# NewType for semantic clarity
from typing import NewType

UserId = NewType('UserId', int)
OrderId = NewType('OrderId', int)

def get_user(user_id: UserId) -> str:
    return f"User {user_id}"

# Type-safe usage
user_id = UserId(123)
order_id = OrderId(456)
get_user(user_id)  # ✅ OK
# get_user(order_id)  # ❌ Type error (if using mypy)
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
# Output: slow_function took 1.0012 seconds

# Decorator with arguments
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

# Built-in LRU cache
@functools.lru_cache(maxsize=128)
def fibonacci(n: int) -> int:
    """Fibonacci with memoization"""
    if n < 2:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

# Fast computation due to caching
result = fibonacci(100)

# Custom cache decorator
def memoize(func: Callable[..., Any]) -> Callable[..., Any]:
    """Simple memoization decorator"""
    cache: Dict[str, Any] = {}

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
# area = calculate_area(-5, 10)  # ❌ ValueError

# Type validation decorator
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
# user = create_user("Alice", "30")  # ❌ TypeError
```

**When to use**: When you need runtime validation of function arguments.

---

## 🔷 Context Managers

Context managers ensure proper resource cleanup using `with` statements.

### Recipe 12: File Context Manager

**Problem**: You need to ensure files are properly closed even if errors occur.

**Solution**:

```python
# Built-in file context manager
with open('data.txt', 'r') as f:
    content = f.read()
    # File automatically closed, even if exception occurs

# Multiple files
with open('input.txt', 'r') as infile, open('output.txt', 'w') as outfile:
    for line in infile:
        outfile.write(line.upper())

# Custom context manager using class
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

# Usage
with DatabaseConnection("postgresql://localhost") as conn:
    print(f"Using {conn}")
# Output:
# Connecting to postgresql://localhost
# Using Connection(postgresql://localhost)
# Closing connection

# Context manager using contextlib
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

# Usage
with timer("Database query"):
    time.sleep(0.5)
    result = "data"
# Output: Database query took 0.5012 seconds

# More sophisticated timer with result capture
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

# Usage with access to elapsed time
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
from typing import List

# Basic async function
async def fetch_data(url: str) -> str:
    """Simulate async data fetching"""
    print(f"Fetching {url}")
    await asyncio.sleep(1)  # Simulate network delay
    return f"Data from {url}"

# Run single async function
async def main():
    result = await fetch_data("https://api.example.com")
    print(result)

# Run async function
asyncio.run(main())

# Concurrent execution with gather
async def fetch_all():
    urls = [
        "https://api.example.com/users",
        "https://api.example.com/posts",
        "https://api.example.com/comments"
    ]

    # Run all concurrently
    results = await asyncio.gather(*[fetch_data(url) for url in urls])
    return results

# Run concurrent operations
results = asyncio.run(fetch_all())
# All three requests run in parallel, total time ~1 second instead of 3

# Create tasks for more control
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
from typing import List, Dict

async def fetch_url(session: aiohttp.ClientSession, url: str) -> str:
    """Fetch URL content asynchronously"""
    async with session.get(url) as response:
        return await response.text()

async def fetch_multiple_urls(urls: List[str]) -> List[str]:
    """Fetch multiple URLs concurrently"""
    async with aiohttp.ClientSession() as session:
        tasks = [fetch_url(session, url) for url in urls]
        results = await asyncio.gather(*tasks)
        return results

# Usage
urls = [
    'https://httpbin.org/delay/1',
    'https://httpbin.org/delay/1',
    'https://httpbin.org/delay/1'
]
results = asyncio.run(fetch_multiple_urls(urls))
# Total time ~1 second (concurrent) instead of 3 seconds (sequential)

# With error handling
async def fetch_with_error_handling(url: str) -> Dict[str, any]:
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

# Async generator for streaming
async def fetch_stream(url: str):
    """Stream large response line by line"""
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            async for line in response.content:
                yield line.decode('utf-8')
```

**When to use**: When making multiple HTTP requests or streaming large responses.

---

### Recipe 16: Async Queue Pattern

**Problem**: You need producer-consumer pattern with async workers.

**Solution**:

```python
import asyncio
from asyncio import Queue
from typing import Optional

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
# Basic try-except
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero")

# Multiple exceptions
try:
    value = int("not a number")
except (ValueError, TypeError) as e:
    print(f"Conversion error: {e}")

# Catch all with logging
import logging

try:
    risky_operation()
except Exception as e:
    logging.error(f"Operation failed: {e}", exc_info=True)
    raise  # Re-raise after logging

# try-except-else-finally
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

# Custom exceptions
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

# Context manager for error handling
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

---

## 🔷 File and Path Operations

Modern Python uses `pathlib` for path handling and provides robust file operations.

### Recipe 18: Path Operations

**Problem**: You need to work with file paths safely across platforms.

**Solution**:

```python
from pathlib import Path

# Create Path object
path = Path('data/files/document.txt')

# Get parts
print(path.name)       # document.txt
print(path.stem)       # document
print(path.suffix)     # .txt
print(path.parent)     # data/files
print(path.parts)      # ('data', 'files', 'document.txt')

# Join paths (works on all platforms)
base = Path('data')
full_path = base / 'files' / 'document.txt'  # Pythonic!

# Check existence
if path.exists():
    print("File exists")

if path.is_file():
    print("It's a file")

if path.is_dir():
    print("It's a directory")

# Create directory
output_dir = Path('output/results')
output_dir.mkdir(parents=True, exist_ok=True)  # Create all parent dirs

# List directory contents
data_dir = Path('data')
for item in data_dir.iterdir():
    print(item)

# Find files by pattern
for txt_file in data_dir.glob('*.txt'):
    print(txt_file)

# Recursive search
for py_file in data_dir.rglob('*.py'):
    print(py_file)

# Read/write files
path = Path('config.txt')
path.write_text('key=value')
content = path.read_text()

# Binary files
data = b'\x00\x01\x02'
path.write_bytes(data)
binary_content = path.read_bytes()

# Get absolute path
absolute = path.resolve()

# Home directory
home = Path.home()
config_file = home / '.config' / 'app' / 'settings.ini'

# Temporary directory
import tempfile
with tempfile.TemporaryDirectory() as temp_dir:
    temp_path = Path(temp_dir)
    # Use temp_path
    # Automatically cleaned up after with block
```

**When to use**: Always prefer `pathlib` over `os.path` for modern Python code.

---

### Recipe 19: File Reading Patterns

**Problem**: You need to read files efficiently in various formats.

**Solution**:

```python
from pathlib import Path
from typing import List, Iterator

# Read entire file
content = Path('data.txt').read_text()

# Read file line by line (memory efficient)
def read_lines(filepath: Path) -> Iterator[str]:
    with filepath.open('r') as f:
        for line in f:
            yield line.strip()

for line in read_lines(Path('large_file.txt')):
    process(line)

# Read file as list
lines: List[str] = Path('data.txt').read_text().splitlines()

# Read CSV
import csv
from pathlib import Path

def read_csv(filepath: Path) -> List[Dict[str, str]]:
    with filepath.open('r') as f:
        reader = csv.DictReader(f)
        return list(reader)

# Read JSON
import json

def read_json(filepath: Path) -> dict:
    return json.loads(filepath.read_text())

# Or using json.load
def read_json_stream(filepath: Path) -> dict:
    with filepath.open('r') as f:
        return json.load(f)

# Read YAML (requires PyYAML)
import yaml

def read_yaml(filepath: Path) -> dict:
    return yaml.safe_load(filepath.read_text())

# Read with specific encoding
content = Path('data.txt').read_text(encoding='utf-8')

# Read binary file
data = Path('image.png').read_bytes()

# Context manager for multiple operations
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

### Recipe 20: String Formatting

**Problem**: You need to format strings with variables.

**Solution**:

```python
name = "Alice"
age = 30
balance = 1234.5678

# f-strings (Python 3.6+) - RECOMMENDED
message = f"Hello, {name}! You are {age} years old."
# Result: "Hello, Alice! You are 30 years old."

# Expressions in f-strings
status = f"{name} has ${balance:.2f} in account"
# Result: "Alice has $1234.57 in account"

# Calling functions in f-strings
upper_name = f"Name: {name.upper()}"
# Result: "Name: ALICE"

# Format specifiers
pi = 3.14159
formatted = f"Pi is approximately {pi:.2f}"  # 3.14

# Number formatting
num = 1234567
formatted_num = f"{num:,}"  # 1,234,567
formatted_num = f"{num:_}"  # 1_234_567

# Padding and alignment
text = "hello"
print(f"|{text:>10}|")  # Right-align:  |     hello|
print(f"|{text:<10}|")  # Left-align:   |hello     |
print(f"|{text:^10}|")  # Center:       |  hello   |

# Date formatting
from datetime import datetime
now = datetime.now()
formatted_date = f"{now:%Y-%m-%d %H:%M:%S}"
# Result: "2025-12-17 14:30:45"

# Multiline f-strings
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

# str.format() (older style)
message = "Hello, {}! You are {} years old.".format(name, age)
message = "Hello, {name}! You are {age} years old.".format(name=name, age=age)

# Template strings (for user-provided templates)
from string import Template
template = Template("Hello, $name! You are $age years old.")
result = template.substitute(name=name, age=age)
```

**When to use**: f-strings for most cases, Template for user-provided format strings.

---

### Recipe 21: String Manipulation

**Problem**: You need to parse, split, or transform strings.

**Solution**:

```python
# Split and join
text = "apple,banana,cherry"
fruits = text.split(',')  # ['apple', 'banana', 'cherry']
joined = ', '.join(fruits)  # 'apple, banana, cherry'

# Strip whitespace
dirty = "  hello world  "
clean = dirty.strip()  # "hello world"
left_clean = dirty.lstrip()  # "hello world  "
right_clean = dirty.rstrip()  # "  hello world"

# Replace
text = "Hello World"
replaced = text.replace("World", "Python")  # "Hello Python"
replaced_all = text.replace("l", "L")  # "HeLLo WorLd"

# Case conversion
text = "Hello World"
print(text.lower())      # hello world
print(text.upper())      # HELLO WORLD
print(text.capitalize()) # Hello world
print(text.title())      # Hello World
print(text.swapcase())   # hELLO wORLD

# Check string properties
text = "hello123"
print(text.isalnum())    # True (alphanumeric)
print(text.isalpha())    # False (has numbers)
print(text.isdigit())    # False (has letters)
print(text.islower())    # True
print(text.isupper())    # False

# starts/endswith
filename = "document.txt"
if filename.endswith('.txt'):
    print("Text file")

url = "https://example.com"
if url.startswith('https://'):
    print("Secure URL")

# Count occurrences
text = "hello world hello"
count = text.count('hello')  # 2

# Find position
position = text.find('world')  # 6
position = text.rfind('hello')  # 12 (last occurrence)

# Padding
text = "42"
padded = text.zfill(5)  # "00042"
padded = text.rjust(5, '0')  # "00042"
padded = text.ljust(5, '0')  # "42000"

# Remove prefix/suffix (Python 3.9+)
text = "https://example.com"
without_protocol = text.removeprefix('https://')  # "example.com"

filename = "document.txt"
without_ext = filename.removesuffix('.txt')  # "document"
```

**When to use**: For text parsing, cleaning, and transformation operations.

---

## 🔷 Date and Time Operations

Python's `datetime` module provides comprehensive date/time handling.

### Recipe 22: Working with Dates

**Problem**: You need to parse, format, and manipulate dates.

**Solution**:

```python
from datetime import datetime, date, time, timedelta
from zoneinfo import ZoneInfo  # Python 3.9+

# Current date and time
now = datetime.now()
today = date.today()
current_time = datetime.now().time()

# Create specific date/time
specific = datetime(2025, 12, 17, 14, 30, 0)
specific_date = date(2025, 12, 17)

# Parse from string
date_str = "2025-12-17"
parsed = datetime.strptime(date_str, "%Y-%m-%d")

time_str = "2025-12-17 14:30:45"
parsed_time = datetime.strptime(time_str, "%Y-%m-%d %H:%M:%S")

# Format to string
formatted = now.strftime("%Y-%m-%d %H:%M:%S")
# Result: "2025-12-17 14:30:45"

# Common formats
formats = {
    'ISO': now.isoformat(),  # "2025-12-17T14:30:45.123456"
    'US': now.strftime("%m/%d/%Y"),  # "12/17/2025"
    'EU': now.strftime("%d/%m/%Y"),  # "17/12/2025"
    'Long': now.strftime("%B %d, %Y"),  # "December 17, 2025"
}

# Date arithmetic
tomorrow = today + timedelta(days=1)
yesterday = today - timedelta(days=1)
next_week = today + timedelta(weeks=1)
three_hours_ago = now - timedelta(hours=3)

# Difference between dates
start = datetime(2025, 1, 1)
end = datetime(2025, 12, 31)
difference = end - start
print(f"Days: {difference.days}")
print(f"Total seconds: {difference.total_seconds()}")

# Timezone-aware datetime (Python 3.9+)
utc_now = datetime.now(ZoneInfo("UTC"))
jakarta_now = datetime.now(ZoneInfo("Asia/Jakarta"))

# Convert timezone
utc_time = datetime.now(ZoneInfo("UTC"))
jakarta_time = utc_time.astimezone(ZoneInfo("Asia/Jakarta"))

# Compare dates
if date.today() < date(2026, 1, 1):
    print("Still in 2025")

# Get date components
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

### Recipe 23: Configuration from Environment

**Problem**: You need to load configuration from environment variables.

**Solution**:

```python
import os
from typing import Optional

# Get environment variable
api_key = os.getenv('API_KEY')
port = os.getenv('PORT', '8000')  # Default value

# Convert types
port_int = int(os.getenv('PORT', '8000'))
debug_mode = os.getenv('DEBUG', 'false').lower() == 'true'

# Using python-decouple (recommended)
from decouple import config

# Load from .env file or environment
api_key = config('API_KEY')
port = config('PORT', default=8000, cast=int)
debug = config('DEBUG', default=False, cast=bool)
database_url = config('DATABASE_URL')

# Pydantic for validation (recommended for production)
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

# Load and validate settings
settings = Settings()
print(f"API Key: {settings.api_key}")
print(f"Port: {settings.port}")

# Type-safe access
def connect_to_db(url: str):
    print(f"Connecting to {url}")

connect_to_db(settings.database_url)  # Type-checked!

# Environment-specific config
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

### Recipe 24: Configuration from Files

**Problem**: You need to load configuration from JSON or YAML files.

**Solution**:

```python
import json
import yaml
from pathlib import Path
from typing import Dict, Any

# Load JSON config
def load_json_config(filepath: Path) -> Dict[str, Any]:
    return json.loads(filepath.read_text())

config = load_json_config(Path('config.json'))

# Load YAML config
def load_yaml_config(filepath: Path) -> Dict[str, Any]:
    return yaml.safe_load(filepath.read_text())

config = load_yaml_config(Path('config.yaml'))

# Configuration class
class Config:
    def __init__(self, config_path: Path):
        self.config = self._load_config(config_path)

    def _load_config(self, path: Path) -> Dict[str, Any]:
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

# Usage
config = Config(Path('config.yaml'))
database_url = config.get('database_url', 'sqlite:///default.db')
api_key = config['api_key']

# Layered configuration (environment overrides file)
def load_layered_config(config_file: Path) -> Dict[str, Any]:
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

### Recipe 25: Basic pytest Patterns

**Problem**: You need to write effective unit tests.

**Solution**:

```python
# test_calculator.py
import pytest

def add(a: int, b: int) -> int:
    return a + b

def divide(a: int, b: int) -> float:
    if b == 0:
        raise ValueError("Cannot divide by zero")
    return a / b

# Basic test
def test_add():
    assert add(2, 3) == 5
    assert add(-1, 1) == 0
    assert add(0, 0) == 0

# Test with multiple assertions
def test_add_multiple():
    assert add(1, 1) == 2
    assert add(2, 2) == 4
    assert add(3, 3) == 6

# Test exceptions
def test_divide_by_zero():
    with pytest.raises(ValueError, match="Cannot divide by zero"):
        divide(10, 0)

# Parametrized tests (multiple inputs)
@pytest.mark.parametrize("a,b,expected", [
    (2, 3, 5),
    (-1, 1, 0),
    (0, 0, 0),
    (100, -50, 50),
])
def test_add_parametrized(a, b, expected):
    assert add(a, b) == expected

# Fixtures for setup/teardown
@pytest.fixture
def sample_data():
    """Provide sample data for tests"""
    return [1, 2, 3, 4, 5]

def test_with_fixture(sample_data):
    assert len(sample_data) == 5
    assert sum(sample_data) == 15

# Fixture with cleanup
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

# Class-based tests
class TestCalculator:
    def test_add(self):
        assert add(1, 1) == 2

    def test_divide(self):
        assert divide(10, 2) == 5.0

    def test_divide_by_zero(self):
        with pytest.raises(ValueError):
            divide(10, 0)

# Markers for test organization
@pytest.mark.slow
def test_slow_operation():
    import time
    time.sleep(1)
    assert True

@pytest.mark.integration
def test_database_connection():
    # Integration test code
    pass

# Run specific markers: pytest -m slow
```

**When to use**: For all unit testing (pytest is preferred over unittest).

---

### Recipe 26: Advanced pytest Patterns

**Problem**: You need mocking, async tests, or complex fixtures.

**Solution**:

```python
import pytest
from unittest.mock import Mock, patch, MagicMock
import asyncio

# Mocking
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

# Patching
@patch('requests.get')
def test_with_patch(mock_get):
    mock_get.return_value.status_code = 200
    mock_get.return_value.json.return_value = {'status': 'ok'}

    import requests
    response = requests.get('http://api.example.com')
    assert response.status_code == 200

# Async tests
@pytest.mark.asyncio
async def test_async_function():
    async def async_add(a, b):
        await asyncio.sleep(0.1)
        return a + b

    result = await async_add(2, 3)
    assert result == 5

# Complex fixture with dependencies
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

# Fixture scope (function, class, module, session)
@pytest.fixture(scope="module")
def expensive_setup():
    """Setup once per module"""
    print("Expensive setup")
    yield "resource"
    print("Expensive teardown")

# Parametrized fixtures
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

### Recipe 27: argparse for CLI

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

# Usage examples:
# python script.py input.txt
# python script.py input.txt -o result.txt -v
# python script.py input.txt --format csv --number 20
```

**When to use**: For building command-line tools with simple argument parsing.

---

### Recipe 28: Click for Advanced CLI

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

# Usage:
# python script.py process input.txt -v -f csv
# python script.py generate -n 20
# python script.py merge file1.txt file2.txt file3.txt
# python script.py --help
```

**When to use**: For complex CLIs with subcommands, progress bars, or interactive prompts.

---

## 🔷 Logging

Proper logging is essential for debugging and monitoring production applications.

### Recipe 29: Logging Setup

**Problem**: You need structured logging for your application.

**Solution**:

```python
import logging
from pathlib import Path

# Basic logging setup
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%Y-%m-%d %H:%M:%S'
)

logger = logging.getLogger(__name__)

# Log messages at different levels
logger.debug("Debug message")      # Not shown (level is INFO)
logger.info("Info message")        # Shown
logger.warning("Warning message")  # Shown
logger.error("Error message")      # Shown
logger.critical("Critical message") # Shown

# Log with exception info
try:
    result = 10 / 0
except ZeroDivisionError:
    logger.exception("Division error occurred")  # Includes traceback

# Advanced logging configuration
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

# Usage
setup_logging(Path('app.log'), level='DEBUG')
logger = logging.getLogger(__name__)
logger.info("Application started")

# Structured logging with extra fields
logger.info("User login", extra={
    'user_id': 123,
    'ip_address': '192.168.1.1'
})

# Module-specific loggers
# module1.py
logger = logging.getLogger(__name__)  # __name__ is 'module1'

# Different log levels per module
logging.getLogger('module1').setLevel(logging.DEBUG)
logging.getLogger('module2').setLevel(logging.WARNING)
```

**When to use**: Always! Prefer logging over print() for anything beyond debugging.

---

## 🔷 Common Patterns and Best Practices

### Recipe 30: Enum for Constants

**Problem**: You need type-safe constants or enumerations.

**Solution**:

```python
from enum import Enum, auto

# Basic enum
class Status(Enum):
    PENDING = 'pending'
    PROCESSING = 'processing'
    COMPLETED = 'completed'
    FAILED = 'failed'

# Usage
current_status = Status.PENDING
print(current_status)  # Status.PENDING
print(current_status.value)  # 'pending'

# Compare enums
if current_status == Status.PENDING:
    print("Waiting to start")

# Auto-assign values
class Color(Enum):
    RED = auto()     # 1
    GREEN = auto()   # 2
    BLUE = auto()    # 3

# Iterate over enum
for color in Color:
    print(f"{color.name}: {color.value}")

# Access by name or value
color = Color['RED']
color = Color(1)

# IntEnum for integer enums
from enum import IntEnum

class Priority(IntEnum):
    LOW = 1
    MEDIUM = 2
    HIGH = 3
    CRITICAL = 4

# Can compare with integers
if Priority.HIGH >= 3:
    print("High priority or above")

# Flag for bitwise operations
from enum import Flag

class Permission(Flag):
    READ = auto()
    WRITE = auto()
    EXECUTE = auto()

# Combine flags
user_permissions = Permission.READ | Permission.WRITE
if Permission.READ in user_permissions:
    print("Can read")
```

**When to use**: For any set of related constants or states.

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
