---
title: "Common Python Anti-Patterns"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 704
description: "Common Python anti-patterns that seem reasonable but create problems, with explanations and solutions"
tags: ["python", "anti-patterns", "pitfalls", "code-quality"]
categories: ["learn"]
---

## Overview

Anti-patterns are solutions that appear reasonable but create more problems than they solve. Python's flexibility and dynamic nature make certain anti-patterns particularly tempting, especially for developers coming from statically-typed languages. Recognizing these patterns helps you avoid common traps.

## Data Structure Anti-Patterns

### Mutable Default Arguments

Python evaluates default arguments once when defining the function, not each time you call it. Mutable defaults persist across calls, creating shared state.

**Why it's problematic:**

- Default list/dict shared across all calls
- Creates hidden coupling between calls
- Violates principle of least surprise
- Difficult to debug

**Example:**

```python
# ❌ Mutable default - shared across calls
def add_item(item, items=[]):
    items.append(item)
    return items

print(add_item(1))  # [1]
print(add_item(2))  # [1, 2] - Unexpected!
print(add_item(3))  # [1, 2, 3] - List persists

# ✅ Use None as sentinel
def add_item(item, items=None):
    if items is None:
        items = []
    items.append(item)
    return items

print(add_item(1))  # [1]
print(add_item(2))  # [2] - Fresh list each time
print(add_item(3))  # [3]

# ✅ Or make it explicit
def add_item(item, items=None):
    items = items if items is not None else []
    items.append(item)
    return items
```

**When mutable defaults are okay:**

```python
# ✅ Intentional cache or state (document it!)
def create_logger(name, _cache={}):
    """Creates or returns cached logger.

    Note: _cache persists across calls by design.
    """
    if name not in _cache:
        _cache[name] = setup_logger(name)
    return _cache[name]
```

### Using Lists for Frequent Lookups

Lists require O(n) linear search for membership checks. For frequent lookups, this becomes a performance bottleneck.

**Why it's problematic:**

- O(n) for membership checks
- Scales poorly with data size
- Wastes CPU on repeated searches
- Set provides O(1) lookups

**Example:**

```python
# ❌ List for frequent membership checks
ADMIN_USERS = ['alice', 'bob', 'charlie', 'david']

def is_admin(username):
    return username in ADMIN_USERS  # O(n) search

# With 1000 checks on 1000-item list = 1,000,000 comparisons

# ✅ Set for O(1) membership checks
ADMIN_USERS = {'alice', 'bob', 'charlie', 'david'}

def is_admin(username):
    return username in ADMIN_USERS  # O(1) lookup

# ✅ Frozen set if truly constant
ADMIN_USERS = frozenset(['alice', 'bob', 'charlie', 'david'])
```

**Performance comparison:**

```python
import timeit

# List with 1000 items
users_list = list(range(1000))
print(timeit.timeit(lambda: 999 in users_list, number=100000))  # ~0.5s

# Set with 1000 items
users_set = set(range(1000))
print(timeit.timeit(lambda: 999 in users_set, number=100000))   # ~0.005s
```

### Not Using Comprehensions

Writing loops to build lists when comprehensions would be clearer and faster.

**Why it's problematic:**

- More verbose than necessary
- Slower than comprehensions
- Less clear intent
- More opportunities for bugs

**Example:**

```python
# ❌ Verbose loop to build list
squares = []
for x in range(10):
    squares.append(x ** 2)

# ❌ Verbose loop with condition
evens = []
for x in range(20):
    if x % 2 == 0:
        evens.append(x)

# ✅ List comprehension - clear and fast
squares = [x ** 2 for x in range(10)]

# ✅ Filtering with comprehension
evens = [x for x in range(20) if x % 2 == 0]

# ✅ Transforming with comprehension
words = ['hello', 'world', 'python']
upper_words = [w.upper() for w in words]

# ✅ Dict comprehension
word_lengths = {word: len(word) for word in words}

# ✅ Generator for memory efficiency
sum_of_squares = sum(x ** 2 for x in range(1_000_000))
```

**When loops are better:**

```python
# ✅ Complex logic doesn't fit comprehension cleanly
results = []
for item in items:
    try:
        processed = complex_processing(item)
        if validate(processed):
            results.append(transform(processed))
    except ProcessingError:
        log_error(item)
        continue
```

## Exception Handling Anti-Patterns

### Bare Except Clauses

Catching all exceptions without specifying which ones hides bugs and makes debugging impossible.

**Why it's problematic:**

- Catches unexpected exceptions (KeyboardInterrupt, SystemExit)
- Makes debugging nearly impossible
- Hides programming errors
- Violates "errors should never pass silently"

**Example:**

```python
# ❌ Bare except - catches everything
def load_config():
    try:
        with open('config.json') as f:
            return json.load(f)
    except:  # Catches KeyboardInterrupt, SystemExit, etc.
        return {}  # Silently fails

# ✅ Catch specific exceptions
def load_config():
    try:
        with open('config.json') as f:
            return json.load(f)
    except FileNotFoundError:
        return {}  # Expected case
    except json.JSONDecodeError as e:
        logger.error(f"Invalid JSON in config: {e}")
        raise  # Re-raise to surface the error

# ✅ If you must catch all, be explicit
def load_config():
    try:
        with open('config.json') as f:
            return json.load(f)
    except Exception as e:  # Doesn't catch system exceptions
        logger.error(f"Failed to load config: {e}")
        return {}
```

### Swallowing Exceptions

Catching exceptions but doing nothing with them hides problems and makes debugging difficult.

**Why it's problematic:**

- Lost error context
- Silent failures corrupt state
- Debugging becomes guesswork
- Violates fail-fast principle

**Example:**

```python
# ❌ Swallowing exceptions
def fetch_user(user_id):
    try:
        return database.query(f"SELECT * FROM users WHERE id = {user_id}")
    except:
        pass  # Silent failure - caller gets None

# Later code assumes user exists - crashes mysteriously
user = fetch_user(123)
print(user.name)  # AttributeError: 'NoneType' has no attribute 'name'

# ✅ Let exceptions propagate
def fetch_user(user_id):
    return database.query(f"SELECT * FROM users WHERE id = {user_id}")
    # Caller handles exceptions or lets them propagate

# ✅ Or handle explicitly with context
def fetch_user(user_id):
    try:
        return database.query(f"SELECT * FROM users WHERE id = {user_id}")
    except DatabaseError as e:
        logger.error(f"Failed to fetch user {user_id}: {e}")
        raise  # Re-raise with context logged

# ✅ Return explicit failure
from typing import Optional

def fetch_user(user_id) -> Optional[User]:
    try:
        return database.query(f"SELECT * FROM users WHERE id = {user_id}")
    except UserNotFoundError:
        return None  # Expected case, not an error
```

### Using Exceptions for Flow Control

Exceptions should represent exceptional conditions, not normal program flow.

**Why it's problematic:**

- Exceptions are slow (stack trace creation)
- Unclear what's error vs normal flow
- Violates principle of least surprise
- Difficult to optimize

**Example:**

```python
# ❌ Exception for control flow
def get_user_age(users, user_id):
    try:
        return users[user_id].age
    except KeyError:
        return None  # Normal case, not exceptional

# Caller forced to use try/except for normal flow
try:
    age = get_user_age(users, 123)
except KeyError:
    age = None

# ✅ Return Optional for normal cases
from typing import Optional

def get_user_age(users, user_id) -> Optional[int]:
    user = users.get(user_id)
    return user.age if user else None

# Clean handling
age = get_user_age(users, 123)
if age is not None:
    print(f"Age: {age}")

# ✅ Or use EAFP when exceptions are truly rare
def get_user_age(users, user_id):
    try:
        return users[user_id].age
    except KeyError:
        raise ValueError(f"User {user_id} not found")  # Exceptional condition
```

## Resource Management Anti-Patterns

### Not Using Context Managers

Manually managing resources with try/finally when context managers provide automatic cleanup.

**Why it's problematic:**

- Easy to forget cleanup
- Verbose and error-prone
- Suppressed exceptions in finally
- Context managers are cleaner and safer

**Example:**

```python
# ❌ Manual resource management
def process_file(filename):
    f = open(filename)
    try:
        data = f.read()
        return process(data)
    finally:
        f.close()  # Easy to forget

# ❌ Multiple resources - messy
def copy_file(src, dest):
    f1 = open(src, 'rb')
    try:
        f2 = open(dest, 'wb')
        try:
            f2.write(f1.read())
        finally:
            f2.close()
    finally:
        f1.close()

# ✅ Context manager handles cleanup
def process_file(filename):
    with open(filename) as f:
        data = f.read()
        return process(data)

# ✅ Multiple resources cleanly
def copy_file(src, dest):
    with open(src, 'rb') as source, open(dest, 'wb') as target:
        target.write(source.read())

# ✅ Custom context manager
from contextlib import contextmanager

@contextmanager
def database_transaction(connection):
    try:
        yield connection
        connection.commit()
    except Exception:
        connection.rollback()
        raise

with database_transaction(conn) as db:
    db.execute("INSERT INTO users ...")
```

### Manually Closing Resources

Forgetting to close files, connections, or other resources leads to leaks.

**Why it's problematic:**

- Resource leaks (file handles, connections)
- Unexpected behavior (buffering issues)
- System limits (max open files)
- Memory leaks for long-running processes

**Example:**

```python
# ❌ Forgetting to close
def read_config():
    f = open('config.json')
    return json.load(f)  # File never closed!

# In long-running process
for i in range(10000):
    config = read_config()  # Eventually runs out of file handles

# ✅ Context manager guarantees closure
def read_config():
    with open('config.json') as f:
        return json.load(f)

# ✅ pathlib handles resources
from pathlib import Path

def read_config():
    return json.loads(Path('config.json').read_text())
```

## Import Anti-Patterns

### Circular Imports

Modules that import each other create initialization problems and tight coupling.

**Why it's problematic:**

- Import errors at runtime
- Order-dependent behavior
- Tight coupling between modules
- Indicates poor separation of concerns

**Example:**

```python
# ❌ Circular import - module_a.py
from module_b import function_b

def function_a():
    return function_b()

# ❌ Circular import - module_b.py
from module_a import function_a  # ImportError!

def function_b():
    return function_a()

# ✅ Extract common code to third module
# shared.py
def shared_function():
    return "shared"

# module_a.py
from shared import shared_function

def function_a():
    return shared_function()

# module_b.py
from shared import shared_function

def function_b():
    return shared_function()

# ✅ Or use dependency injection
# module_a.py
def function_a(dependency):
    return dependency()

# module_b.py
def function_b():
    return "result"

# Usage
from module_a import function_a
from module_b import function_b

result = function_a(function_b)  # Inject dependency
```

### Wildcard Imports

Using `from module import *` pollutes namespace and makes code unclear.

**Why it's problematic:**

- Unclear where names come from
- Name collisions
- Makes refactoring difficult
- Breaks static analysis tools

**Example:**

```python
# ❌ Wildcard import
from math import *
from statistics import *

result = mean([1, 2, 3])  # Which mean? math.mean or statistics.mean?

# ✅ Explicit imports
from statistics import mean, median
from math import sqrt, pi

result = mean([1, 2, 3])  # Clear origin

# ✅ Import module for many items
import math

result = math.sqrt(math.pi)  # Clear namespace

# ✅ Alias if needed
import statistics as stats

result = stats.mean([1, 2, 3])
```

## String Anti-Patterns

### String Concatenation in Loops

Building strings with `+=` in loops creates many intermediate strings.

**Why it's problematic:**

- O(n²) time complexity (strings are immutable)
- Creates garbage for each concatenation
- Slow for large strings
- List join is O(n)

**Example:**

```python
# ❌ String concatenation in loop - O(n²)
result = ""
for i in range(1000):
    result += str(i) + ","  # Creates 1000 intermediate strings

# ✅ List join - O(n)
parts = []
for i in range(1000):
    parts.append(str(i))
result = ",".join(parts)

# ✅ Even better - generator expression
result = ",".join(str(i) for i in range(1000))

# ✅ For complex building, use StringIO
from io import StringIO

output = StringIO()
for i in range(1000):
    output.write(str(i))
    output.write(",")
result = output.getvalue()
```

**Performance comparison:**

```python
import timeit

# Concatenation
print(timeit.timeit(
    '".".join(str(i) for i in range(1000))',
    number=1000
))  # ~0.15s

# Join
print(timeit.timeit(
    'result = ""; [result := result + str(i) for i in range(1000)]',
    number=1000
))  # ~0.50s
```

### Not Using F-Strings

Using old-style string formatting when f-strings are clearer and faster.

**Why it's problematic:**

- Less readable than f-strings
- Slower than f-strings
- More error-prone
- Harder to maintain

**Example:**

```python
name = "Alice"
age = 30

# ❌ % formatting - error-prone
msg = "Hello %s, you are %d years old" % (name, age)

# ❌ .format() - verbose
msg = "Hello {}, you are {} years old".format(name, age)

# ❌ Manual concatenation - ugly
msg = "Hello " + name + ", you are " + str(age) + " years old"

# ✅ F-strings - clear, fast, safe
msg = f"Hello {name}, you are {age} years old"

# ✅ With expressions
msg = f"In 5 years: {age + 5}"

# ✅ With formatting
price = 19.99
msg = f"Price: ${price:.2f}"

# ✅ Debugging
print(f"{name=}, {age=}")  # name='Alice', age=30
```

## Class Design Anti-Patterns

### God Classes

Classes that do too much violate Single Responsibility Principle and become unmaintainable.

**Why it's problematic:**

- Difficult to understand
- Difficult to test
- High coupling
- Changes ripple through codebase

**Example:**

```python
# ❌ God class - too many responsibilities
class UserManager:
    def create_user(self, data): ...
    def validate_email(self, email): ...
    def hash_password(self, password): ...
    def send_welcome_email(self, user): ...
    def save_to_database(self, user): ...
    def generate_report(self, users): ...
    def export_to_csv(self, users): ...
    def import_from_json(self, data): ...
    def authenticate(self, email, password): ...
    # ... 20 more methods

# ✅ Split by responsibility
class UserValidator:
    def validate_email(self, email): ...
    def validate_password(self, password): ...

class PasswordHasher:
    def hash_password(self, password): ...
    def verify_password(self, password, hash): ...

class UserRepository:
    def save(self, user): ...
    def find_by_email(self, email): ...

class EmailService:
    def send_welcome_email(self, user): ...

class UserService:
    def __init__(self, validator, hasher, repository, email_service):
        self.validator = validator
        self.hasher = hasher
        self.repository = repository
        self.email_service = email_service

    def create_user(self, data):
        self.validator.validate_email(data['email'])
        hashed = self.hasher.hash_password(data['password'])
        user = User(data['email'], hashed)
        self.repository.save(user)
        self.email_service.send_welcome_email(user)
        return user
```

### Overusing Classes

Creating classes when functions or simpler structures would suffice (Java-itis).

**Why it's problematic:**

- Unnecessary complexity
- More boilerplate to maintain
- Obscures simple logic
- Python is multi-paradigm, not just OOP

**Example:**

```python
# ❌ Unnecessary class for simple utility
class StringUtils:
    @staticmethod
    def reverse(s):
        return s[::-1]

    @staticmethod
    def capitalize_words(s):
        return s.title()

result = StringUtils.reverse("hello")

# ✅ Simple functions
def reverse_string(s):
    return s[::-1]

def capitalize_words(s):
    return s.title()

result = reverse_string("hello")

# ❌ Class for single operation
class DiscountCalculator:
    def __init__(self, rate):
        self.rate = rate

    def calculate(self, amount):
        return amount * (1 - self.rate)

calc = DiscountCalculator(0.1)
result = calc.calculate(100)

# ✅ Simple function
def calculate_discount(amount, rate):
    return amount * (1 - rate)

result = calculate_discount(100, 0.1)
```

**When classes make sense:**

- State management across operations
- Encapsulation with data + behavior
- Inheritance or protocols needed
- Context managers or iterators

## Performance Anti-Patterns

### Premature Optimization

Optimizing code before profiling shows it's a bottleneck.

**Why it's problematic:**

- Makes code complex prematurely
- Optimizes wrong parts
- Wastes development time
- Reduces readability

**Example:**

```python
# ❌ Premature optimization - complex and unclear
def calculate_total(items):
    # "Optimized" with caching and pre-allocation
    _cache = {}
    result = [0] * len(items)
    for i, item in enumerate(items):
        key = (item['price'], item['qty'])
        if key not in _cache:
            _cache[key] = item['price'] * item['qty']
        result[i] = _cache[key]
    return sum(result)

# ✅ Start simple and readable
def calculate_total(items):
    return sum(item['price'] * item['qty'] for item in items)

# ✅ Optimize ONLY if profiling shows it's slow
# Profile first:
# import cProfile
# cProfile.run('calculate_total(items)')
#
# If it's a bottleneck, then optimize with measurements
```

**Optimization workflow:**

1. Write simple, correct code
2. Profile to find actual bottlenecks
3. Optimize bottlenecks with benchmarks
4. Keep readable code in non-critical paths

### Not Using Built-in Functions

Reimplementing functionality that exists in standard library.

**Why it's problematic:**

- Slower than optimized built-ins
- More code to maintain
- Likely has bugs
- Misses edge cases

**Example:**

```python
# ❌ Reimplementing sum
def calculate_sum(numbers):
    total = 0
    for num in numbers:
        total += num
    return total

# ✅ Use built-in
total = sum(numbers)

# ❌ Reimplementing max
def find_maximum(numbers):
    max_val = numbers[0]
    for num in numbers[1:]:
        if num > max_val:
            max_val = num
    return max_val

# ✅ Use built-in
max_val = max(numbers)

# ❌ Reimplementing filtering
def get_evens(numbers):
    result = []
    for num in numbers:
        if num % 2 == 0:
            result.append(num)
    return result

# ✅ Use filter or comprehension
evens = [num for num in numbers if num % 2 == 0]
evens = list(filter(lambda x: x % 2 == 0, numbers))
```

## Summary

Python's flexibility makes certain anti-patterns particularly tempting, but recognizing them helps you write more maintainable code. Mutable default arguments create hidden state that persists across function calls - use None as a sentinel instead. Using lists for frequent lookups wastes CPU on linear searches when sets provide constant-time membership checks. Comprehensions aren't just shorter than equivalent loops - they express intent more clearly and run faster.

Exception handling requires care to avoid masking problems. Bare except clauses catch system exceptions like KeyboardInterrupt that should propagate. Swallowing exceptions silently hides failures that corrupt state and make debugging impossible. Using exceptions for control flow confuses error conditions with normal program logic - return Optional types for expected cases instead.

Context managers automatically clean up resources even when exceptions occur. Manual try/finally blocks require discipline to close files, connections, and locks reliably. Circular imports indicate tight coupling and poor separation of concerns - extract shared code to a third module or use dependency injection. Wildcard imports pollute namespaces and make code impossible to trace - import explicitly or import the module itself.

String concatenation in loops creates O(n²) behavior because strings are immutable. Building strings through intermediate list items and joining them once provides O(n) performance. F-strings make string interpolation readable and fast compared to % formatting or .format() - use them as your default choice.

God classes that handle too many responsibilities become unmaintainable tangles of coupled code. Split classes by responsibility to enable focused testing and independent evolution. Not every operation needs a class - Python supports multiple paradigms, and simple functions often express simple operations more clearly than classes with static methods.

Premature optimization wastes effort on code that isn't actually slow. Write simple, correct code first, profile to find real bottlenecks, then optimize with benchmarks to validate improvements. The standard library provides optimized implementations of common operations - prefer built-in functions over custom implementations that are slower and more likely to have bugs.

These anti-patterns share a common theme: they trade clarity or correctness for perceived benefits that don't materialize. Focus on writing clear, correct code that follows Python's idioms. When optimization matters, let profiling guide you to actual bottlenecks worth improving.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [How to Avoid Common Pitfalls](/en/learn/swe/prog-lang/python/how-to/avoid-common-pitfalls)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/python/how-to/handle-errors-effectively)
- [How to Write Pythonic Code](/en/learn/swe/prog-lang/python/how-to/write-pythonic-code)
