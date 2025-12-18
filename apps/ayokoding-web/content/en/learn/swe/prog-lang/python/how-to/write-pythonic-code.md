---
title: "How to Write Pythonic Code"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 604
description: "Embrace Python idioms for cleaner, more maintainable code"
tags: ["python", "pythonic", "idioms", "zen-of-python"]
categories: ["learn"]
---

## Problem

Python supports multiple programming styles, but code that follows Python's idioms ("Pythonic" code) is more readable and maintainable. Developers from other languages often import non-Pythonic patterns that work but miss Python's expressive power.

This guide shows how to write Pythonic code that embraces Python's philosophy and idioms.

## The Zen of Python

```python
import this
```

Key principles from PEP 20:

- Beautiful is better than ugly
- Explicit is better than implicit
- Simple is better than complex
- Readability counts
- There should be one obvious way to do it

## Pythonic Idioms

### EAFP Over LBYL

Easier to Ask for Forgiveness than Permission vs Look Before You Leap.

```python
# ❌ LBYL - Look Before You Leap
def get_user_email(users, user_id):
    if user_id in users:
        user = users[user_id]
        if hasattr(user, 'email'):
            if user.email is not None:
                return user.email
    return None

# ✅ EAFP - Easier to Ask for Forgiveness than Permission
def get_user_email(users, user_id):
    try:
        return users[user_id].email
    except (KeyError, AttributeError):
        return None
```

### Use enumerate, not range(len())

```python
names = ["Alice", "Bob", "Charlie"]

# ❌ C-style indexing
for i in range(len(names)):
    print(f"{i}: {names[i]}")

# ✅ Pythonic enumerate
for i, name in enumerate(names):
    print(f"{i}: {name}")

# ✅ Start from different index
for i, name in enumerate(names, start=1):
    print(f"{i}: {name}")
```

### Use zip for Parallel Iteration

```python
names = ["Alice", "Bob"]
ages = [30, 25]

# ❌ Manual indexing
for i in range(len(names)):
    print(f"{names[i]} is {ages[i]} years old")

# ✅ Pythonic zip
for name, age in zip(names, ages):
    print(f"{name} is {age} years old")

# ✅ Zip multiple iterables
ids = [1, 2]
for id, name, age in zip(ids, names, ages):
    print(f"{id}: {name} ({age})")
```

### Unpacking and Multiple Assignment

```python
# ✅ Swap variables
a, b = b, a  # No temp variable needed

# ✅ Multiple return values
def get_user():
    return "Alice", 30, "alice@example.com"

name, age, email = get_user()

# ✅ Ignore values with _
name, _, email = get_user()  # Ignore age

# ✅ Extended unpacking (Python 3+)
first, *middle, last = [1, 2, 3, 4, 5]
print(first)   # 1
print(middle)  # [2, 3, 4]
print(last)    # 5

# ✅ Dict unpacking
defaults = {"color": "blue", "size": 10}
custom = {"size": 20}
merged = {**defaults, **custom}  # {"color": "blue", "size": 20}
```

### Comprehensions for Transformation

```python
numbers = [1, 2, 3, 4, 5]

# ❌ Verbose loop
squares = []
for num in numbers:
    squares.append(num ** 2)

# ✅ List comprehension
squares = [num ** 2 for num in numbers]

# ✅ With filtering
even_squares = [num ** 2 for num in numbers if num % 2 == 0]

# ✅ Dict comprehension
word_lengths = {word: len(word) for word in ["hello", "world"]}

# ✅ Set comprehension
unique_squares = {num ** 2 for num in numbers}

# ✅ Generator expression (memory efficient)
sum_of_squares = sum(num ** 2 for num in numbers)
```

### Context Managers

```python
# ❌ Manual resource management
f = open("file.txt")
try:
    data = f.read()
finally:
    f.close()

# ✅ Context manager
with open("file.txt") as f:
    data = f.read()

# ✅ Multiple context managers
with open("input.txt") as infile, open("output.txt", "w") as outfile:
    outfile.write(infile.read())

# ✅ Custom context manager
from contextlib import contextmanager

@contextmanager
def timer(name):
    import time
    start = time.time()
    try:
        yield
    finally:
        print(f"{name} took {time.time() - start:.2f}s")

with timer("Database query"):
    results = database.query()
```

### String Formatting with F-Strings

```python
name = "Alice"
age = 30

# ❌ Old-style % formatting
msg = "Hello %s, you are %d years old" % (name, age)

# ❌ str.format() - verbose
msg = "Hello {}, you are {} years old".format(name, age)

# ✅ F-strings - clean and fast
msg = f"Hello {name}, you are {age} years old"

# ✅ Expressions
msg = f"In 5 years: {age + 5}"

# ✅ Formatting
price = 19.99
msg = f"Price: ${price:.2f}"

# ✅ Debugging (Python 3.8+)
print(f"{name=}, {age=}")  # name='Alice', age=30
```

### Truthiness

```python
# ✅ Use truthiness instead of explicit comparisons
users = []

# ❌ Explicit comparison
if len(users) == 0:
    print("No users")

# ✅ Pythonic truthiness
if not users:
    print("No users")

# Truthy: non-empty collections, non-zero numbers, non-empty strings
# Falsy: empty collections, 0, None, False, empty strings

# ✅ Examples
if users:  # Non-empty list
    process(users)

if user_name:  # Non-empty string
    greet(user_name)

if count:  # Non-zero number
    display(count)

# ⚠️ Be careful with 0 and False
value = get_value()
if value:  # Wrong if value could be 0 or False legitimately
    process(value)

if value is not None:  # Better when 0 or False are valid
    process(value)
```

### Default Values with get()

```python
config = {"host": "localhost", "port": 8080}

# ❌ Manual default handling
if "timeout" in config:
    timeout = config["timeout"]
else:
    timeout = 30

# ✅ Use get() with default
timeout = config.get("timeout", 30)

# ✅ setdefault for initialization
cache = {}
if "results" not in cache:
    cache["results"] = []
cache["results"].append(item)

# Better
cache.setdefault("results", []).append(item)
```

### Chain Comparisons

```python
x = 5

# ❌ Multiple comparisons
if x > 0 and x < 10:
    print("Single digit")

# ✅ Chained comparisons
if 0 < x < 10:
    print("Single digit")

# ✅ Works with any comparison
if a < b <= c < d:
    do_something()
```

### Walrus Operator (Python 3.8+)

```python
# ✅ Assignment expressions
# Read and check in one line
if (line := file.readline()):
    process(line)

# ✅ List comprehension with intermediate value
data = [1, 2, 3, 4, 5]
squared_evens = [y for x in data if (y := x ** 2) % 2 == 0]

# ✅ While loops
while (chunk := file.read(1024)):
    process(chunk)
```

### Decorators for Reusable Logic

```python
import functools
import time

# ✅ Timing decorator
def timer(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        print(f"{func.__name__} took {time.time() - start:.2f}s")
        return result
    return wrapper

@timer
def slow_function():
    time.sleep(2)

# ✅ Caching decorator
from functools import lru_cache

@lru_cache(maxsize=128)
def fibonacci(n):
    if n < 2:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

# ✅ Property decorator
class Rectangle:
    def __init__(self, width, height):
        self._width = width
        self._height = height

    @property
    def area(self):
        return self._width * self._height

rect = Rectangle(10, 5)
print(rect.area)  # Computed property, attribute syntax
```

### Path Manipulation with pathlib

```python
import os

# ❌ String manipulation for paths
file_path = os.path.join("data", "users", "alice.txt")
directory = os.path.dirname(file_path)
filename = os.path.basename(file_path)

# ✅ pathlib - object-oriented paths
from pathlib import Path

file_path = Path("data") / "users" / "alice.txt"
directory = file_path.parent
filename = file_path.name

# ✅ pathlib methods
file_path.exists()
file_path.is_file()
file_path.is_dir()
file_path.read_text()
file_path.write_text("content")

# ✅ Glob patterns
for txt_file in Path("data").glob("**/*.txt"):
    process(txt_file)
```

### Itertools for Efficient Iteration

```python
from itertools import chain, islice, groupby, count, cycle

# ✅ Chain iterables
list1 = [1, 2, 3]
list2 = [4, 5, 6]
combined = list(chain(list1, list2))  # [1, 2, 3, 4, 5, 6]

# ✅ Slice iterator (memory efficient)
# Read first 100 lines without loading all
with open("huge.txt") as f:
    first_100 = list(islice(f, 100))

# ✅ Infinite counter
for i in count(start=1):
    if i > 10:
        break
    print(i)

# ✅ Cycle through values
from itertools import cycle
colors = cycle(["red", "green", "blue"])
for _ in range(10):
    print(next(colors))  # Repeats colors infinitely
```

## Anti-Patterns to Avoid

### Don't Use len() for Empty Check

```python
# ❌ Explicit length check
if len(users) == 0:
    return

# ✅ Use truthiness
if not users:
    return
```

### Don't Build Strings in Loops

```python
# ❌ String concatenation in loop
result = ""
for item in items:
    result += str(item) + ","

# ✅ Join list
result = ",".join(str(item) for item in items)
```

### Don't Check Type with type()

```python
# ❌ Exact type check
if type(value) == list:
    process_list(value)

# ✅ Use isinstance (handles subclasses)
if isinstance(value, list):
    process_list(value)

# ✅ Duck typing (even better)
try:
    for item in value:  # Works with any iterable
        process(item)
except TypeError:
    process_single(value)
```

### Don't Use range(len())

```python
items = ["a", "b", "c"]

# ❌ C-style iteration
for i in range(len(items)):
    print(items[i])

# ✅ Direct iteration
for item in items:
    print(item)

# ✅ With index
for i, item in enumerate(items):
    print(f"{i}: {item}")
```

## Summary

Pythonic code embraces Python's idioms rather than importing patterns from other languages. The EAFP principle (Easier to Ask for Forgiveness than Permission) focuses code on the happy path, handling exceptions when they occur instead of checking conditions upfront. This approach produces cleaner code that's often faster and more resistant to race conditions than LBYL (Look Before You Leap) defensive programming.

Built-in functions like enumerate, zip, and range provide expressive iteration without manual index management. Enumerate yields both index and value, zip combines multiple iterables, and unpacking enables elegant multiple assignment and variable swapping. These idioms replace verbose C-style loops with clear, concise expressions.

Comprehensions transform collections more clearly than equivalent loops. List comprehensions, dict comprehensions, and set comprehensions express filtering and transformation in single expressions that reveal intent immediately. Generator expressions provide the same syntax with memory efficiency for large datasets or infinite sequences.

F-strings make string formatting readable and fast. They support expressions, formatting specifications, and debugging output through the = operator. Context managers guarantee resource cleanup through the with statement, replacing error-prone try/finally blocks. Custom context managers through @contextmanager decorator enable reusable resource management patterns.

Truthiness enables concise condition checking - empty collections, None, 0, and False are falsy, everything else is truthy. The get() method provides defaults for dictionaries without explicit existence checks. Chained comparisons like 0 < x < 10 read naturally and execute efficiently. The walrus operator (:=) assigns and uses values in single expressions.

Decorators wrap functions to add cross-cutting concerns like timing, caching, or validation. The @property decorator provides computed attributes with clean syntax. Pathlib replaces string manipulation for filesystem operations with object-oriented paths. Itertools provides memory-efficient tools for combining and transforming iterables.

Avoid anti-patterns like using len() for empty checks (use truthiness), building strings in loops (use join), checking exact types with type() (use isinstance or duck typing), and iterating with range(len()) (use enumerate or direct iteration). These patterns work but miss Python's expressive power.

Writing Pythonic code means leveraging Python's idioms to produce clear, concise code that communicates intent. The result is more maintainable and often faster than verbose alternatives that fight the language's design.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [Common Python Anti-Patterns](/en/learn/swe/prog-lang/python/explanation/anti-patterns)
- [How to Use Collections Effectively](/en/learn/swe/prog-lang/python/how-to/use-collections-effectively)
- [How to Avoid Common Pitfalls](/en/learn/swe/prog-lang/python/how-to/avoid-common-pitfalls)
