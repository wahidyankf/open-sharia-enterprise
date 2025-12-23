---
title: "Cheat Sheet"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000030
description: Quick reference for Python syntax, operators, and common patterns
---

**Quick reference guide** for essential Python syntax and patterns. Copy-paste ready snippets for daily development.

## Variables and Types

### Variable Assignment

```python
# Dynamic typing (no declaration needed)
message = "Hello, World!"
count = 42
price = 19.99

# Multiple assignment
x, y, z = 1, 2, 3
a = b = c = 0

# Swapping
x, y = y, x

# Constants (convention: uppercase, not enforced)
MAX_SIZE = 100
PI = 3.14159
```

### Basic Types

```python
# Numbers
integer = 42
floating = 3.14
complex_num = 3 + 4j
scientific = 1.5e-3  # 0.0015

# Strings
single = 'Hello'
double = "World"
triple = '''Multi
line
string'''

# Boolean
flag = True
is_valid = False

# None (null equivalent)
value = None

# Type checking
type(42)           # <class 'int'>
isinstance(42, int)  # True
```

### Type Hints (Optional)

```python
# Variable annotations (Python 3.6+)
name: str = "Alice"
age: int = 25
height: float = 5.8
is_student: bool = True

# Function annotations
def greet(name: str) -> str:
    return f"Hello, {name}"

# Multiple types (Union)
from typing import Union, Optional, List, Dict

def process(value: Union[int, str]) -> str:
    return str(value)

# Optional (can be None)
def find_user(id: int) -> Optional[str]:
    return None

# Collections
numbers: List[int] = [1, 2, 3]
mapping: Dict[str, int] = {"a": 1, "b": 2}
```

## Functions

### Function Declaration

```python
# Basic function
def greet(name):
    return f"Hello, {name}"

# With type hints
def add(a: int, b: int) -> int:
    return a + b

# Multiple return values
def get_coordinates():
    return 10, 20

x, y = get_coordinates()

# Default parameters
def greet(name, greeting="Hello"):
    return f"{greeting}, {name}"

greet("Alice")              # "Hello, Alice"
greet("Bob", "Hi")          # "Hi, Bob"
greet("Carol", greeting="Hey")  # Named argument

# Variable arguments (*args, **kwargs)
def sum_all(*args):
    return sum(args)

sum_all(1, 2, 3, 4)  # 10

def print_info(**kwargs):
    for key, value in kwargs.items():
        print(f"{key}: {value}")

print_info(name="Alice", age=25)
```

### Lambda Functions

```python
# Lambda syntax
multiply = lambda a, b: a * b
multiply(3, 4)  # 12

# Single parameter
double = lambda x: x * 2
double(5)  # 10

# With map, filter, reduce
numbers = [1, 2, 3, 4, 5]
squared = list(map(lambda x: x**2, numbers))  # [1, 4, 9, 16, 25]
evens = list(filter(lambda x: x % 2 == 0, numbers))  # [2, 4]

from functools import reduce
total = reduce(lambda acc, x: acc + x, numbers, 0)  # 15
```

### Decorators

```python
# Basic decorator
def logger(func):
    def wrapper(*args, **kwargs):
        print(f"Calling {func.__name__}")
        result = func(*args, **kwargs)
        print(f"Finished {func.__name__}")
        return result
    return wrapper

@logger
def greet(name):
    return f"Hello, {name}"

# Decorator with arguments
def repeat(times):
    def decorator(func):
        def wrapper(*args, **kwargs):
            for _ in range(times):
                func(*args, **kwargs)
        return wrapper
    return decorator

@repeat(3)
def say_hello():
    print("Hello!")

# Built-in decorators
class MyClass:
    @property
    def value(self):
        return self._value

    @value.setter
    def value(self, v):
        self._value = v

    @staticmethod
    def static_method():
        return "Static"

    @classmethod
    def class_method(cls):
        return f"Class: {cls.__name__}"
```

## Control Flow

### If Statement

```python
# Basic if-elif-else
if x > 0:
    print("Positive")
elif x < 0:
    print("Negative")
else:
    print("Zero")

# Ternary operator (conditional expression)
result = "positive" if x > 0 else "non-positive"

# Multiple conditions
if x > 0 and y > 0:
    print("Both positive")

if x < 0 or y < 0:
    print("At least one negative")

# Truthiness
if value:  # True for non-empty, non-zero, not None
    print("Truthy")

# Walrus operator (Python 3.8+)
if (n := len(items)) > 10:
    print(f"Too many items: {n}")
```

### Loops

```python
# For loop
for i in range(5):
    print(i)  # 0, 1, 2, 3, 4

# Range variations
for i in range(1, 6):      # 1, 2, 3, 4, 5
    print(i)

for i in range(0, 10, 2):  # 0, 2, 4, 6, 8 (step=2)
    print(i)

for i in range(5, 0, -1):  # 5, 4, 3, 2, 1 (reverse)
    print(i)

# Iterate with index
items = ["a", "b", "c"]
for index, value in enumerate(items):
    print(f"{index}: {value}")

# While loop
while condition:
    # code
    pass

# Break and continue
for i in range(10):
    if i == 5:
        break  # Exit loop
    if i % 2 == 0:
        continue  # Skip to next iteration
    print(i)

# Else clause (runs if no break)
for i in range(5):
    if i == 10:
        break
else:
    print("Completed without break")
```

## Data Structures

### Lists

```python
# Creating lists
empty = []
numbers = [1, 2, 3, 4, 5]
mixed = [1, "two", 3.0, True]

# List operations
numbers.append(6)          # Add to end
numbers.insert(0, 0)       # Insert at index
numbers.extend([7, 8])     # Add multiple
numbers.remove(3)          # Remove first occurrence
popped = numbers.pop()     # Remove and return last
popped = numbers.pop(0)    # Remove at index

# Indexing and slicing
first = numbers[0]         # First element
last = numbers[-1]         # Last element
slice = numbers[1:4]       # Elements 1-3 (excludes 4)
slice = numbers[:3]        # First 3 elements
slice = numbers[3:]        # From index 3 to end
slice = numbers[::2]       # Every 2nd element
reversed = numbers[::-1]   # Reverse list

# List methods
numbers.sort()             # Sort in place
sorted_nums = sorted(numbers)  # Return sorted copy
numbers.reverse()          # Reverse in place
count = numbers.count(2)   # Count occurrences
index = numbers.index(3)   # Find index

# List comprehension
squares = [x**2 for x in range(5)]  # [0, 1, 4, 9, 16]
evens = [x for x in range(10) if x % 2 == 0]  # [0, 2, 4, 6, 8]
```

### Tuples

```python
# Immutable sequences
empty = ()
single = (1,)  # Note the comma
coords = (10, 20)
rgb = (255, 128, 0)

# Unpacking
x, y = coords
r, g, b = rgb

# Named tuples
from collections import namedtuple

Point = namedtuple('Point', ['x', 'y'])
p = Point(10, 20)
print(p.x, p.y)  # 10 20
```

### Sets

```python
# Unordered, unique elements
empty = set()
numbers = {1, 2, 3, 4, 5}
mixed = {1, "two", 3.0}

# Set operations
numbers.add(6)             # Add element
numbers.remove(3)          # Remove (raises KeyError if not found)
numbers.discard(3)         # Remove (no error if not found)
popped = numbers.pop()     # Remove arbitrary element

# Set operations
a = {1, 2, 3}
b = {3, 4, 5}

union = a | b              # {1, 2, 3, 4, 5}
intersection = a & b       # {3}
difference = a - b         # {1, 2}
symmetric_diff = a ^ b     # {1, 2, 4, 5}

# Set comprehension
squares = {x**2 for x in range(5)}  # {0, 1, 4, 9, 16}
```

### Dictionaries

```python
# Key-value mappings
empty = {}
person = {"name": "Alice", "age": 25, "city": "NYC"}

# Dictionary operations
person["email"] = "alice@example.com"  # Add/update
age = person["age"]                    # Get value
age = person.get("age", 0)             # Get with default
del person["city"]                     # Delete key
popped = person.pop("age", None)       # Remove and return

# Dictionary methods
keys = person.keys()                   # dict_keys(['name', 'email'])
values = person.values()               # dict_values(['Alice', 'alice@...'])
items = person.items()                 # dict_items([('name', 'Alice'), ...])

# Merging (Python 3.9+)
defaults = {"theme": "dark", "lang": "en"}
settings = defaults | {"theme": "light"}  # {'theme': 'light', 'lang': 'en'}

# Dictionary comprehension
squares = {x: x**2 for x in range(5)}  # {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}
filtered = {k: v for k, v in person.items() if len(k) > 3}
```

## Classes and OOP

### Class Declaration

```python
# Basic class
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def greet(self):
        return f"Hello, I'm {self.name}"

    def celebrate_birthday(self):
        self.age += 1

# Creating instance
person = Person("Alice", 25)
print(person.greet())
person.celebrate_birthday()
```

### Properties and Methods

```python
class Rectangle:
    def __init__(self, width, height):
        self._width = width
        self._height = height

    @property
    def area(self):
        return self._width * self._height

    @property
    def width(self):
        return self._width

    @width.setter
    def width(self, value):
        if value <= 0:
            raise ValueError("Width must be positive")
        self._width = value

    @staticmethod
    def from_square(side):
        return Rectangle(side, side)

    @classmethod
    def unit_square(cls):
        return cls(1, 1)

rect = Rectangle(10, 5)
print(rect.area)  # 50
rect.width = 20   # Calls setter
square = Rectangle.from_square(5)
```

### Magic Methods

```python
class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        return f"Vector({self.x}, {self.y})"

    def __repr__(self):
        return f"Vector(x={self.x}, y={self.y})"

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __len__(self):
        return int((self.x**2 + self.y**2)**0.5)

    def __getitem__(self, index):
        return [self.x, self.y][index]

    def __call__(self, scalar):
        return Vector(self.x * scalar, self.y * scalar)

v1 = Vector(1, 2)
v2 = Vector(3, 4)
v3 = v1 + v2           # __add__
print(v1)              # __str__
length = len(v1)       # __len__
scaled = v1(2)         # __call__
```

### Inheritance

```python
# Single inheritance
class Animal:
    def __init__(self, name):
        self.name = name

    def speak(self):
        pass

class Dog(Animal):
    def speak(self):
        return f"{self.name} says Woof!"

class Cat(Animal):
    def speak(self):
        return f"{self.name} says Meow!"

# Multiple inheritance
class Flyable:
    def fly(self):
        return "Flying"

class Bird(Animal, Flyable):
    def speak(self):
        return f"{self.name} says Chirp!"

# Method resolution order (MRO)
print(Bird.__mro__)

# Super() usage
class Employee:
    def __init__(self, name, salary):
        self.name = name
        self.salary = salary

class Manager(Employee):
    def __init__(self, name, salary, department):
        super().__init__(name, salary)
        self.department = department
```

### Dataclasses (Python 3.7+)

```python
from dataclasses import dataclass, field

@dataclass
class User:
    id: int
    name: str
    email: str
    active: bool = True
    tags: list = field(default_factory=list)

user = User(1, "Alice", "alice@example.com")
print(user)  # User(id=1, name='Alice', email='alice@...', active=True, tags=[])

# Auto-generated methods
user2 = User(1, "Alice", "alice@example.com")
print(user == user2)  # True (auto __eq__)

# Frozen (immutable)
@dataclass(frozen=True)
class Point:
    x: int
    y: int
```

## Comprehensions

### List Comprehensions

```python
# Basic
squares = [x**2 for x in range(10)]

# With condition
evens = [x for x in range(10) if x % 2 == 0]

# Nested
matrix = [[i*j for j in range(3)] for i in range(3)]

# Flatten
nested = [[1, 2], [3, 4], [5, 6]]
flat = [x for sublist in nested for x in sublist]  # [1, 2, 3, 4, 5, 6]
```

### Dictionary Comprehensions

```python
# Basic
squares = {x: x**2 for x in range(5)}

# From two lists
keys = ['a', 'b', 'c']
values = [1, 2, 3]
mapping = {k: v for k, v in zip(keys, values)}

# Swap keys and values
inverted = {v: k for k, v in mapping.items()}

# With condition
filtered = {k: v for k, v in mapping.items() if v > 1}
```

### Set Comprehensions

```python
# Basic
unique_lengths = {len(word) for word in ["hello", "world", "python"]}

# With condition
vowels = {char for char in "hello world" if char in "aeiou"}
```

### Generator Expressions

```python
# Memory-efficient (lazy evaluation)
squares = (x**2 for x in range(1000000))  # Doesn't compute all at once

# Use in loops
for square in squares:
    if square > 100:
        break
    print(square)

# Convert to list
list_squares = list((x**2 for x in range(10)))
```

## Iterators and Generators

### Iterators

```python
# Creating iterator
numbers = [1, 2, 3]
iterator = iter(numbers)

print(next(iterator))  # 1
print(next(iterator))  # 2
print(next(iterator))  # 3
# next(iterator)  # Raises StopIteration

# Custom iterator
class Counter:
    def __init__(self, max_count):
        self.max_count = max_count
        self.count = 0

    def __iter__(self):
        return self

    def __next__(self):
        if self.count < self.max_count:
            self.count += 1
            return self.count
        raise StopIteration

for num in Counter(5):
    print(num)  # 1, 2, 3, 4, 5
```

### Generators

```python
# Generator function (uses yield)
def count_up_to(max_count):
    count = 1
    while count <= max_count:
        yield count
        count += 1

for num in count_up_to(5):
    print(num)  # 1, 2, 3, 4, 5

# Generator expression (see above)
squares = (x**2 for x in range(10))

# Generator methods
def fibonacci():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b

fib = fibonacci()
print(next(fib))  # 0
print(next(fib))  # 1
print(next(fib))  # 1

# Yield from (delegate to sub-generator)
def chain(*iterables):
    for iterable in iterables:
        yield from iterable

list(chain([1, 2], [3, 4], [5, 6]))  # [1, 2, 3, 4, 5, 6]
```

## Modules and Packages

### Importing

```python
# Import module
import math
print(math.pi)

# Import specific items
from math import pi, sqrt
print(pi)

# Import with alias
import numpy as np
import pandas as pd

# Import all (not recommended)
from math import *

# Relative imports (within package)
from . import sibling_module
from .. import parent_module
from .submodule import function
```

### Creating Modules

```python
# mymodule.py
def greet(name):
    return f"Hello, {name}"

PI = 3.14159

class Calculator:
    def add(self, a, b):
        return a + b

# Usage in other file
import mymodule
print(mymodule.greet("Alice"))
print(mymodule.PI)
calc = mymodule.Calculator()
```

### Package Structure

```
mypackage/
├── __init__.py        # Makes directory a package
├── module1.py
├── module2.py
└── subpackage/
    ├── __init__.py
    └── module3.py
```

```python
# __init__.py
from .module1 import function1
from .module2 import function2

__all__ = ['function1', 'function2']

# Usage
from mypackage import function1
```

## Exception Handling

### Try-Except

```python
# Basic try-except
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero")

# Multiple exceptions
try:
    result = risky_operation()
except (ValueError, TypeError) as e:
    print(f"Error: {e}")
except Exception as e:
    print(f"Unexpected error: {e}")

# Finally (always executes)
try:
    file = open("data.txt")
    data = file.read()
except FileNotFoundError:
    print("File not found")
finally:
    file.close()  # Always close

# Else (runs if no exception)
try:
    result = safe_operation()
except Exception as e:
    print(f"Error: {e}")
else:
    print("Success!")
finally:
    cleanup()
```

### Raising Exceptions

```python
# Raise exception
def divide(a, b):
    if b == 0:
        raise ValueError("Cannot divide by zero")
    return a / b

# Custom exception
class InvalidEmailError(Exception):
    pass

def validate_email(email):
    if "@" not in email:
        raise InvalidEmailError(f"Invalid email: {email}")

# Re-raising
try:
    risky_operation()
except Exception as e:
    log_error(e)
    raise  # Re-raise the same exception
```

### Context Managers

```python
# With statement (auto cleanup)
with open("data.txt", "r") as file:
    content = file.read()
# File automatically closed

# Multiple context managers
with open("input.txt") as infile, open("output.txt", "w") as outfile:
    outfile.write(infile.read())

# Custom context manager
class Timer:
    def __enter__(self):
        self.start = time.time()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.end = time.time()
        print(f"Time: {self.end - self.start}s")

with Timer():
    # Code to time
    pass

# Using contextlib
from contextlib import contextmanager

@contextmanager
def managed_resource():
    resource = acquire_resource()
    try:
        yield resource
    finally:
        release_resource(resource)

with managed_resource() as r:
    use_resource(r)
```

## File I/O

### Reading Files

```python
# Read entire file
with open("file.txt", "r") as f:
    content = f.read()

# Read lines
with open("file.txt", "r") as f:
    lines = f.readlines()  # List of lines

# Iterate lines (memory efficient)
with open("file.txt", "r") as f:
    for line in f:
        print(line.strip())

# Read specific number of characters
with open("file.txt", "r") as f:
    chunk = f.read(100)  # First 100 characters
```

### Writing Files

```python
# Write (overwrites)
with open("output.txt", "w") as f:
    f.write("Hello, World!\n")
    f.writelines(["Line 1\n", "Line 2\n"])

# Append
with open("output.txt", "a") as f:
    f.write("Appended line\n")

# Binary mode
with open("image.jpg", "rb") as f:
    data = f.read()

with open("copy.jpg", "wb") as f:
    f.write(data)
```

### Pathlib (Modern File Handling)

```python
from pathlib import Path

# Path operations
path = Path("data/file.txt")
path.exists()              # Check existence
path.is_file()             # Is file?
path.is_dir()              # Is directory?

# Read/write
content = path.read_text()
path.write_text("New content")

data = path.read_bytes()
path.write_bytes(data)

# Path manipulation
parent = path.parent       # data/
name = path.name           # file.txt
stem = path.stem           # file
suffix = path.suffix       # .txt

# Joining paths
new_path = path.parent / "other.txt"  # data/other.txt

# Listing directory
for item in Path("data").iterdir():
    print(item)

# Glob patterns
txt_files = Path("data").glob("*.txt")
all_files = Path("data").rglob("*")  # Recursive
```

## String Operations

### String Formatting

```python
name = "Alice"
age = 25

# f-strings (Python 3.6+, recommended)
message = f"Name: {name}, Age: {age}"
formatted = f"Price: ${price:.2f}"  # 2 decimal places

# Format method
message = "Name: {}, Age: {}".format(name, age)
message = "Name: {n}, Age: {a}".format(n=name, a=age)

# Old-style (not recommended)
message = "Name: %s, Age: %d" % (name, age)

# Multi-line f-strings (includes newlines at start/end)
message = f"""
Name: {name}
Age: {age}
"""  # Result: "\nName: Alice\nAge: 25\n"

# Without leading/trailing newlines
message = f"""Name: {name}
Age: {age}"""  # Result: "Name: Alice\nAge: 25"
```

### String Methods

```python
text = "  Hello, World!  "

# Case
text.upper()               # "  HELLO, WORLD!  "
text.lower()               # "  hello, world!  "
text.capitalize()          # "  hello, world!  "
text.title()               # "  Hello, World!  "

# Whitespace
text.strip()               # "Hello, World!"
text.lstrip()              # "Hello, World!  "
text.rstrip()              # "  Hello, World!"

# Search
text.startswith("  He")    # True
text.endswith("!  ")       # True
text.find("World")         # 9 (or -1 if not found)
text.index("World")        # 9 (raises ValueError if not found)
text.count("l")            # 3

# Replace
text.replace("World", "Python")  # "  Hello, Python!  "

# Split and join
words = "a,b,c".split(",")       # ["a", "b", "c"]
joined = ",".join(["a", "b", "c"])  # "a,b,c"

# Check content
"123".isdigit()            # True
"abc".isalpha()            # True
"abc123".isalnum()         # True
```

## Common Patterns

### EAFP vs LBYL

```python
# EAFP (Easier to Ask Forgiveness than Permission) - Pythonic
try:
    value = dictionary[key]
except KeyError:
    value = default

# LBYL (Look Before You Leap) - Not Pythonic
if key in dictionary:
    value = dictionary[key]
else:
    value = default

# Better: Use get()
value = dictionary.get(key, default)
```

### Duck Typing

```python
# If it walks like a duck and quacks like a duck...
def process_items(items):
    # Doesn't check if items is a list, just that it's iterable
    for item in items:
        print(item)

process_items([1, 2, 3])        # Works
process_items((1, 2, 3))        # Works
process_items({1, 2, 3})        # Works
process_items(range(3))         # Works
```

### Unpacking

```python
# Sequence unpacking
a, b = 1, 2
x, y, z = [1, 2, 3]

# Extended unpacking
first, *rest = [1, 2, 3, 4, 5]  # first=1, rest=[2,3,4,5]
*beginning, last = [1, 2, 3, 4, 5]  # beginning=[1,2,3,4], last=5
first, *middle, last = [1, 2, 3, 4, 5]  # middle=[2,3,4]

# Dictionary unpacking
defaults = {"a": 1, "b": 2}
overrides = {"b": 3, "c": 4}
merged = {**defaults, **overrides}  # {"a": 1, "b": 3, "c": 4}

# Function argument unpacking
def greet(name, age):
    print(f"{name} is {age}")

args = ("Alice", 25)
greet(*args)  # Unpack positional

kwargs = {"name": "Bob", "age": 30}
greet(**kwargs)  # Unpack keyword
```

### Enumerate and Zip

```python
# Enumerate (index + value)
for index, value in enumerate(["a", "b", "c"]):
    print(f"{index}: {value}")

# With custom start
for index, value in enumerate(["a", "b", "c"], start=1):
    print(f"{index}: {value}")  # 1: a, 2: b, 3: c

# Zip (parallel iteration)
names = ["Alice", "Bob", "Carol"]
ages = [25, 30, 35]
for name, age in zip(names, ages):
    print(f"{name}: {age}")

# Zip to dictionary
mapping = dict(zip(names, ages))  # {"Alice": 25, "Bob": 30, "Carol": 35}

# Unzip
pairs = [(1, "a"), (2, "b"), (3, "c")]
numbers, letters = zip(*pairs)  # (1, 2, 3), ("a", "b", "c")
```

### Common Idioms

```python
# Swap variables
a, b = b, a

# Chain comparisons
if 0 < x < 10:
    print("x is between 0 and 10")

# Default dictionary
from collections import defaultdict
counts = defaultdict(int)
counts["a"] += 1  # No KeyError

# Counter
from collections import Counter
counts = Counter(["a", "b", "a", "c", "b", "a"])
print(counts)  # Counter({'a': 3, 'b': 2, 'c': 1})
counts.most_common(2)  # [('a', 3), ('b', 2)]

# Named tuples (see earlier)
# Dataclasses (see earlier)

# Sorting with key
items = ["apple", "Banana", "cherry"]
sorted(items, key=str.lower)  # Case-insensitive sort
sorted(items, key=len)        # Sort by length

people = [{"name": "Alice", "age": 25}, {"name": "Bob", "age": 30}]
sorted(people, key=lambda p: p["age"])  # Sort by age
```

## Testing Quick Reference

```python
import unittest

class TestCalculator(unittest.TestCase):
    def setUp(self):
        # Runs before each test
        self.calc = Calculator()

    def tearDown(self):
        # Runs after each test
        pass

    def test_addition(self):
        result = self.calc.add(2, 3)
        self.assertEqual(result, 5)

    def test_division_by_zero(self):
        with self.assertRaises(ZeroDivisionError):
            self.calc.divide(10, 0)

    def test_multiple_assertions(self):
        self.assertTrue(1 < 2)
        self.assertFalse(1 > 2)
        self.assertIsNone(None)
        self.assertIsNotNone(1)
        self.assertIn(1, [1, 2, 3])
        self.assertGreater(10, 5)

if __name__ == "__main__":
    unittest.main()
```

```python
# Pytest (modern, recommended)
import pytest

def test_addition():
    assert 2 + 3 == 5

def test_division_by_zero():
    with pytest.raises(ZeroDivisionError):
        10 / 0

def test_approximate():
    assert 0.1 + 0.2 == pytest.approx(0.3)

# Fixtures
@pytest.fixture
def sample_data():
    return [1, 2, 3, 4, 5]

def test_with_fixture(sample_data):
    assert len(sample_data) == 5

# Parametrize
@pytest.mark.parametrize("input,expected", [
    (1, 1),
    (2, 4),
    (3, 9),
])
def test_square(input, expected):
    assert input ** 2 == expected
```

## Learn More

**Detailed Documentation**:

- [Quick Start Tutorial](/en/learn/software-engineering/programming-language/python/tutorials/quick-start) - Python touchpoints
- [Beginner Tutorial](/en/learn/software-engineering/programming-language/python/tutorials/beginner) - Comprehensive fundamentals
- [Cookbook](/en/learn/software-engineering/programming-language/python/how-to/cookbook) - Practical recipes
- [How-To Guides](/en/learn/software-engineering/programming-language/python/how-to) - Problem-solving guides

**Official Resources**:

- [Python Documentation](https://docs.python.org/3/) - Complete language documentation
- [Python Standard Library](https://docs.python.org/3/library/) - Built-in modules reference
- [PEP 8](https://peps.python.org/pep-0008/) - Python style guide
