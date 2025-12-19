---
title: "Cheat Sheet"
date: 2025-12-19T00:00:00+07:00
draft: false
description: "Quick reference for Python syntax, operators, built-in functions, and common patterns"
weight: 803
tags: ["python", "reference", "cheat-sheet", "syntax"]
---

**Need a quick Python syntax lookup?** This cheat sheet provides instant reference for Python's most commonly used syntax, operators, built-in functions, and standard library patterns. Perfect for when you know what you want to do but need to remember the exact syntax.

For comprehensive explanations, see the [tutorials section](/en/learn/swe/prog-lang/python/tutorials/overview). For problem-solving patterns, check the [how-to guides](/en/learn/swe/prog-lang/python/how-to/overview).

## 🔤 Basic Syntax

### Variables and Assignment

```python
# Basic assignment
x = 10
name = "Alice"

# Multiple assignment
a, b, c = 1, 2, 3

# Swapping
x, y = y, x

# Type hints (optional)
age: int = 30
price: float = 19.99
```

### Comments

```python
# Single line comment

"""
Multi-line comment
or docstring
"""
```

## 📊 Data Types

### Primitives

```python
# Integer (unlimited precision)
x = 42
big = 10**100

# Float
pi = 3.14159
scientific = 1.5e-10

# Boolean
is_valid = True
is_empty = False

# None
result = None

# String
text = "Hello"
multiline = """Line 1
Line 2"""
```

### Type Checking

```python
type(x)              # Get type
isinstance(x, int)   # Check type
```

## 🔄 Operators

### Arithmetic

```python
x + y       # Addition
x - y       # Subtraction
x * y       # Multiplication
x / y       # Division (float)
x // y      # Floor division (int)
x % y       # Modulo
x ** y      # Exponentiation
```

### Comparison

```python
x == y      # Equal
x != y      # Not equal
x > y       # Greater than
x < y       # Less than
x >= y      # Greater or equal
x <= y      # Less or equal
```

### Logical

```python
x and y     # Logical AND
x or y      # Logical OR
not x       # Logical NOT
```

### Bitwise

```python
x & y       # Bitwise AND
x | y       # Bitwise OR
x ^ y       # Bitwise XOR
~x          # Bitwise NOT
x << n      # Left shift
x >> n      # Right shift
```

### Membership & Identity

```python
x in seq        # Membership
x not in seq    # Not in
x is y          # Identity (same object)
x is not y      # Not same object
```

## 🔀 Control Flow

### If/Elif/Else

```python
if condition:
    # do something
elif other_condition:
    # do something else
else:
    # default case

# Ternary
result = value_if_true if condition else value_if_false
```

### Loops

```python
# For loop
for item in iterable:
    # process item

for i in range(10):
    # 0 to 9

for i in range(5, 10):
    # 5 to 9

for i in range(0, 10, 2):
    # 0, 2, 4, 6, 8

# While loop
while condition:
    # repeat

# Loop control
break       # Exit loop
continue    # Next iteration
pass        # Do nothing
```

### Enumerate & Zip

```python
# Enumerate (index + value)
for i, item in enumerate(items):
    print(f"{i}: {item}")

# Zip (combine iterables)
for a, b in zip(list1, list2):
    print(a, b)
```

## 📚 Collections

### Lists

```python
# Create
lst = [1, 2, 3]
empty = []

# Access
lst[0]          # First
lst[-1]         # Last
lst[1:3]        # Slice

# Modify
lst.append(4)           # Add to end
lst.insert(0, 0)        # Insert at index
lst.extend([5, 6])      # Add multiple
lst.remove(3)           # Remove first occurrence
lst.pop()               # Remove & return last
lst.pop(0)              # Remove & return at index

# Operations
len(lst)        # Length
x in lst        # Membership
lst.sort()      # Sort in place
sorted(lst)     # Return sorted copy
lst.reverse()   # Reverse in place
lst.count(x)    # Count occurrences
lst.index(x)    # Find index

# List comprehension
[x**2 for x in range(10)]
[x for x in range(20) if x % 2 == 0]
```

### Tuples

```python
# Create (immutable)
tup = (1, 2, 3)
single = (42,)      # Note comma

# Unpack
a, b, c = tup
```

### Sets

```python
# Create (unique, unordered)
s = {1, 2, 3}
empty = set()

# Modify
s.add(4)            # Add element
s.remove(2)         # Remove (raises error if not found)
s.discard(2)        # Remove (no error)

# Operations
s1 | s2             # Union
s1 & s2             # Intersection
s1 - s2             # Difference
s1 ^ s2             # Symmetric difference
```

### Dictionaries

```python
# Create
d = {"key": "value", "age": 30}
empty = {}

# Access
d["key"]            # Direct (raises KeyError if missing)
d.get("key")        # Safe (returns None if missing)
d.get("key", "default")  # With default

# Modify
d["new"] = "value"  # Add/update
del d["key"]        # Delete
d.pop("key")        # Remove & return

# Iterate
for key in d:
    pass
for key, value in d.items():
    pass
for key in d.keys():
    pass
for value in d.values():
    pass

# Dict comprehension
{k: v**2 for k, v in d.items()}
```

## 🔧 Functions

### Basic Functions

```python
# Simple function
def greet(name):
    return f"Hello, {name}"

# With type hints
def add(a: int, b: int) -> int:
    return a + b

# Default arguments
def power(base, exp=2):
    return base ** exp

# Multiple returns
def stats(nums):
    return min(nums), max(nums), sum(nums)
```

### Advanced Parameters

```python
# *args (variable positional)
def sum_all(*args):
    return sum(args)

# **kwargs (variable keyword)
def print_info(**kwargs):
    for k, v in kwargs.items():
        print(f"{k}: {v}")

# Combined
def func(required, *args, **kwargs):
    pass
```

### Lambda

```python
# Anonymous function
square = lambda x: x**2
add = lambda x, y: x + y

# In sorting
sorted(items, key=lambda x: x['age'])
```

### Decorators (Basic)

```python
@decorator
def function():
    pass

# Common decorators
@property
@staticmethod
@classmethod
```

## 🎭 Classes

### Basic Class

```python
class Person:
    # Class attribute
    species = "Homo sapiens"

    def __init__(self, name, age):
        # Instance attributes
        self.name = name
        self.age = age

    def greet(self):
        return f"Hello, I'm {self.name}"

# Create instance
p = Person("Alice", 30)
p.greet()
```

### Inheritance

```python
class Employee(Person):
    def __init__(self, name, age, salary):
        super().__init__(name, age)
        self.salary = salary
```

### Magic Methods

```python
def __str__(self):          # str() and print()
    return f"Person({self.name})"

def __repr__(self):         # repr()
    return f"Person(name={self.name})"

def __len__(self):          # len()
    return self.age

def __eq__(self, other):    # ==
    return self.name == other.name

def __add__(self, other):   # +
    return Person(...)
```

## 📖 Strings

### Common Methods

```python
s = "  Hello, World!  "

# Case
s.lower()           # lowercase
s.upper()           # UPPERCASE
s.title()           # Title Case
s.capitalize()      # Capitalize first

# Whitespace
s.strip()           # Remove leading/trailing
s.lstrip()          # Remove leading
s.rstrip()          # Remove trailing

# Search
s.find("World")     # Index or -1
s.index("World")    # Index or ValueError
s.count("l")        # Count occurrences
s.startswith("He")  # Boolean
s.endswith("!")     # Boolean

# Transform
s.replace("World", "Python")
s.split(",")        # Split into list
" ".join(["a", "b"])  # Join list

# Formatting
f"Hello, {name}"    # f-string
"Hello, {}".format(name)  # .format()
```

## 📁 File I/O

### Reading

```python
# Read entire file
with open("file.txt", "r") as f:
    content = f.read()

# Read lines
with open("file.txt", "r") as f:
    for line in f:
        print(line.strip())

# Read all lines into list
with open("file.txt", "r") as f:
    lines = f.readlines()
```

### Writing

```python
# Write (overwrite)
with open("file.txt", "w") as f:
    f.write("Hello\n")
    f.writelines(["Line 1\n", "Line 2\n"])

# Append
with open("file.txt", "a") as f:
    f.write("More data\n")
```

### File Modes

```python
"r"     # Read (default)
"w"     # Write (create/truncate)
"a"     # Append
"r+"    # Read + write
"b"     # Binary mode ("rb", "wb")
```

### Pathlib (Modern)

```python
from pathlib import Path

p = Path("file.txt")
content = p.read_text()
p.write_text("Hello")
p.exists()
p.is_file()
p.is_dir()
```

## ⚠️ Error Handling

### Try/Except

```python
try:
    # risky code
    result = 10 / x
except ZeroDivisionError:
    # handle specific error
    print("Cannot divide by zero")
except (ValueError, TypeError) as e:
    # handle multiple errors
    print(f"Error: {e}")
except Exception as e:
    # catch all
    print(f"Unexpected: {e}")
else:
    # runs if no exception
    print("Success")
finally:
    # always runs
    cleanup()
```

### Raising Exceptions

```python
raise ValueError("Invalid input")
raise RuntimeError("Something went wrong")

# Re-raise
except Exception:
    # log error
    raise  # re-raise same exception
```

## 📦 Modules & Imports

### Import Syntax

```python
import module
import module as alias
from module import item
from module import item1, item2
from module import *  # Avoid in production
```

### Common Standard Library

```python
import os           # OS interface
import sys          # System
import json         # JSON
import re           # Regex
import math         # Math
import random       # Random
import datetime     # Date/time
import collections  # Data structures
import itertools    # Iterators
import functools    # Higher-order functions
```

## 🧪 Common Patterns

### List Comprehensions

```python
# Basic
[x**2 for x in range(10)]

# With condition
[x for x in range(20) if x % 2 == 0]

# Nested
[[i*j for j in range(3)] for i in range(3)]
```

### Dict/Set Comprehensions

```python
# Dict
{k: v**2 for k, v in dict.items()}

# Set
{x for x in range(10) if x % 2 == 0}
```

### Generator Expression

```python
# Memory efficient (lazy evaluation)
gen = (x**2 for x in range(1000000))
```

### Context Managers

```python
with open("file.txt") as f:
    content = f.read()
# File automatically closed

# Multiple context managers
with open("in.txt") as fin, open("out.txt", "w") as fout:
    fout.write(fin.read())
```

### Unpacking

```python
# List unpacking
first, *middle, last = [1, 2, 3, 4, 5]

# Dict unpacking
d1 = {**d2, **d3}  # Merge dicts

# Function unpacking
args = [1, 2, 3]
func(*args)  # Same as func(1, 2, 3)

kwargs = {"a": 1, "b": 2}
func(**kwargs)  # Same as func(a=1, b=2)
```

## 🔢 Built-in Functions

### Common Built-ins

```python
abs(x)              # Absolute value
len(seq)            # Length
min(seq)            # Minimum
max(seq)            # Maximum
sum(seq)            # Sum
sorted(seq)         # Return sorted copy
reversed(seq)       # Return reversed iterator

any(seq)            # True if any element is true
all(seq)            # True if all elements are true

enumerate(seq)      # (index, value) pairs
zip(seq1, seq2)     # Combine sequences

map(func, seq)      # Apply function
filter(func, seq)   # Filter elements

range(n)            # 0 to n-1
range(start, end)   # start to end-1
range(start, end, step)  # with step

input("prompt")     # Read user input
print(*values)      # Print to stdout

open(file, mode)    # Open file
type(obj)           # Get type
isinstance(obj, type)  # Check type
```

### Type Conversion

```python
int("42")           # String to int
float("3.14")       # String to float
str(42)             # To string
bool(value)         # To boolean

list(iterable)      # To list
tuple(iterable)     # To tuple
set(iterable)       # To set
dict(pairs)         # To dict
```

## 📊 Collections Module

```python
from collections import Counter, defaultdict, deque, namedtuple

# Counter - count elements
Counter([1, 2, 2, 3, 3, 3])
# Counter({3: 3, 2: 2, 1: 1})

# defaultdict - default values
d = defaultdict(int)  # default to 0
d = defaultdict(list) # default to []

# deque - double-ended queue
dq = deque([1, 2, 3])
dq.append(4)        # Right
dq.appendleft(0)    # Left
dq.pop()            # Right
dq.popleft()        # Left

# namedtuple - tuple with named fields
Point = namedtuple('Point', ['x', 'y'])
p = Point(1, 2)
print(p.x, p.y)
```

## 🎯 Virtual Environment Commands

```bash
# Create virtual environment
python -m venv venv

# Activate
source venv/bin/activate  # macOS/Linux
venv\Scripts\activate     # Windows

# Deactivate
deactivate

# Install package
pip install package-name

# Install from requirements
pip install -r requirements.txt

# Freeze dependencies
pip freeze > requirements.txt

# List installed
pip list
```

## 🧪 Testing (pytest)

```python
# Test function (starts with test_)
def test_function():
    assert 1 + 1 == 2
    assert "hello".upper() == "HELLO"

# Test with exception
import pytest
def test_exception():
    with pytest.raises(ValueError):
        int("not a number")

# Parametrized test
@pytest.mark.parametrize("input,expected", [
    (1, 2),
    (2, 4),
    (3, 6),
])
def test_double(input, expected):
    assert input * 2 == expected
```

## 🚀 Performance Tips

```python
# Use list comprehension instead of loop
# Fast
squares = [x**2 for x in range(1000)]

# Slower
squares = []
for x in range(1000):
    squares.append(x**2)

# Use join for string concatenation
# Fast
result = "".join(strings)

# Slower
result = ""
for s in strings:
    result += s

# Use sets for membership testing
# Fast (O(1))
if x in my_set:
    pass

# Slower (O(n))
if x in my_list:
    pass
```

## 📖 Quick Reference Links

- **Official Python Docs**: [docs.python.org](https://docs.python.org/)
- **Python Package Index**: [pypi.org](https://pypi.org/)
- **PEP 8 Style Guide**: [pep8.org](https://pep8.org/)
- **Python Standard Library**: [docs.python.org/3/library](https://docs.python.org/3/library/)

## 🔗 See Also

- [Glossary](/en/learn/swe/prog-lang/python/reference/glossary) - Terminology definitions
- [Resources](/en/learn/swe/prog-lang/python/reference/resources) - Learning materials and tools
- [Tutorials](/en/learn/swe/prog-lang/python/tutorials/overview) - Comprehensive guides
- [How-To Guides](/en/learn/swe/prog-lang/python/how-to/overview) - Problem-solving patterns
