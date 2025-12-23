---
title: "Glossary"
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000040
description: Python-specific terminology and definitions for learners and developers
---

**Comprehensive glossary** of Python-specific terms, concepts, and language features. Organized alphabetically for quick reference.

## A

### Args (\*args)

**Definition**: Variable-length positional arguments that allow a function to accept any number of positional parameters, collected as a tuple.

**Example**:

```python
def sum_all(*args):
    return sum(args)

result = sum_all(1, 2, 3, 4, 5)  # 15
```

**See Also**: Kwargs, Function Parameters, Unpacking

### Asyncio

**Definition**: Standard library module providing infrastructure for writing asynchronous concurrent code using async/await syntax.

**Example**:

```python
import asyncio

async def fetch_data():
    await asyncio.sleep(1)
    return "Data"

asyncio.run(fetch_data())
```

**See Also**: Async/Await, Coroutine, Event Loop

### Async/Await

**Definition**: Keywords for defining and calling asynchronous functions (coroutines) that can pause and resume execution.

**Example**:

```python
async def process_data():
    data = await fetch_data()
    return data.upper()
```

**See Also**: Asyncio, Coroutine, Generator

## B

### Bytecode

**Definition**: Intermediate low-level representation of Python code that the interpreter executes, stored in `.pyc` files.

**See Also**: CPython, Interpreter, PYC File

## C

### Class Method

**Definition**: Method decorated with `@classmethod` that receives the class as first argument (conventionally `cls`) instead of instance.

**Example**:

```python
class User:
    count = 0

    def __init__(self, name):
        self.name = name
        User.count += 1

    @classmethod
    def get_count(cls):
        return cls.count
```

**See Also**: Static Method, Instance Method, Decorator

### Comprehension

**Definition**: Concise syntax for creating collections (lists, dicts, sets) by transforming and filtering iterables.

**Example**:

```python
# List comprehension
squares = [x**2 for x in range(10)]

# Dict comprehension
word_lengths = {word: len(word) for word in ["hello", "world"]}

# Set comprehension
unique_squares = {x**2 for x in [1, 2, 2, 3, 3, 3]}

# With condition
evens = [x for x in range(20) if x % 2 == 0]
```

**See Also**: Generator Expression, Filter, Map

### Context Manager

**Definition**: Object that defines runtime context using `__enter__` and `__exit__` methods, used with `with` statement.

**Example**:

```python
with open("file.txt", "r") as f:
    content = f.read()
# File automatically closed

# Custom context manager
class Timer:
    def __enter__(self):
        self.start = time.time()
        return self

    def __exit__(self, *args):
        print(f"Elapsed: {time.time() - self.start}")

with Timer():
    # code to time
```

**See Also**: With Statement, Magic Method, Decorator

### Coroutine

**Definition**: Function defined with `async def` that can be paused and resumed, used for asynchronous programming.

**Example**:

```python
async def fetch_user(user_id):
    await asyncio.sleep(0.1)
    return {"id": user_id, "name": "Alice"}
```

**See Also**: Async/Await, Asyncio, Generator

### CPython

**Definition**: Reference implementation of Python written in C, the most widely used Python interpreter.

**See Also**: Interpreter, GIL, Bytecode, PyPy

## D

### Dataclass

**Definition**: Class decorator from `dataclasses` module that automatically generates special methods (`__init__`, `__repr__`, `__eq__`).

**Example**:

```python
from dataclasses import dataclass

@dataclass
class Point:
    x: float
    y: float
    name: str = "Origin"

p = Point(1.0, 2.0)
print(p)  # Point(x=1.0, y=2.0, name='Origin')
```

**See Also**: Type Hints, Decorator, Named Tuple

### Decorator

**Definition**: Function that wraps another function or class to modify its behavior, applied using `@decorator_name` syntax.

**Example**:

```python
def log_calls(func):
    def wrapper(*args, **kwargs):
        print(f"Calling {func.__name__}")
        return func(*args, **kwargs)
    return wrapper

@log_calls
def greet(name):
    return f"Hello, {name}"

greet("Alice")  # Prints: Calling greet
```

**See Also**: Higher-Order Function, Wrapper, Functools

### Descriptor

**Definition**: Object that defines attribute access behavior through `__get__`, `__set__`, and `__delete__` methods.

**Example**:

```python
class Positive:
    def __get__(self, obj, objtype=None):
        return obj._value

    def __set__(self, obj, value):
        if value < 0:
            raise ValueError("Must be positive")
        obj._value = value

class Account:
    balance = Positive()
```

**See Also**: Property, Magic Method, Attribute Access

### Dictionary (Dict)

**Definition**: Built-in mutable mapping type that stores key-value pairs with fast lookup, using hash table implementation.

**Example**:

```python
user = {"name": "Alice", "age": 30}
user["email"] = "alice@example.com"
age = user.get("age", 0)

# Dictionary methods
keys = user.keys()
values = user.values()
items = user.items()
```

**See Also**: Hash, Mapping, Comprehension

### Docstring

**Definition**: String literal appearing as first statement in module, function, class, or method, used for documentation.

**Example**:

```python
def calculate_area(radius):
    """
    Calculate the area of a circle.

    Args:
        radius (float): The radius of the circle

    Returns:
        float: The area of the circle
    """
    return 3.14159 * radius ** 2
```

**See Also**: Documentation, Help Function, PEP 257

### Duck Typing

**Definition**: Programming style where object's suitability is determined by presence of methods/attributes, not inheritance.

**Example**:

```python
# If it walks like a duck and quacks like a duck, it's a duck
class Duck:
    def quack(self):
        print("Quack!")

class Person:
    def quack(self):
        print("I'm imitating a duck!")

def make_it_quack(thing):
    thing.quack()  # Works with any object that has quack()

make_it_quack(Duck())
make_it_quack(Person())
```

**See Also**: Protocol, ABC, Type Hints

### Dunder Method (Magic Method)

**Definition**: Special methods with double underscores before and after name (e.g., `__init__`, `__str__`) that provide object behavior.

**Example**:

```python
class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)

    def __repr__(self):
        return f"Vector({self.x}, {self.y})"

v1 = Vector(1, 2)
v2 = Vector(3, 4)
v3 = v1 + v2  # Vector(4, 6)
```

**See Also**: Magic Method, Operator Overloading, Special Method

## E

### EAFP (Easier to Ask for Forgiveness than Permission)

**Definition**: Python coding style that assumes validity and catches exceptions, rather than checking conditions beforehand.

**Example**:

```python
# EAFP style (Pythonic)
try:
    value = my_dict[key]
except KeyError:
    value = default_value

# LBYL style (not recommended)
if key in my_dict:
    value = my_dict[key]
else:
    value = default_value
```

**See Also**: Exception Handling, Pythonic, Try/Except

### Enumerate

**Definition**: Built-in function that adds a counter to an iterable, returning tuples of (index, value).

**Example**:

```python
fruits = ["apple", "banana", "cherry"]

for index, fruit in enumerate(fruits):
    print(f"{index}: {fruit}")

# Start from custom index
for i, fruit in enumerate(fruits, start=1):
    print(f"{i}. {fruit}")
```

**See Also**: Iterable, Zip, Range

### Exception

**Definition**: Object representing an error or abnormal condition, which can be raised and caught to handle errors.

**Example**:

```python
try:
    result = 10 / 0
except ZeroDivisionError as e:
    print(f"Error: {e}")
except Exception as e:
    print(f"Unexpected error: {e}")
finally:
    print("Cleanup code")
```

**See Also**: Try/Except, Raise, EAFP

## F

### F-string (Formatted String Literal)

**Definition**: String literal prefixed with `f` that allows embedded expressions inside curly braces, evaluated at runtime.

**Example**:

```python
name = "Alice"
age = 30
greeting = f"Hello, {name}! You are {age} years old."

# Expressions and formatting
price = 19.99
message = f"Total: ${price * 1.1:.2f}"  # Total: $21.99

# Multi-line f-strings
info = f"""
Name: {name}
Age: {age}
Year born: {2025 - age}
"""
```

**See Also**: String Formatting, Format Method, % Formatting

### Filter

**Definition**: Built-in function that constructs an iterator from elements of iterable for which a function returns true.

**Example**:

```python
numbers = [1, 2, 3, 4, 5, 6]
evens = list(filter(lambda x: x % 2 == 0, numbers))  # [2, 4, 6]
```

**See Also**: Map, Comprehension, Lambda

### First-Class Function

**Definition**: Functions that can be assigned to variables, passed as arguments, and returned from other functions.

**Example**:

```python
def add(a, b):
    return a + b

# Assign to variable
operation = add

# Pass as argument
def apply_twice(func, x):
    return func(func(x, x), x)

# Return from function
def make_multiplier(n):
    def multiplier(x):
        return x * n
    return multiplier
```

**See Also**: Higher-Order Function, Lambda, Closure

## G

### Generator

**Definition**: Function that uses `yield` to produce a sequence of values lazily, maintaining state between yields.

**Example**:

```python
def fibonacci(n):
    a, b = 0, 1
    for _ in range(n):
        yield a
        a, b = b, a + b

for num in fibonacci(10):
    print(num)

# Generator expression
squares = (x**2 for x in range(1000000))  # Memory efficient
```

**See Also**: Yield, Iterator, Comprehension

### Generator Expression

**Definition**: Compact syntax for creating generators, similar to list comprehension but with parentheses.

**Example**:

```python
# List comprehension (creates list in memory)
squares_list = [x**2 for x in range(1000000)]

# Generator expression (lazy evaluation)
squares_gen = (x**2 for x in range(1000000))

# Use in functions
total = sum(x**2 for x in range(100))
```

**See Also**: Generator, Comprehension, Lazy Evaluation

### GIL (Global Interpreter Lock)

**Definition**: Mutex in CPython that prevents multiple threads from executing Python bytecode simultaneously.

**Example**:

```python
# GIL limits CPU-bound multi-threading
import threading

# Not truly parallel for CPU tasks
threads = [threading.Thread(target=cpu_intensive) for _ in range(4)]

# Use multiprocessing for parallel CPU tasks
import multiprocessing
processes = [multiprocessing.Process(target=cpu_intensive) for _ in range(4)]
```

**See Also**: Threading, Multiprocessing, CPython

## H

### Hash

**Definition**: Integer value computed from an object's content, used for dictionary keys and set membership.

**Example**:

```python
# Immutable objects are hashable
hash("hello")
hash(42)
hash((1, 2, 3))

# Mutable objects are not hashable
# hash([1, 2, 3])  # TypeError

# Custom hash
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __hash__(self):
        return hash((self.x, self.y))
```

**See Also**: Dictionary, Set, Immutable, **hash**

### Higher-Order Function

**Definition**: Function that takes functions as arguments or returns functions as results.

**Example**:

```python
def apply_operation(func, x, y):
    return func(x, y)

result = apply_operation(lambda a, b: a + b, 5, 3)  # 8

# Returning functions
def make_adder(n):
    def adder(x):
        return x + n
    return adder

add_5 = make_adder(5)
print(add_5(10))  # 15
```

**See Also**: First-Class Function, Lambda, Decorator

## I

### Immutable

**Definition**: Objects whose value cannot be changed after creation (e.g., int, str, tuple, frozenset).

**Example**:

```python
# Immutable types
text = "hello"
# text[0] = "H"  # TypeError

number = 42
coordinates = (10, 20)

# Mutable types
my_list = [1, 2, 3]
my_list[0] = 10  # OK
```

**See Also**: Mutable, Hashable, Tuple

### Import

**Definition**: Statement that loads modules or specific objects from modules into current namespace.

**Example**:

```python
# Module import
import math
print(math.pi)

# Specific imports
from datetime import datetime, timedelta

# Aliasing
import numpy as np
from collections import defaultdict as dd

# Relative imports (in packages)
from . import sibling_module
from .. import parent_module
```

**See Also**: Module, Package, Namespace

### \_\_init\_\_ Method

**Definition**: Special method called when creating a new instance of a class, used for initialization.

**Example**:

```python
class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age
        print(f"Created person: {name}")

person = Person("Alice", 30)
```

**See Also**: Constructor, Dunder Method, \_\_new\_\_

### Iterable

**Definition**: Object capable of returning its elements one at a time, implementing `__iter__` method.

**Example**:

```python
# Built-in iterables
for char in "hello":  # String is iterable
    print(char)

for num in [1, 2, 3]:  # List is iterable
    print(num)

# Custom iterable
class Countdown:
    def __init__(self, start):
        self.start = start

    def __iter__(self):
        return CountdownIterator(self.start)
```

**See Also**: Iterator, Generator, For Loop

### Iterator

**Definition**: Object representing a stream of data, implementing `__iter__` and `__next__` methods.

**Example**:

```python
my_list = [1, 2, 3]
iterator = iter(my_list)

print(next(iterator))  # 1
print(next(iterator))  # 2
print(next(iterator))  # 3
# next(iterator)  # StopIteration

# Custom iterator
class CountdownIterator:
    def __init__(self, start):
        self.current = start

    def __iter__(self):
        return self

    def __next__(self):
        if self.current <= 0:
            raise StopIteration
        self.current -= 1
        return self.current + 1
```

**See Also**: Iterable, Generator, Next Function

## K

### Kwargs (\*\*kwargs)

**Definition**: Variable-length keyword arguments that allow a function to accept any number of named parameters, collected as a dictionary.

**Example**:

```python
def create_user(**kwargs):
    return {
        "name": kwargs.get("name", "Unknown"),
        "age": kwargs.get("age", 0),
        "email": kwargs.get("email")
    }

user = create_user(name="Alice", age=30, city="NYC")
```

**See Also**: Args, Function Parameters, Unpacking

## L

### Lambda

**Definition**: Anonymous function defined with `lambda` keyword, limited to single expression.

**Example**:

```python
# Simple lambda
square = lambda x: x ** 2
print(square(5))  # 25

# With multiple arguments
add = lambda a, b: a + b

# In higher-order functions
numbers = [1, 2, 3, 4, 5]
squares = list(map(lambda x: x**2, numbers))
evens = list(filter(lambda x: x % 2 == 0, numbers))

# Sorting with key
users = [("Alice", 30), ("Bob", 25)]
sorted_users = sorted(users, key=lambda x: x[1])
```

**See Also**: Function, Higher-Order Function, Map, Filter

### List

**Definition**: Built-in mutable sequence type that stores ordered collection of items.

**Example**:

```python
# Creating lists
numbers = [1, 2, 3, 4, 5]
mixed = [1, "hello", 3.14, True]

# List operations
numbers.append(6)
numbers.extend([7, 8])
numbers.insert(0, 0)
numbers.remove(3)
popped = numbers.pop()

# List methods
numbers.sort()
numbers.reverse()
count = numbers.count(2)
```

**See Also**: Comprehension, Sequence, Mutable

### List Comprehension

**Definition**: Concise syntax for creating lists by transforming and filtering iterables.

**Example**:

```python
# Basic comprehension
squares = [x**2 for x in range(10)]

# With condition
evens = [x for x in range(20) if x % 2 == 0]

# Nested comprehension
matrix = [[i*j for j in range(3)] for i in range(3)]

# Multiple conditions
values = [x for x in range(100) if x % 2 == 0 if x % 3 == 0]
```

**See Also**: Comprehension, Generator Expression, Filter

## M

### \_\_main\_\_ Module

**Definition**: Special name assigned to the module being run as main program. Used to execute code only when script is run directly.

**Example**:

```python
def main():
    print("Running as main program")

if __name__ == "__main__":
    main()
```

**See Also**: Module, Import, Script

### Map

**Definition**: Built-in function that applies a function to every item of an iterable, returning an iterator.

**Example**:

```python
numbers = [1, 2, 3, 4, 5]
squares = list(map(lambda x: x**2, numbers))  # [1, 4, 9, 16, 25]

# Multiple iterables
a = [1, 2, 3]
b = [4, 5, 6]
sums = list(map(lambda x, y: x + y, a, b))  # [5, 7, 9]
```

**See Also**: Filter, Lambda, Comprehension

### Metaclass

**Definition**: Class of a class that defines how a class behaves. A class is an instance of a metaclass.

**Example**:

```python
class Meta(type):
    def __new__(cls, name, bases, dct):
        dct['added_attribute'] = 100
        return super().__new__(cls, name, bases, dct)

class MyClass(metaclass=Meta):
    pass

print(MyClass.added_attribute)  # 100
```

**See Also**: Type, Class, OOP

### Method Resolution Order (MRO)

**Definition**: Order in which base classes are searched when executing a method, following C3 linearization.

**Example**:

```python
class A:
    def method(self):
        print("A")

class B(A):
    def method(self):
        print("B")

class C(A):
    def method(self):
        print("C")

class D(B, C):
    pass

print(D.__mro__)  # Shows: D -> B -> C -> A -> object
D().method()  # Prints: B
```

**See Also**: Inheritance, Super, Multiple Inheritance

### Module

**Definition**: File containing Python definitions and statements. The filename is module name with `.py` suffix.

**Example**:

```python
# mymodule.py
def greet(name):
    return f"Hello, {name}"

PI = 3.14159

# Using the module
import mymodule
print(mymodule.greet("Alice"))
print(mymodule.PI)
```

**See Also**: Package, Import, \_\_name\_\_

### Mutable

**Definition**: Objects whose value can be changed after creation (e.g., list, dict, set).

**Example**:

```python
# Mutable types
my_list = [1, 2, 3]
my_list[0] = 10
my_list.append(4)

my_dict = {"a": 1}
my_dict["b"] = 2

# Be careful with mutable default arguments
def bad_function(items=[]):  # Dangerous!
    items.append(1)
    return items

# Better approach
def good_function(items=None):
    if items is None:
        items = []
    items.append(1)
    return items
```

**See Also**: Immutable, List, Dictionary

## N

### Namespace

**Definition**: Mapping from names to objects, implementing scope in Python (local, enclosing, global, built-in).

**Example**:

```python
# Global namespace
global_var = "global"

def outer():
    # Enclosing namespace
    enclosing_var = "enclosing"

    def inner():
        # Local namespace
        local_var = "local"
        print(locals())

# Built-in namespace (always available)
print(len([1, 2, 3]))
```

**See Also**: Scope, Global, Nonlocal

### None

**Definition**: Built-in constant representing the absence of a value or null value.

**Example**:

```python
def find_user(user_id):
    if user_id == 0:
        return None
    return {"id": user_id}

result = find_user(0)
if result is None:
    print("User not found")

# Default arguments
def greet(name=None):
    if name is None:
        name = "Guest"
    return f"Hello, {name}"
```

**See Also**: Null, Boolean, NoneType

## P

### Package

**Definition**: Directory containing Python modules and an `__init__.py` file, creating a hierarchical module namespace.

**Example**:

```python
# Directory structure:
# mypackage/
#   __init__.py
#   module1.py
#   module2.py
#   subpackage/
#     __init__.py
#     module3.py

# Using the package
from mypackage import module1
from mypackage.subpackage import module3
import mypackage.module2
```

**See Also**: Module, Import, \_\_init\_\_.py

### PEP (Python Enhancement Proposal)

**Definition**: Design document providing information to the Python community or describing a new feature for Python.

**Example**:

```python
# PEP 8: Style Guide for Python Code
# PEP 20: The Zen of Python
import this

# PEP 484: Type Hints
def greet(name: str) -> str:
    return f"Hello, {name}"

# PEP 498: F-strings
message = f"Hello, {name}"
```

**See Also**: PEP 8, PEP 20, Type Hints, Zen of Python

### pip

**Definition**: Package installer for Python, used to install and manage software packages from PyPI (Python Package Index).

**Example**:

```bash
# Install package
pip install requests

# Install specific version
pip install requests==2.28.0

# Install from requirements file
pip install -r requirements.txt

# List installed packages
pip list

# Show package info
pip show requests
```

**See Also**: PyPI, Virtual Environment, Requirements.txt

### Property

**Definition**: Built-in decorator that allows methods to be accessed like attributes, providing getter, setter, and deleter.

**Example**:

```python
class Temperature:
    def __init__(self, celsius):
        self._celsius = celsius

    @property
    def celsius(self):
        return self._celsius

    @celsius.setter
    def celsius(self, value):
        if value < -273.15:
            raise ValueError("Temperature below absolute zero")
        self._celsius = value

    @property
    def fahrenheit(self):
        return self._celsius * 9/5 + 32

temp = Temperature(25)
print(temp.celsius)  # 25
temp.celsius = 30
print(temp.fahrenheit)  # 86.0
```

**See Also**: Decorator, Getter/Setter, Descriptor

### Pythonic

**Definition**: Code that follows Python idioms and best practices, embracing the language's philosophy.

**Example**:

```python
# Pythonic
names = [person.name for person in people if person.age >= 18]

# Not Pythonic
names = []
for person in people:
    if person.age >= 18:
        names.append(person.name)

# Pythonic
with open("file.txt") as f:
    content = f.read()

# Not Pythonic
f = open("file.txt")
content = f.read()
f.close()
```

**See Also**: PEP 8, Zen of Python, EAFP

### PyPI (Python Package Index)

**Definition**: Official repository of third-party Python packages, accessible via pip.

**See Also**: pip, Package, Installation

## R

### Range

**Definition**: Built-in immutable sequence type representing a sequence of numbers, commonly used in loops.

**Example**:

```python
# range(stop)
for i in range(5):
    print(i)  # 0, 1, 2, 3, 4

# range(start, stop)
for i in range(2, 7):
    print(i)  # 2, 3, 4, 5, 6

# range(start, stop, step)
for i in range(0, 10, 2):
    print(i)  # 0, 2, 4, 6, 8

# Create list from range
numbers = list(range(5))  # [0, 1, 2, 3, 4]
```

**See Also**: For Loop, Iterable, Sequence

### REPL (Read-Eval-Print Loop)

**Definition**: Interactive Python shell that reads input, evaluates it, prints the result, and loops.

**Example**:

```python
# In terminal, run: python
>>> x = 10
>>> y = 20
>>> x + y
30
>>> print("Hello, Python!")
Hello, Python!
>>> import math
>>> math.pi
3.141592653589793
```

**See Also**: Interactive Mode, Python Shell, IPython

## S

### Self

**Definition**: Conventional name for the first parameter of instance methods, representing the instance itself.

**Example**:

```python
class Circle:
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return 3.14159 * self.radius ** 2

    def resize(self, factor):
        self.radius *= factor
```

**See Also**: Instance Method, Class, OOP

### Set

**Definition**: Built-in unordered collection of unique, hashable items, supporting mathematical set operations.

**Example**:

```python
# Creating sets
fruits = {"apple", "banana", "cherry"}
numbers = set([1, 2, 2, 3, 3, 3])  # {1, 2, 3}

# Set operations
a = {1, 2, 3, 4}
b = {3, 4, 5, 6}

union = a | b           # {1, 2, 3, 4, 5, 6}
intersection = a & b    # {3, 4}
difference = a - b      # {1, 2}
symmetric_diff = a ^ b  # {1, 2, 5, 6}

# Set methods
fruits.add("date")
fruits.remove("apple")
```

**See Also**: Frozenset, Dictionary, Hash

### Slice

**Definition**: Object representing a sequence of indices, used to extract portions of sequences.

**Example**:

```python
numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

# Basic slicing
numbers[2:5]      # [2, 3, 4]
numbers[:5]       # [0, 1, 2, 3, 4]
numbers[5:]       # [5, 6, 7, 8, 9]
numbers[:]        # Copy of entire list

# Step slicing
numbers[::2]      # [0, 2, 4, 6, 8]
numbers[1::2]     # [1, 3, 5, 7, 9]
numbers[::-1]     # [9, 8, 7, 6, 5, 4, 3, 2, 1, 0] (reverse)

# Slice object
s = slice(2, 7, 2)
numbers[s]        # [2, 4, 6]
```

**See Also**: Indexing, List, Sequence

### Static Method

**Definition**: Method decorated with `@staticmethod` that doesn't receive implicit first argument (no `self` or `cls`).

**Example**:

```python
class MathUtils:
    @staticmethod
    def add(a, b):
        return a + b

    @staticmethod
    def is_even(n):
        return n % 2 == 0

result = MathUtils.add(5, 3)
print(MathUtils.is_even(4))  # True
```

**See Also**: Class Method, Instance Method, Decorator

### String (str)

**Definition**: Built-in immutable sequence type representing text as sequence of Unicode characters.

**Example**:

```python
# Creating strings
text = "Hello, World!"
multiline = """This is
a multiline
string"""

# String methods
upper = text.upper()
lower = text.lower()
stripped = "  hello  ".strip()
replaced = text.replace("World", "Python")

# String operations
concatenated = "Hello" + " " + "World"
repeated = "Ha" * 3  # "HaHaHa"
```

**See Also**: F-string, Format, Unicode

## T

### Tuple

**Definition**: Built-in immutable sequence type that stores ordered collection of items.

**Example**:

```python
# Creating tuples
coordinates = (10, 20)
single = (42,)  # Note the comma
point3d = (1, 2, 3)

# Tuple unpacking
x, y = coordinates
first, *rest = (1, 2, 3, 4, 5)

# Named tuples
from collections import namedtuple
Point = namedtuple('Point', ['x', 'y'])
p = Point(11, 22)
print(p.x, p.y)
```

**See Also**: Immutable, Sequence, Unpacking, Named Tuple

### Type Hints

**Definition**: Optional annotations specifying expected types of variables, function parameters, and return values.

**Example**:

```python
from typing import List, Dict, Optional, Union

def greet(name: str) -> str:
    return f"Hello, {name}"

def process_items(items: List[int]) -> Dict[str, int]:
    return {"total": sum(items), "count": len(items)}

def find_user(user_id: int) -> Optional[Dict[str, str]]:
    if user_id == 0:
        return None
    return {"name": "Alice"}

# Complex types
def combine(x: Union[int, float], y: Union[int, float]) -> float:
    return x + y
```

**See Also**: PEP 484, Typing Module, Mypy, Static Analysis

## U

### Unpacking

**Definition**: Extracting values from iterables or mapping them to multiple variables simultaneously.

**Example**:

```python
# Tuple unpacking
x, y = (10, 20)
first, *rest, last = [1, 2, 3, 4, 5]

# Function arguments
def add(a, b, c):
    return a + b + c

numbers = [1, 2, 3]
result = add(*numbers)  # Unpacking positional args

# Dictionary unpacking
kwargs = {"name": "Alice", "age": 30}
def greet(name, age):
    return f"{name} is {age}"

greeting = greet(**kwargs)  # Unpacking keyword args

# Extended unpacking
a, *middle, b = range(10)
```

**See Also**: Args, Kwargs, Destructuring, Tuple

## V

### Virtual Environment

**Definition**: Isolated Python environment with its own packages and dependencies, independent of system Python.

**Example**:

```bash
# Create virtual environment
python -m venv myenv

# Activate (Unix/macOS)
source myenv/bin/activate

# Activate (Windows)
myenv\Scripts\activate

# Install packages in virtual environment
pip install requests

# Deactivate
deactivate
```

**See Also**: pip, Venv, Virtualenv, Package Management

## W

### Walrus Operator (:=)

**Definition**: Assignment expression operator (`:=`) that assigns value to variable as part of larger expression.

**Example**:

```python
# Without walrus operator
data = fetch_data()
if data:
    process(data)

# With walrus operator
if (data := fetch_data()):
    process(data)

# In list comprehension
results = [y for x in range(10) if (y := expensive_func(x)) > 5]

# In while loop
while (line := file.readline()):
    process(line)
```

**See Also**: Assignment, Expression, PEP 572

### With Statement

**Definition**: Statement that wraps execution of a block with methods defined by a context manager.

**Example**:

```python
# File handling
with open("file.txt", "r") as f:
    content = f.read()
# File automatically closed

# Multiple context managers
with open("input.txt") as infile, open("output.txt", "w") as outfile:
    outfile.write(infile.read())

# Custom context manager
from contextlib import contextmanager

@contextmanager
def timer():
    start = time.time()
    yield
    print(f"Elapsed: {time.time() - start}")

with timer():
    # code to time
```

**See Also**: Context Manager, Try/Finally, Resource Management

## Y

### Yield

**Definition**: Keyword used in generator functions to produce a value and suspend execution, resuming on next iteration.

**Example**:

```python
def countdown(n):
    while n > 0:
        yield n
        n -= 1

for i in countdown(5):
    print(i)  # 5, 4, 3, 2, 1

# Yield from (delegation)
def flatten(nested_list):
    for sublist in nested_list:
        yield from sublist

list(flatten([[1, 2], [3, 4]]))  # [1, 2, 3, 4]
```

**See Also**: Generator, Iterator, Yield From

## Z

### Zen of Python

**Definition**: Collection of 19 guiding principles for writing computer programs in Python, by Tim Peters.

**Example**:

```python
import this

# Prints:
# Beautiful is better than ugly.
# Explicit is better than implicit.
# Simple is better than complex.
# Complex is better than complicated.
# Readability counts.
# ...
```

**See Also**: PEP 20, Pythonic, Philosophy

### Zip

**Definition**: Built-in function that aggregates elements from multiple iterables into tuples.

**Example**:

```python
names = ["Alice", "Bob", "Charlie"]
ages = [25, 30, 35]

# Basic zip
pairs = list(zip(names, ages))
# [('Alice', 25), ('Bob', 30), ('Charlie', 35)]

# Unzip with unpacking
names2, ages2 = zip(*pairs)

# Multiple iterables
for name, age, city in zip(names, ages, ["NYC", "LA", "SF"]):
    print(f"{name}, {age}, {city}")

# Different lengths (stops at shortest)
zip([1, 2, 3], [4, 5])  # [(1, 4), (2, 5)]
```

**See Also**: Enumerate, Iterable, Unpacking

## Learn More

**Comprehensive Documentation**:

- [Beginner Tutorial](/en/learn/software-engineering/programming-language/python/tutorials/beginner) - Detailed explanations of concepts
- [Quick Start](/en/learn/software-engineering/programming-language/python/tutorials/quick-start) - Overview of key features
- [How-To Guides](/en/learn/software-engineering/programming-language/python/how-to) - Practical usage examples
- [Cheat Sheet](/en/learn/software-engineering/programming-language/python/reference/cheat-sheet) - Quick syntax reference

**Official Resources**:

- [Python Documentation](https://docs.python.org/) - Official documentation
- [Python Glossary](https://docs.python.org/3/glossary.html) - Official glossary
- [PEP Index](https://www.python.org/dev/peps/) - Python Enhancement Proposals
