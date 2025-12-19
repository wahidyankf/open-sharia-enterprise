---
title: "Glossary"
date: 2025-12-19T00:00:00+07:00
draft: false
description: "Comprehensive glossary of Python terminology, concepts, and jargon"
weight: 804
tags: ["python", "reference", "glossary", "terminology"]
---

**Need to understand Python terminology?** This glossary defines Python-specific terms, concepts, and jargon. Each entry includes a clear definition and, where appropriate, a simple example.

For quick syntax reference, see the [Cheat Sheet](/en/learn/swe/prog-lang/python/reference/cheat-sheet). For learning materials, check the [Resources](/en/learn/swe/prog-lang/python/reference/resources).

## A

### Abstract Base Class (ABC)

A class that cannot be instantiated directly and serves as a template for other classes. Defined using the `abc` module.

```python
from abc import ABC, abstractmethod

class Shape(ABC):
    @abstractmethod
    def area(self):
        pass
```

### Argument

A value passed to a function when calling it. Can be positional or keyword-based.

```python
def greet(name, greeting="Hello"):
    print(f"{greeting}, {name}")

greet("Alice")                # Positional argument
greet(name="Bob")             # Keyword argument
greet("Charlie", "Hi")        # Multiple positional
```

### Asynchronous Programming

Programming paradigm where operations can run concurrently without blocking. Uses `async`/`await` keywords.

```python
import asyncio

async def fetch_data():
    await asyncio.sleep(1)
    return "data"
```

### Attribute

A value associated with an object, accessed using dot notation. Can be data or methods.

```python
class Person:
    def __init__(self, name):
        self.name = name  # Instance attribute

p = Person("Alice")
print(p.name)  # Access attribute
```

## B

### Boolean

A data type with two values: `True` or `False`. Used for logical operations.

```python
is_valid = True
is_empty = False
result = (5 > 3)  # True
```

### Built-in Function

A function that's always available without importing. Examples: `len()`, `print()`, `range()`.

```python
print(len([1, 2, 3]))  # 3
print(max(1, 5, 3))    # 5
```

### Bytecode

Platform-independent intermediate representation of Python source code. Stored in `.pyc` files and executed by the Python interpreter.

## C

### Class

A blueprint for creating objects. Defines attributes and methods.

```python
class Dog:
    def __init__(self, name):
        self.name = name

    def bark(self):
        return f"{self.name} barks!"
```

### Class Method

A method bound to the class rather than instances. Defined with `@classmethod` decorator.

```python
class MyClass:
    count = 0

    @classmethod
    def increment(cls):
        cls.count += 1
```

### Closure

A function that references variables from its enclosing scope.

```python
def outer(x):
    def inner(y):
        return x + y  # References x from outer scope
    return inner

add_5 = outer(5)
print(add_5(3))  # 8
```

### Comprehension

Concise syntax for creating sequences (lists, dicts, sets) from iterables.

```python
# List comprehension
squares = [x**2 for x in range(10)]

# Dict comprehension
{k: v**2 for k, v in items.items()}

# Set comprehension
{x for x in range(10) if x % 2 == 0}
```

### Context Manager

An object that defines `__enter__` and `__exit__` methods, used with `with` statement for resource management.

```python
with open("file.txt") as f:
    content = f.read()
# File automatically closed
```

## D

### Decorator

A function that modifies the behavior of another function or class. Applied using `@` syntax.

```python
def my_decorator(func):
    def wrapper(*args, **kwargs):
        print("Before")
        result = func(*args, **kwargs)
        print("After")
        return result
    return wrapper

@my_decorator
def greet():
    print("Hello")
```

### Dictionary (dict)

Mutable collection of key-value pairs. Maintains insertion order (Python 3.7+).

```python
person = {"name": "Alice", "age": 30}
print(person["name"])  # Alice
```

### Docstring

A string literal that documents a module, class, function, or method. Accessed via `__doc__` attribute.

```python
def add(a, b):
    """Add two numbers and return the result."""
    return a + b

print(add.__doc__)
```

### Duck Typing

Programming style where an object's suitability is determined by its methods and properties, not its type.

> "If it walks like a duck and quacks like a duck, it's a duck."

```python
# Any object with write() method works
def save(file_like):
    file_like.write("data")
```

### Dynamic Typing

Type checking performed at runtime. Variables can hold different types.

```python
x = 10       # int
x = "hello"  # now str - allowed in Python
```

## E

### EAFP

"Easier to Ask for Forgiveness than Permission" - Python idiom of trying operations and handling exceptions rather than checking conditions first.

```python
# EAFP (Pythonic)
try:
    value = my_dict["key"]
except KeyError:
    value = "default"

# LBYL (less Pythonic)
if "key" in my_dict:
    value = my_dict["key"]
else:
    value = "default"
```

### Encapsulation

Bundling data and methods that operate on that data within a class. Python uses naming conventions (single underscore for "internal use").

```python
class Account:
    def __init__(self, balance):
        self._balance = balance  # Convention: internal use
```

### Exception

An error that occurs during program execution. Can be caught and handled with try/except.

```python
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero")
```

### Expression

A combination of values, variables, and operators that evaluates to a value.

```python
5 + 3           # Expression evaluates to 8
x * 2           # Expression
len([1, 2, 3])  # Expression evaluates to 3
```

## F

### First-Class Function

Functions are objects that can be assigned to variables, passed as arguments, and returned from other functions.

```python
def greet(name):
    return f"Hello, {name}"

# Assign to variable
say_hello = greet

# Pass as argument
def call_twice(func, arg):
    func(arg)
    func(arg)

call_twice(greet, "Alice")
```

### F-String

Formatted string literal (Python 3.6+). Prefix string with `f` and embed expressions in `{}`.

```python
name = "Alice"
age = 30
print(f"{name} is {age} years old")
# Alice is 30 years old
```

## G

### Generator

A function that yields values one at a time instead of returning all at once. Memory efficient for large sequences.

```python
def count_up_to(n):
    i = 0
    while i < n:
        yield i
        i += 1

for num in count_up_to(5):
    print(num)  # 0, 1, 2, 3, 4
```

### Generator Expression

A memory-efficient alternative to list comprehension. Creates a generator object.

```python
# List comprehension (creates list)
squares = [x**2 for x in range(1000)]

# Generator expression (lazy evaluation)
squares = (x**2 for x in range(1000))
```

### Global Interpreter Lock (GIL)

A mutex that protects access to Python objects, preventing multiple threads from executing Python bytecode simultaneously. Affects multi-threaded performance.

### Global Variable

A variable defined at the module level, accessible throughout the module.

```python
GLOBAL_VAR = 100

def use_global():
    global GLOBAL_VAR
    GLOBAL_VAR = 200  # Modify global
```

## H

### Hashable

An object is hashable if it has a hash value that never changes during its lifetime. Required for dict keys and set elements. Immutable types (int, str, tuple) are hashable.

```python
# Hashable (can be dict key)
d = {(1, 2): "value"}  # Tuple as key

# Not hashable (cannot be dict key)
# d = {[1, 2]: "value"}  # Error: list is mutable
```

## I

### Immutable

An object whose value cannot be changed after creation. Examples: int, float, str, tuple.

```python
s = "hello"
# s[0] = "H"  # Error: strings are immutable

# Create new string instead
s = "H" + s[1:]  # "Hello"
```

### Import

Statement that makes code from another module available in the current module.

```python
import math
from collections import Counter
import datetime as dt
```

### Inheritance

Mechanism where a class derives attributes and methods from another class.

```python
class Animal:
    def speak(self):
        pass

class Dog(Animal):  # Dog inherits from Animal
    def speak(self):
        return "Woof!"
```

### Instance

A concrete occurrence of a class. Created by calling the class.

```python
class Dog:
    pass

buddy = Dog()  # buddy is an instance of Dog
```

### Instance Method

A method that operates on an instance of a class. First parameter is `self`.

```python
class Counter:
    def __init__(self):
        self.count = 0

    def increment(self):  # Instance method
        self.count += 1
```

### Iterator

An object that implements `__iter__()` and `__next__()` methods. Can be iterated over in a for loop.

```python
my_list = [1, 2, 3]
iterator = iter(my_list)
print(next(iterator))  # 1
print(next(iterator))  # 2
```

### Iterable

An object capable of returning its members one at a time. Examples: list, tuple, dict, set, string.

```python
for char in "hello":  # String is iterable
    print(char)
```

## K

### Keyword Argument

An argument passed to a function using parameter name.

```python
def greet(name, greeting="Hello"):
    print(f"{greeting}, {name}")

greet(name="Alice", greeting="Hi")  # Keyword arguments
```

### kwargs

Convention for `**kwargs` parameter that captures variable keyword arguments as a dictionary.

```python
def print_info(**kwargs):
    for key, value in kwargs.items():
        print(f"{key}: {value}")

print_info(name="Alice", age=30)
```

## L

### Lambda

Anonymous function defined with `lambda` keyword. Limited to single expressions.

```python
add = lambda x, y: x + y
print(add(3, 5))  # 8

# Common in sorting
sorted(people, key=lambda p: p['age'])
```

### List

Ordered, mutable sequence of elements.

```python
numbers = [1, 2, 3, 4, 5]
numbers.append(6)
print(numbers[0])  # 1
```

### List Comprehension

Concise syntax for creating lists.

```python
squares = [x**2 for x in range(10)]
evens = [x for x in range(20) if x % 2 == 0]
```

## M

### Magic Method (Dunder Method)

Special methods with double underscores (e.g., `__init__`, `__str__`). Define how objects behave with built-in operations.

```python
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):  # Define + operator
        return Point(self.x + other.x, self.y + other.y)
```

### Method

A function defined inside a class.

```python
class Dog:
    def bark(self):  # Method
        return "Woof!"
```

### Module

A file containing Python code (definitions, functions, classes). Can be imported into other modules.

```python
# File: my_module.py
def greet(name):
    return f"Hello, {name}"

# File: main.py
import my_module
print(my_module.greet("Alice"))
```

### Mutable

An object whose value can be changed after creation. Examples: list, dict, set.

```python
lst = [1, 2, 3]
lst[0] = 10  # Allowed: lists are mutable
print(lst)   # [10, 2, 3]
```

## N

### Namespace

A mapping from names to objects. Examples: global namespace, local namespace, built-in namespace.

```python
x = 10  # Global namespace

def func():
    y = 20  # Local namespace (function)
    print(x)  # Access global
```

### None

A special constant representing the absence of a value. Similar to `null` in other languages.

```python
result = None
if result is None:
    print("No value")
```

## O

### Object

An instance of a class. Everything in Python is an object.

```python
x = 10          # int object
s = "hello"     # str object
lst = [1, 2, 3] # list object
```

### Object-Oriented Programming (OOP)

Programming paradigm based on objects that contain data and code. Python supports classes, inheritance, encapsulation, and polymorphism.

## P

### Package

A directory containing Python modules and a `__init__.py` file.

```
my_package/
    __init__.py
    module1.py
    module2.py
```

### Parameter

A variable in a function definition that receives an argument when the function is called.

```python
def greet(name):  # 'name' is a parameter
    print(f"Hello, {name}")

greet("Alice")  # "Alice" is an argument
```

### PEP (Python Enhancement Proposal)

A design document providing information or describing a new feature for Python. PEP 8 is the style guide.

### pip

Package installer for Python. Used to install packages from PyPI (Python Package Index).

```bash
pip install requests
pip list
pip freeze > requirements.txt
```

### Polymorphism

Ability of different classes to be treated as instances of the same class through inheritance.

```python
class Animal:
    def speak(self):
        pass

class Dog(Animal):
    def speak(self):
        return "Woof!"

class Cat(Animal):
    def speak(self):
        return "Meow!"

animals = [Dog(), Cat()]
for animal in animals:
    print(animal.speak())  # Polymorphic behavior
```

### Property

A way to define getters, setters, and deleters for class attributes using the `@property` decorator.

```python
class Temperature:
    def __init__(self, celsius):
        self._celsius = celsius

    @property
    def fahrenheit(self):
        return self._celsius * 9/5 + 32

    @fahrenheit.setter
    def fahrenheit(self, value):
        self._celsius = (value - 32) * 5/9
```

### PyPI (Python Package Index)

Official repository for third-party Python packages. Accessible at [pypi.org](https://pypi.org/).

## R

### REPL (Read-Eval-Print Loop)

Interactive Python shell for executing Python code and seeing immediate results.

```bash
$ python
>>> 2 + 2
4
>>> print("Hello")
Hello
```

## S

### Scope

The region of code where a variable is accessible. Python has LEGB rule: Local, Enclosing, Global, Built-in.

```python
x = "global"

def outer():
    x = "enclosing"

    def inner():
        x = "local"
        print(x)  # "local"

    inner()
```

### Sequence

An ordered collection of elements. Examples: list, tuple, string, range.

```python
# All sequences support indexing and slicing
lst = [1, 2, 3]
tup = (1, 2, 3)
s = "hello"
```

### Set

Unordered collection of unique elements.

```python
s = {1, 2, 3, 3}  # {1, 2, 3} - duplicates removed
s.add(4)
```

### Slice

A subset of a sequence specified by start, stop, and step indices.

```python
lst = [0, 1, 2, 3, 4, 5]
lst[1:4]    # [1, 2, 3]
lst[::2]    # [0, 2, 4]
lst[::-1]   # [5, 4, 3, 2, 1, 0] (reversed)
```

### Static Method

A method that doesn't operate on instances or the class. Defined with `@staticmethod` decorator.

```python
class Math:
    @staticmethod
    def add(x, y):
        return x + y

Math.add(3, 5)  # 8 - called without instance
```

### String

Immutable sequence of characters. Can use single, double, or triple quotes.

```python
s1 = 'single'
s2 = "double"
s3 = """triple
quotes"""
```

## T

### Truthy/Falsy

Values that evaluate to True or False in boolean context.

**Falsy values**: `False`, `None`, `0`, `0.0`, `''`, `[]`, `{}`, `set()`

**Truthy values**: Everything else

```python
if []:  # Empty list is falsy
    print("Won't print")

if [1]:  # Non-empty list is truthy
    print("Will print")
```

### Tuple

Immutable, ordered sequence of elements.

```python
coords = (10, 20)
single = (42,)  # Note comma
x, y = coords   # Unpacking
```

### Type Hint

Optional syntax for specifying expected types (Python 3.5+). Not enforced at runtime.

```python
def add(a: int, b: int) -> int:
    return a + b

name: str = "Alice"
```

## U

### Unpacking

Extracting values from sequences or mappings.

```python
# Tuple unpacking
a, b = (1, 2)

# Extended unpacking
first, *middle, last = [1, 2, 3, 4, 5]

# Dict unpacking
d = {**dict1, **dict2}  # Merge dicts
```

## V

### Virtual Environment

Isolated Python environment with its own packages and dependencies. Created with `venv` module.

```bash
python -m venv venv
source venv/bin/activate  # macOS/Linux
venv\Scripts\activate     # Windows
```

## Y

### yield

Keyword used in generators to produce a value and pause function execution.

```python
def count_up(n):
    i = 0
    while i < n:
        yield i  # Produces value, pauses
        i += 1
```

## Z

### Zen of Python

Guiding principles for Python design. View with `import this`.

Key principles:

- Beautiful is better than ugly
- Explicit is better than implicit
- Simple is better than complex
- Readability counts

## 🔗 See Also

- [Cheat Sheet](/en/learn/swe/prog-lang/python/reference/cheat-sheet) - Quick syntax reference
- [Resources](/en/learn/swe/prog-lang/python/reference/resources) - Learning materials and tools
- [Tutorials](/en/learn/swe/prog-lang/python/tutorials/overview) - Comprehensive learning paths
- [PEP Index](https://peps.python.org/) - Python Enhancement Proposals
