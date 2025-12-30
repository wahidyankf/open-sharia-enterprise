---
title: "Beginner"
date: 2025-12-17T15:00:24+07:00
draft: false
weight: 10000000
description: "Comprehensive Python programming tutorial covering 0-60% of the language from scratch with hands-on exercises"
tags:
  - python
  - programming-languages
  - beginner
  - comprehensive
  - object-oriented
---

**Want to harness the power of Python?** Python powers everything from web applications (Instagram, Spotify) to data science (NumPy, Pandas), from AI research (TensorFlow, PyTorch) to automation scripts. Created with a focus on readability and simplicity, Python brings expressive syntax with a massive ecosystem of libraries.

In this Beginner's guide, you'll learn Python from scratch. By the end, you'll write object-oriented programs, handle files and APIs, and understand why companies like Google, Netflix, and NASA chose Python for their critical systems. This tutorial provides comprehensive coverage of Python fundamentals, from basic syntax to object-oriented programming and testing.

## 🎯 What You'll Learn

This Beginner tutorial teaches **comprehensive Python fundamentals** - everything from installation through intermediate patterns, giving you a solid foundation for building real applications:

- Python syntax and core language features
- Data types, functions, and control flow
- Lists, dictionaries, sets, and tuples
- **Object-oriented programming** - classes, inheritance, polymorphism
- **Type hints and duck typing** - Python's approach to typing
- File I/O and working with APIs
- Exception handling - how to raise and handle errors
- Modules and packages - organizing Python code
- Virtual environments and package management with pip
- Testing basics - how to write and run tests with pytest
- Practical exercises at 4 difficulty levels
- Common patterns and troubleshooting

**After this Beginner tutorial**, check out the [Python Cookbook](/en/learn/software-engineering/programming-languages/python/how-to/cookbook) and [How-To Guides](/en/learn/software-engineering/programming-languages/python/how-to/overview) for practical patterns and real-world problem solving, or progress to the [Intermediate tutorial](/en/learn/software-engineering/programming-languages/python/tutorials/by-concept/intermediate) for production-level techniques.

## 📋 Prerequisites

- Basic programming knowledge in any language
- Familiarity with command line/terminal
- Understanding of basic computer science concepts
- Python installed (see [Initial Setup](/en/learn/software-engineering/programming-languages/python/tutorials/initial-setup) if needed)

## Learning Path

This comprehensive tutorial covers Python fundamentals progressively from basic syntax to advanced object-oriented programming:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TB
    Start[Start: Python Basics] --> Syntax[Basic Syntax<br/>Variables, Types, Operators]
    Syntax --> Flow[Control Flow<br/>If, Loops, Logic]
    Flow --> Collections[Collections<br/>Lists, Dicts, Sets, Tuples]
    Collections --> Functions[Functions<br/>Definition, Parameters, Returns]
    Functions --> OOP[Object-Oriented<br/>Classes, Inheritance]
    OOP --> Advanced[Advanced Topics<br/>File I/O, Modules, Testing]
    Advanced --> Ready[Ready to Build!]

    style Start fill:#0173B2
    style Ready fill:#029E73
    style Syntax fill:#DE8F05
    style Flow fill:#DE8F05
    style Collections fill:#DE8F05
    style Functions fill:#DE8F05
    style OOP fill:#CC78BC
    style Advanced fill:#CC78BC
```

This tutorial provides **0-60% coverage** of Python knowledge, giving you a solid foundation to build real applications and progress to [Intermediate](/en/learn/software-engineering/programming-languages/python/tutorials/by-concept/intermediate) and [Advanced](/en/learn/software-engineering/programming-languages/python/tutorials/by-concept/advanced) topics.

## 🚀 Why Python?

- **Readable syntax** - Code reads like English
- **Gentle learning curve** - Perfect for beginners
- **Massive ecosystem** - Libraries for everything (web, data, ML, automation)
- **Versatile** - Backend, scripting, data analysis, ML, automation
- **Dynamic typing** - Fast development with type hints for safety
- **Great community** - Extensive documentation and support
- **Cross-platform** - Write once, run anywhere

## 📦 Setup

### Installation

If you haven't installed Python yet, follow the [Initial Setup](/en/learn/software-engineering/programming-languages/python/tutorials/initial-setup) tutorial for step-by-step installation instructions.

Quick verification:

```bash
python --version

python3 --version
```

### Your First Program

Create `hello.py`:

```python
print("Hello, World!")
```

Run it:

```bash
python hello.py

python3 hello.py
```

## 🔤 Basic Syntax

### Variables and Dynamic Typing

```python
name = "Alice"
age = 30
height = 1.65
is_active = True

city: str = "Jakarta"
count: int = 42
price: float = 99.99

x, y, z = 10, 20, 30

a, b = 5, 10
a, b = b, a  # Now a=10, b=5

MAX_RETRIES = 3
PI = 3.14159

print(name, age, height, is_active)
```

### Basic Types

```python
small_int = 42
big_int = 12345678901234567890  # No overflow!

price = 19.99
scientific = 1.5e-4  # 0.00015

single_quote = 'Hello'
double_quote = "World"
multi_line = """This is
a multi-line
string"""

is_ready = True
is_done = False

result = None

print(type(name))        # <class 'str'>
print(type(age))         # <class 'int'>
print(isinstance(age, int))  # True
```

### String Operations

```python
first = "Hello"
last = "World"
full = first + " " + last  # "Hello World"

repeat = "Ha" * 3  # "HaHaHa"

name = "Alice"
age = 30

message = f"{name} is {age} years old"

text = "  Hello, World!  "
print(text.strip())       # "Hello, World!" (remove whitespace)
print(text.upper())       # "  HELLO, WORLD!  "
print(text.lower())       # "  hello, world!  "
print(text.replace("World", "Python"))  # "  Hello, Python!  "
print("Hello" in text)    # True (substring check)

text = "Python"
print(text[0])      # "P" (first character)
print(text[-1])     # "n" (last character)
print(text[0:3])    # "Pyt" (slice from 0 to 2)
print(text[2:])     # "thon" (slice from 2 to end)
print(text[:4])     # "Pyth" (slice from start to 3)
```

## 🔄 Control Flow

### If/Elif/Else

```python
age = 18

if age >= 18:
    print("Adult")  # Output: Adult

if age >= 18:
    print("Adult")
else:
    print("Minor")

score = 85
if score >= 90:
    grade = "A"
elif score >= 80:
    grade = "B"  # This branch executes
elif score >= 70:
    grade = "C"
else:
    grade = "F"

print(f"Grade: {grade}")  # Grade: B

status = "Adult" if age >= 18 else "Minor"
print(status)  # Adult
```

**Important**: Python uses **indentation** (4 spaces standard) to define code blocks, not braces `{}`.

### Loops

```python
for i in range(5):
    print(i)  # Prints: 0, 1, 2, 3, 4

fruits = ["apple", "banana", "cherry"]
for fruit in fruits:
    print(fruit)

for index, fruit in enumerate(fruits):
    print(f"{index}: {fruit}")
    # Output: 0: apple
    #         1: banana
    #         2: cherry

count = 0
while count < 3:
    print(count)
    count += 1  # Output: 0, 1, 2

for i in range(10):
    if i == 3:
        continue  # Skip 3
    if i == 5:
        break     # Stop at 5
    print(i)      # Prints: 0, 1, 2, 4

for i in range(5):
    if i == 10:
        break
else:
    print("Loop completed")  # This executes
```

### ✅ Checkpoint: Control Flow

Before moving forward, ensure you can:

- [ ] Write `if/elif/else` statements with proper indentation
- [ ] Use ternary operators for simple conditionals
- [ ] Iterate with `for` loops over ranges and collections
- [ ] Use `enumerate()` to get both index and value
- [ ] Apply `break` and `continue` in loops
- [ ] Understand the `for-else` pattern

**Quick Check**: Can you write a program that iterates over a list of numbers and prints only the even ones, stopping when it finds a number greater than 20?

## 📊 Data Structures

### Lists

Ordered, mutable collections:

```python
numbers = [1, 2, 3, 4, 5]
mixed = [1, "hello", 3.14, True]  # Can hold different types

numbers.append(6)           # Add to end: [1, 2, 3, 4, 5, 6]
numbers.insert(0, 0)        # Insert at position: [0, 1, 2, 3, 4, 5, 6]
numbers.extend([7, 8, 9])   # Add multiple: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
numbers.remove(0)           # Remove first occurrence: [1, 2, 3, 4, 5, 6, 7, 8, 9]
popped = numbers.pop()      # Remove and return last: 9
popped_at = numbers.pop(0)  # Remove at index: 1

first = numbers[0]          # 2
last = numbers[-1]          # 8
slice_nums = numbers[1:4]   # [3, 4, 5] (from index 1 to 3)
every_second = numbers[::2] # [2, 4, 6, 8] (every 2nd element)
reversed_nums = numbers[::-1]  # [8, 7, 6, 5, 4, 3, 2] (reversed)

squares = [x**2 for x in range(10)]  # [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
even_squares = [x**2 for x in range(10) if x % 2 == 0]  # [0, 4, 16, 36, 64]

print(len(numbers))         # 7 (length)
print(max(numbers))         # 8 (maximum)
print(min(numbers))         # 2 (minimum)
print(sum(numbers))         # 35 (sum of all elements)
print(sorted(numbers))      # [2, 3, 4, 5, 6, 7, 8] (sorted copy)
numbers.sort()              # Sort in place
numbers.reverse()           # Reverse in place
```

### Tuples

Ordered, **immutable** collections:

```python
coordinates = (10, 20)
person = ("Alice", 30, "Jakarta")

x, y = coordinates
name, age, city = person

single = (42,)  # With comma: tuple
not_tuple = (42)  # Without comma: int


def get_dimensions():
    return 100, 200  # Returns tuple

width, height = get_dimensions()

locations = {
    (0, 0): "Origin",
    (10, 20): "Point A"
}
```

### Sets

Unordered, unique collections:

```python
numbers = {1, 2, 3, 4, 5}
unique = {1, 2, 2, 3, 3, 3}  # {1, 2, 3} (duplicates removed)

nums = [1, 2, 2, 3, 3, 3]
unique_nums = set(nums)  # {1, 2, 3}

a = {1, 2, 3, 4}
b = {3, 4, 5, 6}

print(a | b)  # {1, 2, 3, 4, 5, 6} (union)
print(a & b)  # {3, 4} (intersection)
print(a - b)  # {1, 2} (difference)
print(a ^ b)  # {1, 2, 5, 6} (symmetric difference)

numbers.add(6)          # Add element
numbers.remove(1)       # Remove (raises KeyError if not found)
numbers.discard(10)     # Remove (no error if not found)
numbers.clear()         # Remove all elements
```

### Dictionaries

Key-value pairs (like maps/hashmaps):

```python
person = {
    "name": "Alice",
    "age": 30,
    "city": "Jakarta"
}

scores = {"Alice": 90, "Bob": 85, "Carol": 92}

print(person["name"])       # "Alice"
print(person.get("age"))    # 30 (safe access)
print(person.get("email", "N/A"))  # "N/A" (default value)

person["email"] = "alice@example.com"
person["age"] = 31

del person["city"]
removed = person.pop("email")  # Remove and return value

keys = person.keys()          # dict_keys(['name', 'age'])
values = person.values()      # dict_values(['Alice', 31])
items = person.items()        # dict_items([('name', 'Alice'), ('age', 31)])

for key, value in person.items():
    print(f"{key}: {value}")

squares_dict = {x: x**2 for x in range(5)}

if "name" in person:
    print("Name exists")

defaults = {"theme": "dark", "lang": "en"}
user_prefs = {"lang": "id"}
merged = defaults | user_prefs  # {'theme': 'dark', 'lang': 'id'}
```

## 🔧 Functions

### Basic Functions

```python
def greet(name):
    print(f"Hello, {name}")

def add(a, b):
    return a + b

def multiply(a: int, b: int) -> int:
    return a * b

def power(base, exponent=2):
    return base ** exponent

def describe_person(name, age, city="Unknown"):
    print(f"{name}, {age} years old, from {city}")

describe_person("Alice", 30)                    # city="Unknown"
describe_person("Bob", 25, city="Jakarta")      # Using keyword
describe_person(age=28, name="Carol")           # Keywords, different order

def divide(a, b):
    quotient = a // b
    remainder = a % b
    return quotient, remainder

q, r = divide(10, 3)
print(q, r)  # 3 1

def sum_all(*numbers):
    return sum(numbers)

print(sum_all(1, 2, 3, 4, 5))  # 15

def print_info(**kwargs):
    for key, value in kwargs.items():
        print(f"{key}: {value}")

print_info(name="Alice", age=30, city="Jakarta")
```

### Lambda Functions

Anonymous, one-line functions:

```python
square = lambda x: x ** 2
print(square(5))  # 25

add = lambda a, b: a + b
print(add(3, 4))  # 7

numbers = [1, 2, 3, 4, 5]

squared = list(map(lambda x: x**2, numbers))

evens = list(filter(lambda x: x % 2 == 0, numbers))

people = [
    {"name": "Alice", "age": 30},
    {"name": "Bob", "age": 25},
    {"name": "Carol", "age": 35}
]
sorted_by_age = sorted(people, key=lambda p: p["age"])
```

### ✅ Checkpoint: Functions

Before moving forward, ensure you can:

- [ ] Define functions with and without return values
- [ ] Use default and keyword arguments
- [ ] Add type hints to function signatures
- [ ] Use `*args` and `**kwargs` for variable arguments
- [ ] Write lambda functions for simple operations
- [ ] Apply `map()`, `filter()`, and `sorted()` with lambdas

**Quick Check**: Can you write a function that takes a list of dictionaries (each with "name" and "score" keys) and returns a sorted list by score (highest first)?

## 🎭 Object-Oriented Programming

Object-oriented programming is central to Python. For deeper coverage, see the [Intermediate tutorial](/en/learn/software-engineering/programming-languages/python/tutorials/by-concept/intermediate) for decorators, metaclasses, and design patterns, or check [Work with classes effectively](/en/learn/software-engineering/programming-languages/python/how-to/work-with-classes-effectively) for practical patterns.

### Classes and Objects

```python
class Person:
    # Class variable (shared by all instances)
    species = "Homo sapiens"

    # Constructor (__init__ is the initializer)
    def __init__(self, name, age):
        # Instance variables (unique to each instance)
        self.name = name
        self.age = age

    # Instance method
    def greet(self):
        print(f"Hello, I'm {self.name}")

    # Method with parameters
    def have_birthday(self):
        self.age += 1
        print(f"{self.name} is now {self.age} years old")

    # String representation
    def __str__(self):
        return f"Person(name={self.name}, age={self.age})"

alice = Person("Alice", 30)
bob = Person("Bob", 25)

alice.greet()           # Hello, I'm Alice
alice.have_birthday()   # Alice is now 31 years old

print(alice.name)       # Alice
print(alice.species)    # Homo sapiens
print(alice)            # Person(name=Alice, age=31)
```

### Inheritance

```python
class Animal:
    def __init__(self, name):
        self.name = name

    def speak(self):
        pass  # Abstract method (to be overridden)

class Dog(Animal):
    def __init__(self, name, breed):
        super().__init__(name)  # Call parent constructor
        self.breed = breed

    def speak(self):
        return f"{self.name} says Woof!"

class Cat(Animal):
    def __init__(self, name, color):
        super().__init__(name)
        self.color = color

    def speak(self):
        return f"{self.name} says Meow!"

dog = Dog("Buddy", "Golden Retriever")
cat = Cat("Whiskers", "Orange")

print(dog.speak())  # Buddy says Woof!
print(cat.speak())  # Whiskers says Meow!

animals = [dog, cat]
for animal in animals:
    print(animal.speak())
```

### Encapsulation

```python
class BankAccount:
    def __init__(self, owner, balance=0):
        self.owner = owner
        self.__balance = balance  # Private attribute (name mangling)

    def deposit(self, amount):
        if amount > 0:
            self.__balance += amount
            print(f"Deposited ${amount}. New balance: ${self.__balance}")
        else:
            print("Invalid deposit amount")

    def withdraw(self, amount):
        if 0 < amount <= self.__balance:
            self.__balance -= amount
            print(f"Withdrew ${amount}. New balance: ${self.__balance}")
        else:
            print("Insufficient funds or invalid amount")

    def get_balance(self):
        return self.__balance

account = BankAccount("Alice", 1000)
account.deposit(500)    # Deposited $500. New balance: $1500
account.withdraw(200)   # Withdrew $200. New balance: $1300
print(account.get_balance())  # 1300

```

### Properties

```python
class Temperature:
    def __init__(self, celsius):
        self._celsius = celsius

    # Getter
    @property
    def celsius(self):
        return self._celsius

    # Setter
    @celsius.setter
    def celsius(self, value):
        if value < -273.15:
            raise ValueError("Temperature below absolute zero!")
        self._celsius = value

    # Property with conversion
    @property
    def fahrenheit(self):
        return self._celsius * 9/5 + 32

    @fahrenheit.setter
    def fahrenheit(self, value):
        self.celsius = (value - 32) * 5/9

temp = Temperature(25)
print(temp.celsius)      # 25
print(temp.fahrenheit)   # 77.0

temp.fahrenheit = 86     # Set via Fahrenheit
print(temp.celsius)      # 30.0

```

### Class Methods and Static Methods

```python
class MathUtils:
    PI = 3.14159

    @staticmethod
    def add(a, b):
        """Static method - doesn't need class or instance"""
        return a + b

    @classmethod
    def circle_area(cls, radius):
        """Class method - receives class as first argument"""
        return cls.PI * radius ** 2

print(MathUtils.add(5, 3))           # 8
print(MathUtils.circle_area(10))     # 314.159
```

## ⚠️ Exception Handling

### Try/Except

```python
try:
    number = int("abc")
except ValueError as e:
    print(f"Error: {e}")
    # Output: Error: invalid literal for int() with base 10: 'abc'

try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero")
except ValueError:
    print("Invalid value")
except Exception as e:
    print(f"Unexpected error: {e}")

try:
    file = open("data.txt", "r")
except FileNotFoundError:
    print("File not found")
else:
    # Runs if no exception
    content = file.read()
    print(content)
finally:
    # Always runs (cleanup)
    if 'file' in locals() and not file.closed:
        file.close()
```

### Raising Exceptions

```python
def validate_age(age):
    if age < 0:
        raise ValueError("Age cannot be negative")
    if age > 150:
        raise ValueError("Age is unrealistic")
    return True

try:
    validate_age(-5)
except ValueError as e:
    print(f"Validation error: {e}")
    # Output: Validation error: Age cannot be negative

class InsufficientFundsError(Exception):
    """Raised when withdrawal exceeds balance"""
    pass

def withdraw(balance, amount):
    if amount > balance:
        raise InsufficientFundsError(
            f"Cannot withdraw ${amount}, balance is ${balance}"
        )
    return balance - amount

try:
    new_balance = withdraw(100, 150)
except InsufficientFundsError as e:
    print(f"Transaction failed: {e}")
```

### Context Managers

```python
with open("data.txt", "w") as file:
    file.write("Hello, World!")

with open("input.txt", "r") as infile, open("output.txt", "w") as outfile:
    content = infile.read()
    outfile.write(content.upper())

class Timer:
    def __enter__(self):
        import time
        self.start = time.time()
        return self

    def __exit__(self, *args):
        import time
        self.end = time.time()
        print(f"Elapsed: {self.end - self.start:.2f}s")

with Timer():
    # Some operation
    sum(range(1000000))
```

## 📁 File I/O

### Reading Files

```python
with open("data.txt", "r") as file:
    content = file.read()
    print(content)

with open("data.txt", "r") as file:
    for line in file:
        print(line.strip())  # Remove newline

with open("data.txt", "r") as file:
    lines = file.readlines()

with open("data.txt", "r", encoding="utf-8") as file:
    content = file.read()
```

### Writing Files

```python
with open("output.txt", "w") as file:
    file.write("Hello, World!\n")
    file.write("Second line\n")

with open("output.txt", "a") as file:
    file.write("Appended line\n")

lines = ["Line 1\n", "Line 2\n", "Line 3\n"]
with open("output.txt", "w") as file:
    file.writelines(lines)
```

### Working with JSON

```python
import json

data = {
    "name": "Alice",
    "age": 30,
    "skills": ["Python", "JavaScript", "SQL"]
}

with open("data.json", "w") as file:
    json.dump(data, file, indent=2)

json_string = json.dumps(data, indent=2)
print(json_string)

with open("data.json", "r") as file:
    loaded_data = json.load(file)

data_from_string = json.loads(json_string)
```

### Working with CSV

```python
import csv

data = [
    ["Name", "Age", "City"],
    ["Alice", 30, "Jakarta"],
    ["Bob", 25, "Bandung"]
]

with open("people.csv", "w", newline="") as file:
    writer = csv.writer(file)
    writer.writerows(data)

with open("people.csv", "r") as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)

with open("people.csv", "w", newline="") as file:
    fieldnames = ["Name", "Age", "City"]
    writer = csv.DictWriter(file, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerow({"Name": "Alice", "Age": 30, "City": "Jakarta"})
```

## 📦 Modules and Packages

### Creating Modules

Create `mymath.py`:

```python
def add(a, b):
    return a + b

def multiply(a, b):
    return a * b

PI = 3.14159
```

Use in another file:

```python
import mymath

result = mymath.add(5, 3)
print(result)  # 8
print(mymath.PI)  # 3.14159

from mymath import add, PI
result = add(10, 5)
print(result)  # 15

import mymath as mm
result = mm.multiply(4, 5)
```

### Standard Library Modules

```python
from datetime import datetime, timedelta

now = datetime.now()
print(now)  # 2025-12-17 15:00:24.123456

tomorrow = now + timedelta(days=1)
formatted = now.strftime("%Y-%m-%d %H:%M:%S")

import random

print(random.randint(1, 10))           # Random int between 1 and 10
print(random.choice(["a", "b", "c"]))  # Random choice
print(random.random())                  # Random float between 0 and 1

import os

print(os.getcwd())                 # Current directory
os.makedirs("new_folder", exist_ok=True)
print(os.listdir("."))             # List directory contents

from pathlib import Path

path = Path("data/file.txt")
print(path.exists())               # Check if exists
print(path.parent)                 # Parent directory
print(path.name)                   # File name
```

## 🧪 Virtual Environments and Package Management

### Virtual Environments

```bash
python -m venv venv

venv\Scripts\activate

source venv/bin/activate

deactivate
```

### pip - Package Manager

```bash
pip install requests

pip install requests==2.28.0

pip install -r requirements.txt

pip list

pip show requests

pip uninstall requests

pip freeze > requirements.txt
```

## 🧪 Testing Basics

Testing is essential for reliable software. For comprehensive testing strategies, see [Write effective tests](/en/learn/software-engineering/programming-languages/python/how-to/write-effective-tests) and the [Intermediate tutorial](/en/learn/software-engineering/programming-languages/python/tutorials/by-concept/intermediate).

### Writing Tests with pytest

Install pytest:

```bash
pip install pytest
```

Create `calculator.py`:

```python
def add(a, b):
    return a + b

def divide(a, b):
    if b == 0:
        raise ValueError("Cannot divide by zero")
    return a / b
```

Create `test_calculator.py`:

```python
import pytest
from calculator import add, divide

def test_add():
    assert add(2, 3) == 5
    assert add(-1, 1) == 0
    assert add(0, 0) == 0

def test_divide():
    assert divide(10, 2) == 5
    assert divide(9, 3) == 3

def test_divide_by_zero():
    with pytest.raises(ValueError):
        divide(10, 0)
```

Run tests:

```bash
pytest test_calculator.py
pytest
```

## 🎯 Practice Exercises

### Level 1: Basics (Beginner)

**Exercise 1**: FizzBuzz

Write a program that prints numbers from 1 to 100. For multiples of 3, print "Fizz"; for multiples of 5, print "Buzz"; for multiples of both, print "FizzBuzz".

**Exercise 2**: Palindrome Checker

Write a function `is_palindrome(text)` that returns `True` if the text is a palindrome (reads the same forwards and backwards), ignoring spaces and case.

### Level 2: Data Structures

**Exercise 3**: Word Counter

Write a function that takes a string and returns a dictionary with word frequencies.

```python
text = "hello world hello"
```

**Exercise 4**: List Flattener

Write a function that flattens a nested list.

```python
nested = [1, [2, 3], [4, [5, 6]], 7]
```

### Level 3: Object-Oriented

**Exercise 5**: Shopping Cart

Create a `ShoppingCart` class with methods to add items, remove items, calculate total, and apply discounts.

**Exercise 6**: Bank System

Create a `BankAccount` class hierarchy with `SavingsAccount` and `CheckingAccount` subclasses, each with different interest rates and withdrawal rules.

### Level 4: Real-World

**Exercise 7**: CSV Data Analyzer

Write a program that reads a CSV file with sales data and calculates:

- Total sales
- Average sale amount
- Best-selling product
- Sales by month

**Exercise 8**: Simple API Client

Create a class that fetches data from a public API (e.g., JSONPlaceholder), parses the response, and provides methods to query the data.

## 📚 Common Patterns

### EAFP vs LBYL

```python
try:
    value = my_dict["key"]
except KeyError:
    value = "default"

if "key" in my_dict:
    value = my_dict["key"]
else:
    value = "default"
```

### List Comprehensions vs Loops

```python
squares = []
for i in range(10):
    squares.append(i ** 2)

squares = [i ** 2 for i in range(10)]
```

### Enumerate Instead of Range(len())

```python
items = ["a", "b", "c"]

for i in range(len(items)):
    print(i, items[i])

for i, item in enumerate(items):
    print(i, item)
```

### Dictionary get() with Default

```python
if "key" in my_dict:
    value = my_dict["key"]
else:
    value = "default"

value = my_dict.get("key", "default")
```

## 🎉 Summary

Congratulations! You've completed the Python Beginner tutorial. You've learned:

- ✅ Python syntax and core language features
- ✅ Data types, functions, and control flow
- ✅ Lists, dictionaries, sets, and tuples
- ✅ Object-oriented programming with classes and inheritance
- ✅ Exception handling and context managers
- ✅ File I/O and working with JSON/CSV
- ✅ Modules, packages, and virtual environments
- ✅ Testing basics with pytest
- ✅ Common Python patterns and idioms

## 📚 What's Next?

**Intermediate Tutorial**: [Python Intermediate](/en/learn/software-engineering/programming-languages/python/tutorials/by-concept/intermediate)

- Advanced OOP (decorators, metaclasses, descriptors)
- Concurrency and parallelism (threading, multiprocessing, asyncio)
- Performance optimization and profiling
- Working with databases
- Building REST APIs with Flask/FastAPI
- Production-ready patterns

**Practical Recipes**: [Python Cookbook](/en/learn/software-engineering/programming-languages/python/how-to/cookbook)

- 30+ copy-paste-modify solutions
- Real-world problem solving
- Quick reference for common tasks

**How-To Guides**: [Python How-To Guides](/en/learn/software-engineering/programming-languages/python/how-to/overview)

- Focused tutorials on specific techniques
- Best practices and patterns
- Deep dives into Python features

**Advanced Tutorial**: [Python Advanced](/en/learn/software-engineering/programming-languages/python/tutorials/by-concept/advanced)

- Python internals (GIL, memory management)
- Advanced metaprogramming
- C extensions and optimization
- System design patterns

**Best Practices**: [Python Best Practices](/en/learn/software-engineering/programming-languages/python/explanation/best-practices) and [Anti-Patterns](/en/learn/software-engineering/programming-languages/python/explanation/anti-patterns)

- Industry-standard coding conventions
- What to do and what to avoid
- Performance and maintainability tips

## 🆘 Troubleshooting

**Problem**: IndentationError

**Solution**: Python requires consistent indentation (4 spaces standard). Mix of tabs and spaces causes errors.

**Problem**: NameError: name 'X' is not defined

**Solution**: Variable or function used before definition. Check spelling and order of definitions.

**Problem**: TypeError: unsupported operand type(s)

**Solution**: Operation not supported for the types involved (e.g., adding string and int). Use type conversion if needed.

**Problem**: Import errors in modules

**Solution**: Ensure files are in the same directory or use proper package structure. Check for circular imports.

---

**Still stuck?** Visit [Python Documentation](https://docs.python.org/) or [Python Community Forums](https://discuss.python.org/). Also check our [Python Resources](/en/learn/software-engineering/programming-languages/python/reference/resources) for more learning materials and [Python Glossary](/en/learn/software-engineering/programming-languages/python/reference/glossary) for term definitions.
