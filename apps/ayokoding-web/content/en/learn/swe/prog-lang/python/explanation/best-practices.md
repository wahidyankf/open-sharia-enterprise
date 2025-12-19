---
title: "Python Best Practices"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 703
description: "Essential Python best practices and Pythonic idioms for writing maintainable code"
tags: ["python", "best-practices", "pythonic", "code-quality"]
---

## Overview

Writing Pythonic code means embracing the language's philosophy and idioms rather than importing patterns from other languages. Python's design prioritizes readability, explicitness, and simplicity. These best practices emerge from decades of community experience and align with "The Zen of Python" (PEP 20).

## Core Pythonic Principles

### EAFP Over LBYL

Easier to Ask for Forgiveness than Permission (EAFP) embodies Python's pragmatic approach. Try operations and handle exceptions rather than checking conditions upfront.

**Why it matters:**

- More readable by focusing on the happy path
- Avoids race conditions in concurrent code
- Handles edge cases you might not anticipate
- Aligns with duck typing philosophy

**Example:**

```python
# ❌ LBYL - Look Before You Leap (defensive)
def get_user_age(user_data):
    if user_data is not None:
        if 'age' in user_data:
            if isinstance(user_data['age'], int):
                if user_data['age'] >= 0:
                    return user_data['age']
    return None

# ✅ EAFP - Easier to Ask for Forgiveness than Permission
def get_user_age(user_data):
    try:
        age = user_data['age']
        if age < 0:
            raise ValueError("Age cannot be negative")
        return age
    except (KeyError, TypeError):
        return None
```

**Trade-offs:**

- EAFP is faster when exceptions are rare (the common case)
- LBYL makes sense when exceptions are expensive in tight loops
- Use EAFP as default, LBYL only with profiling evidence

### Duck Typing and Protocols

Focus on what an object can do, not what type it is. Python's dynamic typing enables flexible interfaces.

**Why it matters:**

- Decouples code from concrete types
- Enables polymorphism without inheritance
- Makes code more reusable and testable
- Supports composition naturally

**Example:**

```python
# ❌ Type checking restricts flexibility
def save_data(database: MySQLDatabase, data: dict):
    database.save(data)

# Only works with MySQLDatabase
save_data(mysql_db, data)  # OK
save_data(postgres_db, data)  # Type error!

# ✅ Duck typing - accepts any object with save method
def save_data(database, data):
    """Save data to any database that implements save()."""
    database.save(data)

# Works with any database
save_data(mysql_db, data)
save_data(postgres_db, data)
save_data(mock_db, data)  # Easy testing

# ✅ Protocol (Python 3.8+) - explicit interface without inheritance
from typing import Protocol

class Saveable(Protocol):
    def save(self, data: dict) -> None:
        ...

def save_data(database: Saveable, data: dict):
    database.save(data)
```

### Use Type Hints for Clarity

Modern Python embraces optional static typing for better tooling and documentation.

**Why it matters:**

- Enables static analysis and IDE support
- Documents expected types better than comments
- Catches type errors before runtime
- Supports gradual typing (add hints incrementally)

**Example:**

```python
# ❌ No type hints - unclear expectations
def process_order(order, discount, notify):
    total = calculate_total(order)
    final = apply_discount(total, discount)
    if notify:
        send_email(order)
    return final

# ✅ Type hints clarify expectations
from typing import Optional

def process_order(
    order: dict,
    discount: float,
    notify: bool = True
) -> float:
    total = calculate_total(order)
    final = apply_discount(total, discount)
    if notify:
        send_email(order)
    return final

# ✅ Better - domain types with type hints
from dataclasses import dataclass

@dataclass
class Order:
    items: list[str]
    total: float

def process_order(
    order: Order,
    discount: float = 0.0,
    notify: bool = True
) -> float:
    final = order.total * (1 - discount)
    if notify:
        send_email(order)
    return final
```

**When to use type hints:**

- Public APIs and interfaces (always)
- Complex functions with multiple parameters
- Functions returning Optional or Union types
- Data structures and domain models

## Pythonic Idioms

### Comprehensions Over Loops

List, dict, and set comprehensions express transformations concisely and efficiently.

**Why it matters:**

- More readable than equivalent loops
- Often faster (optimized at C level)
- Clearly expresses intent
- Naturally produces new collections

**Example:**

```python
# ❌ Verbose loops
squares = []
for x in range(10):
    squares.append(x ** 2)

active_users = {}
for user in users:
    if user.is_active:
        active_users[user.id] = user

# ✅ List comprehension
squares = [x ** 2 for x in range(10)]

# ✅ Dict comprehension
active_users = {
    user.id: user
    for user in users
    if user.is_active
}

# ✅ Set comprehension (for uniqueness)
unique_domains = {
    email.split('@')[1]
    for email in email_list
}

# ✅ Generator expression (memory efficient)
total = sum(x ** 2 for x in range(1_000_000))  # No intermediate list
```

**When to use loops instead:**

- Complex logic that doesn't fit cleanly
- Multiple operations per iteration
- Breaking early based on condition
- Side effects like printing or saving

### Context Managers for Resources

Use `with` statements to manage resources automatically.

**Why it matters:**

- Guarantees cleanup even with exceptions
- More readable than try/finally
- Prevents resource leaks
- Handles complex cleanup logic

**Example:**

```python
# ❌ Manual cleanup - error prone
def read_config():
    f = open('config.json')
    try:
        data = json.load(f)
        return data
    finally:
        f.close()  # Might forget this

# ✅ Context manager handles cleanup
def read_config():
    with open('config.json') as f:
        return json.load(f)

# ✅ Multiple resources
def copy_file(src, dest):
    with open(src, 'rb') as source, open(dest, 'wb') as target:
        target.write(source.read())

# ✅ Custom context manager
from contextlib import contextmanager
import time

@contextmanager
def timer(name):
    start = time.time()
    try:
        yield
    finally:
        print(f"{name} took {time.time() - start:.2f}s")

with timer("Database query"):
    results = database.query("SELECT * FROM users")
```

### F-Strings for String Formatting

F-strings (formatted string literals) provide the clearest string interpolation.

**Why it matters:**

- More readable than % formatting or .format()
- Faster than alternatives
- Supports expressions directly
- Type-safe with proper IDE support

**Example:**

```python
name = "Alice"
age = 30
balance = 1234.5678

# ❌ Old-style % formatting
msg = "Hello %s, you are %d years old" % (name, age)

# ❌ str.format() - verbose
msg = "Hello {}, you are {} years old".format(name, age)

# ✅ F-strings - clear and concise
msg = f"Hello {name}, you are {age} years old"

# ✅ Expressions and formatting
msg = f"Balance: ${balance:.2f}"  # Balance: $1234.57
msg = f"In 5 years: {age + 5}"     # In 5 years: 35

# ✅ Multi-line f-strings
report = f"""
User Report:
  Name: {name}
  Age: {age}
  Status: {'Active' if balance > 0 else 'Inactive'}
"""

# ✅ Debugging with = (Python 3.8+)
print(f"{name=}, {age=}")  # name='Alice', age=30
```

### Decorators for Cross-Cutting Concerns

Decorators modify function behavior without changing function code.

**Why it matters:**

- Separates concerns cleanly
- Reusable across functions
- Preserves original function
- Enables powerful patterns (caching, logging, validation)

**Example:**

```python
import functools
import time

# ✅ Timing decorator
def timer(func):
    @functools.wraps(func)  # Preserves func metadata
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        print(f"{func.__name__} took {time.time() - start:.2f}s")
        return result
    return wrapper

@timer
def fetch_data():
    time.sleep(2)
    return {"data": "loaded"}

# ✅ Caching decorator (stdlib)
from functools import lru_cache

@lru_cache(maxsize=128)
def fibonacci(n):
    if n < 2:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

# ✅ Validation decorator
def validate_positive(func):
    @functools.wraps(func)
    def wrapper(x):
        if x <= 0:
            raise ValueError("Must be positive")
        return func(x)
    return wrapper

@validate_positive
def calculate_sqrt(x):
    return x ** 0.5
```

## Data Structures

### Use Dataclasses for Data Containers

Dataclasses reduce boilerplate for classes that primarily hold data.

**Why it matters:**

- Automatic **init**, **repr**, **eq**
- Less code to maintain
- Clear intent (this is a data container)
- Supports type hints naturally

**Example:**

```python
# ❌ Manual class - boilerplate heavy
class User:
    def __init__(self, id, name, email, age):
        self.id = id
        self.name = name
        self.email = email
        self.age = age

    def __repr__(self):
        return f"User(id={self.id}, name={self.name}, ...)"

    def __eq__(self, other):
        if not isinstance(other, User):
            return False
        return (self.id == other.id and
                self.name == other.name and ...)

# ✅ Dataclass - concise and clear
from dataclasses import dataclass

@dataclass
class User:
    id: int
    name: str
    email: str
    age: int

# Gets __init__, __repr__, __eq__ automatically
user = User(1, "Alice", "alice@example.com", 30)
print(user)  # User(id=1, name='Alice', email='alice@example.com', age=30)

# ✅ With defaults and validation
from dataclasses import dataclass, field

@dataclass
class Product:
    name: str
    price: float
    quantity: int = 0
    tags: list[str] = field(default_factory=list)  # Avoid mutable default

    def __post_init__(self):
        if self.price < 0:
            raise ValueError("Price cannot be negative")
```

### Prefer Named Tuples for Immutable Data

Named tuples provide lightweight immutable containers with named fields.

**Why it matters:**

- Immutable by default (thread-safe)
- Less memory than classes
- Named fields improve readability
- Backwards compatible with regular tuples

**Example:**

```python
from typing import NamedTuple

# ✅ Named tuple for simple data
class Point(NamedTuple):
    x: float
    y: float

    def distance(self):
        return (self.x ** 2 + self.y ** 2) ** 0.5

p = Point(3.0, 4.0)
print(p.x, p.y)  # Named access
print(p[0], p[1])  # Also supports indexing
print(p.distance())  # 5.0

# ✅ Can be unpacked
x, y = p

# ❌ Cannot modify (immutable)
# p.x = 5  # AttributeError
```

**When to use dataclass vs namedtuple:**

- Dataclass: Mutable data, methods, inheritance
- NamedTuple: Immutable data, simple structure

## Virtual Environments and Dependencies

### Always Use Virtual Environments

Isolate project dependencies to avoid conflicts and ensure reproducibility.

**Why it matters:**

- Prevents version conflicts between projects
- Makes dependencies explicit and portable
- Matches production environment
- Enables reproducible builds

**Example:**

```bash
# ✅ Create virtual environment
python -m venv venv

# ✅ Activate (Unix/macOS)
source venv/bin/activate

# ✅ Activate (Windows)
venv\Scripts\activate

# ✅ Install dependencies
pip install -r requirements.txt

# ✅ Freeze exact versions
pip freeze > requirements.txt
```

### Pin Dependencies with Lock Files

Lock files ensure reproducible installations across environments.

**Example:**

```python
# requirements.txt - specify ranges
requests>=2.28.0,<3.0.0
flask>=2.3.0

# requirements-lock.txt - exact versions from pip freeze
requests==2.31.0
certifi==2023.7.22
charset-normalizer==3.2.0
flask==2.3.3
# ... all transitive dependencies

# ✅ Development dependencies separate
# requirements-dev.txt
-r requirements.txt
pytest>=7.4.0
black>=23.0.0
mypy>=1.5.0
```

## Code Organization

### Keep Functions Small and Focused

Each function should do one thing well.

**Why it matters:**

- Easier to test in isolation
- Easier to understand and debug
- Promotes reuse
- Follows Single Responsibility Principle

**Example:**

```python
# ❌ Large function doing too much
def process_order(order_data):
    # Validate
    if not order_data.get('items'):
        raise ValueError("Empty order")

    # Calculate total
    total = 0
    for item in order_data['items']:
        total += item['price'] * item['quantity']

    # Apply discount
    if order_data.get('discount_code'):
        discount = lookup_discount(order_data['discount_code'])
        total *= (1 - discount)

    # Save to database
    db.save_order({
        'items': order_data['items'],
        'total': total,
        'customer': order_data['customer']
    })

    # Send email
    send_email(
        to=order_data['customer']['email'],
        subject="Order Confirmation",
        body=f"Total: ${total:.2f}"
    )

# ✅ Small, focused functions
def validate_order(order_data):
    if not order_data.get('items'):
        raise ValueError("Empty order")

def calculate_total(items):
    return sum(
        item['price'] * item['quantity']
        for item in items
    )

def apply_discount(total, discount_code):
    if not discount_code:
        return total
    discount = lookup_discount(discount_code)
    return total * (1 - discount)

def save_order(order_data, total):
    db.save_order({
        'items': order_data['items'],
        'total': total,
        'customer': order_data['customer']
    })

def send_confirmation(email, total):
    send_email(
        to=email,
        subject="Order Confirmation",
        body=f"Total: ${total:.2f}"
    )

def process_order(order_data):
    validate_order(order_data)
    total = calculate_total(order_data['items'])
    total = apply_discount(total, order_data.get('discount_code'))
    save_order(order_data, total)
    send_confirmation(order_data['customer']['email'], total)
```

### Use Properties for Computed Attributes

Properties provide attribute syntax for computed values.

**Why it matters:**

- Clean attribute access syntax
- Can add validation without API changes
- Lazy computation
- Backwards compatible with attributes

**Example:**

```python
# ❌ Getter methods break attribute syntax
class Rectangle:
    def __init__(self, width, height):
        self.width = width
        self.height = height

    def get_area(self):
        return self.width * self.height

    def set_width(self, value):
        if value <= 0:
            raise ValueError("Width must be positive")
        self.width = value

rect = Rectangle(10, 5)
print(rect.get_area())  # Method call syntax
rect.set_width(20)

# ✅ Properties provide attribute syntax
class Rectangle:
    def __init__(self, width, height):
        self._width = width
        self._height = height

    @property
    def width(self):
        return self._width

    @width.setter
    def width(self, value):
        if value <= 0:
            raise ValueError("Width must be positive")
        self._width = value

    @property
    def area(self):
        return self._width * self._height

rect = Rectangle(10, 5)
print(rect.area)  # Computed property, attribute syntax
rect.width = 20   # Validated assignment
```

## Design Philosophy

### The Zen of Python

Run `import this` in Python to see the guiding principles:

- **Beautiful is better than ugly** - Favor readable, elegant solutions
- **Explicit is better than implicit** - Make behavior clear, not magical
- **Simple is better than complex** - Choose simple solutions over complex ones
- **Flat is better than nested** - Avoid deep nesting
- **Readability counts** - Code is read more than written
- **Special cases aren't special enough to break the rules** - Follow conventions consistently
- **Errors should never pass silently** - Don't swallow exceptions
- **There should be one obvious way to do it** - Prefer the Pythonic way

### When to Break the Rules

Best practices are guidelines, not absolute laws. Break them when:

- **Performance critical sections**: Profiling shows a practice hurts performance
- **Compatibility requirements**: Need to match existing API or external system
- **Library constraints**: Third-party library requires specific pattern
- **Team consensus**: Team agrees on different approach with clear reasoning

Always document deviations with clear explanations.

## Summary

Pythonic code emerges from embracing the language's philosophy rather than importing patterns from other ecosystems. The EAFP principle focuses code on the happy path while handling exceptional cases gracefully through Python's exception system. Duck typing and protocols enable flexible interfaces that accept any object with the right behavior rather than the right inheritance hierarchy.

Type hints bridge the gap between Python's dynamic nature and the benefits of static analysis. They document expectations clearly while preserving the flexibility to gradually add types as code evolves. Modern Python development includes type hints not as a burden but as a tool for better IDE support and clearer interfaces.

Comprehensions, f-strings, context managers, and decorators represent Python's approach to common programming patterns. List comprehensions express transformations more clearly than equivalent loops. F-strings make string interpolation readable at a glance. Context managers ensure resources clean up reliably. Decorators separate cross-cutting concerns from core logic. These idioms don't just save keystrokes - they communicate intent more directly.

Dataclasses and named tuples reduce boilerplate for common data structures. Choose dataclasses when you need mutability and methods, named tuples for immutable data with simple structure. Both integrate naturally with type hints to create self-documenting data models.

Virtual environments and dependency management might seem like operational concerns, but they're fundamental to writing Python that works reliably across different environments. Isolate dependencies, pin versions precisely, and separate development from production requirements. This discipline prevents the "works on my machine" problem.

Small, focused functions compose into larger behaviors while remaining individually testable and comprehensible. Properties provide the clean syntax of attributes with the power to validate and compute values. These patterns make code easier to work with whether you're adding features, fixing bugs, or bringing new developers onto the project.

The Zen of Python isn't just philosophical - it provides practical guidance for everyday decisions. When you face a choice between clever and clear, choose clear. When you can make behavior explicit rather than implicit, be explicit. When simple and complex solutions both work, prefer simple. These principles compound over time into codebases that remain maintainable as they grow.

## Related Content

- [Common Python Anti-Patterns](/en/learn/swe/prog-lang/python/explanation/anti-patterns)
- [How to Write Pythonic Code](/en/learn/swe/prog-lang/python/how-to/write-pythonic-code)
- [How to Use Type Hints Effectively](/en/learn/swe/prog-lang/python/how-to/use-type-hints-effectively)
- [How to Avoid Common Pitfalls](/en/learn/swe/prog-lang/python/how-to/avoid-common-pitfalls)
