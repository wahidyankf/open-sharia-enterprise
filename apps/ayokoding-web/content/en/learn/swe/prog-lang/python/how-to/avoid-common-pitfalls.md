---
title: "How to Avoid Common Pitfalls"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 503
description: "Practical guide to recognizing and preventing frequent Python mistakes"
tags: ["python", "pitfalls", "debugging", "best-practices"]
categories: ["learn"]
---

## Problem

Python's flexibility and dynamic typing create traps that catch even experienced developers. These pitfalls stem from Python's design choices and differ significantly from patterns in statically-typed languages.

This guide shows how to recognize and prevent the most common Python pitfalls with working examples.

## Mutable Default Arguments Trap

Python evaluates default arguments once at function definition time, not at call time. Mutable defaults create shared state across calls.

### The Trap

```python
# ❌ Mutable default creates shared state
def add_user(user, users=[]):
    users.append(user)
    return users

print(add_user("Alice"))  # ['Alice']
print(add_user("Bob"))    # ['Alice', 'Bob'] - Unexpected!
print(add_user("Charlie")) # ['Alice', 'Bob', 'Charlie']

# The default list persists across all calls
```

### The Fix

```python
# ✅ Use None as sentinel value
def add_user(user, users=None):
    if users is None:
        users = []
    users.append(user)
    return users

print(add_user("Alice"))   # ['Alice']
print(add_user("Bob"))     # ['Bob'] - Fresh list
print(add_user("Charlie")) # ['Charlie']

# ✅ One-liner alternative
def add_user(user, users=None):
    users = users if users is not None else []
    users.append(user)
    return users

# ✅ Using dataclass with field factory
from dataclasses import dataclass, field

@dataclass
class Team:
    name: str
    members: list[str] = field(default_factory=list)

team1 = Team("Engineering")
team2 = Team("Design")
team1.members.append("Alice")
print(team2.members)  # [] - Separate lists
```

### Why It Happens

```python
def show_default(items=[]):
    print(f"ID: {id(items)}, Contents: {items}")
    items.append("new")

show_default()  # ID: 140234567, Contents: []
show_default()  # ID: 140234567, Contents: ['new'] - Same object!
show_default()  # ID: 140234567, Contents: ['new', 'new']

# The default list is created once and reused
```

## Late Binding Closure Gotcha

Python closures bind to variables, not values. In loops, this creates unexpected behavior.

### The Trap

```python
# ❌ All functions reference the same variable
def create_multipliers():
    funcs = []
    for i in range(5):
        funcs.append(lambda x: x * i)
    return funcs

multipliers = create_multipliers()
print(multipliers[0](2))  # Expected: 0, Actual: 8
print(multipliers[1](2))  # Expected: 2, Actual: 8
print(multipliers[2](2))  # Expected: 4, Actual: 8

# All functions use i=4 (final value from loop)
```

### The Fix

```python
# ✅ Bind value as default argument
def create_multipliers():
    funcs = []
    for i in range(5):
        funcs.append(lambda x, i=i: x * i)  # Capture current i
    return funcs

multipliers = create_multipliers()
print(multipliers[0](2))  # 0
print(multipliers[1](2))  # 2
print(multipliers[2](2))  # 4

# ✅ Use functools.partial
from functools import partial

def multiply(x, i):
    return x * i

def create_multipliers():
    return [partial(multiply, i=i) for i in range(5)]

# ✅ List comprehension (creates new scope)
def create_multipliers():
    return [lambda x, i=i: x * i for i in range(5)]
```

### Why It Happens

```python
# Closures capture variables, not values
x = 10
f = lambda: x
x = 20
print(f())  # 20, not 10

# Variables are looked up when function executes, not when defined
```

## Name Shadowing Built-ins

Python allows shadowing built-in names, creating confusing bugs.

### The Trap

```python
# ❌ Shadowing list built-in
def process_data():
    list = [1, 2, 3]  # Shadows built-in list!
    # Later in function...
    numbers = list(range(10))  # TypeError: 'list' object is not callable

# ❌ Shadowing in loops
for sum in [1, 2, 3]:  # Shadows built-in sum!
    print(sum)

total = sum([1, 2, 3])  # TypeError!

# ❌ Common shadows: list, dict, set, str, int, id, type, max, min, sum
```

### The Fix

```python
# ✅ Use descriptive names
def process_data():
    items = [1, 2, 3]  # Clear, doesn't shadow
    numbers = list(range(10))  # Works!

# ✅ Plural for collections
for value in [1, 2, 3]:
    print(value)

total = sum([1, 2, 3])  # Works!

# ✅ Check for built-in names
import builtins
name = "list"
print(hasattr(builtins, name))  # True - it's a built-in!
```

### Why It Happens

```python
# Python's namespace lookup: local -> enclosing -> global -> built-in
# Local variables shadow built-ins without warning

# ✅ Prevent shadowing with linters
# Configure pylint, flake8, or ruff to warn on built-in shadowing
```

## Iterator Exhaustion

Iterators can only be consumed once. Reusing them yields nothing.

### The Trap

```python
# ❌ Iterator exhaustion
numbers = (x ** 2 for x in range(5))  # Generator expression

# First use
print(list(numbers))  # [0, 1, 4, 9, 16]

# Second use
print(list(numbers))  # [] - Iterator exhausted!

# ❌ Similar issue with files
with open('data.txt') as f:
    lines = f  # File object is an iterator
    print(len(list(lines)))  # Works
    print(len(list(lines)))  # 0 - Exhausted!
```

### The Fix

```python
# ✅ Convert to list if needed multiple times
numbers = [x ** 2 for x in range(5)]  # List, not generator
print(numbers)  # [0, 1, 4, 9, 16]
print(numbers)  # [0, 1, 4, 9, 16] - Still available

# ✅ Re-create iterator when needed
def get_numbers():
    return (x ** 2 for x in range(5))

numbers1 = get_numbers()
numbers2 = get_numbers()
print(list(numbers1))  # [0, 1, 4, 9, 16]
print(list(numbers2))  # [0, 1, 4, 9, 16]

# ✅ For files, read once into memory
with open('data.txt') as f:
    lines = list(f)  # Load into list
print(len(lines))  # Works
print(len(lines))  # Still works

# ✅ Or use itertools.tee for splitting
from itertools import tee

numbers = (x ** 2 for x in range(5))
iter1, iter2 = tee(numbers, 2)
print(list(iter1))  # [0, 1, 4, 9, 16]
print(list(iter2))  # [0, 1, 4, 9, 16]
```

### Why It Happens

```python
# Generators and iterators maintain state
gen = (x for x in range(3))
print(next(gen))  # 0
print(next(gen))  # 1
print(next(gen))  # 2
print(next(gen))  # StopIteration - No more items

# Once exhausted, can't reset (unlike lists)
```

## Dictionary KeyError Handling

Accessing non-existent keys raises KeyError. Several patterns handle this safely.

### The Trap

```python
# ❌ Unhandled KeyError
user_data = {"name": "Alice", "age": 30}
email = user_data["email"]  # KeyError: 'email'

# ❌ LBYL approach - verbose
if "email" in user_data:
    email = user_data["email"]
else:
    email = None
```

### The Fix

```python
# ✅ Use get() with default
email = user_data.get("email")  # None if missing
email = user_data.get("email", "no-email@example.com")  # Custom default

# ✅ Use defaultdict for auto-initialization
from collections import defaultdict

# Auto-creates empty list for missing keys
user_posts = defaultdict(list)
user_posts["alice"].append("Post 1")  # No KeyError
user_posts["alice"].append("Post 2")
print(user_posts["bob"])  # [] - Auto-initialized

# ✅ Use setdefault for initialization
settings = {}
log_level = settings.setdefault("log_level", "INFO")  # Returns and sets default
print(settings)  # {"log_level": "INFO"}

# ✅ EAFP approach with try/except
try:
    email = user_data["email"]
except KeyError:
    email = None  # Or raise custom error with context
```

### When to Use Each

```python
# get() - Simple default value
email = config.get("email", "default@example.com")

# defaultdict - Multiple insertions for same key
from collections import defaultdict
word_counts = defaultdict(int)
for word in words:
    word_counts[word] += 1  # No KeyError even for new words

# setdefault - Initialize and use value
cache = {}
result = cache.setdefault(key, expensive_computation())

# try/except - Need different handling for missing key
try:
    value = config["required_key"]
except KeyError:
    raise ConfigurationError("Missing required_key in config")
```

## List/String Mutability Confusions

Lists are mutable, strings are immutable. This creates different behavior patterns.

### The Trap

```python
# ❌ Expecting string methods to mutate
text = "hello"
text.upper()  # Creates new string, doesn't modify text
print(text)  # Still "hello"

# ❌ Unexpected list mutation
def add_item(item, items):
    items.append(item)  # Modifies original list!
    return items

original = [1, 2, 3]
result = add_item(4, original)
print(original)  # [1, 2, 3, 4] - Modified!

# ❌ Aliasing confusion
list1 = [1, 2, 3]
list2 = list1  # Same object, not a copy
list2.append(4)
print(list1)  # [1, 2, 3, 4] - Also changed!
```

### The Fix

```python
# ✅ Strings require assignment
text = "hello"
text = text.upper()  # Assign result
print(text)  # "HELLO"

# ✅ Create copy to avoid mutation
def add_item(item, items):
    result = items.copy()  # or items[:]
    result.append(item)
    return result

original = [1, 2, 3]
result = add_item(4, original)
print(original)  # [1, 2, 3] - Unchanged
print(result)   # [1, 2, 3, 4]

# ✅ Deep copy for nested structures
import copy

nested = [[1, 2], [3, 4]]
shallow = nested.copy()  # Copies outer list only
deep = copy.deepcopy(nested)  # Copies everything

nested[0].append(99)
print(shallow)  # [[1, 2, 99], [3, 4]] - Inner lists shared
print(deep)     # [[1, 2], [3, 4]] - Completely separate

# ✅ Explicit about mutation
def add_item_inplace(item, items):
    """Mutates items in place."""
    items.append(item)

def add_item_copy(item, items):
    """Returns new list, original unchanged."""
    return items + [item]
```

## Integer Caching Surprises

Python caches small integers (-5 to 256). Identity checks can surprise.

### The Trap

```python
# ❌ Unexpected identity behavior
a = 256
b = 256
print(a is b)  # True - Cached

a = 257
b = 257
print(a is b)  # False - Not cached!

# ❌ Using 'is' for number comparison
x = 1000
y = 1000
if x is y:  # False - Don't use 'is' for numbers!
    print("Equal")
```

### The Fix

```python
# ✅ Use == for value comparison
a = 257
b = 257
print(a == b)  # True - Correct comparison

# ✅ Use 'is' only for identity checks
x = None
if x is None:  # Correct - Testing identity
    print("x is None")

y = []
z = y
if y is z:  # Correct - Testing if same object
    print("Same object")

# ✅ Understanding integer interning
# Python caches integers in range [-5, 256]
print(id(10) == id(10))  # True
print(id(300) == id(300))  # May be False

# Don't rely on integer caching behavior
```

### Why It Matters

```python
# Memory optimization vs correct comparison
# is: Tests object identity (same memory address)
# ==: Tests value equality

# ✅ Always use == for value comparison
# ✅ Use is only for None, True, False, or intentional identity checks
```

## Summary

Python's pitfalls mostly stem from its dynamic nature and design choices that prioritize flexibility. Mutable default arguments create shared state because defaults are evaluated once at function definition time - use None as a sentinel and initialize inside the function instead. Late binding in closures means loop variables are looked up when functions execute, not when they're created - capture values as default arguments or use comprehensions that create new scopes.

Shadowing built-in names creates confusing bugs because Python's namespace lookup puts local variables before built-ins. Use descriptive names and configure linters to warn about shadowing. Iterators and generators can only be consumed once - convert to lists if you need multiple passes, or re-create the iterator for each use.

Dictionary access with square brackets raises KeyError for missing keys. The get() method provides safe access with defaults, defaultdict automatically initializes missing keys, and setdefault initializes and returns in one operation. Choose based on your usage pattern - get() for simple defaults, defaultdict for repeated insertions, try/except when missing keys represent errors.

Lists mutate in place while strings are immutable and return new values. This difference creates aliasing issues where multiple variables point to the same mutable list. Create copies explicitly when you need to avoid mutation, and use deepcopy for nested structures. Integer caching in the range -5 to 256 makes identity checks with 'is' unreliable - always use == for value comparison and reserve 'is' for None checks and intentional identity tests.

These pitfalls share a pattern: behavior that makes sense given Python's design but differs from other languages or intuition. Understanding why they occur helps you avoid them and debug issues when they arise. Configure linters to catch common mistakes, use type hints for better tooling support, and follow Python's idioms rather than importing patterns from other languages.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [Common Python Anti-Patterns](/en/learn/swe/prog-lang/python/explanation/anti-patterns)
- [How to Use Collections Effectively](/en/learn/swe/prog-lang/python/how-to/use-collections-effectively)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/python/how-to/handle-errors-effectively)
