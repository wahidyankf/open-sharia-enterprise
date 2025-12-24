---
title: "Use Collections Effectively"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 1000006
description: "Choose and use Python collection types for optimal performance and clarity"
tags: ["python", "collections", "list", "dict", "set", "performance"]
categories: ["learn"]
---

## Problem

Python provides multiple collection types (list, tuple, dict, set) and specialized variants in the collections module. Choosing the wrong collection leads to poor performance or unnecessarily complex code.

This guide shows how to select and use Python collections effectively.

## Core Collection Types

### List - Ordered, Mutable Sequence

Lists maintain order and allow modifications.

```python

users = ["alice", "bob", "charlie"]
users.append("david")
users[0] = "ALICE"
users.sort()

numbers = [1, 2, 3, 4, 5]

numbers.append(6)

numbers.insert(0, 0)  # Insert at beginning

numbers.remove(3)

last = numbers.pop()

first = numbers.pop(0)

third = numbers[2]

subset = numbers[1:4]

numbers.sort()
numbers.sort(reverse=True)

squares = [x ** 2 for x in range(10)]
evens = [x for x in numbers if x % 2 == 0]
```

### Tuple - Ordered, Immutable Sequence

Tuples are like lists but immutable.

```python

point = (10, 20)
rgb = (255, 128, 0)

x, y = point
r, g, b = rgb

from typing import NamedTuple

class Point(NamedTuple):
    x: float
    y: float

p = Point(10.0, 20.0)
print(p.x, p.y)  # Named access
print(p[0], p[1])  # Also supports indexing

def get_user():
    return "alice", 30, "alice@example.com"  # Tuple

name, age, email = get_user()  # Unpacking
```

### Dict - Key-Value Mapping

Dictionaries map keys to values with O(1) lookup.

```python

user = {
    "id": 1,
    "name": "Alice",
    "email": "alice@example.com"
}

email = user.get("email")
phone = user.get("phone", "N/A")  # Default if missing

user["age"] = 30

if "email" in user:
    send_email(user["email"])

del user["age"]

for key in user:
    print(key)

for key, value in user.items():
    print(f"{key}: {value}")

word_lengths = {word: len(word) for word in ["hello", "world"]}
squared = {x: x ** 2 for x in range(5)}

defaults = {"color": "blue", "size": 10}
custom = {"size": 20}
merged = defaults | custom  # {"color": "blue", "size": 20}

defaults.update(custom)
```

### Set - Unordered, Unique Elements

Sets ensure uniqueness with fast membership testing.

```python

numbers = {1, 2, 3, 4, 5}
unique_words = set(["hello", "world", "hello"])  # {"hello", "world"}

numbers.add(6)

numbers.remove(3)  # Raises KeyError if not present
numbers.discard(3)  # No error if not present

if 5 in numbers:
    print("Found")

a = {1, 2, 3, 4}
b = {3, 4, 5, 6}

print(a | b)  # {1, 2, 3, 4, 5, 6}
print(a.union(b))

print(a & b)  # {3, 4}
print(a.intersection(b))

print(a - b)  # {1, 2}
print(a.difference(b))

print(a ^ b)  # {1, 2, 5, 6}
print(a.symmetric_difference(b))

even_squares = {x ** 2 for x in range(10) if x % 2 == 0}
```

## Performance Comparison

```python
import timeit

big_list = list(range(10000))
print(timeit.timeit(lambda: 9999 in big_list, number=1000))  # ~0.15s

big_set = set(range(10000))
print(timeit.timeit(lambda: 9999 in big_set, number=1000))   # ~0.00003s

```

## Specialized Collections

### defaultdict - Auto-Initialize Missing Keys

```python
from collections import defaultdict

items = [
    ("fruit", "apple"),
    ("vegetable", "carrot"),
    ("fruit", "banana"),
    ("vegetable", "lettuce")
]

groups = {}
for category, item in items:
    if category not in groups:
        groups[category] = []
    groups[category].append(item)

groups = defaultdict(list)
for category, item in items:
    groups[category].append(item)

print(groups)  # {"fruit": ["apple", "banana"], "vegetable": ["carrot", "lettuce"]}

word_counts = defaultdict(int)
for word in ["hello", "world", "hello"]:
    word_counts[word] += 1  # Auto-initializes to 0

print(word_counts)  # {"hello": 2, "world": 1}
```

### Counter - Count Hashable Objects

```python
from collections import Counter

words = ["apple", "banana", "apple", "cherry", "banana", "apple"]
counts = Counter(words)
print(counts)  # Counter({"apple": 3, "banana": 2, "cherry": 1})

print(counts.most_common(2))  # [("apple", 3), ("banana", 2)]

counter1 = Counter(["a", "b", "c"])
counter2 = Counter(["b", "c", "d"])
combined = counter1 + counter2  # Counter({"b": 2, "c": 2, "a": 1, "d": 1})

text = "hello world"
char_counts = Counter(text)
print(char_counts)  # Counter({"l": 3, "o": 2, "h": 1, ...})
```

### deque - Double-Ended Queue

```python
from collections import deque


queue = []
queue.append(1)
queue.append(2)
first = queue.pop(0)  # O(n) - shifts all elements

queue = deque()
queue.append(1)
queue.append(2)
first = queue.popleft()  # O(1)

recent = deque(maxlen=3)
recent.append(1)
recent.append(2)
recent.append(3)
recent.append(4)  # Automatically removes 1
print(recent)  # deque([2, 3, 4])

items = deque([1, 2, 3, 4, 5])
items.rotate(2)  # Rotate right
print(items)  # deque([4, 5, 1, 2, 3])
```

### namedtuple - Lightweight Data Classes

```python
from collections import namedtuple

Point = namedtuple("Point", ["x", "y"])
point = Point(10, 20)

print(point.x, point.y)  # Named access
print(point[0], point[1])  # Index access
x, y = point  # Unpacking

Point = namedtuple("Point", ["x", "y"], defaults=[0, 0])
origin = Point()  # Point(x=0, y=0)

data = {"name": "Alice", "age": 30}
User = namedtuple("User", data.keys())
user = User(**data)
print(user.name, user.age)
```

### OrderedDict - Preserve Insertion Order

```python
from collections import OrderedDict


class LRUCache:
    def __init__(self, capacity):
        self.cache = OrderedDict()
        self.capacity = capacity

    def get(self, key):
        if key not in self.cache:
            return None
        self.cache.move_to_end(key)  # Mark as recently used
        return self.cache[key]

    def put(self, key, value):
        if key in self.cache:
            self.cache.move_to_end(key)
        self.cache[key] = value
        if len(self.cache) > self.capacity:
            self.cache.popitem(last=False)  # Remove oldest
```

## Collection Patterns

### Remove While Iterating

```python
numbers = [1, 2, 3, 4, 5]
for num in numbers:
    if num % 2 == 0:
        numbers.remove(num)  # RuntimeError or wrong result

numbers = [1, 2, 3, 4, 5]
odd_numbers = [num for num in numbers if num % 2 != 0]

odd_numbers = list(filter(lambda x: x % 2 != 0, numbers))

numbers[:] = [num for num in numbers if num % 2 != 0]

data = {"a": 1, "b": 2, "c": 3}
to_remove = [k for k, v in data.items() if v % 2 == 0]
for key in to_remove:
    del data[key]
```

### Flatten Nested Collections

```python
nested = [[1, 2], [3, 4], [5, 6]]
flat = [item for sublist in nested for item in sublist]
print(flat)  # [1, 2, 3, 4, 5, 6]

from itertools import chain
flat = list(chain.from_iterable(nested))

def flatten(items):
    for item in items:
        if isinstance(item, list):
            yield from flatten(item)
        else:
            yield item

nested = [1, [2, 3], [[4, 5], 6]]
flat = list(flatten(nested))  # [1, 2, 3, 4, 5, 6]
```

### Group By Key

```python
from itertools import groupby
from operator import itemgetter

from collections import defaultdict

data = [
    {"category": "fruit", "name": "apple"},
    {"category": "vegetable", "name": "carrot"},
    {"category": "fruit", "name": "banana"}
]

groups = defaultdict(list)
for item in data:
    groups[item["category"]].append(item["name"])

data_sorted = sorted(data, key=itemgetter("category"))
groups = {k: list(v) for k, v in groupby(data_sorted, key=itemgetter("category"))}
```

## Memory Efficiency

### Generator Expressions vs List Comprehensions

```python
squares = [x ** 2 for x in range(1_000_000)]  # Uses ~8MB
total = sum(squares)

squares = (x ** 2 for x in range(1_000_000))  # Uses ~100 bytes
total = sum(squares)

def process_large_file(filename):
    # Generator - processes line by line
    with open(filename) as f:
        for line in f:  # File object is iterator
            yield process_line(line)

for result in process_large_file("huge.txt"):
    handle(result)
```

## Summary

Python's core collections serve distinct purposes based on order requirements, mutability, and uniqueness constraints. Lists maintain order and allow modifications, making them the default choice for sequences. Tuples provide immutable sequences ideal for constants and multiple return values. Dictionaries enable O(1) lookups by key for fast data retrieval. Sets guarantee uniqueness and provide O(1) membership testing along with mathematical set operations.

Performance characteristics guide collection choice. Lists excel at indexed access and appending to the end but perform poorly for membership tests and removals from the beginning. Sets provide constant-time membership testing, making them far superior to lists when you frequently check if items exist. Dictionaries offer constant-time key lookups but consume more memory than lists due to hashing overhead.

The collections module provides specialized variants for common patterns. defaultdict eliminates boilerplate for initializing missing keys, automatically creating default values. Counter simplifies counting occurrences with built-in aggregation methods. deque provides fast operations at both ends, making it superior to lists for queue implementations. namedtuple creates lightweight classes with named fields without the overhead of full class definitions.

List comprehensions and generator expressions transform collections concisely. Comprehensions create new lists, dicts, or sets in single expressions more clearly than equivalent loops. Generator expressions provide the same syntax but yield items one at a time, dramatically reducing memory usage for large datasets. Choose comprehensions when you need the whole result, generators when processing items individually.

Modifying collections during iteration requires care to avoid errors and unexpected behavior. List comprehensions create new collections from filtered or transformed elements without mutation issues. For in-place modification, assign the comprehension result to a slice. For sets and dicts, collect keys to remove first, then delete them in a separate loop.

The right collection type makes code both clearer and faster. Use sets for membership testing, dicts for key-based lookup, deques for queues, and defaultdict for grouping. These specialized collections express intent better than generic lists while providing better performance for their specific use cases.

## Related Content

- [Python Best Practices](/en/learn/software-engineering/programming-language/python/explanation/best-practices)
- [How to Avoid Common Pitfalls](/en/learn/software-engineering/programming-language/python/how-to/avoid-common-pitfalls)
- [How to Write Pythonic Code](/en/learn/software-engineering/programming-language/python/how-to/write-pythonic-code)
