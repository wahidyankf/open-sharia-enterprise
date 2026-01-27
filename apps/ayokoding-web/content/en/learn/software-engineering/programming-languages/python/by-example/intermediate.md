---
title: "Intermediate"
weight: 10000002
date: 2025-12-30T01:00:00+07:00
draft: false
description: "Examples 28-54: Decorators, generators, context managers, testing (40-75% coverage)"
tags: ["python", "tutorial", "by-example", "intermediate", "production"]
---

This section covers production Python patterns from examples 28-54, achieving 40-75% topic coverage.

## Example 28: Basic Decorator

Decorators wrap functions to modify behavior without changing function code.

```mermaid
%% Decorator wrapping pattern
graph TD
    A["Original Function<br/>add(a, b)"] --> B["Decorator<br/>@trace"]
    B --> C["Wrapper Function<br/>logs calls + result"]
    C --> D["Returns<br/>wrapped function"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
```

```python
def trace(func):
    """Decorator that logs function calls"""
    def wrapper(*args, **kwargs):
        """Wrapper that adds logging behavior"""
        print(f"Calling {func.__name__}")  # => Output: Calling add
                                            # => Logs function name
        result = func(*args, **kwargs)      # => Execute original function
                                            # => result = add(3, 5) = 8
        print(f"Returned {result}")         # => Output: Returned 8
                                            # => Logs return value
        return result                       # => Return original result
                                            # => Returns 8
    return wrapper                          # => Return wrapper function
                                            # => Closure captures func

@trace                                      # => Apply decorator
                                            # => Equivalent to: add = trace(add)
def add(a, b):
    """Add two numbers"""
    return a + b                            # => Original function logic

result = add(3, 5)                          # => Calls wrapper(3, 5)
                                            # => Output: Calling add
                                            # => Output: Returned 8
                                            # => result = 8
```

**Key Takeaway**: Decorators use closure to wrap functions, enabling cross-cutting concerns like logging and timing.

**Why It Matters**: Decorators enable cross-cutting concerns like logging, timing, and authentication to be separated from business logic, improving code maintainability and reusability. The closure-based pattern is fundamental to Python frameworks like Flask and Django for routing and middleware. Understanding decorators is essential for framework development and applying aspect-oriented programming patterns in production systems.

## Example 29: Decorator with Arguments

Decorators can accept configuration parameters for flexible behavior modification.

```mermaid
%% Three-layer decorator factory pattern
graph TD
    A["@repeat(3)<br/>Decorator Factory"] --> B["Returns<br/>decorator function"]
    B --> C["decorator(func)<br/>Actual Decorator"]
    C --> D["Returns<br/>wrapper function"]
    D --> E["wrapper(*args)<br/>Executes 3 times"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#CA9161,color:#fff
```

```python
def repeat(times):
    """Decorator factory that repeats function calls"""
    # => times parameter configures behavior
    # => Returns a decorator function
    def decorator(func):                   # => Actual decorator
                                            # => func = original function
        def wrapper(*args, **kwargs):
            """Innermost wrapper that executes multiple times"""
            results = []                   # => Collect all results
            for i in range(times):         # => Repeat 'times' times
                                            # => Loop 3 times in this example
                result = func(*args, **kwargs)  # => Call original function
                                            # => result = greet("Alice")
                results.append(result)      # => Add to results list
                                            # => results = ['Hello, Alice!', ...]
            return results                  # => Return list of all results
                                            # => ['Hello, Alice!', 'Hello, Alice!', 'Hello, Alice!']
        return wrapper                      # => Return configured wrapper
    return decorator                        # => Return decorator function
                                            # => Closure captures 'times'

@repeat(3)                                  # => Call repeat(3)
                                            # => Returns decorator
                                            # => Then: greet = decorator(greet)
def greet(name):
    """Greet someone"""
    return f"Hello, {name}!"                # => Original function

messages = greet("Alice")                   # => Calls wrapper("Alice")
                                            # => Executes greet("Alice") 3 times
                                            # => messages = ['Hello, Alice!', 'Hello, Alice!', 'Hello, Alice!']
```

**Key Takeaway**: Decorator factories return decorators configured with parameters, enabling reusable behavior customization.

**Why It Matters**: Decorator factories enable parameterized behavior modification, allowing the same decorator pattern to be reused with different configurations across a codebase. The three-layer nesting (factory → decorator → wrapper) is complex but essential for building flexible frameworks and libraries. Mastering decorator factories enables writing reusable infrastructure code that adapts to different requirements without duplication.

## Example 30: Preserving Function Metadata

Use functools.wraps to preserve original function metadata in decorated functions.

```python
from functools import wraps

def debug(func):
    """Decorator with metadata preservation"""
    @wraps(func)                           # => Copies __name__, __doc__, __module__ from func
                                           # => Without this, wrapper.__name__ would be 'wrapper'
    def wrapper(*args, **kwargs):
        """Inner wrapper function"""
        result = func(*args, **kwargs)     # => Call original function
                                           # => result = calculate(x, y)
        print(f"{func.__name__}: {result}")  # => Uses original function name
                                             # => Output: calculate: [result]
        return result                      # => Return original result
    return wrapper                         # => Return decorated wrapper

@debug                                     # => calculate = debug(calculate)
def calculate(x, y):
    """Adds two numbers"""                # => Original docstring preserved
    return x + y                           # => Original function logic

result = calculate(3, 5)                   # => Calls wrapper(3, 5)
                                           # => Output: calculate: 8
                                           # => result = 8
print(calculate.__name__)                  # => Output: 'calculate' (not 'wrapper')
                                           # => Thanks to @wraps
print(calculate.__doc__)                   # => Output: 'Adds two numbers'
                                           # => Original docstring preserved
print(calculate.__module__)                # => Output: '__main__' (or module name)
                                           # => Module info preserved
```

**Key Takeaway**: functools.wraps copies metadata from decorated function to wrapper, preserving introspection capabilities.

**Why It Matters**: Preserving function metadata prevents broken introspection in documentation tools and debugging frameworks that rely on **name**, **doc**, and other attributes. The functools.wraps decorator is a best practice that maintains function identity through decoration layers. Failing to use wraps creates confusing stack traces and documentation in production systems.

## Example 31: Basic Generator

Generators produce values lazily using yield, enabling memory-efficient iteration.

```mermaid
%% Generator yield flow and state
stateDiagram-v2
    [*] --> Created: countdown(3)
    Created --> Running: next() called
    Running --> Suspended: yield 3
    Suspended --> Running: next() called
    Running --> Suspended2: yield 2
    Suspended2 --> Running2: next() called
    Running2 --> Suspended3: yield 1
    Suspended3 --> Running3: next() called
    Running3 --> [*]: StopIteration

    note right of Suspended: Generator pauses<br/>retains state
    note right of Running: Executes until<br/>next yield
```

```python
def countdown(n):
    """Generate numbers from n down to 1"""
    while n > 0:                            # => Loop condition
        yield n                             # => Pause and return current value
                                            # => Function state preserved
                                            # => Returns n to caller
        n -= 1                              # => Decrement for next iteration
                                            # => Executes when next() called again

gen = countdown(3)                          # => Creates generator object
                                            # => Does NOT execute function yet
                                            # => gen is iterator
print(next(gen))                            # => First next() call
                                            # => Executes until first yield
                                            # => Returns 3, n becomes 2
                                            # => Output: 3
print(next(gen))                            # => Second next() call
                                            # => Resumes from yield
                                            # => Returns 2, n becomes 1
                                            # => Output: 2
print(next(gen))                            # => Third next() call
                                            # => Returns 1, n becomes 0
                                            # => Output: 1
# next(gen)                                 # => Fourth next() call
                                            # => while condition false (n=0)
                                            # => Raises StopIteration

for num in countdown(3):                    # => for loop calls next() automatically
                                            # => Stops on StopIteration
    print(num)                              # => Output: 3
                                            # => Output: 2
                                            # => Output: 1
```

**Key Takeaway**: Generators compute values on demand using yield, ideal for large or infinite sequences.

**Why It Matters**: Generators enable memory-efficient iteration over large or infinite sequences by computing values lazily rather than materializing entire collections. The yield statement creates pausable functions that maintain state between calls, essential for processing large files and data streams. Understanding generators is critical for writing scalable Python applications that handle datasets larger than available memory.

## Example 32: Generator Expression

Generator expressions provide concise syntax for simple generators with minimal memory overhead.

```python
# List comprehension (creates full list in memory)
squares_list = [x**2 for x in range(5)]    # => Creates list immediately
                                            # => [0, 1, 4, 9, 16]
                                            # => All values computed and stored
                                            # => Uses memory for entire list

# Generator expression (computes values on demand)
squares_gen = (x**2 for x in range(5))     # => Generator object (lazy)
                                            # => Parentheses instead of brackets
                                            # => No values computed yet
                                            # => <generator object> in memory

print(next(squares_gen))                    # => First call: compute 0**2
                                            # => Output: 0
                                            # => Generator advances to next
print(next(squares_gen))                    # => Second call: compute 1**2
                                            # => Output: 1
                                            # => Previous value forgotten

# Use in sum (generator consumed automatically)
total = sum(x**2 for x in range(1000000))  # => Memory efficient
                                            # => Computes one value at a time
                                            # => No 1M-element list created
                                            # => total = 333332833333500000
                                            # => Generator exhausted after sum
```

**Key Takeaway**: Generator expressions use parentheses instead of brackets, computing values lazily without storing intermediate lists.

**Why It Matters**: Generator expressions provide list comprehension syntax with generator laziness, enabling memory-efficient data pipelines without intermediate list allocation. They compose well in function chains like sum() and max(), improving both memory usage and readability. Using generator expressions instead of list comprehensions for large datasets prevents memory exhaustion in production systems.

## Example 33: Context Manager (with statement)

Context managers handle setup/cleanup automatically using **enter** and **exit** methods.

```mermaid
%% Context manager lifecycle
sequenceDiagram
    participant Code as "with FileManager as f"
    participant CM as "Context Manager"
    participant File as "File Resource"

    Code->>CM: Enter with block
    CM->>CM: __enter__#40;#41; called
    CM->>File: open#40;'data.txt', 'w'#41;
    File-->>CM: File object
    CM-->>Code: Return file object to 'f'

    Note over Code,File: Code block executes
    Code->>File: f.write#40;'Hello'#41;

    Code->>CM: Exit with block
    CM->>CM: __exit__#40;#41; called
    CM->>File: f.close#40;#41;
    Note over CM,File: Guaranteed cleanup<br/>even on exception
```

```python
class FileManager:
    """Context manager for file operations"""
    def __init__(self, filename, mode):
        self.filename = filename           # => Store filename
        self.mode = mode                   # => Store mode ('w', 'r', etc.)
        self.file = None                   # => File handle placeholder

    def __enter__(self):
        """Called when entering 'with' block"""
        self.file = open(self.filename, self.mode)  # => Open file
                                            # => self.file = file object
        return self.file                    # => Return to 'as' variable
                                            # => This becomes 'f' in with statement

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Called when exiting 'with' block"""
        # => exc_type: Exception class if exception occurred, else None
        # => exc_val: Exception instance if exception occurred, else None
        # => exc_tb: Traceback object if exception occurred, else None
        if self.file:                       # => Check if file was opened
            self.file.close()                # => Always close file
                                             # => Guaranteed cleanup
        return False                        # => Return False → propagate exceptions
                                             # => Return True → suppress exceptions

with FileManager('data.txt', 'w') as f:    # => Calls __enter__
                                            # => f = returned file object
    f.write('Hello, World!')                # => File is open
                                            # => Write to file
# => Exits 'with' block
# => Calls __exit__(None, None, None) automatically
# => File closed even if exception occurs
```

**Key Takeaway**: Context managers guarantee cleanup code execution via **exit**, preventing resource leaks.

**Why It Matters**: Context managers guarantee cleanup code execution through **exit**, preventing resource leaks from unclosed files, database connections, and network sockets. The protocol enables custom resource management patterns essential for transaction handling and temporary state changes. Mastering context managers is fundamental for writing reliable production code that handles resources safely.

## Example 34: contextlib for Simple Context Managers

Use @contextmanager decorator to create context managers from generator functions.

```mermaid
%% Contextlib setup-yield-cleanup flow
graph TD
    A["with timer('Processing')"] --> B["Setup Code<br/>start = time.time()"]
    B --> C["yield<br/>(pause generator)"]
    C --> D["Execute with block<br/>total = sum(...)"]
    D --> E["Resume generator<br/>finally block"]
    E --> F["Cleanup Code<br/>print duration"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#CC78BC,color:#fff
    style D fill:#029E73,color:#fff
    style E fill:#CC78BC,color:#fff
    style F fill:#CA9161,color:#fff
```

```python
from contextlib import contextmanager
import time

@contextmanager                            # => Decorator converts generator to context manager
def timer(label):
    """Context manager that measures execution time"""
    start = time.time()                    # => Setup code: record start time
                                           # => Runs BEFORE yield
    try:
        yield                              # => Pause generator
                                           # => Control returns to 'with' block
                                           # => Code inside 'with' executes here
    finally:
        end = time.time()                  # => Cleanup code: record end time
                                           # => Runs AFTER 'with' block
        duration = end - start             # => Calculate elapsed time
        print(f"{label}: {duration:.3f}s") # => Output: Processing: 0.XXX

with timer("Processing"):                  # => Calls timer("Processing")
                                           # => Executes setup (start = time.time())
                                           # => Pauses at yield
    total = sum(range(1000000))            # => Execute code being timed
                                           # => total = 499999500000
# => Exits 'with' block
# => Resumes generator at finally
# => Executes cleanup (print duration)
# => Output: Processing: 0.045s (example)
```

**Key Takeaway**: @contextmanager simplifies context manager creation using yield for separation of setup/cleanup logic.

**Why It Matters**: The contextlib decorator simplifies context manager creation for common patterns, reducing boilerplate compared to implementing the full protocol. The try-finally separation through yield makes setup and cleanup logic explicit and maintainable. This pattern is essential for timing code, managing temporary state, and creating lightweight resource wrappers in production applications.

## Example 35: Regular Expression Matching

Use re module for pattern matching with compile for performance in repeated use.

```python
import re

# Pattern matching
text = "Email: user@example.com, Phone: 555-1234"
                                            # => Text to search
email_pattern = r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b'
                                            # => Raw string pattern (r'' prefix)
                                            # => \b = word boundary
                                            # => + = one or more, . = any char
                                            # => @ required, {2,} = 2+ chars

match = re.search(email_pattern, text)     # => Find first match
                                            # => Searches entire text
                                            # => Returns Match object or None
if match:                                   # => Check if pattern found
    print(match.group())                    # => Output: 'user@example.com'
                                            # => .group() returns matched string

# Find all matches
phone_pattern = r'\d{3}-\d{4}'             # => \d = digit [0-9]
                                            # => {3} = exactly 3 digits
                                            # => - literal hyphen
                                            # => {4} = exactly 4 digits
phones = re.findall(phone_pattern, text)   # => Returns list of all matches
                                            # => phones = ['555-1234']
                                            # => findall returns strings, not Match objects

# Compiled pattern (faster for repeated use)
email_re = re.compile(email_pattern)       # => Compile once
                                            # => Pre-processes pattern
                                            # => Improves performance for multiple uses
result = email_re.search(text)             # => Reuse compiled pattern
                                            # => result = Match object
                                            # => result.group() = 'user@example.com'
```

**Key Takeaway**: Compile patterns for repeated use; use search for first match, findall for all matches.

**Why It Matters**: Compiled regex patterns improve performance for repeated matching by preprocessing patterns once rather than on every call. Regular expressions provide powerful pattern matching for validation, parsing, and data extraction essential for production data processing. However, complex regex patterns should be carefully tested and documented as they can become maintenance nightmares.

## Example 36: Regular Expression Groups and Substitution

Capture groups extract parts of matches; sub performs replacements.

```python
import re

# Capture groups with named groups
pattern = r'(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})'
                                            # => (?P<name>...) = named capture group
                                            # => \d{4} = exactly 4 digits (year)
                                            # => \d{2} = exactly 2 digits (month, day)
text = "Date: 2025-12-30"                   # => Text with date format

match = re.search(pattern, text)            # => Search for date pattern
                                            # => Returns Match object with groups
if match:                                   # => Check if date found
    print(match.group('year'))              # => Access by name: '2025'
                                            # => Named group improves readability
    print(match.group('month'))             # => Access by name: '12'
                                            # => Better than numeric indices
    print(match.groups())                   # => Returns all groups as tuple
                                            # => Output: ('2025', '12', '30')
                                            # => Ordered by appearance in pattern

# Substitution with backreferences
phone_text = "Call 555-1234 or 555-5678"   # => Text with phone numbers
formatted = re.sub(r'(\d{3})-(\d{4})', r'(\1) \2', phone_text)
                                            # => sub(pattern, replacement, text)
                                            # => (\d{3}) = group 1 (area code)
                                            # => (\d{4}) = group 2 (number)
                                            # => \1 = reference to group 1
                                            # => \2 = reference to group 2
                                            # => formatted = 'Call (555) 1234 or (555) 5678'
                                            # => Replaces ALL occurrences
```

**Key Takeaway**: Named groups improve readability; backreferences in sub enable pattern-based transformations.

**Why It Matters**: Named capture groups improve regex readability and make extracted data self-documenting compared to positional access. The sub() function enables powerful pattern-based transformations for data cleaning and normalization tasks. Understanding regex groups and substitution is essential for text processing pipelines in production systems.

## Example 37: JSON Serialization

Convert Python objects to JSON strings and vice versa for data interchange.

```python
import json

# Python to JSON
data = {
    'name': 'Alice',
    'age': 30,
    'skills': ['Python', 'SQL'],
    'active': True
}                                           # => Python dict with various types
                                            # => dict → JSON object
                                            # => list → JSON array
                                            # => bool → JSON boolean

json_str = json.dumps(data, indent=2)      # => Convert to JSON string
                                            # => dumps = "dump string"
                                            # => indent=2 for readable formatting
                                            # => json_str = '{\n  "name": "Alice",\n  "age": 30, ...'
                                            # => Returns string

# JSON to Python
parsed = json.loads(json_str)              # => Parse JSON string
                                            # => loads = "load string"
                                            # => Returns Python dict
                                            # => parsed = {'name': 'Alice', 'age': 30, ...}
print(parsed['name'])                       # => Output: 'Alice'
                                            # => Access dict key

# File operations
with open('data.json', 'w') as f:          # => Open file for writing
    json.dump(data, f, indent=2)           # => Write JSON directly to file
                                            # => dump (no 's') for files
                                            # => Creates formatted JSON file

with open('data.json', 'r') as f:          # => Open file for reading
    loaded = json.load(f)                  # => Read JSON from file
                                            # => load (no 's') for files
                                            # => Returns Python dict
                                            # => loaded = {'name': 'Alice', ...}
```

**Key Takeaway**: Use dumps/loads for strings, dump/load for files; indent parameter enables readable formatting.

**Why It Matters**: JSON serialization provides language-agnostic data exchange essential for REST APIs, configuration files, and inter-service communication. The dumps/loads and dump/load pairs separate string and file operations, reducing confusion and bugs. Mastering JSON handling is fundamental for web development, API integration, and data persistence in production systems.

## Example 38: CSV Reading and Writing

Process tabular data with csv module for proper quoting and escaping.

```python
import csv

# Writing CSV
data = [
    ['Name', 'Age', 'City'],
    ['Alice', '30', 'NYC'],
    ['Bob', '25', 'LA']
]                                               # => List of lists (rows)
                                                # => First row is header

with open('people.csv', 'w', newline='') as f:  # => Open for writing
                                                # => newline='' prevents blank rows on Windows
                                                # => Required for csv module
    writer = csv.writer(f)                      # => Create writer object
                                                # => Handles escaping automatically
    writer.writerows(data)                      # => Write all rows at once
                                                # => Creates: Name,Age,City
                                                # =>          Alice,30,NYC
                                                # =>          Bob,25,LA

# Reading CSV
with open('people.csv', 'r') as f:             # => Open for reading
    reader = csv.reader(f)                     # => Create reader object
                                                # => Returns iterator of rows
    header = next(reader)                       # => Get first row
                                                # => header = ['Name', 'Age', 'City']
    for row in reader:                          # => Iterate remaining rows
                                                # => row = ['Alice', '30', 'NYC']
        print(f"{row[0]} is {row[1]}")          # => Access by index
                                                # => Output: Alice is 30
                                                # => Output: Bob is 25

# DictReader (dict per row)
with open('people.csv', 'r') as f:             # => Open for reading
    reader = csv.DictReader(f)                  # => Creates OrderedDict per row
                                                # => Uses first row as keys
                                                # => More readable than indices
    for row in reader:                          # => row = {'Name': 'Alice', 'Age': '30', 'City': 'NYC'}
        print(row['Name'])                      # => Access by column name
                                                # => Output: Alice
                                                # => Output: Bob
```

**Key Takeaway**: Use newline='' for writers; DictReader provides dict access for more readable code.

**Why It Matters**: CSV handling requires proper quoting and escaping to prevent data corruption from special characters in fields. The DictReader/DictWriter classes improve code readability by using field names rather than indices, reducing bugs in data processing pipelines. Understanding CSV nuances is essential for data import/export, reporting, and integration with spreadsheet applications.

## Example 39: Pathlib for Modern File Operations

Pathlib provides object-oriented path manipulation replacing os.path functions.

```python
from pathlib import Path

# Path creation and properties
p = Path('/home/user/documents/file.txt')
print(p.name)       # => 'file.txt'
print(p.stem)       # => 'file' (without extension)
print(p.suffix)     # => '.txt'
print(p.parent)     # => '/home/user/documents'

# Path operations
config = Path.home() / '.config' / 'app.conf'  # => Path joining with /
print(config.exists())                          # => True/False

# Create directories
logs_dir = Path('logs')
logs_dir.mkdir(parents=True, exist_ok=True)    # => Create with parents

# Read/write files
config_file = Path('config.ini')
config_file.write_text('[Settings]\ntheme=dark')  # => Write text
content = config_file.read_text()                 # => Read text

# Glob patterns
py_files = list(Path('.').glob('**/*.py'))     # => All .py files recursively
```

**Key Takeaway**: Pathlib unifies path operations with intuitive / operator and chainable methods.

**Why It Matters**: Pathlib provides cross-platform path handling with object-oriented API that prevents common path manipulation errors from string concatenation. The / operator for path joining improves readability over os.path.join and eliminates platform-specific separator issues. Modern Python code should prefer pathlib for its type safety and chainable methods over legacy os.path functions.

## Example 40: Collections - namedtuple

namedtuple creates lightweight immutable classes with named fields.

```python
from collections import namedtuple

# Define namedtuple
Point = namedtuple('Point', ['x', 'y'])       # => Class with x, y fields

p1 = Point(3, 4)                              # => Create instance
print(p1.x, p1.y)  # => 3 4 (field access by name)
print(p1[0], p1[1])  # => 3 4 (also accessible by index)

# Unpacking
x, y = p1                                     # => Tuple unpacking works

# Immutable (like tuples)
# p1.x = 5  # => AttributeError: can't set attribute

# _replace for creating modified copies
p2 = p1._replace(x=5)                         # => Point(x=5, y=4)

# _asdict for conversion to dict
print(p1._asdict())  # => {'x': 3, 'y': 4}
```

**Key Takeaway**: namedtuples provide tuple efficiency with struct-like field access for readable code.

**Why It Matters**: Namedtuples provide lightweight data structures with named field access and tuple efficiency, making them ideal for function return values and data transfer objects. The immutability enables use as dictionary keys and thread-safe data sharing without locking. Understanding when to use namedtuples versus dataclasses balances memory efficiency with feature richness in production code.

## Example 41: Collections - Counter

Counter tallies hashable objects, providing convenient frequency counting.

```python
from collections import Counter

# Count occurrences
words = ['apple', 'banana', 'apple', 'cherry', 'banana', 'apple']
counts = Counter(words)                       # => Counter({'apple': 3, 'banana': 2, ...})

print(counts['apple'])     # => 3
print(counts['orange'])    # => 0 (missing keys default to 0)

# Most common elements
print(counts.most_common(2))  # => [('apple', 3), ('banana', 2)]

# Arithmetic operations
c1 = Counter(['a', 'b', 'c', 'a'])
c2 = Counter(['a', 'b', 'd'])
print(c1 + c2)  # => Counter({'a': 3, 'b': 2, 'c': 1, 'd': 1})
print(c1 - c2)  # => Counter({'a': 1, 'c': 1}) (removes common counts)
```

**Key Takeaway**: Counter simplifies frequency counting with arithmetic operations and most_common method.

**Why It Matters**: Counter simplifies frequency analysis and histogram generation with specialized methods like most_common() that eliminate manual counting loops. The arithmetic operations enable set-style operations on multisets for comparing frequency distributions. Mastering Counter is essential for analytics, data processing, and text analysis in production applications.

## Example 42: Collections - defaultdict

defaultdict provides default values for missing keys, eliminating KeyError checks.

```mermaid
%% defaultdict key access with factory
graph TD
    A["Access groups['a']"] --> B{Key 'a'<br/>exists?}
    B -->|Yes| C["Return existing<br/>list"]
    B -->|No| D["Call factory<br/>list()"]
    D --> E["Create empty list<br/>[]"]
    E --> F["Store at key 'a'"]
    F --> G["Return new list"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#CA9161,color:#fff
    style F fill:#CA9161,color:#fff
    style G fill:#029E73,color:#fff
```

```python
from collections import defaultdict

# Group items by first letter
words = ['apple', 'apricot', 'banana', 'blueberry', 'cherry']
                                              # => List of words to group
groups = defaultdict(list)                    # => Default factory: list()
                                              # => Missing keys auto-create empty list

for word in words:                            # => Iterate: 'apple', 'apricot', ...
    groups[word[0]].append(word)              # => word[0] = first letter
                                              # => groups['a'] auto-creates []
                                              # => No KeyError for new keys
                                              # => groups['a'].append('apple')
                                              # => groups = {'a': ['apple', 'apricot'], ...}

print(dict(groups))                           # => Convert to regular dict
                                              # => Output: {'a': ['apple', 'apricot'],
                                              # =>          'b': ['banana', 'blueberry'],
                                              # =>          'c': ['cherry']}

# Count with defaultdict(int)
counts = defaultdict(int)                     # => Default factory: int() = 0
                                              # => Missing keys return 0
for char in "mississippi":                    # => Iterate each character
    counts[char] += 1                         # => First access: counts['m'] = 0
                                              # => Then increment: counts['m'] = 1
                                              # => No KeyError check needed
                                              # => counts = {'m': 1, 'i': 4, 's': 4, 'p': 2}

print(dict(counts))                           # => Output: {'m': 1, 'i': 4, 's': 4, 'p': 2}
```

**Key Takeaway**: defaultdict eliminates missing key checks by calling factory function for new keys.

**Why It Matters**: Defaultdict eliminates boilerplate key existence checks that clutter code with if-key-exists conditionals, improving readability and reducing errors. The factory function pattern enables automatic initialization of complex default values like lists and sets. Understanding defaultdict is essential for grouping operations and data aggregation in production pipelines.

## Example 43: Collections - deque

deque (double-ended queue) provides O(1) append/pop from both ends.

```mermaid
%% Deque double-ended operations
graph TD
    A["appendleft#40;0#41;<br/>O#40;1#41;"] --> B["#91;0, 1, 2, 3#93;<br/>deque"]
    B --> C["append#40;4#41;<br/>O#40;1#41;"]

    D["popleft#40;#41;<br/>O#40;1#41;<br/>returns 0"] --> B
    B --> E["pop#40;#41;<br/>O#40;1#41;<br/>returns 4"]

    F["rotate#40;1#41;"] --> G["#91;3, 1, 2#93;<br/>shift right"]
    G --> H["rotate#40;-1#41;"] --> I["#91;1, 2, 3#93;<br/>shift left"]

    style A fill:#0173B2,color:#fff
    style C fill:#0173B2,color:#fff
    style B fill:#029E73,color:#fff
    style D fill:#DE8F05,color:#fff
    style E fill:#DE8F05,color:#fff
    style F fill:#CC78BC,color:#fff
    style H fill:#CC78BC,color:#fff
```

```python
from collections import deque

# Create deque
d = deque([1, 2, 3])                          # => deque([1, 2, 3])

# Add to both ends (O(1))
d.append(4)                                   # => deque([1, 2, 3, 4])
d.appendleft(0)                               # => deque([0, 1, 2, 3, 4])

# Remove from both ends (O(1))
d.pop()                                       # => 4, deque([0, 1, 2, 3])
d.popleft()                                   # => 0, deque([1, 2, 3])

# Rotation
d.rotate(1)                                   # => deque([3, 1, 2]) (rotate right)
d.rotate(-1)                                  # => deque([1, 2, 3]) (rotate left)

# Bounded deque (circular buffer)
buffer = deque(maxlen=3)                      # => Fixed size
buffer.extend([1, 2, 3])                      # => deque([1, 2, 3])
buffer.append(4)                              # => deque([2, 3, 4]) (1 dropped)
```

**Key Takeaway**: deque optimizes both-end operations; maxlen creates circular buffers for sliding windows.

**Why It Matters**: Deque provides O(1) operations on both ends compared to list's O(n) for operations at the beginning, making it essential for queue and stack implementations. The maxlen parameter creates bounded collections that automatically evict old items, perfect for sliding windows and recent item caches. Choosing deque over list for queue operations prevents performance bottlenecks in high-throughput systems.

## Example 44: functools - partial

partial creates new functions with pre-filled arguments.

```python
from functools import partial

def power(base, exponent):
    return base ** exponent                   # => Calculate base^exponent
                                              # => power(2, 3) = 2^3 = 8

# Create specialized functions
square = partial(power, exponent=2)           # => Pre-fill exponent parameter
                                              # => square = power(base=?, exponent=2)
                                              # => Creates new function with one parameter
cube = partial(power, exponent=3)             # => cube = power(base=?, exponent=3)
                                              # => Another specialized version

print(square(4))                              # => Calls power(4, exponent=2)
                                              # => Output: 16 (4^2)
print(cube(4))                                # => Calls power(4, exponent=3)
                                              # => Output: 64 (4^3)

# Useful for callbacks with extra context
from operator import mul                      # => mul(a, b) = a * b
double = partial(mul, 2)                      # => Pre-fill first argument
                                              # => double(x) = mul(2, x) = 2 * x
numbers = [1, 2, 3, 4]                        # => List to process
doubled = list(map(double, numbers))          # => Apply double to each
                                              # => map(double, [1, 2, 3, 4])
                                              # => [double(1), double(2), double(3), double(4)]
                                              # => [2, 4, 6, 8]
```

**Key Takeaway**: partial binds arguments to functions, creating specialized versions without wrapper functions.

**Why It Matters**: Partial application reduces function parameter count by pre-binding arguments, improving code reuse and enabling adapter patterns. The technique is essential for creating specialized functions from general ones without writing wrapper functions. Understanding partial is critical for functional programming patterns and callback customization in event-driven systems.

## Example 45: functools - lru_cache

lru_cache memoizes function results for repeated calls with same arguments.

```mermaid
%% LRU cache mechanism
graph TD
    A["fibonacci(10) called"] --> B{Result in<br/>cache?}
    B -->|Yes| C["Return cached<br/>result (hit)"]
    B -->|No| D["Compute result<br/>fib(9) + fib(8)"]
    D --> E["Store in cache<br/>max 128 entries"]
    E --> F["Return result"]

    G["Cache Full?"] --> H{LRU eviction}
    H -->|Yes| I["Remove least<br/>recently used"]
    H -->|No| J["Add to cache"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
    style E fill:#CA9161,color:#fff
```

```python
from functools import lru_cache

@lru_cache(maxsize=128)                       # => Cache up to 128 results
                                              # => LRU = Least Recently Used
                                              # => Evicts oldest when full
def fibonacci(n):
    """Compute nth Fibonacci number"""
    if n < 2:                                 # => Base cases
        return n                              # => fib(0)=0, fib(1)=1
    return fibonacci(n-1) + fibonacci(n-2)    # => Recursive calls
                                              # => WITHOUT cache: exponential time
                                              # => WITH cache: linear time
                                              # => Each n computed once

# First call computes and caches
print(fibonacci(10))                          # => Output: 55
                                              # => Computes fib(0) to fib(10)
                                              # => Stores all intermediate results
                                              # => fibonacci(9) + fibonacci(8)

# Subsequent calls use cache
print(fibonacci(10))                          # => Output: 55
                                              # => Instant retrieval from cache
                                              # => No recomputation
                                              # => Cache hit

# Cache statistics
print(fibonacci.cache_info())                 # => Output: CacheInfo(hits=..., misses=..., maxsize=128, currsize=...)
                                              # => hits: successful cache lookups
                                              # => misses: cache misses (computed)
                                              # => currsize: entries in cache

# Clear cache if needed
fibonacci.cache_clear()                       # => Remove all cached results
                                              # => Resets hits/misses to 0
                                              # => Next call will recompute
```

**Key Takeaway**: lru_cache dramatically speeds up recursive functions by caching results keyed by arguments.

**Why It Matters**: LRU cache dramatically improves performance for expensive recursive functions by memoizing results based on arguments, preventing redundant computation. The automatic cache size management prevents unbounded memory growth while maintaining hit rates. Mastering caching patterns is essential for optimizing production systems without manual cache implementation.

## Example 46: itertools - Powerful Iteration

itertools provides composable iterator building blocks for efficient iteration.

```mermaid
%% Itertools chain and groupby
graph TD
    A["[1, 2]"] --> Chain
    B["[3, 4]"] --> Chain
    C["[5]"] --> Chain["chain()"]
    Chain --> D["1, 2, 3, 4, 5<br/>(single iterable)"]

    E["[('A',1), ('A',2),<br/>('B',3), ('B',4)]"] --> GroupBy["groupby(key=lambda x: x[0])"]
    GroupBy --> F["Group 'A'<br/>[('A',1), ('A',2)]"]
    GroupBy --> G["Group 'B'<br/>[('B',3), ('B',4)]"]

    style Chain fill:#0173B2,color:#fff
    style D fill:#029E73,color:#fff
    style GroupBy fill:#DE8F05,color:#fff
    style F fill:#CC78BC,color:#fff
    style G fill:#CC78BC,color:#fff
```

```python
from itertools import chain, cycle, islice, combinations, groupby

# Chain multiple iterables
chained = chain([1, 2], [3, 4], [5])         # => 1, 2, 3, 4, 5

# Infinite cycling
counter = cycle(['A', 'B', 'C'])             # => A, B, C, A, B, C, ...
first_6 = list(islice(counter, 6))           # => ['A', 'B', 'C', 'A', 'B', 'C']

# Combinations
combos = list(combinations([1, 2, 3], 2))    # => [(1, 2), (1, 3), (2, 3)]

# Group consecutive items
data = [('A', 1), ('A', 2), ('B', 3), ('B', 4)]
for key, group in groupby(data, key=lambda x: x[0]):
    print(key, list(group))
    # => A [('A', 1), ('A', 2)]
    # => B [('B', 3), ('B', 4)]
```

**Key Takeaway**: itertools functions compose for complex iterations without intermediate lists.

**Why It Matters**: Itertools functions compose to create complex iteration patterns without materializing intermediate collections, improving both memory efficiency and code clarity. The lazy evaluation enables processing infinite sequences and large datasets that don't fit in memory. Understanding itertools is essential for data pipeline development and functional-style programming in Python.

## Example 47: Datetime Basics

Datetime module handles dates, times, and timedeltas for temporal calculations.

```python
from datetime import datetime, date, time, timedelta

# Current datetime
now = datetime.now()                          # => Current local datetime
today = date.today()                          # => Current date (no time)

# Creating specific datetime
dt = datetime(2025, 12, 30, 14, 30, 0)       # => 2025-12-30 14:30:00

# Formatting (strftime = string from time)
formatted = dt.strftime("%Y-%m-%d %H:%M")    # => '2025-12-30 14:30'

# Parsing (strptime = string parse time)
parsed = datetime.strptime("2025-12-30", "%Y-%m-%d")  # => datetime object

# Timedelta (duration)
tomorrow = today + timedelta(days=1)         # => Add 1 day
next_week = now + timedelta(weeks=1)         # => Add 7 days
difference = dt - now                        # => timedelta object

print(difference.days)      # => Number of days
print(difference.seconds)   # => Remaining seconds (< 1 day)
```

**Key Takeaway**: Use datetime for timestamps, date for calendar dates, timedelta for durations and arithmetic.

**Why It Matters**: Datetime handling prevents common pitfalls in date arithmetic and formatting essential for logging, scheduling, and business logic. The strftime/strptime format strings provide standardized date serialization for APIs and databases. Understanding timezone-aware datetime is critical for production systems serving global users, though the basic datetime module lacks timezone support (use dateutil or zoneinfo).

## Example 48: Type Hints Basics

Type hints improve code clarity and enable static type checking with mypy.

```python
from typing import List, Dict, Optional, Union

def greet(name: str) -> str:
    """Function with type hints"""
    # => name: str = parameter type hint
    # => -> str = return type hint
    return f"Hello, {name}!"                  # => Return type: str
                                              # => Matches declared return type

def process_items(items: List[int]) -> int:
    """Process list of integers"""
    # => items: List[int] = list containing ints
    # => Generic type with type parameter
    return sum(items)                         # => Returns int (sum of list)
                                              # => Matches declared -> int

# Optional (can be None)
def find_user(user_id: int) -> Optional[Dict[str, str]]:
    """Returns user dict or None"""
    # => Optional[X] = Union[X, None]
    # => Can return Dict[str, str] OR None
    # => Dict[str, str] = dict with str keys and str values
    if user_id == 1:                          # => Check condition
        return {'name': 'Alice', 'role': 'admin'}  # => Return dict (valid)
    return None                               # => Return None (also valid)
                                              # => Optional allows both

# Union (multiple types)
def format_value(val: Union[int, str]) -> str:
    """Accept int or str"""
    # => Union[int, str] = can be int OR str
    # => Type checker accepts both types
    return str(val)                           # => Convert to string
                                              # => Works for both int and str

# Usage (types are hints, not enforced at runtime)
result = greet("Bob")                         # => Correct type: str
                                              # => Output: "Hello, Bob!"
                                              # => Type checker happy
# result = greet(123)                         # => Wrong type: int
                                              # => Still works at runtime (dynamic)
                                              # => But mypy/pyright warns: Expected str, got int
```

**Key Takeaway**: Type hints document expected types for tools like mypy without affecting runtime behavior.

**Why It Matters**: Type hints improve code documentation and enable static type checking with mypy, catching type errors before deployment without affecting runtime performance. The gradual typing system allows incremental adoption in existing codebases while maintaining Python's dynamic flexibility. Mastering type hints is essential for large-scale Python projects where IDE autocomplete and static analysis prevent bugs.

## Example 49: Dataclasses

Dataclasses reduce boilerplate for classes primarily used for storing data.

```python
from dataclasses import dataclass, field

@dataclass                                    # => Decorator auto-generates methods
class Product:
    """Product with automatic __init__, __repr__, __eq__"""
    name: str                                 # => Required field (no default)
                                              # => Type annotation required
    price: float                              # => Required field
    quantity: int = 0                         # => Optional with default
                                              # => Default values allowed
    tags: list = field(default_factory=list)  # => Mutable default (safe)
                                              # => field() prevents shared list bug
                                              # => default_factory calls list() per instance

    def total_value(self) -> float:
        return self.price * self.quantity     # => Custom method still works
                                              # => self.price = 999.99
                                              # => self.quantity = 5
                                              # => Returns 4999.95

# Automatic __init__
p1 = Product("Laptop", 999.99, 5)            # => No __init__ method written
                                              # => @dataclass creates it
                                              # => p1.name = "Laptop"
                                              # => p1.price = 999.99
                                              # => p1.quantity = 5

# Automatic __repr__
print(p1)                                     # => Output: Product(name='Laptop', price=999.99, quantity=5, tags=[])
                                              # => Readable representation
                                              # => Shows all field values

# Automatic __eq__
p2 = Product("Laptop", 999.99, 5)            # => Create second product
print(p1 == p2)                               # => Output: True
                                              # => Compares all fields
                                              # => name=='Laptop', price==999.99, etc.

# field() for complex defaults
p3 = Product("Mouse", 29.99, tags=["wireless", "ergonomic"])
                                              # => p3.tags = ["wireless", "ergonomic"]
                                              # => Each instance gets its own list
                                              # => No shared mutable default bug
```

**Key Takeaway**: @dataclass auto-generates **init**, **repr**, **eq** reducing boilerplate for data-focused classes.

**Why It Matters**: Dataclasses eliminate boilerplate for data-focused classes by auto-generating **init**, **repr**, and **eq** methods, reducing code size and maintenance burden. The field() function enables advanced default value handling and metadata for serialization frameworks. Understanding dataclasses is essential for modern Python development, especially for data transfer objects and API models.

## Example 50: Enums for Named Constants

Enums create type-safe named constants preventing magic values.

```python
from enum import Enum, auto

class Status(Enum):
    """Order status enumeration"""
    PENDING = 1
    PROCESSING = 2
    SHIPPED = 3
    DELIVERED = 4

# Access by name or value
print(Status.PENDING)        # => Status.PENDING
print(Status.PENDING.value)  # => 1
print(Status(2))             # => Status.PROCESSING

# Iteration
for status in Status:
    print(status.name, status.value)  # => PENDING 1, PROCESSING 2, ...

# Auto values
class Priority(Enum):
    LOW = auto()      # => 1
    MEDIUM = auto()   # => 2
    HIGH = auto()     # => 3

# Comparison
if Status.PENDING == Status.PENDING:  # => True
    print("Same status")
```

**Key Takeaway**: Enums replace magic numbers/strings with type-safe constants that prevent invalid values.

**Why It Matters**: Enums prevent magic numbers and strings that make code unclear and error-prone, providing type-safe constants with better IDE support. The enumeration pattern enables exhaustive match checking and prevents invalid values from entering the system. Mastering enums is essential for configuration management, state machines, and API design in production systems.

## Example 51: Abstract Base Classes

ABCs define interfaces that subclasses must implement.

```mermaid
%% Abstract base class inheritance
classDiagram
    class PaymentProcessor {
        <<abstract>>
        +process_payment(amount)* abstract
        +refund(transaction_id)* abstract
    }

    class StripeProcessor {
        +process_payment(amount) implemented
        +refund(transaction_id) implemented
    }

    class PayPalProcessor {
        +process_payment(amount) implemented
        +refund(transaction_id) implemented
    }

    PaymentProcessor <|-- StripeProcessor : implements
    PaymentProcessor <|-- PayPalProcessor : implements

    note for PaymentProcessor "Cannot instantiate<br/>abstract class directly"
    note for StripeProcessor "Must implement all<br/>abstract methods"
```

```python
from abc import ABC, abstractmethod

class PaymentProcessor(ABC):
    """Abstract payment processor interface"""
    # => ABC = Abstract Base Class
    # => Cannot be instantiated directly
    # => Enforces interface contract

    @abstractmethod                         # => Decorator marks method as abstract
    def process_payment(self, amount: float) -> bool:
        """All subclasses must implement this method"""
        pass                                # => No implementation in base class
                                             # => Subclasses MUST override this

    @abstractmethod
    def refund(self, transaction_id: str) -> bool:
        """All subclasses must implement this method"""
        pass                                # => Abstract methods use pass

class StripeProcessor(PaymentProcessor):
    """Concrete implementation of payment processor"""
    # => Must implement ALL abstract methods
    # => Otherwise TypeError on instantiation

    def process_payment(self, amount: float) -> bool:
        """Concrete implementation for Stripe"""
        print(f"Processing ${amount} via Stripe")  # => Output: Processing $100.00 via Stripe
        return True                           # => Return success status

    def refund(self, transaction_id: str) -> bool:
        """Concrete implementation for refunds"""
        print(f"Refunding transaction {transaction_id}")  # => Output: Refunding transaction TXN123
        return True                           # => Return success status

# processor = PaymentProcessor()            # => TypeError: Can't instantiate abstract class
                                             # => Abstract classes cannot be instantiated
processor = StripeProcessor()                # => OK, implements all abstract methods
                                             # => Creates concrete instance
processor.process_payment(100.00)            # => Output: Processing $100.00 via Stripe
                                             # => Returns True
```

**Key Takeaway**: ABCs enforce interface contracts at instantiation time preventing incomplete implementations.

**Why It Matters**: Abstract base classes enforce interface contracts at instantiation time rather than runtime, catching implementation errors earlier in development. The protocol-based approach enables framework design where plugins must implement specific methods. Understanding ABCs is essential for library development and ensuring subclasses fulfill their contracts in large codebases.

## Example 52: Basic pytest Tests

pytest provides powerful testing with minimal boilerplate.

```python
# test_calculator.py
def add(a, b):
    """Function to test"""
    return a + b                              # => Returns sum of a and b

def test_add_positive_numbers():
    """Test addition of positive numbers"""
    assert add(2, 3) == 5                     # => Verify 2 + 3 equals 5
                                               # => If false, pytest shows: AssertionError
                                               # => ✓ PASS (assertion true)

def test_add_negative_numbers():
    """Test addition of negative numbers"""
    assert add(-1, -1) == -2                  # => Verify -1 + -1 equals -2
                                               # => ✓ PASS

def test_add_zero():
    """Test addition with zero"""
    assert add(5, 0) == 5                     # => Verify 5 + 0 equals 5
                                               # => ✓ PASS
    assert add(0, 5) == 5                     # => Verify 0 + 5 equals 5
                                               # => ✓ PASS

# Run with: pytest test_calculator.py
# => pytest discovers test files (test_*.py, *_test.py)
# => Collects functions starting with test_
# => Executes each test function
# => Reports: test_add_positive_numbers PASSED
# =>         test_add_negative_numbers PASSED
# =>         test_add_zero PASSED
# => Summary: 3 passed in 0.01s
```

**Key Takeaway**: pytest uses simple assert statements with automatic discovery of test files and functions.

**Why It Matters**: Pytest's simple assert statements with automatic introspection provide clear failure messages without boilerplate assertion methods, improving test maintainability. The convention-based test discovery eliminates configuration overhead and encourages consistent test organization. Mastering pytest is essential for professional Python development where automated testing prevents regressions.

## Example 53: pytest Fixtures

Fixtures provide reusable test data and setup/teardown logic.

```python
import pytest

@pytest.fixture                               # => Decorator marks function as fixture
def sample_data():
    """Fixture providing sample data"""
    data = [1, 2, 3, 4, 5]                    # => Create test data
    print("Setup: creating data")             # => Run BEFORE test executes
                                               # => Output during test run
    yield data                                # => Provide data to test function
                                               # => Test receives data as parameter
    print("Teardown: cleaning up")            # => Run AFTER test completes
                                               # => Cleanup code here

def test_sum_with_fixture(sample_data):
    """Test using fixture (injected as parameter)"""
    # => pytest sees 'sample_data' parameter
    # => Calls sample_data() fixture
    # => Injects returned value
    # => sample_data = [1, 2, 3, 4, 5]
    assert sum(sample_data) == 15             # => Verify sum is 15
                                               # => ✓ PASS (15 == 15)

@pytest.fixture(scope="module")               # => Module-scoped fixture
                                               # => Created once per test module
                                               # => Shared across all tests in module
def database_connection():
    """Module-scoped fixture (created once per module)"""
    conn = "DB_CONNECTION"                    # => Setup once (expensive operation)
                                               # => Shared by all tests
    yield conn                                # => Provide to tests
    # Cleanup code here                       # => Runs once at module end
    # conn.close()                            # => Close connection

def test_query(database_connection):
    """Test using shared connection"""
    # => database_connection = "DB_CONNECTION"
    # => Same instance used by all module tests
    assert database_connection == "DB_CONNECTION"  # => ✓ PASS
```

**Key Takeaway**: Fixtures enable DRY tests with dependency injection and automatic setup/teardown.

**Why It Matters**: Fixtures enable test isolation and reusable test data setup, preventing test interdependencies that cause flaky tests in CI/CD pipelines. The scope management (function, module, session) balances test isolation with performance for expensive setup operations. Understanding pytest fixtures is critical for building maintainable test suites that scale with codebase growth.

## Example 54: pytest Parametrize

Parametrize runs same test with different inputs avoiding repetitive test functions.

```python
import pytest

def is_palindrome(s):
    """Check if string is palindrome"""
    return s == s[::-1]                       # => Compare string with reversed version

@pytest.mark.parametrize("text,expected", [  # => Decorator with parameter combinations
    ("racecar", True),                        # => Test case 1: palindrome
    ("hello", False),                         # => Test case 2: not palindrome
    ("level", True),                          # => Test case 3: palindrome
    ("python", False),                        # => Test case 4: not palindrome
])                                            # => Creates 4 test instances
def test_palindrome(text, expected):
    """Single test function, multiple executions"""
    # => pytest injects (text, expected) for each tuple
    # => Execution 1: text="racecar", expected=True
    # => Execution 2: text="hello", expected=False
    # => Execution 3: text="level", expected=True
    # => Execution 4: text="python", expected=False
    assert is_palindrome(text) == expected    # => Verify against expected result
                                               # => ✓ All 4 test cases PASS

# pytest generates 4 separate test runs:
# => test_palindrome[racecar-True] PASSED
# => test_palindrome[hello-False] PASSED
# => test_palindrome[level-True] PASSED
# => test_palindrome[python-False] PASSED

@pytest.mark.parametrize("a,b,expected", [   # => Another parametrized test
    (2, 3, 5),                                # => 2 + 3 should equal 5
    (0, 0, 0),                                # => 0 + 0 should equal 0
    (-1, 1, 0),                               # => -1 + 1 should equal 0
])                                            # => Creates 3 test instances
def test_add(a, b, expected):
    """Test addition with multiple cases"""
    # => Execution 1: a=2, b=3, expected=5
    # => Execution 2: a=0, b=0, expected=0
    # => Execution 3: a=-1, b=1, expected=0
    assert a + b == expected                  # => Verify each case
                                               # => ✓ All 3 cases PASS
```

**Key Takeaway**: @pytest.mark.parametrize eliminates duplicate test code by running same logic with different inputs.

**Why It Matters**: Parametrized tests eliminate duplicate test code for checking multiple inputs, improving test coverage while reducing maintenance burden. The pytest-generated test IDs enable pinpointing failures to specific parameter sets, improving debugging efficiency. Mastering parametrize is essential for thorough testing without code duplication, especially for validation logic and boundary condition testing.

## Summary

Intermediate Python (examples 28-54) covers production patterns for real-world development: decorators for behavior modification, generators for memory efficiency, context managers for resource safety, testing with pytest, and essential standard library modules. Master these patterns to write maintainable, professional Python code.
