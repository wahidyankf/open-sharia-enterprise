---
title: "Optimize Performance"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000017
description: "Practical techniques for optimizing Python performance using cProfile, memory profiling, NumPy, and Cython for faster applications"
---

## Problem

Python applications can suffer from performance bottlenecks due to the interpreted nature of the language, inefficient algorithms, excessive memory allocation, or inappropriate data structure choices. Profiling and optimization require systematic approaches to identify and resolve actual performance issues rather than premature optimization.

## Solution

### 1. CPU Profiling with cProfile

Identify CPU-intensive code sections using Python's built-in profiler.

```python
import cProfile
import pstats
from pstats import SortKey

def slow_function():
    """Example function with performance issues"""
    total = 0
    for i in range(1000000):
        total += i
    return total

def another_function():
    """Another function to profile"""
    result = []
    for i in range(100000):
        result.append(i ** 2)
    return result

def main():
    """Main function calling multiple functions"""
    slow_function()
    another_function()

if __name__ == '__main__':
    profiler = cProfile.Profile()
    profiler.enable()

    main()

    profiler.disable()
    stats = pstats.Stats(profiler)
    stats.sort_stats(SortKey.CUMULATIVE)
    stats.print_stats(10)  # Show top 10 functions

cProfile.run('slow_function()', 'profile_stats')

with cProfile.Profile() as pr:
    main()

    stats = pstats.Stats(pr)
    stats.sort_stats(SortKey.TIME)
    stats.print_stats()

stats = pstats.Stats('profile_stats')
stats.strip_dirs()
stats.sort_stats('cumulative')
stats.print_stats(10)

stats.print_stats('slow_function')
```

### 2. Memory Profiling with memory_profiler

Track memory usage line-by-line to identify memory leaks and inefficient allocations.

```python
from memory_profiler import profile

@profile
def memory_intensive():
    """Function with high memory usage"""
    # List comprehension - creates entire list in memory
    data = [i for i in range(1000000)]

    # Dictionary creation
    mapping = {i: i**2 for i in range(100000)}

    # String concatenation (inefficient)
    result = ""
    for i in range(10000):
        result += str(i)

    return sum(data)


import tracemalloc

tracemalloc.start()

data = [i for i in range(1000000)]

current, peak = tracemalloc.get_traced_memory()
print(f"Current memory usage: {current / 10**6:.2f} MB")
print(f"Peak memory usage: {peak / 10**6:.2f} MB")

tracemalloc.stop()
```

### 3. Optimize with NumPy for Numerical Operations

Replace pure Python loops with vectorized NumPy operations for massive speed gains.

```python
import numpy as np
import time

def python_sum(n):
    total = 0
    for i in range(n):
        total += i
    return total

def numpy_sum(n):
    return np.arange(n).sum()

n = 1_000_000

start = time.perf_counter()
result1 = python_sum(n)
python_time = time.perf_counter() - start

start = time.perf_counter()
result2 = numpy_sum(n)
numpy_time = time.perf_counter() - start

print(f"Python: {python_time:.4f}s")
print(f"NumPy: {numpy_time:.4f}s")
print(f"Speedup: {python_time / numpy_time:.1f}x")

data = np.random.rand(1000, 1000)

result = (data * 2 + 3) / 5  # All at once

result_slow = np.zeros_like(data)
for i in range(data.shape[0]):
    for j in range(data.shape[1]):
        result_slow[i, j] = (data[i, j] * 2 + 3) / 5
```

### 4. Use Appropriate Data Structures

Choose the right data structure for your use case.

```python
import time

data_list = list(range(10000))
data_set = set(range(10000))

start = time.perf_counter()
for _ in range(1000):
    9999 in data_list
list_time = time.perf_counter() - start

start = time.perf_counter()
for _ in range(1000):
    9999 in data_set
set_time = time.perf_counter() - start

print(f"List: {list_time:.4f}s, Set: {set_time:.4f}s")
print(f"Set is {list_time / set_time:.1f}x faster")

from collections import deque

queue_list = []
for i in range(10000):
    queue_list.append(i)
    if len(queue_list) > 100:
        queue_list.pop(0)  # O(n) operation

queue_deque = deque(maxlen=100)
for i in range(10000):
    queue_deque.append(i)  # O(1) operation
```

### 5. Generator Expressions for Memory Efficiency

Use generators instead of list comprehensions for large datasets.

```python
large_list = [x**2 for x in range(1_000_000)]
total = sum(large_list)

large_gen = (x**2 for x in range(1_000_000))
total = sum(large_gen)

def read_large_file_list(filepath):
    """Loads entire file into memory"""
    with open(filepath) as f:
        return [line.strip() for line in f]

def read_large_file_gen(filepath):
    """Yields lines one at a time"""
    with open(filepath) as f:
        for line in f:
            yield line.strip()

for line in read_large_file_gen('large_file.txt'):
    process(line)
```

### 6. Caching and Memoization

Cache expensive function calls to avoid redundant computation.

```python
from functools import lru_cache
import time

def fibonacci_slow(n):
    if n < 2:
        return n
    return fibonacci_slow(n-1) + fibonacci_slow(n-2)

@lru_cache(maxsize=None)
def fibonacci_fast(n):
    if n < 2:
        return n
    return fibonacci_fast(n-1) + fibonacci_fast(n-2)

start = time.perf_counter()
result1 = fibonacci_slow(30)
slow_time = time.perf_counter() - start

start = time.perf_counter()
result2 = fibonacci_fast(30)
fast_time = time.perf_counter() - start

print(f"Without cache: {slow_time:.4f}s")
print(f"With cache: {fast_time:.4f}s")
print(f"Speedup: {slow_time / fast_time:.1f}x")

_cache = {}

def expensive_computation(x, y):
    key = (x, y)
    if key not in _cache:
        time.sleep(0.1)  # Simulate expensive work
        _cache[key] = x ** y
    return _cache[key]
```

### 7. String Optimization

Optimize string operations for performance.

```python
import time

def concatenate_slow(n):
    result = ""
    for i in range(n):
        result += str(i)
    return result

def concatenate_fast(n):
    parts = []
    for i in range(n):
        parts.append(str(i))
    return ''.join(parts)

def concatenate_fastest(n):
    return ''.join(str(i) for i in range(n))

n = 10000

start = time.perf_counter()
concatenate_slow(n)
slow_time = time.perf_counter() - start

start = time.perf_counter()
concatenate_fast(n)
fast_time = time.perf_counter() - start

start = time.perf_counter()
concatenate_fastest(n)
fastest_time = time.perf_counter() - start

print(f"Slow: {slow_time:.4f}s")
print(f"Fast: {fast_time:.4f}s")
print(f"Fastest: {fastest_time:.4f}s")
```

### 8. Use Built-in Functions and Libraries

Leverage C-optimized built-in functions instead of reimplementing.

```python
def manual_sum(data):
    total = 0
    for item in data:
        total += item
    return total

data = list(range(1_000_000))
result = sum(data)

def manual_max(data):
    maximum = data[0]
    for item in data:
        if item > maximum:
            maximum = item
    return maximum

result = max(data)

def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(0, n-i-1):
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]
    return arr

sorted_data = sorted(data)
```

## How It Works

### Profiling Workflow

1. **Baseline Measurement**: Profile code without changes to establish baseline
2. **Identify Hotspots**: Focus on functions consuming most time/memory
3. **Optimize**: Apply targeted optimizations to hotspots only
4. **Re-measure**: Profile again to verify improvements
5. **Iterate**: Repeat until performance targets met

### Memory Optimization Principles

- **Generators**: Lazy evaluation reduces peak memory usage
- **In-place Operations**: Modify data structures instead of creating copies
- **Reference Counting**: Python's garbage collection frees unused objects
- **Data Structure Choice**: Use appropriate containers (set for membership, deque for queues)

### CPU Optimization Techniques

- **Vectorization**: NumPy/Pandas operations run at C speed
- **Caching**: Memoization trades memory for CPU cycles
- **Built-ins**: C-implemented functions are faster than pure Python
- **Algorithm Choice**: O(n log n) sorting beats O(n²) bubble sort

## Variations

### Line Profiler for Line-by-Line Analysis

Use `line_profiler` for detailed line-by-line CPU profiling:

```python

from line_profiler import LineProfiler

def process_data(data):
    result = []
    for item in data:
        result.append(item ** 2)
    return sum(result)

profiler = LineProfiler()
profiler.add_function(process_data)

data = list(range(10000))
profiler.runcall(process_data, data)
profiler.print_stats()


@profile  # noqa
def process_data_decorated(data):
    result = []
    for item in data:
        result.append(item ** 2)
    return sum(result)
```

### PyPy for JIT Compilation

Use PyPy as drop-in replacement for CPython for automatic speed improvements:

```bash
pypy3 script.py

```

### Cython for C-Speed Extensions

Compile Python code to C for near-native performance:

```python
def compute_sum(int n):
    cdef int i, total = 0
    for i in range(n):
        total += i
    return total

from setuptools import setup
from Cython.Build import cythonize

setup(
    ext_modules=cythonize("example.pyx")
)

```

### Multiprocessing for CPU-Bound Tasks

Bypass GIL with multiprocessing for parallel CPU work:

```python
from multiprocessing import Pool
import time

def cpu_intensive(n):
    """Simulates CPU-bound work"""
    total = 0
    for i in range(n):
        total += i ** 2
    return total

start = time.perf_counter()
results = [cpu_intensive(1_000_000) for _ in range(4)]
sequential_time = time.perf_counter() - start

start = time.perf_counter()
with Pool(processes=4) as pool:
    results = pool.map(cpu_intensive, [1_000_000] * 4)
parallel_time = time.perf_counter() - start

print(f"Sequential: {sequential_time:.2f}s")
print(f"Parallel: {parallel_time:.2f}s")
print(f"Speedup: {sequential_time / parallel_time:.1f}x")
```

### Threading for I/O-Bound Tasks

Use threading for I/O-bound operations (file I/O, network requests):

```python
import concurrent.futures
import requests
import time

urls = [
    'https://httpbin.org/delay/1',
    'https://httpbin.org/delay/1',
    'https://httpbin.org/delay/1',
    'https://httpbin.org/delay/1',
]

def fetch_url(url):
    response = requests.get(url)
    return response.status_code

start = time.perf_counter()
results = [fetch_url(url) for url in urls]
sequential_time = time.perf_counter() - start

start = time.perf_counter()
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
    results = list(executor.map(fetch_url, urls))
parallel_time = time.perf_counter() - start

print(f"Sequential: {sequential_time:.2f}s")
print(f"Parallel: {parallel_time:.2f}s")
print(f"Speedup: {sequential_time / parallel_time:.1f}x")
```

## Common Pitfalls

### Premature Optimization

**Problem**: Optimizing code before identifying actual bottlenecks wastes time.

**Solution**: Always profile first. Optimize only measured hotspots.

```python
def optimized_function(data):
    # Complex, hard-to-read optimized code
    return sum(x**2 for x in data if x % 2 == 0)

def readable_function(data):
    # Simple, readable code
    evens = [x for x in data if x % 2 == 0]
    squares = [x**2 for x in evens]
    return sum(squares)

```

### Ignoring Algorithm Complexity

**Problem**: Micro-optimizations don't fix O(n²) algorithms.

**Solution**: Choose appropriate algorithms before micro-optimizing.

```python
def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(0, n-i-1):
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]
    return arr

sorted_arr = sorted(arr)

```

### Over-Caching

**Problem**: Excessive caching consumes memory and may slow down simple operations.

**Solution**: Cache selectively. Use bounded caches (`maxsize` parameter).

```python
from functools import lru_cache

@lru_cache(maxsize=None)
def simple_multiply(a, b):
    return a * b  # Too simple to benefit from caching

@lru_cache(maxsize=128)
def expensive_computation(n):
    # Complex calculation that benefits from caching
    return sum(i**2 for i in range(n))
```

### Misusing Generators

**Problem**: Generators can't be reused and may be slower for small datasets.

**Solution**: Use generators for large datasets or single-pass iteration only.

```python
data_gen = (x**2 for x in range(10))
sum1 = sum(data_gen)
sum2 = sum(data_gen)  # Returns 0! Generator exhausted

data_list = [x**2 for x in range(10)]
sum1 = sum(data_list)
sum2 = sum(data_list)  # Works correctly

large_gen = (x**2 for x in range(1_000_000))
result = sum(large_gen)
```

### Profiling in Debug Mode

**Problem**: Profiling debug/development code gives inaccurate results.

**Solution**: Profile production-optimized code without debug flags.

```bash
python -m cProfile script.py

python -O -m cProfile script.py  # -O disables assertions
```

### Not Considering Trade-offs

**Problem**: Optimizing for speed may increase memory usage or complexity.

**Solution**: Balance speed, memory, and maintainability based on actual requirements.

```python
def process_large_file_gen(filepath):
    with open(filepath) as f:
        for line in f:
            yield expensive_transform(line)

def process_large_file_list(filepath):
    with open(filepath) as f:
        data = [expensive_transform(line) for line in f]
    return data

```

### Ignoring GIL for CPU-Bound Tasks

**Problem**: Threading doesn't speed up CPU-bound Python code due to GIL.

**Solution**: Use multiprocessing or async I/O based on workload type.

```python
import threading

def cpu_work(n):
    return sum(i**2 for i in range(n))

threads = [threading.Thread(target=cpu_work, args=(1_000_000,)) for _ in range(4)]
for t in threads:
    t.start()
for t in threads:
    t.join()

from multiprocessing import Pool

with Pool(4) as pool:
    results = pool.map(cpu_work, [1_000_000] * 4)
```

## Related Patterns

**Related Tutorial**: See [Intermediate Tutorial - Performance](/en/learn/software-engineering/programming-languages/python/tutorials/intermediate#performance).
**Related Cookbook**: See Cookbook recipe "Performance Optimization".
