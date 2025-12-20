---
title: "How to Handle Concurrency Properly"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 1000120
description: "Navigate Python's GIL with threading, multiprocessing, and asyncio"
tags:
  ["python", "concurrency", "threading", "multiprocessing", "asyncio", "gil"]
categories: ["learn"]
---

## Problem

Python's Global Interpreter Lock (GIL) prevents true parallel execution of Python code in threads. Understanding when to use threading, multiprocessing, or asyncio is crucial for performant concurrent Python applications.

This guide shows how to handle concurrency in Python based on workload type.

## Understanding the GIL

The Global Interpreter Lock ensures only one thread executes Python bytecode at a time.

```python
# The GIL means:
# ✅ Threading works for I/O-bound tasks (network, disk, database)
# ❌ Threading doesn't help CPU-bound tasks (computation)
# ✅ Multiprocessing works for CPU-bound tasks
# ✅ asyncio works for I/O-bound tasks (single-threaded)
```

## Threading for I/O-Bound Tasks

Use threading when waiting for external resources (network, disk, database).

```python
import threading
import time
import requests

# ✅ Threading for I/O-bound tasks
def fetch_url(url):
    response = requests.get(url)
    print(f"Fetched {url}: {len(response.content)} bytes")

urls = [
    "https://example.com",
    "https://example.org",
    "https://example.net"
]

# Without threading - sequential (slow)
start = time.time()
for url in urls:
    fetch_url(url)
print(f"Sequential: {time.time() - start:.2f}s")  # ~3s

# With threading - concurrent (fast)
start = time.time()
threads = []
for url in urls:
    thread = threading.Thread(target=fetch_url, args=(url,))
    thread.start()
    threads.append(thread)

for thread in threads:
    thread.join()  # Wait for completion
print(f"Threaded: {time.time() - start:.2f}s")  # ~1s
```

### ThreadPoolExecutor (Easier)

```python
from concurrent.futures import ThreadPoolExecutor, as_completed

# ✅ Thread pool - manages threads automatically
def fetch_url(url):
    response = requests.get(url)
    return url, len(response.content)

urls = ["https://example.com", "https://example.org", "https://example.net"]

# Map pattern - preserves order
with ThreadPoolExecutor(max_workers=3) as executor:
    results = executor.map(fetch_url, urls)
    for url, size in results:
        print(f"{url}: {size} bytes")

# Submit pattern - process as they complete
with ThreadPoolExecutor(max_workers=3) as executor:
    futures = {executor.submit(fetch_url, url): url for url in urls}

    for future in as_completed(futures):
        url = futures[future]
        try:
            result_url, size = future.result()
            print(f"{result_url}: {size} bytes")
        except Exception as e:
            print(f"{url} failed: {e}")
```

### Thread Safety

```python
import threading

# ❌ Race condition - unsafe
counter = 0

def increment():
    global counter
    for _ in range(100000):
        counter += 1  # Not atomic!

threads = [threading.Thread(target=increment) for _ in range(10)]
for t in threads:
    t.start()
for t in threads:
    t.join()

print(counter)  # Wrong! Should be 1,000,000

# ✅ Use locks for thread safety
counter = 0
lock = threading.Lock()

def increment_safe():
    global counter
    for _ in range(100000):
        with lock:  # Synchronized
            counter += 1

threads = [threading.Thread(target=increment_safe) for _ in range(10)]
for t in threads:
    t.start()
for t in threads:
    t.join()

print(counter)  # Correct: 1,000,000

# ✅ Thread-safe queue for producer-consumer
from queue import Queue

def producer(queue, items):
    for item in items:
        queue.put(item)
    queue.put(None)  # Sentinel

def consumer(queue):
    while True:
        item = queue.get()
        if item is None:
            break
        process(item)
        queue.task_done()

q = Queue()
prod = threading.Thread(target=producer, args=(q, range(100)))
cons = threading.Thread(target=consumer, args=(q,))

prod.start()
cons.start()
prod.join()
cons.join()
```

## Multiprocessing for CPU-Bound Tasks

Use multiprocessing to bypass the GIL for CPU-intensive work.

```python
import multiprocessing
import time

# ❌ Threading doesn't help CPU-bound tasks (GIL)
def cpu_bound(n):
    return sum(i ** 2 for i in range(n))

# With threading - slow (GIL limits to one core)
start = time.time()
with ThreadPoolExecutor(max_workers=4) as executor:
    results = list(executor.map(cpu_bound, [10_000_000] * 4))
print(f"Threading: {time.time() - start:.2f}s")  # ~8s on 4 cores

# ✅ With multiprocessing - fast (uses all cores)
from concurrent.futures import ProcessPoolExecutor

start = time.time()
with ProcessPoolExecutor(max_workers=4) as executor:
    results = list(executor.map(cpu_bound, [10_000_000] * 4))
print(f"Multiprocessing: {time.time() - start:.2f}s")  # ~2s on 4 cores
```

### Process Pool Patterns

```python
from multiprocessing import Pool

# ✅ Map pattern
def square(x):
    return x ** 2

with Pool(processes=4) as pool:
    results = pool.map(square, range(10))
    print(results)

# ✅ Async map (non-blocking)
with Pool(processes=4) as pool:
    result = pool.map_async(square, range(10))
    # Do other work
    print(result.get())  # Wait for results

# ✅ Starmap for multiple arguments
def multiply(x, y):
    return x * y

with Pool(processes=4) as pool:
    results = pool.starmap(multiply, [(1, 2), (3, 4), (5, 6)])
    print(results)  # [2, 12, 30]
```

### Inter-Process Communication

```python
from multiprocessing import Queue, Process

# ✅ Queue for process communication
def worker(queue):
    while True:
        item = queue.get()
        if item is None:
            break
        result = process_item(item)
        print(result)

q = Queue()
processes = [Process(target=worker, args=(q,)) for _ in range(4)]

for p in processes:
    p.start()

# Send work
for item in range(100):
    q.put(item)

# Send stop signals
for _ in range(4):
    q.put(None)

for p in processes:
    p.join()
```

## Asyncio for Async I/O

Use asyncio for I/O-bound tasks with a single-threaded event loop.

```python
import asyncio
import aiohttp

# ✅ Async I/O with asyncio
async def fetch_url(session, url):
    async with session.get(url) as response:
        content = await response.read()
        print(f"Fetched {url}: {len(content)} bytes")
        return content

async def main():
    urls = [
        "https://example.com",
        "https://example.org",
        "https://example.net"
    ]

    async with aiohttp.ClientSession() as session:
        # Concurrent execution
        tasks = [fetch_url(session, url) for url in urls]
        results = await asyncio.gather(*tasks)
        print(f"Fetched {len(results)} URLs")

# Run async function
asyncio.run(main())
```

### Async Patterns

```python
# ✅ Async context managers
async def fetch_with_timeout(url, timeout=5):
    async with aiohttp.ClientSession() as session:
        try:
            async with session.get(url, timeout=timeout) as response:
                return await response.text()
        except asyncio.TimeoutError:
            return None

# ✅ Gather with error handling
async def fetch_all(urls):
    async with aiohttp.ClientSession() as session:
        tasks = [fetch_url(session, url) for url in urls]
        results = await asyncio.gather(*tasks, return_exceptions=True)

        for url, result in zip(urls, results):
            if isinstance(result, Exception):
                print(f"{url} failed: {result}")
            else:
                print(f"{url} succeeded")

# ✅ as_completed for processing results as they arrive
async def fetch_as_completed(urls):
    async with aiohttp.ClientSession() as session:
        tasks = [fetch_url(session, url) for url in urls]

        for coro in asyncio.as_completed(tasks):
            result = await coro
            # Process result immediately
            print(f"Completed: {len(result)} bytes")
```

### Async/Await Best Practices

```python
# ✅ Always await async functions
async def good():
    result = await async_operation()  # ✓ Waits for completion
    return result

# ❌ Forgetting await
async def bad():
    result = async_operation()  # ✗ Returns coroutine, doesn't execute!
    return result

# ✅ Run CPU-bound tasks in executor
async def mixed_workload():
    loop = asyncio.get_event_loop()

    # I/O-bound - run directly
    response = await fetch_url("https://example.com")

    # CPU-bound - run in thread pool
    result = await loop.run_in_executor(None, cpu_intensive_work, data)

    return result
```

## Choosing the Right Concurrency Model

```python
# Decision guide:

# I/O-bound (network, disk, database):
# - Few tasks: asyncio (single-threaded, low overhead)
# - Many tasks: ThreadPoolExecutor (simpler than asyncio)
# - Mixed sync/async libraries: threading

# CPU-bound (computation, image processing):
# - Always use multiprocessing (bypasses GIL)

# Mixed workload:
# - Use asyncio for I/O, run_in_executor for CPU tasks
# - Or multiprocessing with queues
```

### Examples by Use Case

```python
# ✅ Web scraping (I/O-bound)
from concurrent.futures import ThreadPoolExecutor
# Or asyncio with aiohttp

# ✅ Image processing (CPU-bound)
from concurrent.futures import ProcessPoolExecutor

# ✅ Web server (I/O-bound, many connections)
# Use asyncio (aiohttp, FastAPI)

# ✅ Data processing pipeline
# Use multiprocessing.Pool

# ✅ Database queries (I/O-bound)
from concurrent.futures import ThreadPoolExecutor
# Or asyncio with aiomysql/asyncpg
```

## Summary

Python's Global Interpreter Lock limits one thread to execute Python bytecode at a time, making threading ineffective for CPU-bound tasks but perfectly suitable for I/O-bound work. When threads wait for network responses, disk access, or database queries, the GIL releases, allowing other threads to run. This makes threading ideal for tasks that spend most of their time waiting.

ThreadPoolExecutor simplifies thread management with automatic thread creation, task distribution, and result collection. The map() method preserves order and blocks until all tasks complete, while submit() enables processing results as they become available through as_completed(). Thread pools handle thread lifecycle automatically, preventing resource leaks from manual thread management.

Multiprocessing bypasses the GIL by running separate Python processes, each with its own interpreter and memory space. This enables true parallel execution on multiple cores for CPU-intensive tasks like data processing, image manipulation, or mathematical computation. ProcessPoolExecutor provides the same interface as ThreadPoolExecutor but creates processes instead of threads.

Asyncio uses a single-threaded event loop to handle many I/O operations concurrently without threads. Async functions yield control during I/O operations, allowing other async functions to run. This approach works well for I/O-bound tasks with many concurrent operations like web servers or API clients. The async/await syntax makes asynchronous code readable, though it requires async-compatible libraries.

Choose threading for I/O-bound tasks with moderate concurrency, multiprocessing for CPU-bound tasks that need parallel execution, and asyncio for I/O-bound tasks with high concurrency. For mixed workloads, asyncio can run CPU-bound work in thread or process pools through run_in_executor().

Thread safety requires synchronization when multiple threads access shared data. Locks prevent race conditions but must be acquired and released correctly to avoid deadlocks. Queue provides thread-safe communication, making producer-consumer patterns simple. For processes, multiprocessing.Queue enables safe inter-process communication.

Understanding Python's concurrency models prevents common mistakes like using threading for CPU-bound work or forgetting that asyncio requires async-compatible libraries. Match the concurrency approach to your workload type for optimal performance.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/python/how-to/handle-errors-effectively)
- [How to Write Pythonic Code](/en/learn/swe/prog-lang/python/how-to/write-pythonic-code)
