---
title: "Advanced Async Patterns"
date: 2025-12-19T00:00:00+07:00
draft: false
description: "Master asynchronous programming patterns in Python with asyncio, concurrent requests, task management, and real-world examples"
weight: 620
tags: ["python", "async", "asyncio", "concurrency", "coroutines", "how-to"]
---

**Need to handle concurrent operations efficiently?** This guide covers advanced asynchronous programming patterns in Python using `asyncio`. Learn when to use async, how to manage concurrent tasks, handle errors, and avoid common pitfalls.

For fundamentals, see the [Intermediate Tutorial](/en/learn/swe/prog-lang/python/tutorials/intermediate). For related concurrency patterns, check [Handle Concurrency Properly](/en/learn/swe/prog-lang/python/how-to/handle-concurrency-properly).

## 🎯 When to Use Async

### Use Async For (I/O-Bound Operations)

- **Network requests** - API calls, web scraping
- **Database queries** - Concurrent DB operations
- **File I/O** - Reading/writing multiple files
- **WebSockets** - Real-time communication

### Don't Use Async For (CPU-Bound Operations)

- **Heavy computations** - Use multiprocessing instead
- **Data processing** - NumPy, Pandas operations
- **Image/video processing** - CPU-intensive work

**Quick Decision**:

- Waiting for external resources? → **Use async**
- Doing heavy calculations? → **Use multiprocessing/threading**

## 🚀 Basic Async Patterns

### Pattern 1: Concurrent API Requests

Fetch multiple URLs concurrently instead of sequentially.

```python
import asyncio
import aiohttp
from typing import List, Dict

async def fetch_url(session: aiohttp.ClientSession, url: str) -> Dict[str, any]:
    """Fetch a single URL asynchronously."""
    try:
        async with session.get(url, timeout=aiohttp.ClientTimeout(total=10)) as response:
            data = await response.json()
            return {"url": url, "status": response.status, "data": data}
    except asyncio.TimeoutError:
        return {"url": url, "error": "Timeout"}
    except Exception as e:
        return {"url": url, "error": str(e)}

async def fetch_multiple_urls(urls: List[str]) -> List[Dict]:
    """Fetch multiple URLs concurrently."""
    async with aiohttp.ClientSession() as session:
        tasks = [fetch_url(session, url) for url in urls]
        results = await asyncio.gather(*tasks)
        return results

# Usage
urls = [
    "https://api.example.com/data1",
    "https://api.example.com/data2",
    "https://api.example.com/data3",
]

results = asyncio.run(fetch_multiple_urls(urls))
for result in results:
    print(f"{result['url']}: {result.get('status', 'error')}")
```

**Performance**: 3 concurrent requests complete in ~1 second instead of ~3 seconds sequentially.

### Pattern 2: Task Management with asyncio.create_task

```python
import asyncio

async def process_item(item: int) -> int:
    """Process a single item."""
    await asyncio.sleep(1)  # Simulate I/O
    return item * 2

async def process_all_items(items: List[int]) -> List[int]:
    """Process multiple items concurrently with task management."""
    # Create tasks immediately (don't wait)
    tasks = [asyncio.create_task(process_item(item)) for item in items]

    # Wait for all tasks to complete
    results = await asyncio.gather(*tasks)
    return results

# Usage
items = [1, 2, 3, 4, 5]
results = asyncio.run(process_all_items(items))
print(results)  # [2, 4, 6, 8, 10] in ~1 second
```

**Key**: `asyncio.create_task()` schedules coroutines immediately, enabling true concurrency.

### Pattern 3: Rate Limiting with Semaphore

Limit concurrent operations to avoid overwhelming resources.

```python
import asyncio
from typing import List

class RateLimiter:
    """Rate limiter using semaphore."""

    def __init__(self, max_concurrent: int):
        self.semaphore = asyncio.Semaphore(max_concurrent)

    async def fetch_with_limit(
        self,
        session: aiohttp.ClientSession,
        url: str
    ) -> Dict:
        """Fetch URL with rate limiting."""
        async with self.semaphore:
            # Only max_concurrent requests run simultaneously
            async with session.get(url) as response:
                return await response.json()

async def fetch_with_rate_limit(urls: List[str], max_concurrent: int = 5):
    """Fetch URLs with maximum concurrent requests limit."""
    limiter = RateLimiter(max_concurrent)

    async with aiohttp.ClientSession() as session:
        tasks = [limiter.fetch_with_limit(session, url) for url in urls]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        return results

# Usage: Only 5 concurrent requests, even with 100 URLs
urls = [f"https://api.example.com/data/{i}" for i in range(100)]
results = asyncio.run(fetch_with_rate_limit(urls, max_concurrent=5))
```

**Why**: Prevents overwhelming servers, respects API rate limits, manages resource usage.

## 🔄 Advanced Patterns

### Pattern 4: Producer-Consumer with Queues

Process items as they arrive without waiting for all to complete.

```python
import asyncio
from asyncio import Queue
from typing import Any

async def producer(queue: Queue, items: List[Any]):
    """Produce items and add to queue."""
    for item in items:
        await queue.put(item)
        print(f"Produced: {item}")
        await asyncio.sleep(0.1)  # Simulate work

    # Signal completion
    await queue.put(None)

async def consumer(queue: Queue, consumer_id: int):
    """Consume items from queue."""
    while True:
        item = await queue.get()

        if item is None:
            # End signal
            queue.task_done()
            break

        # Process item
        print(f"Consumer {consumer_id} processing: {item}")
        await asyncio.sleep(0.5)  # Simulate processing
        queue.task_done()

async def producer_consumer_pattern():
    """Run producer-consumer pattern with multiple consumers."""
    queue = Queue(maxsize=10)
    items = list(range(20))

    # Start producer
    producer_task = asyncio.create_task(producer(queue, items))

    # Start multiple consumers
    consumers = [
        asyncio.create_task(consumer(queue, i))
        for i in range(3)  # 3 concurrent consumers
    ]

    # Wait for producer to finish
    await producer_task

    # Wait for queue to be empty
    await queue.join()

    # Cancel consumers (they're waiting for None signal)
    for c in consumers:
        c.cancel()

# Usage
asyncio.run(producer_consumer_pattern())
```

**Use Case**: Processing stream of data, web scraping, log processing.

### Pattern 5: Timeout and Cancellation

Handle slow operations gracefully.

```python
import asyncio

async def slow_operation():
    """Simulates a slow operation."""
    await asyncio.sleep(10)
    return "Completed"

async def with_timeout():
    """Execute operation with timeout."""
    try:
        # Wait maximum 5 seconds
        result = await asyncio.wait_for(slow_operation(), timeout=5.0)
        print(result)
    except asyncio.TimeoutError:
        print("Operation timed out!")

# Cancel tasks manually
async def cancellable_tasks():
    """Demonstrate task cancellation."""
    task = asyncio.create_task(slow_operation())

    await asyncio.sleep(2)  # Let it run for 2 seconds

    # Cancel the task
    task.cancel()

    try:
        await task
    except asyncio.CancelledError:
        print("Task was cancelled")

# Usage
asyncio.run(with_timeout())
asyncio.run(cancellable_tasks())
```

### Pattern 6: Retry Logic with Exponential Backoff

Retry failed operations with increasing delays.

```python
import asyncio
from typing import Callable, TypeVar, Optional

T = TypeVar('T')

async def retry_with_backoff(
    coro_func: Callable[..., T],
    max_retries: int = 3,
    initial_delay: float = 1.0,
    backoff_factor: float = 2.0,
    *args,
    **kwargs
) -> Optional[T]:
    """
    Retry async function with exponential backoff.

    Args:
        coro_func: Async function to retry
        max_retries: Maximum number of retry attempts
        initial_delay: Initial delay in seconds
        backoff_factor: Multiplier for delay after each retry
    """
    delay = initial_delay

    for attempt in range(max_retries):
        try:
            result = await coro_func(*args, **kwargs)
            return result
        except Exception as e:
            if attempt == max_retries - 1:
                # Last attempt failed
                print(f"All {max_retries} attempts failed")
                raise

            print(f"Attempt {attempt + 1} failed: {e}")
            print(f"Retrying in {delay} seconds...")
            await asyncio.sleep(delay)
            delay *= backoff_factor

# Usage
async def unreliable_api_call():
    """Simulates unreliable API."""
    import random
    if random.random() < 0.7:  # 70% chance of failure
        raise Exception("API Error")
    return "Success"

result = asyncio.run(
    retry_with_backoff(unreliable_api_call, max_retries=5)
)
```

### Pattern 7: Graceful Shutdown

Handle cleanup when program terminates.

```python
import asyncio
import signal

class AsyncApplication:
    """Application with graceful shutdown."""

    def __init__(self):
        self.running = True
        self.tasks = []

    async def worker(self, worker_id: int):
        """Long-running worker."""
        while self.running:
            try:
                print(f"Worker {worker_id} working...")
                await asyncio.sleep(1)
            except asyncio.CancelledError:
                print(f"Worker {worker_id} cancelled")
                break

    def shutdown(self, signum, frame):
        """Signal handler for graceful shutdown."""
        print(f"\nReceived signal {signum}, shutting down...")
        self.running = False

    async def run(self):
        """Main application loop."""
        # Register signal handlers
        signal.signal(signal.SIGINT, self.shutdown)
        signal.signal(signal.SIGTERM, self.shutdown)

        # Start workers
        self.tasks = [
            asyncio.create_task(self.worker(i))
            for i in range(3)
        ]

        # Wait for shutdown signal
        while self.running:
            await asyncio.sleep(0.1)

        # Cancel all tasks
        print("Cancelling tasks...")
        for task in self.tasks:
            task.cancel()

        # Wait for cancellation to complete
        await asyncio.gather(*self.tasks, return_exceptions=True)
        print("Shutdown complete")

# Usage
app = AsyncApplication()
asyncio.run(app.run())
```

## ⚠️ Error Handling

### Pattern 8: Handle Errors in gather()

```python
async def fetch_with_error_handling(urls: List[str]):
    """Fetch URLs and handle errors gracefully."""
    async with aiohttp.ClientSession() as session:
        tasks = [fetch_url(session, url) for url in urls]

        # Option 1: Collect exceptions
        results = await asyncio.gather(*tasks, return_exceptions=True)

        for url, result in zip(urls, results):
            if isinstance(result, Exception):
                print(f"Error fetching {url}: {result}")
            else:
                print(f"Success {url}: {result}")

        # Option 2: Fail fast (stop on first exception)
        try:
            results = await asyncio.gather(*tasks)  # No return_exceptions
        except Exception as e:
            print(f"First error: {e}")
            # Cancel remaining tasks
            for task in tasks:
                task.cancel()
```

### Pattern 9: Context Manager for Async Resources

```python
from contextlib import asynccontextmanager

@asynccontextmanager
async def async_resource_manager(resource_name: str):
    """Async context manager for resource cleanup."""
    print(f"Acquiring {resource_name}")
    resource = {"name": resource_name, "data": []}

    try:
        yield resource
    finally:
        print(f"Releasing {resource_name}")
        # Cleanup code
        resource.clear()

# Usage
async def use_resource():
    async with async_resource_manager("database") as db:
        db["data"].append("item")
        print(f"Using {db['name']}")
    # Automatic cleanup

asyncio.run(use_resource())
```

## 🎯 Real-World Examples

### Example 1: Concurrent Web Scraper

```python
import asyncio
import aiohttp
from bs4 import BeautifulSoup
from typing import List, Dict

class AsyncWebScraper:
    """Concurrent web scraper with rate limiting."""

    def __init__(self, max_concurrent: int = 5):
        self.semaphore = asyncio.Semaphore(max_concurrent)
        self.session = None

    async def __aenter__(self):
        self.session = aiohttp.ClientSession()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.session.close()

    async def fetch_page(self, url: str) -> str:
        """Fetch page content."""
        async with self.semaphore:
            try:
                async with self.session.get(url) as response:
                    return await response.text()
            except Exception as e:
                print(f"Error fetching {url}: {e}")
                return ""

    async def parse_page(self, html: str) -> Dict:
        """Parse HTML content."""
        soup = BeautifulSoup(html, 'html.parser')
        return {
            "title": soup.title.string if soup.title else "",
            "links": [a['href'] for a in soup.find_all('a', href=True)][:10]
        }

    async def scrape_urls(self, urls: List[str]) -> List[Dict]:
        """Scrape multiple URLs concurrently."""
        tasks = [self.fetch_page(url) for url in urls]
        pages = await asyncio.gather(*tasks)

        # Parse pages
        results = []
        for url, html in zip(urls, pages):
            if html:
                data = await self.parse_page(html)
                results.append({"url": url, **data})

        return results

# Usage
async def main():
    urls = ["https://example.com/page1", "https://example.com/page2"]

    async with AsyncWebScraper(max_concurrent=3) as scraper:
        results = await scraper.scrape_urls(urls)
        for result in results:
            print(f"{result['url']}: {result['title']}")

asyncio.run(main())
```

### Example 2: Async Database Operations

```python
import asyncio
import asyncpg  # pip install asyncpg
from typing import List, Dict

class AsyncDatabase:
    """Async database wrapper."""

    def __init__(self, dsn: str):
        self.dsn = dsn
        self.pool = None

    async def connect(self, min_size: int = 10, max_size: int = 20):
        """Create connection pool."""
        self.pool = await asyncpg.create_pool(
            self.dsn,
            min_size=min_size,
            max_size=max_size
        )

    async def close(self):
        """Close connection pool."""
        await self.pool.close()

    async def fetch_user(self, user_id: int) -> Dict:
        """Fetch single user."""
        async with self.pool.acquire() as conn:
            row = await conn.fetchrow(
                "SELECT * FROM users WHERE id = $1",
                user_id
            )
            return dict(row) if row else None

    async def fetch_multiple_users(self, user_ids: List[int]) -> List[Dict]:
        """Fetch multiple users concurrently."""
        tasks = [self.fetch_user(uid) for uid in user_ids]
        users = await asyncio.gather(*tasks)
        return [u for u in users if u is not None]

# Usage
async def main():
    db = AsyncDatabase("postgresql://user:pass@localhost/db")
    await db.connect()

    try:
        # Fetch 100 users concurrently
        user_ids = list(range(1, 101))
        users = await db.fetch_multiple_users(user_ids)
        print(f"Fetched {len(users)} users")
    finally:
        await db.close()

# asyncio.run(main())
```

## 🚫 Common Pitfalls

### Pitfall 1: Blocking Operations in Async Code

```python
# ❌ Bad: Blocking operation in async function
async def bad_async():
    import time
    time.sleep(5)  # Blocks entire event loop!
    return "done"

# ✅ Good: Use async sleep
async def good_async():
    await asyncio.sleep(5)  # Doesn't block
    return "done"

# ✅ Good: Run blocking code in executor
import time

async def run_blocking():
    loop = asyncio.get_event_loop()
    result = await loop.run_in_executor(
        None,  # Use default executor
        time.sleep,  # Blocking function
        5  # Arguments
    )
    return "done"
```

### Pitfall 2: Not Awaiting Coroutines

```python
# ❌ Bad: Coroutine not awaited
async def fetch_data():
    return "data"

async def bad_usage():
    result = fetch_data()  # Returns coroutine, not result!
    print(result)  # <coroutine object>

# ✅ Good: Await the coroutine
async def good_usage():
    result = await fetch_data()
    print(result)  # "data"
```

### Pitfall 3: Sharing Mutable State

```python
# ❌ Bad: Race conditions with shared state
counter = 0

async def increment():
    global counter
    for _ in range(1000):
        counter += 1  # Not atomic!

# ✅ Good: Use asyncio.Lock
counter = 0
lock = asyncio.Lock()

async def safe_increment():
    global counter
    for _ in range(1000):
        async with lock:
            counter += 1
```

## 🎯 Best Practices

1. **Use async for I/O, not CPU**: Async shines with I/O-bound operations
2. **Limit concurrency**: Use Semaphore to avoid overwhelming resources
3. **Handle timeouts**: Use `asyncio.wait_for()` for timeout protection
4. **Clean up resources**: Use context managers and proper shutdown
5. **Error handling**: Use `return_exceptions=True` in `gather()` when appropriate
6. **Avoid blocking**: Never use blocking calls in async functions
7. **Test async code**: Use `pytest-asyncio` for testing

## 📚 See Also

- [Handle Concurrency Properly](/en/learn/swe/prog-lang/python/how-to/handle-concurrency-properly) - Threading and multiprocessing
- [Intermediate Tutorial](/en/learn/swe/prog-lang/python/tutorials/intermediate) - Async fundamentals
- [Official asyncio Docs](https://docs.python.org/3/library/asyncio.html) - Complete reference
- [aiohttp Documentation](https://docs.aiohttp.org/) - Async HTTP client/server
