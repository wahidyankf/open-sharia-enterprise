---
title: Advanced Concurrency Patterns
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 625
description: Master CompletableFuture, virtual threads, and concurrent collections for scalable Java applications
tags:
  ["java", "concurrency", "completablefuture", "virtual-threads", "patterns"]
---

## CompletableFuture Composition

### Basic Async Operations

```java
import java.util.concurrent.*;

// Async supply
CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
    // Long-running task
    return fetchDataFromAPI();
});

// Async run (void result)
CompletableFuture<Void> voidFuture = CompletableFuture.runAsync(() -> {
    cleanupTemporaryFiles();
});

// With custom executor
ExecutorService executor = Executors.newFixedThreadPool(10);
CompletableFuture<String> customFuture = CompletableFuture.supplyAsync(
    () -> fetchData(),
    executor
);
```

### Chaining Operations

```java
// Sequential chaining
CompletableFuture<String> chain = CompletableFuture.supplyAsync(() -> {
    return fetchUserId();
})
.thenApply(userId -> {
    // Transform result
    return fetchUserData(userId);
})
.thenApply(userData -> {
    // Transform again
    return processUserData(userData);
})
.thenAccept(result -> {
    // Consume final result
    System.out.println("Result: " + result);
});

// Async chaining (each stage runs async)
CompletableFuture<String> asyncChain = CompletableFuture.supplyAsync(() -> {
    return fetchUserId();
})
.thenApplyAsync(userId -> {
    return fetchUserData(userId);
})
.thenApplyAsync(userData -> {
    return processUserData(userData);
});
```

### Combining Multiple Futures

```java
// Combine two futures
CompletableFuture<String> future1 = CompletableFuture.supplyAsync(() -> "Hello");
CompletableFuture<String> future2 = CompletableFuture.supplyAsync(() -> "World");

CompletableFuture<String> combined = future1.thenCombine(future2,
    (s1, s2) -> s1 + " " + s2
);

// Both complete
CompletableFuture<Void> both = future1.thenAcceptBoth(future2,
    (s1, s2) -> System.out.println(s1 + " " + s2)
);

// Either complete
CompletableFuture<String> either = future1.applyToEither(future2,
    result -> result.toUpperCase()
);

// Wait for all
CompletableFuture<String> f1 = CompletableFuture.supplyAsync(() -> "One");
CompletableFuture<String> f2 = CompletableFuture.supplyAsync(() -> "Two");
CompletableFuture<String> f3 = CompletableFuture.supplyAsync(() -> "Three");

CompletableFuture<Void> allOf = CompletableFuture.allOf(f1, f2, f3);
allOf.thenRun(() -> {
    // All completed
    System.out.println("All done");
});

// Wait for any
CompletableFuture<Object> anyOf = CompletableFuture.anyOf(f1, f2, f3);
anyOf.thenAccept(result -> {
    System.out.println("First done: " + result);
});
```

### Error Handling

```java
CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
    if (Math.random() > 0.5) {
        throw new RuntimeException("Random failure");
    }
    return "Success";
})
.exceptionally(ex -> {
    // Handle exception
    System.err.println("Error: " + ex.getMessage());
    return "Fallback value";
})
.handle((result, ex) -> {
    // Handle both result and exception
    if (ex != null) {
        return "Error: " + ex.getMessage();
    }
    return "Success: " + result;
});

// Catch specific exceptions
future.exceptionallyCompose(ex -> {
    if (ex instanceof IOException) {
        return CompletableFuture.supplyAsync(() -> retryOperation());
    }
    return CompletableFuture.failedFuture(ex);
});
```

### Timeout and Cancellation

```java
// Timeout (Java 9+)
CompletableFuture<String> timeoutFuture = CompletableFuture.supplyAsync(() -> {
    sleep(5000);
    return "Done";
})
.orTimeout(2, TimeUnit.SECONDS)
.exceptionally(ex -> {
    if (ex instanceof TimeoutException) {
        return "Timed out";
    }
    return "Error";
});

// Complete with timeout value (Java 9+)
CompletableFuture<String> completeOnTimeout = CompletableFuture.supplyAsync(() -> {
    sleep(5000);
    return "Done";
})
.completeOnTimeout("Default value", 2, TimeUnit.SECONDS);

// Manual cancellation
CompletableFuture<String> cancellable = CompletableFuture.supplyAsync(() -> {
    for (int i = 0; i < 100; i++) {
        if (Thread.currentThread().isInterrupted()) {
            throw new CancellationException();
        }
        sleep(100);
    }
    return "Completed";
});

// Cancel after 1 second
CompletableFuture.delayedExecutor(1, TimeUnit.SECONDS).execute(() -> {
    cancellable.cancel(true);
});
```

## Virtual Threads (Java 21+)

### Creating Virtual Threads

```java
// Traditional platform thread
Thread platformThread = new Thread(() -> {
    System.out.println("Platform thread: " + Thread.currentThread());
});
platformThread.start();

// Virtual thread
Thread virtualThread = Thread.startVirtualThread(() -> {
    System.out.println("Virtual thread: " + Thread.currentThread());
});

// Virtual thread with builder
Thread.Builder.OfVirtual builder = Thread.ofVirtual().name("worker-", 0);
Thread vt = builder.start(() -> {
    System.out.println("Named virtual thread");
});

// Virtual thread factory
ThreadFactory factory = Thread.ofVirtual().factory();
Thread t1 = factory.newThread(() -> task1());
Thread t2 = factory.newThread(() -> task2());
t1.start();
t2.start();
```

### ExecutorService with Virtual Threads

```java
// Virtual thread per task executor
try (ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor()) {
    // Submit many tasks - each gets own virtual thread
    List<Future<String>> futures = new ArrayList<>();
    for (int i = 0; i < 10000; i++) {
        final int taskId = i;
        Future<String> future = executor.submit(() -> {
            // Simulate I/O
            Thread.sleep(1000);
            return "Task " + taskId + " completed";
        });
        futures.add(future);
    }

    // Collect results
    for (Future<String> future : futures) {
        System.out.println(future.get());
    }
} // Auto-shutdown with try-with-resources

// Structured concurrency (preview)
try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
    Future<String> user = scope.fork(() -> fetchUser());
    Future<String> order = scope.fork(() -> fetchOrder());

    scope.join();  // Wait for all
    scope.throwIfFailed();  // Propagate exception

    String result = user.resultNow() + ", " + order.resultNow();
    System.out.println(result);
}
```

### When to Use Virtual Threads

```java
// ✅ Good use case: I/O-bound operations
try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
    // Handle many concurrent HTTP requests
    for (int i = 0; i < 100000; i++) {
        executor.submit(() -> {
            String data = httpClient.get("https://api.example.com/data");
            processData(data);
        });
    }
}

// ✅ Good: Database queries
try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
    List<CompletableFuture<User>> futures = userIds.stream()
        .map(id -> CompletableFuture.supplyAsync(
            () -> database.findUser(id),
            executor
        ))
        .toList();

    CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
}

// ❌ Not ideal: CPU-bound operations
// Use platform threads or parallel streams instead
try (var executor = Executors.newFixedThreadPool(
        Runtime.getRuntime().availableProcessors())) {
    // CPU-intensive work
    executor.submit(() -> heavyComputation());
}
```

## Concurrent Collections

### ConcurrentHashMap

```java
import java.util.concurrent.*;

// Thread-safe map
ConcurrentHashMap<String, Integer> map = new ConcurrentHashMap<>();

// Atomic operations
map.putIfAbsent("key", 1);
map.computeIfAbsent("key", k -> expensiveComputation(k));
map.computeIfPresent("key", (k, v) -> v + 1);
map.merge("key", 1, Integer::sum);

// Atomic update
map.compute("key", (k, v) -> {
    if (v == null) return 1;
    return v + 1;
});

// Bulk operations
map.forEach((key, value) -> {
    System.out.println(key + ": " + value);
});

map.replaceAll((key, value) -> value * 2);

// Parallel operations (threshold: 1000)
map.forEach(1000, (key, value) -> {
    System.out.println(key + ": " + value);
});

long sum = map.reduceValues(1000, Integer::intValue, 0, Long::sum);
```

### CopyOnWriteArrayList

```java
// Thread-safe list for read-heavy workloads
CopyOnWriteArrayList<String> list = new CopyOnWriteArrayList<>();

// Write operations copy entire array (expensive)
list.add("item1");
list.add("item2");

// Read operations no locking (fast)
for (String item : list) {
    System.out.println(item);
}

// Snapshot iteration (won't see concurrent modifications)
Iterator<String> iterator = list.iterator();
while (iterator.hasNext()) {
    String item = iterator.next();
    // Safe even if list modified concurrently
}

// Use case: Event listeners
class EventBus {
    private final CopyOnWriteArrayList<EventListener> listeners =
        new CopyOnWriteArrayList<>();

    public void addListener(EventListener listener) {
        listeners.add(listener);  // Infrequent
    }

    public void fireEvent(Event event) {
        for (EventListener listener : listeners) {  // Frequent
            listener.onEvent(event);
        }
    }
}
```

### BlockingQueue

```java
// Producer-consumer pattern
BlockingQueue<String> queue = new ArrayBlockingQueue<>(100);

// Producer thread
Thread producer = new Thread(() -> {
    try {
        for (int i = 0; i < 100; i++) {
            String item = "Item " + i;
            queue.put(item);  // Blocks if queue full
            System.out.println("Produced: " + item);
        }
    } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
    }
});

// Consumer thread
Thread consumer = new Thread(() -> {
    try {
        while (true) {
            String item = queue.take();  // Blocks if queue empty
            System.out.println("Consumed: " + item);
            processItem(item);
        }
    } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
    }
});

producer.start();
consumer.start();

// With timeout
String item = queue.poll(5, TimeUnit.SECONDS);  // Returns null if timeout
queue.offer("item", 5, TimeUnit.SECONDS);  // Returns false if timeout

// Different queue types
BlockingQueue<String> linked = new LinkedBlockingQueue<>();  // Unbounded
BlockingQueue<String> priority = new PriorityBlockingQueue<>();  // Priority order
BlockingQueue<String> sync = new SynchronousQueue<>();  // Zero capacity
```

## Lock-Free Algorithms

### Atomic Variables

```java
import java.util.concurrent.atomic.*;

// AtomicInteger
AtomicInteger counter = new AtomicInteger(0);

// Atomic operations
int current = counter.get();
counter.set(10);
int previous = counter.getAndSet(20);
int incremented = counter.incrementAndGet();
int updated = counter.addAndGet(5);

// Compare and swap (CAS)
boolean success = counter.compareAndSet(20, 30);

// Update with function
counter.updateAndGet(value -> value * 2);
counter.accumulateAndGet(5, (current, update) -> current + update);

// AtomicReference
AtomicReference<User> userRef = new AtomicReference<>(initialUser);
User user = userRef.get();
userRef.set(newUser);

// CAS for complex updates
User oldUser, newUser;
do {
    oldUser = userRef.get();
    newUser = updateUser(oldUser);
} while (!userRef.compareAndSet(oldUser, newUser));

// AtomicStampedReference (avoid ABA problem)
AtomicStampedReference<String> stamped =
    new AtomicStampedReference<>("initial", 0);

int[] stampHolder = new int[1];
String value = stamped.get(stampHolder);
int stamp = stampHolder[0];

stamped.compareAndSet(value, "new", stamp, stamp + 1);
```

### Lock-Free Stack

```java
public class LockFreeStack<T> {
    private static class Node<T> {
        final T value;
        Node<T> next;

        Node(T value) {
            this.value = value;
        }
    }

    private final AtomicReference<Node<T>> head = new AtomicReference<>();

    public void push(T value) {
        Node<T> newHead = new Node<>(value);
        Node<T> oldHead;
        do {
            oldHead = head.get();
            newHead.next = oldHead;
        } while (!head.compareAndSet(oldHead, newHead));
    }

    public T pop() {
        Node<T> oldHead;
        Node<T> newHead;
        do {
            oldHead = head.get();
            if (oldHead == null) {
                return null;
            }
            newHead = oldHead.next;
        } while (!head.compareAndSet(oldHead, newHead));
        return oldHead.value;
    }
}
```

## Thread Pool Best Practices

### Choosing Thread Pool Size

```java
// CPU-bound tasks
int cpuBound = Runtime.getRuntime().availableProcessors();
ExecutorService cpuExecutor = Executors.newFixedThreadPool(cpuBound);

// I/O-bound tasks (higher thread count)
int ioBound = Runtime.getRuntime().availableProcessors() * 2;
ExecutorService ioExecutor = Executors.newFixedThreadPool(ioBound);

// Or use virtual threads for I/O
ExecutorService virtualExecutor = Executors.newVirtualThreadPerTaskExecutor();

// Custom thread pool with tuning
ThreadPoolExecutor customExecutor = new ThreadPoolExecutor(
    10,  // Core pool size
    50,  // Max pool size
    60L, TimeUnit.SECONDS,  // Keep alive time
    new LinkedBlockingQueue<>(1000),  // Work queue
    new ThreadFactory() {
        private final AtomicInteger counter = new AtomicInteger();
        @Override
        public Thread newThread(Runnable r) {
            Thread t = new Thread(r, "worker-" + counter.incrementAndGet());
            t.setDaemon(false);
            return t;
        }
    },
    new ThreadPoolExecutor.CallerRunsPolicy()  // Rejection policy
);
```

### Proper Shutdown

```java
ExecutorService executor = Executors.newFixedThreadPool(10);

try {
    // Submit tasks
    executor.submit(() -> task1());
    executor.submit(() -> task2());
} finally {
    // Graceful shutdown
    executor.shutdown();
    try {
        // Wait for termination
        if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
            // Force shutdown
            executor.shutdownNow();
            // Wait again
            if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
                System.err.println("Executor did not terminate");
            }
        }
    } catch (InterruptedException e) {
        executor.shutdownNow();
        Thread.currentThread().interrupt();
    }
}
```

## Related Resources

**Learn more**:

- [Java Concurrency in Practice](https://jcip.net/) - Definitive concurrency guide
- [Glossary](/en/learn/swe/prog-lang/java/reference/glossary) - Concurrency terms

**Related guides**:

- [Exception Handling](/en/learn/swe/prog-lang/java/how-to/exception-handling) - Error handling patterns
- [Write Effective Tests](/en/learn/swe/prog-lang/java/how-to/write-effective-tests) - Testing concurrent code
