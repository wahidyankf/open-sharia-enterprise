---
title: "Concurrency and Parallelism"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Comprehensive guide to concurrency and parallelism in Java from thread fundamentals to virtual threads
weight: 10000017
tags: ["java", "concurrency", "threads", "virtual-threads", "executorservice", "parallelism"]
---

## Why Concurrency Matters

Concurrency enables applications to handle multiple tasks simultaneously, improving responsiveness and throughput. Modern applications require concurrent programming to utilize multi-core processors efficiently and handle I/O-bound operations without blocking.

**Core Benefits**:

- **Responsiveness**: UI remains responsive during long operations
- **Throughput**: Process multiple requests simultaneously
- **Resource utilization**: Utilize multi-core processors effectively
- **Scalability**: Handle increasing load by adding cores
- **I/O efficiency**: Continue work while waiting for I/O

**Problem**: Single-threaded programs can only execute one task at a time, wasting CPU resources during I/O operations and failing to utilize multiple cores.

**Solution**: Use concurrency primitives (threads, executors, locks) to coordinate multiple execution contexts safely and efficiently.

## Concurrency vs Parallelism

**Concurrency**: Multiple tasks making progress (not necessarily simultaneously). Tasks interleave execution on shared resources.

**Parallelism**: Multiple tasks executing simultaneously on different processors/cores.

**Example distinction**:

- **Concurrent**: Single-core CPU switching between two tasks rapidly (context switching)
- **Parallel**: Dual-core CPU executing two tasks simultaneously on separate cores

## Thread Basics (Standard Library)

Java provides built-in threading through the Thread class and Runnable interface. Understand thread fundamentals before using higher-level abstractions.

### Creating Threads

Two approaches: extend Thread or implement Runnable.

**Extending Thread**:

```java
public class WorkerThread extends Thread {
    private final String taskName;

    public WorkerThread(String taskName) {
        this.taskName = taskName;
    }

    @Override
    public void run() {
        System.out.println(taskName + " started on thread: " + Thread.currentThread().getName());

        for (int i = 1; i <= 5; i++) {
            System.out.println(taskName + " - Step " + i);
            try {
                Thread.sleep(500);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }

        System.out.println(taskName + " completed");
    }
}

// Usage
public class ThreadExample {
    public static void main(String[] args) {
        WorkerThread thread1 = new WorkerThread("Task-1");
        WorkerThread thread2 = new WorkerThread("Task-2");

        thread1.start(); // Starts new thread, calls run()
        thread2.start();

        System.out.println("Main thread continues...");
    }
}
```

**Implementing Runnable (Preferred)**:

```java
public class WorkerTask implements Runnable {
    private final String taskName;

    public WorkerTask(String taskName) {
        this.taskName = taskName;
    }

    @Override
    public void run() {
        System.out.println(taskName + " executing on: " + Thread.currentThread().getName());

        for (int i = 1; i <= 5; i++) {
            System.out.println(taskName + " - Iteration " + i);
            try {
                Thread.sleep(300);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return;
            }
        }
    }
}

// Usage
public class RunnableExample {
    public static void main(String[] args) {
        Thread thread1 = new Thread(new WorkerTask("Worker-1"));
        Thread thread2 = new Thread(new WorkerTask("Worker-2"));

        thread1.start();
        thread2.start();

        // Lambda syntax (Java 8+)
        Thread thread3 = new Thread(() -> {
            System.out.println("Lambda task running");
        });
        thread3.start();
    }
}
```

**Why Runnable is preferred**:

- Separates task definition from thread execution
- Allows implementing other interfaces (Java single inheritance)
- More flexible - can be used with ExecutorService
- Better separation of concerns

### Thread Lifecycle

Threads transition through multiple states during their lifetime.

**Thread states**:

| State             | Description                             | Transitions From            | Transitions To                  |
| ----------------- | --------------------------------------- | --------------------------- | ------------------------------- |
| **NEW**           | Thread created but not started          | -                           | RUNNABLE (via start())          |
| **RUNNABLE**      | Executing or ready to execute           | NEW, WAITING, TIMED_WAITING | BLOCKED, WAITING, TERMINATED    |
| **BLOCKED**       | Waiting to acquire lock                 | RUNNABLE                    | RUNNABLE (when lock acquired)   |
| **WAITING**       | Waiting indefinitely for another thread | RUNNABLE                    | RUNNABLE (via notify/interrupt) |
| **TIMED_WAITING** | Waiting for specified time              | RUNNABLE                    | RUNNABLE (timeout or notify)    |
| **TERMINATED**    | Completed execution                     | RUNNABLE                    | -                               |

**Lifecycle example**:

```java
public class ThreadLifecycleDemo {
    public static void main(String[] args) throws InterruptedException {
        Object lock = new Object();

        Thread thread = new Thread(() -> {
            synchronized (lock) {
                try {
                    System.out.println("Thread entering WAITING state");
                    lock.wait(); // WAITING
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });

        System.out.println("State: " + thread.getState()); // NEW

        thread.start();
        System.out.println("State: " + thread.getState()); // RUNNABLE

        Thread.sleep(100); // Give thread time to wait
        System.out.println("State: " + thread.getState()); // WAITING

        synchronized (lock) {
            lock.notify(); // Wake up thread
        }

        thread.join(); // Wait for completion
        System.out.println("State: " + thread.getState()); // TERMINATED
    }
}
```

### Thread Operations

Common thread operations for coordination and control.

**Thread.sleep()**:

```java
public class SleepExample {
    public static void main(String[] args) {
        Thread worker = new Thread(() -> {
            System.out.println("Worker starting...");

            try {
                // Sleep for 2 seconds (TIMED_WAITING)
                Thread.sleep(2000);
            } catch (InterruptedException e) {
                System.out.println("Worker interrupted during sleep");
                Thread.currentThread().interrupt();
                return;
            }

            System.out.println("Worker completed");
        });

        worker.start();
        System.out.println("Main thread continues immediately");
    }
}
```

**Thread.yield()**:

```java
public class YieldExample {
    public static void main(String[] args) {
        Thread highPriority = new Thread(() -> {
            for (int i = 0; i < 5; i++) {
                System.out.println("High priority task - " + i);
                Thread.yield(); // Hint to scheduler: let others run
            }
        });

        Thread lowPriority = new Thread(() -> {
            for (int i = 0; i < 5; i++) {
                System.out.println("Low priority task - " + i);
            }
        });

        highPriority.start();
        lowPriority.start();
    }
}
```

**Thread.join()** (wait for completion):

```java
public class JoinExample {
    public static void main(String[] args) throws InterruptedException {
        Thread dataLoader = new Thread(() -> {
            System.out.println("Loading data...");
            try {
                Thread.sleep(2000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            System.out.println("Data loaded");
        });

        dataLoader.start();

        System.out.println("Main waiting for data...");
        dataLoader.join(); // Block until dataLoader completes
        System.out.println("Main proceeding with loaded data");
    }
}
```

### Daemon Threads

Daemon threads run in background and don't prevent JVM shutdown.

**Pattern**:

```java
public class DaemonThreadExample {
    public static void main(String[] args) throws InterruptedException {
        Thread userThread = new Thread(() -> {
            try {
                Thread.sleep(3000);
                System.out.println("User thread completed");
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        Thread daemonThread = new Thread(() -> {
            while (true) {
                System.out.println("Daemon thread running...");
                try {
                    Thread.sleep(500);
                } catch (InterruptedException e) {
                    break;
                }
            }
        });

        daemonThread.setDaemon(true); // Mark as daemon BEFORE start()

        userThread.start();
        daemonThread.start();

        // JVM waits for userThread but NOT daemonThread
    }
}
```

**Daemon thread characteristics**:

- Background services (garbage collection, monitoring)
- JVM terminates immediately when only daemon threads remain
- Cannot prevent JVM shutdown
- Must set daemon status before start()

### Why Raw Threads Don't Scale

**Limitations of manual thread management**:

1. **Resource overhead**: Each thread consumes memory (stack size ~1MB)
2. **Context switching**: OS switching between thousands of threads is expensive
3. **Thread creation cost**: Creating/destroying threads has overhead
4. **No task queuing**: Cannot queue work when threads are busy
5. **Manual lifecycle**: Must manage start, join, exception handling
6. **No resource pooling**: Repeatedly create/destroy threads

**Before**: Manual thread creation for each task
**After**: Thread pools with ExecutorService for managed concurrency

## Synchronization (Standard Library)

Synchronization prevents race conditions when multiple threads access shared mutable state.

### Synchronized Keyword

Use synchronized to ensure mutual exclusion.

**Method-level synchronization**:

```java
public class Counter {
    private int count = 0;

    // Synchronized method (locks this object)
    public synchronized void increment() {
        count++; // Atomic operation under lock
    }

    public synchronized int getCount() {
        return count;
    }

    public static void main(String[] args) throws InterruptedException {
        Counter counter = new Counter();

        // Create 10 threads incrementing 1000 times each
        Thread[] threads = new Thread[10];
        for (int i = 0; i < 10; i++) {
            threads[i] = new Thread(() -> {
                for (int j = 0; j < 1000; j++) {
                    counter.increment();
                }
            });
            threads[i].start();
        }

        // Wait for all threads
        for (Thread thread : threads) {
            thread.join();
        }

        System.out.println("Final count: " + counter.getCount()); // 10000 (correct)
    }
}
```

**Block-level synchronization**:

```java
public class BankAccount {
    private double balance = 0.0;
    private final Object balanceLock = new Object();

    public void deposit(double amount) {
        // Only synchronize critical section
        synchronized (balanceLock) {
            balance += amount;
        }
    }

    public void withdraw(double amount) {
        synchronized (balanceLock) {
            if (balance >= amount) {
                balance -= amount;
            } else {
                throw new IllegalStateException("Insufficient funds");
            }
        }
    }

    public double getBalance() {
        synchronized (balanceLock) {
            return balance;
        }
    }
}
```

**Synchronized on class**:

```java
public class IdGenerator {
    private static int nextId = 1;

    // Class-level lock (locks IdGenerator.class)
    public static synchronized int generateId() {
        return nextId++;
    }

    // Equivalent to:
    public static int generateIdExplicit() {
        synchronized (IdGenerator.class) {
            return nextId++;
        }
    }
}
```

### Wait, Notify, NotifyAll

Use wait/notify for thread coordination with condition variables.

**Producer-Consumer pattern**:

```java
import java.util.LinkedList;
import java.util.Queue;

public class ProducerConsumer {
    private final Queue<Integer> queue = new LinkedList<>();
    private final int capacity = 5;
    private final Object lock = new Object();

    public void produce(int value) throws InterruptedException {
        synchronized (lock) {
            // Wait while queue is full
            while (queue.size() == capacity) {
                System.out.println("Queue full, producer waiting...");
                lock.wait(); // Release lock and wait
            }

            queue.add(value);
            System.out.println("Produced: " + value + ", Queue size: " + queue.size());

            // Notify consumers
            lock.notifyAll();
        }
    }

    public int consume() throws InterruptedException {
        synchronized (lock) {
            // Wait while queue is empty
            while (queue.isEmpty()) {
                System.out.println("Queue empty, consumer waiting...");
                lock.wait();
            }

            int value = queue.poll();
            System.out.println("Consumed: " + value + ", Queue size: " + queue.size());

            // Notify producers
            lock.notifyAll();

            return value;
        }
    }

    public static void main(String[] args) {
        ProducerConsumer pc = new ProducerConsumer();

        // Producer thread
        Thread producer = new Thread(() -> {
            for (int i = 1; i <= 10; i++) {
                try {
                    pc.produce(i);
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });

        // Consumer thread
        Thread consumer = new Thread(() -> {
            for (int i = 1; i <= 10; i++) {
                try {
                    pc.consume();
                    Thread.sleep(200);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });

        producer.start();
        consumer.start();
    }
}
```

**Wait/notify mechanics**:

- **wait()**: Releases lock and enters WAITING state
- **notify()**: Wakes up one waiting thread
- **notifyAll()**: Wakes up all waiting threads
- Must be called within synchronized block
- Always use while loop (not if) for condition check

### Why Intrinsic Locking is Limited

**Limitations of synchronized**:

1. **No timeout**: Cannot timeout waiting for lock
2. **Not interruptible**: Cannot interrupt thread waiting for lock
3. **Single condition**: Only one wait/notify condition per lock
4. **No try-lock**: Cannot attempt to acquire lock without blocking
5. **Block-scoped**: Cannot acquire lock in one method, release in another

**Before**: synchronized with limitations
**After**: Explicit locks (ReentrantLock) with advanced features

## Explicit Locks (Standard Library)

java.util.concurrent.locks provides more flexible locking mechanisms.

### ReentrantLock

Explicit lock with additional capabilities beyond synchronized.

**Basic pattern**:

```java
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class BankAccountWithLock {
    private double balance = 0.0;
    private final Lock lock = new ReentrantLock();

    public void deposit(double amount) {
        lock.lock();
        try {
            balance += amount;
        } finally {
            lock.unlock(); // ALWAYS in finally
        }
    }

    public void withdraw(double amount) {
        lock.lock();
        try {
            if (balance >= amount) {
                balance -= amount;
            } else {
                throw new IllegalStateException("Insufficient funds");
            }
        } finally {
            lock.unlock();
        }
    }

    public double getBalance() {
        lock.lock();
        try {
            return balance;
        } finally {
            lock.unlock();
        }
    }
}
```

**tryLock() with timeout**:

```java
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class TryLockExample {
    private final Lock lock = new ReentrantLock();

    public void performOperation() {
        boolean acquired = false;
        try {
            // Try to acquire lock for 5 seconds
            acquired = lock.tryLock(5, TimeUnit.SECONDS);

            if (acquired) {
                System.out.println("Lock acquired, performing operation");
                // Critical section
            } else {
                System.out.println("Could not acquire lock, aborting");
            }

        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        } finally {
            if (acquired) {
                lock.unlock();
            }
        }
    }
}
```

**Interruptible lock acquisition**:

```java
public class InterruptibleLockExample {
    private final Lock lock = new ReentrantLock();

    public void performTask() throws InterruptedException {
        lock.lockInterruptibly(); // Can be interrupted while waiting
        try {
            // Critical section
            System.out.println("Performing task");
        } finally {
            lock.unlock();
        }
    }
}
```

### ReadWriteLock

Allow multiple readers or single writer for improved concurrency.

**Pattern**:

```java
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.HashMap;
import java.util.Map;

public class CachedData {
    private final Map<String, String> cache = new HashMap<>();
    private final ReadWriteLock rwLock = new ReentrantReadWriteLock();

    public String get(String key) {
        rwLock.readLock().lock(); // Multiple readers allowed
        try {
            return cache.get(key);
        } finally {
            rwLock.readLock().unlock();
        }
    }

    public void put(String key, String value) {
        rwLock.writeLock().lock(); // Exclusive writer
        try {
            cache.put(key, value);
        } finally {
            rwLock.writeLock().unlock();
        }
    }

    public void clear() {
        rwLock.writeLock().lock();
        try {
            cache.clear();
        } finally {
            rwLock.writeLock().unlock();
        }
    }
}
```

**When to use ReadWriteLock**:

- Read operations significantly outnumber writes
- Read operations are slow enough to benefit from parallelism
- Data structure supports concurrent reads

### Condition Objects

Multiple condition variables for complex coordination.

**Pattern**:

```java
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.LinkedList;
import java.util.Queue;

public class BoundedQueue<T> {
    private final Queue<T> queue = new LinkedList<>();
    private final int capacity;
    private final Lock lock = new ReentrantLock();
    private final Condition notFull = lock.newCondition();
    private final Condition notEmpty = lock.newCondition();

    public BoundedQueue(int capacity) {
        this.capacity = capacity;
    }

    public void put(T item) throws InterruptedException {
        lock.lock();
        try {
            while (queue.size() == capacity) {
                notFull.await(); // Wait on notFull condition
            }

            queue.add(item);
            notEmpty.signal(); // Signal notEmpty condition

        } finally {
            lock.unlock();
        }
    }

    public T take() throws InterruptedException {
        lock.lock();
        try {
            while (queue.isEmpty()) {
                notEmpty.await(); // Wait on notEmpty condition
            }

            T item = queue.poll();
            notFull.signal(); // Signal notFull condition

            return item;

        } finally {
            lock.unlock();
        }
    }
}
```

### Lock vs Synchronized Trade-offs

| Feature                 | synchronized    | ReentrantLock           |
| ----------------------- | --------------- | ----------------------- |
| **Lock acquisition**    | Automatic       | Manual (lock/unlock)    |
| **Try-lock**            | No              | Yes (tryLock)           |
| **Timeout**             | No              | Yes                     |
| **Interruptible**       | No              | Yes                     |
| **Multiple conditions** | No (single)     | Yes (newCondition)      |
| **Fairness**            | No guarantee    | Optional fair mode      |
| **Performance**         | Slightly faster | Comparable              |
| **Complexity**          | Simpler         | More complex            |
| **Forget to unlock**    | Impossible      | Possible (use finally!) |

**Recommendation**: Use synchronized unless you need explicit lock features (timeout, try-lock, multiple conditions).

## Thread Pools (Standard Library)

ExecutorService manages thread pools for efficient task execution.

### Executor Framework

Framework for decoupling task submission from execution mechanics.

**ExecutorService interface**:

```java
import java.util.concurrent.*;
import java.util.List;
import java.util.ArrayList;

public class ExecutorExample {
    public static void main(String[] args) throws InterruptedException {
        // Fixed thread pool
        ExecutorService executor = Executors.newFixedThreadPool(4);

        // Submit tasks
        List<Future<Integer>> futures = new ArrayList<>();
        for (int i = 1; i <= 10; i++) {
            final int taskId = i;
            Future<Integer> future = executor.submit(() -> {
                System.out.println("Task " + taskId + " on " +
                    Thread.currentThread().getName());
                Thread.sleep(1000);
                return taskId * 2;
            });
            futures.add(future);
        }

        // Get results
        for (Future<Integer> future : futures) {
            try {
                Integer result = future.get(); // Blocking call
                System.out.println("Result: " + result);
            } catch (ExecutionException e) {
                System.err.println("Task failed: " + e.getCause());
            }
        }

        // Shutdown executor
        executor.shutdown();
        executor.awaitTermination(1, TimeUnit.MINUTES);
    }
}
```

### ThreadPoolExecutor Configuration

Configure thread pool sizing and behavior.

**Pattern**:

```java
import java.util.concurrent.*;

public class CustomThreadPool {
    public static void main(String[] args) {
        ThreadPoolExecutor executor = new ThreadPoolExecutor(
            2,                              // Core pool size
            4,                              // Maximum pool size
            60,                             // Keep-alive time
            TimeUnit.SECONDS,               // Keep-alive unit
            new LinkedBlockingQueue<>(100), // Work queue
            new ThreadFactory() {           // Thread factory
                private int count = 1;
                @Override
                public Thread newThread(Runnable r) {
                    return new Thread(r, "Worker-" + count++);
                }
            },
            new ThreadPoolExecutor.CallerRunsPolicy() // Rejection policy
        );

        // Submit tasks
        for (int i = 1; i <= 20; i++) {
            final int taskId = i;
            executor.submit(() -> {
                System.out.println("Task " + taskId + " executing");
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
        }

        executor.shutdown();
    }
}
```

**ThreadPoolExecutor parameters**:

- **corePoolSize**: Minimum threads to keep alive
- **maximumPoolSize**: Maximum threads allowed
- **keepAliveTime**: Idle time before excess threads terminate
- **workQueue**: Queue for tasks when all threads busy
- **threadFactory**: Creates new threads
- **rejectionHandler**: Policy when queue is full

**Rejection policies**:

| Policy                  | Behavior                          |
| ----------------------- | --------------------------------- |
| **AbortPolicy**         | Throws RejectedExecutionException |
| **CallerRunsPolicy**    | Caller thread executes task       |
| **DiscardPolicy**       | Silently discards task            |
| **DiscardOldestPolicy** | Discards oldest task in queue     |

### ScheduledExecutorService

Schedule tasks with delays or periodic execution.

**Pattern**:

```java
import java.util.concurrent.*;

public class ScheduledTaskExample {
    public static void main(String[] args) throws InterruptedException {
        ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(2);

        // One-time delayed execution
        scheduler.schedule(() -> {
            System.out.println("Delayed task executed");
        }, 3, TimeUnit.SECONDS);

        // Fixed-rate periodic execution (every 2 seconds)
        ScheduledFuture<?> fixedRate = scheduler.scheduleAtFixedRate(() -> {
            System.out.println("Fixed rate task at " + System.currentTimeMillis());
        }, 0, 2, TimeUnit.SECONDS);

        // Fixed-delay periodic execution (2 seconds after previous completion)
        ScheduledFuture<?> fixedDelay = scheduler.scheduleWithFixedDelay(() -> {
            System.out.println("Fixed delay task starting");
            try {
                Thread.sleep(1000); // Task takes 1 second
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }, 0, 2, TimeUnit.SECONDS);

        // Run for 10 seconds then cancel
        Thread.sleep(10000);
        fixedRate.cancel(false);
        fixedDelay.cancel(false);

        scheduler.shutdown();
    }
}
```

### Choosing Pool Sizes

Guidelines for determining appropriate thread pool sizes.

**CPU-bound tasks**:

```
Optimal threads = Number of CPU cores + 1
```

**I/O-bound tasks**:

```
Optimal threads = Number of CPU cores * (1 + Wait time / Compute time)
```

**Example calculation**:

```java
public class PoolSizingExample {
    public static void main(String[] args) {
        int cores = Runtime.getRuntime().availableProcessors();

        // CPU-bound tasks
        int cpuBoundPoolSize = cores + 1;
        ExecutorService cpuExecutor = Executors.newFixedThreadPool(cpuBoundPoolSize);

        // I/O-bound tasks (80% wait time, 20% compute time)
        // Wait/Compute ratio = 0.8 / 0.2 = 4
        int ioBoundPoolSize = cores * (1 + 4);
        ExecutorService ioExecutor = Executors.newFixedThreadPool(ioBoundPoolSize);

        System.out.println("CPU cores: " + cores);
        System.out.println("CPU-bound pool size: " + cpuBoundPoolSize);
        System.out.println("I/O-bound pool size: " + ioBoundPoolSize);

        cpuExecutor.shutdown();
        ioExecutor.shutdown();
    }
}
```

### Shutting Down Properly

Always shutdown executors to release resources.

**Pattern**:

```java
public class ExecutorShutdownExample {
    public static void main(String[] args) {
        ExecutorService executor = Executors.newFixedThreadPool(4);

        // Submit tasks
        for (int i = 0; i < 10; i++) {
            executor.submit(() -> {
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
        }

        // Graceful shutdown
        executor.shutdown(); // No new tasks accepted, existing tasks continue

        try {
            // Wait for tasks to complete
            if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
                // Timeout reached, force shutdown
                List<Runnable> droppedTasks = executor.shutdownNow();
                System.out.println("Dropped tasks: " + droppedTasks.size());

                // Wait a bit for threads to respond to interruption
                if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
                    System.err.println("Executor did not terminate");
                }
            }
        } catch (InterruptedException e) {
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
}
```

**shutdown() vs shutdownNow()**:

| Method            | Behavior                                          |
| ----------------- | ------------------------------------------------- |
| **shutdown()**    | Graceful: finish submitted tasks, reject new ones |
| **shutdownNow()** | Forceful: interrupt running tasks, return pending |

### Why Manual Thread Pools Are Complex

**Thread pool tuning challenges**:

1. **Pool sizing**: Incorrect size causes underutilization or thread starvation
2. **Queue sizing**: Unbounded queues risk memory issues, bounded queues risk rejection
3. **Rejection handling**: Must handle when pool and queue are full
4. **Task dependencies**: Deadlock risk if tasks wait for each other
5. **Exception handling**: Uncaught exceptions can silently kill threads
6. **Monitoring**: Need metrics for pool health and performance

**Before**: Manual ThreadPoolExecutor configuration
**After**: Use Executors factory methods or framework-provided pools (Spring @Async)

## Concurrent Collections (Standard Library)

Thread-safe collections in java.util.concurrent optimize for concurrent access.

### ConcurrentHashMap

Thread-safe hash map with segment-level locking.

**Pattern**:

```java
import java.util.concurrent.ConcurrentHashMap;
import java.util.Map;

public class ConcurrentMapExample {
    private final Map<String, Integer> userScores = new ConcurrentHashMap<>();

    public void updateScore(String userId, int points) {
        // Atomic compute if absent
        userScores.putIfAbsent(userId, 0);

        // Atomic update
        userScores.compute(userId, (key, currentScore) ->
            currentScore == null ? points : currentScore + points);
    }

    public void incrementScore(String userId, int points) {
        // Atomic merge
        userScores.merge(userId, points, Integer::sum);
    }

    public Integer getScore(String userId) {
        return userScores.get(userId);
    }

    public static void main(String[] args) throws InterruptedException {
        ConcurrentMapExample example = new ConcurrentMapExample();

        // Multiple threads updating concurrently
        Thread[] threads = new Thread[10];
        for (int i = 0; i < 10; i++) {
            threads[i] = new Thread(() -> {
                for (int j = 0; j < 100; j++) {
                    example.incrementScore("user-1", 1);
                }
            });
            threads[i].start();
        }

        for (Thread thread : threads) {
            thread.join();
        }

        System.out.println("Final score: " + example.getScore("user-1")); // 1000
    }
}
```

**ConcurrentHashMap features**:

- No null keys or values allowed
- Segment-level locking (not whole map)
- Atomic operations (putIfAbsent, compute, merge)
- Weakly consistent iterators (don't throw ConcurrentModificationException)

### CopyOnWriteArrayList

Thread-safe list where writes create a copy.

**Pattern**:

```java
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.List;

public class EventListenerRegistry {
    private final List<EventListener> listeners = new CopyOnWriteArrayList<>();

    public void addListener(EventListener listener) {
        listeners.add(listener); // Creates copy
    }

    public void removeListener(EventListener listener) {
        listeners.remove(listener); // Creates copy
    }

    public void fireEvent(Event event) {
        // Iteration uses snapshot, no locking needed
        for (EventListener listener : listeners) {
            listener.onEvent(event);
        }
    }
}

interface EventListener {
    void onEvent(Event event);
}

class Event {
    private final String message;
    public Event(String message) { this.message = message; }
    public String getMessage() { return message; }
}
```

**When to use CopyOnWriteArrayList**:

- Reads vastly outnumber writes
- Small collection size
- Iteration must not block writers
- Examples: event listeners, observer patterns

**Trade-offs**:

- ✅ No locking for reads
- ✅ Thread-safe iteration
- ❌ Expensive writes (copy entire array)
- ❌ Memory overhead (multiple copies)

### BlockingQueue

Thread-safe queues with blocking operations for producer-consumer.

**ArrayBlockingQueue (bounded)**:

```java
import java.util.concurrent.*;

public class ProducerConsumerQueue {
    public static void main(String[] args) {
        BlockingQueue<Integer> queue = new ArrayBlockingQueue<>(10);

        // Producer
        Thread producer = new Thread(() -> {
            try {
                for (int i = 1; i <= 20; i++) {
                    queue.put(i); // Blocks if queue is full
                    System.out.println("Produced: " + i);
                    Thread.sleep(100);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        // Consumer
        Thread consumer = new Thread(() -> {
            try {
                while (true) {
                    Integer item = queue.take(); // Blocks if queue is empty
                    System.out.println("Consumed: " + item);
                    Thread.sleep(200);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        });

        producer.start();
        consumer.start();
    }
}
```

**LinkedBlockingQueue (optionally bounded)**:

```java
import java.util.concurrent.*;

public class TaskQueue {
    private final BlockingQueue<Runnable> taskQueue = new LinkedBlockingQueue<>(100);

    public void submitTask(Runnable task) throws InterruptedException {
        taskQueue.put(task);
    }

    public Runnable takeTask() throws InterruptedException {
        return taskQueue.take();
    }

    public static void main(String[] args) throws InterruptedException {
        TaskQueue queue = new TaskQueue();

        // Worker threads
        for (int i = 0; i < 4; i++) {
            new Thread(() -> {
                while (!Thread.currentThread().isInterrupted()) {
                    try {
                        Runnable task = queue.takeTask();
                        task.run();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                    }
                }
            }).start();
        }

        // Submit tasks
        for (int i = 1; i <= 20; i++) {
            final int taskId = i;
            queue.submitTask(() -> {
                System.out.println("Executing task " + taskId);
            });
        }
    }
}
```

**BlockingQueue implementations**:

| Implementation            | Characteristics                           |
| ------------------------- | ----------------------------------------- |
| **ArrayBlockingQueue**    | Bounded, array-backed, FIFO               |
| **LinkedBlockingQueue**   | Optionally bounded, linked nodes, FIFO    |
| **PriorityBlockingQueue** | Unbounded, priority heap                  |
| **SynchronousQueue**      | No capacity, direct handoff               |
| **DelayQueue**            | Unbounded, elements available after delay |

### ConcurrentLinkedQueue

Non-blocking concurrent queue using lock-free algorithm.

**Pattern**:

```java
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.Queue;

public class NonBlockingQueue {
    private final Queue<String> queue = new ConcurrentLinkedQueue<>();

    public void addMessage(String message) {
        queue.offer(message); // Non-blocking
    }

    public String pollMessage() {
        return queue.poll(); // Non-blocking, returns null if empty
    }

    public static void main(String[] args) throws InterruptedException {
        NonBlockingQueue nbQueue = new NonBlockingQueue();

        // Multiple producers
        for (int i = 0; i < 5; i++) {
            final int producerId = i;
            new Thread(() -> {
                for (int j = 0; j < 100; j++) {
                    nbQueue.addMessage("Producer-" + producerId + "-Msg-" + j);
                }
            }).start();
        }

        // Multiple consumers
        for (int i = 0; i < 3; i++) {
            new Thread(() -> {
                while (true) {
                    String msg = nbQueue.pollMessage();
                    if (msg != null) {
                        System.out.println("Consumed: " + msg);
                    }
                }
            }).start();
        }

        Thread.sleep(2000);
    }
}
```

### Thread-Safe vs Synchronized Collections

| Collection Type  | Thread-Safety Approach            | Performance        |
| ---------------- | --------------------------------- | ------------------ |
| **Synchronized** | Locks entire collection           | Lower concurrency  |
| **Concurrent**   | Fine-grained locking or lock-free | Higher concurrency |

**Example comparison**:

```java
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class CollectionComparison {
    public static void main(String[] args) {
        // Synchronized wrapper (coarse-grained locking)
        Map<String, Integer> syncMap = Collections.synchronizedMap(new HashMap<>());

        // Concurrent collection (fine-grained locking)
        Map<String, Integer> concurrentMap = new ConcurrentHashMap<>();

        // syncMap: Each operation locks entire map
        synchronized (syncMap) {
            syncMap.put("key1", 1);
            syncMap.put("key2", 2);
        }

        // concurrentMap: Operations lock segments independently
        concurrentMap.put("key1", 1);
        concurrentMap.put("key2", 2);
    }
}
```

## Atomic Operations (Standard Library)

java.util.concurrent.atomic provides lock-free thread-safe operations.

### AtomicInteger, AtomicLong

Atomic operations on integers without locks.

**Pattern**:

```java
import java.util.concurrent.atomic.AtomicInteger;

public class AtomicCounter {
    private final AtomicInteger count = new AtomicInteger(0);

    public void increment() {
        count.incrementAndGet(); // Atomic increment
    }

    public void decrement() {
        count.decrementAndGet(); // Atomic decrement
    }

    public void add(int value) {
        count.addAndGet(value); // Atomic add
    }

    public int get() {
        return count.get();
    }

    public static void main(String[] args) throws InterruptedException {
        AtomicCounter counter = new AtomicCounter();

        Thread[] threads = new Thread[10];
        for (int i = 0; i < 10; i++) {
            threads[i] = new Thread(() -> {
                for (int j = 0; j < 1000; j++) {
                    counter.increment();
                }
            });
            threads[i].start();
        }

        for (Thread thread : threads) {
            thread.join();
        }

        System.out.println("Final count: " + counter.get()); // 10000
    }
}
```

### AtomicReference

Atomic operations on object references.

**Pattern**:

```java
import java.util.concurrent.atomic.AtomicReference;

public class AtomicReferenceExample {
    private final AtomicReference<UserProfile> currentProfile = new AtomicReference<>();

    public void updateProfile(UserProfile newProfile) {
        currentProfile.set(newProfile);
    }

    public boolean updateIfChanged(UserProfile expected, UserProfile newProfile) {
        // Atomic compare-and-swap
        return currentProfile.compareAndSet(expected, newProfile);
    }

    public UserProfile getProfile() {
        return currentProfile.get();
    }

    public static void main(String[] args) {
        AtomicReferenceExample example = new AtomicReferenceExample();

        UserProfile oldProfile = new UserProfile("Alice", 25);
        UserProfile newProfile = new UserProfile("Alice", 26);

        example.updateProfile(oldProfile);

        // Only updates if current value is oldProfile
        boolean updated = example.updateIfChanged(oldProfile, newProfile);
        System.out.println("Updated: " + updated); // true

        // Second attempt fails (current is now newProfile)
        updated = example.updateIfChanged(oldProfile, new UserProfile("Alice", 27));
        System.out.println("Updated: " + updated); // false
    }
}

class UserProfile {
    private final String name;
    private final int age;

    public UserProfile(String name, int age) {
        this.name = name;
        this.age = age;
    }
}
```

### Compare-and-Swap (CAS) Operations

CAS is the foundation of lock-free algorithms.

**Pattern**:

```java
import java.util.concurrent.atomic.AtomicInteger;

public class CASExample {
    private final AtomicInteger value = new AtomicInteger(0);

    public void safeIncrement() {
        while (true) {
            int current = value.get();
            int next = current + 1;

            // Only update if value hasn't changed
            if (value.compareAndSet(current, next)) {
                break; // Success
            }
            // Else retry (value was modified by another thread)
        }
    }

    public static void main(String[] args) throws InterruptedException {
        CASExample example = new CASExample();

        Thread[] threads = new Thread[5];
        for (int i = 0; i < 5; i++) {
            threads[i] = new Thread(() -> {
                for (int j = 0; j < 1000; j++) {
                    example.safeIncrement();
                }
            });
            threads[i].start();
        }

        for (Thread thread : threads) {
            thread.join();
        }

        System.out.println("Final value: " + example.value.get()); // 5000
    }
}
```

**CAS advantages**:

- No locks needed (lock-free)
- No thread blocking
- No deadlock possible
- Better performance under low contention

**CAS disadvantages**:

- Retry loops under high contention
- ABA problem (value changes A→B→A)

### When to Use Atomic vs Locks

| Scenario                       | Use Atomic                   | Use Locks                  |
| ------------------------------ | ---------------------------- | -------------------------- |
| Single variable updates        | ✅ AtomicInteger, AtomicLong | Overkill                   |
| Multiple variable coordination | ❌ Complex, error-prone      | ✅ synchronized or Lock    |
| High contention                | ⚠️ Retry overhead            | ✅ Better under contention |
| Low contention                 | ✅ Faster (no blocking)      | Slower                     |
| Complex state transitions      | ❌ Difficult                 | ✅ Easier to reason about  |

## CompletableFuture (Standard Library)

CompletableFuture provides asynchronous computation chains with functional composition.

### Basic CompletableFuture

Create and compose asynchronous operations.

**Pattern**:

```java
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

public class CompletableFutureBasics {
    public static void main(String[] args) throws ExecutionException, InterruptedException {
        // Create completed future
        CompletableFuture<String> future1 = CompletableFuture.completedFuture("Hello");

        // Run async task
        CompletableFuture<Void> future2 = CompletableFuture.runAsync(() -> {
            System.out.println("Async task running on: " + Thread.currentThread().getName());
        });

        // Supply async value
        CompletableFuture<Integer> future3 = CompletableFuture.supplyAsync(() -> {
            return 42;
        });

        // Get result (blocking)
        String result1 = future1.get();
        Integer result3 = future3.get();

        System.out.println("Result1: " + result1);
        System.out.println("Result3: " + result3);
    }
}
```

### Transformation Methods

Transform results with thenApply, thenCompose, thenCombine.

**thenApply (map)**:

```java
import java.util.concurrent.CompletableFuture;

public class ThenApplyExample {
    public static void main(String[] args) {
        CompletableFuture<Integer> future = CompletableFuture.supplyAsync(() -> {
            return 10;
        }).thenApply(value -> {
            return value * 2; // Transform result
        }).thenApply(value -> {
            return value + 5; // Chain transformations
        });

        future.thenAccept(result -> {
            System.out.println("Result: " + result); // 25
        });
    }
}
```

**thenCompose (flatMap)**:

```java
import java.util.concurrent.CompletableFuture;

public class ThenComposeExample {
    public static void main(String[] args) {
        CompletableFuture<String> future = CompletableFuture.supplyAsync(() -> {
            return "user-123";
        }).thenCompose(userId -> {
            // Return new CompletableFuture
            return fetchUserProfile(userId);
        }).thenCompose(profile -> {
            // Chain async operations
            return fetchUserOrders(profile.getUserId());
        });

        future.thenAccept(orders -> {
            System.out.println("Orders: " + orders);
        });
    }

    static CompletableFuture<UserProfile> fetchUserProfile(String userId) {
        return CompletableFuture.supplyAsync(() -> {
            // Simulate async DB query
            return new UserProfile(userId, "Alice");
        });
    }

    static CompletableFuture<String> fetchUserOrders(String userId) {
        return CompletableFuture.supplyAsync(() -> {
            return "Orders for " + userId;
        });
    }

    static class UserProfile {
        private final String userId;
        private final String name;
        public UserProfile(String userId, String name) {
            this.userId = userId;
            this.name = name;
        }
        public String getUserId() { return userId; }
    }
}
```

**thenCombine (combine two futures)**:

```java
import java.util.concurrent.CompletableFuture;

public class ThenCombineExample {
    public static void main(String[] args) {
        CompletableFuture<Integer> future1 = CompletableFuture.supplyAsync(() -> {
            return 10;
        });

        CompletableFuture<Integer> future2 = CompletableFuture.supplyAsync(() -> {
            return 20;
        });

        CompletableFuture<Integer> combined = future1.thenCombine(future2, (result1, result2) -> {
            return result1 + result2; // Combine both results
        });

        combined.thenAccept(result -> {
            System.out.println("Combined result: " + result); // 30
        });
    }
}
```

### Exception Handling

Handle exceptions in async chains.

**exceptionally**:

```java
import java.util.concurrent.CompletableFuture;

public class ExceptionallyExample {
    public static void main(String[] args) {
        CompletableFuture<Integer> future = CompletableFuture.supplyAsync(() -> {
            if (Math.random() > 0.5) {
                throw new RuntimeException("Random failure");
            }
            return 42;
        }).exceptionally(ex -> {
            System.out.println("Exception: " + ex.getMessage());
            return 0; // Fallback value
        });

        future.thenAccept(result -> {
            System.out.println("Result: " + result);
        });
    }
}
```

**handle (process both success and failure)**:

```java
import java.util.concurrent.CompletableFuture;

public class HandleExample {
    public static void main(String[] args) {
        CompletableFuture<Integer> future = CompletableFuture.supplyAsync(() -> {
            if (Math.random() > 0.5) {
                throw new RuntimeException("Random failure");
            }
            return 42;
        }).handle((result, ex) -> {
            if (ex != null) {
                System.out.println("Exception: " + ex.getMessage());
                return 0;
            } else {
                return result * 2;
            }
        });

        future.thenAccept(result -> {
            System.out.println("Final result: " + result);
        });
    }
}
```

### Combining Multiple Futures

Wait for all or any future to complete.

**allOf (wait for all)**:

```java
import java.util.concurrent.CompletableFuture;
import java.util.List;
import java.util.stream.Collectors;

public class AllOfExample {
    public static void main(String[] args) {
        List<CompletableFuture<String>> futures = List.of(
            fetchDataFromService1(),
            fetchDataFromService2(),
            fetchDataFromService3()
        );

        CompletableFuture<Void> allFutures = CompletableFuture.allOf(
            futures.toArray(new CompletableFuture[0])
        );

        CompletableFuture<List<String>> allResults = allFutures.thenApply(v -> {
            return futures.stream()
                .map(CompletableFuture::join) // Get all results
                .collect(Collectors.toList());
        });

        allResults.thenAccept(results -> {
            System.out.println("All results: " + results);
        });
    }

    static CompletableFuture<String> fetchDataFromService1() {
        return CompletableFuture.supplyAsync(() -> "Data from Service 1");
    }

    static CompletableFuture<String> fetchDataFromService2() {
        return CompletableFuture.supplyAsync(() -> "Data from Service 2");
    }

    static CompletableFuture<String> fetchDataFromService3() {
        return CompletableFuture.supplyAsync(() -> "Data from Service 3");
    }
}
```

**anyOf (wait for first)**:

```java
import java.util.concurrent.CompletableFuture;

public class AnyOfExample {
    public static void main(String[] args) {
        CompletableFuture<String> future1 = CompletableFuture.supplyAsync(() -> {
            sleep(2000);
            return "Result from slow service";
        });

        CompletableFuture<String> future2 = CompletableFuture.supplyAsync(() -> {
            sleep(500);
            return "Result from fast service";
        });

        CompletableFuture<Object> fastest = CompletableFuture.anyOf(future1, future2);

        fastest.thenAccept(result -> {
            System.out.println("First result: " + result); // Fast service wins
        });
    }

    static void sleep(int millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
```

### Why CompletableFuture Simplifies Async Code

**Before (callback hell)**:

```java
// Nested callbacks (callback hell)
executor.submit(() -> {
    String userId = fetchUserId();
    executor.submit(() -> {
        UserProfile profile = fetchProfile(userId);
        executor.submit(() -> {
            List<Order> orders = fetchOrders(profile.getId());
            processOrders(orders);
        });
    });
});
```

**After (CompletableFuture chain)**:

```java
CompletableFuture.supplyAsync(() -> fetchUserId())
    .thenCompose(userId -> fetchProfile(userId))
    .thenCompose(profile -> fetchOrders(profile.getId()))
    .thenAccept(orders -> processOrders(orders));
```

## Virtual Threads (Standard Library - Java 21+)

Virtual threads are lightweight threads managed by the JVM, enabling massive concurrency with minimal overhead.

### Platform Threads vs Virtual Threads

**Platform threads** (traditional):

- Mapped 1:1 to OS threads
- Expensive (1MB stack each)
- Limited by OS (thousands max)
- Block OS thread when waiting

**Virtual threads** (Java 21+):

- Lightweight (few KB each)
- Millions possible
- Managed by JVM
- Mounted/unmounted from carrier threads

### Creating Virtual Threads

Simple API for creating virtual threads.

**Pattern**:

```java
public class VirtualThreadExample {
    public static void main(String[] args) throws InterruptedException {
        // Create and start virtual thread
        Thread vThread = Thread.startVirtualThread(() -> {
            System.out.println("Virtual thread executing");
        });

        vThread.join();

        // Using builder
        Thread vThread2 = Thread.ofVirtual()
            .name("virtual-worker")
            .start(() -> {
                System.out.println("Named virtual thread");
            });

        vThread2.join();

        // Create but don't start
        Thread vThread3 = Thread.ofVirtual()
            .unstarted(() -> {
                System.out.println("Unstarted virtual thread");
            });

        vThread3.start();
        vThread3.join();
    }
}
```

### ExecutorService with Virtual Threads

Use executor with virtual thread pool.

**Pattern**:

```java
import java.util.concurrent.*;

public class VirtualThreadExecutor {
    public static void main(String[] args) throws InterruptedException {
        // Virtual thread executor
        ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();

        // Submit many tasks
        for (int i = 0; i < 1_000_000; i++) {
            final int taskId = i;
            executor.submit(() -> {
                // Each task gets its own virtual thread
                simulateIoOperation();
                if (taskId % 100_000 == 0) {
                    System.out.println("Task " + taskId + " completed");
                }
            });
        }

        executor.shutdown();
        executor.awaitTermination(1, TimeUnit.MINUTES);
    }

    static void simulateIoOperation() {
        try {
            Thread.sleep(100); // Virtual thread parks, doesn't block carrier
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
```

### Migration from Platform Threads

Replace platform threads with virtual threads.

**Before (platform threads)**:

```java
ExecutorService executor = Executors.newFixedThreadPool(100);

for (int i = 0; i < 10_000; i++) {
    executor.submit(() -> {
        // Limited to 100 concurrent tasks
        handleRequest();
    });
}
```

**After (virtual threads)**:

```java
ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();

for (int i = 0; i < 10_000; i++) {
    executor.submit(() -> {
        // 10,000 concurrent virtual threads (no problem!)
        handleRequest();
    });
}
```

### Performance Characteristics

Virtual threads excel at I/O-bound workloads.

**CPU-bound workloads**: Virtual threads offer no advantage (same number of CPU cores)

**I/O-bound workloads**: Virtual threads scale to millions of concurrent operations

**Example comparison**:

```java
import java.time.Duration;
import java.time.Instant;
import java.util.concurrent.*;

public class VirtualThreadPerformance {
    public static void main(String[] args) throws InterruptedException {
        int taskCount = 10_000;

        // Platform threads (limited)
        Instant start1 = Instant.now();
        ExecutorService platformExecutor = Executors.newFixedThreadPool(100);
        submitTasks(platformExecutor, taskCount);
        platformExecutor.shutdown();
        platformExecutor.awaitTermination(1, TimeUnit.MINUTES);
        Duration platform = Duration.between(start1, Instant.now());

        // Virtual threads (unlimited)
        Instant start2 = Instant.now();
        ExecutorService virtualExecutor = Executors.newVirtualThreadPerTaskExecutor();
        submitTasks(virtualExecutor, taskCount);
        virtualExecutor.shutdown();
        virtualExecutor.awaitTermination(1, TimeUnit.MINUTES);
        Duration virtual = Duration.between(start2, Instant.now());

        System.out.println("Platform threads: " + platform.toMillis() + "ms");
        System.out.println("Virtual threads: " + virtual.toMillis() + "ms");
    }

    static void submitTasks(ExecutorService executor, int count) {
        for (int i = 0; i < count; i++) {
            executor.submit(() -> {
                try {
                    Thread.sleep(100); // Simulate I/O
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
        }
    }
}
```

## Common Concurrency Patterns

### Producer-Consumer Pattern

See Synchronization section for wait/notify implementation and BlockingQueue section for concurrent collection implementation.

### Reader-Writer Pattern

See ReadWriteLock section for explicit lock implementation.

### Thread-Local Storage

ThreadLocal provides per-thread variables.

**Pattern**:

```java
public class ThreadLocalExample {
    private static final ThreadLocal<String> userContext = new ThreadLocal<>();

    public static void setUser(String userId) {
        userContext.set(userId);
    }

    public static String getUser() {
        return userContext.get();
    }

    public static void clearUser() {
        userContext.remove(); // Important: prevent memory leaks
    }

    public static void main(String[] args) {
        Thread thread1 = new Thread(() -> {
            setUser("user-1");
            System.out.println(Thread.currentThread().getName() + " user: " + getUser());
            clearUser();
        });

        Thread thread2 = new Thread(() -> {
            setUser("user-2");
            System.out.println(Thread.currentThread().getName() + " user: " + getUser());
            clearUser();
        });

        thread1.start();
        thread2.start();
    }
}
```

**ThreadLocal use cases**:

- User authentication context
- Database transaction management
- Request-scoped data in web applications
- Date/number formatters (SimpleDateFormat is not thread-safe)

### Double-Checked Locking (AVOID)

Double-checked locking is broken without volatile.

**Problematic pattern**:

```java
// BROKEN: Don't use this pattern!
public class BrokenSingleton {
    private static BrokenSingleton instance;

    public static BrokenSingleton getInstance() {
        if (instance == null) { // Check 1 (unsynchronized)
            synchronized (BrokenSingleton.class) {
                if (instance == null) { // Check 2 (synchronized)
                    instance = new BrokenSingleton(); // PROBLEM: Not atomic!
                }
            }
        }
        return instance;
    }
}
```

**Fixed pattern (requires volatile)**:

```java
public class CorrectSingleton {
    private static volatile CorrectSingleton instance; // volatile required

    public static CorrectSingleton getInstance() {
        if (instance == null) {
            synchronized (CorrectSingleton.class) {
                if (instance == null) {
                    instance = new CorrectSingleton();
                }
            }
        }
        return instance;
    }
}
```

**Better alternatives**:

```java
// Initialization-on-demand holder idiom (preferred)
public class BestSingleton {
    private BestSingleton() {}

    private static class Holder {
        static final BestSingleton INSTANCE = new BestSingleton();
    }

    public static BestSingleton getInstance() {
        return Holder.INSTANCE; // Thread-safe, lazy, no synchronization
    }
}
```

### Immutable Objects for Thread Safety

Immutable objects are inherently thread-safe.

**Pattern**:

```java
public final class ImmutablePoint {
    private final int x;
    private final int y;

    public ImmutablePoint(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() { return x; }
    public int getY() { return y; }

    public ImmutablePoint move(int dx, int dy) {
        return new ImmutablePoint(x + dx, y + dy); // Return new instance
    }
}

// Thread-safe without synchronization
public class PointManager {
    private volatile ImmutablePoint currentPoint = new ImmutablePoint(0, 0);

    public void updatePoint(int dx, int dy) {
        currentPoint = currentPoint.move(dx, dy); // Atomic reference update
    }

    public ImmutablePoint getPoint() {
        return currentPoint; // Safe to read
    }
}
```

## Parallel Streams (Standard Library)

Parallel streams use ForkJoinPool for parallel collection processing.

### Using Parallel Streams

Convert sequential streams to parallel.

**Pattern**:

```java
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class ParallelStreamExample {
    public static void main(String[] args) {
        List<Integer> numbers = IntStream.rangeClosed(1, 1_000_000)
            .boxed()
            .collect(Collectors.toList());

        // Sequential stream
        long start1 = System.currentTimeMillis();
        long sum1 = numbers.stream()
            .mapToLong(n -> expensiveComputation(n))
            .sum();
        long sequential = System.currentTimeMillis() - start1;

        // Parallel stream
        long start2 = System.currentTimeMillis();
        long sum2 = numbers.parallelStream()
            .mapToLong(n -> expensiveComputation(n))
            .sum();
        long parallel = System.currentTimeMillis() - start2;

        System.out.println("Sequential: " + sequential + "ms");
        System.out.println("Parallel: " + parallel + "ms");
        System.out.println("Speedup: " + (double) sequential / parallel + "x");
    }

    static long expensiveComputation(int n) {
        return n * n; // Simulate CPU-intensive work
    }
}
```

### When to Use Parallel Streams

Parallel streams have overhead - only beneficial for appropriate workloads.

**Good candidates**:

- Large data sets (thousands+ elements)
- CPU-intensive operations
- Independent operations (no shared state)
- Stateless operations

**Poor candidates**:

- Small data sets (overhead exceeds benefit)
- I/O-bound operations (blocking)
- Operations with side effects
- Ordered streams requiring sequential processing

**Example comparison**:

```java
import java.util.List;
import java.util.stream.IntStream;

public class ParallelStreamPitfalls {
    public static void main(String[] args) {
        List<Integer> numbers = IntStream.rangeClosed(1, 100).boxed().toList();

        // BAD: Small dataset (overhead not worth it)
        long sum1 = numbers.parallelStream().mapToLong(n -> n * 2).sum();

        // BAD: I/O operations (blocking)
        numbers.parallelStream().forEach(n -> {
            // Don't do I/O in parallel streams
            // writeToDatabase(n);
        });

        // BAD: Shared mutable state (race condition)
        Counter counter = new Counter();
        numbers.parallelStream().forEach(n -> {
            counter.increment(); // RACE CONDITION!
        });

        // GOOD: Large dataset, CPU-bound, no side effects
        long sum2 = IntStream.rangeClosed(1, 1_000_000)
            .parallel()
            .mapToLong(n -> n * n)
            .sum();
    }

    static class Counter {
        private int count = 0;
        public void increment() { count++; }
    }
}
```

### Common Pitfalls

**Stateful operations**:

```java
// BAD: Stateful lambda (race condition)
List<Integer> results = new ArrayList<>();
numbers.parallelStream().forEach(n -> {
    results.add(n * 2); // ArrayList not thread-safe!
});

// GOOD: Use collect
List<Integer> results = numbers.parallelStream()
    .map(n -> n * 2)
    .collect(Collectors.toList());
```

**Blocking operations**:

```java
// BAD: Blocking I/O in parallel stream
numbers.parallelStream().forEach(n -> {
    try {
        Thread.sleep(1000); // Blocks ForkJoinPool thread!
    } catch (InterruptedException e) {
        Thread.currentThread().interrupt();
    }
});

// GOOD: Use CompletableFuture for async I/O
List<CompletableFuture<Void>> futures = numbers.stream()
    .map(n -> CompletableFuture.runAsync(() -> {
        // Async I/O operation
    }))
    .collect(Collectors.toList());
```

**See**: [Functional Programming](/en/learn/software-engineering/programming-languages/java/in-practice/functional-programming) for parallel stream patterns and best practices.

## Avoiding Anti-Patterns

Common concurrency anti-patterns are documented in the anti-patterns guide.

**See**: [Anti-Patterns](/en/learn/software-engineering/programming-languages/java/in-practice/anti-patterns) for detailed coverage of:

- **Thread Leakage**: Creating threads without lifecycle management
- **Race Conditions**: Unsynchronized shared mutable state
- **Deadlocks**: Circular lock dependencies
- **Busy Waiting**: Polling instead of proper synchronization
- **Ignoring InterruptedException**: Suppressing interruption signals

Consult the anti-patterns guide for recognition signals, examples, and solutions.

## Best Practices

### Prefer Immutability

Immutable objects eliminate most concurrency issues.

```java
// Mutable (requires synchronization)
public class MutableCounter {
    private int count = 0;

    public synchronized void increment() {
        count++;
    }

    public synchronized int get() {
        return count;
    }
}

// Immutable (no synchronization needed)
public final class ImmutableCounter {
    private final int count;

    public ImmutableCounter(int count) {
        this.count = count;
    }

    public ImmutableCounter increment() {
        return new ImmutableCounter(count + 1);
    }

    public int get() {
        return count;
    }
}
```

### Use High-Level Abstractions

Prefer ExecutorService over raw threads.

```java
// DON'T: Manual thread management
for (int i = 0; i < 100; i++) {
    new Thread(() -> {
        processTask();
    }).start();
}

// DO: Use executor service
ExecutorService executor = Executors.newFixedThreadPool(10);
for (int i = 0; i < 100; i++) {
    executor.submit(() -> processTask());
}
executor.shutdown();
```

### Avoid Shared Mutable State

Design systems to minimize shared state.

```java
// BAD: Shared mutable state
public class SharedCounter {
    private static int globalCounter = 0; // Shared mutable

    public void increment() {
        globalCounter++; // Race condition
    }
}

// GOOD: Message passing (no shared state)
public class MessagePassingCounter {
    private final BlockingQueue<CounterMessage> queue = new ArrayBlockingQueue<>(100);

    public void increment() {
        queue.offer(new IncrementMessage());
    }

    public void processMessages() {
        // Single thread processes messages
        while (true) {
            CounterMessage msg = queue.take();
            msg.process();
        }
    }
}
```

### Test for Thread Safety

Verify thread safety with stress tests.

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class ConcurrentCounterTest {

    @Test
    void incrementConcurrently() throws InterruptedException {
        Counter counter = new Counter();
        int threadCount = 10;
        int incrementsPerThread = 1000;

        Thread[] threads = new Thread[threadCount];
        for (int i = 0; i < threadCount; i++) {
            threads[i] = new Thread(() -> {
                for (int j = 0; j < incrementsPerThread; j++) {
                    counter.increment();
                }
            });
            threads[i].start();
        }

        for (Thread thread : threads) {
            thread.join();
        }

        assertEquals(threadCount * incrementsPerThread, counter.get());
    }
}
```

### Document Thread-Safety Guarantees

Make thread safety explicit in documentation.

```java
/**
 * Thread-safe counter using AtomicInteger.
 * All public methods are safe for concurrent access.
 */
public class ThreadSafeCounter {
    private final AtomicInteger count = new AtomicInteger(0);

    public void increment() {
        count.incrementAndGet();
    }

    public int get() {
        return count.get();
    }
}

/**
 * NOT thread-safe.
 * Caller must synchronize external access.
 */
public class UnsafeCounter {
    private int count = 0;

    public void increment() {
        count++;
    }

    public int get() {
        return count;
    }
}
```

## Related Content

- [Anti-Patterns](/en/learn/software-engineering/programming-languages/java/in-practice/anti-patterns) - Threading anti-patterns (deadlock, race conditions, thread leakage, busy waiting)
- [Functional Programming](/en/learn/software-engineering/programming-languages/java/in-practice/functional-programming) - Parallel streams and immutable data structures
- [Performance](/en/learn/software-engineering/programming-languages/java/in-practice/performance) - Thread pool tuning and concurrency optimization
- [Best Practices](/en/learn/software-engineering/programming-languages/java/in-practice/best-practices) - Concurrency best practices and patterns
