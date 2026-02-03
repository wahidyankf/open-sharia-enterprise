---
title: "Advanced"
date: 2026-01-02T05:01:35+07:00
draft: false
weight: 10000003
description: "Master advanced Java through 109 examples: concurrency, parallelism, virtual threads, JVM internals, memory management, GC tuning, design patterns, reflection, profiling, FSM patterns, performance optimization, JFR, async-profiler, JMH benchmarking, and modern Java features"
tags:
  [
    "java",
    "tutorial",
    "by-example",
    "advanced",
    "concurrency",
    "jvm",
    "memory-management",
    "garbage-collection",
    "design-patterns",
    "reflection",
    "virtual-threads",
    "reactive",
    "profiling",
    "performance",
    "finite-state-machines",
    "fsm",
    "state-pattern",
    "sealed-classes",
  ]
---

Master advanced Java concepts through 109 annotated code examples. Build on intermediate foundations to explore advanced concurrency, parallelism, virtual threads, JVM internals, memory management, garbage collection tuning, design patterns, profiling tools, finite state machines, performance optimization (JVM tuning, JFR, async-profiler, JMH), and modern Java features.

## Example 51: Concurrent Collections

Concurrent collections provide thread safety with better performance than synchronized wrappers. `ConcurrentHashMap` offers lock striping. `BlockingQueue` supports producer-consumer patterns.

**BlockingQueue (Producer-Consumer):**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Producer["Producer Thread<br/>put elements"]
    Producer --> Queue["BlockingQueue<br/>bounded capacity"]
    Queue --> Consumer["Consumer Thread<br/>take elements"]

    style Producer fill:#0173B2,color:#fff
    style Queue fill:#DE8F05,color:#fff
    style Consumer fill:#029E73,color:#fff
```

**ConcurrentHashMap (Lock Striping):**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    CHM["ConcurrentHashMap<br/>parallel access"]
    CHM --> Segment1["Segment 1<br/>independent lock"]
    CHM --> Segment2["Segment 2<br/>independent lock"]
    CHM --> Segment3["Segment N<br/>independent lock"]

    style CHM fill:#0173B2,color:#fff
    style Segment1 fill:#029E73,color:#fff
    style Segment2 fill:#029E73,color:#fff
    style Segment3 fill:#029E73,color:#fff
```

**Code**:

```java
import java.util.concurrent.*;
import java.util.*;

// ConcurrentHashMap - thread-safe without blocking entire map
ConcurrentHashMap<String, Integer> map = new ConcurrentHashMap<>(); // => Empty map, uses lock striping for segments

// putIfAbsent - atomic operation (check + insert)
Integer result1 = map.putIfAbsent("key1", 100);
                                 // => Atomically checks if key1 exists (thread-safe)
                                 // => CAS-based operation: check-then-act is atomic
                                 // => Key doesn't exist, inserts key1=100 atomically
                                 // => Returns null (previous value was null/absent)
                                 // => No race condition: another thread cannot insert between check and insert
System.out.println(map.get("key1")); // => 100 (key1 now maps to 100)
                                 // => Volatile read: guarantees visibility across threads
Integer existing = map.putIfAbsent("key1", 200);
                                 // => Atomically checks if key1 exists (thread-safe check)
                                 // => Key exists with value 100, does NOT replace
                                 // => Atomic constraint: insertion only if key absent
                                 // => Returns 100 (existing value, map unchanged)
                                 // => Thread-safe: no lost updates even with concurrent calls
System.out.println(map.get("key1")); // => 100 (unchanged, still 100)
                                 // => Key value remains 100 (atomic guarantee)

// computeIfAbsent - compute value if absent
Integer computed = map.computeIfAbsent("key2", k -> k.length() * 10);
                                 // => Checks if key2 exists, it doesn't
                                 // => Calls lambda with key: k="key2"
                                 // => Computes "key2".length() = 4, then * 10 = 40
                                 // => Inserts key2=40 atomically
                                 // => Returns computed value 40
System.out.println(map.get("key2")); // => 40 (key2 now maps to 40)
Integer recompute = map.computeIfAbsent("key2", k -> k.length() * 20);
                                 // => Checks if key2 exists, it does (value is 40)
                                 // => Lambda NOT called (optimization, key present)
                                 // => Returns existing value 40 (map unchanged)

// merge - combine values atomically
map.put("count", 1);                 // => Inserts count=1 into map
Integer merged = map.merge("count", 5, (old, val) -> old + val);
                                 // => Retrieves existing value: old=1
                                 // => Calls lambda with old=1, val=5 (merge value)
                                 // => Computes 1 + 5 = 6
                                 // => Updates count=6 atomically
                                 // => Returns merged result 6
System.out.println(map.get("count")); // => 6 (count now holds 6)

// CopyOnWriteArrayList - thread-safe list, copy-on-write semantics
CopyOnWriteArrayList<String> list = new CopyOnWriteArrayList<>(); // => Empty list, writes create new array copy
list.add("A"); // => Creates new internal array with ["A"]
list.add("B"); // => Creates new internal array with ["A", "B"]
System.out.println(list.size()); // => 2

// Safe iteration even if list modified during iteration
for (String s : list) { // => Iterator uses snapshot of list at iteration start
    System.out.println(s); // => Prints "A", then "B" (original snapshot)
    list.add("C"); // => Safe - iterator uses snapshot, won't see "C" in this loop
} // => Loop completes with 2 iterations (A, B), not affected by concurrent adds
System.out.println(list.size()); // => 4 (A, B, C, C - two "C"s added during loop)

// ConcurrentLinkedQueue - non-blocking FIFO queue (lock-free)
ConcurrentLinkedQueue<String> queue = new ConcurrentLinkedQueue<>(); // => Empty queue, uses CAS operations
boolean offered1 = queue.offer("First"); // => true (element added to tail)
boolean offered2 = queue.offer("Second"); // => true (element added after "First")
System.out.println(queue.size()); // => 2 (weakly consistent size, may be stale in concurrent env)
String head = queue.poll(); // => "First" (removed from head, queue now has ["Second"])
String head2 = queue.poll(); // => "Second" (removed from head, queue now empty)
String head3 = queue.poll(); // => null (queue empty)

// BlockingQueue - producer-consumer pattern with blocking coordination
BlockingQueue<Integer> blockingQueue = new ArrayBlockingQueue<>(10);
                                 // => Bounded queue, capacity 10 elements
                                 // => ArrayBlockingQueue uses single ReentrantLock for put/take
                                 // => Backed by array, FIFO ordering (first in, first out)
                                 // => Thread-safe: no external synchronization needed

// Producer thread
Thread producer = new Thread(() -> { // => Creates producer thread (not started yet)
                                 // => Lambda implements Runnable.run() method
                                 // => Closure captures blockingQueue reference
    try {
        for (int i = 0; i < 5; i++) { // => Will produce items 0-4 (5 iterations)
                                 // => i is loop counter (local variable per iteration)
            blockingQueue.put(i); // => Blocks if queue full (capacity 10), waits until space available
                                 // => put() blocks thread using Condition.await() if full
                                 // => Wakes up when consumer calls take() (creates space)
                                 // => Atomically inserts item and signals waiting consumers
            System.out.println("Produced: " + i);
                                 // => Output: "Produced: 0", "Produced: 1", etc.
                                 // => Print happens AFTER successful insertion
            Thread.sleep(100);   // => Simulate production work (100ms delay)
                                 // => Yields CPU, allows other threads to run
                                 // => InterruptedException if thread interrupted during sleep
        }
    } catch (InterruptedException e) {
                                 // => Caught if Thread.interrupt() called during put() or sleep()
        Thread.currentThread().interrupt();
                                 // => Restore interrupted status (preserve interruption)
                                 // => Allows caller to detect interruption later
    }
}); // => Thread created but not running (must call start())

// Consumer thread
Thread consumer = new Thread(() -> { // => Creates consumer thread (not started yet)
                                 // => Separate thread from producer (runs concurrently)
    try {
        for (int i = 0; i < 5; i++) { // => Will consume 5 items (matches producer count)
            Integer item = blockingQueue.take();
                                 // => Blocks if queue empty, waits until item available
                                 // => take() blocks thread using Condition.await() if empty
                                 // => Wakes up when producer calls put() (adds item)
                                 // => Atomically removes item and signals waiting producers
            System.out.println("Consumed: " + item);
                                 // => Output: "Consumed: 0", "Consumed: 1", etc.
                                 // => item is Integer (autoboxed from queue)
            Thread.sleep(150);   // => Simulate consumption work (150ms delay)
                                 // => Slower than producer (150ms vs 100ms)
                                 // => Creates backpressure: queue fills up over time
        }
    } catch (InterruptedException e) {
                                 // => Caught if interrupted during take() or sleep()
        Thread.currentThread().interrupt();
                                 // => Restore interrupted status
    }
}); // => Thread created but not running (must call start())

producer.start();                // => Start producer thread, begins producing items
                                 // => Calls producer.run() in new thread
                                 // => Producer thread now executing concurrently
consumer.start();                // => Start consumer thread, begins consuming items
                                 // => Calls consumer.run() in new thread
                                 // => Consumer thread now executing concurrently
// => Both threads run concurrently, queue coordinates between them
// => BlockingQueue handles synchronization (no manual locks needed)
// => Output order may vary due to concurrent execution and scheduling
// => Typical pattern: producer may produce multiple before consumer starts
// => Queue acts as buffer between producer and consumer speeds
```

**Key Takeaway**: `ConcurrentHashMap` provides thread-safe operations with lock striping. `CopyOnWriteArrayList` is safe for iteration-heavy workloads. `BlockingQueue` enables producer-consumer patterns with blocking operations. These collections offer better performance than `Collections.synchronizedXxx()` wrappers.

**Why It Matters**: Concurrent collections provide thread-safe operations without manual synchronization, preventing data races and deadlocks. ConcurrentHashMap enables high-throughput concurrent reads/writes with fine-grained locking. CopyOnWriteArrayList optimizes read-heavy scenarios (event listeners, caches) by copying on modification. BlockingQueue implementations enable producer-consumer patterns with built-in waiting/notification. Understanding which collection fits which access pattern (read-heavy vs write-heavy, bounded vs unbounded) prevents performance bottlenecks. These collections are essential building blocks for scalable concurrent systems—caches, work queues, shared state—enabling correctness without synchronization complexity.

---

## Example 52: Atomic Variables

Atomic variables use hardware-level Compare-And-Swap (CAS) operations for lock-free concurrency. They provide better performance than synchronized blocks for simple state updates. Ideal for counters, flags, and references.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Thread1["Thread 1"] --> CAS1["compareAndSet<br/>(expected, new)"]
    Thread2["Thread 2"] --> CAS2["compareAndSet<br/>(expected, new)"]

    CAS1 --> Check1{"Current == expected?"}
    CAS2 --> Check2{"Current == expected?"}

    Check1 -->|Yes| Update1["Update atomically<br/>Return true"]
    Check1 -->|No| Retry1["Return false<br/>(retry)"]

    Check2 -->|Yes| Update2["Update atomically<br/>Return true"]
    Check2 -->|No| Retry2["Return false<br/>(retry)"]

    Update1 --> Value["Shared AtomicInteger"]
    Update2 --> Value
    Retry1 -.->|Read again| CAS1
    Retry2 -.->|Read again| CAS2

    style Thread1 fill:#0173B2,color:#fff
    style Thread2 fill:#0173B2,color:#fff
    style Check1 fill:#DE8F05,color:#fff
    style Check2 fill:#DE8F05,color:#fff
    style Update1 fill:#029E73,color:#fff
    style Update2 fill:#029E73,color:#fff
    style Value fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.util.concurrent.atomic.*;

// AtomicInteger - thread-safe integer operations without locks
AtomicInteger counter = new AtomicInteger(0); // => Initialized to 0, uses CAS internally

// Atomic read and write operations
int current = counter.get(); // => 0 (volatile read, guaranteed visibility across threads)
counter.set(10); // => Set to 10 (volatile write, visible to all threads)
System.out.println(counter.get()); // => 10

// Atomic increment operations
int previous = counter.getAndIncrement(); // => 10 (returns old value, then increments to 11)
System.out.println(counter.get()); // => 11 (counter now 11)
int newValue = counter.incrementAndGet(); // => 12 (increments first, then returns new value)
System.out.println(counter.get()); // => 12 (counter now 12)

// compareAndSet - atomic compare-and-swap (CAS operation)
boolean success = counter.compareAndSet(12, 20);
                                 // => Hardware-level CAS instruction (CMPXCHG on x86)
                                 // => Step 1: Read current value (12)
                                 // => Step 2: Compare with expected (12)
                                 // => Step 3: If equal, atomically set to 20
                                 // => All 3 steps happen atomically (single CPU instruction)
                                 // => true (current was 12, set to 20 atomically)
                                 // => No race condition: no thread can modify between compare and set
System.out.println(counter.get()); // => 20 (successfully updated)
                                 // => Volatile read: guaranteed visibility
boolean failure = counter.compareAndSet(12, 30);
                                 // => Attempts CAS with expected value 12
                                 // => Step 1: Read current value (20, not 12)
                                 // => Step 2: Compare with expected (20 != 12)
                                 // => Step 3: Comparison fails, no update performed
                                 // => false (current is 20, not 12, no update)
                                 // => Atomic guarantee: all-or-nothing operation
System.out.println(counter.get()); // => 20 (unchanged, CAS failed)
                                 // => Value remains 20 (update rejected)

// updateAndGet - atomic update with lambda function
int updated = counter.updateAndGet(v -> v * 2);
                                 // => CAS loop with function application
                                 // => Iteration 1: Reads current value: v=20
                                 // => Applies lambda: 20 * 2 = 40 (computes new value)
                                 // => Attempts compareAndSet(20, 40)
                                 // => If succeeds: returns 40
                                 // => If fails (concurrent modification): retry with new value
                                 // => Atomically sets counter to 40 using CAS
                                 // => Returns new value 40 (post-update value)
System.out.println(counter.get()); // => 40 (counter now 40)
                                 // => Function successfully applied atomically
// => Internally uses CAS loop: read, apply function, compareAndSet, retry if failed
// => Retries if another thread modified counter between read and update
// => Lock-free algorithm: no blocking, just retry on contention
// => Eventually succeeds even under high contention (progress guarantee)

// accumulateAndGet - atomic accumulation with binary operator
int accumulated = counter.accumulateAndGet(5, (curr, update) -> curr + update);
                                 // => Reads current value: curr=40
                                 // => Calls lambda with curr=40, update=5
                                 // => Computes 40 + 5 = 45
                                 // => Atomically sets counter to 45 using CAS
                                 // => Returns accumulated result 45
System.out.println(counter.get()); // => 45 (counter now 45)

// AtomicLong - for long values (same API as AtomicInteger)
AtomicLong longCounter = new AtomicLong(1000L); // => Initialized to 1000
long result = longCounter.addAndGet(500); // => 1500 (atomically adds 500, returns new value)
System.out.println(longCounter.get()); // => 1500

// AtomicBoolean - for thread-safe flags
AtomicBoolean flag = new AtomicBoolean(false); // => Initialized to false
boolean wasSet = flag.getAndSet(true); // => false (old value), flag now true
System.out.println(flag.get()); // => true (flag flipped atomically)
boolean swapped = flag.compareAndSet(true, false); // => true (was true, now false)
System.out.println(flag.get()); // => false (back to false)

// AtomicReference - for object references (any object type)
AtomicReference<String> ref = new AtomicReference<>("Hello"); // => Reference to "Hello"
boolean updated2 = ref.compareAndSet("Hello", "World"); // => true (swapped to "World")
String value = ref.get(); // => "World" (new reference)
ref.set("Java"); // => Reference now points to "Java"
System.out.println(ref.get()); // => "Java"

// Practical example: thread-safe counter without locks
AtomicInteger sharedCounter = new AtomicInteger(0); // => Shared between threads, starts at 0
Runnable task = () -> { // => Each thread runs this task
    for (int i = 0; i < 1000; i++) {
        sharedCounter.incrementAndGet(); // => Thread-safe atomic increment (CAS-based)
        // => No synchronized block needed, no lock contention
    }
}; // => Task increments counter 1000 times

Thread t1 = new Thread(task); // => First thread
Thread t2 = new Thread(task); // => Second thread
t1.start(); // => Start thread 1 (runs concurrently)
t2.start(); // => Start thread 2 (runs concurrently)
t1.join(); // => Wait for thread 1 to complete
t2.join(); // => Wait for thread 2 to complete
System.out.println("Final count: " + sharedCounter.get());
// => Output: "Final count: 2000" (always correct, 1000 + 1000)
// => With non-atomic int, result would be unpredictable (lost updates)
```

**Key Takeaway**: Atomic variables use CAS for lock-free thread safety. Better performance than synchronized for simple operations. Use `incrementAndGet()` for post-increment, `getAndIncrement()` for pre-increment behavior. `compareAndSet()` enables atomic conditional updates.

**Why It Matters**: Atomic variables enable lock-free concurrency through compare-and-swap (CAS) operations—providing thread-safe updates without synchronization overhead. They're essential for counters, flags, and lock-free data structures in high-performance systems. AtomicInteger, AtomicLong, and AtomicReference prevent races while avoiding lock contention that limits scalability. Understanding CAS enables optimistic concurrency—retry on failure instead of blocking. Atomics power millions of operations per second in systems where synchronized blocks cause bottlenecks. However, they don't replace locks for complex multi-variable invariants. Use atomics for hotspot optimization where lock contention is measured and proven problematic.

---

## Example 53: CountDownLatch and CyclicBarrier

`CountDownLatch` allows threads to wait until a set of operations completes. `CyclicBarrier` synchronizes threads at a common barrier point. Both coordinate multi-threaded workflows but serve different patterns.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[CountDownLatch] --> B[Initialize with count N]
    B --> C[Threads call countDown]
    C --> D{Count reaches 0?}
    D -->|Yes| E[Waiting threads released]
    D -->|No| C

    F[CyclicBarrier] --> G[Initialize with parties N]
    G --> H[Threads call await]
    H --> I{All N threads waiting?}
    I -->|Yes| J[All threads released]
    I -->|No| H
    J --> K[Barrier resets for reuse]

    style A fill:#0173B2,color:#fff
    style F fill:#DE8F05,color:#fff
    style E fill:#029E73,color:#fff
    style J fill:#029E73,color:#fff
```

**Code**:

```java
import java.util.concurrent.*;

// CountDownLatch - wait for multiple events
CountDownLatch latch = new CountDownLatch(3);
                                 // => Creates latch with count = 3
                                 // => Latch starts with internal counter at 3
                                 // => Thread calling await() blocks until count reaches 0
                                 // => One-time use: cannot reset after reaching 0

// Worker threads count down
for (int i = 0; i < 3; i++) {
                                 // => Creates 3 worker threads
                                 // => Each thread will decrement latch counter
    int taskId = i;              // => Capture loop variable for lambda
                                 // => Effectively final for lambda closure
    new Thread(() -> {           // => Creates and starts new worker thread
                                 // => Lambda runs in separate thread
        System.out.println("Task " + taskId + " starting");
                                 // => Output: "Task 0 starting", "Task 1 starting", etc.
                                 // => Order non-deterministic (thread scheduling)
        try { Thread.sleep(1000); } catch (InterruptedException e) {}
                                 // => Simulates 1 second of work
                                 // => Sleep may be interrupted (cooperative cancellation)
        System.out.println("Task " + taskId + " done");
                                 // => Output: "Task 0 done", etc. after ~1 second
                                 // => Signals work completion
        latch.countDown();       // => Decrements latch counter by 1
                                 // => Atomic operation: thread-safe decrement
                                 // => When counter reaches 0, releases all waiting threads
                                 // => Does NOT block: decrements and returns immediately
    }).start();                  // => Starts thread execution
                                 // => start() returns immediately (non-blocking)
}

// Main thread waits for all tasks
latch.await();                   // => Blocks current thread until count reaches 0
                                 // => Waits for all 3 countDown() calls
                                 // => Can throw InterruptedException (not caught here)
                                 // => After release: main thread continues execution
System.out.println("All tasks completed!");
                                 // => Output: "All tasks completed!" after all 3 tasks finish
                                 // => Guaranteed to print AFTER all "Task N done" messages
                                 // => Coordination primitive ensures happens-after relationship

// CyclicBarrier - synchronize threads at barrier
CyclicBarrier barrier = new CyclicBarrier(3, () -> {
                                 // => Creates barrier for 3 threads (parties)
                                 // => Second argument: barrier action (runs when all threads arrive)
                                 // => Barrier action executes in last thread to arrive
    System.out.println("All threads reached barrier!");
                                 // => Output: "All threads reached barrier!" when all 3 threads call await()
                                 // => Prints ONCE per barrier cycle
                                 // => Runs before any thread continues past barrier
});                              // => Barrier is reusable after all threads pass

// Threads wait at barrier
for (int i = 0; i < 3; i++) {
                                 // => Creates 3 threads for barrier synchronization
    int threadId = i;            // => Capture loop variable for lambda
                                 // => Effectively final for closure
    new Thread(() -> {           // => Creates new thread with lambda runnable
        try {
            System.out.println("Thread " + threadId + " working");
                                 // => Output: "Thread 0 working", etc.
                                 // => Order non-deterministic
            Thread.sleep(1000);  // => Simulates 1 second of work
                                 // => Each thread works independently before barrier
            System.out.println("Thread " + threadId + " waiting at barrier");
                                 // => Output: "Thread 0 waiting at barrier", etc.
                                 // => Signals thread arrival at synchronization point
            barrier.await();     // => Blocks until all 3 threads call await()
                                 // => When last thread arrives, barrier action runs
                                 // => Then ALL threads released simultaneously
                                 // => Barrier resets automatically for next cycle
            System.out.println("Thread " + threadId + " continues after barrier");
                                 // => Output: "Thread 0 continues after barrier", etc.
                                 // => All 3 messages print nearly simultaneously
                                 // => Happens-after relationship with barrier action
        } catch (Exception e) {} // => Catches InterruptedException, BrokenBarrierException
                                 // => BrokenBarrierException if another thread interrupted
    }).start();                  // => Starts thread execution
}

// Barrier can be reused (cyclic)
                                 // => "Cyclic" means barrier resets after all threads pass
                                 // => Same barrier can coordinate multiple rounds
                                 // => Unlike CountDownLatch (one-time use only)
// After all threads pass, barrier resets for next use
                                 // => Internal count returns to 3 (parties value)
                                 // => Ready for next synchronization round

// Semaphore - control access to limited resources
Semaphore semaphore = new Semaphore(2);
                                 // => Creates semaphore with 2 permits
                                 // => Allows 2 threads to acquire permit simultaneously
                                 // => Third thread blocks until permit released
                                 // => Fair=false (default): no FIFO guarantee

Runnable task = () -> {          // => Task definition (runs in each thread)
    try {
        semaphore.acquire();     // => Acquires 1 permit from semaphore
                                 // => Blocks if no permits available (waits for release)
                                 // => Decrements available permits by 1
                                 // => Throws InterruptedException if interrupted while waiting
        System.out.println(Thread.currentThread().getName() + " acquired permit");
                                 // => Output: "Thread-0 acquired permit", etc.
                                 // => At most 2 threads print this simultaneously
                                 // => Third thread waits until one releases
        Thread.sleep(2000);      // => Simulates 2 seconds of work with permit
                                 // => Holds permit for 2 seconds (resource usage)
        System.out.println(Thread.currentThread().getName() + " releasing permit");
                                 // => Output: "Thread-0 releasing permit", etc.
                                 // => Signals permit about to be returned
        semaphore.release();     // => Releases permit back to semaphore
                                 // => Increments available permits by 1
                                 // => Unblocks one waiting thread (if any)
                                 // => MUST match acquire (no try-finally = leak risk)
    } catch (InterruptedException e) {}
                                 // => Catches interrupt during acquire() or sleep()
};

// 5 threads compete for 2 permits
for (int i = 0; i < 5; i++) {
                                 // => Creates 5 threads competing for 2 permits
                                 // => Threads 0-1 acquire immediately
                                 // => Threads 2-4 wait until permit available
    new Thread(task, "Thread-" + i).start();
                                 // => Starts thread with custom name "Thread-0", etc.
                                 // => All threads execute same task runnable
}
```

**Key Takeaway**: `CountDownLatch` is one-time use for waiting on multiple events. `CyclicBarrier` is reusable for synchronizing threads at a common point. `Semaphore` controls access to limited resources with permits. Choose based on coordination pattern.

**Why It Matters**: CountDownLatch and CyclicBarrier enable thread coordination patterns. CountDownLatch waits for N events before proceeding—useful for phased initialization (start processing after all services ready) and parallel testing (wait for async operations). CyclicBarrier waits for N threads to reach a barrier then resets—ideal for iterative parallel algorithms. Understanding one-time (latch) vs reusable (barrier) determines appropriate choice. These primitives prevent busy-waiting and race conditions in concurrent coordination. They're common in production systems requiring synchronization across multiple threads without complex locking logic.

---

## Example 54: Fork/Join Framework

Fork/Join framework enables efficient parallel processing of recursive tasks. It uses work-stealing queues where idle threads steal work from busy threads. Powers parallel streams under the hood.

**Code**:

```java
import java.util.concurrent.*;

// RecursiveTask - returns a result
class SumTask extends RecursiveTask<Long> {
                                 // => RecursiveTask<Long>: returns Long result
                                 // => Part of Fork/Join framework
    private final long[] array;  // => Array to sum
                                 // => Immutable: shared across tasks safely
    private final int start, end;// => Task's range: [start, end)
                                 // => Defines which portion this task processes
    private static final int THRESHOLD = 1000;
                                 // => Granularity threshold: when to stop splitting
                                 // => Tasks smaller than this execute sequentially

    public SumTask(long[] array, int start, int end) {
                                 // => Constructor initializes task range
        this.array = array;      // => Store array reference
        this.start = start;      // => Store start index
        this.end = end;          // => Store end index
    }

    @Override
    protected Long compute() {   // => Called by ForkJoinPool when task executes
                                 // => Runs in worker thread (part of thread pool)
        int length = end - start;// => Calculate subarray size
                                 // => Determines if task should split further

        // Base case: compute directly if small enough
        if (length <= THRESHOLD) {
                                 // => THRESHOLD (1000): minimum work unit size
                                 // => Below threshold: sequential computation faster than forking overhead
                                 // => Prevents excessive task creation (granularity control)
            long sum = 0;        // => Accumulator for sequential sum
            for (int i = start; i < end; i++) {
                                 // => Sequential loop: no parallelism here
                sum += array[i]; // => Add array element to sum
                                 // => Direct memory access (no synchronization needed)
            }
            return sum;          // => Return computed sum for this range
        }                        // => Base case completes, task finishes

        // Recursive case: split into subtasks
        int mid = start + length / 2;
                                 // => Calculate midpoint for binary split
                                 // => Divides work evenly: left half + right half
        SumTask leftTask = new SumTask(array, start, mid);
                                 // => Create left subtask (first half of range)
                                 // => New RecursiveTask instance (not started yet)
        SumTask rightTask = new SumTask(array, mid, end);
                                 // => Create right subtask (second half of range)
                                 // => Separate task for concurrent execution

        leftTask.fork();         // => Async execute left subtask
                                 // => fork() submits task to ForkJoinPool queue
                                 // => Other threads can steal this task (work-stealing)
                                 // => Returns immediately (non-blocking)
                                 // => Left task runs concurrently in another thread
        long rightResult = rightTask.compute();
                                 // => Compute right in current thread (synchronous)
                                 // => Recursive call: may fork further if still too large
                                 // => Current thread does useful work (not idle)
                                 // => Avoids forking overhead for one subtask
        long leftResult = leftTask.join();
                                 // => Wait for left result (blocking if not complete)
                                 // => join() blocks until forked task finishes
                                 // => Returns computed sum from left subtask
                                 // => May help with work-stealing while waiting

        return leftResult + rightResult;
                                 // => Combine results from left and right subtasks
                                 // => Recursive merge: bubble up results through call stack
    }                            // => Task completes, returns sum for this range
}

// Using ForkJoinPool
long[] numbers = new long[10000];
                                 // => Array of 10,000 elements
for (int i = 0; i < numbers.length; i++) {
    numbers[i] = i + 1;          // => Populates array: 1, 2, 3, ..., 10000
}

ForkJoinPool pool = new ForkJoinPool();
                                 // => Default: Runtime.availableProcessors() threads
                                 // => Creates thread pool for work-stealing
SumTask task = new SumTask(numbers, 0, numbers.length);
                                 // => Creates root task: sum entire array
long result = pool.invoke(task);// => Executes task, returns result
                                 // => Output: 50005000 (sum of 1 to 10000)

// RecursiveAction - no result (void)
                                 // => Use for tasks that perform actions without returning values
class PrintTask extends RecursiveAction {
                                 // => RecursiveAction: no return value
                                 // => Alternative to RecursiveTask when no result needed
    private final int start, end;// => Range to process
                                 // => Defines task boundaries
    private static final int THRESHOLD = 10;
                                 // => Threshold: stop splitting at 10 elements
                                 // => Smaller threshold = more fine-grained parallelism

    public PrintTask(int start, int end) {
                                 // => Constructor sets task range
        this.start = start;      // => Task range start
        this.end = end;          // => Task range end
    }

    @Override
    protected void compute() {
        if (end - start <= THRESHOLD) {
                                 // => Base case: print directly if small enough
            for (int i = start; i < end; i++) {
                System.out.println("Processing: " + i);
                                 // => Sequential processing
            }
        } else {                 // => Recursive case: split task
            int mid = start + (end - start) / 2;
                                 // => Calculate midpoint
            PrintTask left = new PrintTask(start, mid);
                                 // => Left subtask
            PrintTask right = new PrintTask(mid, end);
                                 // => Right subtask
            invokeAll(left, right);
                                 // => Fork both and wait for completion
                                 // => Parallel execution of both subtasks
        }
    }
}

PrintTask printTask = new PrintTask(0, 100);
                                 // => Create task to print 0-99
pool.invoke(printTask);          // => Executes task
                                 // => Prints 0 to 99 in parallel (order non-deterministic)

// Parallel streams use ForkJoinPool.commonPool()
long parallelSum = java.util.stream.LongStream.range(1, 10001)
                                 // => Creates LongStream: 1 to 10000
    .parallel();                 // => Uses ForkJoinPool internally
                                 // => ForkJoinPool.commonPool() by default
    .sum();                      // => Parallel reduction
                                 // => Output: 50005000
```

**Key Takeaway**: Fork/Join splits recursive tasks for parallel execution. `RecursiveTask<V>` returns results, `RecursiveAction` doesn't. Work-stealing balances load across threads. Use for divide-and-conquer algorithms. Parallel streams leverage this framework.

**Why It Matters**: Fork/Join framework parallelizes divide-and-conquer algorithms—merge sort, quicksort, tree processing. Work-stealing load balancing ensures even CPU utilization—idle threads steal tasks from busy threads. RecursiveTask/RecursiveAction hide thread pool complexity, enabling focus on problem decomposition. However, overhead limits effectiveness to large tasks—small tasks underperform sequential code. Understanding task granularity (split until work exceeds threshold) optimizes performance. Fork/Join powers parallel streams and is essential for CPU-bound parallel algorithms on multi-core hardware, enabling scalability without manual thread management.

---

## Example 55: Annotations and Reflection

Annotations add metadata to code for compile-time and runtime processing. Reflection inspects and manipulates code at runtime. Together they enable frameworks like Spring and JUnit to work their magic.

**Code**:

```java
import java.lang.annotation.*;
import java.lang.reflect.*;

// Custom annotation definition
@Retention(RetentionPolicy.RUNTIME) // => Annotation accessible at runtime via reflection
// => Without RUNTIME, annotation discarded after compilation (SOURCE/CLASS retention)
@Target(ElementType.METHOD) // => Restricts annotation to methods only
// => Other targets: TYPE (classes), FIELD, PARAMETER, CONSTRUCTOR, etc.
@interface Test {
    String description() default ""; // => Annotation parameter with default value
    // => Usage: @Test(description = "test name") or @Test() (uses default "")
    int timeout() default 0;
    // => Multiple parameters allowed in annotation
    // => Defaults make parameters optional
}

// Another annotation for classes
@Retention(RetentionPolicy.RUNTIME)
// => Class annotation also needs RUNTIME retention for reflection
@Target(ElementType.TYPE) // => Can only be applied to classes/interfaces
// => ElementType.TYPE includes classes, interfaces, enums, records
@interface Component {
    String value();
    // => No default = required parameter
    // => Must specify: @Component("ServiceName")
}

// Using custom annotations
@Component("UserService") // => Class-level annotation with value parameter
// => Accessible via clazz.getAnnotation(Component.class)
class UserService {
    @Test(description = "Verify user creation", timeout = 5000)
    // => Method-level annotation with both parameters specified
    // => timeout = 5000 milliseconds (5 seconds)
    public void testCreateUser() {
        System.out.println("Testing user creation");
        // => Prints: "Testing user creation" when invoked via reflection
    }

    @Test(description = "Verify user deletion")
    // => timeout parameter uses default value (0)
    // => Only description specified explicitly
    public void testDeleteUser() {
        System.out.println("Testing user deletion");
        // => Prints: "Testing user deletion" when invoked
    }

    public void helperMethod() {
        // No annotation
        // => Not detected by reflection scan for @Test
        // => Won't be invoked by test runner
    }
}

// Reflection - inspecting annotations at runtime
Class<?> clazz = UserService.class;
// => Gets Class object for UserService
// => Class<?> wildcard allows any class type

// Check if class has annotation
if (clazz.isAnnotationPresent(Component.class)) {
    // => Returns true if @Component annotation present on UserService class
    // => Returns false if annotation missing or not RUNTIME retention
    Component component = clazz.getAnnotation(Component.class);
    // => Retrieves @Component annotation instance
    // => Returns proxy object implementing Component interface
    System.out.println("Component name: " + component.value()); // => "UserService"
    // => Calls value() method on annotation proxy
    // => Prints: "Component name: UserService"
}

// Find all methods with @Test annotation
Method[] methods = clazz.getDeclaredMethods();
// => Gets ALL methods (public, private, protected, package-private)
// => Returns array of Method objects (reflection API)
// => getDeclaredMethods() excludes inherited methods
for (Method method : methods) {
    // => Iterates through all 3 methods: testCreateUser, testDeleteUser, helperMethod
    if (method.isAnnotationPresent(Test.class)) {
        // => Returns true for testCreateUser and testDeleteUser
        // => Returns false for helperMethod (no @Test annotation)
        Test test = method.getAnnotation(Test.class);
        // => Retrieves @Test annotation from method
        // => Returns proxy object with description() and timeout() methods
        System.out.println("Test: " + method.getName());
        // => Prints method name: "testCreateUser" or "testDeleteUser"
        System.out.println("  Description: " + test.description());
        // => Prints annotation parameter value
        // => "Verify user creation" or "Verify user deletion"
        System.out.println("  Timeout: " + test.timeout());
        // => Prints timeout value: 5000 or 0 (default)

        // Invoke method using reflection
        try {
            Object instance = clazz.getDeclaredConstructor().newInstance();
            // => Creates new UserService instance via no-arg constructor
            // => getDeclaredConstructor() finds constructor with matching params (none)
            // => newInstance() invokes constructor, returns new object
            method.invoke(instance); // Calls the test method
            // => Dynamically invokes method on instance
            // => Equivalent to: instance.testCreateUser() or instance.testDeleteUser()
            // => Prints: "Testing user creation" or "Testing user deletion"
        } catch (Exception e) {
            // => Catches: IllegalAccessException, InvocationTargetException, etc.
            // => Reflection throws checked exceptions
            e.printStackTrace();
            // => Prints stack trace if method invocation fails
        }
    }
}

// Reflection - inspecting class structure
Field[] fields = clazz.getDeclaredFields();
// => Gets all fields declared in UserService (none in this example)
// => Returns empty array for UserService (no fields)
for (Field field : fields) {
    // => Loop doesn't execute (UserService has no fields)
    System.out.println("Field: " + field.getName() + " (" + field.getType() + ")");
    // => Would print field name and type if fields existed
    // => Example output: "Field: counter (int)"
}

// Accessing private fields (be careful!)
class Person {
    private String name = "Alice";
    // => Private field normally inaccessible outside class
    // => Reflection can bypass access modifiers
}

Person person = new Person();
// => Creates Person instance with name = "Alice"
Field nameField = Person.class.getDeclaredField("name");
// => Gets Field object for "name" field
// => getDeclaredField("name") finds field by exact name
// => Throws NoSuchFieldException if field doesn't exist
nameField.setAccessible(true); // Bypass private access
// => Disables Java access control for this field
// => Allows reading/writing private field from outside class
// => Required to access private members via reflection
String name = (String) nameField.get(person); // => "Alice"
// => Reads field value from person instance
// => Returns Object, requires cast to String
// => Result: "Alice" (original value)
nameField.set(person, "Bob"); // Modify private field
// => Writes new value to field
// => Changes person.name from "Alice" to "Bob"
// => Bypasses immutability and encapsulation

// Built-in annotations
@Override // Compile-time check for method overriding
// => Compiler verifies method actually overrides superclass method
// => Compile error if method doesn't override anything
@Deprecated(since = "2.0") // Mark as deprecated
// => Warns users this class is deprecated
// => Compiler generates warnings when used
// => "since" parameter documents deprecation version
@SuppressWarnings("unchecked") // Suppress compiler warnings
// => Disables specific compiler warnings for this class
// => "unchecked" suppresses generic type safety warnings
class Example {
    @Override
    public String toString() { return "Example"; }
    // => Overrides Object.toString()
    // => @Override ensures this actually overrides (compile-time check)
    // => Returns: "Example"
}
```

**Key Takeaway**: Annotations add metadata with `@interface`. `@Retention` controls lifecycle (SOURCE/CLASS/RUNTIME). `@Target` specifies where annotations apply. Reflection reads annotations and manipulates code at runtime via `Class`, `Method`, `Field`. Enables framework magic but slower than direct access.

**Why It Matters**: Annotations enable declarative programming—express intent without boilerplate. Reflection enables processing annotations at runtime, powering dependency injection (Spring's @Autowired), persistence (JPA's @Entity), validation (@NotNull), and testing (@Test). Understanding retention policies (SOURCE, CLASS, RUNTIME) determines annotation availability. Custom annotations enable domain-specific configurations. However, reflection is slow, fragile (breaks with refactoring), and bypasses type safety—use sparingly. Annotations are foundational to modern Java development, but understanding their reflection-based implementation enables effective usage and debugging when frameworks misbehave.

---

## Example 56: Enums with Behavior

Enums are type-safe constants that can have fields, methods, and constant-specific behavior. They're more powerful than simple integer constants and integrate seamlessly with switch statements.

**Code**:

```java
// Basic enum
enum Day {                       // => Defines enum type Day
                                 // => Enum class implicitly extends java.lang.Enum
                                 // => Cannot extend other classes (single inheritance)
    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
                                 // => Enum constants (instances of Day)
                                 // => Created once at class loading (singletons)
                                 // => Implicitly public static final
}

Day today = Day.MONDAY;          // => References MONDAY constant
                                 // => Type-safe: can only assign Day enum values
                                 // => Cannot be null unless explicitly assigned
System.out.println(today);       // => Calls toString(): "MONDAY"
                                 // => Output: MONDAY
                                 // => toString() returns constant name by default
System.out.println(today.ordinal());
                                 // => Returns position in enum declaration (0-based)
                                 // => Output: 0 (MONDAY is first constant)
                                 // => TUESDAY.ordinal() would be 1, etc.

// Enum with fields and methods
enum Planet {                    // => Enum with instance fields and methods
    MERCURY(3.303e23, 2.4397e6), // => Calls constructor Planet(double, double)
                                 // => mass = 3.303e23 kg, radius = 2.4397e6 m
                                 // => Each constant is separate instance with own fields
    VENUS(4.869e24, 6.0518e6),   // => Venus instance with different mass/radius
    EARTH(5.976e24, 6.37814e6),  // => Earth instance
    MARS(6.421e23, 3.3972e6);    // => Mars instance (semicolon required before members)

    private final double mass;   // => Instance field (each constant has own)
                                 // => Final: immutable after construction
                                 // => Private: encapsulated (accessible via methods)
    private final double radius; // => Radius in meters
                                 // => All enum constants have these fields

    // Constructor (always private)
    Planet(double mass, double radius) {
                                 // => Constructor implicitly private (cannot be public)
                                 // => Called once per constant during class loading
                                 // => Cannot create new Planet instances externally
        this.mass = mass;        // => Initializes instance field
                                 // => Each constant stores its own mass
        this.radius = radius;    // => Initializes radius field
                                 // => Fields immutable after constructor completes
    }

    public double surfaceGravity() {
                                 // => Instance method (each constant can call)
                                 // => Calculates gravity using this constant's mass/radius
        final double G = 6.67300E-11;
                                 // => Gravitational constant (m³/kg·s²)
                                 // => Final local variable
        return G * mass / (radius * radius);
                                 // => Newton's law: g = GM/r²
                                 // => Returns gravity in m/s²
    }

    public double surfaceWeight(double otherMass) {
                                 // => Calculates weight on this planet's surface
                                 // => otherMass in kg
        return otherMass * surfaceGravity();
                                 // => Weight = mass × gravity
                                 // => Returns weight in Newtons
    }
}

double earthWeight = 75.0;       // => Person's weight on Earth (kg)
                                 // => Will calculate equivalent weight on Mars
double marsWeight = Planet.MARS.surfaceWeight(earthWeight);
                                 // => Calls MARS instance's surfaceWeight method
                                 // => MARS.surfaceGravity() ≈ 3.71 m/s²
                                 // => Result: 75 × 3.71 ≈ 28.4 kg
                                 // => Output: ~28.4 (Mars gravity is 38% of Earth's)

// Enum with abstract methods (constant-specific behavior)
enum Operation {                 // => Enum with constant-specific method implementations
    PLUS {                       // => Anonymous class body for PLUS constant
        @Override                // => Overrides abstract apply method
        public double apply(double x, double y) { return x + y; }
                                 // => PLUS-specific implementation: addition
                                 // => This implementation only for PLUS constant
    },
    MINUS {                      // => MINUS constant with subtraction implementation
        @Override
        public double apply(double x, double y) { return x - y; }
                                 // => MINUS-specific: subtraction
    },
    TIMES {                      // => TIMES constant with multiplication
        @Override
        public double apply(double x, double y) { return x * y; }
                                 // => TIMES-specific: multiplication
    },
    DIVIDE {                     // => DIVIDE constant with division
        @Override
        public double apply(double x, double y) { return x / y; }
                                 // => DIVIDE-specific: division (no zero check)
    };

    public abstract double apply(double x, double y);
                                 // => Abstract method: each constant must implement
                                 // => Forces constant-specific behavior
                                 // => Enables polymorphism via enum constants
}

double result = Operation.PLUS.apply(5, 3);
                                 // => Calls PLUS constant's apply implementation
                                 // => Returns 5 + 3 = 8.0
                                 // => Output: 8.0
double product = Operation.TIMES.apply(4, 7);
                                 // => Calls TIMES constant's apply implementation
                                 // => Returns 4 × 7 = 28.0
                                 // => Output: 28.0

// Enum methods
Day[] days = Day.values();       // => Returns array of all enum constants
                                 // => Returns: [MONDAY, TUESDAY, ..., SUNDAY]
                                 // => New array each call (defensive copy)
                                 // => Order matches declaration order
Day day = Day.valueOf("FRIDAY"); // => Converts string to enum constant
                                 // => Case-sensitive: must exactly match constant name
                                 // => Returns: Day.FRIDAY
                                 // => Throws IllegalArgumentException if name invalid

// Switch with enum (exhaustive in modern Java)
String typeOfDay = switch (today) {
                                 // => Switch expression (Java 14+)
                                 // => Returns value assigned to typeOfDay
                                 // => Compiler checks exhaustiveness for enums
    case MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY -> "Weekday";
                                 // => Multiple cases with arrow syntax
                                 // => Returns: "Weekday" for any weekday
                                 // => No fall-through (arrow syntax)
    case SATURDAY, SUNDAY -> "Weekend";
                                 // => Handles weekend cases
                                 // => Returns: "Weekend"
                                 // => All 7 days covered: exhaustive
};                               // => Semicolon required for switch expression
                                 // => Compiler enforces: all enum constants handled

// EnumSet - efficient set implementation for enums
import java.util.*;
EnumSet<Day> weekend = EnumSet.of(Day.SATURDAY, Day.SUNDAY);
                                 // => Creates EnumSet with SATURDAY and SUNDAY
                                 // => Internally uses bit vector (very efficient)
                                 // => EnumSet faster than HashSet for enums
                                 // => No nulls allowed
EnumSet<Day> weekdays = EnumSet.range(Day.MONDAY, Day.FRIDAY);
                                 // => Creates EnumSet with range (inclusive)
                                 // => Contains: MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY
                                 // => Uses ordinal values for range calculation
                                 // => O(1) operations due to bit vector

// EnumMap - efficient map with enum keys
EnumMap<Day, String> schedule = new EnumMap<>(Day.class);
                                 // => Creates EnumMap with Day keys
                                 // => Requires enum class token for initialization
                                 // => Internally uses array (faster than HashMap)
                                 // => Maintains natural enum order
schedule.put(Day.MONDAY, "Team meeting");
                                 // => Maps MONDAY to "Team meeting"
                                 // => O(1) put operation (array-based)
                                 // => schedule.get(MONDAY) returns "Team meeting"
schedule.put(Day.WEDNESDAY, "Code review");
                                 // => Maps WEDNESDAY to "Code review"
                                 // => Null values allowed, null keys not allowed
```

**Key Takeaway**: Enums are type-safe constants with fields, methods, and constructors. Constant-specific behavior via abstract methods enables polymorphism. `values()` and `valueOf()` provide iteration and lookup. `EnumSet` and `EnumMap` offer efficient enum-based collections.

**Why It Matters**: Enums provide type-safe constants with behavior, eliminating magic strings/numbers. Methods on enums encapsulate state-specific logic (Status.ACTIVE.canTransitionTo(INACTIVE)), making code self-documenting. Exhaustive switch statements with enums enable compiler-checked completeness. EnumSet and EnumMap are highly optimized for enum keys. Enums model finite state machines, configuration options, strategy patterns. Understanding enum construction (singletons, instance-specific behavior) enables sophisticated designs. Enums prevent stringly-typed code that's fragile and hard to refactor, making them essential for modeling closed sets of values in type-safe, maintainable systems.

---

## Example 57: Sealed Classes and Pattern Matching

Sealed classes restrict which classes can extend or implement them, enabling exhaustive pattern matching. Pattern matching eliminates casts and enables type-safe, concise code. Available in Java 17+.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[sealed class Shape] --> B[final class Circle]
    A --> C[final class Rectangle]
    A --> D[non-sealed class Triangle]
    D --> E[class RightTriangle]
    D --> F[class IsoscelesTriangle]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#DE8F05,color:#fff
    style D fill:#029E73,color:#fff
    style E fill:#CC78BC,color:#fff
    style F fill:#CC78BC,color:#fff
```

**Code**:

```java
// Sealed class - restricted inheritance hierarchy
sealed class Shape permits Circle, Rectangle, Triangle {
                                 // => sealed keyword restricts which classes can extend Shape
                                 // => permits clause lists allowed subclasses explicitly
                                 // => Compiler enforces: only Circle, Rectangle, Triangle can extend
                                 // => Enables exhaustive pattern matching (all subtypes known)
    // Only Circle, Rectangle, Triangle can extend Shape
                                 // => Attempting to create other subclasses causes compile error
                                 // => Permits must be in same module or package
}

// Permitted subclasses must be final, sealed, or non-sealed
final class Circle extends Shape {
                                 // => final: Circle cannot be further subclassed
                                 // => Required choice for sealed subclass
                                 // => Closes inheritance hierarchy at this level
    private final double radius; // => Immutable field (final)
    public Circle(double radius) { this.radius = radius; }
                                 // => Constructor initializes immutable radius
                                 // => this.radius cannot change after construction
    public double area() { return Math.PI * radius * radius; }
                                 // => Calculates circle area: πr²
                                 // => Uses Math.PI constant (≈ 3.14159...)
}

final class Rectangle extends Shape {
                                 // => final: Rectangle cannot be subclassed
                                 // => Second permitted subclass of Shape
    private final double width, height;
                                 // => Two immutable fields
                                 // => Compact field declaration (both final double)
    public Rectangle(double width, double height) {
                                 // => Constructor for rectangle dimensions
        this.width = width;      // => Initializes width field
        this.height = height;    // => Initializes height field
    }
    public double area() { return width * height; }
                                 // => Rectangle area: width × height
}

non-sealed class Triangle extends Shape {
                                 // => non-sealed: Triangle CAN be further subclassed
                                 // => Opens inheritance hierarchy at this level
                                 // => Allows external classes to extend Triangle
    // non-sealed: allows further subclassing
                                 // => RightTriangle, IsoscelesTriangle, etc. can extend Triangle
                                 // => Breaks sealed constraint intentionally
    private final double base, height;
                                 // => Triangle dimensions (immutable)
    public Triangle(double base, double height) {
                                 // => Constructor for triangle dimensions
        this.base = base;        // => Base of triangle
        this.height = height;    // => Height of triangle
    }
    public double area() { return 0.5 * base * height; }
                                 // => Triangle area: ½ × base × height
}

// Pattern matching for instanceof (Java 16+)
Object obj = "Hello";            // => Object reference to String instance
                                 // => Runtime type is String, compile-time type is Object
if (obj instanceof String s) {   // => instanceof with pattern variable
                                 // => Tests if obj is String AND assigns to s
                                 // => s is automatically cast to String
                                 // => No explicit cast needed: (String) obj
    // s is automatically cast to String in this scope
                                 // => s is String type, not Object
                                 // => Scope: s available only in if block
    System.out.println(s.toUpperCase());
                                 // => Calls String.toUpperCase() directly on s
                                 // => Output: "HELLO"
                                 // => No ClassCastException risk (already verified by instanceof)
}

// Pattern matching with sealed classes
Shape shape = new Circle(5.0);   // => Creates Circle with radius 5.0
                                 // => Shape reference (polymorphism)
                                 // => Runtime type is Circle

// Switch pattern matching (Java 17+, enhanced in Java 21)
double area = switch (shape) {   // => Switch on sealed type (exhaustive)
                                 // => Compiler knows all subtypes: Circle, Rectangle, Triangle
                                 // => No default case needed (exhaustive)
    case Circle c -> Math.PI * c.radius * c.radius;
                                 // => Pattern variable c (type Circle)
                                 // => Direct access to c.radius (no cast)
                                 // => Returns πr² for circles
    case Rectangle r -> r.width * r.height;
                                 // => Pattern variable r (type Rectangle)
                                 // => Returns width × height for rectangles
    case Triangle t -> 0.5 * t.base * t.height;
                                 // => Pattern variable t (type Triangle)
                                 // => Returns ½bh for triangles
    // Exhaustive: compiler knows all possible subtypes
                                 // => All Shape subtypes covered
                                 // => Compile error if any subtype missing
};                               // => Switch expression returns double
                                 // => area = calculated area value

// Pattern matching with guards (Java 21+)
String description = switch (shape) {
                                 // => Switch with guard conditions (when clause)
                                 // => More specific cases checked first
    case Circle c when c.radius > 10 -> "Large circle";
                                 // => Guard: c.radius > 10 must be true
                                 // => Returns "Large circle" if radius > 10
                                 // => Falls through to next Circle case if false
    case Circle c -> "Small circle";
                                 // => Catches all other Circle cases (radius ≤ 10)
                                 // => Returns "Small circle"
    case Rectangle r when r.width == r.height -> "Square";
                                 // => Guard: width == height (square detection)
                                 // => Returns "Square" for rectangles with equal sides
    case Rectangle r -> "Rectangle";
                                 // => Catches non-square rectangles
    case Triangle t -> "Triangle";
                                 // => All triangles (no guard)
};                               // => description = matched string value

// Record patterns (Java 19+, finalized in Java 21)
record Point(int x, int y) {}    // => Record with x, y components
                                 // => Auto-generates constructor, getters, equals, hashCode
                                 // => Immutable: components final

Object point = new Point(10, 20);// => Creates Point(10, 20)
                                 // => Reference type Object (upcasting)
if (point instanceof Point(int x, int y)) {
                                 // => Record pattern: destructures Point into x, y
                                 // => Checks if point is Point AND extracts components
                                 // => x = 10, y = 20 (pattern variables)
                                 // => No need for point.x(), point.y() calls
    System.out.println("x: " + x + ", y: " + y);
                                 // => Direct access to destructured variables
                                 // => Output: "x: 10, y: 20"
}

// Nested record patterns
record ColoredPoint(Point point, String color) {}
                                 // => Record containing another record
                                 // => point field is Point type
                                 // => color field is String type

Object cp = new ColoredPoint(new Point(5, 10), "red");
                                 // => Creates ColoredPoint with nested Point
                                 // => cp.point() returns Point(5, 10)
                                 // => cp.color() returns "red"
if (cp instanceof ColoredPoint(Point(int x, int y), String color)) {
                                 // => Nested record pattern: destructures TWO levels
                                 // => First level: ColoredPoint → Point, String
                                 // => Second level: Point → int x, int y
                                 // => Pattern variables: x, y, color
    System.out.println("Point at (" + x + ", " + y + ") is " + color);
                                 // => Accesses x, y, color directly (no getters)
                                 // => Output: "Point at (5, 10) is red"
}

// Sealed interfaces
sealed interface Result permits Success, Failure {}
                                 // => Sealed interface (Java 17+)
                                 // => Only Success and Failure can implement Result
                                 // => Enables exhaustive switch on Result type
record Success(String data) implements Result {}
                                 // => Success record implements Result
                                 // => Implicitly final (records cannot be extended)
                                 // => data field accessible via data() getter
record Failure(String error) implements Result {}
                                 // => Failure record implements Result
                                 // => error field accessible via error() getter

Result result = new Success("Data loaded");
                                 // => Creates Success with "Data loaded"
                                 // => Result reference (polymorphism)
                                 // => Runtime type is Success
String message = switch (result) {
                                 // => Switch on sealed interface Result
                                 // => Exhaustive: Success and Failure are only implementations
    case Success(String data) -> "Success: " + data;
                                 // => Record pattern on Success
                                 // => Destructures: data = "Data loaded"
                                 // => Returns "Success: Data loaded"
    case Failure(String error) -> "Error: " + error;
                                 // => Record pattern on Failure
                                 // => Destructures: error = failure message
                                 // => Returns "Error: " + error
};                               // => message = "Success: Data loaded"
                                 // => No default case needed (exhaustive)
```

**Key Takeaway**: Sealed classes restrict inheritance with `sealed` and `permits`. Subclasses must be `final`, `sealed`, or `non-sealed`. Pattern matching for `instanceof` eliminates casts. Switch pattern matching enables exhaustive type checking. Record patterns destructure records in pattern matching.

**Why It Matters**: Sealed classes enable exhaustive hierarchies—all subtypes known at compile time, enabling complete case analysis. Combined with pattern matching (Java 17+), they provide type-safe destructuring without casts. Sealed classes complement enums (which lack per-variant data) and interfaces (which allow unlimited implementations). They're ideal for modeling closed domain concepts—Result<T, Error>, AST nodes, state machines—where all variants are known and fixed. Sealed classes prevent external extension, ensuring exhaustive handling and enabling better compiler errors, refactoring support, and reasoning about code correctness.

---

## Example 58: Modules (Java Platform Module System)

Modules provide stronger encapsulation than packages, enabling better dependency management and smaller runtime images. Defined via `module-info.java`, modules explicitly declare dependencies and exports.

**Code**:

```java
// module-info.java in com.example.myapp module
module com.example.myapp {      // => Module declaration (file: module-info.java at module root)
                                 // => Module name: com.example.myapp (reverse domain convention)
    // Require other modules
    requires java.base;          // => Dependency on java.base (implicit, all modules auto-require)
                                 // => Contains core classes: Object, String, System
    requires java.sql;           // => Explicit dependency on SQL module (JDBC classes)
                                 // => Module graph: myapp → java.sql → java.base
    requires transitive java.logging;  // => Transitive dependency (modules requiring myapp also get java.logging)
                                 // => Implied readability propagates dependency to consumers

    // Export packages (make them accessible to other modules)
    exports com.example.myapp.api;  // => Makes api package public to all modules
                                 // => Non-exported packages remain internal (strong encapsulation)
    exports com.example.myapp.internal to com.example.test;  // => Qualified export: only to com.example.test module
                                 // => Enables white-box testing of internals

    // Open packages for reflection (for frameworks like Spring, Hibernate)
    opens com.example.myapp.model;  // => Allows deep reflection on model package
                                 // => Frameworks can access private fields/methods (needed for serialization, DI, ORM)
    opens com.example.myapp.entity to org.hibernate.orm;  // => Qualified open: reflection only for Hibernate
                                 // => More secure than unconditional opens

    // Provide service implementation
    provides com.example.myapp.api.Service  // => Service provider declaration (SPI)
        with com.example.myapp.impl.ServiceImpl;  // => ServiceLoader can discover this implementation
                                 // => Enables plugin architecture

    // Use service
    uses com.example.myapp.api.Service;  // => Service consumer declaration
                                 // => Declares module will use ServiceLoader for Service
}

// Without modules (pre-Java 9), all public classes are globally accessible
                                 // => Classpath: no encapsulation beyond public/private/protected
// With modules, only exported packages are accessible
                                 // => Module system: strong encapsulation at package level

// Checking module from code
Module module = String.class.getModule();  // => Gets module containing String class
                                 // => Module reflection API (java.lang.Module)
System.out.println(module.getName());  // => Output: "java.base"
                                 // => String is in java.base module (core module)
System.out.println(module.isNamed());  // => Output: true
                                 // => Named module (has module-info.java), vs unnamed module (classpath code)

// Unnamed module (classpath code)
                                 // => Code on classpath runs in unnamed module
                                 // => Unnamed module can read all named modules (one-way)
// Code on classpath runs in unnamed module, can access all modules
                                 // => Enables gradual migration (mix classpath and modules)

// Module layers and layers
ModuleLayer bootLayer = ModuleLayer.boot();  // => Boot layer contains platform modules (java.base, etc.)
                                 // => ModuleLayer: container for set of modules
Set<Module> modules = bootLayer.modules();  // => Gets all modules in boot layer
                                 // => Returns Set<Module> (platform + application)
modules.forEach(m -> System.out.println(m.getName()));  // => Prints all module names in boot layer
                                 // => Output: java.base, java.sql, com.example.myapp, etc.

// Creating custom runtime images with jlink
// jlink --module-path $JAVA_HOME/jmods:mods --add-modules com.example.myapp --output customjre
                                 // => jlink tool creates custom JRE
                                 // => --module-path: where to find modules
                                 // => --add-modules: root module to include
                                 // => Transitive closure: includes all dependencies
// Creates minimal JRE with only required modules
                                 // => Only includes modules needed by myapp
                                 // => Smaller JRE: ~50MB vs ~300MB full JDK
                                 // => Faster startup: fewer modules to load

// Module visibility example
// In com.example.myapp.api package (exported):
package com.example.myapp.api;   // => Exported package (public API)
public class PublicService {     // => Public class in exported package
                                 // => Accessible: other modules can import and use
    // Accessible to other modules
                                 // => Methods, fields accessible per visibility (public/protected)
}

// In com.example.myapp.internal package (not exported):
package com.example.myapp.internal;  // => Non-exported package (internal implementation)
public class InternalUtil {      // => Public BUT in non-exported package (not accessible outside module)
    // NOT accessible to other modules, even though public
                                 // => Strong encapsulation: public doesn't mean globally accessible
}

// Module benefits:
// 1. Reliable configuration: missing dependencies detected at startup  // => Fail fast vs classpath NoClassDefFoundError
// 2. Strong encapsulation: internal packages truly internal  // => Non-exported packages inaccessible
// 3. Scalable: module graph prevents accidental dependencies  // => Explicit requires, no circular deps
// 4. Smaller deployments: jlink creates custom runtime images  // => Include only needed modules
```

**Key Takeaway**: Modules provide stronger encapsulation via `module-info.java`. `requires` declares dependencies, `exports` makes packages accessible. `opens` allows deep reflection. `transitive` propagates dependencies. Modules enable reliable configuration and smaller runtime images with jlink.

**Why It Matters**: Modules (JPMS) provide explicit dependencies (requires/exports), strong encapsulation (internal packages hidden), and improved security (reduced attack surface). They enable faster startup (optimized module graph), smaller deployments (jlink custom runtimes), and compile-time dependency checking. Understanding unnamed modules, automatic modules, and split packages enables gradual migration from classpath. Modules are essential for large codebases and library development—enforcing clean boundaries and preventing internal API usage. However, migration complexity and ecosystem maturity issues make modules optional for many applications. Use them for explicit dependency management and API protection.

---

## Example 59: var and Type Inference

`var` enables local variable type inference, reducing boilerplate while preserving static typing. The compiler infers types from initializers. Use for readability when types are obvious, avoid when clarity suffers.

**Code**:

```java
// var for local variables (Java 10+)
var message = "Hello";           // => Inferred type: String (still statically typed at compile time)
var count = 42;                  // => Inferred as int (not Integer wrapper)
var price = 19.99;               // => Inferred as double (default for decimals)

// Works with generics (reduces verbosity)
var list = new ArrayList<String>();  // => Inferred as ArrayList<String>
                                 // => Removes redundant type declaration on left
var map = new HashMap<String, Integer>();  // => Inferred as HashMap<String, Integer>

// Diamond operator with var
var names = new ArrayList<>();   // => Inferred as ArrayList<Object> (loses type safety!)
                                 // => Be careful without explicit generic
var scores = List.of(95, 87, 92);// => Inferred as List<Integer>
                                 // => Type from method return and arguments

// var in loops
var numbers = List.of(1, 2, 3, 4, 5);  // => List<Integer>
for (var num : numbers) {        // => num inferred as Integer from collection element type
    System.out.println(num);     // => Output: 1, 2, 3, 4, 5
}

for (var i = 0; i < 10; i++) {   // => i inferred as int from literal 0
    System.out.println(i);       // => Output: 0, 1, 2, ..., 9
}

// var with streams
var stream = numbers.stream()    // => Inferred type: Stream<Integer>
    .filter(n -> n > 2)          // => Filter keeps Integer type
    .map(n -> n * 2);            // => var avoids verbose Stream<Integer> declaration

// When var improves readability
var userRepository = new UserRepositoryImpl();  // => Type obvious from constructor name
                                 // => var eliminates redundancy
var configuration = ConfigurationLoader.load("config.json");  // => Type clear from method name

// When var reduces readability (avoid these)
var data = process();            // => What type is data? Not obvious, reduces clarity
                                 // => Avoid var when type unclear
var x = calculate(y);            // => Double readability problem: vague name + var
                                 // => Need to check calculate() return type

// var limitations
// Cannot use without initializer
// var x;                        // => ERROR: cannot infer type without initializer

// Cannot use with null
// var name = null;              // => ERROR: null has no specific type (ambiguous)

// Cannot use for fields
class Example {
    // var field = "value";       // => ERROR: var only for local variables
}

// Cannot use for method parameters
// void method(var param) {}     // => ERROR: method signatures require explicit types

// Cannot use for method return types
// var getValue() { return 42; } // => ERROR: return type must be explicit

// var with method references
var comparator = Comparator.comparing(String::length);  // => Inferred as Comparator<String>

// var doesn't change semantics, only reduces verbosity
var text = "Hello";              // => Still statically typed as String (type locked at compile time)
                                 // => Not dynamic typing (like JavaScript var)
// text = 42;                    // => ERROR: incompatible types (var doesn't weaken type safety)

// Best practices
// ✅ Use var when type is obvious from right-hand side
var users = userService.getAllUsers();
                                 // => Method name clearly suggests returns users
                                 // => Improves readability: focus on logic, not types

// ✅ Use var for complex generic types
var result = new HashMap<String, List<Map<String, Object>>>();
                                 // => Complex nested generics: verbose to repeat
                                 // => Right side specifies full type clearly
                                 // => var eliminates redundant left-side declaration

// ❌ Avoid var when type isn't clear
var value = compute();           // => Compute what? Type unclear
                                 // => Better: explicit type or better variable name
                                 // => Harms readability and maintainability

// ❌ Avoid var for primitives when literal type unclear
var flag = false;                // => Inferred as boolean (not Boolean)
                                 // => Obvious here, but ambiguity possible
var number = 1;                  // => Inferred as int, but could be long
                                 // => Better: int number = 1; (explicit primitive)
                                 // => Clarity over brevity for primitive literals
```

**Key Takeaway**: `var` infers local variable types from initializers, reducing boilerplate while preserving static typing. Use when types are obvious from context. Limited to local variables—not fields, parameters, or return types. Doesn't change semantics, only syntax.

**Why It Matters**: var (Java 10+) reduces boilerplate through local variable type inference—eliminating redundant type declarations while maintaining compile-time type safety. It improves readability for obvious types (var list = new ArrayList<String>()) but can harm clarity when type isn't evident. Understanding where var works (local variables with initializers) vs where it doesn't (fields, parameters, method returns) prevents misuse. var doesn't make Java dynamically typed—it's purely compile-time sugar. Use it to reduce verbosity without sacrificing type safety, improving code maintainability by focusing on logic rather than repetitive type declarations.

---

## Example 60: Garbage Collection Basics

Garbage collection automatically reclaims memory from unreachable objects. The generational hypothesis (most objects die young) drives GC design. Understanding GC helps optimize application performance.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[Java Heap] --> B[Young Generation]
    A --> C[Old Generation]
    B --> D[Eden Space]
    B --> E[Survivor S0]
    B --> F[Survivor S1]

    G[New Object] --> D
    D --> H{Survives Minor GC?}
    H -->|No| I[Collected]
    H -->|Yes| E
    E --> J{Survives multiple GCs?}
    J -->|Yes| C
    J -->|No| F

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
```

**Code**:

```java
// Object lifecycle
class MyObject {
    private byte[] data = new byte[1024]; // => 1KB per object, allocated on heap
}

// Objects created in Eden space (Young Gen)
MyObject obj1 = new MyObject(); // => Allocated in Eden space (Young Generation)
MyObject obj2 = new MyObject(); // => Also allocated in Eden space
System.out.println("Created obj1 and obj2"); // => Both objects reachable via local variables

// obj1 becomes unreachable
obj1 = null; // => obj1 now eligible for GC (no references), obj2 still reachable
// => GC will reclaim obj1's memory during next Minor GC

// Minor GC occurs when Eden fills
// => Surviving objects move to Survivor S0 or S1
// => After multiple survivals (age threshold, typically 15), promoted to Old Generation
// => Objects that die young (majority) never leave Eden

// GC roots (always reachable, prevent GC):
// 1. Local variables in active methods (stack references)
// 2. Static fields (Class metadata references)
// 3. Active threads (Thread objects and their stacks)
// 4. JNI references (native code references)

// Example: memory leak via static collection
class MemoryLeakExample {
    private static List<byte[]> cache = new ArrayList<>(); // => Static field is GC root

    public static void addToCache(byte[] data) {
        cache.add(data); // => Never removed, always reachable, grows indefinitely
        // => Eventually leads to OutOfMemoryError: Java heap space
    }
}

// Weak references allow GC even if referenced
import java.lang.ref.*;

// Strong reference (default) - prevents GC
String strongRef = new String("Will not be collected"); // => Object not collected while strongRef exists
// => strongRef is GC root (local variable), prevents collection

// Soft reference - collected only if memory pressure
SoftReference<String> softRef = new SoftReference<>(new String("Collected if needed"));
// => Object inside SoftReference can be collected if JVM needs memory
String value = softRef.get(); // => May return null if object was collected
if (value != null) {
    System.out.println(value); // => Only prints if object still in memory
}

// Weak reference - collected at next GC
WeakReference<String> weakRef = new WeakReference<>(new String("Collected soon"));
// => Creates WeakReference wrapping new String object
// => Object will be collected at next GC, regardless of memory pressure
// => Useful for caches where entries can be discarded freely
String weakValue = weakRef.get(); // => May return null after GC runs
// => get() returns wrapped object if still alive, null if collected
// => Check for null before using to avoid NullPointerException
System.out.println(weakValue); // => Prints string or throws NPE if collected
// => If GC ran: null reference causes NPE
// => If object alive: prints "Collected soon"

// Phantom reference - for cleanup hooks
ReferenceQueue<String> queue = new ReferenceQueue<>(); // => Queue for GC notifications
// => ReferenceQueue receives references after object finalized
// => Allows post-finalization cleanup actions
PhantomReference<String> phantomRef = new PhantomReference<>(
    new String("For cleanup"), queue // => Object for cleanup tracking
    // => PhantomReference never prevents collection
    // => Added to queue after object finalized, before memory reclaimed
);
// => get() always returns null, used with ReferenceQueue for post-finalization cleanup
// => Unlike finalize(), cleanup happens AFTER object finalized
// => Useful for resource cleanup (file handles, native memory, off-heap resources)
// => More reliable than finalize() for deterministic cleanup

// System.gc() suggests GC (doesn't guarantee it)
System.gc(); // => Hint to JVM to run GC, not a command (JVM may ignore)
// => Triggers full GC (both young and old generations) if honored
// => Never rely on System.gc() for deterministic cleanup
// => JVM may disable via -XX:+DisableExplicitGC flag
// => Expensive operation: pauses application threads

// Monitoring GC (via JVM flags)
// -XX:+PrintGCDetails - Print GC logs
// => Logs GC events: collections, timings, heap sizes
// => Example output: [GC (Allocation Failure) [PSYoungGen: 512K->128K(1024K)] 512K->256K(4096K), 0.001s]
// -XX:+PrintGCDateStamps - Add timestamps
// => Adds absolute timestamps to GC logs
// => Example: 2024-01-15T10:30:45.123+0000: [GC (Allocation Failure)...]
// -Xms512m -Xmx2g - Set min/max heap size
// => -Xms512m: Initial heap size (512 megabytes)
// => -Xmx2g: Maximum heap size (2 gigabytes)
// => Heap grows from Xms to Xmx as needed
// => Setting Xms = Xmx prevents heap resizing overhead

// GC types (algorithmic overview)
// Serial GC: Single-threaded, simple
// => -XX:+UseSerialGC flag
// => Stops application during collection (stop-the-world)
// => Best for: small heaps (<100MB), single-CPU systems, batch jobs
// Parallel GC: Multi-threaded, throughput-focused
// => -XX:+UseParallelGC flag (default in Java 8)
// => Multiple threads for young/old generation collection
// => Best for: throughput-critical apps, multi-CPU servers
// G1 GC: Region-based, balanced (default in Java 9+)
// => -XX:+UseG1GC flag (default in Java 9+)
// => Divides heap into regions, collects incrementally
// => Targets pause time goals (-XX:MaxGCPauseMillis)
// => Best for: large heaps (>4GB), balanced latency/throughput
// ZGC: Ultra-low latency, large heaps (Java 15+)
// => -XX:+UseZGC flag
// => Concurrent GC: <10ms pauses even for terabyte heaps
// => Best for: latency-critical apps, very large heaps (100GB+)
// Shenandoah: Low-latency, concurrent (Java 12+)
// => -XX:+UseShenandoahGC flag
// => Similar to ZGC: concurrent, low-latency
// => Best for: low-latency apps, large heaps

// Finalizers (deprecated, avoid!)
@Deprecated
// => finalize() deprecated in Java 9, removed in future versions
class BadExample {
    @Override
    protected void finalize() throws Throwable {
        // Unpredictable timing, performance impact
        // => finalize() called by GC thread before object reclaimed
        // => Timing unpredictable: may run seconds/minutes/never after object unreachable
        // => Slows GC: objects with finalizers require extra GC cycles
        // => Exceptions in finalize() silently ignored
        // Use try-with-resources or Cleaner instead
        // => Cleaner provides deterministic cleanup
        // => try-with-resources ensures cleanup via AutoCloseable.close()
    }
}

// Modern cleanup with Cleaner (Java 9+)
import java.lang.ref.Cleaner;
// => Replacement for finalize(), provides cleanup hooks

class Resource {
    private static final Cleaner cleaner = Cleaner.create();
    // => Shared Cleaner instance for all Resource objects
    // => Cleaner manages cleanup thread pool
    // => Create once per class, reuse for all instances

    private final Cleaner.Cleanable cleanable;
    // => Registration handle for this resource's cleanup action
    // => Allows manual cleanup via cleanable.clean()

    Resource() {
        this.cleanable = cleaner.register(this, new CleanupAction());
        // => Registers cleanup action for this Resource instance
        // => CleanupAction.run() invoked after Resource becomes unreachable
        // => Cleaner tracks Resource via phantom reference
        // => More reliable than finalize(): guaranteed cleanup order
    }

    private static class CleanupAction implements Runnable {
        // => MUST be static: cannot hold reference to Resource
        // => Non-static would prevent GC (circular reference)
        @Override
        public void run() {
            // Cleanup code here
            // => Invoked after Resource unreachable and finalized
            // => Runs on Cleaner thread, not GC thread
            // => Can release native resources, close file handles, etc.
            System.out.println("Resource cleaned up");
            // => Prints when cleanup triggered (automatic or manual)
        }
    }
}
```

**Key Takeaway**: GC reclaims memory from unreachable objects. Young generation (Eden + Survivor) for new objects, old generation for long-lived objects. GC roots determine reachability. Soft/weak references allow GC-friendly caching. Avoid finalizers; use Cleaner or try-with-resources.

**Why It Matters**: Garbage collection automates memory management, preventing leaks and use-after-free bugs. Understanding GC algorithms (G1, ZGC, Shenandoah) enables choosing appropriate collectors for latency vs throughput requirements. Heap sizing (-Xmx, -Xms) balances memory usage vs GC frequency. GC pauses impact application latency—low-latency systems need pause-sensitive collectors (ZGC, Shenandoah provide sub-millisecond pauses). Monitoring GC via logs and JMX reveals memory issues—leaks, excessive allocation, inappropriate heap size. Understanding GC fundamentals enables effective memory tuning, preventing OutOfMemoryError and minimizing pause impact on production applications.

---

## Example 61: Memory Management and Reference Types

Java provides four reference types to control GC behavior. Strong references prevent collection. Soft references enable memory-sensitive caches. Weak references allow collection despite references. Phantom references enable pre-mortem cleanup.

**Code**:

```java
import java.lang.ref.*;
import java.util.*;

// Strong reference (default) - prevents GC
String strong = new String("Cannot be collected while referenced");
                                 // => Strong reference prevents GC
                                 // => Object stays in memory while strong references exist
strong = null;                   // => Removes strong reference
                                 // => Object now eligible for GC

// Soft reference - memory-sensitive caching
class ImageCache {              // => Cache implementation using soft references
    private Map<String, SoftReference<byte[]>> cache = new HashMap<>();  // => Cache storage
                                 // => Map keys = cache keys (String)
                                 // => Map values = SoftReference wrapping byte[] images

    public void addImage(String key, byte[] image) {  // => Adds image to cache
        cache.put(key, new SoftReference<>(image));   // => Stores soft-referenced image
                                 // => Wraps image in SoftReference
                                 // => GC can collect image if memory pressure
    }

    public byte[] getImage(String key) {  // => Retrieves image from cache
        SoftReference<byte[]> ref = cache.get(key);  // => Gets SoftReference for key
                                 // => Retrieves SoftReference from map
        if (ref != null) {       // => Check if key exists in cache
            byte[] image = ref.get();  // => Extracts byte[] from reference
                                 // => Extract wrapped image
                                 // => Returns null if GC collected it
            if (image != null) {
                return image;    // => Cache hit: image still in memory
            } else {
                cache.remove(key);
                                 // => Image was collected, clean up map entry
            }
        }
        return null;             // => Cache miss: key not found or collected
    }
}

// Soft references cleared only when heap is nearly full
                                 // => JVM clears soft refs before OutOfMemoryError
// Ideal for caches that can be regenerated
                                 // => Data loss acceptable if memory needed

// Weak reference - collected at next GC
WeakReference<String> weak = new WeakReference<>(new String("Collected soon"));
                                 // => Wraps new String in WeakReference
                                 // => String collected at next GC regardless of memory
String value = weak.get();       // => Retrieves wrapped object
                                 // => Returns null if already collected
if (value != null) {
    System.out.println(value);   // => Prints if object still alive
} else {
    System.out.println("Already collected");
                                 // => Prints if GC ran and collected object
}

// WeakHashMap - auto-remove entries when keys collected
WeakHashMap<Object, String> weakMap = new WeakHashMap<>();
                                 // => Map with weak keys
                                 // => Entries auto-removed when key collected
Object key = new Object();       // => Strong reference to key object
weakMap.put(key, "value");       // => Add entry with weak key
System.out.println(weakMap.size());
                                 // => Output: 1 (entry present)

key = null;                      // => Remove strong reference
                                 // => Key becomes weakly reachable (only weakMap holds it)
System.gc();                     // => Suggest GC (may collect weakly reachable objects)
Thread.sleep(100);               // => Wait for GC to run
System.out.println(weakMap.size());
                                 // => Output: 0 (entry auto-removed after key collected)

// Phantom reference - for cleanup notification
class ResourceWithCleanup {
    private static ReferenceQueue<ResourceWithCleanup> queue =
        new ReferenceQueue<>();  // => Queue receives phantom refs after finalization
                                 // => Enables post-GC cleanup notifications
    private static Set<PhantomReference<ResourceWithCleanup>> refs =
        new HashSet<>();         // => Track all phantom refs to prevent collection
                                 // => PhantomReference itself must be strongly reachable

    private String resourceId;

    ResourceWithCleanup(String id) {
        this.resourceId = id;
        refs.add(new PhantomReference<>(this, queue));
                                 // => Register phantom ref for this instance
                                 // => Added to queue after object finalized
    }

    static void cleanupThread() {
        new Thread(() -> {       // => Background cleanup thread
            while (true) {
                try {
                    Reference<?> ref = queue.remove();
                                 // => Blocks until phantom ref added to queue
                                 // => Indicates object finalized, ready for cleanup
                    // Object has been finalized, perform cleanup
                    System.out.println("Cleanup triggered");
                                 // => Custom cleanup logic here
                    refs.remove(ref);
                                 // => Remove from tracking set
                } catch (InterruptedException e) {}
            }
        }).start();
    }
}

// Reference comparison
Object obj = new Object();       // => Strong reference to Object

// Strong: obj -> Object (prevents GC)
                                 // => obj is GC root, prevents collection
SoftReference<Object> soft = new SoftReference<>(obj);
                                 // => Soft reference to same Object
// Soft: obj -> SoftRef -> Object (GC if memory pressure)
                                 // => Object still protected by strong ref (obj)
                                 // => If obj set to null, only soft ref remains

WeakReference<Object> weak2 = new WeakReference<>(obj);
                                 // => Weak reference to same Object
// Weak: obj -> WeakRef -> Object (GC at next cycle)
                                 // => Object still protected by strong ref (obj)
                                 // => If obj set to null, collected at next GC

PhantomReference<Object> phantom = new PhantomReference<>(obj, new ReferenceQueue<>());
                                 // => Phantom reference to same Object
// Phantom: obj -> PhantomRef -> Object (get() always null, for cleanup)
                                 // => get() always returns null (can't retrieve object)
                                 // => Used for cleanup notification, not object access

// Reachability hierarchy (from strongest to weakest)
// 1. Strongly reachable: via strong reference chain from GC root
                                 // => Regular references (local vars, fields, etc.)
// 2. Softly reachable: only via soft references
                                 // => No strong refs, only soft refs
// 3. Weakly reachable: only via weak references
                                 // => No strong/soft refs, only weak refs
// 4. Phantom reachable: finalized, only phantom refs remain
                                 // => Finalized but not yet reclaimed
// 5. Unreachable: no references, ready for collection
                                 // => Memory can be reclaimed

// Practical: metadata cache with weak keys
class MetadataCache {
    private WeakHashMap<Class<?>, String> metadata = new WeakHashMap<>();
                                 // => Weak keys: Class<?> objects
                                 // => Entries removed when Class unloaded

    public void register(Class<?> clazz, String meta) {
        metadata.put(clazz, meta);
                                 // => Associates metadata with class
    }

    public String get(Class<?> clazz) {
        return metadata.get(clazz);
                                 // => Retrieves metadata for class
    }

    // When Class<?> objects are unloaded (classloader gone),
                                 // => Classloader unload triggers Class unload
    // entries auto-removed from map
                                 // => WeakHashMap automatically cleans up entries
}
```

**Key Takeaway**: Four reference types control GC: strong (default, prevents GC), soft (memory-sensitive caching), weak (GC-friendly caching), phantom (cleanup hooks). `WeakHashMap` auto-removes entries when keys collected. Soft references cleared under memory pressure, weak at next GC.

**Why It Matters**: Understanding reference types (strong, weak, soft, phantom) enables advanced memory management—caches that don't prevent GC, resource cleanup, memory-sensitive collections. Weak references enable caches that release entries under memory pressure. Soft references provide memory-sensitive caching—retain while memory available. Phantom references enable post-GC cleanup for native resources. Reference queues enable notification when objects are collected. These tools power memory-efficient caching, resource management, and preventing memory leaks. However, they're complex—use existing libraries (Guava caches) when possible. Understanding references enables building memory-efficient systems handling large datasets.

---

## Example 62: Performance Monitoring and Profiling

Java provides rich tools for monitoring and profiling applications. JMX exposes runtime metrics. JFR enables low-overhead production profiling. JMH provides accurate microbenchmarks. Profile before optimizing.

**Code**:

```java
import java.lang.management.*;
import javax.management.*;

// JMX - Java Management Extensions
ManagementFactory.getRuntimeMXBean().getName();
                                 // => Returns "PID@hostname" format
                                 // => Example: "12345@myserver.com"

// Memory monitoring
MemoryMXBean memoryBean = ManagementFactory.getMemoryMXBean();
                                 // => MBean for heap/non-heap memory monitoring
MemoryUsage heapUsage = memoryBean.getHeapMemoryUsage();
                                 // => Current heap memory usage statistics
System.out.println("Heap used: " + heapUsage.getUsed() / 1024 / 1024 + " MB");
                                 // => Prints current heap usage in megabytes
System.out.println("Heap max: " + heapUsage.getMax() / 1024 / 1024 + " MB");
                                 // => Prints maximum heap size (-Xmx)

// Thread monitoring
ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
                                 // => MBean for thread monitoring
System.out.println("Live threads: " + threadBean.getThreadCount());
                                 // => Current number of live threads
System.out.println("Peak threads: " + threadBean.getPeakThreadCount());
                                 // => Maximum thread count since JVM start

// Detect deadlocks
long[] deadlockedThreads = threadBean.findDeadlockedThreads();
                                 // => Returns thread IDs in deadlock
                                 // => null if no deadlock detected
if (deadlockedThreads != null) {
    System.out.println("Deadlock detected!");
                                 // => Warning when threads deadlocked
}

// GC monitoring
List<GarbageCollectorMXBean> gcBeans = ManagementFactory.getGarbageCollectorMXBeans();
                                 // => List of GC MBeans (young, old generation)
for (GarbageCollectorMXBean gcBean : gcBeans) {
    System.out.println("GC: " + gcBean.getName());
                                 // => GC name: "G1 Young Generation", "G1 Old Generation", etc.
    System.out.println("  Collections: " + gcBean.getCollectionCount());
                                 // => Number of GC collections performed
    System.out.println("  Time (ms): " + gcBean.getCollectionTime());
                                 // => Total time spent in GC (milliseconds)
}

// CPU time for current thread
long cpuTime = threadBean.getCurrentThreadCpuTime();
                                 // => CPU time for current thread (nanoseconds)
long userTime = threadBean.getCurrentThreadUserTime();
                                 // => User-mode CPU time (excludes kernel time)
System.out.println("CPU time (ns): " + cpuTime);
                                 // => Prints CPU time in nanoseconds

// Class loading monitoring
ClassLoadingMXBean classBean = ManagementFactory.getClassLoadingMXBean();
                                 // => MBean for class loading statistics
System.out.println("Loaded classes: " + classBean.getLoadedClassCount());
                                 // => Currently loaded classes
System.out.println("Total loaded: " + classBean.getTotalLoadedClassCount());
                                 // => Total classes loaded since JVM start

// Compilation monitoring (JIT)
CompilationMXBean compBean = ManagementFactory.getCompilationMXBean();
                                 // => MBean for JIT compiler monitoring
System.out.println("JIT compilation time (ms): " + compBean.getTotalCompilationTime());
                                 // => Total time JIT spent compiling (milliseconds)

// Simple performance measurement
long start = System.nanoTime();  // => High-precision timestamp (nanoseconds)
// Code to measure
for (int i = 0; i < 1000000; i++) {
    Math.sqrt(i);                // => CPU-bound operation to measure
}
long end = System.nanoTime();    // => End timestamp
System.out.println("Elapsed (ms): " + (end - start) / 1_000_000);
                                 // => Converts nanoseconds to milliseconds
                                 // => Warning: nanoTime has measurement overhead

// JMH (Java Microbenchmark Harness) - external library
// More accurate than System.nanoTime() due to warmup, JIT, etc.
                                 // => JMH handles JIT warmup, dead code elimination
                                 // => Provides statistical analysis of results
/*
@Benchmark
public void benchmarkMethod() {
    // Code to benchmark
    doWork();                    // => JMH manages iterations, warmup, statistics
}
*/

// JFR (Java Flight Recorder) - low-overhead profiling
// Start: java -XX:StartFlightRecording=duration=60s,filename=recording.jfr MyApp
                                 // => Records JVM events for 60 seconds
                                 // => Low overhead: ~1% typical
// Analyze with JDK Mission Control
                                 // => GUI tool for analyzing JFR recordings

// Common profiling tools (external)
// - jconsole: GUI for JMX monitoring
                                 // => Real-time monitoring of MBeans
// - jvisualvm: Profiling, heap dumps, thread dumps
                                 // => Free, bundled with JDK
// - JDK Mission Control: JFR analysis
                                 // => Advanced profiling, flame graphs
// - Async-profiler: Low-overhead CPU/memory profiling
                                 // => Production-safe, native profiler
// - YourKit, JProfiler: Commercial profilers
                                 // => Feature-rich, paid tools

// Heap dump on OutOfMemoryError
// -XX:+HeapDumpOnOutOfMemoryError -XX:HeapDumpPath=/path/to/dump
                                 // => Automatically dumps heap on OOM
                                 // => Enables post-mortem analysis

// Thread dump
// kill -3 <pid> (sends SIGQUIT, prints thread dump to stdout)
// jstack <pid> (external tool)

// Performance anti-patterns to avoid:
// 1. String concatenation in loops (use StringBuilder)
// 2. Excessive object creation in hot paths
// 3. Synchronization bottlenecks
// 4. Inefficient algorithms (O(n²) when O(n log n) exists)
// 5. Premature optimization (profile first!)
```

**Key Takeaway**: JMX exposes runtime metrics via MXBeans (memory, threads, GC, classes). JFR enables low-overhead production profiling. Tools: jconsole, jvisualvm, JMC. JMH for accurate microbenchmarks. Profile before optimizing—measure, don't guess.

**Why It Matters**: Performance monitoring and profiling identify bottlenecks—CPU hotspots, memory leaks, lock contention. JMX exposes runtime metrics (heap usage, thread count, GC stats) for monitoring. Profilers (JProfiler, YourKit, async-profiler) identify slow methods, allocation hotspots, and synchronization issues. Understanding sampling vs instrumentation balances accuracy vs overhead. JFR (Java Flight Recorder) provides low-overhead production profiling. Profiling prevents premature optimization by measuring before changing code. It's essential for optimizing performance-critical systems, enabling data-driven decisions about where optimization effort produces meaningful results.

---

## Example 63: Common Performance Patterns

Choosing appropriate data structures and algorithms dramatically impacts performance. StringBuilder for string building. ArrayList for indexed access. HashMap for lookups. Lazy initialization for expensive objects. Profile before optimizing.

**Code**:

```java
import java.util.*;

// String concatenation - avoid in loops
String bad = "";                 // => Immutable empty string
for (int i = 0; i < 1000; i++) {
    bad += i;                    // => Creates new String each iteration
                                 // => O(n²) complexity: copy entire string each time
                                 // => 1000 iterations = ~500,000 character copies
}

// Use StringBuilder instead
StringBuilder good = new StringBuilder();
                                 // => Mutable character buffer
                                 // => Grows dynamically without copying
for (int i = 0; i < 1000; i++) {
    good.append(i);              // => O(1) amortized append
                                 // => O(n) total complexity
}
String result = good.toString(); // => Convert to immutable String

// ArrayList vs LinkedList
List<Integer> arrayList = new ArrayList<>();
                                 // => Array-backed list: contiguous memory
List<Integer> linkedList = new LinkedList<>();
                                 // => Node-based list: linked nodes

// ArrayList: O(1) random access, O(n) insertion in middle
arrayList.get(50);               // => Fast: array index access O(1)
                                 // => Direct memory access
arrayList.add(0, 100);           // => Slow: shifts all elements right O(n)
                                 // => Array copy: expensive for large lists

// LinkedList: O(n) random access, O(1) insertion if iterator
linkedList.get(50);              // => Slow: traverses 50 nodes O(n)
                                 // => No random access: sequential traversal
Iterator<Integer> it = linkedList.iterator();
it.next();                       // => Iterator tracks position
((LinkedList<Integer>) linkedList).add(1, 100);
                                 // => Still O(n) without iterator position
                                 // => Must traverse to position 1

// Prefer ArrayList unless frequent insertions/deletions in middle
                                 // => ArrayList wins for most use cases
                                 // => Better cache locality, lower memory overhead

// HashMap resizing - set initial capacity if size known
Map<String, Integer> inefficient = new HashMap<>();
                                 // => Default capacity 16, resizes at 75% load
                                 // => Multiple resizes for 1000 entries: 16 → 32 → 64 → 128 → 256 → 512 → 1024
for (int i = 0; i < 1000; i++) {
    inefficient.put("key" + i, i);
                                 // => Multiple resizes: expensive rehashing
                                 // => Each resize: allocate new array, rehash all entries
}

Map<String, Integer> efficient = new HashMap<>(1000 * 4 / 3);
                                 // => Initial capacity ~1333: no resize needed for 1000 entries
                                 // => Load factor 0.75: resize at 1333 * 0.75 = 1000
for (int i = 0; i < 1000; i++) {
    efficient.put("key" + i, i); // => No resizes: direct insertion
                                 // => Much faster: avoids rehashing overhead
}

// Stream vs for-loop performance
List<Integer> numbers = new ArrayList<>();
for (int i = 0; i < 1000000; i++) {
    numbers.add(i);              // => Creates list with 1M integers
}

// Stream: more readable, potential parallelization, overhead
long streamSum = numbers.stream()
                                 // => Creates stream from list
    .filter(n -> n % 2 == 0)     // => Filters even numbers
                                 // => Lambda + stream overhead
    .mapToLong(n -> n)           // => Converts Integer to long
                                 // => Avoids boxing overhead in sum()
    .sum();                      // => Sums filtered numbers
                                 // => More readable but slower than loop

// For-loop: faster for simple operations, less overhead
long loopSum = 0;                // => Accumulator
for (int n : numbers) {          // => Enhanced for loop
                                 // => Direct iteration: no stream overhead
    if (n % 2 == 0) {            // => Filter condition
        loopSum += n;            // => Accumulate sum
                                 // => Primitive operations: faster than streams
    }
}                                // => ~2-3x faster than stream for simple operations

// Parallel stream for CPU-intensive work
long parallelSum = numbers.parallelStream()
                                 // => Creates parallel stream
                                 // => Uses ForkJoinPool.commonPool()
    .filter(n -> n % 2 == 0)     // => Parallel filtering
    .mapToLong(n -> n)           // => Parallel mapping
    .sum();                      // => Parallel reduction
                                 // => Faster for large datasets, CPU-bound tasks
                                 // => Overhead: only worthwhile for large data/expensive operations

// Lazy initialization - delay expensive object creation
class ExpensiveObject {
    private static ExpensiveObject instance;
                                 // => Static field: shared across all calls

    public static ExpensiveObject getInstance() {
        if (instance == null) {  // => Lazy: check if already created
            instance = new ExpensiveObject();
                                 // => Created only when needed
                                 // => Not thread-safe: race condition possible
        }
        return instance;         // => Returns singleton instance
    }
}

// Thread-safe lazy initialization (double-checked locking)
class ThreadSafeLazy {
    private static volatile ThreadSafeLazy instance;
                                 // => volatile: ensures visibility across threads
                                 // => Prevents partial construction visibility

    public static ThreadSafeLazy getInstance() {
        if (instance == null) {  // => First check (no locking)
                                 // => Fast path: if already initialized, skip lock
            synchronized (ThreadSafeLazy.class) {
                                 // => Lock only if instance null
                                 // => Prevents multiple threads creating instances
                if (instance == null) {
                                 // => Second check (with lock)
                                 // => Prevents race: another thread may have created between checks
                    instance = new ThreadSafeLazy();
                                 // => Creates instance inside lock
                }
            }
        }
        return instance;         // => Returns singleton instance
                                 // => Thread-safe: only one instance created
    }
}

// Object pooling (rarely beneficial in modern JVMs)
// Modern GC is fast; pooling adds complexity
                                 // => Modern GC (G1, ZGC) very efficient at allocation
                                 // => Pooling adds complexity: thread-safety, eviction
// Only consider for:
// - JDBC connections       // => Connection creation expensive (network handshake)
// - Thread pools            // => Thread creation expensive (OS resources)
// - Expensive external resources
                                 // => Native resources: file handles, sockets
                                 // => Don't pool regular Java objects

// Avoiding unnecessary object creation
// Bad
Integer badCount = new Integer(42);
                                 // => Creates new Integer object
                                 // => Bypasses Integer cache
                                 // => Deprecated: don't use constructor
// Good
Integer goodCount = 42;          // => Uses Integer.valueOf() cache (-128 to 127)
                                 // => Returns cached Integer for small values
                                 // => No object creation for cached range

// EnumSet/EnumMap - efficient for enums
enum Color { RED, GREEN, BLUE }
Set<Color> colors = EnumSet.of(Color.RED, Color.BLUE);
                                 // => Bit vector internally: very efficient
                                 // => 64 bits = 64 enums without allocation
                                 // => Much faster than HashSet for enums

// Effective caching with appropriate eviction
Map<String, String> cache = new LinkedHashMap<>(100, 0.75f, true) {
                                 // => LinkedHashMap with access-order mode
                                 // => true = access-order (LRU), false = insertion-order
    @Override
    protected boolean removeEldestEntry(Map.Entry<String, String> eldest) {
        return size() > 100;     // => LRU cache with max 100 entries
                                 // => Automatically evicts oldest accessed entry
                                 // => Called after each put: returns true to remove eldest
    }
};
```

**Key Takeaway**: Use `StringBuilder` for string concatenation in loops. Prefer `ArrayList` over `LinkedList` for most cases. Set HashMap initial capacity to avoid resizes. Streams for readability, loops for raw performance. Lazy initialization delays expensive object creation. Profile before optimizing.

**Why It Matters**: Common performance patterns (object pooling, lazy initialization, caching, batching) optimize hot paths in production code. Understanding when to apply each pattern prevents both premature optimization and performance bugs. Object pooling reduces allocation in high-throughput paths (connection pools, buffer pools). Lazy initialization defers expensive work until needed. Caching trades memory for speed—memoizing expensive computations. Batching reduces per-operation overhead (batch database inserts, network requests). However, patterns have costs—pooling adds complexity, caching adds memory pressure, batching adds latency. Measure before optimizing, and understand trade-offs.

---

## Example 64: Connection Pool Factory Pattern

Production database applications use connection pooling to reuse expensive database connections. This example demonstrates Singleton (pool manager), Factory (connection creation), and Builder (configuration) patterns in a real-world context.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Client1["Client Thread 1"] --> Pool["ConnectionPool<br/>(Singleton)"]
    Client2["Client Thread 2"] --> Pool
    Client3["Client Thread 3"] --> Pool

    Pool --> Available["Available<br/>Connections"]
    Pool --> InUse["In-Use<br/>Connections"]

    Available --> Conn1["Connection 1"]
    Available --> Conn2["Connection 2"]
    InUse --> Conn3["Connection 3"]

    Pool --> Factory["ConnectionFactory<br/>(creates connections)"]
    Factory --> DB["Database"]

    style Client1 fill:#0173B2,color:#fff
    style Client2 fill:#0173B2,color:#fff
    style Client3 fill:#0173B2,color:#fff
    style Pool fill:#DE8F05,color:#fff
    style Available fill:#029E73,color:#fff
    style InUse fill:#CC78BC,color:#fff
    style Factory fill:#CA9161,color:#fff
    style DB fill:#0173B2,color:#fff
```

**Code**:

```java
import java.sql.*;            // => JDBC API: Connection, ResultSet, SQLException
                                 // => Core database connectivity interfaces
import java.util.*;             // => Utilities: List, Set, Queue
import java.util.concurrent.*;  // => Concurrency: ConcurrentLinkedQueue, CopyOnWriteArraySet

// Database connection interface
interface DatabaseConnection {
                                 // => Abstraction for different database types (Strategy pattern)
                                 // => Enables polymorphism: PostgresConnection, MySQLConnection
                                 // => Contract: all implementations must provide these methods
    void connect() throws SQLException;
                                 // => Method signature: no return value, may throw SQLException
                                 // => Opens database connection (expensive operation: 100-1000ms)
                                 // => May throw SQLException for connection failures
    void disconnect();           // => Closes connection and releases resources
                                 // => Method signature: no parameters, no return value
    ResultSet executeQuery(String sql) throws SQLException;
                                 // => Executes SQL query, returns result set
                                 // => Method signature: accepts SQL string, may throw SQLException
    boolean isConnected();       // => Checks if connection is currently open
                                 // => Method signature: returns boolean (true if connected)
    String getConnectionString();// => Returns JDBC connection string
                                 // => Method signature: returns String (connection URL)
}
                                 // => End of DatabaseConnection interface

// PostgreSQL connection implementation
class PostgresConnection implements DatabaseConnection {
                                 // => Concrete implementation for PostgreSQL databases
                                 // => JDBC URL pattern: jdbc:postgresql://host:port/database
    private Connection connection;
                                 // => Actual JDBC connection object (java.sql.Connection)
                                 // => null until connect() called
    private final String connString;
                                 // => Immutable connection string (formatted in constructor)

    public PostgresConnection(String host, int port, String database) {
                                 // => Constructor parameters: host, port, database name
        this.connString = String.format("jdbc:postgresql://%s:%d/%s",
                                       host, port, database);
                                 // => Example: "jdbc:postgresql://localhost:5432/mydb"
                                 // => String.format builds JDBC URL from components
    }

    @Override
    public void connect() throws SQLException {
                                 // => Opens connection to PostgreSQL database
        connection = DriverManager.getConnection(connString);
                                 // => DriverManager finds JDBC driver and establishes connection
                                 // => Expensive operation: TCP handshake + authentication + initialization
                                 // => May take 100-1000ms depending on network/database
        System.out.println("Connected to PostgreSQL: " + connString);
                                 // => Output: "Connected to PostgreSQL: jdbc:postgresql://localhost:5432/mydb"
    }

    @Override
    public void disconnect() {   // => Closes connection and releases resources
        try {
                                 // => try-catch wraps close operation to handle exceptions
            if (connection != null) connection.close();
                                 // => Closes JDBC connection (releases TCP socket, database session)
                                 // => null check prevents NPE if connect() never called
            System.out.println("Disconnected from PostgreSQL");
                                 // => Output: "Disconnected from PostgreSQL"
        } catch (SQLException e) {
                                 // => Catches errors during disconnect (rare but possible)
            e.printStackTrace(); // => Logs exception stack trace
        }
    }

    @Override
    public ResultSet executeQuery(String sql) throws SQLException {
                                 // => Executes SELECT query and returns results
        return connection.createStatement().executeQuery(sql);
                                 // => Creates Statement, executes query, returns ResultSet
                                 // => ResultSet contains query results (rows and columns)
    }

    @Override
    public boolean isConnected() {
                                 // => Checks if connection is currently open and valid
        try {
                                 // => try-catch handles potential SQLException from isClosed()
            return connection != null && !connection.isClosed();
                                 // => true if connection exists and not closed
                                 // => false if null or closed
        } catch (SQLException e) {
                                 // => isClosed() may throw SQLException (rare)
            return false;        // => Return false on exception (treat as disconnected)
        }
    }

    @Override
    public String getConnectionString() { return connString; }
                                 // => Returns JDBC connection string (e.g., "jdbc:postgresql://localhost:5432/mydb")
                                 // => Useful for debugging or logging connection details
}
                                 // => End of PostgresConnection class

// MySQL connection implementation
class MySQLConnection implements DatabaseConnection {
                                 // => Concrete implementation for MySQL databases
                                 // => JDBC URL pattern: jdbc:mysql://host:port/database
                                 // => Parallel to PostgresConnection (Factory can return either)
    private Connection connection;
                                 // => JDBC connection object (null until connect() called)
    private final String connString;
                                 // => Immutable MySQL connection string

    public MySQLConnection(String host, int port, String database) {
                                 // => Constructor for MySQL connection parameters
        this.connString = String.format("jdbc:mysql://%s:%d/%s",
                                       host, port, database);
                                 // => Example: "jdbc:mysql://localhost:3306/mydb"
                                 // => Note: MySQL default port 3306 (vs Postgres 5432)
    }

    @Override
    public void connect() throws SQLException {
                                 // => Opens connection to MySQL database
        connection = DriverManager.getConnection(connString);
                                 // => DriverManager finds MySQL JDBC driver (com.mysql.cj.jdbc.Driver)
                                 // => Establishes connection: TCP handshake + authentication + MySQL handshake
        System.out.println("Connected to MySQL: " + connString);
                                 // => Output: "Connected to MySQL: jdbc:mysql://localhost:3306/mydb"
    }

    @Override
    public void disconnect() {   // => Closes MySQL connection
        try {
                                 // => try-catch wraps close to handle SQLException
            if (connection != null) connection.close();
                                 // => Closes connection, releases MySQL session
            System.out.println("Disconnected from MySQL");
                                 // => Output: "Disconnected from MySQL"
        } catch (SQLException e) {
            e.printStackTrace(); // => Logs disconnect errors (rare)
        }
    }

    @Override
    public ResultSet executeQuery(String sql) throws SQLException {
                                 // => Executes SQL query against MySQL database
        return connection.createStatement().executeQuery(sql);
                                 // => Creates Statement, executes query, returns ResultSet with rows
    }

    @Override
    public boolean isConnected() {
                                 // => Checks MySQL connection status
        try {
                                 // => try-catch handles potential SQLException
            return connection != null && !connection.isClosed();
                                 // => true if connection valid and open
        } catch (SQLException e) {
            return false;        // => false on exception or if closed
        }
    }

    @Override
    public String getConnectionString() { return connString; }
                                 // => Returns MySQL JDBC connection string
                                 // => Example: "jdbc:mysql://localhost:3306/mydb"
}
                                 // => End of MySQLConnection class

// Factory for creating database connections
class ConnectionFactory {        // => Factory pattern: creates objects without exposing creation logic
                                 // => Centralizes connection creation, supports multiple database types
    public static DatabaseConnection create(String dbType, String host,
                                           int port, String database) {
                                 // => Factory method returns interface type (DatabaseConnection)
                                 // => Decouples client from concrete implementations
                                 // => Static factory method (no instance needed)
                                 // => Parameters: database type, host, port, database name
        return switch (dbType.toLowerCase()) {
                                 // => Switch expression on database type (case-insensitive)
                                 // => Returns appropriate DatabaseConnection implementation
            case "postgres", "postgresql" ->
                new PostgresConnection(host, port, database);
                                 // => Returns PostgresConnection for "postgres" or "postgresql"
            case "mysql" ->
                new MySQLConnection(host, port, database);
                                 // => Returns MySQLConnection for "mysql"
            default ->
                throw new IllegalArgumentException("Unsupported database: " + dbType);
                                 // => Throws exception for unsupported database types
                                 // => Example error: "Unsupported database: oracle"
        };
                                 // => Switch expression returns DatabaseConnection instance
    }
}
                                 // => End of ConnectionFactory class

// Connection configuration using Builder pattern
class ConnectionConfig {        // => Immutable configuration object (all fields final)
                                 // => Uses Builder pattern to construct complex configuration
    private final String dbType; // => Database type: "postgres", "mysql"
    private final String host;   // => Database host: "localhost", "db.example.com"
    private final int port;      // => Database port: 5432 (Postgres), 3306 (MySQL)
    private final String database;
                                 // => Database name: "mydb", "production"
    private final int poolSize;  // => Number of connections in pool (default: 10)
    private final int timeout;   // => Connection timeout in seconds (default: 30)

    private ConnectionConfig(Builder builder) {
                                 // => Private constructor (only Builder can create instances)
                                 // => Ensures Builder pattern enforced
        this.dbType = builder.dbType;
                                 // => Copies dbType from Builder
        this.host = builder.host;
                                 // => Copies host from Builder
        this.port = builder.port;
                                 // => Copies port from Builder (may be default)
        this.database = builder.database;
                                 // => Copies database name from Builder
        this.poolSize = builder.poolSize;
                                 // => Copies poolSize from Builder (default: 10)
        this.timeout = builder.timeout;
                                 // => Copies timeout from Builder (default: 30)
    }

    // Fluent Builder
    public static class Builder {
                                 // => Inner static Builder class (nested within ConnectionConfig)
                                 // => Provides fluent API for building configuration
        private final String dbType;
                                 // => Required field: database type (no default)
        private final String host;
                                 // => Required field: host (no default)
        private final String database;
                                 // => Required field: database name (no default)
        private int port = 5432; // Default for Postgres
                                 // => Field initialization sets default value
                                 // => Optional field with default: Postgres port
        private int poolSize = 10;
                                 // => Optional field with default: 10 connections
        private int timeout = 30;
                                 // => Optional field with default: 30 seconds

        public Builder(String dbType, String host, String database) {
                                 // => Constructor requires only mandatory fields
                                 // => Optional fields use defaults (can be overridden with setters)
            this.dbType = dbType;
                                 // => Sets required database type
            this.host = host;    // => Sets required host
            this.database = database;
                                 // => Sets required database name
        }

        public Builder port(int port) {
                                 // => Optional setter for port (overrides default)
            this.port = port;    // => Sets custom port (e.g., 3306 for MySQL)
            return this; // => Fluent API
                                 // => Returns this Builder for method chaining
                                 // => Enables: builder.port(3306).poolSize(20).build()
        }

        public Builder poolSize(int size) {
                                 // => Optional setter for pool size
                                 // => Method chaining: returns Builder
            this.poolSize = size;
                                 // => Sets number of connections (e.g., 20, 50)
                                 // => Overrides default value of 10
            return this;         // => Returns this for chaining
                                 // => Enables fluent API
        }

        public Builder timeout(int seconds) {
                                 // => Optional setter for connection timeout
                                 // => Method chaining pattern
            this.timeout = seconds;
                                 // => Sets timeout in seconds (e.g., 60, 120)
                                 // => Overrides default of 30 seconds
            return this;         // => Returns this for chaining
                                 // => Fluent API continuation
        }

        public ConnectionConfig build() {
                                 // => Terminal operation: creates ConnectionConfig instance
                                 // => Finalizes builder pattern (no more method chaining)
            return new ConnectionConfig(this);
                                 // => Calls private constructor with this Builder
                                 // => Returns immutable ConnectionConfig
                                 // => Builder fields copied to final configuration
        }
    }
                                 // => End of Builder inner class

    public String getDbType() { return dbType; }
                                 // => Getter for database type
    public String getHost() { return host; }
                                 // => Getter for host
    public int getPort() { return port; }
                                 // => Getter for port
    public String getDatabase() { return database; }
                                 // => Getter for database name
    public int getPoolSize() { return poolSize; }
                                 // => Getter for pool size
    public int getTimeout() { return timeout; }
                                 // => Getter for timeout
                                 // => Returns timeout value in seconds
}
                                 // => End of ConnectionConfig class

// Connection Pool using Singleton pattern
class ConnectionPool {          // => Singleton pattern: only one pool instance per application
                                 // => Manages connection reuse (pool), prevents connection exhaustion
    private static volatile ConnectionPool instance; // Singleton instance
                                 // => volatile: ensures visibility across threads (happens-before)
                                 // => static: single instance shared across all callers
                                 // => null initially, created on first getInstance() call
    private final Queue<DatabaseConnection> availableConnections;
                                 // => Queue of connections available for use (not currently in use)
                                 // => ConcurrentLinkedQueue: thread-safe, lock-free queue
    private final Set<DatabaseConnection> inUseConnections;
                                 // => Set of connections currently in use by clients
                                 // => ConcurrentHashMap.newKeySet(): thread-safe set
    private final ConnectionConfig config;
                                 // => Configuration: database type, host, pool size, etc.

    private ConnectionPool(ConnectionConfig config) {
                                 // => Private constructor (Singleton: no public instantiation)
                                 // => Only getInstance() can create instance
                                 // => Constructor called once by getInstance()
        this.config = config;    // => Saves configuration
                                 // => Stores reference to immutable configuration
                                 // => Configuration contains dbType, host, port, database, poolSize, timeout
        this.availableConnections = new ConcurrentLinkedQueue<>();
                                 // => Initializes available connections queue (thread-safe)
                                 // => ConcurrentLinkedQueue: lock-free, unbounded queue
        this.inUseConnections = ConcurrentHashMap.newKeySet();
                                 // => Initializes in-use connections set (thread-safe)
                                 // => newKeySet() creates Set backed by ConcurrentHashMap

        // Pre-create pool connections
        for (int i = 0; i < config.getPoolSize(); i++) {
                                 // => Loops poolSize times (default: 10)
                                 // => Pre-creates connections to avoid lazy initialization delays
                                 // => Warm start: connections ready immediately
            DatabaseConnection conn = ConnectionFactory.create(
                config.getDbType(),
                config.getHost(),
                config.getPort(),
                config.getDatabase()
            );                   // => Creates connection using Factory pattern
                                 // => Connection not yet opened (connect() not called)
            availableConnections.offer(conn);
                                 // => Adds connection to available queue
                                 // => offer(): adds to tail of queue (non-blocking)
                                 // => Connection ready for use via acquire()
        }
        System.out.println("Connection pool initialized with " +
                         config.getPoolSize() + " connections");
                                 // => Logs initialization completion
                                 // => Output: "Connection pool initialized with 10 connections"
    }

    // Thread-safe Singleton with double-checked locking
    public static ConnectionPool getInstance(ConnectionConfig config) {
                                 // => Static factory method for Singleton instance
                                 // => Thread-safe: uses double-checked locking pattern
        if (instance == null) {  // => First check (no synchronization, fast path)
                                 // => Avoids synchronization overhead if already initialized
                                 // => Optimistic check: most calls take this fast path
            synchronized (ConnectionPool.class) {
                                 // => Synchronizes on class object (ensures single instance)
                                 // => Lock acquired: other threads block here
                                 // => synchronized block ensures mutual exclusion
                if (instance == null) {
                                 // => Second check inside synchronized block
                                 // => Prevents race condition: two threads pass first check
                    instance = new ConnectionPool(config);
                                 // => Creates Singleton instance (called once)
                }
            }
        }
        return instance;         // => Returns existing instance (all subsequent calls)
                                 // => volatile ensures visibility across threads
    }

    public DatabaseConnection acquire() throws InterruptedException {
                                 // => Acquires connection from pool (blocking if pool exhausted)
                                 // => throws InterruptedException if thread interrupted while waiting
        DatabaseConnection conn = availableConnections.poll();
                                 // => poll() is non-blocking (returns immediately)
                                 // => poll(): removes and returns head of queue (non-blocking)
                                 // => Returns null if queue empty (no available connections)
        if (conn == null) {      // => Pool exhausted: no available connections
                                 // => All connections currently in use
            System.out.println("Pool exhausted, creating new connection");
                                 // => Output: "Pool exhausted, creating new connection"
                                 // => Indicates pool undersized or connection leak
                                 // => Warning: dynamic creation can lead to unbounded growth
            conn = ConnectionFactory.create(
                config.getDbType(),
                                 // => Passes database type from config
                config.getHost(),
                                 // => Passes host from config
                config.getPort(),
                                 // => Passes port from config
                config.getDatabase()
                                 // => Passes database name from config
            );                   // => Creates new connection dynamically (overflow strategy)
                                 // => Alternative: block until connection available (HikariCP strategy)
                                 // => Factory method handles database-specific creation
        }
        inUseConnections.add(conn);
                                 // => Thread-safe add operation (ConcurrentHashMap.newKeySet())
                                 // => Adds connection to in-use set
                                 // => Tracks borrowed connections for leak detection
        return conn; // => Connection from pool or newly created
                                 // => Caller must call release(conn) when done
                                 // => Connection leak if release() not called (pool exhaustion)
    }

    public void release(DatabaseConnection conn) {
                                 // => Returns connection to pool for reuse
                                 // => MUST be called after acquire() to prevent leaks
        if (inUseConnections.remove(conn)) {
                                 // => Removes from in-use set
                                 // => Returns true if conn was in set (valid release)
                                 // => Returns false if conn not in set (double-release or invalid)
            availableConnections.offer(conn);
                                 // => Adds connection back to available queue
                                 // => Connection now available for next acquire() call
                                 // => offer() adds to tail of queue (non-blocking)
            System.out.println("Connection returned to pool");
                                 // => Output: "Connection returned to pool"
        }
                                 // => If remove() returns false, connection not released (already released or invalid)
    }

    public void shutdown() {     // => Closes all connections and shuts down pool
        for (DatabaseConnection conn : availableConnections) {
                                 // => Iterates available connections
                                 // => Enhanced for loop over queue elements
            conn.disconnect();   // => Closes each available connection
                                 // => Releases database resources
        }
        for (DatabaseConnection conn : inUseConnections) {
                                 // => Iterates in-use connections (may be leaked)
                                 // => Enhanced for loop over set elements
            conn.disconnect();   // => Closes each in-use connection (cleanup)
                                 // => Force-closes even if not properly released
        }
        System.out.println("Connection pool shut down");
                                 // => Output: "Connection pool shut down"
                                 // => Indicates cleanup complete
    }

    public int getAvailableCount() { return availableConnections.size(); }
                                 // => Returns number of available connections (not in use)
                                 // => Useful for monitoring pool utilization
    public int getInUseCount() { return inUseConnections.size(); }
                                 // => Returns number of in-use connections (borrowed)
                                 // => Helps detect connection leaks
}
                                 // => End of ConnectionPool class

// Usage example combining all patterns
ConnectionConfig config = new ConnectionConfig.Builder("postgres", "localhost", "mydb")
                                 // => Creates Builder with required fields: dbType, host, database
                                 // => Builder pattern: fluent API for complex configuration
    .port(5432)                  // => Optional: sets port to 5432 (Postgres default)
                                 // => Method chaining: port() returns Builder
    .poolSize(20)                // => Optional: sets pool size to 20 connections (default: 10)
    .timeout(60)                 // => Optional: sets timeout to 60 seconds (default: 30)
    .build(); // => Builder pattern
                                 // => Terminal operation: builds immutable ConnectionConfig
                                 // => config is now ready for ConnectionPool.getInstance()

ConnectionPool pool = ConnectionPool.getInstance(config); // => Singleton
                                 // => Gets Singleton ConnectionPool instance
                                 // => First call creates pool with 20 pre-initialized connections
                                 // => Subsequent calls return same instance (Singleton)

// Acquire connection from pool
DatabaseConnection conn = pool.acquire(); // => Factory creates connection
                                 // => Acquires connection from available queue
                                 // => Returns existing connection (fast: ~1ms) or creates new (slow: 100-1000ms)
                                 // => conn must be released in finally block to avoid leaks
try {
                                 // => try-catch-finally ensures connection always released
    conn.connect();              // => Opens connection (TCP handshake + authentication)
                                 // => May throw SQLException
    ResultSet rs = conn.executeQuery("SELECT * FROM users");
                                 // => Executes SQL query, returns ResultSet
                                 // => rs contains all rows from users table
    // Process results...
                                 // => Placeholder for result processing logic (iterate ResultSet rows)
} catch (SQLException e) {       // => Catches SQL exceptions (connection failures, query errors)
    e.printStackTrace();         // => Logs exception stack trace
                                 // => Prints SQLException details to stderr
} finally {
                                 // => finally block always executes (even if exception thrown)
    pool.release(conn); // => Return to pool for reuse
                                 // => CRITICAL: always release in finally block
                                 // => Failure to release causes connection leak and pool exhaustion
                                 // => Connection returned to availableConnections queue
}

// Pool statistics
System.out.println("Available: " + pool.getAvailableCount());
                                 // => Prints number of available connections
                                 // => Output: "Available: 19" (20 total - 1 in use)
                                 // => Shows connections ready for reuse
System.out.println("In use: " + pool.getInUseCount());
                                 // => Output: "In use: 1" (conn acquired but not released yet)
                                 // => Shows connections currently borrowed
                                 // => Helps monitor pool usage and detect leaks

pool.shutdown(); // Cleanup
                                 // => Closes all connections (available + in-use)
                                 // => Call on application shutdown (cleanup resources)
                                 // => Releases all database resources
```

**Key Takeaway**: This demonstrates all three creational patterns in production context. **Singleton** ensures one connection pool instance (thread-safe with double-checked locking). **Factory** creates database connections by type (Postgres, MySQL) enabling polymorphism. **Builder** constructs complex configuration with fluent API, avoiding telescoping constructors. Connection pooling is essential for production databases—creating connections is expensive (100-1000ms), reusing them is fast (1ms). This pattern appears in all production database libraries (HikariCP, C3P0, Apache DBCP).

**Why It Matters**: Connection pooling reuses database connections, eliminating expensive connection setup (TCP handshake, authentication, initialization). Pools prevent exhaustion by limiting max connections, and improve performance by maintaining warm connections. Understanding pool sizing (max connections, min idle, connection timeout) optimizes performance vs resource usage. HikariCP is the standard high-performance pool. Connection leaks (not returning connections) cause pool exhaustion and application failure—use try-with-resources. Pooling is essential for scalable database-backed applications—unpooled connections can't handle production load. Similar patterns apply to thread pools, object pools, and other expensive resources.

---

## Example 65: Strategy, Observer, Decorator

Behavioral patterns define communication between objects. Strategy encapsulates algorithms. Observer enables one-to-many notifications. Decorator adds responsibilities dynamically without subclassing.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[Context] --> B[Strategy Interface]
    B --> C[ConcreteStrategyA]
    B --> D[ConcreteStrategyB]
    B --> E[ConcreteStrategyC]

    F[Client] --> A
    F -->|Selects| C

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
    style E fill:#029E73,color:#fff
```

**Code**:

```java
// Strategy pattern - encapsulate algorithms

interface PaymentStrategy {
    // => Defines contract for payment algorithms
    // => Different implementations = different payment methods
    void pay(int amount);
    // => All strategies implement same interface method
    // => Enables runtime strategy swapping
}

class CreditCardStrategy implements PaymentStrategy {
    // => Concrete strategy: credit card payment
    // => Encapsulates credit card payment logic
    private String cardNumber;
    // => Strategy-specific data (card number)
    // => Each strategy can have different fields

    public CreditCardStrategy(String cardNumber) {
        this.cardNumber = cardNumber;
        // => Constructor receives strategy-specific configuration
        // => Enables parameterized strategy instances
    }

    @Override
    public void pay(int amount) {
        System.out.println("Paid " + amount + " using Credit Card " + cardNumber);
        // => Strategy-specific implementation
        // => In real app: calls payment gateway API
        // => Prints: "Paid 100 using Credit Card 1234-5678"
    }
}

class PayPalStrategy implements PaymentStrategy {
    // => Alternative concrete strategy: PayPal payment
    // => Same interface, different implementation
    private String email;
    // => PayPal-specific data (email instead of card number)
    // => Strategies can have different internal structures

    public PayPalStrategy(String email) {
        this.email = email;
        // => PayPal requires email, not card number
        // => Each strategy configured independently
    }

    @Override
    public void pay(int amount) {
        System.out.println("Paid " + amount + " using PayPal " + email);
        // => PayPal-specific implementation
        // => Different logic, same interface contract
        // => Prints: "Paid 50 using PayPal user@example.com"
    }
}

class ShoppingCart {
    // => Context: uses strategy without knowing concrete type
    // => Delegates payment to strategy object
    private PaymentStrategy paymentStrategy;
    // => Holds reference to strategy interface, not concrete class
    // => Enables runtime strategy switching

    public void setPaymentStrategy(PaymentStrategy strategy) {
        // => Runtime strategy selection
        // => Can change payment method on-the-fly
        this.paymentStrategy = strategy;
        // => Stores new strategy, replaces previous one
        // => No code changes needed to add new payment methods
    }

    public void checkout(int amount) {
        paymentStrategy.pay(amount);
        // => Delegates to strategy's pay() method
        // => ShoppingCart doesn't know which strategy (credit card, PayPal, etc.)
        // => Open/closed principle: open for extension, closed for modification
    }
}

ShoppingCart cart = new ShoppingCart();
// => Creates context (shopping cart) without strategy
// => Strategy set later via setPaymentStrategy()
cart.setPaymentStrategy(new CreditCardStrategy("1234-5678"));
// => Runtime strategy selection: credit card
// => Passes card number to strategy constructor
cart.checkout(100); // => "Paid 100 using Credit Card 1234-5678"
// => Executes credit card payment strategy
// => ShoppingCart delegates to CreditCardStrategy.pay()

cart.setPaymentStrategy(new PayPalStrategy("user@example.com"));
// => Changes strategy at runtime without modifying ShoppingCart code
// => New strategy with different configuration (email instead of card)
cart.checkout(50); // => "Paid 50 using PayPal user@example.com"
// => Executes PayPal payment strategy
// => Same checkout() method, different behavior

// Strategy with lambdas (simpler for simple algorithms)
interface Comparator<T> {
    // => Functional interface: single abstract method
    // => Can be implemented with lambda expressions
    int compare(T o1, T o2);
    // => Returns: negative (o1 < o2), zero (equal), positive (o1 > o2)
    // => Enables custom sorting logic
}

List<String> names = Arrays.asList("Alice", "Bob", "Charlie");
// => Initial list: ["Alice", "Bob", "Charlie"]
// => Arrays.asList() creates fixed-size list
names.sort((a, b) -> a.length() - b.length()); // Lambda as strategy
// => Lambda implements Comparator interface
// => Sorts by string length: "Bob" (3), "Alice" (5), "Charlie" (7)
// => Result: ["Bob", "Alice", "Charlie"]
// => Strategy pattern simplified: no explicit classes needed for simple algorithms

// Observer pattern - one-to-many dependency

interface Observer {
    // => Defines contract for observers (subscribers)
    // => Observers notified when subject state changes
    void update(String message);
    // => Callback method invoked by subject
    // => Receives notification with message payload
}

class Subject {
    // => Observable object (publisher)
    // => Maintains list of observers, notifies them on state change
    private List<Observer> observers = new ArrayList<>();
    // => Stores all registered observers
    // => One-to-many relationship: one subject, multiple observers

    public void attach(Observer observer) {
        // => Registers new observer (subscribe)
        // => Observer added to notification list
        observers.add(observer);
        // => Now observer will receive all future notifications
        // => No limit on number of observers
    }

    public void detach(Observer observer) {
        // => Unregisters observer (unsubscribe)
        // => Observer removed from notification list
        observers.remove(observer);
        // => Observer no longer receives notifications
        // => Prevents memory leaks from abandoned observers
    }

    public void notifyObservers(String message) {
        // => Broadcasts message to all registered observers
        // => Called when subject state changes
        for (Observer observer : observers) {
            // => Iterates through all registered observers
            // => Order dependent on registration sequence
            observer.update(message);
            // => Invokes each observer's update() callback
            // => Push model: subject pushes data to observers
        }
    }
}

class EmailObserver implements Observer {
    // => Concrete observer: sends email notifications
    // => Implements update() to define notification behavior
    @Override
    public void update(String message) {
        System.out.println("Email sent: " + message);
        // => Observer-specific action: send email
        // => In real app: calls email service API
        // => Prints: "Email sent: New update available"
    }
}

class SMSObserver implements Observer {
    // => Alternative concrete observer: sends SMS notifications
    // => Same interface, different notification channel
    @Override
    public void update(String message) {
        System.out.println("SMS sent: " + message);
        // => Observer-specific action: send SMS
        // => Different behavior, same interface contract
        // => Prints: "SMS sent: New update available"
    }
}

Subject subject = new Subject();
// => Creates observable object (publisher)
// => No observers yet (empty list)
subject.attach(new EmailObserver());
// => Registers first observer: email notifications
// => Subject now notifies email observer on state change
subject.attach(new SMSObserver());
// => Registers second observer: SMS notifications
// => Subject now notifies both observers
// => Easy to add new notification channels without modifying Subject
subject.notifyObservers("New update available");
// => Triggers notification to all observers
// => Both observers receive same message
// => Email observer prints: "Email sent: New update available"
// => SMS observer prints: "SMS sent: New update available"
// => Output:
// => Email sent: New update available
// => SMS sent: New update available

// Decorator pattern - add behavior dynamically

interface Coffee {
    // => Component interface: base coffee contract
    // => Both base coffee and decorators implement this interface
    double cost();
    // => Returns total cost (base + decorations)
    // => Decorators add their cost to wrapped object's cost
    String description();
    // => Returns full description (base + decorations)
    // => Decorators append their description to wrapped object's description
}

class SimpleCoffee implements Coffee {
    // => Concrete component: base coffee without decorations
    // => Starting point for decoration chain
    @Override
    public double cost() { return 2.0; }
    // => Base cost: $2.00
    // => Decorators add to this cost

    @Override
    public String description() { return "Simple coffee"; }
    // => Base description: "Simple coffee"
    // => Decorators append to this description
}

// Decorator base class
abstract class CoffeeDecorator implements Coffee {
    // => Abstract decorator: wraps Coffee object
    // => All concrete decorators extend this class
    protected Coffee coffee;
    // => Wrapped coffee object (base or another decorator)
    // => Enables decorator chaining (decorator wraps decorator)

    public CoffeeDecorator(Coffee coffee) {
        // => Constructor receives coffee to wrap
        // => Delegation pattern: decorator delegates to wrapped object
        this.coffee = coffee;
        // => Stores wrapped coffee for delegation
        // => Forms decorator chain: SimpleCoffee -> MilkDecorator -> SugarDecorator
    }
}

class MilkDecorator extends CoffeeDecorator {
    // => Concrete decorator: adds milk to coffee
    // => Wraps Coffee object, adds milk cost/description
    public MilkDecorator(Coffee coffee) { super(coffee); }
    // => Passes wrapped coffee to parent constructor
    // => Can wrap SimpleCoffee or another decorator

    @Override
    public double cost() { return coffee.cost() + 0.5; }
    // => Adds milk cost ($0.50) to wrapped coffee's cost
    // => Delegates to wrapped coffee.cost(), then adds decoration cost
    // => SimpleCoffee: 2.0 + 0.5 = 2.5

    @Override
    public String description() { return coffee.description() + ", milk"; }
    // => Appends ", milk" to wrapped coffee's description
    // => Delegates to wrapped coffee.description(), then adds decoration
    // => SimpleCoffee: "Simple coffee" + ", milk" = "Simple coffee, milk"
}

class SugarDecorator extends CoffeeDecorator {
    // => Concrete decorator: adds sugar to coffee
    // => Can wrap SimpleCoffee, MilkDecorator, or any Coffee
    public SugarDecorator(Coffee coffee) { super(coffee); }
    // => Receives coffee to wrap (base or decorated)
    // => Enables multiple decorations on same object

    @Override
    public double cost() { return coffee.cost() + 0.2; }
    // => Adds sugar cost ($0.20) to wrapped coffee's cost
    // => MilkDecorator(SimpleCoffee): 2.5 + 0.2 = 2.7

    @Override
    public String description() { return coffee.description() + ", sugar"; }
    // => Appends ", sugar" to wrapped coffee's description
    // => MilkDecorator(SimpleCoffee): "Simple coffee, milk" + ", sugar" = "Simple coffee, milk, sugar"
}

Coffee coffee = new SimpleCoffee();
// => Creates base coffee: cost = 2.0, description = "Simple coffee"
// => Starting point for decoration
coffee = new MilkDecorator(coffee);
// => Wraps SimpleCoffee with MilkDecorator
// => coffee now: cost = 2.5, description = "Simple coffee, milk"
// => Decoration chain: SimpleCoffee -> MilkDecorator
coffee = new SugarDecorator(coffee);
// => Wraps MilkDecorator with SugarDecorator
// => coffee now: cost = 2.7, description = "Simple coffee, milk, sugar"
// => Decoration chain: SimpleCoffee -> MilkDecorator -> SugarDecorator
System.out.println(coffee.description()); // => "Simple coffee, milk, sugar"
// => Calls SugarDecorator.description()
// => SugarDecorator delegates to MilkDecorator.description()
// => MilkDecorator delegates to SimpleCoffee.description()
// => Chain: SimpleCoffee ("Simple coffee") -> MilkDecorator (+ ", milk") -> SugarDecorator (+ ", sugar")
System.out.println(coffee.cost()); // => 2.7
// => Calls SugarDecorator.cost()
// => SugarDecorator delegates to MilkDecorator.cost() + 0.2
// => MilkDecorator delegates to SimpleCoffee.cost() + 0.5
// => Chain: SimpleCoffee (2.0) -> MilkDecorator (2.0 + 0.5 = 2.5) -> SugarDecorator (2.5 + 0.2 = 2.7)
```

**Key Takeaway**: Strategy encapsulates algorithms, enabling runtime selection. Lambdas simplify strategy for simple cases. Observer enables one-to-many notifications, decoupling subjects from observers. Decorator adds behavior dynamically through composition, avoiding subclass explosion.

**Why It Matters**: Design patterns (Strategy, Observer, Decorator) solve recurring problems with proven solutions. Strategy enables algorithm selection at runtime (payment methods, sorting strategies). Observer enables event-driven architectures (UI events, distributed events). Decorator adds behavior dynamically without subclassing (I/O streams, servlet filters). Understanding patterns improves communication—"use observer pattern" conveys more than detailed explanation. However, overuse causes over-engineering—apply patterns when appropriate, not reflexively. Patterns are essential for maintainable systems, providing shared vocabulary and proven structures for common problems.

---

## Example 66: Dependency Injection Basics

Dependency Injection (DI) inverts control, allowing dependencies to be provided externally rather than created internally. Enhances testability, flexibility, and maintainability. Constructor injection preferred for required dependencies.

**Code**:

```java
// Without DI - tight coupling
class OrderServiceBad {
    private EmailService emailService = new EmailService();
                                 // => Hardcoded dependency creation
                                 // => Tight coupling: OrderServiceBad depends on concrete EmailService
                                 // => Cannot swap implementation without modifying class
                                 // => Cannot test with mock (EmailService hardcoded)

    public void placeOrder(Order order) {
        // Process order
        emailService.send("Order placed");
                                 // => Calls hardcoded EmailService
                                 // => Cannot mock for unit testing
                                 // => Cannot change to SMS without code modification
    }
}

// With DI - loose coupling via interface
interface NotificationService {
                                 // => Abstraction: defines contract, not implementation
                                 // => OrderService depends on interface, not concrete class
    void send(String message);   // => Abstract method: implementations provide behavior
}

class EmailService implements NotificationService {
                                 // => Concrete implementation: email notifications
                                 // => Implements NotificationService contract
    @Override
    public void send(String message) {
        System.out.println("Email: " + message);
                                 // => Email-specific implementation
                                 // => Output: "Email: [message]"
    }
}

class SMSService implements NotificationService {
                                 // => Alternative implementation: SMS notifications
                                 // => Same interface, different behavior
    @Override
    public void send(String message) {
        System.out.println("SMS: " + message);
                                 // => SMS-specific implementation
                                 // => Output: "SMS: [message]"
    }
}

// Constructor injection (preferred for required dependencies)
class OrderService {
    private final NotificationService notificationService;
                                 // => Final field: immutable after construction
                                 // => Dependency must be provided via constructor
                                 // => Makes dependencies explicit and required

    public OrderService(NotificationService notificationService) {
                                 // => Constructor receives dependency from outside
                                 // => Inversion of control: caller provides dependency
        this.notificationService = notificationService;
                                 // => Injected dependency stored for use
                                 // => Can be EmailService, SMSService, or any NotificationService
    }

    public void placeOrder(Order order) {
        // Process order
        notificationService.send("Order placed");
                                 // => Uses injected dependency (EmailService or SMSService)
                                 // => Actual implementation determined at construction
                                 // => Polymorphism: interface reference, concrete behavior
    }
}

// Using the service
NotificationService emailService = new EmailService();
                                 // => Creates EmailService instance
OrderService orderService = new OrderService(emailService);
                                 // => Injects EmailService into OrderService
                                 // => OrderService uses email notifications
orderService.placeOrder(new Order());
                                 // => Calls placeOrder, sends email notification
                                 // => Output: "Email: Order placed"

// Easily switch implementation
NotificationService smsService = new SMSService();
                                 // => Creates SMSService instance
OrderService orderService2 = new OrderService(smsService);
                                 // => Injects SMSService into OrderService
                                 // => No code change in OrderService needed
                                 // => Same OrderService code, different behavior

// Setter injection (for optional dependencies)
class ReportService {
    private Logger logger;       // => Optional dependency (not final)
                                 // => Can be null, service works without it

    public void setLogger(Logger logger) {
        this.logger = logger;    // => Optional dependency injected via setter
                                 // => Can be called after construction
                                 // => Allows optional configuration
    }

    public void generateReport() {
        if (logger != null) {    // => Check if logger injected
                                 // => Service degrades gracefully without logger
            logger.log("Report generated");
                                 // => Uses logger if available
        }
    }
}

// Benefits of DI
// 1. Testability - inject mocks/stubs
class MockNotificationService implements NotificationService {
                                 // => Test double: implements same interface
                                 // => Used for unit testing OrderService
    public List<String> sentMessages = new ArrayList<>();
                                 // => Tracks sent messages for assertions
                                 // => Verification: check what messages sent

    @Override
    public void send(String message) {
        sentMessages.add(message);
                                 // => Records message instead of sending
                                 // => Enables verification in tests
    }
}

MockNotificationService mock = new MockNotificationService();
                                 // => Creates mock for testing
OrderService testService = new OrderService(mock);
                                 // => Injects mock into OrderService
                                 // => OrderService doesn't know it's a mock
testService.placeOrder(new Order());
                                 // => Executes order placement with mock
                                 // => Mock records message instead of sending
assert mock.sentMessages.contains("Order placed");
                                 // => Verifies message was sent
                                 // => Unit test: validates behavior without real email

// 2. Flexibility - change behavior without modifying code
                                 // => Swap EmailService for SMSService at runtime
                                 // => Add new NotificationService implementations
// 3. Maintainability - dependencies explicit in constructor
                                 // => Constructor signature documents required dependencies
                                 // => No hidden dependencies (no new inside)

// Manual DI container (simple example)
class DIContainer {
    private Map<Class<?>, Object> services = new HashMap<>();
                                 // => Service registry: maps types to instances
                                 // => Simple service locator pattern

    public <T> void register(Class<T> type, T instance) {
                                 // => Registers service instance for type
                                 // => Generic method: works for any type
        services.put(type, instance);
                                 // => Stores instance keyed by class type
    }

    @SuppressWarnings("unchecked")
    public <T> T resolve(Class<T> type) {
                                 // => Retrieves registered service by type
                                 // => Returns null if not registered
        return (T) services.get(type);
                                 // => Cast required due to type erasure
                                 // => SuppressWarnings: we know cast is safe
    }
}

DIContainer container = new DIContainer();
                                 // => Creates DI container instance
container.register(NotificationService.class, new EmailService());
                                 // => Registers EmailService as NotificationService
                                 // => Container manages service lifecycle
NotificationService service = container.resolve(NotificationService.class);
                                 // => Retrieves NotificationService from container
                                 // => Returns EmailService instance
                                 // => Decouples service creation from usage

// Spring Framework DI (conceptual)
/*
@Component                       // => Spring manages lifecycle
class OrderService {
    private final NotificationService notificationService;

    @Autowired                   // => Spring injects dependency automatically
    public OrderService(NotificationService notificationService) {
        this.notificationService = notificationService;
    }                            // => Spring finds NotificationService bean and injects
}

@Component                       // => Spring creates instance automatically
class EmailService implements NotificationService {
    // Spring creates instance
                                 // => Spring scans classpath, creates beans
                                 // => Wires dependencies based on types
}
*/

// DI principles
// - Depend on abstractions (interfaces), not concretions
                                 // => OrderService depends on NotificationService, not EmailService
                                 // => Enables swapping implementations
// - Constructor injection for required dependencies
                                 // => Makes dependencies explicit and immutable
// - Setter injection for optional dependencies
                                 // => Allows optional configuration
// - Avoid service locator pattern (anti-pattern)
                                 // => Service locator hides dependencies
                                 // => Constructor injection preferred
```

**Key Takeaway**: DI inverts control—dependencies injected, not created internally. Constructor injection for required dependencies, setter for optional. Enhances testability via mock injection. Depend on interfaces for flexibility. DI containers automate wiring in frameworks.

**Why It Matters**: Dependency injection decouples code from concrete dependencies, improving testability and flexibility. Constructor injection (preferred) makes dependencies explicit and enables immutability. Field injection (convenient) hides dependencies but prevents immutability. Method injection enables optional dependencies. Understanding DI enables effective framework usage (Spring, Guice, Dagger). DI inverts control—framework instantiates objects, not application code. This enables swapping implementations (mock for test, real for production), reduces coupling, and improves modularity. DI is foundational to modern Java development, enabling loosely coupled, testable, maintainable systems.

---

## Example 67: Immutability Patterns

Immutable objects cannot be modified after creation, providing inherent thread safety and simplicity. Use final fields, no setters, defensive copying for mutable components. Records automate immutable class creation.

**Code**:

```java
// Immutable class - traditional approach
final class ImmutablePoint {   // => final: class cannot be subclassed
                                 // => Prevents mutable subclasses breaking immutability
    private final int x;         // => final field: cannot be reassigned after construction
    private final int y;         // => Immutable: both fields final

    public ImmutablePoint(int x, int y) {
                                 // => Constructor: only place fields can be set
        this.x = x;              // => Assigns x once, never changes
        this.y = y;              // => Assigns y once, never changes
    }

    public int getX() { return x; }
                                 // => Getter: returns value, cannot modify
    public int getY() { return y; }
                                 // => No setters: state cannot change after construction

    // No setters - state cannot change
                                 // => Immutability guarantee: no mutation methods

    // Operations return new instances
    public ImmutablePoint move(int dx, int dy) {
                                 // => Functional style: returns new instance
        return new ImmutablePoint(x + dx, y + dy);
                                 // => Creates new ImmutablePoint with updated coordinates
                                 // => Original object unchanged (immutable)
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ImmutablePoint other)) return false;
                                 // => Type check with pattern variable
        return x == other.x && y == other.y;
                                 // => Value equality: compares coordinates
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
                                 // => Stable hash: never changes (immutable)
                                 // => Safe as HashMap key
    }
}

ImmutablePoint p1 = new ImmutablePoint(10, 20);
                                 // => Creates point at (10, 20)
ImmutablePoint p2 = p1.move(5, 5);
                                 // => Returns new ImmutablePoint(15, 25)
                                 // => p1 unchanged: (10, 20)
// p1 unchanged: (10, 20)       // => Immutability: original unmodified

// Immutable class with mutable component - defensive copying
final class ImmutablePerson {
    private final String name;   // => String immutable: safe
    private final Date birthDate;// => Date MUTABLE: requires defensive copy
                                 // => Risk: caller could mutate Date after passing

    public ImmutablePerson(String name, Date birthDate) {
        this.name = name;        // => String immutable: direct assignment safe
        this.birthDate = new Date(birthDate.getTime());
                                 // => Defensive copy: creates new Date
                                 // => Prevents caller from mutating our internal state
    }

    public String getName() { return name; }
                                 // => String immutable: safe to return directly

    public Date getBirthDate() {
        return new Date(birthDate.getTime());
                                 // => Defensive copy: returns copy, not original
                                 // => Prevents caller from mutating internal state
    }
}

Date date = new Date();          // => Creates mutable Date
ImmutablePerson person = new ImmutablePerson("Alice", date);
                                 // => Constructor makes defensive copy
date.setTime(0);                 // => Mutates original Date
                                 // => Doesn't affect person.birthDate (defensive copy protected)

// Records - immutable data carriers (Java 14+)
record Point(int x, int y) {
                                 // => Record: compact immutable class syntax
    // Automatically generates:
    // - private final fields
                                 // => x and y are final fields
    // - constructor
                                 // => Point(int x, int y) generated
    // - getters (x(), y())
                                 // => Accessor methods, not setters
    // - equals(), hashCode(), toString()
                                 // => Value-based equality
}

Point p = new Point(10, 20);     // => Creates Point record
System.out.println(p.x());       // => Calls generated accessor
                                 // => Output: 10
// No setters - immutable by default
                                 // => Records cannot have setters

// Compact constructor for validation
record PositivePoint(int x, int y) {
    public PositivePoint {       // => Compact constructor: no parameter list
                                 // => Runs before field assignment
        if (x < 0 || y < 0) {    // => Validation: ensures positive coordinates
            throw new IllegalArgumentException("Coordinates must be positive");
                                 // => Prevents invalid state
        }
    }                            // => Fields assigned after validation succeeds
}

// Immutable collections (Java 9+)
List<String> immutableList = List.of("A", "B", "C");
                                 // => Creates immutable list
// immutableList.add("D");       // => UnsupportedOperationException thrown
                                 // => Modification attempts fail at runtime

Set<Integer> immutableSet = Set.of(1, 2, 3);
                                 // => Immutable set: no add/remove operations
Map<String, Integer> immutableMap = Map.of("one", 1, "two", 2);
                                 // => Immutable map: no put/remove operations

// Copying to immutable (Java 10+)
List<String> mutable = new ArrayList<>(List.of("X", "Y"));
                                 // => Creates mutable ArrayList
List<String> copy = List.copyOf(mutable);
                                 // => Creates immutable copy
                                 // => Changes to mutable don't affect copy

// Benefits of immutability
// 1. Thread safety - no synchronization needed
final class SharedCounter {
    private final int count;     // => Immutable: safe to share across threads

    public SharedCounter(int count) { this.count = count; }
                                 // => Constructor initializes immutable state

    public SharedCounter increment() {
        return new SharedCounter(count + 1);
                                 // => Returns new instance with incremented count
    }

    public int getCount() { return count; }

    // Safe to share across threads without locks
                                 // => Immutability eliminates race conditions
}

// 2. Cacheability - hash code never changes
Map<ImmutablePoint, String> cache = new HashMap<>();
                                 // => HashMap with immutable keys
ImmutablePoint key = new ImmutablePoint(5, 10);
                                 // => Immutable key: hash never changes
cache.put(key, "Value");         // => Safe: key cannot mutate
                                 // => Hash code stability guarantees correct lookup

// 3. Simplicity - no defensive copying needed
public void processPoint(ImmutablePoint point) {
                                 // => Receives immutable object
    // No worry about caller modifying point
                                 // => Caller cannot mutate point
                                 // => No need for defensive copy
}

// 4. Failure atomicity - partially constructed objects impossible
                                 // => All fields set in constructor before object accessible
                                 // => No "half-constructed" state visible

// Persistent data structures (conceptual)
                                 // => Immutable collections share structure for efficiency
// Example: adding to immutable list creates new list sharing most nodes
                                 // => Copy-on-write: only modified parts copied
```

**Key Takeaway**: Immutable objects use final fields, no setters, and defensive copying for mutable components. Records automate immutable class creation. Immutable collections via `List.of()`, `Set.of()`, `Map.of()`. Benefits: thread safety, simplicity, cacheability, failure atomicity.

**Why It Matters**: Immutability prevents bugs from unintended mutations—immutable objects are thread-safe, easier to reason about, and prevent data races. Immutable collections (List.of, Set.of, Map.of) prevent modification bugs. Records provide immutable data carriers. Defensive copying prevents mutations through references. Understanding when immutability helps (shared data, concurrent access, caching) vs when mutability is needed (performance-critical updates, large data structures) determines appropriate design. Immutability reduces cognitive load—no need to track mutations. It's essential for thread-safe code without synchronization and prevents entire categories of bugs.

---

## Example 68: SOLID Principles in Java

SOLID principles guide maintainable object-oriented design. Single Responsibility (one reason to change). Open/Closed (open for extension, closed for modification). Liskov Substitution (subtypes substitutable). Interface Segregation (many specific interfaces). Dependency Inversion (depend on abstractions).

**Code**:

```java
// Single Responsibility Principle (SRP)
// A class should have one reason to change
                                 // => Each class has ONE responsibility
                                 // => Changes to one responsibility don't affect others
                                 // => SRP reduces coupling, improves maintainability

// Bad - multiple responsibilities
class UserServiceBad {
                                 // => Violates SRP: multiple responsibilities in one class
    public void createUser(User user) {
                                 // => Method with mixed concerns (validation, persistence, notification, logging)
        // Validate user         // => Responsibility 1: Validation
        // Save to database      // => Responsibility 2: Persistence
        // Send email            // => Responsibility 3: Notification
        // Log activity          // => Responsibility 4: Logging
        // Multiple reasons to change!
                                 // => Changes to email format require modifying UserService
                                 // => Changes to database require modifying UserService
                                 // => Violates SRP: too many reasons to change
    }
}

// Good - single responsibility per class
class UserValidator {
                                 // => SRP compliant: only validates users
                                 // => One reason to change: validation rules
    public boolean validate(User user) {
        // Only validation logic
                                 // => Single responsibility: validation
        return user.getName() != null && user.getEmail() != null;
                                 // => Returns true if user data valid
                                 // => No persistence, notification, or logging concerns
    }
}

class UserRepository {
                                 // => SRP compliant: only handles database persistence
                                 // => One reason to change: database schema or access patterns
    public void save(User user) {
        // Only database operations
                                 // => Single responsibility: persistence
                                 // => Database changes isolated to this class
                                 // => No validation, notification, or logging
    }
}

class EmailService {
                                 // => SRP compliant: only sends emails
                                 // => One reason to change: email templates or SMTP configuration
    public void sendWelcomeEmail(User user) {
        // Only email sending   // => Single responsibility: email notifications
                                 // => Email format changes isolated to this class
                                 // => No validation, persistence, or logging
    }
}

class UserService {
                                 // => SRP compliant: orchestrates user creation workflow
                                 // => One reason to change: workflow logic
    private final UserValidator validator;
                                 // => Dependency: validator (injected or constructed)
    private final UserRepository repository;
                                 // => Dependency: repository (injected or constructed)
    private final EmailService emailService;
                                 // => Dependency: email service (injected or constructed)
                                 // => Coordinates single-responsibility classes
                                 // => Delegates to specialized classes

    public void createUser(User user) {
                                 // => Orchestrates workflow: validate → persist → notify
        if (validator.validate(user)) {
                                 // => Delegates validation
                                 // => Calls UserValidator.validate()
            repository.save(user);
                                 // => Delegates persistence
                                 // => Calls UserRepository.save()
            emailService.sendWelcomeEmail(user);
                                 // => Delegates notification
                                 // => Calls EmailService.sendWelcomeEmail()
        }                        // => Orchestration: composing single-responsibility classes
                                 // => UserService doesn't implement validation, persistence, or email
    }
}

// Open/Closed Principle (OCP)
// Open for extension, closed for modification
                                 // => Classes open for extension (new features)
                                 // => Closed for modification (existing code unchanged)
                                 // => OCP prevents breaking existing code when adding features

// Bad - modifying class for new behavior
class AreaCalculatorBad {
                                 // => Violates OCP: must modify for each new shape
    public double calculate(Object shape) {
                                 // => Accepts generic Object (no type safety)
        if (shape instanceof Circle) {
                                 // => Type check for each shape type
                                 // => instanceof runtime type checking
            Circle c = (Circle) shape;
                                 // => Explicit cast after instanceof check
            return Math.PI * c.radius * c.radius;
                                 // => Circle area formula: π × r²
        } else if (shape instanceof Rectangle) {
            Rectangle r = (Rectangle) shape;
                                 // => Cast to Rectangle
            return r.width * r.height;
                                 // => Rectangle area formula: width × height
        }
        // Adding new shape requires modifying this method
                                 // => Violates OCP: must modify class to add Triangle
                                 // => Each new shape = modify existing code
                                 // => Adding Triangle requires new else-if branch
        return 0;
                                 // => Default return for unknown types
    }
}

// Good - extend via polymorphism
interface Shape {              // => Abstraction for all shapes
                                 // => OCP compliant: new shapes extend interface
    double area();               // => Contract: all shapes calculate area
                                 // => Each implementation provides own formula
}

class Circle implements Shape {
                                 // => OCP compliant: extends Shape without modifying it
    double radius;
                                 // => Circle-specific field
    public double area() { return Math.PI * radius * radius; }
                                 // => Circle-specific area calculation
                                 // => Implements Shape contract
}

class Rectangle implements Shape {
                                 // => OCP compliant: extends Shape without modifying it
    double width, height;
                                 // => Rectangle-specific fields
    public double area() { return width * height; }
                                 // => Rectangle-specific area calculation
                                 // => Implements Shape contract
}

class Triangle implements Shape {
                                 // => NEW shape added without modifying existing code
                                 // => OCP: extended Shape interface via new implementation
    double base, height;
                                 // => Triangle-specific fields for area calculation
    public double area() { return 0.5 * base * height; }
                                 // => Triangle added WITHOUT modifying existing classes
                                 // => Formula: area = (base × height) / 2
                                 // => Implements Shape contract
}
                                 // => End of Triangle class

class AreaCalculator {
                                 // => OCP compliant: works with any Shape implementation
                                 // => Never needs modification when new shapes added
    public double calculate(Shape shape) {
                                 // => Accepts Shape interface (polymorphic parameter)
        return shape.area();     // => Polymorphism: delegates to shape implementation
                                 // => No modification needed for new shapes
                                 // => Open for extension (add Triangle), closed for modification
                                 // => Same code works for Circle, Rectangle, Triangle, etc.
    }
}

// Liskov Substitution Principle (LSP)
// Subtypes must be substitutable for base types
                                 // => Subclass should be usable wherever parent expected
                                 // => LSP ensures behavioral compatibility in inheritance
                                 // => Subclass must not break parent's contract

// Bad - violates LSP
class Bird {
                                 // => Base class establishes contract: all birds can fly
    public void fly() { System.out.println("Flying"); }
                                 // => Contract: all Birds can fly
                                 // => Output: "Flying"
}

class Penguin extends Bird {
                                 // => Violates LSP: Penguin cannot substitute Bird
    @Override
    public void fly() {
                                 // => Breaks parent contract (throws exception instead of flying)
                                 // => Overrides method but changes behavior fundamentally
        throw new UnsupportedOperationException("Penguins can't fly");
                                 // => Breaks Bird contract: fly() expected to work
                                 // => Runtime exception violates substitutability
        // Violates LSP: cannot substitute Penguin for Bird
                                 // => Code expecting Bird will fail with Penguin
                                 // => Subclass breaks parent's behavioral contract
                                 // => Example: processbird(Bird b) { b.fly(); } fails with Penguin
    }
}

// Good - respects LSP
interface Flyable {              // => Specific capability: flying
    void fly();                  // => Only flyable things implement this
}

class Sparrow implements Flyable {
    public void fly() { System.out.println("Sparrow flying"); }
                                 // => Sparrow fulfills Flyable contract
}

class Penguin2 {                 // => Penguin doesn't claim to be Flyable
    public void swim() { System.out.println("Penguin swimming"); }
                                 // => Penguin has its own capabilities
    // Doesn't implement Flyable - no LSP violation
                                 // => No false promise: penguin never claimed to fly
}

// Interface Segregation Principle (ISP)
// Many specific interfaces better than one general
                                 // => Clients shouldn't depend on methods they don't use
                                 // => Many small interfaces > one fat interface
                                 // => ISP prevents interface pollution

// Bad - fat interface
interface WorkerBad {            // => Single interface with all capabilities
                                 // => Violates ISP: forces implementers to support all methods
    void work();                 // => Method 1
    void eat();                  // => Method 2 (not all workers eat)
                                 // => Robots don't eat (irrelevant method)
    void sleep();                // => Method 3 (not all workers sleep)
                                 // => Robots don't sleep (irrelevant method)
}

class RobotBad implements WorkerBad {
                                 // => Forced to implement all WorkerBad methods
                                 // => Violates ISP: must implement irrelevant methods
    public void work() { /* work */ }
                                 // => Robot can work
                                 // => Only relevant method implementation
    public void eat() { /* robots don't eat - forced to implement */ }
                                 // => Forced to implement irrelevant method
                                 // => Empty or exception-throwing implementation
    public void sleep() { /* robots don't sleep - forced to implement */ }
                                 // => Violates ISP: depends on methods it doesn't use
                                 // => Interface too broad for this implementer
}

// Good - segregated interfaces
interface Workable {             // => Small, focused interface: working
                                 // => ISP compliant: single capability
    void work();                 // => Single capability
                                 // => Implementers only need to support work()
}

interface Eatable {              // => Small, focused interface: eating
                                 // => ISP compliant: single capability
    void eat();                  // => Single capability
                                 // => Only used by entities that eat
}

interface Sleepable {            // => Small, focused interface: sleeping
                                 // => ISP compliant: single capability
    void sleep();                // => Single capability
                                 // => Only used by entities that sleep
}
                                 // => End of Sleepable interface

class Human implements Workable, Eatable, Sleepable {
                                 // => Human implements all three capabilities
                                 // => ISP compliant: multiple interface composition
    public void work() { /* work */ }
                                 // => Human can work
                                 // => Implements Workable interface
    public void eat() { /* eat */ }
                                 // => Human can eat
                                 // => Implements Eatable interface
    public void sleep() { /* sleep */ }
                                 // => Human can sleep
                                 // => Implements Sleepable interface
}
                                 // => End of Human class (implements 3 focused interfaces)

class Robot implements Workable {
                                 // => Robot implements ONLY what it needs
                                 // => ISP compliant: no irrelevant methods
    public void work() { /* work */ }
                                 // => Robot can work
                                 // => Only method implementation required
    // Only implements what it needs
                                 // => No forced implementation of irrelevant methods
                                 // => Respects ISP: depends only on Workable
                                 // => Compare to RobotBad: no eat() or sleep() pollution
}

// Dependency Inversion Principle (DIP)
// Depend on abstractions, not concretions
                                 // => High-level modules shouldn't depend on low-level modules
                                 // => Both should depend on abstractions
                                 // => DIP enables loose coupling and testability

// Bad - depends on concrete class
class OrderProcessorBad {
                                 // => Violates DIP: depends on concrete implementation
    private MySQLDatabase database = new MySQLDatabase();
                                 // => Hardcoded dependency on concrete MySQLDatabase
                                 // => Tightly coupled: cannot switch to PostgreSQL
                                 // => Hard to test: cannot mock database

    public void process(Order order) {
                                 // => Uses concrete MySQLDatabase reference
        database.save(order);    // => Tightly coupled to MySQL
                                 // => Cannot use different database without code modification
                                 // => Cannot inject test double
    }
}

// Good - depends on abstraction
interface Database {             // => Abstraction: defines contract
                                 // => DIP: both high and low-level depend on this
    void save(Order order);      // => Contract: save method
                                 // => Interface defines behavior, not implementation
}

class MySQLDatabase implements Database {
                                 // => Concrete implementation: MySQL
                                 // => DIP compliant: implements abstraction
    public void save(Order order) { /* MySQL implementation */ }
                                 // => MySQL-specific persistence logic
                                 // => Swappable with any Database implementation
}

class PostgreSQLDatabase implements Database {
                                 // => Alternative implementation: PostgreSQL
                                 // => DIP compliant: implements same abstraction
    public void save(Order order) { /* PostgreSQL implementation */ }
                                 // => PostgreSQL-specific persistence logic
                                 // => PostgreSQL-specific persistence logic
}

class OrderProcessor {
                                 // => DIP compliant: depends on Database abstraction
                                 // => High-level module (business logic)
    private final Database database;
                                 // => Depends on abstraction (Database interface)
                                 // => Not coupled to any specific implementation
                                 // => Enables dependency injection

    public OrderProcessor(Database database) {
                                 // => Constructor injection (Dependency Injection pattern)
        this.database = database;// => Depends on abstraction
                                 // => Injected: caller provides concrete implementation
                                 // => Can inject MySQLDatabase, PostgreSQLDatabase, or mock
    }

    public void process(Order order) {
                                 // => Business logic (high-level)
        database.save(order);    // => Works with any Database implementation
                                 // => Polymorphism: MySQL, PostgreSQL, or any Database
                                 // => Testable: can inject mock Database
                                 // => Respects DIP: depends on abstraction, not concretion
    }
}
```

**Key Takeaway**: SOLID principles enable maintainable design. SRP: one reason to change per class. OCP: extend via polymorphism, not modification. LSP: subtypes substitutable without breaking behavior. ISP: many specific interfaces over one fat interface. DIP: depend on abstractions via interfaces.

**Why It Matters**: SOLID principles guide object-oriented design toward maintainable systems. Single Responsibility (one reason to change) prevents god classes. Open/Closed (open for extension, closed for modification) enables adding features without changing existing code. Liskov Substitution (subclasses replaceable for parents) prevents inheritance misuse. Interface Segregation (many small interfaces vs few large) prevents forcing clients to depend on unused methods. Dependency Inversion (depend on abstractions) enables loose coupling. Understanding SOLID enables writing code that's easier to extend, test, and maintain. However, over-application causes over-engineering—apply principles judiciously based on concrete needs.

---

## Example 69: Custom ClassLoaders

ClassLoaders dynamically load classes into the JVM. The delegation model ensures core classes load first. Custom loaders enable plugins, hot-reloading, and bytecode manipulation. Each loader creates an isolation boundary.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Custom["Custom ClassLoader<br/>(app-specific)"] --> System["System ClassLoader<br/>(classpath)"]
    System --> Platform["Platform ClassLoader<br/>(Java SE modules)"]
    Platform --> Bootstrap["Bootstrap ClassLoader<br/>(core Java classes)"]

    Request["Load MyClass"] --> Custom
    Custom -->|Delegate| System
    System -->|Delegate| Platform
    Platform -->|Delegate| Bootstrap
    Bootstrap -->|Not found| Platform
    Platform -->|Not found| System
    System -->|Not found| Custom
    Custom -->|Load| MyClass["MyClass.class"]

    style Custom fill:#0173B2,color:#fff
    style System fill:#DE8F05,color:#fff
    style Platform fill:#029E73,color:#fff
    style Bootstrap fill:#CC78BC,color:#fff
    style Request fill:#CA9161,color:#fff
    style MyClass fill:#0173B2,color:#fff
```

**Code**:

```java
import java.io.*;
import java.nio.file.*;

// ClassLoader hierarchy
ClassLoader systemClassLoader = ClassLoader.getSystemClassLoader();
                                 // => System/Application ClassLoader (loads from classpath)
                                 // => Loads application classes (-cp, CLASSPATH)
System.out.println("System: " + systemClassLoader);
// => sun.misc.Launcher$AppClassLoader
                                 // => Output: sun.misc.Launcher$AppClassLoader@... (Java 8)
                                 // => Output: jdk.internal.loader.ClassLoaders$AppClassLoader@... (Java 9+)

ClassLoader platformClassLoader = systemClassLoader.getParent();
                                 // => Platform ClassLoader (parent of System)
                                 // => Loads Java SE platform modules (Java 9+)
System.out.println("Platform: " + platformClassLoader);
// => sun.misc.Launcher$ExtClassLoader (Java 8) or PlatformClassLoader (Java 9+)
                                 // => Java 8: ExtClassLoader (loads from jre/lib/ext)
                                 // => Java 9+: PlatformClassLoader (loads platform modules)

ClassLoader bootstrapClassLoader = platformClassLoader.getParent();
                                 // => Bootstrap ClassLoader (top of hierarchy)
                                 // => Loads core Java classes (java.lang, java.util, etc.)
System.out.println("Bootstrap: " + bootstrapClassLoader);
// => null (bootstrap loader is native)
                                 // => Output: null (bootstrap implemented in native code, not Java)
                                 // => Loads rt.jar (Java 8) or java.base module (Java 9+)

// Delegation model: child delegates to parent before loading
// Bootstrap -> Platform -> System -> Custom
                                 // => Parent-first delegation: ask parent before loading yourself
                                 // => Prevents class conflicts (core classes always from Bootstrap)
                                 // => Custom → System → Platform → Bootstrap (delegation chain)

// Custom ClassLoader
class CustomClassLoader extends ClassLoader {
                                 // => Extends ClassLoader for custom class loading behavior
                                 // => Override findClass() to define custom loading logic
    private String classPath;    // => Custom classpath (directory containing .class files)
                                 // => Example: "/custom/classes/"

    public CustomClassLoader(String classPath) {
                                 // => Constructor: sets custom classpath
        this.classPath = classPath;
                                 // => Stores path for later use in findClass()
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
                                 // => Called when class not found in parent loaders
                                 // => name: fully qualified class name (e.g., "com.example.MyClass")
        try {
            // Convert class name to file path
            String fileName = name.replace('.', '/') + ".class";
                                 // => Converts com.example.MyClass → com/example/MyClass.class
                                 // => File path relative to classPath
            Path path = Paths.get(classPath, fileName);
                                 // => Builds absolute path: classPath + fileName
                                 // => Example: "/custom/classes/com/example/MyClass.class"
            byte[] classBytes = Files.readAllBytes(path);
                                 // => Reads .class file bytes from disk
                                 // => classBytes: bytecode for class definition

            // Define class from bytes
            return defineClass(name, classBytes, 0, classBytes.length);
                                 // => Converts bytecode to Class<?> object
                                 // => defineClass(): JVM method to create class from bytes
                                 // => Returns Class<?> ready for instantiation
        } catch (IOException e) {
                                 // => Catches file read errors
            throw new ClassNotFoundException("Could not load class: " + name, e);
                                 // => Wraps IOException as ClassNotFoundException
        }
    }
}

// Using custom ClassLoader
CustomClassLoader loader = new CustomClassLoader("/path/to/classes");
                                 // => Creates custom loader with specific classpath
                                 // => loader will search /path/to/classes for .class files
Class<?> clazz = loader.loadClass("com.example.MyClass");
                                 // => Loads class using custom loader
                                 // => Delegates to parent first, then findClass() if not found
                                 // => clazz is Class<?> for com.example.MyClass
Object instance = clazz.getDeclaredConstructor().newInstance();
                                 // => Creates instance of loaded class
                                 // => getDeclaredConstructor() gets no-arg constructor
                                 // => newInstance() invokes constructor, returns Object

// Class identity determined by (class name + ClassLoader)
// Same class loaded by different loaders are different classes!
                                 // => Class identity: (fully qualified name, ClassLoader instance)
                                 // => Important: same source, different loaders → different classes
ClassLoader loader1 = new CustomClassLoader("/path1");
                                 // => First custom loader instance
Class<?> class1 = loader1.loadClass("MyClass");
                                 // => Loads MyClass using loader1
ClassLoader loader2 = new CustomClassLoader("/path2");
                                 // => Second custom loader instance (different object)
Class<?> class2 = loader2.loadClass("MyClass");
                                 // => Loads MyClass using loader2
// class1 != class2 (different loaders)
                                 // => class1 and class2 are DIFFERENT classes
                                 // => Same name, different loaders → incompatible types
                                 // => class1.isInstance(class2.newInstance()) → false!

// Hot reload use case
class HotReloadClassLoader extends ClassLoader {
                                 // => Enables reloading classes without JVM restart
    private String classPath;    // => Path to .class files

    public HotReloadClassLoader(String classPath) {
                                 // => Constructor: stores classpath
        this.classPath = classPath;
                                 // => Saved for creating fresh loaders
    }

    public Class<?> reload(String name) throws Exception {
                                 // => Reloads class with fresh loader
        // Create new loader for each reload (isolates class versions)
                                 // => Key: new loader creates new class identity
                                 // => Old class and new class are different (different loaders)
        CustomClassLoader freshLoader = new CustomClassLoader(classPath);
                                 // => Fresh loader instance (not cached)
        return freshLoader.loadClass(name);
                                 // => Loads class with fresh loader
                                 // => Returns NEW Class<?> instance (old version unaffected)
    }
}

// Plugin system use case
class PluginLoader extends ClassLoader {
    public void loadPlugin(String jarPath) throws Exception {
        // Load classes from JAR file
        // Each plugin gets isolated ClassLoader
    }
}

// ClassLoader methods
Class<?> loadedClass = String.class;
ClassLoader loader3 = loadedClass.getClassLoader();
// => null (bootstrap loader loaded String)

URL resource = loader3.getResource("config.properties");
// Find resource in classpath

InputStream stream = loader3.getResourceAsStream("data.txt");
// Load resource as stream

// Context ClassLoader (for frameworks)
Thread thread = Thread.currentThread();
ClassLoader contextLoader = thread.getContextClassLoader();
thread.setContextClassLoader(new CustomClassLoader("/custom/path"));

// OSGi and module systems use ClassLoader isolation
// Each bundle/module has own ClassLoader for dependency isolation
```

**Key Takeaway**: ClassLoaders dynamically load classes with delegation model (child → parent → bootstrap). Custom loaders enable plugins, hot-reloading, and isolation. Class identity = class name + ClassLoader. `findClass()` for custom loading, `defineClass()` converts bytes to classes.

**Why It Matters**: Custom ClassLoaders enable dynamic loading, isolation (plugins, web apps), and hot-reloading (development tools). Understanding delegation model (parent-first by default) prevents classpath conflicts. Custom loaders enable loading classes from non-standard sources (network, databases, encrypted jars). They power plugin systems (IDE plugins, app containers), application servers (servlet isolation), and dynamic languages (Groovy, Scala). However, classloading complexity causes subtle bugs—class identity includes loader, preventing casting across loaders. Most applications use default loaders—custom loaders are for frameworks and specialized systems requiring dynamic behavior.

---

## Example 70: Bytecode Manipulation with ASM/ByteBuddy

Bytecode manipulation enables runtime code generation, proxying, and instrumentation. ASM provides low-level bytecode control. ByteBuddy offers high-level API. Used in AOP frameworks, mocking libraries, and profilers.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Java["Java Source<br/>.java"] --> Compiler["javac<br/>Compiler"]
    Compiler --> Bytecode["Bytecode<br/>.class"]
    Bytecode --> JVM["JVM<br/>Execution"]

    ASM["ASM Library<br/>(low-level)"] -->|Generate| Bytecode
    ByteBuddy["ByteBuddy<br/>(high-level)"] -->|Generate| Bytecode

    Bytecode --> Agent["Java Agent<br/>(Instrumentation)"]
    Agent -->|Modify at<br/>load time| JVM

    style Java fill:#0173B2,color:#fff
    style Compiler fill:#DE8F05,color:#fff
    style Bytecode fill:#029E73,color:#fff
    style JVM fill:#CC78BC,color:#fff
    style ASM fill:#CA9161,color:#fff
    style ByteBuddy fill:#CA9161,color:#fff
    style Agent fill:#0173B2,color:#fff
```

**Bytecode Basics**:

Java source code compiles to `.class` files containing bytecode instructions. The JVM executes this bytecode. Class files have a fixed structure: magic number (0xCAFEBABE), version, constant pool, fields, methods, and attributes.

**Approach 1: ASM Library (Low-Level)**

ASM provides direct bytecode manipulation with fine-grained control. Requires manually constructing bytecode instructions. Used when you need precise control over generated bytecode.

```java
import org.objectweb.asm.*;      // => ASM library for bytecode manipulation

ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
                                 // => Creates bytecode writer with automatic frame computation
                                 // => COMPUTE_FRAMES: ASM calculates stack map frames automatically
cw.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, "GeneratedClass", null, "java/lang/Object", null);
                                 // => Defines class structure: Java 17, public, extends Object
                                 // => Parameters: version, access flags, name, signature, superclass, interfaces

MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "hello", "()Ljava/lang/String;", null, null);
                                 // => Creates public String hello() method visitor
                                 // => Method signature: ()Ljava/lang/String; = no params, returns String
mv.visitCode();                  // => Starts method body bytecode generation
mv.visitLdcInsn("Hello");        // => Load constant "Hello" onto operand stack
                                 // => LDC instruction: load constant from constant pool
mv.visitInsn(Opcodes.ARETURN);   // => Return string reference from top of stack
                                 // => ARETURN: return reference type (vs IRETURN for int)
mv.visitMaxs(1, 1);              // => Sets max stack size=1, max locals=1
                                 // => Required for bytecode verification
mv.visitEnd();                   // => Finishes method bytecode generation

byte[] bytecode = cw.toByteArray();
                                 // => Generates .class file bytes (bytecode array)
                                 // => Ready to be loaded by ClassLoader

CustomClassLoader loader = new CustomClassLoader();
                                 // => Creates custom class loader instance
Class<?> clazz = loader.defineClass("GeneratedClass", bytecode);
                                 // => Loads generated bytecode into JVM as Class object
                                 // => defineClass converts byte[] to Class<?> instance
Object instance = clazz.getDeclaredConstructor().newInstance();
                                 // => Creates instance of dynamically generated class
                                 // => getDeclaredConstructor() gets no-arg constructor
Method method = clazz.getMethod("hello");
                                 // => Retrieves hello() method via reflection
String result = (String) method.invoke(instance);
                                 // => Invokes hello() on instance reflectively
                                 // => Returns "Hello" (from LDC instruction)
                                 // => Result is "Hello" (String)
```

**ASM Trade-offs**: Powerful and fast, but verbose (requires understanding JVM opcodes). Manual stack management. No type safety. Used by frameworks needing maximum performance and control.

**Approach 2: ByteBuddy (High-Level)**

ByteBuddy offers fluent API for common bytecode tasks. Abstracts away low-level details. Preferred for most use cases.

```java
import net.bytebuddy.ByteBuddy;
import net.bytebuddy.implementation.FixedValue;
import net.bytebuddy.matcher.ElementMatchers;

Class<?> dynamicType = new ByteBuddy()
    .subclass(Object.class)
    // => Extends Object
    .method(ElementMatchers.named("toString"))
    // => Find toString() method
    .intercept(FixedValue.value("Hello from ByteBuddy"))
    // => Override to return fixed value
    .make()
    .load(getClass().getClassLoader())
    .getLoaded();
    // => Generates and loads class (5 lines vs ASM's 15)

Object instance = dynamicType.getDeclaredConstructor().newInstance();
System.out.println(instance.toString());
// => Output: "Hello from ByteBuddy"
```

**ByteBuddy Trade-offs**: Easy to use, type-safe, readable. Slightly slower than ASM. Generates more bytecode overhead. Used by Mockito, Hibernate, Spring.

**Approach 3: Java's Built-In Proxy**

Java provides `java.lang.reflect.Proxy` for interface proxying without external libraries. Limited to interfaces only.

```java
interface HelloService {
    String sayHello(String name);
}

HelloService proxy = (HelloService) java.lang.reflect.Proxy.newProxyInstance(
    HelloService.class.getClassLoader(),
    new Class<?>[] { HelloService.class },
    (proxyObj, method, args) -> {
        // => InvocationHandler: intercepts all method calls
        System.out.println("Before method: " + method.getName());
        // => Output: "Before method: sayHello"
        String result = "Hello, " + args[0];
        System.out.println("After method");
        // => Output: "After method"
        return result;
    }
);

proxy.sayHello("Alice");
// => Returns: "Hello, Alice"
```

**Java Proxy Trade-offs**: No external dependencies. Simple API. **Limitation**: Only works with interfaces. Cannot proxy classes. For class proxying, use ByteBuddy or CGLib.

**Common Use Cases**:

- **AOP**: Spring AOP (method interception, transactions)
- **Mocking**: Mockito (test doubles)
- **ORM**: Hibernate (lazy loading proxies)
- **Profiling**: JProfiler, YourKit (method timing)
- **Code Generation**: Lombok (compile-time), MapStruct

**Key Takeaway**: Bytecode manipulation enables runtime code generation and instrumentation. ASM provides low-level control, ByteBuddy offers high-level API. Used in AOP, mocking, ORM, profiling. Java's `Proxy` for interfaces, ByteBuddy for classes. Agents instrument classes at load time.

**Why It Matters**: Bytecode manipulation enables runtime code generation, instrumentation (profilers, debuggers, APM tools), and aspect-oriented programming (logging, security, transactions). ASM provides low-level bytecode access, ByteBuddy offers high-level API. Understanding bytecode structure enables powerful tools—mocking frameworks (Mockito), code coverage (JaCoCo), APM agents (New Relic, Datadog). However, bytecode manipulation is fragile—breaks with Java version changes, obscures code behavior, and complicates debugging. Use it for tools and frameworks, not application code. It's essential for framework developers but too complex for most application developers.

---

## Example 71: JNI and Native Code

Java Native Interface (JNI) bridges Java and native code (C/C++). Useful for legacy system integration, hardware access, and performance-critical operations. Adds complexity and platform dependency.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    JavaApp["Java Application<br/>(managed code)"] --> JNI["JNI Boundary"]
    JNI --> NativeLib["Native Library<br/>(.so/.dll/.dylib)"]
    NativeLib --> Hardware["Hardware/<br/>OS Resources"]

    JavaApp -->|Declare<br/>native methods| JNI
    JNI -->|Type conversion<br/>Java ↔ C| NativeLib
    NativeLib -->|Direct access| Hardware

    style JavaApp fill:#0173B2,color:#fff
    style JNI fill:#DE8F05,color:#fff
    style NativeLib fill:#029E73,color:#fff
    style Hardware fill:#CC78BC,color:#fff
```

**Code**:

```java
// JNI workflow:
// 1. Declare native method in Java
                                 // => Mark method with 'native' keyword (no implementation)
// 2. Compile Java class
                                 // => javac NativeExample.java produces .class file
// 3. Generate C header with javah (Java 8) or javac -h (Java 9+)
                                 // => Creates NativeExample.h with C function signatures
// 4. Implement native method in C/C++
                                 // => Write C code implementing JNI functions
// 5. Compile native code to shared library (.so, .dll, .dylib)
                                 // => gcc/clang produces platform-specific shared library
// 6. Load library and call native method
                                 // => System.loadLibrary() loads library, Java calls C

// Step 1: Declare native method
class NativeExample {        // => Java class with native methods
                                 // => Bridges Java application and C/C++ native code
    // Native method declaration
    public native int add(int a, int b);
                                 // => native: method implemented in C/C++, not Java
                                 // => No method body in Java (body in C)
                                 // => Returns int, takes two int parameters
    public native String getMessage();
                                 // => native method returning String (JNI converts to jstring)
                                 // => Implemented in C, returns Java String object

    // Load native library
    static {                     // => Static initializer block (runs once when class loaded)
                                 // => Loads native library before any methods called
        System.loadLibrary("native_example"); // Loads libnative_example.so/.dll/.dylib
                                 // => Searches for library: libnative_example.so (Linux), native_example.dll (Windows), libnative_example.dylib (macOS)
                                 // => Looks in java.library.path directories
                                 // => UnsatisfiedLinkError if library not found
    }

    public static void main(String[] args) {
                                 // => Main entry point for testing native methods
        NativeExample example = new NativeExample();
                                 // => Creates instance (triggers static block, loads library)
        int result = example.add(5, 3); // => 8 (computed in C)
                                 // => Calls native C function with args 5, 3
                                 // => C function computes 5 + 3 = 8, returns to Java
                                 // => JNI boundary crossing: Java → C → Java
        System.out.println("Result: " + result);
                                 // => Output: "Result: 8"

        String message = example.getMessage(); // => "Hello from C"
                                 // => Calls native C function (no args)
                                 // => C function creates jstring "Hello from C"
                                 // => JNI converts jstring to Java String
        System.out.println(message);
                                 // => Output: "Hello from C"
    }
}

// Step 2 & 3: Generate header
// javac -h . NativeExample.java
                                 // => Compiles Java and generates C header in current directory
                                 // => -h flag: generate JNI header files
// Creates: NativeExample.h
                                 // => Header contains C function prototypes for native methods
                                 // => Function naming: Java_ClassName_methodName
                                 // => Example: Java_NativeExample_add

/*
Step 4: Implement in C (NativeExample.c)

#include <jni.h>
                                 // => JNI header with types: jint, jstring, JNIEnv, jobject
                                 // => Defines JNIEXPORT, JNICALL macros
#include "NativeExample.h"
                                 // => Generated header with function prototypes

JNIEXPORT jint JNICALL Java_NativeExample_add(JNIEnv *env, jobject obj, jint a, jint b) {
                                 // => JNIEXPORT: export symbol for dynamic linking
                                 // => JNICALL: calling convention (platform-specific)
                                 // => JNIEnv *env: JNI environment pointer (for JNI functions)
                                 // => jobject obj: 'this' reference (Java object calling method)
                                 // => jint a, jint b: Java int parameters (32-bit signed)
    return a + b;
                                 // => Simple C addition: returns sum
                                 // => jint is C typedef for int32_t
                                 // => Return value crosses JNI boundary back to Java
}

JNIEXPORT jstring JNICALL Java_NativeExample_getMessage(JNIEnv *env, jobject obj) {
                                 // => Native implementation of getMessage()
                                 // => Returns jstring (JNI type for Java String)
    return (*env)->NewStringUTF(env, "Hello from C");
                                 // => NewStringUTF: JNI function creates Java String from UTF-8 C string
                                 // => (*env)->: dereference JNIEnv pointer, access function table
                                 // => Creates new Java String object on heap (managed by GC)
                                 // => Returns jstring reference to Java
}
*/

// Step 5: Compile native code
// gcc -shared -fPIC -I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux -o libnative_example.so NativeExample.c
                                 // => gcc: GNU C compiler
                                 // => -shared: create shared library (not executable)
                                 // => -fPIC: Position Independent Code (required for shared libraries)
                                 // => -I: include directories for jni.h and platform-specific headers
                                 // => -o: output file name (libnative_example.so on Linux)
                                 // => Platform-specific: .so (Linux), .dll (Windows), .dylib (macOS)

// JNI type mappings
// Java      -> C
                                 // => JNI defines C types corresponding to Java primitives/objects
                                 // => Ensures type safety across JNI boundary
// boolean   -> jboolean
                                 // => Java boolean (true/false) → C jboolean (unsigned 8-bit)
// byte      -> jbyte
                                 // => Java byte (8-bit signed) → C jbyte (int8_t)
// char      -> jchar
                                 // => Java char (16-bit Unicode) → C jchar (uint16_t)
// short     -> jshort
                                 // => Java short (16-bit signed) → C jshort (int16_t)
// int       -> jint
                                 // => Java int (32-bit signed) → C jint (int32_t)
// long      -> jlong
                                 // => Java long (64-bit signed) → C jlong (int64_t)
// float     -> jfloat
                                 // => Java float (32-bit IEEE 754) → C jfloat (float)
// double    -> jdouble
                                 // => Java double (64-bit IEEE 754) → C jdouble (double)
// Object    -> jobject
                                 // => Java Object reference → C jobject (opaque pointer, managed by JVM)
// String    -> jstring
                                 // => Java String → C jstring (subtype of jobject)
// array     -> jarray
                                 // => Java arrays → C jarray (subtypes: jintArray, jobjectArray, etc.)

// Calling Java methods from C
/*
JNIEXPORT void JNICALL Java_Example_callbackDemo(JNIEnv *env, jobject obj) {
    // Get class
    jclass cls = (*env)->GetObjectClass(env, obj);

    // Get method ID
    jmethodID mid = (*env)->GetMethodID(env, cls, "javaMethod", "()V");

    // Call method
    (*env)->CallVoidMethod(env, obj, mid);
}
*/

// Accessing Java fields from C
/*
JNIEXPORT jint JNICALL Java_Example_getField(JNIEnv *env, jobject obj) {
    jclass cls = (*env)->GetObjectClass(env, obj);
    jfieldID fid = (*env)->GetFieldID(env, cls, "fieldName", "I");
    return (*env)->GetIntField(env, obj, fid);
}
*/

// JNI exceptions
/*
if (error) {
    jclass exClass = (*env)->FindClass(env, "java/lang/Exception");
    (*env)->ThrowNew(env, exClass, "Error message");
    return;
}
*/

// Use cases for JNI
// 1. Legacy system integration (existing C/C++ code)
                                 // => Reuse existing C/C++ libraries without rewriting in Java
                                 // => Example: integrating 20-year-old C banking system
// 2. Hardware access (device drivers, system calls)
                                 // => Direct hardware control not available in Java
                                 // => Example: USB device drivers, GPIO pins (Raspberry Pi)
// 3. Performance-critical operations (though JIT often matches C)
                                 // => CPU-bound algorithms where C outperforms Java
                                 // => Note: Modern JIT (HotSpot) often matches C performance
// 4. Platform-specific features (Windows API, POSIX)
                                 // => OS-specific APIs not in Java standard library
                                 // => Example: Windows registry access, Linux epoll

// Alternatives to JNI
// 1. Java Native Access (JNA) - easier than JNI, no C code needed
                                 // => JNA: call native libraries without writing C glue code
                                 // => Slower than JNI but much easier to use
                                 // => Example: JNA.invoke("libc.so", "strlen", string)
// 2. Panama Foreign Function API (Java 19+) - modern replacement
                                 // => Project Panama: modern, safe, fast foreign function interface
                                 // => Replaces JNI with safer, easier API
                                 // => Stable in Java 21+
// 3. Pure Java implementations - often sufficient with modern JIT
                                 // => Java performance improved significantly (JIT optimization)
                                 // => Native code less necessary than 20 years ago

// Performance considerations
// - JNI calls have overhead (crossing JVM boundary)
                                 // => JNI call overhead: ~10-50ns per call (context switch, parameter marshalling)
                                 // => Significant for trivial operations (addition: JNI slower than Java)
// - Use for computationally intensive operations, not trivial calls
                                 // => Only use JNI when native work >> JNI overhead
                                 // => Example: image processing (millions of pixels), not single arithmetic
// - Batch operations to minimize crossings
                                 // => Pass arrays instead of individual values (one crossing vs thousands)
                                 // => Example: processArray(int[] data) better than loop calling processInt(int)

// Memory management
// - Java GC doesn't manage native memory
                                 // => Native allocations (malloc, new) invisible to GC
                                 // => Native memory leaks if not freed manually
// - Must manually free native allocations
                                 // => C code must call free() for every malloc()
                                 // => JNI: use DeleteLocalRef to free JNI references
// - Use try-finally or try-with-resources pattern
                                 // => Ensures native resources freed even if exception thrown
                                 // => Example: acquire native resource in try, free in finally

// Example: Panama Foreign Function API (Java 19+, preview)
/*
import java.lang.foreign.*;

Linker linker = Linker.nativeLinker();
SymbolLookup stdlib = linker.defaultLookup();

FunctionDescriptor descriptor = FunctionDescriptor.of(
    ValueLayout.JAVA_INT,
    ValueLayout.JAVA_INT,
    ValueLayout.JAVA_INT
);

MethodHandle strlen = linker.downcallHandle(
    stdlib.find("strlen").orElseThrow(),
    FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.ADDRESS)
);
*/
```

**Key Takeaway**: JNI bridges Java and native C/C++ code. Declare `native` methods, generate headers, implement in C, compile to shared library, load with `System.loadLibrary()`. Used for legacy integration, hardware access, platform-specific features. Adds complexity and platform dependency. Modern alternatives: JNA, Panama Foreign API.

**Why It Matters**: Atomic variables enable lock-free concurrency, providing thread-safe operations (increment, compareAndSet) without locking overhead. They power high-performance concurrent systems—counters, flags, lock-free data structures. Understanding compare-and-swap (CAS) enables optimistic concurrency—retry instead of blocking. Atomics outperform locks for simple updates but don't replace synchronization for complex multi-variable invariants. They're essential for scalable concurrent programming—enabling millions of operations per second without contention. Use them for hotspots where synchronized causes bottlenecks.

---

## Example 72: MicroProfile and Cloud-Native Java

MicroProfile standardizes enterprise Java microservices. Specifications for REST, configuration, health checks, metrics, fault tolerance, and JWT authentication. Enables cloud-native Java with containers and Kubernetes.

**Code**:

```java
// MicroProfile specifications overview
// - JAX-RS: RESTful web services
                                 // => Standard Java REST API (part of Jakarta EE)
// - Config: Externalized configuration
                                 // => Environment variables, properties, cloud config
// - Health: Health check endpoints
                                 // => Kubernetes liveness/readiness probes
// - Metrics: Application metrics
                                 // => Prometheus-compatible metrics (counters, gauges, timers)
// - Fault Tolerance: Circuit breaker, retry, bulkhead, timeout
                                 // => Resilience patterns for microservices
// - JWT Auth: JSON Web Token authentication
                                 // => Stateless authentication for microservices
// - OpenAPI: API documentation
                                 // => Auto-generated API docs (Swagger/OpenAPI spec)
// - Rest Client: Type-safe REST clients
                                 // => Interface-based REST clients (like Feign)

// JAX-RS - REST endpoints
import javax.ws.rs.*;        // => JAX-RS annotations: @Path, @GET, @POST, @PUT, @DELETE
                                 // => Core REST endpoint annotations
import javax.ws.rs.core.*;   // => Core classes: Response, MediaType
                                 // => Response builder and content type constants

@Path("/users")              // => Base path for all methods in this class
                                 // => Maps to /users endpoint
                                 // => Class-level annotation applies to all methods
@Produces(MediaType.APPLICATION_JSON)
                                 // => All responses return JSON
                                 // => Default content type for all endpoints
@Consumes(MediaType.APPLICATION_JSON)
                                 // => All requests accept JSON
                                 // => Expects JSON in request body for POST/PUT
class UserResource {         // => JAX-RS resource class (REST controller)
                                 // => Container discovers and registers this class
    @GET                     // => HTTP GET method (retrieve resources)
                                 // => Responds to GET /users (no path parameter)
    public Response getAllUsers() {
                                 // => GET /users - returns all users
                                 // => List all resources operation
        List<User> users = userService.findAll();
                                 // => Fetches users from service layer
                                 // => Service layer handles business logic
        return Response.ok(users).build();
                                 // => Returns HTTP 200 OK with users JSON
                                 // => Response.ok() creates 200 status, build() finalizes
    }

    @GET                     // => HTTP GET method
    @Path("/{id}")           // => Path parameter: /users/123
                                 // => {id} is placeholder for user ID
    public Response getUser(@PathParam("id") Long id) {
                                 // => @PathParam extracts {id} from URL
                                 // => Converts to Long automatically
        User user = userService.find(id);
                                 // => Fetches user by ID
        return Response.ok(user).build();
                                 // => Returns HTTP 200 OK with user JSON
    }

    @POST                    // => HTTP POST method (create resource)
                                 // => Responds to POST /users with JSON body
    public Response createUser(User user) {
                                 // => POST /users - creates new user
                                 // => User deserialized from JSON request body
                                 // => JAX-RS automatically converts JSON to User object
        userService.create(user);
                                 // => Persists user to database
                                 // => Delegates to service layer for creation logic
        return Response.status(Response.Status.CREATED).entity(user).build();
                                 // => Returns HTTP 201 Created with created user
                                 // => 201 indicates successful resource creation
    }

    @PUT                     // => HTTP PUT method (update resource)
    @Path("/{id}")           // => PUT /users/123
    public Response updateUser(@PathParam("id") Long id, User user) {
                                 // => Updates user with ID from URL path
        userService.update(id, user);
                                 // => Updates user in database
        return Response.ok(user).build();
                                 // => Returns HTTP 200 OK with updated user
    }

    @DELETE                  // => HTTP DELETE method (delete resource)
    @Path("/{id}")           // => DELETE /users/123
                                 // => Path parameter for target user ID
    public Response deleteUser(@PathParam("id") Long id) {
                                 // => Deletes user with specified ID
                                 // => Extracts ID from URL path
        userService.delete(id);
                                 // => Removes user from database
                                 // => Service layer handles deletion logic
        return Response.noContent().build();
                                 // => Returns HTTP 204 No Content (successful deletion)
                                 // => 204 indicates success without response body
    }
}
                                 // => End of UserResource class

// MicroProfile Config - externalized configuration
import org.eclipse.microprofile.config.inject.ConfigProperty;
                                 // => MicroProfile Config annotation for property injection
                                 // => Enables 12-factor app configuration

@ApplicationScoped           // => CDI scope: one instance per application
                                 // => Shared across all requests (singleton-like)
                                 // => Bean lifecycle managed by container
class ConfigExample {
                                 // => Demonstrates externalized configuration pattern
    @Inject                  // => CDI dependency injection
                                 // => Container injects configuration at runtime
    @ConfigProperty(name = "database.url")
                                 // => Injects value from config property "database.url"
                                 // => Checks system properties, env vars, config files
                                 // => Config sources: META-INF/microprofile-config.properties, env vars
    String databaseUrl; // Injected from config source
                                 // => databaseUrl = value from DATABASE_URL env var or database.url property
                                 // => Example: "jdbc:postgresql://localhost:5432/mydb"

    @Inject
                                 // => CDI injection for second property
    @ConfigProperty(name = "database.timeout", defaultValue = "30")
                                 // => Injects "database.timeout" property with default 30
                                 // => Falls back to 30 if property not found
                                 // => defaultValue prevents deployment failure if missing
    int timeout;             // => timeout = 30 (default) or configured value
                                 // => Type conversion automatic (String to int)
}
                                 // => End of ConfigExample class

// Config sources: system properties, environment variables, microprofile-config.properties
                                 // => Config priority: system properties > env vars > config files
                                 // => DATABASE_URL env var overrides database.url property

// Health checks
import org.eclipse.microprofile.health.*;
                                 // => MicroProfile Health API for health check endpoints

@Health                      // => Marks class as health check (deprecated, use @Liveness/@Readiness)
                                 // => Container registers this as health check endpoint
@ApplicationScoped           // => One instance per application
                                 // => Bean managed by CDI container
class DatabaseHealthCheck implements HealthCheck {
                                 // => HealthCheck interface: single call() method
                                 // => MicroProfile Health contract
    @Override
    public HealthCheckResponse call() {
                                 // => Called by health check endpoint (GET /health)
                                 // => Executed when Kubernetes probes health endpoint
        boolean isHealthy = checkDatabaseConnection();
                                 // => Checks if database connection is alive
                                 // => Returns true if DB accessible, false otherwise
        return HealthCheckResponse.named("database")
                                 // => Health check name: "database"
                                 // => Identifies this check in aggregated response
            .state(isHealthy)    // => UP if true, DOWN if false
                                 // => Sets health state based on check result
            .withData("connection", "active")
                                 // => Additional metadata in health response
                                 // => Optional diagnostic information
            .build();            // => Returns JSON: {"name":"database","state":"UP","data":{"connection":"active"}}
                                 // => Builder pattern creates HealthCheckResponse
    }
}
                                 // => End of DatabaseHealthCheck class

// Liveness vs Readiness
@Liveness // Pod should be restarted if fails
                                 // => Liveness probe: checks if app is running
                                 // => Kubernetes restarts pod if fails (app deadlocked/crashed)
                                 // => Typical checks: thread deadlock, memory exhaustion
class LivenessCheck implements HealthCheck { /*...*/ }
                                 // => GET /health/live - returns UP if app alive
                                 // => Kubernetes periodically calls this endpoint

@Readiness // Pod should not receive traffic if fails
                                 // => Readiness probe: checks if app can handle requests
                                 // => Kubernetes stops sending traffic if fails (warming up, dependencies down)
                                 // => Typical checks: database connection, external service availability
class ReadinessCheck implements HealthCheck { /*...*/ }
                                 // => GET /health/ready - returns UP if ready for traffic
                                 // => Kubernetes adds pod to load balancer when ready

// Metrics
import org.eclipse.microprofile.metrics.annotation.*;
                                 // => MicroProfile Metrics annotations

@ApplicationScoped
class MetricsExample {
    @Counted(name = "user_requests", description = "Number of user requests")
                                 // => Counter metric: increments each time method called
                                 // => Exposed as Prometheus metric: user_requests_total
                                 // => Annotations automatically instrument method
    @Timed(name = "user_request_time", description = "Time to process user requests")
                                 // => Timer metric: records execution duration
                                 // => Exposes p50, p99, max, mean latencies
                                 // => Multiple annotations on same method allowed
    public void processUser() {
                                 // => Method automatically instrumented by metrics annotations
        // Method execution counted and timed
                                 // => Each call increments counter and records duration
                                 // => Metrics available at GET /metrics (Prometheus format)
                                 // => No manual instrumentation code required
    }

    @Gauge(name = "active_users", unit = MetricUnits.NONE)
                                 // => Gauge metric: current value (not cumulative)
                                 // => Polled periodically, returns current active users
                                 // => Unlike counters (always increase), gauges can go up/down
    public long getActiveUsers() {
                                 // => Method called periodically by metrics system
        return userService.countActive();
                                 // => Returns current count of active users
                                 // => Gauge value = return value
                                 // => Prometheus scrapes this value at configured interval
    }
}
                                 // => End of metrics example class

// Fault Tolerance
import org.eclipse.microprofile.faultto tolerance.*;
                                 // => MicroProfile Fault Tolerance for resilience patterns

@ApplicationScoped
class FaultToleranceExample {
    @Retry(maxRetries = 3, delay = 1000) // Retry up to 3 times, 1 second delay
                                 // => Automatically retries on exception
                                 // => maxRetries = 3: tries up to 4 times total (initial + 3 retries)
                                 // => delay = 1000ms between retry attempts
                                 // => Fault tolerance pattern for transient failures
    public String callExternalService() {
                                 // => Calls external service with automatic retry
                                 // => Protected by retry policy
        return externalService.getData();
                                 // => If fails, retries 3 times with 1 second delay
                                 // => Total max time: ~3 seconds (3 retries × 1 second)
                                 // => Throws exception if all retries exhausted
    }

    @CircuitBreaker(
                                 // => Circuit breaker pattern: prevents cascading failures
        requestVolumeThreshold = 4,
                                 // => Minimum 4 requests before circuit opens
                                 // => Circuit doesn't activate until 4 requests received
                                 // => Requires initial request history
        failureRatio = 0.5,      // => Opens if 50% of requests fail
                                 // => Example: 2 of 4 requests fail → circuit opens
                                 // => Decimal between 0.0 and 1.0
        delay = 5000             // => Circuit stays open for 5 seconds
                                 // => After 5 seconds, enters half-open state (allows 1 request)
                                 // => Delay in milliseconds (5000ms = 5 seconds)
    )
    public String callUnreliableService() {
                                 // => Protected by circuit breaker pattern
        // Circuit opens if 50% of 4 requests fail
        // Opens for 5 seconds before half-open state
                                 // => While open: immediately throws CircuitBreakerOpenException
                                 // => Half-open: allows 1 request. Success → closes, failure → opens
                                 // => Three states: closed (normal), open (failing fast), half-open (testing)
        return unreliableService.getData();
                                 // => Returns data if circuit closed/half-open and request succeeds
                                 // => Actual call to unreliable external service
    }

    @Timeout(1000) // Timeout after 1 second
                                 // => Throws TimeoutException if method exceeds 1000ms
                                 // => Interrupts long-running operations
                                 // => Prevents indefinite blocking on slow services
    @Fallback(fallbackMethod = "fallbackMethod")
                                 // => Calls fallbackMethod if timeout or exception occurs
                                 // => fallbackMethod must have same signature
                                 // => Provides graceful degradation
    public String callSlowService() {
                                 // => Calls service with 1 second timeout
                                 // => Protected by both timeout and fallback
        return slowService.getData();
                                 // => If exceeds 1000ms → TimeoutException → fallbackMethod
                                 // => If > 1 second, throws TimeoutException → calls fallbackMethod
    }

    public String fallbackMethod() {
                                 // => Fallback method (same signature as callSlowService)
                                 // => Must match original method signature exactly
        return "Fallback data"; // => Returns default data when main method fails
                                 // => Called on timeout, exception, or circuit breaker open
                                 // => Provides degraded but functional response
    }

    @Bulkhead(value = 5, waitingTaskQueue = 10)
                                 // => Limits concurrent executions (semaphore pattern)
                                 // => value = 5: max 5 concurrent executions
                                 // => waitingTaskQueue = 10: max 10 waiting requests
                                 // => Bulkhead pattern prevents resource exhaustion
    public String limitedConcurrency() {
                                 // => Protected by bulkhead (concurrency limit)
        // Max 5 concurrent executions, 10 waiting
                                 // => 6th concurrent request waits in queue (up to 10)
                                 // => 16th concurrent request immediately rejected (BulkheadException)
                                 // => Total capacity: 5 executing + 10 queued = 15 max
        return service.getData();
                                 // => Returns data if semaphore acquired (not at limit)
                                 // => Releases semaphore when method completes
    }
}

// JWT Authentication
import org.eclipse.microprofile.jwt.*;
                                 // => MicroProfile JWT for stateless authentication

@ApplicationScoped
class SecureResource {
    @Inject
    JsonWebToken jwt;            // => Injected JWT token from HTTP Authorization header
                                 // => Header: "Authorization: Bearer <token>"
                                 // => JWT contains claims: username, roles, expiration

    @GET
    @Path("/secure")             // => GET /secure endpoint
    @RolesAllowed("user")        // => Requires "user" role in JWT
                                 // => Returns HTTP 403 Forbidden if role not present
    public String secureEndpoint() {
                                 // => Secured endpoint requiring JWT with "user" role
        String username = jwt.getName();
                                 // => Extracts username from JWT "sub" (subject) claim
                                 // => Example: "john.doe"
        return "Hello, " + username;
                                 // => Returns personalized greeting with username
                                 // => Example: "Hello, john.doe"
    }
}

// Containerization (Docker)
/*
FROM openjdk:17-jdk-slim
COPY target/myapp.jar /app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "/app.jar"]
*/

// Kubernetes deployment
/*
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
spec:
  replicas: 3
  template:
    spec:
      containers:
      - name: myapp
        image: myapp:latest
        ports:
        - containerPort: 8080
        livenessProbe:
          httpGet:
            path: /health/live
            port: 8080
        readinessProbe:
          httpGet:
            path: /health/ready
            port: 8080
*/
```

**Key Takeaway**: MicroProfile standardizes cloud-native Java microservices. JAX-RS for REST, Config for externalized configuration, Health for liveness/readiness probes, Metrics for observability, Fault Tolerance (retry, circuit breaker, timeout, bulkhead) for resilience. Integrates with Docker and Kubernetes.

**Why It Matters**: CountDownLatch enables thread coordination—wait for multiple tasks to complete before proceeding. It's essential for parallel initialization (wait for services to start), barrier synchronization (all threads reach checkpoint), and testing (wait for async operations). Understanding one-time use (latch can't reset) determines appropriate scenarios vs CyclicBarrier. Latches power phased execution—start phase 2 only after phase 1 completes. They're common in production systems requiring coordination across concurrent operations without busy waiting.

---

## Example 73: Reactive Programming with Reactive Streams

Reactive Streams enable asynchronous data processing with backpressure. Publishers emit data, Subscribers consume, Subscriptions control flow. Project Reactor (Flux/Mono) provides practical implementation. Ideal for high-throughput, non-blocking I/O.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[Publisher] -->|subscribe| B[Subscriber]
    B -->|request n| C[Subscription]
    C -->|onNext x n| B
    C -->|onComplete/onError| B

    D[Backpressure] --> C

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#fff
```

**Code**:

```java
// Reactive Streams API (Java 9+ Flow API)
import java.util.concurrent.Flow.*;
                                 // => Java 9+ standard Reactive Streams API
                                 // => Classes: Publisher, Subscriber, Subscription, Processor

// Publisher interface
interface Publisher<T> {        // => Source of data (emits elements)
                                 // => Generic type T: type of emitted elements
                                 // => Contract for all Publishers in Reactive Streams
    void subscribe(Subscriber<? super T> subscriber);
                                 // => Connects subscriber to this publisher
                                 // => Triggers data flow when subscriber ready
                                 // => ? super T: contravariant, accepts any supertype of T
}                                // => Interface defines Publisher contract only

// Subscriber interface
interface Subscriber<T> {        // => Consumer of data (receives elements)
                                 // => Generic type T: type of consumed elements
                                 // => Contract for all Subscribers in Reactive Streams
    void onSubscribe(Subscription subscription);
                                 // => Called first when subscription starts
                                 // => Receives Subscription for backpressure control
                                 // => Subscriber stores Subscription to request items
    void onNext(T item);         // => Called for each emitted item
                                 // => Receives one element at a time
                                 // => May be called 0 to N times
    void onError(Throwable throwable);
                                 // => Called on error (terminal event)
                                 // => No more onNext calls after onError
                                 // => Either onError OR onComplete called (mutually exclusive)
    void onComplete();           // => Called when publisher finished (terminal event)
                                 // => No more onNext calls after onComplete
                                 // => Either onError OR onComplete called (never both)
}                                // => Interface defines Subscriber lifecycle contract

// Subscription interface
interface Subscription {         // => Controls data flow between Publisher and Subscriber
                                 // => Implements backpressure mechanism
                                 // => Mediates between Publisher and Subscriber
    void request(long n);        // => Request n items (backpressure signal)
                                 // => Subscriber requests n items from Publisher
                                 // => Publisher sends ≤ n items (respects backpressure)
                                 // => Pull-based: subscriber controls rate (backpressure)
    void cancel();               // => Cancel subscription (stop emitting)
                                 // => Subscriber cancels subscription (no more items)
                                 // => Publisher stops emitting
                                 // => Cleanup: releases resources
}                                // => Interface defines Subscription flow control contract

// Simple Publisher implementation
class SimplePublisher implements Publisher<Integer> {
                                 // => Concrete Publisher emitting Integer values
    @Override
    public void subscribe(Subscriber<? super Integer> subscriber) {
                                 // => Called when subscriber subscribes
        subscriber.onSubscribe(new Subscription() {
                                 // => Creates Subscription instance (anonymous inner class)
                                 // => Sends Subscription to subscriber
            private boolean cancelled = false;
                                 // => Tracks cancellation state

            @Override
            public void request(long n) {
                                 // => Called when subscriber requests n items
                                 // => n: number of items requested (backpressure signal)
                if (cancelled) return;
                                 // => Don't emit if subscription cancelled
                for (int i = 0; i < n; i++) {
                                 // => Emits n integers (0 to n-1)
                    subscriber.onNext(i);
                                 // => Calls subscriber's onNext for each item
                                 // => Pushes item to subscriber
                }
                subscriber.onComplete();
                                 // => Signals completion (no more items)
                                 // => Terminal event
            }

            @Override
            public void cancel() {
                                 // => Called when subscriber cancels
                cancelled = true;
                                 // => Sets flag to stop emitting items
            }
        });
    }
}

// Simple Subscriber implementation
class SimpleSubscriber implements Subscriber<Integer> {
                                 // => Concrete Subscriber consuming Integer values
    private Subscription subscription;
                                 // => Stores Subscription for requesting items

    @Override
    public void onSubscribe(Subscription subscription) {
                                 // => Called first when subscription starts
        this.subscription = subscription;
                                 // => Saves Subscription for later use
        subscription.request(5); // Request 5 items (backpressure control)
                                 // => Requests 5 items from Publisher
                                 // => Backpressure: subscriber controls flow (pulls data)
    }

    @Override
    public void onNext(Integer item) {
                                 // => Called for each item emitted by Publisher
        System.out.println("Received: " + item);
                                 // => Output: "Received: 0", "Received: 1", ..., "Received: 4"
    }

    @Override
    public void onError(Throwable throwable) {
                                 // => Called if Publisher errors
        System.err.println("Error: " + throwable.getMessage());
                                 // => Logs error message
    }

    @Override
    public void onComplete() {   // => Called when Publisher completes
        System.out.println("Complete");
                                 // => Output: "Complete"
    }
}

SimplePublisher publisher = new SimplePublisher();
                                 // => Creates Publisher instance
publisher.subscribe(new SimpleSubscriber());
                                 // => Subscribes SimpleSubscriber to Publisher
                                 // => Triggers onSubscribe → request(5) → onNext(0..4) → onComplete

// Project Reactor - practical reactive programming (external library)
/*
import reactor.core.publisher.*;

// Mono - 0 or 1 element
Mono<String> mono = Mono.just("Hello");
mono.subscribe(System.out::println); // => "Hello"

Mono<String> empty = Mono.empty();
Mono<String> error = Mono.error(new RuntimeException("Error"));

// Flux - 0 to N elements
Flux<Integer> flux = Flux.just(1, 2, 3, 4, 5);
flux.subscribe(System.out::println); // => 1, 2, 3, 4, 5

// Transformation operators
Flux<Integer> mapped = flux.map(i -> i * 2); // => 2, 4, 6, 8, 10
Flux<Integer> filtered = flux.filter(i -> i % 2 == 0); // => 2, 4
Flux<Integer> flatMapped = flux.flatMap(i -> Flux.range(0, i)); // Flattens nested publishers

// Backpressure strategies
Flux<Integer> limited = flux.limitRate(2); // Request 2 at a time
Flux<Integer> buffered = flux.buffer(3); // Buffer into chunks of 3
Flux<Integer> windowed = flux.window(3); // Window into Flux<Flux<T>>

// Error handling
Flux<Integer> withFallback = flux
    .map(i -> {
        if (i == 3) throw new RuntimeException("Error at 3");
        return i;
    })
    .onErrorReturn(0); // Fallback value on error

Flux<Integer> withRetry = flux.retry(3); // Retry up to 3 times

// Combining publishers
Flux<String> merged = Flux.merge(Flux.just("A", "B"), Flux.just("C", "D"));
Flux<String> zipped = Flux.zip(Flux.just(1, 2), Flux.just("A", "B"),
    (num, letter) -> num + letter); // => "1A", "2B"

// Schedulers (threading)
Flux<Integer> async = flux.subscribeOn(Schedulers.parallel()); // Subscribe on parallel thread
Flux<Integer> asyncOps = flux.publishOn(Schedulers.boundedElastic()); // Operations on elastic thread

// Hot vs Cold publishers
// Cold: starts emitting when subscribed (Flux.just, Flux.range)
// Hot: emits regardless of subscribers (ConnectableFlux)

ConnectableFlux<Integer> hot = flux.publish();
hot.subscribe(i -> System.out.println("Sub 1: " + i));
hot.subscribe(i -> System.out.println("Sub 2: " + i));
hot.connect(); // Start emitting
*/

// Reactive vs Imperative comparison
// Imperative (blocking)
List<String> results = new ArrayList<>();
                                 // => Mutable list to collect results
for (String url : urls) {        // => Sequential loop (one URL at a time)
                                 // => Processes URLs serially (not concurrent)
    String data = httpClient.get(url); // Blocks
                                 // => Blocks thread until HTTP request completes
                                 // => Thread idle during network I/O (inefficient)
                                 // => 100 URLs × 100ms each = 10 seconds total
    results.add(data);           // => Adds result to list
}

// Reactive (non-blocking)
/*
Flux.fromIterable(urls)          // => Creates Flux from URL list
                                 // => Emits each URL asynchronously
    .flatMap(url -> webClient.get(url)) // Non-blocking, concurrent
                                 // => Concurrent HTTP requests (non-blocking)
                                 // => Thread not blocked during I/O
                                 // => 100 URLs concurrent = ~100ms total (100x faster)
    .collectList()               // => Collects all results into List
                                 // => Returns Mono<List<String>>
    .subscribe(results -> {
        // All results ready
                                 // => Callback when all requests complete
                                 // => results: List<String> with all responses
    });
*/

// Use cases for Reactive Streams
// 1. High-throughput systems (millions of requests/sec)
                                 // => Non-blocking I/O scales better than threads
                                 // => Example: WebFlux handles 100k+ concurrent requests
// 2. Non-blocking I/O (database, HTTP, message queues)
                                 // => I/O-bound workloads benefit from non-blocking
                                 // => Reactive drivers: R2DBC (database), WebClient (HTTP)
// 3. Event streaming (real-time data pipelines)
                                 // => Stream processing: Kafka → transform → database
                                 // => Continuous data flow with backpressure
// 4. Backpressure handling (prevent overwhelming slow consumers)
                                 // => Consumer controls rate (request(n))
                                 // => Prevents OutOfMemoryError from fast producers

// When NOT to use reactive
// 1. Simple CRUD applications (overkill, complexity not justified)
                                 // => Traditional blocking I/O sufficient for low traffic
                                 // => Reactive adds complexity without benefit
// 2. CPU-bound operations (blocking acceptable)
                                 // => CPU-bound: reactive provides no benefit (no I/O wait)
                                 // => Example: heavy computation, image processing
// 3. Team unfamiliar with reactive paradigm
                                 // => Steep learning curve (operators, backpressure, debugging)
                                 // => Training required before adoption
```

**Key Takeaway**: Reactive Streams enable async data processing with backpressure. Publisher emits, Subscriber consumes, Subscription controls flow with `request(n)`. Project Reactor provides Flux (0-N) and Mono (0-1) with rich operators. Ideal for high-throughput, non-blocking I/O. Backpressure prevents overwhelming slow consumers.

**Why It Matters**: Concurrent collections provide building blocks for scalable systems—ConcurrentHashMap for caches, CopyOnWriteArrayList for event listeners, BlockingQueue for producer-consumer patterns. Understanding performance characteristics (ConcurrentHashMap: fast reads/writes, CopyOnWrite: fast reads/slow writes, BlockingQueue: coordination) enables choosing appropriately. Weakly consistent iterators prevent ConcurrentModificationException during traversal. These collections prevent manual synchronization bugs (deadlocks, races) while providing better performance than synchronized wrappers. They're foundational to concurrent system design.

---

## Example 74: Virtual Threads (Project Loom, Java 21+)

Virtual threads enable millions of lightweight threads with low overhead. M:N mapping to platform threads. Ideal for I/O-bound workloads. Simplifies concurrent code without callbacks. Available in Java 21+.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[Platform Threads] --> B[OS Thread 1]
    A --> C[OS Thread 2]
    A --> D[OS Thread N]

    E[Virtual Threads] --> F[Virtual 1-1000]
    E --> G[Virtual 1001-2000]
    E --> H[Virtual N]

    F & G & H --> I[Carrier Thread Pool]
    I --> B & C & D

    style A fill:#0173B2,color:#fff
    style E fill:#DE8F05,color:#fff
    style I fill:#029E73,color:#fff
```

**Code**:

```java
// Traditional platform thread (heavyweight)
Thread platformThread = new Thread(() -> {                             // => Creates platform thread
    System.out.println("Platform thread: " + Thread.currentThread());  // => Output: Platform thread: Thread[Thread-0,5,main]
});                                                                     // => Maps 1:1 to OS thread (heavyweight)
platformThread.start();                                                // => Starts thread execution

// Virtual thread (lightweight, Java 21+)
Thread virtualThread = Thread.ofVirtual().start(() -> {                // => Creates and starts virtual thread
    System.out.println("Virtual thread: " + Thread.currentThread());   // => Output: Virtual thread: VirtualThread[#21]/runnable@ForkJoinPool-1-worker-1
});                                                                     // => M:N mapping to carrier threads (lightweight)

// Create virtual thread with name
Thread namedVirtual = Thread.ofVirtual()                               // => Builder for virtual thread
    .name("my-virtual-thread")                                         // => Sets thread name
    .start(() -> {                                                     // => Starts named virtual thread
        System.out.println(Thread.currentThread().getName());          // => Output: my-virtual-thread
    });

// Executor for virtual threads
ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor(); // => Creates executor that spawns virtual thread per task
executor.submit(() -> {                                                 // => Submits task to executor
    System.out.println("Task in virtual thread");                       // => Output: Task in virtual thread
});                                                                      // => Task runs in virtual thread
executor.close();                                                        // => Auto-closes when all tasks complete (Java 19+ AutoCloseable)

// Massive concurrency with virtual threads
// => Platform threads: 1000s max (each ~1MB stack, OS limit)
// => Virtual threads: millions possible (each ~1KB, JVM-managed)

try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
                                 // => Creates executor for virtual threads
                                 // => Auto-closeable (try-with-resources)
                                 // => Each submit() creates new virtual thread (no pooling)
    for (int i = 0; i < 1_000_000; i++) {
                                 // => 1 million iterations (would crash with platform threads)
        int taskId = i;          // => Capture loop variable for lambda
        executor.submit(() -> {  // => Creates virtual thread per task
                                 // => Virtual thread is cheap (~1KB overhead)
                                 // => Million virtual threads map to ~N carrier threads
            System.out.println("Task " + taskId);
                                 // => Output: "Task 0", "Task 1", etc.
                                 // => Output order non-deterministic (concurrent execution)
        });                      // => Task submitted, thread created immediately
    }
} // => Auto-waits for completion (try-with-resources close)
  // => All 1 million tasks complete before exiting block
  // => No manual shutdown needed

// Virtual threads excel at I/O-bound work
ExecutorService ioExecutor = Executors.newVirtualThreadPerTaskExecutor();
                                 // => Executor for I/O-bound tasks
ioExecutor.submit(() -> {        // => Submits I/O task in virtual thread
    String data = httpClient.get("https://api.example.com");
                                 // => Blocking I/O call (waits for network response)
                                 // => Virtual thread UNMOUNTS from carrier thread during wait
                                 // => Carrier thread freed to run other virtual threads
                                 // => Virtual thread REMOUNTS when I/O completes
                                 // => data = HTTP response body (after network wait)
    System.out.println(data);    // => Output: [response data]
});                              // => Virtual thread lifecycle: create → mount → unmount → remount → complete
// => Carrier thread never blocks (handles other virtual threads during I/O)

// Structured concurrency (preview feature, Java 21+)
// => Scoped thread lifetime management (parent-child relationship)
// => Ensures child tasks complete before parent continues
/*
try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
                                 // => Creates scope with fail-fast policy
                                 // => Scope shuts down if any subtask fails
                                 // => Auto-closeable (ensures cleanup)
    Future<String> user = scope.fork(() -> fetchUser(userId));
                                 // => Forks subtask to fetch user (runs in virtual thread)
                                 // => Returns Future for result
    Future<String> orders = scope.fork(() -> fetchOrders(userId));
                                 // => Forks subtask to fetch orders (parallel with user)
                                 // => Both tasks run concurrently

    scope.join();                // => Waits for both subtasks to complete
                                 // => Blocks until all forked tasks finish
    scope.throwIfFailed();       // => Propagates exception if any subtask failed
                                 // => Fails fast (cancels other tasks on first failure)

    return new UserData(user.resultNow(), orders.resultNow());
                                 // => resultNow() returns completed result (no blocking)
                                 // => Both results guaranteed available after join()
}                                // => Scope closed, all subtasks guaranteed complete
*/

// Virtual thread characteristics
// => 1. Cheap to create (millions possible, minimal overhead)
// => 2. Unmount from carrier thread when blocking (I/O, sleep, locks)
// => 3. No thread pooling needed (create per-task, lightweight)
// => 4. Same Thread API, different implementation (drop-in replacement)

// When virtual threads excel
// => - High concurrency I/O (web servers, database queries, API calls)
// => - Blocking I/O operations (network, file, database)
// => - Simplifies async code (no callbacks, futures, reactive)

// When virtual threads DON'T help
// => - CPU-bound work (same cores available, no benefit)
// => - Synchronized blocks (pins carrier thread, defeats purpose)
// => - Native code (JNI pins carrier thread, blocks carrier)

// Pinning (carrier thread blocked, BAD)
Object lock = new Object();      // => Shared lock object
synchronized (lock) {            // => Synchronized block pins carrier thread
                                 // => Virtual thread CANNOT unmount during synchronized
                                 // => Carrier thread blocked for entire duration
    Thread.sleep(1000);          // => Blocks carrier thread (defeats virtual thread benefit)
                                 // => Sleep doesn't unmount (pinned by synchronized)
}                                // => Carrier thread unavailable to other virtual threads

// Solution: use ReentrantLock instead
ReentrantLock reentrantLock = new ReentrantLock();
                                 // => ReentrantLock allows unmounting (virtual thread friendly)
reentrantLock.lock();            // => Acquires lock (virtual thread can unmount if needed)
                                 // => Virtual thread unmounts if lock unavailable
try {
    Thread.sleep(1000);          // => Sleep unmounts virtual thread (frees carrier)
                                 // => Carrier thread available to other virtual threads
} finally {
    reentrantLock.unlock();      // => Release lock (always in finally)
                                 // => Ensures lock released even on exception
}

// Thread.sleep() with virtual threads
Thread.sleep(1000);              // => Virtual thread unmounts from carrier (doesn't block)
                                 // => Carrier thread freed for other virtual threads
                                 // => Virtual thread remounts after 1000ms

// Comparing platform vs virtual threads
// => Platform: 1-1 mapping to OS thread, expensive (~1MB stack), limited (1000s)
// => Virtual: M-N mapping to carrier threads, cheap (~1KB), millions possible

// Migration from platform to virtual threads
ExecutorService oldExecutor = Executors.newFixedThreadPool(10);
                                 // => Old: 10 platform threads (heavyweight, pooled)
                                 // => Limits concurrency to 10 tasks

ExecutorService newExecutor = Executors.newVirtualThreadPerTaskExecutor();
                                 // => New: virtual thread per task (lightweight, no limit)
                                 // => Same ExecutorService API, different implementation
                                 // => Code unchanged, just swap executor

// Virtual thread debugging
// => jcmd <pid> Thread.dump_to_file -format=json <file>
// => Virtual threads visible in thread dumps (JFR events)
// => Carrier thread mapping visible for debugging pinning issues
```

**Key Takeaway**: Virtual threads are lightweight (millions possible) with M:N mapping to platform threads. Ideal for I/O-bound workloads—unmount from carrier when blocking. Create per-task with `Thread.ofVirtual()` or `Executors.newVirtualThreadPerTaskExecutor()`. Avoid synchronized blocks (pins carrier), use `ReentrantLock`. Simplifies concurrency without callbacks.

**Why It Matters**: Phaser enables dynamic participant coordination—threads can register/deregister during execution. It generalizes CountDownLatch (one-time) and CyclicBarrier (fixed parties) with multi-phase support. Phasers enable complex coordination patterns (parallel algorithms with phases, test orchestration). Understanding termination (when parties reach zero) prevents resource leaks. Phasers are powerful but complex—use simpler primitives when possible. They shine in scenarios requiring flexible, multi-phase coordination across dynamic sets of threads.

---

## Example 75: Modern Java Best Practices

Modern Java emphasizes immutability, composition, type safety, and simplicity. Records for data, sealed classes for domain modeling, pattern matching for cleaner code. Testing and modularity are essential. Streams and Optional improve expressiveness.

**Code**:

```java
// Prefer composition over inheritance
// Bad - inheritance for code reuse
class Stack extends ArrayList {                                      // => Inherits 20+ ArrayList methods
    // Inherits methods that break stack semantics (add at index, etc.)  // => Violates encapsulation (stack = LIFO only)
}

// Good - composition
class Stack {                                                        // => Composition-based design
    private List items = new ArrayList<>();                         // => Delegates to internal list

    public void push(Object item) { items.add(item); }             // => Only LIFO push operation
    public Object pop() { return items.remove(items.size() - 1); } // => Only LIFO pop operation
    // Only expose stack operations                                  // => Prevents misuse (no add at index)
}

// Use streams judiciously (readability vs performance)
// Stream: readable, potential parallelization
List<String> names = users.stream()                                 // => Creates stream from users list
    .filter(u -> u.isActive())                                      // => Filters active users only
    .map(User::getName)                                             // => Extracts names
    .collect(Collectors.toList());                                  // => names = ["Alice", "Bob", "Charlie"]

// For-loop: faster for simple operations
List<String> names2 = new ArrayList<>();                            // => names2 = [] (empty)
for (User u : users) {                                              // => Iterates each user
    if (u.isActive()) {                                             // => Checks active status
        names2.add(u.getName());                                    // => Adds name to list
    }                                                                // => Faster for small collections
}                                                                    // => names2 = ["Alice", "Bob", "Charlie"]

// Leverage records for data carriers
record Point(int x, int y) {}                                       // => Compact data class (Java 16+)
record Person(String name, int age) {}                              // => Auto-generates constructor, getters, equals, hashCode, toString

Point p = new Point(10, 20);                                        // => p.x() = 10, p.y() = 20
// Auto-generated: constructor, getters, equals, hashCode, toString  // => p.toString() = "Point[x=10, y=20]"

// Sealed classes for domain modeling
sealed interface Result permits Success, Failure {}                 // => Only Success/Failure can implement Result
record Success(String data) implements Result {}                    // => Success case
record Failure(String error) implements Result {}                   // => Failure case

// Pattern matching for cleaner code
String message = switch (result) {                                  // => Exhaustive pattern matching
    case Success(String data) -> "Success: " + data;                // => Destructures Success record
    case Failure(String error) -> "Error: " + error;                // => Destructures Failure record
};                                                                   // => message = "Success: OK" or "Error: Failed"

// Immutability by default
// Bad - mutable
class MutablePoint {                                                 // => Public fields = mutable
    public int x, y;                                                 // => Can be changed: p.x = 999
}

// Good - immutable
record ImmutablePoint(int x, int y) {}                              // => Final fields, no setters
                                                                     // => Cannot change after creation

// Explicit nullability with Optional
// Bad - null return
public User findUser(Long id) {                                     // => Returns User or null (implicit)
    return null;                                                     // => Caller must check for null manually
}

// Good - Optional
public Optional<User> findUser(Long id) {                           // => Returns Optional<User> (explicit nullability)
    return Optional.ofNullable(repository.find(id));                // => Wraps nullable value
}                                                                    // => Caller forced to handle empty case

user.ifPresent(u -> System.out.println(u.getName()));               // => Only prints if user present
                                                                     // => Output: "Alice" (if user exists)

// Use var for obvious types
var users = userRepository.findAll();
                                 // => Type inferred: List<User> (obvious from method return type)
                                 // => Compiler resolves type at compile time (not runtime)
                                 // => users is List<User> (static typing preserved)
var count = users.size();        // => Type inferred: int (obvious from List.size() return type)
                                 // => count is int (static typing)

// Testing as first-class activity
@Test                            // => JUnit test annotation (marks method as test)
void shouldCreateUser() {        // => Test method (void return, descriptive name)
                                 // => Naming: should[Action] describes expected behavior
    // Arrange                   // => Setup phase: prepare test data
    User user = new User("Alice", "alice@example.com");
                                 // => user = User[name=Alice, email=alice@example.com]
                                 // => Test data created

    // Act                       // => Execution phase: invoke system under test
    userService.create(user);    // => Calls create method (operation being tested)
                                 // => Persists user to repository

    // Assert                    // => Verification phase: check expected outcome
    User saved = userRepository.find(user.getId());
                                 // => Retrieves user from repository
                                 // => saved should match original user
    assertEquals("Alice", saved.getName());
                                 // => Verifies name persisted correctly
                                 // => Test fails if names don't match
}

// Modern module organization (Java 9+)
// => module-info.java defines module boundaries
// => Encapsulation at module level (stronger than package-private)
module com.example.app {         // => Declares module com.example.app
    requires java.sql;           // => Declares dependency on java.sql module
                                 // => Module system enforces at compile/runtime
    exports com.example.app.api; // => Makes api package accessible to other modules
                                 // => Internal packages hidden by default
    opens com.example.app.entity to org.hibernate.orm;
                                 // => Allows reflective access to entity package
                                 // => Required for Hibernate to access private fields
}                                // => Module provides strong encapsulation

// Prefer method references over lambdas
// => Lambda: verbose, explicit parameter
users.forEach(u -> System.out.println(u));
                                 // => Lambda receives User u, prints u
                                 // => Output: [User@123, User@456, ...]

// => Method reference: cleaner, same behavior
users.forEach(System.out::println);
                                 // => Method reference (::) to println
                                 // => Equivalent to u -> System.out.println(u)
                                 // => Output: [User@123, User@456, ...]

// Use try-with-resources for AutoCloseable
try (var reader = new BufferedReader(new FileReader("file.txt"))) {
                                 // => Creates BufferedReader (implements AutoCloseable)
                                 // => try-with-resources ensures reader.close() called
                                 // => Automatic cleanup even on exception
    return reader.readLine();    // => Reads first line from file
                                 // => Returns line (reader auto-closed after return)
} // => reader.close() called here (automatic)
  // => Prevents resource leaks

// Avoid premature optimization
// => 1. Write clear code first (readability priority)
// => 2. Profile to find bottlenecks (measure, don't guess)
// => 3. Optimize where measurements prove necessary (data-driven)
// => "Premature optimization is the root of all evil" - Donald Knuth

// Document public APIs
/**
 * Calculates the sum of two integers.
 *                                 // => Javadoc comment for public API
 * @param a the first integer     // => Parameter documentation
 * @param b the second integer    // => Parameter documentation
 * @return the sum of a and b     // => Return value documentation
 */                               // => Generates HTML documentation (javadoc tool)
public int add(int a, int b) {   // => Public API method
    return a + b;                // => Returns sum (a + b)
}

// Use enums for type-safe constants
enum Status { PENDING, APPROVED, REJECTED }
                                 // => Type-safe enumeration (compile-time checked)
                                 // => Status.PENDING, Status.APPROVED, Status.REJECTED
                                 // => Prevents invalid values (no magic strings)

// Modern exception handling (multi-catch, Java 7+)
try {
    riskyOperation();            // => May throw IOException or SQLException
                                 // => Calls operation that might fail
} catch (IOException | SQLException e) {
                                 // => Multi-catch: handles both exception types
                                 // => Single catch block instead of two separate blocks
                                 // => e is final (cannot reassign)
    logger.error("Operation failed", e);
                                 // => Logs error with exception stacktrace
                                 // => Same handler for both exception types
}

// Text blocks for multi-line strings (Java 15+)
String json = """
    {
        "name": "Alice",
        "age": 30
    }
    """;                         // => Text block (multi-line string literal)
                                 // => Preserves formatting (no escape sequences)
                                 // => Automatic indentation handling
                                 // => json = "{\n    \"name\": \"Alice\",\n    \"age\": 30\n}\n"

// Switch expressions (Java 14+)
int numLetters = switch (day) {  // => Switch as expression (returns value)
                                 // => Exhaustiveness checked at compile time
    case MONDAY, FRIDAY, SUNDAY -> 6;
                                 // => Multiple cases with arrow syntax
                                 // => Returns 6 (implicit break)
    case TUESDAY -> 7;           // => Single case, returns 7
    default -> throw new IllegalArgumentException();
                                 // => Default required for exhaustiveness (unless all cases covered)
                                 // => Throws exception for unexpected values
};                               // => numLetters assigned result (6 or 7 or exception)

// Helpful NullPointerExceptions (Java 14+)
// => JVM shows WHICH reference was null in call chain
// => Example: user.getAddress().getStreet()
// => Old message: "NullPointerException" (unhelpful)
// => New message: "Cannot invoke getStreet() because getAddress() returned null"
// => Identifies exact null point in chain (debugging aid)

// Modern Java philosophy
// => - Immutability reduces bugs (fewer side effects, safer concurrency)
// => - Composition over inheritance (flexible, avoids fragile base class)
// => - Explicit over implicit (Optional vs null, sealed classes vs open hierarchies)
// => - Type safety (records, sealed classes, pattern matching prevent runtime errors)
// => - Simplicity (avoid over-engineering, YAGNI principle)
// => - Testing and observability built-in (JUnit, JFR, monitoring)
```

**Key Takeaway**: Modern Java emphasizes immutability (records, final), composition over inheritance, type safety (sealed classes, pattern matching), and explicitness (Optional). Use streams for readability, records for data, sealed classes for modeling. Testing essential. Avoid premature optimization—profile first. Simplicity over complexity.

**Why It Matters**: Semaphores limit concurrent access—rate limiting, connection pooling, resource bounding. They prevent resource exhaustion from too many concurrent operations (database connections, API calls, threads). Understanding fairness (FIFO vs non-FIFO acquisition) impacts performance vs latency. Semaphores enable throttling—control parallelism without rejecting requests. However, they don't prevent deadlocks (acquire multiple permits in different orders). Use them for resource limiting, not mutual exclusion (use locks). Semaphores are essential for building resilient systems with controlled concurrency.

---

## Memory Management and JVM Internals

Master Java's automatic memory management, garbage collection algorithms, JVM internals, and performance optimization techniques for building high-performance applications.

## Example 76: JVM Memory Model and Regions

The JVM divides memory into distinct regions: heap (shared object storage), stack (thread-local execution frames), metaspace (class metadata), and direct buffers (off-heap NIO). Understanding memory regions is essential for diagnosing memory issues and tuning performance.

**JVM Memory Architecture**:

```mermaid
graph TD
    JVM["JVM Memory Space"]
    JVM --> Heap["Heap<br/>Shared Objects"]
    JVM --> NonHeap["Non-Heap<br/>Metadata & Code"]
    JVM --> Thread["Thread-Local<br/>Per-Thread State"]

    Heap --> Young["Young Generation<br/>Eden + Survivor"]
    Heap --> Old["Old Generation<br/>Long-lived Objects"]

    NonHeap --> Metaspace["Metaspace<br/>Class Metadata"]
    NonHeap --> CodeCache["Code Cache<br/>JIT Compiled Code"]

    Thread --> Stack["Thread Stack<br/>Method Frames"]
    Thread --> PC["Program Counter<br/>Instruction Pointer"]

    style JVM fill:#0173B2,color:#fff
    style Heap fill:#029E73,color:#fff
    style NonHeap fill:#DE8F05,color:#fff
    style Thread fill:#CC78BC,color:#fff
    style Young fill:#029E73,color:#fff
    style Old fill:#029E73,color:#fff
    style Metaspace fill:#DE8F05,color:#fff
    style CodeCache fill:#DE8F05,color:#fff
    style Stack fill:#CC78BC,color:#fff
    style PC fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.math.BigDecimal;

// Heap allocation - shared across threads
class Account {                          // => Class metadata stored in Metaspace (non-heap)
    private BigDecimal balance;          // => Field reference stored in object (8 bytes)
                                         // => Points to BigDecimal object on heap

    public Account(BigDecimal initial) { // => Constructor creates heap object
        this.balance = initial;          // => Object allocation path: TLAB → Eden → Heap
                                         // => TLAB: Thread-Local Allocation Buffer (fast, lock-free)
                                         // => Eden: Young generation space for new objects
    }
}

// Stack allocation - thread-local
public void processTransaction(BigDecimal amount) {
    // Stack frame created for processTransaction() method
    // => Contains: local variables, operand stack, frame data
    // => Size: ~100-200 bytes per frame (varies by locals/parameters)

    Account acc = new Account(new BigDecimal("1000"));
                                         // => 'acc' reference stored on stack (8 bytes)
                                         // => Account object stored on heap (shared, GC-managed)
                                         // => BigDecimal object stored on heap

    BigDecimal fee = amount.multiply(new BigDecimal("0.01"));
                                         // => 'fee' reference on stack (thread-local, 8 bytes)
                                         // => BigDecimal object on heap (16 byte header + fields)
                                         // => Stack: fast LIFO allocation/deallocation
                                         // => Heap: slower allocation, GC-managed

    BigDecimal total = acc.balance.add(fee);
                                         // => 'total' reference on stack (thread-local)
                                         // => New BigDecimal on heap (immutable, creates new object)

    // Stack frame destroyed when method returns
    // => Local variables (acc, fee, total) removed from stack
    // => Heap objects remain (GC will collect if unreachable)
    // => Stack deallocation: instant (pop frame)
    // => Heap deallocation: delayed (next GC cycle)
}

// Metaspace - class metadata storage
// => Class structure: Account.class stored in Metaspace
// => Method bytecode: processTransaction() bytecode in Metaspace
// => Constants: String literals, numeric constants
// => Metaspace uses native memory (not heap, grows dynamically)
// => Tuning flags: -XX:MetaspaceSize=128m -XX:MaxMetaspaceSize=512m

// Direct buffers - off-heap memory
import java.nio.ByteBuffer;

ByteBuffer directBuffer = ByteBuffer.allocateDirect(1024);
                                         // => Allocates 1KB off-heap (native memory)
                                         // => Faster I/O: no copy between heap and OS buffers
                                         // => Not managed by heap GC (separate lifecycle)
                                         // => Tuning: -XX:MaxDirectMemorySize=512m
```

**Key Takeaway**: JVM memory has four regions: heap (shared objects, GC-managed), stack (thread-local frames, auto-deallocated), metaspace (class metadata, native memory), and direct buffers (off-heap I/O). Heap objects need GC, stack is automatic, metaspace grows dynamically.

**Why It Matters** (95 words): Understanding JVM memory regions prevents OutOfMemoryErrors and enables performance tuning. Heap sizing affects GC frequency—too small causes frequent GCs, too large causes long pauses. Stack size impacts thread count (1MB per thread default, 1000 threads = 1GB). Metaspace leaks occur from excessive class loading (hot reload, classloader leaks). Direct buffers speed I/O but consume native memory outside heap limits. Knowing allocation paths (TLAB → Eden → Old) helps diagnose premature promotion. Memory region awareness is fundamental to Java performance engineering.

---

## Example 77: Thread-Local Allocation Buffers (TLAB)

TLAB (Thread-Local Allocation Buffer) is a per-thread allocation area within Eden space, enabling fast lock-free object allocation. Each thread allocates in its own TLAB buffer avoiding synchronization overhead.

**TLAB Allocation Flow**:

```mermaid
graph LR
    New["new Object()"]
    New --> TLAB{"TLAB Space<br/>Available?"}
    TLAB -->|Yes| Fast["Allocate in TLAB<br/>Lock-free, Fast"]
    TLAB -->|No| Refill{"Refill TLAB?"}
    Refill -->|Yes| NewTLAB["Allocate New TLAB<br/>from Eden"]
    Refill -->|No| Slow["Allocate in Shared Eden<br/>Synchronized"]

    style New fill:#0173B2,color:#fff
    style TLAB fill:#DE8F05,color:#fff
    style Fast fill:#029E73,color:#fff
    style Refill fill:#DE8F05,color:#fff
    style NewTLAB fill:#CC78BC,color:#fff
    style Slow fill:#CA9161,color:#000
```

**Code**:

```java
import java.util.concurrent.*;
import java.util.*;

// High allocation rate workload - benefits from TLAB
public class TransactionProcessor {

    // Multi-threaded processing (TLAB optimization)
    public void processBatch(List<Transaction> transactions) {
        ExecutorService executor = Executors.newFixedThreadPool(8);
                                         // => Creates 8 worker threads
                                         // => Each thread gets its own TLAB in Eden
                                         // => TLAB size: typically 1-4KB per thread

        transactions.forEach(tx -> executor.submit(() -> {
            // Each thread allocates in its own TLAB
            TransactionResult result = new TransactionResult();
                                         // => Allocates in thread's TLAB (lock-free)
                                         // => TLAB allocation: bump-the-pointer (fast, ~10 CPU cycles)
                                         // => No synchronization needed (thread-local buffer)
                                         // => Contrast: shared Eden requires CAS/lock (~100 cycles)

            BigDecimal fee = tx.getAmount().multiply(new BigDecimal("0.01"));
                                         // => Creates BigDecimal in TLAB
                                         // => Immutable BigDecimal: new object per operation
                                         // => TLAB absorbs allocation burst (no contention)

            result.setFee(fee);          // => Sets field (no allocation)

            validate(result);            // => May allocate validation errors in TLAB
                                         // => All allocations isolated to thread's TLAB
        }));

        executor.shutdown();             // => Waits for all tasks to complete
    }

    // TLAB performance characteristics
    // => Allocation rate: ~100M objects/sec with TLAB (vs 10M without)
    // => Contention: minimal (each thread has private buffer)
    // => Cache locality: thread's objects in contiguous memory
    // => TLAB refill overhead: only when TLAB exhausted (~1% of allocations)

    // TLAB tuning flags
    // -XX:+UseTLAB                      => Enable TLAB (default: on)
    // -XX:TLABSize=256k                 => Set TLAB size (default: varies by heap)
    // -XX:MinTLABSize=2k                => Minimum TLAB size
    // -XX:TLABWasteTargetPercent=1      => Maximum wasted space in TLAB

    private static class Transaction {
        private BigDecimal amount;       // => Reference to heap object
        public BigDecimal getAmount() { return amount; }
    }

    private static class TransactionResult {
        private BigDecimal fee;          // => Reference to heap object
        public void setFee(BigDecimal f) { this.fee = f; }
    }

    private void validate(TransactionResult r) {
        // Validation logic (may allocate error messages)
    }
}

// TLAB benefits for high-throughput systems
// => Before TLAB: Synchronized Eden allocation (lock contention)
// => After TLAB: Lock-free allocation (10x throughput improvement)
// => Scalability: Linear scaling with thread count (no contention bottleneck)
```

**Key Takeaway**: TLAB (Thread-Local Allocation Buffer) enables lock-free object allocation by giving each thread its own buffer in Eden space. Allocations use bump-the-pointer (fast) instead of synchronized access to shared Eden (slow). TLAB delivers 10x allocation throughput in multi-threaded workloads.

**Why It Matters** (93 words): TLAB is critical for high-throughput multi-threaded applications. Without TLAB, all threads compete for shared Eden space using expensive synchronization (CAS or locks). TLAB eliminates contention by isolating allocation—each thread owns its buffer. This enables near-linear scalability: 8 threads achieve 8x allocation throughput. TLAB also improves cache locality (thread's objects contiguous in memory). The trade-off is minor TLAB waste when buffers aren't fully used (typically <1%). TLAB is enabled by default and rarely needs tuning, but understanding it explains Java's allocation performance.

---

## Example 78: Escape Analysis and Stack Allocation

Escape analysis determines if an object's lifetime is confined to a method. Non-escaping objects can be allocated on the stack (faster) or eliminated entirely (scalar replacement). Escaping objects must be heap-allocated for accessibility beyond method scope.

**Escape Scenarios**:

```mermaid
graph TD
    Object["new Object()"]
    Object --> Analysis["Escape Analysis<br/>by JIT Compiler"]

    Analysis -->|Doesn't Escape| Stack["Stack Allocation<br/>Fast, No GC"]
    Analysis -->|Doesn't Escape| Scalar["Scalar Replacement<br/>Eliminate Object"]
    Analysis -->|Escapes| Heap["Heap Allocation<br/>GC-managed"]

    Heap --> Return["Returns Object"]
    Heap --> Field["Stores in Field"]
    Heap --> Param["Passes to Method"]

    style Object fill:#0173B2,color:#fff
    style Analysis fill:#DE8F05,color:#fff
    style Stack fill:#029E73,color:#fff
    style Scalar fill:#029E73,color:#fff
    style Heap fill:#CC78BC,color:#fff
    style Return fill:#CC78BC,color:#fff
    style Field fill:#CC78BC,color:#fff
    style Param fill:#CC78BC,color:#fff
```

**Code**:

```java
import java.math.BigDecimal;

public class EscapeAnalysisDemo {

    private BigDecimal lastResult;       // => Field: escapes method scope

    // Example 1: No escape - stack allocation possible
    public BigDecimal calculateFee(BigDecimal amount) {
        BigDecimal rate = new BigDecimal("0.01");
                                         // => JIT analyzes: does 'rate' escape?
                                         // => No return, no field assignment, no param passing
                                         // => Compiler may allocate on stack (faster than heap)
                                         // => Or eliminate object entirely (scalar replacement)
                                         // => Stack allocation: no GC overhead

        return amount.multiply(rate);    // => 'rate' doesn't escape (used only here)
                                         // => Multiply creates new BigDecimal (escapes via return)
    }

    // Example 2: Escapes via return
    public BigDecimal createFee(BigDecimal amount) {
        BigDecimal fee = amount.multiply(new BigDecimal("0.01"));
                                         // => JIT analyzes: does 'fee' escape?
                                         // => YES: returned to caller (escapes method)
                                         // => Must allocate on heap (caller needs reference)
                                         // => Stack allocation impossible (lifetime extends beyond method)

        return fee;                      // => Escapes: caller receives reference
    }

    // Example 3: Escapes via field assignment
    public void recordResult(BigDecimal amount) {
        BigDecimal result = amount.multiply(new BigDecimal("2"));
                                         // => JIT analyzes: does 'result' escape?
                                         // => YES: assigned to field (escapes via this.lastResult)
                                         // => Field accessible beyond method (other methods, threads)

        this.lastResult = result;        // => Escapes: stored in heap-allocated field
                                         // => Must use heap allocation (reachable from 'this')
    }

    // Example 4: Escapes via method parameter
    public void addToList(BigDecimal amount, List<BigDecimal> fees) {
        BigDecimal fee = amount.multiply(new BigDecimal("0.01"));
                                         // => JIT analyzes: does 'fee' escape?
                                         // => YES: passed to collection (escapes via List)
                                         // => Collection outlives method (heap storage required)

        fees.add(fee);                   // => Escapes: stored in collection
                                         // => Heap allocation mandatory (collection is heap-allocated)
    }

    // Example 5: Scalar replacement optimization
    public BigDecimal complexCalculation(BigDecimal x, BigDecimal y) {
        // Before optimization: creates Point object on heap
        Point p = new Point(x.intValue(), y.intValue());
                                         // => JIT analyzes: 'p' used only in this method
                                         // => Scalar replacement: replaces object with primitives
                                         // => Optimization: int px = x.intValue(), py = y.intValue()
                                         // => No object allocation! (fields become stack variables)

        return new BigDecimal(p.getX() + p.getY());
                                         // => After scalar replacement: no Point object exists
                                         // => Equivalent to: new BigDecimal(px + py)
    }

    // Enabling escape analysis
    // -XX:+DoEscapeAnalysis              => Enable escape analysis (default: on)
    // -XX:+EliminateAllocations          => Eliminate non-escaping allocations
    // -XX:+EliminateLocks                => Eliminate locks on non-escaping objects

    // Viewing escape analysis decisions
    // -XX:+UnlockDiagnosticVMOptions
    // -XX:+PrintEscapeAnalysis
    // -XX:+PrintEliminateAllocations

    private static class Point {
        private final int x, y;
        Point(int x, int y) { this.x = x; this.y = y; }
        int getX() { return x; }
        int getY() { return y; }
    }
}

// Escape analysis benefits
// => Eliminates allocations for temporary objects (no heap, no GC)
// => Stack allocation faster than heap (bump-the-pointer, no synchronization)
// => Scalar replacement best (no object at all, just primitive variables)
// => Limitations: only works with JIT compilation (not interpreted code)
```

**Key Takeaway**: Escape analysis determines if objects can be stack-allocated or eliminated. Non-escaping objects (not returned, not stored in fields/collections) can avoid heap allocation. JIT compiler performs scalar replacement (object becomes stack variables) or stack allocation (no GC overhead). Escaping objects must use heap.

**Why It Matters** (100 words): Escape analysis is a powerful JIT optimization that eliminates allocation overhead for temporary objects. Instead of heap allocating every object (GC pressure, synchronization cost), the compiler stack-allocates or eliminates non-escaping objects. This dramatically reduces GC overhead—temporary objects in hot paths never reach the GC. For example, a loop creating 1 million temporary Points per second becomes zero allocations via scalar replacement. The optimization is automatic (no code changes needed) but requires understanding escape constraints. Avoid premature escaping (unnecessary returns, field storage) to maximize optimization. Escape analysis makes Java's allocation overhead competitive with manual memory management.

---

## Example 79: Garbage Collection with G1GC

G1GC (Garbage First) is Java's default collector (since Java 9), designed for balanced throughput and predictable pause times. It divides the heap into regions and performs incremental collection, prioritizing high-garbage regions first.

**G1GC Heap Regions**:

```mermaid
graph TD
    Heap["G1 Heap<br/>Divided into Regions"]

    Heap --> Eden["Eden Regions<br/>New Objects"]
    Heap --> Survivor["Survivor Regions<br/>Minor GC Survivors"]
    Heap --> Old["Old Regions<br/>Tenured Objects"]
    Heap --> Humongous["Humongous Regions<br/>Large Objects"]

    Eden -->|Minor GC| Survivor
    Survivor -->|Aging| Old
    Eden -->|Large| Humongous

    style Heap fill:#0173B2,color:#fff
    style Eden fill:#029E73,color:#fff
    style Survivor fill:#029E73,color:#fff
    style Old fill:#DE8F05,color:#fff
    style Humongous fill:#CC78BC,color:#fff
```

**Code**:

```java
// G1GC tuning for different workloads

// Scenario 1: Low-latency transaction processing
// Goal: <50ms GC pauses, consistent response times
// JVM flags:
// java -Xms16g -Xmx16g \              => Heap: 16GB (Xms=Xmx avoids resizing)
//      -XX:+UseG1GC \                 => Enable G1GC (default Java 9+)
//      -XX:MaxGCPauseMillis=50 \      => Target max pause: 50ms (SLA requirement)
//      -XX:G1HeapRegionSize=8m \      => Region size: 8MB (16GB heap / 2048 regions)
//      -XX:InitiatingHeapOccupancyPercent=35 \
//                                     => Start concurrent marking at 35% heap occupancy
//                                     => Earlier start → more frequent concurrent cycles
//                                     => Prevents heap exhaustion (old gen growth)
//      -XX:G1NewSizePercent=40 \      => Young gen: 40% of heap (6.4GB)
//      -XX:G1MaxNewSizePercent=60 \   => Max young gen: 60% (9.6GB)
//      -Xlog:gc*:file=gc-tx.log \     => GC logging to file (rotation recommended)
//      TransactionProcessor

// => Region structure with 8MB regions, 16GB heap:
// => Total regions: 2048 (16GB / 8MB)
// => Eden regions: ~800 (40% young gen, ~6.4GB)
// => Survivor regions: ~50 (internal ratio ~8:1:1)
// => Old regions: ~1150 (remaining heap after young gen)
// => Humongous regions: objects >4MB (50% of region size)

// Scenario 2: High-throughput batch processing
// Goal: Maximize throughput, tolerate longer pauses
// JVM flags:
// java -Xms32g -Xmx32g \              => Larger heap for batch processing (32GB)
//      -XX:+UseG1GC \                 => G1GC balances throughput and latency
//      -XX:MaxGCPauseMillis=500 \     => Allow 500ms pauses (throughput priority)
//                                     => Longer pauses → fewer GC cycles → higher throughput
//      -XX:G1HeapRegionSize=32m \     => Larger regions (32GB / 1024 = 32MB regions)
//                                     => Fewer regions → less GC overhead
//      -XX:InitiatingHeapOccupancyPercent=50 \
//                                     => Concurrent marking at 50% (later start)
//                                     => Fewer concurrent cycles → more CPU for application
//      -Xlog:gc*:file=gc-batch.log \  => GC logging
//      BatchProcessor

// => Region structure with 32MB regions, 32GB heap:
// => Total regions: 1024 (32GB / 32MB)
// => Eden regions: ~400 (young gen sized by G1)
// => Old regions: ~600 (remaining after young gen)
// => Humongous objects: >16MB (50% of 32MB region size)

// G1GC phases explained
// 1. Young-Only GC (Evacuation Pause):
//    => Stop-the-world event
//    => Collects Eden regions (copies live objects to survivor/old)
//    => Fast: typically 10-50ms (tuned by MaxGCPauseMillis)
//    => Frequency: continuous (Eden fills frequently)

// 2. Concurrent Marking Cycle:
//    => Triggered when heap occupancy > InitiatingHeapOccupancyPercent
//    => Marks live objects in old generation (concurrent with application)
//    => Phases:
//      a. Initial Mark (STW, <1ms) - marks roots
//      b. Concurrent Mark - traces object graph (parallel with app)
//      c. Remark (STW, 1-10ms) - finalizes marking
//      d. Cleanup (STW, <1ms) - identifies empty regions

// 3. Mixed GC:
//    => Follows concurrent marking
//    => Collects young regions + old regions with most garbage
//    => "Garbage First" algorithm: prioritizes high-garbage regions
//    => Multiple mixed GCs until heap occupancy decreases

// 4. Full GC (fallback, avoid):
//    => Stop-the-world compaction (expensive, ~1-5 seconds)
//    => Triggered when heap exhausted (allocation failure)
//    => Indicates tuning problem: increase heap or adjust IHOP

// Monitoring G1GC performance
import java.lang.management.*;

public class G1Monitoring {
    public static void main(String[] args) {
        List<GarbageCollectorMXBean> gcBeans = ManagementFactory.getGarbageCollectorMXBeans();
                                         // => Get GC MXBeans (JMX monitoring)

        for (GarbageCollectorMXBean gcBean : gcBeans) {
            System.out.println("GC Name: " + gcBean.getName());
                                         // => "G1 Young Generation" or "G1 Old Generation"
            System.out.println("Collection Count: " + gcBean.getCollectionCount());
                                         // => Total GC count since JVM start
            System.out.println("Collection Time: " + gcBean.getCollectionTime() + "ms");
                                         // => Total GC time (sum of all pauses)
                                         // => Throughput: (totalTime - gcTime) / totalTime
        }
    }
}
```

**Key Takeaway**: G1GC divides heap into regions (1-32MB), collecting high-garbage regions first. Tune with MaxGCPauseMillis (target pause time), G1HeapRegionSize (region size), and InitiatingHeapOccupancyPercent (concurrent marking threshold). Low latency: small regions, low IHOP. High throughput: large regions, high IHOP, longer pauses.

**Why It Matters** (98 words): G1GC enables predictable GC pauses for large heaps (4GB+). Unlike CMS (deprecated), G1 compacts incrementally, avoiding fragmentation. The region-based design allows incremental collection—collect 10% of heap per cycle instead of entire heap. This makes sub-100ms pauses achievable even with 64GB heaps. Tuning MaxGCPauseMillis guides G1's effort (more regions per cycle = shorter pauses but more frequent). Understanding IHOP (Initiating Heap Occupancy Percent) prevents allocation failures (start concurrent marking early enough). G1GC is the best default for most workloads—balance of throughput and latency without manual tuning.

---

## Example 80: Ultra-Low Latency with ZGC

ZGC is a scalable low-latency garbage collector designed for heaps up to 16TB with pause times under 10ms. It performs most GC work concurrently using colored pointers and load barriers, achieving sub-millisecond pauses even during heap compaction.

**ZGC Concurrent Operations**:

```mermaid
graph TD
    App["Application Running"]

    App -->|Concurrent| Mark["Concurrent Marking<br/>Trace Object Graph"]
    Mark -->|Concurrent| Relocate["Concurrent Relocation<br/>Move Objects"]
    Relocate -->|Concurrent| Remap["Concurrent Remapping<br/>Update References"]

    Mark -->|STW <1ms| MarkStart["Pause Mark Start"]
    Mark -->|STW <1ms| MarkEnd["Pause Mark End"]
    Relocate -->|STW <1ms| RelocStart["Pause Relocate Start"]

    style App fill:#0173B2,color:#fff
    style Mark fill:#029E73,color:#fff
    style Relocate fill:#029E73,color:#fff
    style Remap fill:#029E73,color:#fff
    style MarkStart fill:#CC78BC,color:#fff
    style MarkEnd fill:#CC78BC,color:#fff
    style RelocStart fill:#CC78BC,color:#fff
```

**Code**:

```java
// ZGC for ultra-low latency payment processing
// Goal: <10ms GC pauses, 99.99% uptime SLA

// JVM flags (ZGC production configuration):
// java -Xms32g -Xmx32g \              => Fixed heap: 32GB (no resizing overhead)
//      -XX:+UseZGC \                  => Enable ZGC (production-ready Java 15+)
//      -XX:ConcGCThreads=8 \          => Concurrent GC threads (typically cores/2)
//                                     => More threads → faster concurrent phases
//                                     => Trade-off: steals CPU from application
//      -XX:ZCollectionInterval=10 \   => Min seconds between GC cycles
//                                     => Prevents back-to-back GCs (rate limiting)
//      -XX:ZAllocationSpikeTolerance=2 \
//                                     => Allocation spike tolerance multiplier
//                                     => Handles 2x normal allocation rate bursts
//      -Xlog:gc*:file=gc-zgc.log \    => GC logging with timestamps
//      PaymentGateway

// ZGC phases (all mostly concurrent, <1ms STW)
// 1. Pause Mark Start (STW, <1ms)
//    => Scans thread stacks and roots
//    => Very fast: only roots, not full heap traversal

// 2. Concurrent Mark
//    => Traces object graph in parallel with application
//    => Uses load barriers: app reads trigger marking on access
//    => Colored pointers encode metadata (3 bits: marked, remapped, finalizable)
//    => No stop-the-world heap scan

// 3. Pause Mark End (STW, <1ms)
//    => Finalizes marking, handles weak references
//    => Identifies garbage regions

// 4. Concurrent Prepare for Relocation
//    => Selects regions to compact
//    => Builds forwarding tables

// 5. Pause Relocate Start (STW, <1ms)
//    => Relocates roots
//    => Very fast: only roots, not object graph

// 6. Concurrent Relocate
//    => Moves live objects to new locations (compaction)
//    => Runs parallel with application (load barriers handle references)
//    => Updates references during relocation (no separate remap phase needed in Gen ZGC)

// ZGC colored pointers (64-bit only, requires compressedOops=false)
// Pointer structure (Linux x86-64):
// [63:48] Unused (16 bits)
// [47:46] Metadata (2 bits: marked0, marked1)
// [45:0]  Object address (46 bits = 64TB addressable)

// => Colored pointers encode GC state in pointer itself
// => Load barriers check pointer metadata on every object access
// => Cost: ~2-4% throughput overhead for load barriers
// => Benefit: Sub-millisecond pauses even during compaction

public class PaymentGateway {

    // High-frequency payment processing (low latency critical)
    public PaymentResult processPayment(Payment payment) {
        // ZGC enables consistent latency:
        // => No GC pauses during payment processing
        // => Application continues running during ZGC cycles
        // => Load barriers: slight overhead (~2-4%) but no pauses

        PaymentResult result = new PaymentResult();
                                         // => Allocation handled by ZGC
                                         // => No allocation stalls (ZGC pages always available)

        result.setStatus(validate(payment));
                                         // => Validation allocates error messages
                                         // => ZGC allocates concurrently (no pause)

        return result;                   // => Consistent sub-5ms latency
                                         // => No latency spikes from GC pauses
    }

    // Generational ZGC (Java 21+) - recommended
    // java -XX:+UseZGC \
    //      -XX:+ZGenerational \          => Enable generational ZGC
    //      -Xms64g -Xmx64g \             => Larger heap for generational benefits
    //      -XX:ConcGCThreads=16 \        => More threads for large heap
    //      PaymentGateway

    // => Generational ZGC advantages:
    // => - Higher throughput (10-25% improvement over non-generational)
    // => - Lower CPU usage (less concurrent work, focuses on young gen)
    // => - Better allocation rate handling (young gen collects frequently)
    // => - Exploits generational hypothesis (most objects die young)

    private PaymentStatus validate(Payment payment) {
        // Validation logic (may allocate error messages)
        return PaymentStatus.SUCCESS;
    }

    private static class Payment { }
    private static class PaymentResult {
        private PaymentStatus status;
        void setStatus(PaymentStatus s) { this.status = s; }
    }
    private enum PaymentStatus { SUCCESS, FAILURE }
}

// ZGC performance characteristics
// Before ZGC (G1GC, 32GB heap):
// - Average GC pause: 150ms
// - P99 GC pause: 400ms
// - Max observed pause: 1200ms
// - Throughput: 100%

// After ZGC (32GB heap):
// - Average GC pause: 2ms (75x better)
// - P99 GC pause: 5ms (80x better)
// - Max observed pause: 8ms (150x better)
// - Throughput: 96-98% (load barrier overhead)

// ZGC trade-offs
// => Lower throughput: 2-5% overhead from load barriers
// => Memory overhead: colored pointers, forwarding tables
// => 64-bit only: requires 64-bit JVM (no 32-bit support)
// => Heap size: works best with >8GB heaps (overhead amortized)

// When to use ZGC
// => Latency-critical applications (trading, real-time systems)
// => Large heaps (>16GB) where G1GC pauses too long
// => 99.99% SLA requirements (<10ms response times)
// => Avoid for: small heaps (<4GB), throughput-critical batch jobs
```

**Key Takeaway**: ZGC achieves sub-10ms pauses for heaps up to 16TB using concurrent marking, relocation, and remapping. Colored pointers encode GC metadata in references. Load barriers enable concurrent operations. Generational ZGC (Java 21+) adds 10-25% throughput by exploiting generational hypothesis. Trade-off: 2-5% throughput overhead for ultra-low latency.

**Why It Matters** (100 words): ZGC enables ultra-low latency for large heaps. Traditional collectors (even G1GC) struggle with >32GB heaps—pauses grow with heap size. ZGC breaks this relationship: pauses stay sub-10ms regardless of heap size (tested to 16TB). This makes Java viable for latency-critical systems (trading platforms, real-time bidding, gaming servers). Colored pointers and load barriers are the key: GC state encoded in pointers themselves, checked on every object access. The 2-5% throughput cost is worth it for predictable latency. Generational ZGC (Java 21+) recovers most throughput while keeping low latency. ZGC is the future of Java GC.

---

## Example 81: Memory Leak Detection and Prevention

Memory leaks occur when objects remain reachable but unused, preventing garbage collection. Common causes: unclosed resources, static collections, listener leaks, and ThreadLocal leaks. Detecting leaks requires heap dump analysis and profiling.

**Code**:

```java
import java.util.*;
import java.util.concurrent.*;
import java.lang.ref.WeakReference;

// LEAK PATTERN 1: Unclosed resources
class ResourceLeak {
    // ❌ BAD: Connection not closed
    public void processTransactions(List<Transaction> txs) throws Exception {
        Connection conn = dataSource.getConnection();
                                         // => Connection allocated from pool
                                         // => Must be returned to pool (conn.close())

        for (Transaction tx : txs) {
            processTransaction(conn, tx);
        }

        // MISSING: conn.close()
        // => Connection never returned to pool
        // => Pool exhausted after N calls (where N = pool size)
        // => Subsequent calls block waiting for available connection
    }

    // ✅ GOOD: Try-with-resources guarantees cleanup
    public void processTransactionsFixed(List<Transaction> txs) throws Exception {
        try (Connection conn = dataSource.getConnection()) {
                                         // => AutoCloseable: conn.close() called automatically
                                         // => Executed even if exception thrown
                                         // => Finally block implicit (JVM-generated)

            for (Transaction tx : txs) {
                processTransaction(conn, tx);
            }

        }                                // => Automatic conn.close() here
                                         // => Connection returned to pool
                                         // => No leak even on exception
    }

    // Supporting types
    private DataSource dataSource;
    private static class Transaction { }
    private interface Connection extends AutoCloseable { }
    private interface DataSource {
        Connection getConnection() throws Exception;
    }
    private void processTransaction(Connection c, Transaction t) { }
}

// LEAK PATTERN 2: Unbounded static collections
class StaticCacheLeak {
    // ❌ BAD: Static cache grows unbounded
    private static final Map<String, Transaction> cache = new HashMap<>();
                                         // => Static: survives JVM lifetime (never GC'd)
                                         // => HashMap: no eviction policy (grows forever)

    public static void cacheTransaction(Transaction tx) {
        cache.put(tx.getId(), tx);       // => Adds entry, never removed
                                         // => After 1M transactions: ~100MB retained
                                         // => After 10M transactions: ~1GB retained
                                         // => Eventually: OutOfMemoryError
    }

    // ✅ GOOD: Bounded cache with eviction
    private static final LoadingCache<String, Transaction> cacheFixed =
        CacheBuilder.newBuilder()
            .maximumSize(10_000)         // => Max 10k entries (LRU eviction)
                                         // => Evicts least-recently-used when full
                                         // => Memory bounded: ~10MB max
            .expireAfterWrite(1, TimeUnit.HOURS)
                                         // => Time-based eviction (1 hour TTL)
                                         // => Prevents stale data accumulation
            .build(CacheLoader.from(StaticCacheLeak::loadTransaction));

    // Supporting methods
    private static class Transaction {
        String getId() { return "tx-123"; }
    }
    private static Transaction loadTransaction(String id) {
        return new Transaction();
    }
}

// LEAK PATTERN 3: Listener leaks
class ListenerLeak {
    private final List<EventListener> listeners = new ArrayList<>();
                                         // => Strong references to listeners
                                         // => Listeners never removed

    // ❌ BAD: Listener registration without removal
    public void addListener(EventListener listener) {
        listeners.add(listener);         // => Adds listener, never removed
                                         // => If listener is UI component:
                                         // => UI closed but listener still registered
                                         // => Entire UI object retained (leak)
    }

    // ✅ GOOD: Weak references for listeners
    private final List<WeakReference<EventListener>> listenersFixed =
        new CopyOnWriteArrayList<>();   // => Thread-safe list

    public void addListenerFixed(EventListener listener) {
        listenersFixed.add(new WeakReference<>(listener));
                                         // => WeakReference: doesn't prevent GC
                                         // => If listener unreachable elsewhere, it's GC'd
                                         // => Weak reference becomes null after GC
    }

    public void fireEvent(Event event) {
        Iterator<WeakReference<EventListener>> it = listenersFixed.iterator();
        while (it.hasNext()) {
            WeakReference<EventListener> ref = it.next();
            EventListener listener = ref.get();
                                         // => get() returns null if GC'd

            if (listener == null) {
                it.remove();             // => Cleanup: remove dead reference
            } else {
                listener.onEvent(event); // => Invoke live listener
            }
        }
    }

    // Supporting types
    private interface EventListener {
        void onEvent(Event e);
    }
    private static class Event { }
}

// LEAK PATTERN 4: ThreadLocal leaks in thread pools
class ThreadLocalLeak {
    private static final ThreadLocal<User> currentUser = new ThreadLocal<>();
                                         // => Thread-local storage
                                         // => Each thread has its own User instance

    // ❌ BAD: ThreadLocal not cleaned in thread pool
    public void handleRequest(HttpRequest request) {
        User user = extractUser(request);
        currentUser.set(user);           // => Sets User in thread-local
                                         // => User stored in thread's ThreadLocalMap

        processRequest(request);

        // MISSING: currentUser.remove()
        // => In thread pool: thread reused for next request
        // => Old User retained (leaked until thread dies)
        // => 1000 threads × 1KB User = 1MB leak per request batch
    }

    // ✅ GOOD: Explicit ThreadLocal cleanup
    public void handleRequestFixed(HttpRequest request) {
        try {
            User user = extractUser(request);
            currentUser.set(user);       // => Sets User in thread-local

            processRequest(request);

        } finally {
            currentUser.remove();        // => Critical: cleanup ThreadLocal
                                         // => Removes entry from ThreadLocalMap
                                         // => Prevents leak in thread pool
        }
    }

    // Supporting types
    private static class User { }
    private static class HttpRequest { }
    private User extractUser(HttpRequest r) { return new User(); }
    private void processRequest(HttpRequest r) { }
}

// Detecting memory leaks with heap dumps
class LeakDetection {

    // Enable heap dump on OutOfMemoryError
    // JVM flags: -XX:+HeapDumpOnOutOfMemoryError
    //            -XX:HeapDumpPath=/var/dumps/heap.hprof
    // => Automatically dumps heap when OOM occurs
    // => Analyze with Eclipse Memory Analyzer (MAT)

    // Manual heap dump for leak detection
    // Command: jmap -dump:live,format=b,file=heap.hprof <pid>
    // => Captures heap snapshot (live objects only)
    // => Compare two dumps: before/after leak operation
    // => Objects that grew significantly = leak candidates

    // Analyzing with Eclipse MAT:
    // 1. Open heap dump in MAT
    // 2. Run "Leak Suspects Report"
    // 3. Examine dominator tree (shows retained size)
    // 4. Identify large collections (HashMap, ArrayList with many entries)
    // 5. Trace references to GC roots (path to leak)

    // Example MAT report:
    // Problem Suspect 1:
    //   One instance of "StaticCacheLeak" occupies 1.2GB (75% of heap)
    //   Referenced by: StaticCacheLeak.cache (static field)
    //   Contains: 10 million Transaction objects
    //   Recommendation: Implement cache eviction
}

// Memory leak prevention checklist
// ✅ Use try-with-resources for all AutoCloseable resources
// ✅ Bound static collections with eviction policies
// ✅ Remove listeners when no longer needed (or use WeakReference)
// ✅ Clear ThreadLocal variables in finally blocks
// ✅ Avoid finalizers (use AutoCloseable instead)
// ✅ Test with heap dumps before/after operations
// ✅ Monitor heap growth in production (alerts on sustained growth)
```

**Key Takeaway**: Memory leaks prevent GC by keeping objects reachable but unused. Common causes: unclosed resources (missing try-with-resources), unbounded static collections (no eviction), listener leaks (strong references), ThreadLocal leaks (thread pool reuse). Detect with heap dumps (jmap, MAT). Prevent with cleanup (remove(), close(), weak references).

**Why It Matters** (100 words): Memory leaks are insidious—they accumulate slowly, causing OutOfMemoryErrors hours or days after deployment. Unlike native languages (missing free() = immediate crash), Java leaks are subtle (objects reachable but unused). A 1KB leak per request becomes 1GB after 1 million requests. Thread pools amplify leaks (threads reused, ThreadLocal persists). Static collections are leak magnets (eternal lifetime). Prevention requires discipline: always close resources (try-with-resources), bound caches (eviction), remove listeners (WeakReference), clear ThreadLocal (finally). Heap dump analysis is essential—compare before/after snapshots to identify accumulation points. Memory leaks are preventable but require vigilance.

---

## Example 82: Object Pooling for High-Throughput Systems

Object pooling reuses expensive-to-create objects instead of allocating new instances. Pooling reduces allocation rate and GC pressure but adds complexity (state reset, synchronization). Use for expensive objects (database connections, threads) or extreme allocation rates (millions/second).

**Object Pool Architecture**:

```mermaid
graph TD
    App["Application"]

    App -->|1. Acquire| Pool["Object Pool<br/>Idle Objects"]
    Pool -->|2. Borrow| Obj["Pooled Object<br/>In Use"]
    Obj -->|3. Use| App2["Process Data"]
    App2 -->|4. Return| Pool

    Pool -->|Empty?| Create["Create New<br/>Object"]
    Create --> Pool

    style App fill:#0173B2,color:#fff
    style Pool fill:#DE8F05,color:#fff
    style Obj fill:#029E73,color:#fff
    style App2 fill:#CC78BC,color:#fff
    style Create fill:#CA9161,color:#000
```

**Code**:

```java
import java.util.concurrent.*;
import com.zaxxer.hikari.*;

// Connection pooling (most common use case)
class DatabaseConnectionPool {

    // HikariCP - high-performance connection pool
    public DataSource createDataSource() {
        HikariConfig config = new HikariConfig();

        config.setJdbcUrl("jdbc:postgresql://localhost:5432/finance");
        config.setUsername("app");
        config.setPassword("secret");

        // Pool sizing (critical for performance)
        config.setMaximumPoolSize(20);   // => Max 20 connections
                                         // => Formula: connections = (cores * 2) + disk_spindles
                                         // => 8 cores + 4 disks = 20 connections
                                         // => Too many: wasted memory, connection overhead
                                         // => Too few: contention, blocking threads

        config.setMinimumIdle(5);        // => Min 5 idle connections
                                         // => Always ready for requests (no create latency)
                                         // => Trade-off: memory vs latency

        config.setConnectionTimeout(30000);
                                         // => Max 30s wait for connection
                                         // => Blocks if pool exhausted
                                         // => Throws SQLException if timeout exceeded

        config.setIdleTimeout(600000);   // => Idle connections closed after 10min
                                         // => Reduces connections during low load
                                         // => Respects minimumIdle (never below 5)

        config.setMaxLifetime(1800000);  // => Max connection lifetime: 30min
                                         // => Prevents stale connections (DB timeout issues)
                                         // => Connections cycled periodically

        return new HikariDataSource(config);
                                         // => Creates connection pool
                                         // => Pre-allocates minimumIdle connections
                                         // => Thread-safe: handles concurrent access
    }

    // Using pooled connections
    public void processTransactions(DataSource ds, List<Transaction> txs) throws Exception {
        try (Connection conn = ds.getConnection()) {
                                         // => Acquires connection from pool (blocking if full)
                                         // => Fast if idle connection available (~1μs)
                                         // => Slow if must create new connection (~100ms)

            PreparedStatement stmt = conn.prepareStatement(
                "INSERT INTO transactions (id, amount) VALUES (?, ?)"
            );                           // => Prepares SQL statement (reusable)

            for (Transaction tx : txs) {
                stmt.setString(1, tx.getId());
                stmt.setBigDecimal(2, tx.getAmount());
                stmt.executeUpdate();    // => Executes insert
            }

        }                                // => Auto-close returns connection to pool
                                         // => Connection reset (autocommit, isolation level)
                                         // => Now available for next request
    }
}

// Thread pooling (ExecutorService)
class ThreadPooling {

    // Thread pool for CPU-bound tasks
    public ExecutorService createCPUBoundPool() {
        int processors = Runtime.getRuntime().availableProcessors();
                                         // => Get CPU core count (e.g., 8 cores)

        return Executors.newFixedThreadPool(processors + 1);
                                         // => Fixed pool: processors + 1 threads
                                         // => +1 for thread blocked on I/O
                                         // => CPU-bound formula: cores + 1
                                         // => Maximizes CPU utilization without oversubscription
    }

    // Thread pool for I/O-bound tasks
    public ExecutorService createIOBoundPool() {
        int processors = Runtime.getRuntime().availableProcessors();

        return Executors.newFixedThreadPool(processors * 2);
                                         // => I/O-bound formula: cores * 2
                                         // => More threads compensate for blocking I/O
                                         // => Threads block on network/disk: CPU idle
                                         // => 2x threads keep CPU busy while others block
    }

    // Custom thread pool with tuning
    public ExecutorService createCustomPool() {
        return new ThreadPoolExecutor(
            5,                           // => Core pool size: always alive
            20,                          // => Max pool size: grows under load
            60L, TimeUnit.SECONDS,       // => Idle thread timeout: 60s
            new LinkedBlockingQueue<>(100),
                                         // => Work queue: 100 tasks buffer
                                         // => Bounded queue prevents memory exhaustion
            new ThreadPoolExecutor.CallerRunsPolicy()
                                         // => Rejection policy: caller executes task
                                         // => Back-pressure: slows producer (submitter)
        );
    }
}

// Custom object pooling (advanced use case)
import org.apache.commons.pool2.*;
import org.apache.commons.pool2.impl.*;

class ValidationResultPool {

    // Object factory (creates pooled objects)
    static class ValidationResultFactory extends BasePooledObjectFactory<ValidationResult> {

        @Override
        public ValidationResult create() {
                                         // => Called when pool needs new object
                                         // => Expensive creation (database lookup, initialization)
            return new ValidationResult();
        }

        @Override
        public PooledObject<ValidationResult> wrap(ValidationResult obj) {
                                         // => Wraps object for pool management
            return new DefaultPooledObject<>(obj);
        }

        @Override
        public void passivateObject(PooledObject<ValidationResult> p) {
            p.getObject().clear();       // => Reset state before returning to pool
                                         // => Critical: clear errors, flags, cached data
        }
    }

    // Using object pool
    public void validateBatch(List<Payment> payments) throws Exception {
        // Create pool with configuration
        ObjectPool<ValidationResult> pool = new GenericObjectPool<>(
            new ValidationResultFactory(),
            createPoolConfig()
        );

        for (Payment payment : payments) {
            ValidationResult result = null;
            try {
                result = pool.borrowObject();
                                         // => Acquires object from pool (blocking if empty)
                                         // => Reuses existing object (no allocation)

                result.validate(payment);
                                         // => Uses pooled object

                if (!result.isValid()) {
                    System.out.println("Validation failed: " + result.getErrors());
                }

            } finally {
                if (result != null) {
                    pool.returnObject(result);
                                         // => Returns object to pool
                                         // => passivateObject() called (clear state)
                }
            }
        }

        pool.close();                    // => Cleanup: close pool
    }

    private GenericObjectPoolConfig<ValidationResult> createPoolConfig() {
        GenericObjectPoolConfig<ValidationResult> config = new GenericObjectPoolConfig<>();
        config.setMaxTotal(100);         // => Max 100 objects in pool
        config.setMaxIdle(20);           // => Max 20 idle objects
        config.setMinIdle(5);            // => Min 5 idle objects (pre-created)
        config.setBlockWhenExhausted(true);
                                         // => Block if pool exhausted (vs throw exception)
        config.setMaxWaitMillis(5000);   // => Max 5s wait for object
        return config;
    }
}

// Pooling performance impact
// Before pooling (1M validations):
// - Allocation rate: 1M ValidationResult objects/sec
// - GC overhead: 15% CPU time
// - Throughput: 50,000 validations/sec

// After pooling (pool size 100):
// - Allocation rate: 100 objects total (99.99% reduction)
// - GC overhead: 0.5% CPU time (97% reduction)
// - Throughput: 65,000 validations/sec (30% improvement)

// When to use object pooling
// ✅ Object creation expensive (database connections, threads, crypto contexts)
// ✅ High allocation rate (millions of objects per second)
// ✅ Object size significant (large buffers, complex initialization)
// ❌ Modern GC handles allocation efficiently (don't pool small objects)
// ❌ Complexity outweighs benefits (state reset, synchronization overhead)
// ❌ Objects are immutable (can't reset state, defeats purpose)
```

**Key Takeaway**: Object pooling reuses expensive objects (connections, threads, buffers) to reduce allocation rate and GC pressure. HikariCP for database connections, ExecutorService for threads, Apache Commons Pool for custom objects. Configure pool size (max, min idle), timeout, and eviction. Trade-off: complexity (state reset, synchronization) vs performance (lower GC overhead).

**Why It Matters** (98 words): Pooling is essential for high-throughput systems where allocation becomes a bottleneck. Database connections are the canonical example—creating a connection takes ~100ms (SSL handshake, authentication), but pooling reduces this to ~1μs (acquire from pool). Thread pooling prevents thread creation overhead (~1ms per thread, thousands of threads = seconds). Custom pooling helps with extreme allocation rates (1M+ objects/sec). However, modern GCs (G1, ZGC) handle allocation efficiently—pooling small objects often hurts performance (synchronization cost > allocation cost). Profile before pooling. Use proven libraries (HikariCP, Executors) rather than custom pools.

---

## Example 83: JVM Profiling with Java Flight Recorder (JFR)

Java Flight Recorder (JFR) is a low-overhead profiling framework built into the JVM. It records runtime events (allocations, GC, locks, I/O) with minimal performance impact (<1% overhead), making it safe for production use.

**Code**:

```java
// Enabling JFR at JVM startup
// JVM flags:
// java -XX:StartFlightRecording=duration=60s,filename=recording.jfr \
//      -XX:FlightRecorderOptions=stackdepth=128 \
//      MyApp

// => duration=60s: Record for 60 seconds then stop
// => filename=recording.jfr: Output file path
// => stackdepth=128: Capture 128 stack frames (default: 64)
// => Overhead: <1% CPU, <1% memory (safe for production)

// Continuous recording (circular buffer)
// java -XX:StartFlightRecording=maxage=6h,maxsize=500M,filename=recording.jfr \
//      MyApp

// => maxage=6h: Keep last 6 hours of events (circular buffer)
// => maxsize=500M: Max recording size 500MB (oldest events discarded)
// => Always-on recording: capture issues as they happen

// Starting JFR on running JVM (no restart)
// Command: jcmd <pid> JFR.start duration=60s filename=recording.jfr
// => Attaches to running process (PID)
// => Starts recording for 60 seconds
// => No JVM restart required (dynamic profiling)

// Stopping JFR recording
// Command: jcmd <pid> JFR.stop filename=recording.jfr
// => Stops ongoing recording
// => Writes events to file

// Dumping current recording without stopping
// Command: jcmd <pid> JFR.dump filename=snapshot.jfr
// => Captures snapshot of current recording
// => Recording continues (non-destructive)

import jdk.jfr.*;
import java.nio.file.*;

// Programmatic JFR control (Java 14+)
class JFRProgrammatic {

    public void profileOperation() throws Exception {
        // Start recording programmatically
        Configuration config = Configuration.getConfiguration("profile");
                                         // => "profile" preset: detailed events
                                         // => Alternative: "default" (lower overhead)

        Recording recording = new Recording(config);
                                         // => Creates new recording with config

        recording.setMaxAge(Duration.ofHours(1));
                                         // => Keep last 1 hour of events
        recording.setMaxSize(100 * 1024 * 1024);
                                         // => Max 100MB recording size

        recording.start();               // => Start recording events
                                         // => JFR begins capturing: GC, allocations, locks, I/O

        try {
            // Run workload to profile
            processLargeDataset();       // => All events recorded: allocations, GC pauses, thread activity

        } finally {
            recording.stop();            // => Stop recording

            Path destination = Paths.get("recording-" + System.currentTimeMillis() + ".jfr");
            recording.dump(destination); // => Write recording to file
                                         // => File contains all captured events

            recording.close();           // => Cleanup: close recording
        }
    }

    private void processLargeDataset() {
        // Workload to profile
    }
}

// Analyzing JFR recordings with JDK Mission Control (JMC)
// GUI tool: jmc recording.jfr
// => Opens visual analysis tool
// => Tabs: Overview, Memory, Threads, I/O, Events

// Key JFR events for memory analysis:
// 1. jdk.ObjectAllocationInNewTLAB
//    => Objects allocated in TLAB (fast path)
//    => Shows allocation hotspots (classes, stack traces)

// 2. jdk.ObjectAllocationOutsideTLAB
//    => Large objects allocated outside TLAB (slow path)
//    => Indicates humongous objects (>50% region size in G1)

// 3. jdk.GarbageCollection
//    => GC events: duration, heap before/after, GC cause
//    => Identifies GC frequency and pause times

// 4. jdk.OldGarbageCollection
//    => Old generation GCs (expensive, focus here)

// 5. jdk.PromoteObjectInNewPLAB
//    => Objects promoted to old generation
//    => High promotion rate indicates premature promotion

// Analyzing allocation hotspots
class AllocationAnalysis {

    // Example: Finding allocation hotspots with JFR
    // 1. Record JFR: jcmd <pid> JFR.start duration=60s filename=alloc.jfr
    // 2. Run workload under load
    // 3. Open in JMC: jmc alloc.jfr
    // 4. Navigate: Memory → Allocations
    // 5. Sort by "Total Allocation" (descending)

    // Example JFR allocation report:
    // Top Allocation Sites:
    // 1. BigDecimal.<init>(String)                    - 1.2GB (35% of allocations)
    //    Stack trace: Account.calculateFee() → BigDecimal.<init>()
    //    => Optimization: Cache common BigDecimal values

    // 2. HashMap.resize()                             - 500MB (15% of allocations)
    //    Stack trace: TransactionCache.put() → HashMap.resize()
    //    => Optimization: Pre-size HashMap with initial capacity

    // 3. String.substring()                           - 200MB (6% of allocations)
    //    Stack trace: Parser.extractId() → String.substring()
    //    => Optimization: Reuse String, avoid unnecessary substring()
}

// JFR custom events (advanced)
@Name("com.example.PaymentProcessed")
@Label("Payment Processed")
@Description("Triggered when payment is processed")
@Category("Application")
class PaymentProcessedEvent extends Event {
    @Label("Payment ID")
    String paymentId;

    @Label("Amount")
    long amount;

    @Label("Processing Time")
    @Timespan(Timespan.MILLISECONDS)
    long processingTime;
}

class CustomEventExample {

    public void processPayment(String id, long amount) {
        long start = System.currentTimeMillis();

        // Process payment...

        long duration = System.currentTimeMillis() - start;

        // Record custom event
        PaymentProcessedEvent event = new PaymentProcessedEvent();
        event.paymentId = id;            // => Set event fields
        event.amount = amount;
        event.processingTime = duration;

        event.commit();                  // => Commit event to JFR recording
                                         // => Appears in JMC event browser
    }
}

// JFR vs other profilers
// JFR advantages:
// - Low overhead (<1%, safe for production)
// - Built into JVM (no agent required)
// - Rich event set (GC, allocations, locks, I/O, CPU)
// - Continuous recording (circular buffer, always on)

// JFR limitations:
// - Sampling profiler (not every allocation captured)
// - GUI required (JMC) for best analysis
// - JFR files can be large (100MB+ for long recordings)

// When to use JFR
// ✅ Production profiling (low overhead, always-on)
// ✅ Memory allocation analysis (hotspots, leak suspects)
// ✅ GC behavior analysis (pause times, frequency, heap usage)
// ✅ Latency investigation (blocking events, lock contention)
```

**Key Takeaway**: JFR (Java Flight Recorder) is a low-overhead (<1%) profiling framework for production use. It records GC events, allocations, locks, and I/O. Enable with -XX:StartFlightRecording or jcmd on running JVMs. Analyze with JMC (JDK Mission Control) to find allocation hotspots, GC issues, and performance bottlenecks. Custom events extend JFR.

**Why It Matters** (100 words): JFR enables continuous production profiling without performance penalties. Traditional profilers (VisualVM, YourKit) add 10-50% overhead, making them unsuitable for production. JFR's <1% overhead allows always-on recording—capture issues as they happen (intermittent GC pauses, allocation spikes). The circular buffer (maxage, maxsize) keeps recent history without unbounded growth. JFR's event-based architecture records low-level JVM events (TLAB allocations, GC phases, lock acquisitions) invisible to other profilers. Analyzing JFR recordings in JMC reveals allocation hotspots (35% from BigDecimal constructor), premature promotion (survivors → old), and GC tuning opportunities. JFR is essential for diagnosing production memory issues.

---

## Example 84: Advanced Memory Optimization Techniques

Modern Java offers advanced memory optimization features: compact object headers (Java 25+), value types (Project Valhalla), and manual memory management (Foreign Function & Memory API). These techniques reduce memory footprint and improve performance for memory-intensive workloads.

**Code**:

```java
// TECHNIQUE 1: Compact Object Headers (Java 25+)
// Traditional object header: 16 bytes (128 bits)
// - Mark word (64 bits): hash code, GC age, lock state
// - Class pointer (64 bits): reference to class metadata

// Compact object header: 8 bytes (64 bits)
// - Combined mark + class pointer (64 bits)
// - Memory savings: 8 bytes per object (50% header reduction)

// Enabling compact headers (Java 25+)
// JVM flag: -XX:+UseCompactObjectHeaders (default in Java 25+)

class CompactHeadersExample {

    // Example: Money value object
    static class Money {
        private final BigDecimal amount;  // => 8 bytes reference
        private final String currency;    // => 8 bytes reference
                                         // => Total fields: 16 bytes
    }

    // Memory layout comparison:
    // Traditional header (Java 8-24):
    // - Object header: 16 bytes
    // - Fields: 16 bytes
    // - Total: 32 bytes per Money object

    // Compact header (Java 25+):
    // - Object header: 8 bytes (50% reduction)
    // - Fields: 16 bytes
    // - Total: 24 bytes per Money object (25% savings)

    // Impact on 10 million Money objects:
    // Traditional: 10M × 32 bytes = 320MB
    // Compact: 10M × 24 bytes = 240MB
    // Savings: 80MB (25% reduction)

    // GC impact:
    // - Lower heap usage → less GC pressure
    // - Fewer GC cycles (heap fills slower)
    // - Better cache locality (more objects fit in CPU cache)
}

// TECHNIQUE 2: Value Types (Project Valhalla, future Java)
// Value types eliminate object headers entirely
// - No identity (no ==, no synchronization)
// - No indirection (inlined in arrays, fields)
// - No GC overhead (managed by stack/register, not heap)

class ValueTypesExample {

    // Current reference type (heap-allocated)
    static class Point {
        private final int x;             // => 4 bytes
        private final int y;             // => 4 bytes
                                         // => Object header: 16 bytes
                                         // => Padding: 8 bytes (alignment)
                                         // => Total: 32 bytes
    }

    // Point array (reference type):
    // Point[] points = new Point[1000];
    // - Array: 8000 bytes (1000 × 8-byte references)
    // - Objects: 32000 bytes (1000 × 32-byte Point objects)
    // - Total: 40000 bytes

    // Future value type (inline class)
    // value class PointValue {
    //     private final int x;          => 4 bytes
    //     private final int y;          => 4 bytes
    //                                   => No object header!
    //                                   => Total: 8 bytes
    // }

    // PointValue array (value type):
    // PointValue[] points = new PointValue[1000];
    // - Array: 8000 bytes (1000 × 8-byte inline values)
    // - No separate objects (values inlined in array)
    // - Total: 8000 bytes (5x reduction!)

    // Value type benefits:
    // - No indirection: values stored inline (cache-friendly)
    // - No GC overhead: values managed by stack/register
    // - Reduced memory: no object headers (8-16 bytes per object)
    // - Better performance: CPU can vectorize operations (SIMD)

    // Value type constraints:
    // - No identity: no == (use equals()), no synchronization
    // - Immutability: all fields must be final
    // - No null: value types cannot be null (use Optional if needed)
}

// TECHNIQUE 3: Foreign Function & Memory API (Java 17+)
import java.lang.foreign.*;

class ForeignMemoryExample {

    // Manual memory management (off-heap)
    public void processLargeBuffer() {
        // Allocate 1GB off-heap memory
        try (MemorySegment segment = Arena.ofConfined().allocate(1024 * 1024 * 1024)) {
                                         // => Allocates 1GB native memory (off-heap)
                                         // => Not managed by GC (no GC overhead)
                                         // => Auto-released when Arena closed (try-with-resources)

            // Write data to memory segment
            for (long i = 0; i < 1024 * 1024; i++) {
                segment.setAtIndex(ValueLayout.JAVA_LONG, i, i * 2);
                                         // => Writes long value at index i
                                         // => Direct memory access (no bounds checking in release mode)
            }

            // Read data from memory segment
            long sum = 0;
            for (long i = 0; i < 1024 * 1024; i++) {
                sum += segment.getAtIndex(ValueLayout.JAVA_LONG, i);
                                         // => Reads long value at index i
            }

            System.out.println("Sum: " + sum);

        }                                // => Memory automatically released (no manual free())
                                         // => Arena cleanup handles deallocation
    }

    // Foreign memory benefits:
    // - No GC overhead: off-heap memory not scanned by GC
    // - Predictable performance: no GC pauses
    // - Large datasets: multi-GB buffers without heap limits
    // - Interop: share memory with native code (JNI, FFI)

    // Foreign memory trade-offs:
    // - Manual lifecycle: must close Arena (try-with-resources)
    // - No bounds checking: segfaults possible (like C/C++)
    // - Complexity: requires understanding of memory layout
    // - Limited safety: use with caution (preview API)
}

// TECHNIQUE 4: String Deduplication (G1GC/ZGC)
class StringDeduplicationExample {

    // String deduplication eliminates duplicate char arrays
    // Enabled with: -XX:+UseStringDeduplication (G1GC, ZGC)

    // Example: Loading 1M customer records
    // - 500k customers in "New York" (duplicate String)
    // - Each "New York" String: 20 bytes (header + char array)

    // Without deduplication:
    // - 500k × 20 bytes = 10MB for duplicate "New York" strings

    // With deduplication:
    // - 1 × 20 bytes = 20 bytes (all Strings share same char array)
    // - Savings: 9.99MB (99.998% reduction)

    // How it works:
    // - G1GC marks Strings during GC
    // - Compares char arrays of Strings with same hash code
    // - Points duplicate Strings to same char array
    // - Original char arrays become garbage (collected)

    // Best for: applications with many duplicate strings (JSON, XML, database results)
    // Overhead: ~1% GC pause time increase (deduplication work)
}

// TECHNIQUE 5: Compressed Ordinary Object Pointers (CompressedOops)
class CompressedOopsExample {

    // CompressedOops reduces pointer size from 64 bits to 32 bits
    // Enabled automatically for heaps <32GB
    // JVM flag: -XX:+UseCompressedOops (default for heaps <32GB)

    // How it works:
    // - 64-bit pointer can address 2^64 bytes (18 exabytes)
    // - Java objects 8-byte aligned (addresses always multiples of 8)
    // - Shift right 3 bits: 32 bits can address 2^35 bytes (32GB)
    // - Pointer compression: store 32-bit shifted address, shift left on dereference

    // Memory savings:
    // - Reference fields: 8 bytes → 4 bytes (50% reduction)
    // - Array references: 8 bytes → 4 bytes
    // - Example: Object with 10 reference fields
    //   - Uncompressed: 80 bytes (10 × 8)
    //   - Compressed: 40 bytes (10 × 4)
    //   - Savings: 40 bytes (50%)

    // Trade-off:
    // - Heap limit: 32GB max (beyond 32GB, compressedOops disabled)
    // - Slight CPU overhead: shift operations on every pointer access
    // - Net benefit: memory savings outweigh CPU cost

    // Best practice: Keep heap <32GB to enable CompressedOops
    // - If need >32GB: consider multiple JVMs or ZGC with larger regions
}

// Memory optimization summary
// 1. Compact Object Headers (Java 25+): 25% header reduction (8 bytes → 8 bytes savings per object)
// 2. Value Types (Valhalla): Eliminate headers entirely (32 bytes → 8 bytes for Point)
// 3. Foreign Memory API: Off-heap memory (no GC overhead, manual lifecycle)
// 4. String Deduplication: Eliminate duplicate strings (99% savings for duplicates)
// 5. CompressedOops: 50% pointer reduction (heaps <32GB)

// Combined impact (example: 10M objects with 5 references each):
// Baseline (Java 8):
// - Object headers: 10M × 16 bytes = 160MB
// - Reference fields: 10M × 5 × 8 bytes = 400MB
// - Total: 560MB

// Optimized (Java 25 + CompressedOops):
// - Object headers: 10M × 8 bytes = 80MB (compact headers)
// - Reference fields: 10M × 5 × 4 bytes = 200MB (compressed oops)
// - Total: 280MB (50% reduction)
```

**Key Takeaway**: Advanced memory optimizations include compact object headers (8-byte savings per object, Java 25+), value types (eliminate headers, Project Valhalla), Foreign Memory API (off-heap, no GC), string deduplication (eliminate duplicates), and CompressedOops (50% pointer reduction for heaps <32GB). Combine techniques for dramatic memory savings (50%+ reduction).

**Why It Matters** (100 words): Memory optimization directly impacts application cost and performance. A 50% memory reduction means half the servers or double the throughput. Compact headers save 80MB per 10M objects—significant for in-memory databases, caches, and analytics. Value types (future) eliminate indirection overhead, enabling billion-object data structures. Foreign Memory API bypasses GC for huge datasets (multi-TB machine learning, genomics). String deduplication saves 10-20% heap in typical applications (JSON, database results). CompressedOops is free (enabled by default <32GB heaps). Understanding these optimizations enables building memory-efficient systems that scale cost-effectively. Always profile first—optimize what matters most.

---

## Finite State Machines

Finite State Machines (FSMs) model entities with discrete states and well-defined transitions. FSMs prevent invalid states, provide audit trails, and enable deterministic behavior in business workflows.

## Example 85: Enum-Based FSM - Order Processing

Enum-based FSMs embed transition logic in enum methods. This lightweight approach suits straightforward workflows with limited state-specific behavior.

**State Transition Diagram:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC
stateDiagram-v2
    [*] --> PENDING

    PENDING --> APPROVED: approve
    PENDING --> REJECTED: reject

    APPROVED --> PROCESSING: process
    APPROVED --> CANCELLED: cancel

    PROCESSING --> COMPLETED: complete
    PROCESSING --> FAILED: fail

    COMPLETED --> [*]
    FAILED --> [*]
    REJECTED --> [*]
    CANCELLED --> [*]

    note right of PENDING
        Initial state
        Awaiting approval
    end note

    note right of PROCESSING
        Order in flight
        Cannot cancel
    end note
```

**Code:**

```java
// Enum-based state machine with transition methods
public enum OrderState {
    PENDING {
        @Override
        public OrderState approve() { // => Override for PENDING state only
                                        // => Transition PENDING → APPROVED
            return APPROVED;            // => Returns next state (immutable transition)
        }                               // => No state mutation: returns new state

        @Override
        public OrderState reject(String reason) { // => PENDING can also be rejected
                                                   // => Transition PENDING → REJECTED
            return REJECTED;                       // => Terminal state (no further transitions)
        }
    },
    APPROVED {
        @Override
        public OrderState process() { // => APPROVED can transition to PROCESSING
                                       // => Indicates order is being fulfilled
            return PROCESSING;         // => Active processing state
        }

        @Override
        public OrderState cancel(String reason) { // => APPROVED orders can be cancelled
                                                   // => Transition APPROVED → CANCELLED
            return CANCELLED;                      // => Terminal state
        }
    },
    PROCESSING {
        @Override
        public OrderState complete() { // => PROCESSING can complete successfully
                                        // => Transition PROCESSING → COMPLETED
            return COMPLETED;           // => Terminal state (successful)
        }

        @Override
        public OrderState fail(String reason) { // => PROCESSING can fail (e.g., inventory issue)
                                                 // => Transition PROCESSING → FAILED
            return FAILED;                       // => Terminal state (failure)
        }
    },
    COMPLETED,  // => Terminal state (order fulfilled)
    FAILED,     // => Terminal state (order failed)
    REJECTED,   // => Terminal state (order rejected)
    CANCELLED;  // => Terminal state (order cancelled)

    // Default implementations throw exception for invalid transitions
    public OrderState approve() { // => Base implementation: invalid for most states
                                   // => Only PENDING overrides this
        throw new IllegalStateException( // => Fail fast on invalid transition
            "Cannot approve order in " + this + " state" // => Clear error message with current state
        );
    }

    public OrderState reject(String reason) { // => Base: reject only valid from PENDING
        throw new IllegalStateException(
            "Cannot reject order in " + this + " state"
        );
    }

    public OrderState process() { // => Base: process only valid from APPROVED
        throw new IllegalStateException(
            "Cannot process order in " + this + " state"
        );
    }

    public OrderState complete() { // => Base: complete only valid from PROCESSING
        throw new IllegalStateException(
            "Cannot complete order in " + this + " state"
        );
    }

    public OrderState fail(String reason) { // => Base: fail only valid from PROCESSING
        throw new IllegalStateException(
            "Cannot fail order in " + this + " state"
        );
    }

    public OrderState cancel(String reason) { // => Base: cancel only valid from APPROVED
        throw new IllegalStateException(
            "Cannot cancel order in " + this + " state"
        );
    }

    // Query methods for state properties
    public boolean isTerminal() { // => Check if state allows no further transitions
        return this == COMPLETED || this == FAILED ||
               this == REJECTED || this == CANCELLED; // => Four terminal states
    }                              // => Terminal states reject all transition attempts

    public boolean canProcess() { // => Business logic query
        return this == APPROVED;  // => Only APPROVED orders can be processed
    }
}

// Order entity uses enum state machine
public class Order {
    private final String id;       // => Immutable order ID
    private final BigDecimal total; // => Immutable order total
    private OrderState state;      // => Mutable state (FSM allows controlled mutation)
    private String failureReason;  // => Optional: tracks failure/cancellation reason
    private LocalDateTime lastTransitionAt; // => Audit: timestamp of last state change

    public Order(String id, BigDecimal total) { // => Constructor creates order in initial state
        this.id = id;
        this.total = total;
        this.state = OrderState.PENDING; // => All orders start PENDING
                                          // => Single entry point: no arbitrary states
        this.lastTransitionAt = LocalDateTime.now(); // => Record creation timestamp
    }

    // Transition methods delegate to state enum
    public void approve() {        // => Public API: approve order
        this.state = this.state.approve(); // => Delegate to enum (validates transition)
                                    // => State enum throws if transition invalid
                                    // => Immutable enum return: get new state
        this.lastTransitionAt = LocalDateTime.now(); // => Update audit timestamp
    }                               // => No direct state assignment: always via enum

    public void reject(String reason) { // => Reject with reason
        this.state = this.state.reject(reason); // => Enum validates PENDING → REJECTED
        this.failureReason = reason; // => Store rejection reason
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void process() {        // => Begin order processing
        this.state = this.state.process(); // => Enum validates APPROVED → PROCESSING
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void complete() {       // => Mark order complete
        this.state = this.state.complete(); // => Enum validates PROCESSING → COMPLETED
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Getters
    public OrderState getState() { return state; } // => Expose current state
    public boolean isTerminal() { return state.isTerminal(); } // => Query terminal status
}
```

**Key Takeaway**: Enum-based FSMs centralize transition logic in the enum. Each state overrides only valid transitions. Default implementations throw exceptions for invalid transitions, providing compile-time state safety and runtime validation.

**Why It Matters**: Enum FSMs eliminate scattered state logic across services. Transition rules live in the enum, making workflows explicit and testable. Terminal states prevent invalid operations automatically.

## Example 86: State Pattern - Loan Application Workflow

The State Pattern represents states as separate classes. Each state class encapsulates state-specific behavior, enabling rich state logic beyond enum capabilities.

**State Transition Diagram:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC
stateDiagram-v2
    [*] --> DRAFT

    DRAFT --> SUBMITTED: submit
    DRAFT --> REJECTED: reject

    SUBMITTED --> UNDER_REVIEW: startReview
    SUBMITTED --> REJECTED: reject

    UNDER_REVIEW --> APPROVED: approve
    UNDER_REVIEW --> REJECTED: reject

    APPROVED --> DISBURSED: disburse

    DISBURSED --> REPAYING: startRepayment

    REPAYING --> COMPLETED: completeRepayment
    REPAYING --> DEFAULTED: default

    COMPLETED --> [*]
    REJECTED --> [*]
    DEFAULTED --> [*]
```

**Code:**

```java
// State interface defines all possible transitions
public interface LoanState { // => Contract for all loan states
    String getStateName();   // => State identification

    // Transition methods (each state implements only valid ones)
    LoanState submit();      // => DRAFT → SUBMITTED
    LoanState startReview(String reviewerId); // => SUBMITTED → UNDER_REVIEW
    LoanState approve(String approverId); // => UNDER_REVIEW → APPROVED
    LoanState reject(String reason); // => Multiple states can reject
    LoanState disburse(BigDecimal amount); // => APPROVED → DISBURSED
    LoanState startRepayment(); // => DISBURSED → REPAYING
    LoanState completeRepayment(); // => REPAYING → COMPLETED
    LoanState defaultLoan(String reason); // => REPAYING → DEFAULTED

    // Query methods
    boolean canDisburse();   // => Business rule: can funds be disbursed?
    boolean isTerminal();    // => Is this a terminal state?
}

// Abstract base provides default "invalid transition" implementations
public abstract class AbstractLoanState implements LoanState {
                                 // => Base class: reduces boilerplate in concrete states
                                 // => Each concrete state overrides only valid transitions
    @Override
    public LoanState submit() { // => Default: most states cannot submit
        throw new IllegalStateException(
            "Cannot submit from " + getStateName() // => Explicit error with state name
        );
    }                            // => Concrete states override if transition valid

    @Override
    public LoanState startReview(String reviewerId) { // => Default: invalid
        throw new IllegalStateException(
            "Cannot start review from " + getStateName()
        );
    }

    @Override
    public LoanState approve(String approverId) { // => Default: invalid
        throw new IllegalStateException(
            "Cannot approve from " + getStateName()
        );
    }

    @Override
    public LoanState reject(String reason) { // => Most states CAN reject (override in many)
        throw new IllegalStateException(
            "Cannot reject from " + getStateName()
        );
    }

    @Override
    public LoanState disburse(BigDecimal amount) { // => Default: invalid
        throw new IllegalStateException(
            "Cannot disburse from " + getStateName()
        );
    }

    @Override
    public LoanState startRepayment() { // => Default: invalid
        throw new IllegalStateException(
            "Cannot start repayment from " + getStateName()
        );
    }

    @Override
    public LoanState completeRepayment() { // => Default: invalid
        throw new IllegalStateException(
            "Cannot complete repayment from " + getStateName()
        );
    }

    @Override
    public LoanState defaultLoan(String reason) { // => Default: invalid
        throw new IllegalStateException(
            "Cannot default from " + getStateName()
        );
    }

    @Override
    public boolean canDisburse() { return false; } // => Default: cannot disburse

    @Override
    public boolean isTerminal() { return false; } // => Default: not terminal
}

// Concrete state: Draft
public class DraftState extends AbstractLoanState {
                                 // => Initial state: loan application being prepared
    @Override
    public String getStateName() { return "DRAFT"; } // => State identifier

    @Override
    public LoanState submit() {  // => DRAFT can transition to SUBMITTED
        return new SubmittedState(); // => Create new state object (immutable transition)
    }                            // => State pattern: each transition creates new state instance

    @Override
    public LoanState reject(String reason) { // => DRAFT can be rejected before submission
        return new RejectedState(reason); // => Pass rejection reason to terminal state
    }
}

// Concrete state: Submitted
public class SubmittedState extends AbstractLoanState {
                                 // => Application submitted, awaiting review assignment
    @Override
    public String getStateName() { return "SUBMITTED"; }

    @Override
    public LoanState startReview(String reviewerId) { // => Assign reviewer
        return new UnderReviewState(reviewerId); // => Pass reviewer ID to next state
    }                            // => State carries data: reviewerId

    @Override
    public LoanState reject(String reason) { // => Can reject before review starts
        return new RejectedState(reason);
    }
}

// Concrete state: Under Review (carries reviewer data)
public class UnderReviewState extends AbstractLoanState {
                                 // => Review in progress
    private final String reviewerId; // => State-specific data: who is reviewing?

    public UnderReviewState(String reviewerId) { // => Constructor requires reviewer
        this.reviewerId = reviewerId; // => State objects can carry data
    }

    @Override
    public String getStateName() { return "UNDER_REVIEW"; }

    @Override
    public LoanState approve(String approverId) { // => Review passed
        return new ApprovedState(approverId); // => Pass approver ID to next state
    }

    @Override
    public LoanState reject(String reason) { // => Review failed
        return new RejectedState(reason);
    }

    public String getReviewerId() { return reviewerId; } // => Expose state data
}

// Concrete state: Approved (carries approver data)
public class ApprovedState extends AbstractLoanState {
                                 // => Loan approved, ready for disbursement
    private final String approverId; // => State data: who approved?

    public ApprovedState(String approverId) {
        this.approverId = approverId;
    }

    @Override
    public String getStateName() { return "APPROVED"; }

    @Override
    public LoanState disburse(BigDecimal amount) { // => Disburse funds
        return new DisbursedState(amount); // => Pass disbursed amount to next state
    }                            // => State data: track disbursement amount

    @Override
    public boolean canDisburse() { return true; } // => Business rule: can disburse
}

// Concrete state: Disbursed (carries disbursement amount)
public class DisbursedState extends AbstractLoanState {
                                 // => Funds disbursed, awaiting repayment start
    private final BigDecimal disbursedAmount; // => State data: how much disbursed?

    public DisbursedState(BigDecimal disbursedAmount) {
        this.disbursedAmount = disbursedAmount;
    }

    @Override
    public String getStateName() { return "DISBURSED"; }

    @Override
    public LoanState startRepayment() { // => Borrower begins repayment
        return new RepayingState(); // => Transition to active repayment
    }

    public BigDecimal getDisbursedAmount() { return disbursedAmount; }
}

// Concrete state: Repaying
public class RepayingState extends AbstractLoanState {
                                 // => Active repayment phase
    @Override
    public String getStateName() { return "REPAYING"; }

    @Override
    public LoanState completeRepayment() { // => All payments made
        return new CompletedState(); // => Terminal state (success)
    }

    @Override
    public LoanState defaultLoan(String reason) { // => Borrower defaults
        return new DefaultedState(reason); // => Terminal state (failure)
    }
}

// Terminal state: Completed
public class CompletedState extends AbstractLoanState {
                                 // => Loan fully repaid
    @Override
    public String getStateName() { return "COMPLETED"; }

    @Override
    public boolean isTerminal() { return true; } // => No further transitions
}

// Terminal state: Rejected (carries rejection reason)
public class RejectedState extends AbstractLoanState {
                                 // => Loan rejected at some stage
    private final String reason; // => State data: why rejected?

    public RejectedState(String reason) {
        this.reason = reason;
    }

    @Override
    public String getStateName() { return "REJECTED"; }

    @Override
    public boolean isTerminal() { return true; } // => No further transitions

    public String getReason() { return reason; } // => Expose rejection reason
}

// Terminal state: Defaulted (carries default reason)
public class DefaultedState extends AbstractLoanState {
                                 // => Loan defaulted during repayment
    private final String reason; // => State data: why defaulted?

    public DefaultedState(String reason) {
        this.reason = reason;
    }

    @Override
    public String getStateName() { return "DEFAULTED"; }

    @Override
    public boolean isTerminal() { return true; }

    public String getReason() { return reason; }
}

// Loan entity uses state pattern
public class LoanApplication { // => Aggregate root using state pattern
    private final String id;    // => Immutable loan ID
    private final BigDecimal principal; // => Immutable loan amount
    private LoanState state;    // => Current state (mutable via FSM)
    private LocalDateTime lastTransitionAt; // => Audit timestamp

    public LoanApplication(String id, BigDecimal principal) {
        this.id = id;
        this.principal = principal;
        this.state = new DraftState(); // => Initial state object
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition methods delegate to state object
    public void submit() {       // => Public API: submit application
        this.state = this.state.submit(); // => State object handles transition
                                 // => Returns new state object (immutable)
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void startReview(String reviewerId) {
        this.state = this.state.startReview(reviewerId);
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void approve(String approverId) {
        this.state = this.state.approve(approverId);
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void reject(String reason) {
        this.state = this.state.reject(reason);
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void disburse(BigDecimal amount) {
        this.state = this.state.disburse(amount);
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void startRepayment() {
        this.state = this.state.startRepayment();
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Getters
    public LoanState getState() { return state; }
    public String getStateName() { return state.getStateName(); }
    public boolean canDisburse() { return state.canDisburse(); }
}
```

**Key Takeaway**: State Pattern enables rich state-specific behavior through separate state classes. States can carry data (reviewerId, amount, reason), making complex workflows explicit. Open/Closed Principle: add new states without modifying existing ones.

**Why It Matters**: When state logic is complex (guards, actions, state data), State Pattern provides better organization than enums. Each state class encapsulates its own behavior, improving testability and maintainability. Polymorphism eliminates conditional logic scattered across the codebase.

## Example 87: Sealed Classes FSM - Contract Lifecycle

Sealed classes (Java 17+) combine enum exhaustiveness with State Pattern flexibility. Pattern matching enables concise state-based logic with compile-time verification.

**State Hierarchy Diagram:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC
graph TD
    Interface["sealed interface<br/>ContractState"]
    Interface --> Draft["record Draft<br/>createdAt"]
    Interface --> Review["record UnderReview<br/>reviewerId, startedAt"]
    Interface --> Active["record Active<br/>startDate, endDate"]
    Interface --> Suspended["record Suspended<br/>reason, canReactivate"]
    Interface --> Archived["record Archived<br/>archivedAt, reason"]

    style Interface fill:#0173B2,color:#fff
    style Draft fill:#029E73,color:#fff
    style Review fill:#029E73,color:#fff
    style Active fill:#029E73,color:#fff
    style Suspended fill:#DE8F05,color:#fff
    style Archived fill:#CC78BC,color:#fff
```

**Code:**

```java
// Sealed interface: compiler knows all permitted states
public sealed interface ContractState // => Sealed: only listed types can implement
    permits Draft, UnderReview, Active, Suspended, Archived {
                                 // => Permits clause: exhaustive set of states
                                 // => Compiler enforces: no other types can implement
    String getStateName();       // => Common behavior: state identification
    boolean isTerminal();        // => Common behavior: terminal status
}                                // => Sealed hierarchy: compile-time exhaustiveness

// State: Draft (initial state, immutable record)
public record Draft(         // => Record: auto-generates constructor, getters, equals, hashCode
    LocalDateTime createdAt  // => State data: when draft was created
) implements ContractState { // => Implements sealed interface
    @Override
    public String getStateName() { return "DRAFT"; } // => Record can override methods

    @Override
    public boolean isTerminal() { return false; } // => Draft is not terminal
}                                // => Records are immutable: all fields final

// State: Under Review (carries reviewer data)
public record UnderReview(
    String reviewerId,       // => State data: who is reviewing?
    LocalDateTime reviewStartedAt // => State data: when review started?
) implements ContractState {
    @Override
    public String getStateName() { return "UNDER_REVIEW"; }

    @Override
    public boolean isTerminal() { return false; }
}

// State: Active (carries contract period data)
public record Active(
    LocalDate startDate,     // => State data: contract start date
    LocalDate endDate,       // => State data: contract end date
    BigDecimal value         // => State data: contract value
) implements ContractState {
    public Active {          // => Compact constructor: validation
        if (endDate.isBefore(startDate)) { // => Invariant: end after start
            throw new IllegalArgumentException(
                "End date must be after start date" // => Fail fast on invalid data
            );
        }                    // => Records enforce invariants in constructor
        if (value.compareTo(BigDecimal.ZERO) <= 0) { // => Invariant: positive value
            throw new IllegalArgumentException("Value must be positive");
        }
    }

    @Override
    public String getStateName() { return "ACTIVE"; }

    @Override
    public boolean isTerminal() { return false; }

    public boolean isExpired() { // => State-specific behavior: check expiry
        return LocalDate.now().isAfter(endDate); // => Business logic in state
    }
}

// State: Suspended (carries suspension data)
public record Suspended(
    String reason,           // => State data: why suspended?
    LocalDateTime suspendedAt, // => State data: when suspended?
    boolean canReactivate    // => State data: can it be reactivated?
) implements ContractState {
    @Override
    public String getStateName() { return "SUSPENDED"; }

    @Override
    public boolean isTerminal() { return false; } // => Suspended is not terminal (can reactivate)
}

// State: Archived (terminal state)
public record Archived(
    LocalDateTime archivedAt, // => State data: when archived?
    String archiveReason     // => State data: why archived?
) implements ContractState {
    @Override
    public String getStateName() { return "ARCHIVED"; }

    @Override
    public boolean isTerminal() { return true; } // => Terminal: no further transitions
}

// Contract entity uses sealed state hierarchy
public class Contract {      // => Aggregate root with sealed state FSM
    private final String id; // => Immutable contract ID
    private final String partyA; // => Immutable party A
    private final String partyB; // => Immutable party B
    private ContractState state; // => Current state (sealed interface)
    private LocalDateTime lastTransitionAt; // => Audit timestamp

    public Contract(String id, String partyA, String partyB) {
        this.id = id;
        this.partyA = partyA;
        this.partyB = partyB;
        this.state = new Draft(LocalDateTime.now()); // => Initial state: Draft record
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: Draft → UnderReview
    public void submitForReview(String reviewerId) {
        if (!(state instanceof Draft)) { // => Guard: only Draft can submit
                                 // => instanceof with sealed types: compiler knows all types
            throw new IllegalStateException(
                "Can only submit draft contracts" // => Explicit error message
            );
        }                        // => Pattern matching: type-safe check
        this.state = new UnderReview(reviewerId, LocalDateTime.now());
                                 // => Create new state record (immutable)
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: UnderReview → Active
    public void activate(BigDecimal value, LocalDate startDate, LocalDate endDate) {
        if (!(state instanceof UnderReview)) { // => Guard: only UnderReview can activate
            throw new IllegalStateException(
                "Can only activate contracts under review"
            );
        }
        this.state = new Active(startDate, endDate, value);
                                 // => Record constructor validates invariants
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: UnderReview → Draft (send back for revisions)
    public void sendBackToDraft() {
        if (!(state instanceof UnderReview)) {
            throw new IllegalStateException(
                "Can only send back contracts under review"
            );
        }
        this.state = new Draft(LocalDateTime.now()); // => Revert to Draft
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: Active → Suspended
    public void suspend(String reason, boolean canReactivate) {
        if (!(state instanceof Active)) { // => Guard: only Active can suspend
            throw new IllegalStateException(
                "Can only suspend active contracts"
            );
        }
        this.state = new Suspended(reason, LocalDateTime.now(), canReactivate);
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: Suspended → Active (if allowed)
    public void reactivate(BigDecimal value, LocalDate startDate, LocalDate endDate) {
        if (!(state instanceof Suspended suspended)) { // => Pattern matching with variable
                                 // => Combines instanceof check + cast
            throw new IllegalStateException(
                "Can only reactivate suspended contracts"
            );
        }
        if (!suspended.canReactivate()) { // => Guard: check reactivation flag
                                 // => Pattern variable 'suspended' in scope
            throw new IllegalStateException(
                "Contract cannot be reactivated: " + suspended.reason()
            );                   // => Access record data via pattern variable
        }
        this.state = new Active(startDate, endDate, value);
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: Any → Archived
    public void archive(String reason) {
        if (state.isTerminal()) { // => Guard: cannot archive terminal states
            throw new IllegalStateException(
                "Cannot archive terminal state"
            );
        }
        this.state = new Archived(LocalDateTime.now(), reason);
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Pattern matching for state-specific behavior
    public String getStateDescription() { // => Switch expression with pattern matching
        return switch (state) {  // => Exhaustive: compiler verifies all states handled
            case Draft(var createdAt) -> // => Deconstruct Draft record
                "Draft created at " + createdAt; // => Access record component

            case UnderReview(var reviewerId, var startedAt) ->
                                 // => Deconstruct UnderReview with multiple components
                "Under review by %s since %s".formatted(reviewerId, startedAt);

            case Active(var startDate, var endDate, var value) ->
                                 // => Deconstruct Active with three components
                "Active contract worth %s (%s to %s)"
                    .formatted(value, startDate, endDate);

            case Suspended(var reason, var suspendedAt, var canReactivate) ->
                "Suspended: %s (reactivate: %s)".formatted(reason, canReactivate);

            case Archived(var archivedAt, var reason) ->
                "Archived at %s: %s".formatted(archivedAt, reason);
        };                       // => No default case: sealed types guarantee exhaustiveness
    }                            // => Compiler error if new state added without handling

    // Pattern matching with guards (when clauses)
    public boolean requiresAction() {
        return switch (state) {
            case Draft d -> true; // => All drafts need submission
            case UnderReview r -> true; // => All reviews need approval/rejection
            case Active a when a.isExpired() -> true;
                                 // => Guard: expired contracts need archival
            case Active a -> false; // => Active non-expired: no action needed
            case Suspended s when s.canReactivate() -> true;
                                 // => Suspended but can reactivate: needs decision
            case Suspended s -> false; // => Suspended permanently: no action
            case Archived a -> false; // => Archived: no action
        };                       // => When clauses: boolean predicates on pattern variables
    }

    // Getters
    public ContractState getState() { return state; }
    public String getStateName() { return state.getStateName(); }
}
```

**Key Takeaway**: Sealed classes provide compile-time exhaustiveness for FSMs. Pattern matching eliminates runtime type checks and casts. Records combine state data with immutability, making FSMs both type-safe and concise.

**Why It Matters**: Sealed FSMs catch missing state handling at compile time. When adding new states, compiler forces updates to all switch expressions. Pattern matching with guards enables complex state-based logic without verbose if-else chains.

## Example 88: Guard Conditions and Entry Actions

Guard conditions validate business rules before transitions. Entry actions execute side effects when entering states. Combining guards and actions creates robust, auditable FSMs.

**Code:**

```java
// Enhanced OrderState with guards and actions
public enum OrderState {
    PENDING {
        @Override
        public OrderState approve(Order order) { // => Pass entity for guards/actions
                                 // => Guard + action pattern
            // GUARD: Validate business rules before transition
            if (order.getTotal().compareTo(BigDecimal.ZERO) <= 0) {
                                 // => Guard: amount must be positive
                throw new BusinessRuleException(
                    "Order total must be positive" // => Fail fast on invalid state
                );
            }                    // => Guards prevent invalid transitions
            if (order.getCustomerId() == null) {
                                 // => Guard: customer required
                throw new BusinessRuleException("Customer is required");
            }

            // ENTRY ACTION: Side effects on successful transition
            auditLog.log("Order " + order.getId() + " approved");
                                 // => Entry action: audit trail
            notificationService.notifyCustomer(order, "ORDER_APPROVED");
                                 // => Entry action: send notification
            return APPROVED;     // => Transition to next state
        }

        @Override
        public OrderState reject(Order order, String reason) {
            // ENTRY ACTION: Log rejection
            auditLog.log("Order " + order.getId() + " rejected: " + reason);
            notificationService.notifyCustomer(order, "ORDER_REJECTED");
            return REJECTED;
        }
    },
    APPROVED {
        @Override
        public OrderState process(Order order) {
            // GUARD: Check inventory before processing
            if (!inventoryService.hasStock(order)) {
                                 // => Guard: inventory availability
                throw new BusinessRuleException(
                    "Insufficient inventory for order " + order.getId()
                );
            }                    // => Guard prevents processing when cannot fulfill

            // GUARD: Check payment before processing
            if (!paymentService.hasValidPayment(order)) {
                                 // => Guard: payment validation
                throw new BusinessRuleException("No valid payment for order");
            }

            // ENTRY ACTION: Reserve inventory
            inventoryService.reserveStock(order);
                                 // => Entry action: side effect (reserve inventory)
                                 // => Reservation happens only on successful transition
            auditLog.log("Order " + order.getId() + " processing started");
            return PROCESSING;
        }

        @Override
        public OrderState cancel(Order order, String reason) {
            // ENTRY ACTION: Log cancellation
            auditLog.log("Order " + order.getId() + " cancelled: " + reason);
            notificationService.notifyCustomer(order, "ORDER_CANCELLED");
            return CANCELLED;
        }
    },
    PROCESSING {
        @Override
        public OrderState complete(Order order) {
            // GUARD: Verify all items shipped
            if (!shippingService.allItemsShipped(order)) {
                                 // => Guard: shipping completion
                throw new BusinessRuleException(
                    "Cannot complete order - not all items shipped"
                );
            }

            // ENTRY ACTION: Execute payment
            paymentService.capturePayment(order);
                                 // => Entry action: finalize payment
                                 // => Payment captured only when order completed
            auditLog.log("Order " + order.getId() + " completed");
            notificationService.notifyCustomer(order, "ORDER_COMPLETED");
            return COMPLETED;
        }

        @Override
        public OrderState fail(Order order, String reason) {
            // ENTRY ACTION: Release reserved inventory
            inventoryService.releaseStock(order);
                                 // => Entry action: undo reservation
                                 // => Cleanup on failure
            auditLog.log("Order " + order.getId() + " failed: " + reason);
            notificationService.notifyCustomer(order, "ORDER_FAILED");
            return FAILED;
        }
    },
    COMPLETED,  // Terminal states have no transitions
    FAILED,
    REJECTED,
    CANCELLED;

    // Default implementations throw exception
    public OrderState approve(Order order) {
        throw new IllegalStateException(
            "Cannot approve order in " + this + " state"
        );
    }

    public OrderState reject(Order order, String reason) {
        throw new IllegalStateException(
            "Cannot reject order in " + this + " state"
        );
    }

    public OrderState process(Order order) {
        throw new IllegalStateException(
            "Cannot process order in " + this + " state"
        );
    }

    public OrderState complete(Order order) {
        throw new IllegalStateException(
            "Cannot complete order in " + this + " state"
        );
    }

    public OrderState fail(Order order, String reason) {
        throw new IllegalStateException(
            "Cannot fail order in " + this + " state"
        );
    }

    public OrderState cancel(Order order, String reason) {
        throw new IllegalStateException(
            "Cannot cancel order in " + this + " state"
        );
    }
}

// Order entity with guard-based transitions
public class Order {
    private final String id;
    private final String customerId;
    private final BigDecimal total;
    private OrderState state;
    private String failureReason;
    private LocalDateTime lastTransitionAt;

    public Order(String id, String customerId, BigDecimal total) {
        this.id = id;
        this.customerId = customerId;
        this.total = total;
        this.state = OrderState.PENDING;
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transitions delegate to state enum (with entity for guards/actions)
    public void approve() {      // => Pass 'this' to enable guards and actions
        this.state = this.state.approve(this);
                                 // => State enum validates guards, executes actions
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void reject(String reason) {
        this.state = this.state.reject(this, reason);
        this.failureReason = reason;
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void process() {
        this.state = this.state.process(this);
        this.lastTransitionAt = LocalDateTime.now();
    }

    public void complete() {
        this.state = this.state.complete(this);
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Getters
    public String getId() { return id; }
    public String getCustomerId() { return customerId; }
    public BigDecimal getTotal() { return total; }
    public OrderState getState() { return state; }
}

// Example usage
Order order = new Order("ORD-001", "CUST-123", new BigDecimal("99.99"));
                                 // => Order created in PENDING state

try {
    order.approve();         // => Validates guards (total > 0, customer exists)
                                 // => Executes entry actions (audit log, notification)
                                 // => Transitions PENDING → APPROVED
} catch (BusinessRuleException e) {
                                 // => Guard failure throws exception
    System.err.println("Approval failed: " + e.getMessage());
}                                // => Order remains in PENDING state if guard fails

order.process();                 // => Validates inventory and payment guards
                                 // => Reserves inventory (entry action)
                                 // => Transitions APPROVED → PROCESSING

order.complete();                // => Validates shipping guard
                                 // => Captures payment (entry action)
                                 // => Transitions PROCESSING → COMPLETED
```

**Key Takeaway**: Guards validate business rules before transitions, preventing invalid state changes. Entry actions execute side effects atomically with transitions. Passing the entity to enum methods enables context-aware guards and actions.

**Why It Matters**: Guards centralize business rule validation at transition points. Entry actions ensure side effects happen only on successful transitions, maintaining consistency. Combined, they create robust FSMs that enforce invariants and provide audit trails.

## Example 89: Hierarchical State Machines (Substates)

Hierarchical FSMs model states within states. Substates add granularity without state explosion. Example: ACTIVE state with substates (NORMAL, GRACE_PERIOD, LATE).

**Hierarchical State Diagram:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC
stateDiagram-v2
    [*] --> DRAFT
    DRAFT --> SUBMITTED: submit
    SUBMITTED --> ACTIVE: activate

    state ACTIVE {
        [*] --> NORMAL
        NORMAL --> GRACE_PERIOD: missPayment
        GRACE_PERIOD --> LATE: gracePeriodExpired
        LATE --> NORMAL: paymentReceived
        GRACE_PERIOD --> NORMAL: paymentReceived
    }

    ACTIVE --> CLOSED: close
    CLOSED --> [*]

    note right of ACTIVE
        Hierarchical state
        Contains substates
    end note
```

**Code:**

```java
// Top-level state enum
public enum SubscriptionState {
                                 // => Top-level states
    DRAFT,                       // => Initial state
    SUBMITTED,                   // => Awaiting activation
    ACTIVE,                      // => Active subscription (has substates)
    CLOSED;                      // => Terminal state

    public boolean isActive() {  // => Query: is subscription active?
        return this == ACTIVE;   // => Only ACTIVE state has substates
    }
}

// Substate enum for ACTIVE state
public enum ActiveSubstate {     // => Substates only valid when top-level state is ACTIVE
                                 // => Hierarchical structure: substates depend on parent state
    NORMAL,                      // => On-time payments
    GRACE_PERIOD,                // => Missed payment, grace period active
    LATE;                        // => Grace period expired, late fees apply

    public boolean isDelinquent() { // => Query: is payment delinquent?
        return this == GRACE_PERIOD || this == LATE;
    }                            // => Substate-specific logic
}

// Subscription entity with hierarchical state
public class Subscription {      // => Two-level FSM: state + substate
    private final String id;
    private final String customerId;
    private SubscriptionState state; // => Top-level state
    private ActiveSubstate activeSubstate; // => Substate (only valid when state=ACTIVE)
                                 // => Hierarchical relationship: substate depends on state
    private LocalDateTime lastTransitionAt;

    public Subscription(String id, String customerId) {
        this.id = id;
        this.customerId = customerId;
        this.state = SubscriptionState.DRAFT; // => Initial top-level state
        this.activeSubstate = null; // => No substate until ACTIVE
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: DRAFT → SUBMITTED
    public void submit() {
        if (state != SubscriptionState.DRAFT) {
            throw new IllegalStateException(
                "Can only submit draft subscriptions"
            );
        }
        this.state = SubscriptionState.SUBMITTED;
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: SUBMITTED → ACTIVE (with initial substate)
    public void activate() {
        if (state != SubscriptionState.SUBMITTED) {
            throw new IllegalStateException(
                "Can only activate submitted subscriptions"
            );
        }
        this.state = SubscriptionState.ACTIVE; // => Enter ACTIVE state
        this.activeSubstate = ActiveSubstate.NORMAL;
                                 // => Initial substate: NORMAL
                                 // => Hierarchical entry: set both state and substate
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Substate transition: NORMAL → GRACE_PERIOD (within ACTIVE)
    public void missPayment() {  // => Substate transition (top-level state unchanged)
        if (state != SubscriptionState.ACTIVE) {
                                 // => Guard: can only miss payment when ACTIVE
            throw new IllegalStateException(
                "Can only miss payment on active subscriptions"
            );
        }
        if (activeSubstate != ActiveSubstate.NORMAL) {
                                 // => Guard: only from NORMAL substate
            throw new IllegalStateException(
                "Already in grace period or late"
            );
        }
        this.activeSubstate = ActiveSubstate.GRACE_PERIOD;
                                 // => Substate transition: NORMAL → GRACE_PERIOD
                                 // => Top-level state remains ACTIVE
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Substate transition: GRACE_PERIOD → LATE
    public void gracePeriodExpired() {
        if (state != SubscriptionState.ACTIVE) {
            throw new IllegalStateException(
                "Grace period only valid for active subscriptions"
            );
        }
        if (activeSubstate != ActiveSubstate.GRACE_PERIOD) {
            throw new IllegalStateException(
                "Not in grace period"
            );
        }
        this.activeSubstate = ActiveSubstate.LATE;
                                 // => Substate transition: GRACE_PERIOD → LATE
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Substate transition: GRACE_PERIOD/LATE → NORMAL
    public void paymentReceived() { // => Payment brings subscription current
        if (state != SubscriptionState.ACTIVE) {
            throw new IllegalStateException(
                "Can only receive payment on active subscriptions"
            );
        }
        if (!activeSubstate.isDelinquent()) {
                                 // => Guard: only from delinquent substates
            throw new IllegalStateException(
                "Already current on payments"
            );
        }
        this.activeSubstate = ActiveSubstate.NORMAL;
                                 // => Return to NORMAL substate
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Transition: ACTIVE → CLOSED (exit hierarchical state)
    public void close() {
        if (state != SubscriptionState.ACTIVE) {
            throw new IllegalStateException(
                "Can only close active subscriptions"
            );
        }
        this.state = SubscriptionState.CLOSED; // => Exit ACTIVE state
        this.activeSubstate = null; // => Clear substate (no longer valid)
                                 // => Hierarchical exit: clear both state and substate
        this.lastTransitionAt = LocalDateTime.now();
    }

    // Query methods leveraging hierarchy
    public boolean isDelinquent() { // => Check if payment delinquent
        return state == SubscriptionState.ACTIVE &&
               activeSubstate != null &&
               activeSubstate.isDelinquent(); // => Substate query
    }                            // => Hierarchical query: check both levels

    public String getFullStateName() { // => Human-readable state description
        if (state == SubscriptionState.ACTIVE && activeSubstate != null) {
            return state + "." + activeSubstate; // => "ACTIVE.GRACE_PERIOD"
        }                        // => Hierarchical naming
        return state.toString(); // => "DRAFT", "SUBMITTED", "CLOSED"
    }

    // Getters
    public SubscriptionState getState() { return state; }
    public ActiveSubstate getActiveSubstate() { return activeSubstate; }
}

// Example usage
Subscription sub = new Subscription("SUB-001", "CUST-123");
                                 // => state=DRAFT, activeSubstate=null

sub.submit();                    // => state=SUBMITTED, activeSubstate=null
sub.activate();                  // => state=ACTIVE, activeSubstate=NORMAL
System.out.println(sub.getFullStateName()); // => "ACTIVE.NORMAL"

sub.missPayment();               // => state=ACTIVE, activeSubstate=GRACE_PERIOD
System.out.println(sub.getFullStateName()); // => "ACTIVE.GRACE_PERIOD"
System.out.println(sub.isDelinquent()); // => true (substate query)

sub.gracePeriodExpired();        // => state=ACTIVE, activeSubstate=LATE
System.out.println(sub.getFullStateName()); // => "ACTIVE.LATE"

sub.paymentReceived();           // => state=ACTIVE, activeSubstate=NORMAL
System.out.println(sub.getFullStateName()); // => "ACTIVE.NORMAL"

sub.close();                     // => state=CLOSED, activeSubstate=null
System.out.println(sub.getFullStateName()); // => "CLOSED"
```

**Key Takeaway**: Hierarchical FSMs add substates to parent states, providing granularity without state explosion. Substates are only valid when parent state is active. Transitions can occur within substates (changing substate) or across states (exiting hierarchy).

**Why It Matters**: Substates avoid combinatorial explosion. Instead of ACTIVE_NORMAL, ACTIVE_GRACE, ACTIVE_LATE as separate top-level states, hierarchy groups related states. Queries leverage both levels for rich business logic without complexity.

## Example 90: Concurrent State Machines

Concurrent FSMs model multiple independent state machines within one entity. Example: Contract with separate state machines for contract status, compliance status, and audit status.

**Concurrent State Diagram:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC
graph TD
    subgraph "Contract FSM"
        C1[DRAFT] --> C2[ACTIVE]
        C2 --> C3[EXPIRED]
    end

    subgraph "Compliance FSM"
        P1[PENDING] --> P2[PASSED]
        P1 --> P3[FAILED]
    end

    subgraph "Audit FSM"
        A1[SCHEDULED] --> A2[IN_PROGRESS]
        A2 --> A3[COMPLETED]
    end

    style C1 fill:#0173B2,color:#fff
    style C2 fill:#0173B2,color:#fff
    style C3 fill:#0173B2,color:#fff
    style P1 fill:#029E73,color:#fff
    style P2 fill:#029E73,color:#fff
    style P3 fill:#029E73,color:#fff
    style A1 fill:#DE8F05,color:#fff
    style A2 fill:#DE8F05,color:#fff
    style A3 fill:#DE8F05,color:#fff
```

**Code:**

```java
// Contract FSM states
public enum ContractStatus {     // => First FSM: contract lifecycle
    DRAFT,                       // => Contract being drafted
    ACTIVE,                      // => Contract active
    EXPIRED;                     // => Contract expired
}

// Compliance FSM states
public enum ComplianceStatus {   // => Second FSM: compliance checks
    PENDING,                     // => Compliance check pending
    PASSED,                      // => Compliance passed
    FAILED;                      // => Compliance failed
}

// Audit FSM states
public enum AuditStatus {        // => Third FSM: audit lifecycle
    SCHEDULED,                   // => Audit scheduled
    IN_PROGRESS,                 // => Audit in progress
    COMPLETED;                   // => Audit completed
}

// Entity with three concurrent FSMs
public class Contract {          // => Three independent state machines
                                 // => Concurrent: each FSM operates independently
    private final String id;
    private final String partyA;
    private final String partyB;

    // Three independent state machines
    private ContractStatus contractStatus; // => FSM 1: contract lifecycle
    private ComplianceStatus complianceStatus; // => FSM 2: compliance checks
    private AuditStatus auditStatus; // => FSM 3: audit lifecycle

    private LocalDateTime lastContractTransition;
    private LocalDateTime lastComplianceTransition;
    private LocalDateTime lastAuditTransition;

    public Contract(String id, String partyA, String partyB) {
        this.id = id;
        this.partyA = partyA;
        this.partyB = partyB;

        // Initialize all FSMs independently
        this.contractStatus = ContractStatus.DRAFT;
                                 // => FSM 1 starts at DRAFT
        this.complianceStatus = ComplianceStatus.PENDING;
                                 // => FSM 2 starts at PENDING
        this.auditStatus = AuditStatus.SCHEDULED;
                                 // => FSM 3 starts at SCHEDULED
                                 // => All three FSMs initialized independently

        this.lastContractTransition = LocalDateTime.now();
        this.lastComplianceTransition = LocalDateTime.now();
        this.lastAuditTransition = LocalDateTime.now();
    }

    // Contract FSM transitions (independent of other FSMs)
    public void activateContract() { // => FSM 1 transition: DRAFT → ACTIVE
        if (contractStatus != ContractStatus.DRAFT) {
            throw new IllegalStateException(
                "Can only activate draft contracts"
            );
        }
        this.contractStatus = ContractStatus.ACTIVE;
                                 // => FSM 1 state changes
                                 // => FSM 2 and FSM 3 remain unchanged
        this.lastContractTransition = LocalDateTime.now();
    }

    public void expireContract() { // => FSM 1 transition: ACTIVE → EXPIRED
        if (contractStatus != ContractStatus.ACTIVE) {
            throw new IllegalStateException(
                "Can only expire active contracts"
            );
        }
        this.contractStatus = ContractStatus.EXPIRED;
        this.lastContractTransition = LocalDateTime.now();
    }

    // Compliance FSM transitions (independent of contract status)
    public void passCompliance() { // => FSM 2 transition: PENDING → PASSED
                                 // => Contract can be DRAFT, ACTIVE, or EXPIRED
                                 // => Compliance FSM operates independently
        if (complianceStatus != ComplianceStatus.PENDING) {
            throw new IllegalStateException(
                "Compliance check already completed"
            );
        }
        this.complianceStatus = ComplianceStatus.PASSED;
                                 // => FSM 2 state changes
                                 // => FSM 1 and FSM 3 remain unchanged
        this.lastComplianceTransition = LocalDateTime.now();
    }

    public void failCompliance() { // => FSM 2 transition: PENDING → FAILED
        if (complianceStatus != ComplianceStatus.PENDING) {
            throw new IllegalStateException(
                "Compliance check already completed"
            );
        }
        this.complianceStatus = ComplianceStatus.FAILED;
        this.lastComplianceTransition = LocalDateTime.now();
    }

    // Audit FSM transitions (independent of contract and compliance)
    public void startAudit() {   // => FSM 3 transition: SCHEDULED → IN_PROGRESS
                                 // => Audit can run regardless of contract/compliance status
        if (auditStatus != AuditStatus.SCHEDULED) {
            throw new IllegalStateException(
                "Audit already started or completed"
            );
        }
        this.auditStatus = AuditStatus.IN_PROGRESS;
                                 // => FSM 3 state changes
                                 // => FSM 1 and FSM 2 remain unchanged
        this.lastAuditTransition = LocalDateTime.now();
    }

    public void completeAudit() { // => FSM 3 transition: IN_PROGRESS → COMPLETED
        if (auditStatus != AuditStatus.IN_PROGRESS) {
            throw new IllegalStateException(
                "Audit not in progress"
            );
        }
        this.auditStatus = AuditStatus.COMPLETED;
        this.lastAuditTransition = LocalDateTime.now();
    }

    // Combined queries across multiple FSMs
    public boolean isFullyCompliant() { // => Query across FSMs
        return contractStatus == ContractStatus.ACTIVE &&
               complianceStatus == ComplianceStatus.PASSED;
                                 // => Combine states from FSM 1 and FSM 2
    }                            // => Business logic: requires both FSMs in specific states

    public boolean requiresAudit() { // => Complex query across all FSMs
        return contractStatus == ContractStatus.ACTIVE &&
               complianceStatus == ComplianceStatus.PASSED &&
               auditStatus == AuditStatus.SCHEDULED;
                                 // => Check all three FSMs
    }

    public String getOverallStatus() { // => Human-readable status combining all FSMs
        return String.format(    // => Format: CONTRACT.COMPLIANCE.AUDIT
            "%s.%s.%s",
            contractStatus,      // => FSM 1 state
            complianceStatus,    // => FSM 2 state
            auditStatus          // => FSM 3 state
        );                       // => Example: "ACTIVE.PASSED.IN_PROGRESS"
    }

    // Getters
    public ContractStatus getContractStatus() { return contractStatus; }
    public ComplianceStatus getComplianceStatus() { return complianceStatus; }
    public AuditStatus getAuditStatus() { return auditStatus; }
}

// Example usage demonstrating independence
Contract contract = new Contract("CNT-001", "CompanyA", "CompanyB");
                                 // => contractStatus=DRAFT, complianceStatus=PENDING, auditStatus=SCHEDULED

// FSM 1 transition (does not affect FSM 2 or FSM 3)
contract.activateContract();     // => contractStatus=ACTIVE
System.out.println(contract.getOverallStatus()); // => "ACTIVE.PENDING.SCHEDULED"

// FSM 3 transition (does not affect FSM 1 or FSM 2)
contract.startAudit();           // => auditStatus=IN_PROGRESS
System.out.println(contract.getOverallStatus()); // => "ACTIVE.PENDING.IN_PROGRESS"

// FSM 2 transition (does not affect FSM 1 or FSM 3)
contract.passCompliance();       // => complianceStatus=PASSED
System.out.println(contract.getOverallStatus()); // => "ACTIVE.PASSED.IN_PROGRESS"

// Combined query
System.out.println(contract.isFullyCompliant()); // => true (FSM 1=ACTIVE && FSM 2=PASSED)

// FSM 3 transition
contract.completeAudit();        // => auditStatus=COMPLETED
System.out.println(contract.getOverallStatus()); // => "ACTIVE.PASSED.COMPLETED"
```

**Key Takeaway**: Concurrent FSMs model multiple independent state dimensions within one entity. Each FSM transitions independently without affecting others. Queries can combine states across FSMs for complex business logic.

**Why It Matters**: Concurrent FSMs avoid state explosion from cross-product of dimensions. Instead of 27 combined states (3×3×3), three independent FSMs manage complexity separately. Business rules combine FSM states as needed, maintaining clarity and testability.

## Example 91: Testing FSM Transition Coverage

Comprehensive FSM testing covers all valid transitions, all invalid transitions, guard conditions, and terminal states. Test strategy ensures FSM correctness and prevents regression.

**Code:**

```java
import org.junit.jupiter.api.*;
import static org.assertj.core.api.Assertions.*;

class OrderStateMachineTest {   // => Comprehensive FSM test suite

    // Test all valid transitions
    @Nested
    @DisplayName("Valid State Transitions")
    class ValidTransitionsTest { // => Test success paths

        @Test
        void testPendingToApproved() { // => PENDING → APPROVED transition
            OrderState state = OrderState.PENDING; // => Initial state
            OrderState next = state.approve(); // => Invoke transition
            assertThat(next).isEqualTo(OrderState.APPROVED);
                                 // => Verify next state correct
        }                        // => Test one valid transition

        @Test
        void testApprovedToProcessing() { // => APPROVED → PROCESSING
            OrderState state = OrderState.APPROVED;
            OrderState next = state.process();
            assertThat(next).isEqualTo(OrderState.PROCESSING);
        }

        @Test
        void testProcessingToCompleted() { // => PROCESSING → COMPLETED
            OrderState state = OrderState.PROCESSING;
            OrderState next = state.complete();
            assertThat(next).isEqualTo(OrderState.COMPLETED);
        }

        @Test
        void testProcessingToFailed() { // => PROCESSING → FAILED
            OrderState state = OrderState.PROCESSING;
            OrderState next = state.fail("Inventory issue");
            assertThat(next).isEqualTo(OrderState.FAILED);
        }

        @Test
        void testPendingToRejected() { // => PENDING → REJECTED
            OrderState state = OrderState.PENDING;
            OrderState next = state.reject("Customer cancelled");
            assertThat(next).isEqualTo(OrderState.REJECTED);
        }

        @Test
        void testApprovedToCancelled() { // => APPROVED → CANCELLED
            OrderState state = OrderState.APPROVED;
            OrderState next = state.cancel("Out of stock");
            assertThat(next).isEqualTo(OrderState.CANCELLED);
        }                        // => All 6 valid transitions tested
    }

    // Test all invalid transitions
    @Nested
    @DisplayName("Invalid State Transitions")
    class InvalidTransitionsTest { // => Test failure paths

        @Test
        void testCannotProcessFromPending() { // => Invalid: PENDING → PROCESSING
            OrderState state = OrderState.PENDING;
            assertThrows(IllegalStateException.class, () -> {
                state.process(); // => Should throw exception
            });                  // => Verify exception thrown
        }                        // => Invalid transition rejected

        @Test
        void testCannotApproveFromProcessing() { // => Invalid: PROCESSING → APPROVED
            OrderState state = OrderState.PROCESSING;
            assertThrows(IllegalStateException.class, () -> {
                state.approve();
            });
        }

        @Test
        void testCannotCompleteFromPending() { // => Invalid: PENDING → COMPLETED
            OrderState state = OrderState.PENDING;
            assertThrows(IllegalStateException.class, () -> {
                state.complete();
            });
        }

        @Test
        void testCannotCancelFromCompleted() { // => Invalid: COMPLETED → CANCELLED
            OrderState state = OrderState.COMPLETED;
            assertThrows(IllegalStateException.class, () -> {
                state.cancel("Too late");
            });
        }                        // => Terminal state rejects transitions
    }

    // Test terminal states reject all transitions
    @Nested
    @DisplayName("Terminal State Invariants")
    class TerminalStateTest {    // => Terminal states must reject all transitions

        @Test
        void testCompletedRejectsAllTransitions() { // => COMPLETED is terminal
            OrderState state = OrderState.COMPLETED;

            assertThrows(IllegalStateException.class, () -> state.approve());
            assertThrows(IllegalStateException.class, () -> state.process());
            assertThrows(IllegalStateException.class, () -> state.complete());
            assertThrows(IllegalStateException.class, () -> state.fail(""));
            assertThrows(IllegalStateException.class, () -> state.cancel(""));
                                 // => All transitions rejected
            assertThat(state.isTerminal()).isTrue();
                                 // => Verify terminal flag
        }

        @Test
        void testFailedRejectsAllTransitions() { // => FAILED is terminal
            OrderState state = OrderState.FAILED;

            assertThrows(IllegalStateException.class, () -> state.approve());
            assertThrows(IllegalStateException.class, () -> state.process());
            assertThrows(IllegalStateException.class, () -> state.complete());

            assertThat(state.isTerminal()).isTrue();
        }
    }

    // Test guard conditions (business rules)
    @Nested
    @DisplayName("Guard Conditions")
    class GuardConditionsTest {  // => Test precondition validation

        @Test
        void testApproveRejectsNegativeAmount() { // => Guard: amount > 0
            Order order = new Order("ORD-001", "CUST-123", new BigDecimal("-10"));
                                 // => Invalid order (negative amount)

            assertThrows(BusinessRuleException.class, () -> {
                order.approve(); // => Guard should reject
            });                  // => Verify guard exception thrown
            assertThat(order.getState()).isEqualTo(OrderState.PENDING);
                                 // => Verify state unchanged (guard prevented transition)
        }

        @Test
        void testApproveRejectsNullCustomer() { // => Guard: customer required
            Order order = new Order("ORD-002", null, new BigDecimal("100"));
                                 // => Invalid order (null customer)

            assertThrows(BusinessRuleException.class, () -> {
                order.approve();
            });
            assertThat(order.getState()).isEqualTo(OrderState.PENDING);
                                 // => State unchanged
        }

        @Test
        void testProcessRejectsInsufficientInventory() { // => Guard: inventory check
            Order order = new Order("ORD-003", "CUST-123", new BigDecimal("100"));
            order.approve();     // => State now APPROVED
            inventoryService.clearStock(); // => Setup: no inventory

            assertThrows(BusinessRuleException.class, () -> {
                order.process(); // => Guard should reject (no inventory)
            });
            assertThat(order.getState()).isEqualTo(OrderState.APPROVED);
                                 // => State unchanged (guard prevented transition)
        }
    }

    // Test entry actions execute
    @Nested
    @DisplayName("Entry Actions")
    class EntryActionsTest {     // => Test side effects on transitions

        @Test
        void testApproveLogsAuditEvent() { // => Entry action: audit logging
            Order order = new Order("ORD-004", "CUST-123", new BigDecimal("100"));

            order.approve();     // => Transition PENDING → APPROVED

            // Verify audit log written
            assertThat(auditLog.getEvents()).anyMatch(event ->
                event.contains("Order ORD-004 approved")
            );                   // => Entry action executed
        }

        @Test
        void testProcessReservesInventory() { // => Entry action: inventory reservation
            Order order = new Order("ORD-005", "CUST-123", new BigDecimal("100"));
            order.approve();

            order.process();     // => Transition APPROVED → PROCESSING

            // Verify inventory reserved
            assertThat(inventoryService.isReserved(order)).isTrue();
                                 // => Entry action executed
        }

        @Test
        void testFailReleasesInventory() { // => Entry action: cleanup
            Order order = new Order("ORD-006", "CUST-123", new BigDecimal("100"));
            order.approve();
            order.process();     // => Inventory now reserved

            order.fail("Shipping failed"); // => Transition PROCESSING → FAILED

            // Verify inventory released
            assertThat(inventoryService.isReserved(order)).isFalse();
                                 // => Entry action executed (cleanup)
        }
    }

    // Test full workflow scenarios
    @Nested
    @DisplayName("Workflow Scenarios")
    class WorkflowTest {         // => Integration tests across multiple transitions

        @Test
        void testSuccessfulOrderWorkflow() { // => Happy path: PENDING → COMPLETED
            Order order = new Order("ORD-007", "CUST-123", new BigDecimal("100"));

            // Full workflow
            order.approve();     // => PENDING → APPROVED
            assertThat(order.getState()).isEqualTo(OrderState.APPROVED);

            order.process();     // => APPROVED → PROCESSING
            assertThat(order.getState()).isEqualTo(OrderState.PROCESSING);

            order.complete();    // => PROCESSING → COMPLETED
            assertThat(order.getState()).isEqualTo(OrderState.COMPLETED);
            assertThat(order.isTerminal()).isTrue();
                                 // => Terminal state reached
        }

        @Test
        void testFailedOrderWorkflow() { // => Failure path: PENDING → FAILED
            Order order = new Order("ORD-008", "CUST-123", new BigDecimal("100"));

            order.approve();
            order.process();
            order.fail("Payment declined"); // => Transition to FAILED

            assertThat(order.getState()).isEqualTo(OrderState.FAILED);
            assertThat(order.isTerminal()).isTrue();
            assertThat(order.getFailureReason()).contains("Payment declined");
                                 // => Failure reason captured
        }

        @Test
        void testCancelledOrderWorkflow() { // => Cancellation path
            Order order = new Order("ORD-009", "CUST-123", new BigDecimal("100"));

            order.approve();
            order.cancel("Customer requested"); // => APPROVED → CANCELLED

            assertThat(order.getState()).isEqualTo(OrderState.CANCELLED);
            assertThat(order.isTerminal()).isTrue();
        }
    }
}
```

**Key Takeaway**: Comprehensive FSM testing covers valid transitions, invalid transitions, guard conditions, entry actions, and full workflows. Nested test classes organize tests by category. Test coverage ensures FSM correctness and prevents regression when adding states.

**Why It Matters**: FSMs encode business rules in state transitions. Testing all transitions validates rule enforcement. Guard tests verify preconditions. Entry action tests verify side effects execute atomically. Workflow tests validate end-to-end scenarios, catching integration issues.

## Concurrency and Parallelism

### Example 92: Virtual Threads (Java 21+)

Virtual threads are lightweight threads managed by the JVM, enabling millions of concurrent tasks without the overhead of traditional platform threads. Unlike platform threads (1:1 OS thread mapping, ~1MB stack), virtual threads unmount during I/O operations, freeing carrier threads for other work.

**Virtual Thread Lifecycle:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
stateDiagram-v2
    [*] --> Created: Thread.ofVirtual().start()
    Created --> Mounted: Schedule on carrier thread
    Mounted --> Running: Execute code
    Running --> Unmounted: I/O operation (frees carrier)
    Unmounted --> Mounted: I/O complete (remount)
    Running --> [*]: Task complete

    note right of Mounted
        Consumes carrier thread
        CPU-bound work executes here
    end note

    note right of Unmounted
        Frees carrier thread
        Thousands can wait concurrently
    end note
```

**Code**:

```java
import java.util.concurrent.*;

// Method 1: Quick virtual thread start
Thread vThread = Thread.startVirtualThread(() -> {
                                 // => Creates virtual thread and starts immediately
                                 // => Managed by JVM ForkJoinPool (carrier threads)
    System.out.println("Running on: " + Thread.currentThread());
                                 // => Output: VirtualThread[#21]/runnable@ForkJoinPool-1-worker-1
                                 // => Shows virtual thread ID and carrier thread
});
vThread.join();                  // => Waits for virtual thread to complete
                                 // => Blocking operation on main thread

// Method 2: Virtual thread builder (more control)
Thread vThread2 = Thread.ofVirtual()
                                 // => Creates virtual thread builder
    .name("data-processor")      // => Sets thread name for debugging
    .start(() -> {               // => Starts virtual thread with task
        System.out.println("Named: " + Thread.currentThread());
                                 // => Output: VirtualThread[#22,data-processor]/runnable@ForkJoinPool-1-worker-2
    });
vThread2.join();                 // => Waits for named virtual thread to complete

// Method 3: ExecutorService (recommended for multiple tasks)
try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
                                 // => Creates executor that creates one virtual thread per task
                                 // => try-with-resources ensures proper shutdown
                                 // => No fixed thread pool limit (creates threads on demand)
    executor.submit(() -> {      // => Submits task to executor
                                 // => Executor creates new virtual thread for this task
        System.out.println("Task 1: " + Thread.currentThread());
                                 // => Each task gets its own virtual thread
    });
    executor.submit(() -> {      // => Second task, second virtual thread
        System.out.println("Task 2: " + Thread.currentThread());
                                 // => Can create millions without memory exhaustion
    });
}                                // => Auto-closes executor, awaits task completion
                                 // => All virtual threads complete before exit
```

**Key Takeaway**: Virtual threads enable massive concurrency for I/O-bound tasks. Use `Thread.startVirtualThread()` for quick starts, `Thread.ofVirtual()` for control, and `Executors.newVirtualThreadPerTaskExecutor()` for managing multiple tasks. Virtual threads unmount during I/O, allowing carrier threads to serve other virtual threads.

**Why It Matters**: Traditional platform threads limit scalability (~10,000 threads). Virtual threads scale to millions, ideal for web servers handling concurrent requests. Each request gets its own virtual thread without resource exhaustion. This transforms Java's concurrency model from thread pooling to thread-per-task.

### Example 93: Thread Synchronization (synchronized vs ReentrantLock)

Thread synchronization prevents race conditions when multiple threads access shared mutable state. `synchronized` blocks provide mutual exclusion but pin virtual threads. `ReentrantLock` offers more flexibility and doesn't pin virtual threads during I/O.

**Synchronization Mechanism Comparison:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    SharedState["Shared Mutable State<br/>int count = 0"]

    subgraph Synchronized ["synchronized (pinning problem)"]
        SyncBlock["synchronized(this)<br/>pins virtual thread"]
        SyncBlock --> IOBlock["I/O operation<br/>carrier thread blocked"]
    end

    subgraph ReentrantLock ["ReentrantLock (virtual thread friendly)"]
        LockAcquire["lock.lock()<br/>does NOT pin"]
        LockAcquire --> IOFree["I/O operation<br/>carrier thread freed"]
    end

    SharedState --> Synchronized
    SharedState --> ReentrantLock

    style SharedState fill:#0173B2,color:#fff
    style SyncBlock fill:#CC78BC,color:#fff
    style IOBlock fill:#CC78BC,color:#fff
    style LockAcquire fill:#029E73,color:#fff
    style IOFree fill:#029E73,color:#fff
```

**Synchronized approach (pinning problem):**

```java
// PROBLEM: synchronized pins virtual threads to carrier threads
public class CounterSynchronized {
    private int count = 0;       // => Shared mutable state (race condition risk)

    public synchronized void increment() {
                                 // => synchronized: acquires intrinsic lock on 'this'
                                 // => Only one thread can execute this method at a time
                                 // => Virtual thread PINS to carrier thread (cannot unmount)
        count++;                 // => Read-modify-write: count=0 → read 0 → write 1
                                 // => Without synchronization: lost updates possible
                                 // => With synchronization: atomic execution guaranteed
    }                            // => Releases lock when method exits

    public synchronized int getCount() {
                                 // => synchronized read: ensures visibility
                                 // => Sees latest value written by other threads
        return count;            // => Returns current count value
    }                            // => Releases lock
}

// Usage with virtual threads (poor performance)
CounterSynchronized counter = new CounterSynchronized();
                                 // => Creates counter instance
try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
                                 // => Creates executor for virtual threads
    for (int i = 0; i < 10_000; i++) {
                                 // => Submits 10,000 concurrent increment tasks
        executor.submit(counter::increment);
                                 // => Each task increments counter
                                 // => PROBLEM: Each synchronized call pins virtual thread
                                 // => Carrier threads (8-16) become bottleneck
                                 // => Virtual thread advantage lost!
    }
}                                // => Waits for all tasks to complete
System.out.println(counter.getCount()); // => 10000 (correct, but slow)
```

**ReentrantLock approach (virtual thread friendly):**

```java
import java.util.concurrent.locks.*;

// SOLUTION: ReentrantLock does NOT pin virtual threads
public class CounterReentrantLock {
    private final Lock lock = new ReentrantLock();
                                 // => Explicit lock object (more flexible than synchronized)
    private int count = 0;       // => Shared mutable state

    public void increment() {    // => Not synchronized, uses explicit lock
        lock.lock();             // => Acquires lock (blocks if held by another thread)
                                 // => Virtual thread does NOT pin to carrier
                                 // => Can unmount during lock wait
        try {
            count++;             // => Critical section: atomic read-modify-write
                                 // => Protected by lock: mutual exclusion guaranteed
        } finally {
            lock.unlock();       // => ALWAYS unlock in finally block
                                 // => Ensures lock released even if exception thrown
        }                        // => Lock released, other threads can acquire
    }

    public int getCount() {      // => Read with explicit lock
        lock.lock();             // => Acquires lock for consistent read
        try {
            return count;        // => Returns current value (visibility guaranteed)
        } finally {
            lock.unlock();       // => Releases lock
        }
    }
}

// Usage with virtual threads (excellent performance)
CounterReentrantLock counter = new CounterReentrantLock();
try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
    for (int i = 0; i < 10_000; i++) {
        executor.submit(counter::increment);
                                 // => Each task increments counter
                                 // => ReentrantLock allows virtual threads to unmount
                                 // => Carrier threads serve thousands of virtual threads
                                 // => Full virtual thread advantage preserved!
    }
}
System.out.println(counter.getCount()); // => 10000 (correct, fast)
```

**Key Takeaway**: Use `ReentrantLock` instead of `synchronized` with virtual threads to avoid pinning. Synchronized blocks pin virtual threads to carrier threads, negating their scalability benefits. ReentrantLock provides mutual exclusion without pinning, allowing virtual threads to unmount during lock contention.

**Why It Matters**: Virtual thread pinning is a performance bottleneck. When a virtual thread enters a synchronized block and performs I/O, the carrier thread (OS thread) blocks, unable to serve other virtual threads. ReentrantLock avoids this by allowing unmounting, enabling true million-thread concurrency. Always use ReentrantLock for I/O-heavy synchronized sections.

### Example 94: Concurrent Collections (ConcurrentHashMap)

Concurrent collections provide thread-safe access without external synchronization. `ConcurrentHashMap` uses lock striping for high concurrency. Unlike `Collections.synchronizedMap()`, it allows concurrent reads and segmented writes.

**ConcurrentHashMap Architecture:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    CHM["ConcurrentHashMap<br/>16 segments (default)"]

    CHM --> Seg1["Segment 1<br/>lock 1"]
    CHM --> Seg2["Segment 2<br/>lock 2"]
    CHM --> Seg3["Segment N<br/>lock N"]

    Seg1 --> Write1["Writer Thread 1<br/>modifies Segment 1"]
    Seg2 --> Write2["Writer Thread 2<br/>modifies Segment 2"]
    Seg3 --> WriteN["Writer Thread N<br/>modifies Segment N"]

    CHM --> Readers["Multiple Reader Threads<br/>lock-free reads"]

    style CHM fill:#0173B2,color:#fff
    style Seg1 fill:#029E73,color:#fff
    style Seg2 fill:#029E73,color:#fff
    style Seg3 fill:#029E73,color:#fff
    style Write1 fill:#DE8F05,color:#fff
    style Write2 fill:#DE8F05,color:#fff
    style WriteN fill:#DE8F05,color:#fff
    style Readers fill:#029E73,color:#fff
```

**Code**:

```java
import java.util.concurrent.*;
import java.util.function.*;

// ConcurrentHashMap - thread-safe, lock striping for concurrency
ConcurrentHashMap<String, Integer> map = new ConcurrentHashMap<>();
                                 // => Creates empty concurrent map
                                 // => Uses lock striping: separate locks for segments
                                 // => Allows multiple concurrent writes to different segments
                                 // => Reads are lock-free (volatile reads)

// put - basic insertion (thread-safe)
map.put("key1", 100);            // => Inserts key1=100 into appropriate segment
                                 // => Acquires lock for segment containing "key1"
                                 // => Other segments remain unlocked (concurrent access)
                                 // => Returns null (no previous value)

// putIfAbsent - atomic check-then-insert
Integer previous = map.putIfAbsent("key1", 200);
                                 // => Atomically checks if key1 exists (thread-safe)
                                 // => Key exists with value 100
                                 // => Does NOT replace (preserves existing value)
                                 // => Returns 100 (previous value, map unchanged)
                                 // => No race condition: check and insert are atomic
System.out.println(map.get("key1")); // => 100 (unchanged)

map.putIfAbsent("key2", 200);    // => key2 doesn't exist, inserts key2=200
                                 // => Returns null (no previous value)
System.out.println(map.get("key2")); // => 200 (newly inserted)

// computeIfAbsent - compute value lazily if absent
Integer computed = map.computeIfAbsent("key3", k -> k.length() * 10);
                                 // => Checks if key3 exists (it doesn't)
                                 // => Calls lambda with key: k="key3"
                                 // => Computes "key3".length() = 4, multiplies by 10 = 40
                                 // => Inserts key3=40 atomically
                                 // => Returns computed value 40
                                 // => Thread-safe: computation and insertion are atomic
System.out.println(map.get("key3")); // => 40

// computeIfPresent - update existing value
Integer updated = map.computeIfPresent("key1", (k, v) -> v + 50);
                                 // => Checks if key1 exists (it does, value=100)
                                 // => Calls lambda with key and current value: k="key1", v=100
                                 // => Computes new value: 100 + 50 = 150
                                 // => Replaces key1=100 with key1=150 atomically
                                 // => Returns new value 150
System.out.println(map.get("key1")); // => 150 (updated)

// merge - combine values atomically
map.merge("key1", 25, (oldValue, newValue) -> oldValue + newValue);
                                 // => key1 exists with value 150
                                 // => Calls lambda with existing value (150) and new value (25)
                                 // => Computes merged value: 150 + 25 = 175
                                 // => Replaces key1=150 with key1=175 atomically
                                 // => Returns merged value 175
System.out.println(map.get("key1")); // => 175 (merged)

map.merge("key4", 99, (oldValue, newValue) -> oldValue + newValue);
                                 // => key4 doesn't exist, inserts key4=99 (no merge)
                                 // => Returns 99
System.out.println(map.get("key4")); // => 99 (inserted, not merged)
```

**Key Takeaway**: ConcurrentHashMap provides atomic operations (`putIfAbsent`, `computeIfAbsent`, `computeIfPresent`, `merge`) that eliminate race conditions in check-then-act scenarios. Lock striping enables high concurrency: multiple threads can modify different segments simultaneously. Reads are lock-free and always see published changes.

**Why It Matters**: `Collections.synchronizedMap()` uses single lock for entire map (poor concurrency). ConcurrentHashMap uses segment-level locking, allowing 16+ concurrent writers. Atomic methods replace error-prone "check-then-act" patterns with single atomic operations. Essential for high-throughput concurrent applications like caching, counters, and shared state management.

### Example 95: Executor Framework

The Executor framework decouples task submission from thread management. Executors provide thread pools for reusable threads, reducing thread creation overhead. Different executor types optimize for different workloads.

**Executor Types Comparison:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Executors["Executors Factory"]

    Executors --> Fixed["newFixedThreadPool(n)<br/>Fixed pool size"]
    Executors --> Cached["newCachedThreadPool()<br/>Grows as needed"]
    Executors --> Single["newSingleThreadExecutor()<br/>Sequential execution"]
    Executors --> Virtual["newVirtualThreadPerTaskExecutor()<br/>Unlimited virtual threads"]

    Fixed --> FixedUse["CPU-bound tasks<br/>n = CPU cores"]
    Cached --> CachedUse["I/O-bound tasks<br/>short-lived connections"]
    Single --> SingleUse["Sequential ordering<br/>background tasks"]
    Virtual --> VirtualUse["Massive I/O concurrency<br/>millions of tasks"]

    style Executors fill:#0173B2,color:#fff
    style Fixed fill:#029E73,color:#fff
    style Cached fill:#029E73,color:#fff
    style Single fill:#029E73,color:#fff
    style Virtual fill:#029E73,color:#fff
    style FixedUse fill:#DE8F05,color:#fff
    style CachedUse fill:#DE8F05,color:#fff
    style SingleUse fill:#DE8F05,color:#fff
    style VirtualUse fill:#DE8F05,color:#fff
```

**Code**:

```java
import java.util.concurrent.*;
import java.util.*;

// Fixed thread pool - bounded parallelism
ExecutorService fixedPool = Executors.newFixedThreadPool(4);
                                 // => Creates pool with 4 worker threads
                                 // => Fixed size: always 4 threads (never grows)
                                 // => Ideal for CPU-bound tasks (4 = CPU cores)
                                 // => Tasks queue when all threads busy

List<Future<Integer>> futures = new ArrayList<>();
for (int i = 0; i < 10; i++) {   // => Submits 10 tasks
    int taskId = i;
    Future<Integer> future = fixedPool.submit(() -> {
                                 // => Submits task returning Integer
                                 // => Task queued if all 4 threads busy
        System.out.println("Task " + taskId + " on " + Thread.currentThread());
                                 // => Output shows thread from pool (pool-1-thread-1/2/3/4)
        return taskId * taskId;  // => Computes square of task ID
    });
    futures.add(future);         // => Stores Future for later result retrieval
}

// Retrieve results
for (Future<Integer> future : futures) {
    try {
        Integer result = future.get();
                                 // => Blocks until task completes
                                 // => Returns computed result (taskId * taskId)
        System.out.println("Result: " + result);
    } catch (Exception e) {      // => Handles ExecutionException, InterruptedException
        e.printStackTrace();
    }
}
fixedPool.shutdown();            // => Initiates graceful shutdown
                                 // => Stops accepting new tasks
                                 // => Completes queued/running tasks
                                 // => Does NOT force-kill threads

// Cached thread pool - grows on demand
ExecutorService cachedPool = Executors.newCachedThreadPool();
                                 // => Creates pool that grows as needed
                                 // => Reuses idle threads (60s idle timeout)
                                 // => Creates new thread if all busy
                                 // => Ideal for many short-lived tasks

for (int i = 0; i < 100; i++) {  // => Submits 100 tasks
    int taskId = i;
    cachedPool.submit(() -> {    // => Submits task
                                 // => Creates new thread if all busy (grows dynamically)
                                 // => Reuses idle thread if available
        System.out.println("Task " + taskId);
        Thread.sleep(100);       // => Simulates short I/O operation
        return null;
    });
}
cachedPool.shutdown();           // => Graceful shutdown

// Single thread executor - sequential ordering
ExecutorService singleExecutor = Executors.newSingleThreadExecutor();
                                 // => Creates pool with exactly 1 worker thread
                                 // => All tasks execute sequentially (FIFO order)
                                 // => Guarantees happens-before relationship
                                 // => Ideal for sequential background tasks

for (int i = 0; i < 5; i++) {    // => Submits 5 tasks
    int taskId = i;
    singleExecutor.submit(() -> {
                                 // => Tasks execute one at a time (sequential)
        System.out.println("Sequential task " + taskId);
                                 // => Output: 0, 1, 2, 3, 4 (always in order)
    });
}
singleExecutor.shutdown();       // => Graceful shutdown
```

**Key Takeaway**: Executor framework provides thread pool management. FixedThreadPool bounds concurrency (CPU-bound tasks). CachedThreadPool grows dynamically (I/O-bound tasks). SingleThreadExecutor guarantees sequential execution. Always call `shutdown()` to release resources and complete tasks gracefully.

**Why It Matters**: Manual thread management (creating/destroying threads per task) is expensive and error-prone. Executors reuse threads, reducing overhead. Thread pools prevent resource exhaustion (limiting concurrent threads). Choosing the right executor type matches workload characteristics: fixed for CPU-bound, cached for I/O-bound, single for ordering requirements.

### Example 96: CompletableFuture Async Patterns

CompletableFuture enables non-blocking asynchronous programming with composable pipelines. Chains operations declaratively without callback hell. Supports error handling, timeouts, and combining multiple futures.

**CompletableFuture Pipeline:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    Input["Input Data"]
    Input --> Stage1["supplyAsync<br/>async operation 1"]
    Stage1 --> Stage2["thenApplyAsync<br/>transform result"]
    Stage2 --> Stage3["thenComposeAsync<br/>chain another async op"]
    Stage3 --> Stage4["thenAcceptAsync<br/>consume result"]
    Stage4 --> Complete["Completion"]

    Stage1 -.->|exception| Error["exceptionally<br/>error handler"]
    Stage2 -.->|exception| Error
    Stage3 -.->|exception| Error
    Error --> Recover["Recovered Value"]

    style Input fill:#0173B2,color:#fff
    style Stage1 fill:#029E73,color:#fff
    style Stage2 fill:#029E73,color:#fff
    style Stage3 fill:#029E73,color:#fff
    style Stage4 fill:#029E73,color:#fff
    style Complete fill:#029E73,color:#fff
    style Error fill:#CC78BC,color:#fff
    style Recover fill:#DE8F05,color:#fff
```

**Code**:

```java
import java.util.concurrent.*;
import java.time.*;

// supplyAsync - start async computation
CompletableFuture<String> future1 = CompletableFuture.supplyAsync(() -> {
                                 // => Starts async task in ForkJoinPool.commonPool()
                                 // => Returns CompletableFuture<String>
    System.out.println("Fetching data on " + Thread.currentThread());
                                 // => Output: ForkJoinPool.commonPool-worker-1
    try { Thread.sleep(1000); } catch (Exception e) {}
                                 // => Simulates I/O operation (1 second)
    return "Data from API";      // => Returns result asynchronously
});                              // => Non-blocking: continues immediately

// thenApply - transform result (sync)
CompletableFuture<Integer> future2 = future1.thenApply(data -> {
                                 // => Chains transformation after future1 completes
                                 // => Runs synchronously in completing thread
    System.out.println("Transforming on " + Thread.currentThread());
    return data.length();        // => Transforms String to Integer (length)
});                              // => Returns CompletableFuture<Integer>

// thenApplyAsync - transform result (async, separate thread)
CompletableFuture<String> future3 = future2.thenApplyAsync(length -> {
                                 // => Chains async transformation
                                 // => Runs in ForkJoinPool (different thread than future1)
    System.out.println("Formatting on " + Thread.currentThread());
    return "Length: " + length;  // => Formats Integer as String
});                              // => Returns CompletableFuture<String>

// thenAccept - consume result without returning value
future3.thenAccept(result -> {   // => Terminal operation: consumes result
    System.out.println("Final result: " + result);
                                 // => Prints final transformed result
});                              // => Returns CompletableFuture<Void>

// Block and get result
String finalResult = future3.join();
                                 // => Blocks until pipeline completes
                                 // => Returns final String result
                                 // => join() vs get(): join() throws unchecked exception
System.out.println("Blocked result: " + finalResult);
                                 // => "Length: 12" (length of "Data from API")

// exceptionally - error handling
CompletableFuture<String> futureWithError = CompletableFuture.supplyAsync(() -> {
    if (Math.random() > 0.5) {   // => 50% chance of error
        throw new RuntimeException("Random failure");
    }
    return "Success";            // => Returns on success
});

CompletableFuture<String> recovered = futureWithError.exceptionally(ex -> {
                                 // => Error handler: called only if exception thrown
    System.out.println("Error: " + ex.getMessage());
                                 // => Logs error message
    return "Default value";      // => Returns fallback value
});                              // => Returns CompletableFuture with recovered value
System.out.println(recovered.join()); // => Either "Success" or "Default value"

// Combining multiple futures - allOf
CompletableFuture<String> f1 = CompletableFuture.supplyAsync(() -> "Task 1");
CompletableFuture<String> f2 = CompletableFuture.supplyAsync(() -> "Task 2");
CompletableFuture<String> f3 = CompletableFuture.supplyAsync(() -> "Task 3");

CompletableFuture<Void> allFutures = CompletableFuture.allOf(f1, f2, f3);
                                 // => Completes when ALL futures complete
                                 // => Returns CompletableFuture<Void> (no combined value)
allFutures.join();               // => Blocks until all 3 tasks complete

// Collect results after allOf
List<String> results = Stream.of(f1, f2, f3)
    .map(CompletableFuture::join) // => Retrieves result from each future (already complete)
    .toList();                   // => Collects into List<String>
System.out.println(results);     // => [Task 1, Task 2, Task 3]

// anyOf - completes when ANY future completes
CompletableFuture<String> a1 = CompletableFuture.supplyAsync(() -> {
    try { Thread.sleep(1000); } catch (Exception e) {}
    return "Slow task";
});
CompletableFuture<String> a2 = CompletableFuture.supplyAsync(() -> {
    try { Thread.sleep(100); } catch (Exception e) {}
    return "Fast task";          // => Completes first
});

CompletableFuture<Object> anyFuture = CompletableFuture.anyOf(a1, a2);
                                 // => Completes when first future completes
                                 // => Returns CompletableFuture<Object> (any result type)
System.out.println(anyFuture.join()); // => "Fast task" (first to complete)
```

**Key Takeaway**: CompletableFuture provides composable asynchronous pipelines. `supplyAsync` starts async computation. `thenApply/thenApplyAsync` chains transformations. `exceptionally` handles errors. `allOf` waits for all futures, `anyOf` waits for first. Avoid blocking with `join()`/`get()` in async code.

**Why It Matters**: Synchronous blocking code wastes threads waiting for I/O. CompletableFuture enables non-blocking reactive programming: threads freed during I/O, serving other requests. Composable pipelines replace callback hell with declarative chains. Error handling is explicit, not scattered. Essential for high-throughput web services and microservices.

### Example 97: Atomic Variables

Atomic variables provide lock-free thread-safe operations using Compare-And-Swap (CAS) hardware instructions. Better performance than locks for simple operations (counters, flags). No blocking, no context switching overhead.

**CAS Operation Mechanism:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant Thread as Thread
    participant Memory as Memory (value=5)

    Note over Thread,Memory: CAS Operation: compareAndSet(5, 6)

    Thread->>Memory: Read current value
    Memory-->>Thread: Returns 5

    Note over Thread: Expected: 5<br/>New: 6

    Thread->>Memory: CAS(expected=5, new=6)

    alt Value still 5 (no interference)
        Memory-->>Thread: SUCCESS (updated 5→6)
        Note over Memory: value = 6
    else Value changed (another thread modified)
        Memory-->>Thread: FAILURE (value unchanged)
        Note over Thread: Retry with new current value
    end

    Note over Thread,Memory: Lock-free: no waiting, no blocking
```

**Code**:

```java
import java.util.concurrent.atomic.*;

// AtomicInteger - lock-free integer operations
AtomicInteger counter = new AtomicInteger(0);
                                 // => Creates atomic integer initialized to 0
                                 // => Uses volatile int internally (visibility)
                                 // => CAS operations for updates (lock-free)

// incrementAndGet - atomic increment
int newValue = counter.incrementAndGet();
                                 // => Atomically increments value: 0 → 1
                                 // => CAS loop: read, increment, compareAndSet
                                 // => Returns new value (1)
                                 // => Thread-safe: no lost updates even with concurrent calls
System.out.println(newValue);    // => 1

// getAndIncrement - atomic increment, returns old value
int oldValue = counter.getAndIncrement();
                                 // => Atomically increments value: 1 → 2
                                 // => Returns old value (1) before increment
System.out.println(oldValue);    // => 1
System.out.println(counter.get()); // => 2 (current value after increment)

// addAndGet - atomic add
int result = counter.addAndGet(10);
                                 // => Atomically adds 10: 2 + 10 = 12
                                 // => CAS-based addition (lock-free)
                                 // => Returns new value (12)
System.out.println(result);      // => 12

// compareAndSet - CAS operation
boolean success = counter.compareAndSet(12, 20);
                                 // => Compares current value (12) with expected (12)
                                 // => Values match, sets new value (20)
                                 // => Returns true (success)
System.out.println(success);     // => true
System.out.println(counter.get()); // => 20 (updated)

boolean failure = counter.compareAndSet(12, 30);
                                 // => Compares current value (20) with expected (12)
                                 // => Values DON'T match (20 ≠ 12)
                                 // => Does NOT update (value remains 20)
                                 // => Returns false (failure)
System.out.println(failure);     // => false
System.out.println(counter.get()); // => 20 (unchanged)

// AtomicLong - for 64-bit counters
AtomicLong longCounter = new AtomicLong(0);
                                 // => Creates atomic long initialized to 0
                                 // => Uses volatile long + CAS
longCounter.incrementAndGet();   // => Atomically increments: 0 → 1
System.out.println(longCounter.get()); // => 1

// AtomicBoolean - for flags
AtomicBoolean flag = new AtomicBoolean(false);
                                 // => Creates atomic boolean initialized to false
boolean previous = flag.getAndSet(true);
                                 // => Atomically sets flag to true
                                 // => Returns previous value (false)
System.out.println(previous);    // => false
System.out.println(flag.get());  // => true (current value)

// AtomicReference - for object references
AtomicReference<String> ref = new AtomicReference<>("initial");
                                 // => Creates atomic reference to String
                                 // => Reference updates are atomic (visibility)
ref.set("updated");              // => Atomically updates reference
                                 // => All threads see new value
System.out.println(ref.get());   // => "updated"

String oldRef = ref.getAndUpdate(s -> s.toUpperCase());
                                 // => Atomically updates reference using function
                                 // => Reads current value ("updated")
                                 // => Applies function: "updated" → "UPDATED"
                                 // => CAS-based update (retry on conflict)
                                 // => Returns old value ("updated")
System.out.println(oldRef);      // => "updated"
System.out.println(ref.get());   // => "UPDATED" (new value)
```

**Key Takeaway**: Atomic variables provide lock-free thread safety using CAS instructions. `AtomicInteger/Long` for counters, `AtomicBoolean` for flags, `AtomicReference` for objects. Operations are atomic, non-blocking, and high-performance. Use for simple concurrent updates without lock overhead.

**Why It Matters**: Locks cause context switching, blocking, and potential deadlocks. Atomic variables use hardware CAS instructions: compare current value, update if unchanged, retry if changed. No locking, no blocking, no deadlock risk. 10-100x faster than locks for simple operations. Essential for high-contention counters, statistics, and flags.

### Example 98: Volatile Fields

The `volatile` keyword ensures visibility of variable changes across threads. Without `volatile`, threads cache variables in CPU registers/caches, missing updates from other threads. Use for flags, status indicators, and single-variable state.

**Memory Visibility Without/With Volatile:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    subgraph Without ["Without volatile (stale reads)"]
        WriterCache["Writer Thread<br/>flag = true (cached)"]
        MainMemory1["Main Memory<br/>flag = false (stale)"]
        ReaderCache["Reader Thread<br/>reads cached flag = false"]

        WriterCache -->|eventually writes| MainMemory1
        ReaderCache -->|reads stale value| MainMemory1
    end

    subgraph With ["With volatile (immediate visibility)"]
        WriterVol["Writer Thread<br/>volatile flag = true"]
        MainMemory2["Main Memory<br/>flag = true (updated)"]
        ReaderVol["Reader Thread<br/>reads fresh flag = true"]

        WriterVol -->|immediate write| MainMemory2
        ReaderVol -->|reads latest value| MainMemory2
    end

    style WriterCache fill:#CC78BC,color:#fff
    style MainMemory1 fill:#CC78BC,color:#fff
    style ReaderCache fill:#CC78BC,color:#fff
    style WriterVol fill:#029E73,color:#fff
    style MainMemory2 fill:#029E73,color:#fff
    style ReaderVol fill:#029E73,color:#fff
```

**Code**:

```java
// PROBLEM: Non-volatile flag (visibility issues)
class WorkerNonVolatile {
    private boolean stopRequested = false;
                                 // => Non-volatile: thread-local caching allowed
                                 // => Writer thread updates, reader thread may not see it

    public void requestStop() {  // => Called by main thread
        stopRequested = true;    // => Sets flag to true
                                 // => Written to main thread's cache (not main memory)
                                 // => Worker thread may never see this update!
    }

    public void doWork() {       // => Runs in worker thread
        int count = 0;
        while (!stopRequested) { // => Reads from worker thread's cache
                                 // => Cache may have stale value (false forever)
                                 // => INFINITE LOOP: never sees stopRequested=true
            count++;
                                 // => Work continues indefinitely
        }
        System.out.println("Stopped after " + count + " iterations");
    }                            // => May never print (infinite loop)
}

// Usage (demonstrates problem)
WorkerNonVolatile worker1 = new WorkerNonVolatile();
Thread workerThread1 = new Thread(worker1::doWork);
workerThread1.start();           // => Starts worker thread (infinite loop)
Thread.sleep(100);               // => Main thread sleeps 100ms
worker1.requestStop();           // => Main thread sets stopRequested=true
                                 // => Worker thread may never see this (cache coherence issue)
// Worker thread may run forever!

// SOLUTION: Volatile flag (guaranteed visibility)
class WorkerVolatile {
    private volatile boolean stopRequested = false;
                                 // => Volatile: bypasses thread-local caches
                                 // => All reads/writes go directly to main memory
                                 // => Guarantees visibility across threads

    public void requestStop() {  // => Called by main thread
        stopRequested = true;    // => Writes directly to main memory
                                 // => Immediately visible to all threads
                                 // => Happens-before guarantee
    }

    public void doWork() {       // => Runs in worker thread
        int count = 0;
        while (!stopRequested) { // => Reads from main memory (always fresh)
                                 // => Sees updates from other threads immediately
                                 // => TERMINATES when stopRequested=true
            count++;
        }
        System.out.println("Stopped after " + count + " iterations");
    }                            // => Prints count (terminates correctly)
}

// Usage (correct behavior)
WorkerVolatile worker2 = new WorkerVolatile();
Thread workerThread2 = new Thread(worker2::doWork);
workerThread2.start();           // => Starts worker thread
Thread.sleep(100);               // => Main thread sleeps 100ms
worker2.requestStop();           // => Main thread sets stopRequested=true
                                 // => Worker thread sees update immediately
workerThread2.join();            // => Worker thread terminates
                                 // => Prints "Stopped after N iterations"

// Volatile for status flags
class StatusMonitor {
    private volatile boolean ready = false;
                                 // => Volatile boolean: visibility guarantee
    private volatile String status = "IDLE";
                                 // => Volatile reference: atomic reference updates

    public void setReady() {     // => Writer method
        status = "READY";        // => Updates status (visible to all threads)
        ready = true;            // => Sets ready flag (visible to all threads)
                                 // => Happens-before: status update visible before ready=true
    }

    public void waitUntilReady() { // => Reader method
        while (!ready) {         // => Spins until ready is true
                                 // => Volatile read: sees latest value
            Thread.onSpinWait(); // => Hint to CPU: optimize spin-wait
        }
        System.out.println("Status: " + status);
                                 // => Guaranteed to see status="READY" (happens-before)
    }                            // => Prints "Status: READY"
}
```

**Key Takeaway**: Volatile ensures visibility of variable changes across threads. Without `volatile`, threads cache variables, missing updates from other threads (infinite loops, stale data). Volatile forces reads/writes to main memory, guaranteeing all threads see latest value. Use for flags, status, and single-variable coordination.

**Why It Matters**: Java Memory Model allows thread-local caching for performance. Non-volatile variables may never propagate changes between threads (data races, infinite loops). Volatile provides happens-before guarantee: writes in one thread are visible to subsequent reads in other threads. Cheaper than locks (no mutual exclusion), but limited to single-variable atomicity.

### Example 99: Thread-Local Storage

ThreadLocal provides per-thread isolated variables. Each thread has its own copy, avoiding synchronization overhead. Useful for thread-confined state (database connections, user contexts, date formatters).

**ThreadLocal Isolation:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    TL["ThreadLocal<String><br/>shared reference"]

    TL --> Thread1["Thread 1<br/>threadLocal.set('User A')"]
    TL --> Thread2["Thread 2<br/>threadLocal.set('User B')"]
    TL --> Thread3["Thread 3<br/>threadLocal.set('User C')"]

    Thread1 --> Storage1["Thread-Local Storage 1<br/>value = 'User A'"]
    Thread2 --> Storage2["Thread-Local Storage 2<br/>value = 'User B'"]
    Thread3 --> Storage3["Thread-Local Storage 3<br/>value = 'User C'"]

    Storage1 --> Read1["Thread 1 reads<br/>gets 'User A' only"]
    Storage2 --> Read2["Thread 2 reads<br/>gets 'User B' only"]
    Storage3 --> Read3["Thread 3 reads<br/>gets 'User C' only"]

    style TL fill:#0173B2,color:#fff
    style Thread1 fill:#029E73,color:#fff
    style Thread2 fill:#029E73,color:#fff
    style Thread3 fill:#029E73,color:#fff
    style Storage1 fill:#DE8F05,color:#fff
    style Storage2 fill:#DE8F05,color:#fff
    style Storage3 fill:#DE8F05,color:#fff
    style Read1 fill:#DE8F05,color:#fff
    style Read2 fill:#DE8F05,color:#fff
    style Read3 fill:#DE8F05,color:#fff
```

**Code**:

```java
import java.util.concurrent.*;
import java.text.*;

// ThreadLocal for user context (per-thread isolation)
public class UserContext {
    private static final ThreadLocal<String> currentUser = new ThreadLocal<>();
                                 // => ThreadLocal variable: one value per thread
                                 // => Each thread has isolated storage
                                 // => No synchronization needed (thread-confined)

    public static void setUser(String username) {
        currentUser.set(username); // => Sets value for current thread only
                                 // => Other threads unaffected (isolated storage)
    }

    public static String getUser() {
        return currentUser.get(); // => Gets value for current thread only
                                 // => Returns null if not set for this thread
    }

    public static void clearUser() {
        currentUser.remove();    // => Removes value for current thread
                                 // => IMPORTANT: prevents memory leaks in thread pools
    }
}

// Usage: Multiple threads with isolated contexts
ExecutorService executor = Executors.newFixedThreadPool(3);
                                 // => Creates thread pool with 3 threads
                                 // => Threads are reused across tasks

for (int i = 1; i <= 5; i++) {   // => Submits 5 tasks to 3-thread pool
    String username = "User" + i;
    executor.submit(() -> {
        UserContext.setUser(username);
                                 // => Sets user for THIS thread only
                                 // => Other threads see their own values

        System.out.println(Thread.currentThread().getName() +
                          " processing request for " + UserContext.getUser());
                                 // => Each thread prints its own user
                                 // => Thread isolation: no interference

        try { Thread.sleep(100); } catch (Exception e) {}
                                 // => Simulates work

        System.out.println(Thread.currentThread().getName() +
                          " completed request for " + UserContext.getUser());
                                 // => Still sees same user (thread-confined)

        UserContext.clearUser(); // => CRITICAL: remove after use
                                 // => Prevents memory leak when thread reused
    });
}
executor.shutdown();             // => Graceful shutdown

// ThreadLocal with initializer (default value)
private static final ThreadLocal<SimpleDateFormat> dateFormatter =
    ThreadLocal.withInitial(() -> new SimpleDateFormat("yyyy-MM-dd"));
                                 // => Creates formatter lazily per thread
                                 // => SimpleDateFormat is NOT thread-safe
                                 // => ThreadLocal makes it safe (one per thread)

// Usage: Thread-safe date formatting
public String formatDate(java.util.Date date) {
    return dateFormatter.get().format(date);
                                 // => Gets THIS thread's formatter instance
                                 // => Safe: no other thread uses this instance
                                 // => No synchronization overhead
}

// ThreadLocal for request context (web applications)
public class RequestContext {
    private static final ThreadLocal<String> requestId = new ThreadLocal<>();
    private static final ThreadLocal<String> clientIp = new ThreadLocal<>();

    public static void initialize(String reqId, String ip) {
        requestId.set(reqId);    // => Sets request ID for this thread
        clientIp.set(ip);        // => Sets client IP for this thread
    }

    public static String getRequestId() {
        return requestId.get();  // => Gets request ID for this thread
    }

    public static String getClientIp() {
        return clientIp.get();   // => Gets client IP for this thread
    }

    public static void cleanup() {
        requestId.remove();      // => IMPORTANT: cleanup after request
        clientIp.remove();       // => Prevents memory leak in servlet container
    }
}

// Usage in web request handler
public void handleRequest(String reqId, String clientIp) {
    try {
        RequestContext.initialize(reqId, clientIp);
                                 // => Sets context for this thread/request

        // Request processing
        processRequest();        // => All methods see same RequestContext

    } finally {
        RequestContext.cleanup(); // => ALWAYS cleanup in finally
                                 // => Critical for thread pool environments
    }
}
```

**Key Takeaway**: ThreadLocal provides per-thread isolated storage. Each thread has its own copy of the variable, avoiding synchronization. Use for thread-confined state: user contexts, formatters, connections. Always call `remove()` after use to prevent memory leaks in thread pools.

**Why It Matters**: Sharing state across threads requires synchronization (performance cost). ThreadLocal eliminates synchronization by giving each thread its own copy. Ideal for non-thread-safe objects (SimpleDateFormat, database connections). Critical for web applications: request context isolated per thread. Memory leak risk: pooled threads retain ThreadLocal values unless explicitly removed.

### Example 100: Parallel Streams

Parallel streams enable data parallelism by splitting collections across multiple threads. Uses Fork/Join framework internally. Ideal for CPU-bound bulk operations on large collections. Simple syntax: `.parallelStream()` instead of `.stream()`.

**Parallel Stream Execution:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Data["Collection<br/>100,000 elements"]

    subgraph Sequential ["Sequential Stream"]
        SeqThread["Single Thread"]
        SeqThread --> SeqProcess["Process all 100,000<br/>sequentially"]
        SeqProcess --> SeqResult["Result: 2,400ms"]
    end

    subgraph Parallel ["Parallel Stream (8 cores)"]
        FJPool["ForkJoinPool.commonPool()"]
        FJPool --> Core1["Core 1<br/>12,500 elements"]
        FJPool --> Core2["Core 2<br/>12,500 elements"]
        FJPool --> Core3["Core 3<br/>12,500 elements"]
        FJPool --> Core4["Core 4-8<br/>50,000 elements"]

        Core1 --> Merge["Merge Results"]
        Core2 --> Merge
        Core3 --> Merge
        Core4 --> Merge
        Merge --> ParResult["Result: 380ms<br/>6.3x faster"]
    end

    Data --> Sequential
    Data --> Parallel

    style Data fill:#0173B2,color:#fff
    style SeqThread fill:#DE8F05,color:#fff
    style SeqProcess fill:#DE8F05,color:#fff
    style SeqResult fill:#DE8F05,color:#fff
    style FJPool fill:#029E73,color:#fff
    style Core1 fill:#029E73,color:#fff
    style Core2 fill:#029E73,color:#fff
    style Core3 fill:#029E73,color:#fff
    style Core4 fill:#029E73,color:#fff
    style Merge fill:#029E73,color:#fff
    style ParResult fill:#029E73,color:#fff
```

**Code**:

```java
import java.util.*;
import java.util.stream.*;

// Sequential stream (baseline)
List<Integer> numbers = IntStream.range(1, 100_001)
                                 // => Generates 1 to 100,000
    .boxed()                     // => Converts int to Integer
    .toList();                   // => Collects to List

long start = System.currentTimeMillis();
List<Integer> sequentialResult = numbers.stream()
                                 // => Creates sequential stream
                                 // => All processing on single thread
    .filter(n -> n % 2 == 0)     // => Keeps only even numbers
                                 // => Processed sequentially: 1, 2, 3, 4, ...
    .map(n -> n * n)             // => Squares each even number
                                 // => Sequential: one element at a time
    .toList();                   // => Collects results
long sequentialTime = System.currentTimeMillis() - start;
System.out.println("Sequential: " + sequentialTime + "ms");
                                 // => ~2,400ms (single thread)

// Parallel stream (multi-threaded)
start = System.currentTimeMillis();
List<Integer> parallelResult = numbers.parallelStream()
                                 // => Creates parallel stream
                                 // => Uses ForkJoinPool.commonPool() (8 threads on 8-core CPU)
                                 // => Splits collection into chunks
    .filter(n -> n % 2 == 0)     // => Filters in parallel across threads
                                 // => Thread 1 processes [1-12,500], Thread 2 [12,501-25,000], ...
    .map(n -> n * n)             // => Maps in parallel across threads
                                 // => Each thread processes its chunk independently
    .toList();                   // => Merges results from all threads
                                 // => Maintains encounter order (insertion order preserved)
long parallelTime = System.currentTimeMillis() - start;
System.out.println("Parallel: " + parallelTime + "ms");
                                 // => ~380ms (8 threads)
                                 // => Speedup: 6.3x (ideal: 8x, overhead: ~1.7x)

// Verify results identical
System.out.println("Results equal: " + sequentialResult.equals(parallelResult));
                                 // => true (parallel preserves order and correctness)

// Reduction with parallel streams
int sum = numbers.parallelStream()
                                 // => Parallel stream
    .reduce(0, Integer::sum);    // => Parallel reduction
                                 // => Identity: 0 (sum starting point)
                                 // => Accumulator: Integer::sum (combines elements)
                                 // => Each thread sums its chunk, then combines results
System.out.println("Sum: " + sum); // => 5,000,050,000 (sum of 1 to 100,000)

// Stateful operation (order matters) - unordered for performance
List<Integer> unorderedResult = numbers.parallelStream()
                                 // => Parallel stream
    .unordered()                 // => Removes ordering constraint (performance boost)
                                 // => Results may be in different order
    .filter(n -> n % 2 == 0)
    .map(n -> n * n)
    .toList();
System.out.println("Unordered size: " + unorderedResult.size());
                                 // => 50,000 (correct count, but order not guaranteed)
                                 // => Faster: skips ordering overhead

// When NOT to use parallel streams
List<Integer> smallList = List.of(1, 2, 3, 4, 5);
                                 // => Small collection (5 elements)
int smallSum = smallList.parallelStream()
                                 // => Parallel overhead NOT worth it
                                 // => Thread coordination cost > computation cost
    .reduce(0, Integer::sum);
System.out.println("Small sum: " + smallSum);
                                 // => Correct result (15), but slower than sequential
                                 // => Rule: Use parallel for 10,000+ elements

// I/O-bound operations - DON'T use parallel streams
List<String> urls = List.of("http://api1.com", "http://api2.com", "http://api3.com");
// WRONG: Parallel stream blocks threads during I/O
List<String> responses = urls.parallelStream()
                                 // => Blocks ForkJoinPool threads (bad!)
    .map(url -> fetchUrl(url))   // => I/O operation blocks thread
                                 // => Wastes CPU resources (threads waiting)
    .toList();
// RIGHT: Use virtual threads + CompletableFuture for I/O
List<CompletableFuture<String>> futures = urls.stream()
    .map(url -> CompletableFuture.supplyAsync(() -> fetchUrl(url)))
                                 // => Async I/O: doesn't block threads
    .toList();                   // => Use virtual threads for I/O concurrency
```

**Key Takeaway**: Parallel streams split collections across CPU cores for data parallelism. Use for CPU-bound operations on large collections (10,000+ elements). Performance scales with core count. Avoid for small collections (overhead exceeds benefit) and I/O-bound operations (use virtual threads instead).

**Why It Matters**: Single-threaded processing leaves CPU cores idle. Parallel streams utilize all cores, reducing processing time linearly with core count. Ideal for bulk transformations, filtering, and aggregations on large datasets. Simple API: `.parallelStream()` instead of `.stream()`. Caution: stateful operations, small collections, and I/O-bound tasks perform worse with parallel streams.

## Web Services and APIs

Java excels at building production web services through mature frameworks (Spring Boot, Jakarta EE), type-safe contracts, and rich ecosystem integration. This section covers REST APIs, GraphQL, gRPC, and modern API patterns.

### Example 101: Basic REST Controller with Spring Boot

**Context**: RESTful APIs follow HTTP semantics with resource-oriented design. Spring Boot @RestController combines @Controller and @ResponseBody for automatic JSON serialization.

```java
import org.springframework.web.bind.annotation.*;
import org.springframework.http.*;
import java.util.*;

@RestController                      // => Marks class as REST endpoint
                                     // => Combines @Controller + @ResponseBody
@RequestMapping("/api/v1/products") // => Base path for all endpoints
                                     // => Results in /api/v1/products/*
public class ProductController {

    @GetMapping                      // => Maps HTTP GET to method
                                     // => URL: GET /api/v1/products
    public ResponseEntity<List<Product>> listProducts() {
        List<Product> products = List.of(
                                     // => Sample data (replace with service call)
            new Product("1", "Laptop", 999.99),
            new Product("2", "Mouse", 29.99)
        );
        return ResponseEntity.ok(products);
                                     // => Returns 200 OK with JSON body
                                     // => Spring auto-serializes to JSON
    }

    @GetMapping("/{id}")             // => Path variable: {id}
                                     // => URL: GET /api/v1/products/123
    public ResponseEntity<Product> getProduct(@PathVariable String id) {
                                     // => @PathVariable extracts {id} from URL
                                     // => Type-safe: id is String
        if (id.equals("1")) {
            return ResponseEntity.ok(new Product("1", "Laptop", 999.99));
                                     // => 200 OK with product JSON
        }
        return ResponseEntity.notFound().build();
                                     // => 404 Not Found (no body)
    }

    @PostMapping                     // => Maps HTTP POST to method
                                     // => URL: POST /api/v1/products
    public ResponseEntity<Product> createProduct(
        @RequestBody Product product // => @RequestBody deserializes JSON to Product
                                     // => Automatic Jackson JSON parsing
    ) {
        product.setId(UUID.randomUUID().toString());
                                     // => Generate new ID
        return ResponseEntity
            .created(URI.create("/api/v1/products/" + product.getId()))
                                     // => 201 Created with Location header
                                     // => Location: /api/v1/products/{id}
            .body(product);          // => Returns created product JSON
    }
}

record Product(String id, String name, double price) {
                                     // => Immutable DTO
                                     // => Auto JSON serialization
    public void setId(String id) {   // => Workaround for records
        // In production, use builder pattern or mutable DTO
    }
}
```

**Output**:

```http
GET /api/v1/products HTTP/1.1
Response: 200 OK
[
  {"id":"1","name":"Laptop","price":999.99},
  {"id":"2","name":"Mouse","price":29.99}
]

POST /api/v1/products HTTP/1.1
Request: {"name":"Keyboard","price":79.99}
Response: 201 Created
Location: /api/v1/products/abc-123
{"id":"abc-123","name":"Keyboard","price":79.99}
```

**Key Takeaway**: Spring Boot @RestController provides zero-config REST endpoints with automatic JSON serialization, HTTP method mapping, and path variables. Use @GetMapping/@PostMapping/@PutMapping/@DeleteMapping for HTTP verbs. ResponseEntity controls status codes and headers.

**Why It Matters**: REST APIs are ubiquitous in modern applications. Spring Boot eliminates boilerplate: no XML config, no manual serialization, no servlet configuration. Type-safe contracts via Java classes ensure compile-time validation. Production-ready features (validation, exception handling, content negotiation) integrate seamlessly. 80% of Java web services use Spring Boot REST for rapid API development.

### Example 102: Request Validation with Bean Validation

**Context**: Invalid input causes runtime errors. Bean Validation (Jakarta Validation) provides declarative constraints (@NotNull, @Size, @Pattern) with automatic validation at API boundaries.

```java
import jakarta.validation.constraints.*;
import jakarta.validation.*;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.*;
import java.util.*;

public record CreateUserRequest(
    @NotNull(message = "Username is required")
                                     // => Null check: username != null
                                     // => Custom error message
    @Size(min = 3, max = 20, message = "Username must be 3-20 characters")
                                     // => Length validation
                                     // => Triggers if username.length() < 3 or > 20
    @Pattern(regexp = "^[a-zA-Z0-9_]+$", message = "Username must be alphanumeric")
                                     // => Regex validation
                                     // => Only letters, numbers, underscores
    String username,

    @NotNull(message = "Email is required")
    @Email(message = "Invalid email format")
                                     // => Email format validation
                                     // => Checks for @, domain, etc.
    String email,

    @NotNull(message = "Password is required")
    @Size(min = 8, message = "Password must be at least 8 characters")
    String password,

    @NotNull(message = "Age is required")
    @Min(value = 18, message = "Must be 18 or older")
                                     // => Numeric minimum validation
    @Max(value = 120, message = "Age cannot exceed 120")
    Integer age
) {}

@RestController
@RequestMapping("/api/v1/users")
public class UserController {

    @PostMapping
    public ResponseEntity<String> createUser(
        @Valid @RequestBody CreateUserRequest request
                                     // => @Valid triggers validation
                                     // => Checks all constraints on CreateUserRequest
                                     // => Throws MethodArgumentNotValidException if invalid
    ) {
        // If we reach here, request is valid
        return ResponseEntity
            .created(URI.create("/api/v1/users/" + request.username()))
            .body("User created: " + request.username());
    }
}

// Global exception handler for validation errors
@RestControllerAdvice                // => Global exception handling
                                     // => Applies to all @RestController classes
public class ValidationExceptionHandler {

    @ExceptionHandler(MethodArgumentNotValidException.class)
                                     // => Catches validation failures
                                     // => Triggered by @Valid annotation
    public ResponseEntity<Map<String, Object>> handleValidation(
        MethodArgumentNotValidException ex
    ) {
        List<String> errors = ex.getBindingResult()
                                     // => Extract validation errors
            .getFieldErrors()        // => Get field-level errors
            .stream()
            .map(error -> error.getField() + ": " + error.getDefaultMessage())
                                     // => Format: "username: Username is required"
            .toList();

        Map<String, Object> response = Map.of(
            "status", 400,
            "error", "Validation Failed",
            "details", errors
        );
        return ResponseEntity.badRequest().body(response);
                                     // => 400 Bad Request with error details
    }
}
```

**Output**:

```http
POST /api/v1/users HTTP/1.1
Request: {"username":"ab","email":"invalid","password":"123","age":15}
Response: 400 Bad Request
{
  "status": 400,
  "error": "Validation Failed",
  "details": [
    "username: Username must be 3-20 characters",
    "email: Invalid email format",
    "password: Password must be at least 8 characters",
    "age: Must be 18 or older"
  ]
}
```

**Key Takeaway**: Bean Validation provides declarative constraints (@NotNull, @Size, @Email, @Min/@Max, @Pattern) enforced at API boundaries via @Valid. Automatic validation before method execution prevents invalid data from reaching business logic. Use @RestControllerAdvice for global exception handling.

**Why It Matters**: Manual validation is error-prone and verbose. Bean Validation centralizes validation logic in DTOs, reducing boilerplate. Constraints are self-documenting (intent visible in field declarations). Integration with Spring Boot provides automatic 400 Bad Request responses with detailed error messages. Reusable across REST, GraphQL, and internal service calls. Industry standard for Java API validation.

### Example 103: HTTP Method Semantics and Idempotency

**Context**: HTTP methods have defined semantics. GET/HEAD are safe (no side effects). PUT/DELETE are idempotent (multiple identical requests have same effect as single request). POST is neither safe nor idempotent.

```java
import org.springframework.web.bind.annotation.*;
import org.springframework.http.*;
import java.util.*;
import java.util.concurrent.*;

@RestController
@RequestMapping("/api/v1/orders")
public class OrderController {
    private final Map<String, Order> orders = new ConcurrentHashMap<>();
                                     // => In-memory storage (use DB in production)
                                     // => Thread-safe for concurrent requests

    // GET: Safe and idempotent (read-only, no side effects)
    @GetMapping("/{id}")             // => GET /api/v1/orders/{id}
                                     // => Multiple calls return same result
    public ResponseEntity<Order> getOrder(@PathVariable String id) {
        Order order = orders.get(id);
        return order != null
            ? ResponseEntity.ok(order)
                                     // => 200 OK with order JSON
            : ResponseEntity.notFound().build();
                                     // => 404 Not Found
    }

    // POST: Neither safe nor idempotent (creates new resource each time)
    @PostMapping                     // => POST /api/v1/orders
                                     // => Multiple calls create multiple resources
    public ResponseEntity<Order> createOrder(
        @RequestBody CreateOrderRequest request
    ) {
        String id = UUID.randomUUID().toString();
                                     // => New ID for each request
                                     // => Multiple POSTs = multiple orders
        Order order = new Order(id, request.productId(), request.quantity(), "PENDING");
        orders.put(id, order);
                                     // => Stores new order

        return ResponseEntity
            .created(URI.create("/api/v1/orders/" + id))
                                     // => 201 Created (indicates resource creation)
            .body(order);            // => Returns created order
    }

    // PUT: Idempotent (replace entire resource, same result if repeated)
    @PutMapping("/{id}")             // => PUT /api/v1/orders/{id}
                                     // => Multiple identical requests = same final state
    public ResponseEntity<Order> replaceOrder(
        @PathVariable String id,
        @RequestBody UpdateOrderRequest request
    ) {
        Order order = new Order(id, request.productId(), request.quantity(), request.status());
        orders.put(id, order);       // => Replaces order entirely
                                     // => Calling twice with same data = same result

        return ResponseEntity.ok(order);
                                     // => 200 OK with replaced order
    }

    // DELETE: Idempotent (delete resource, same result if repeated)
    @DeleteMapping("/{id}")          // => DELETE /api/v1/orders/{id}
                                     // => First call deletes, subsequent calls do nothing
    public ResponseEntity<Void> deleteOrder(@PathVariable String id) {
        orders.remove(id);           // => Deletes order
                                     // => Second DELETE on same ID = already deleted
        return ResponseEntity.noContent().build();
                                     // => 204 No Content (successful deletion)
                                     // => Same 204 even if already deleted (idempotent)
    }

    // PATCH: Not idempotent (partial update, result depends on current state)
    @PatchMapping("/{id}/status")    // => PATCH /api/v1/orders/{id}/status
                                     // => Updates only status field
    public ResponseEntity<Order> updateStatus(
        @PathVariable String id,
        @RequestBody Map<String, String> update
    ) {
        Order order = orders.get(id);
        if (order == null) {
            return ResponseEntity.notFound().build();
        }

        String newStatus = update.get("status");
        Order updated = new Order(order.id(), order.productId(), order.quantity(), newStatus);
        orders.put(id, updated);     // => Partial update
                                     // => Result depends on previous state

        return ResponseEntity.ok(updated);
    }
}

record Order(String id, String productId, int quantity, String status) {}
record CreateOrderRequest(String productId, int quantity) {}
record UpdateOrderRequest(String productId, int quantity, String status) {}
```

**Output**:

```http
POST /api/v1/orders (1st call)
Response: 201 Created, order ID: abc-123

POST /api/v1/orders (2nd identical call)
Response: 201 Created, order ID: def-456 (NEW order, not idempotent)

PUT /api/v1/orders/abc-123 (1st call)
Request: {"productId":"PROD-1","quantity":5,"status":"SHIPPED"}
Response: 200 OK

PUT /api/v1/orders/abc-123 (2nd identical call)
Response: 200 OK (same result, idempotent)

DELETE /api/v1/orders/abc-123 (1st call)
Response: 204 No Content

DELETE /api/v1/orders/abc-123 (2nd call)
Response: 204 No Content (same result even if already deleted, idempotent)
```

**Key Takeaway**: HTTP methods have semantic contracts. GET is safe (read-only). PUT/DELETE are idempotent (repeatable without side effects). POST is neither (creates new resource each time). PATCH is context-dependent. Respect semantics for cache compatibility and retry safety.

**Why It Matters**: Idempotency enables safe retry logic. Network failures, timeouts, and duplicate requests won't corrupt data if operations are idempotent. PUT/DELETE can be retried freely. POST requires idempotency keys (see next example). HTTP caches rely on GET safety. Load balancers and proxies assume semantic correctness. Violating semantics breaks client expectations and infrastructure assumptions.

### Example 104: Idempotency Keys for POST Operations

**Context**: POST is not idempotent (creates new resource each time). Idempotency keys prevent duplicate processing from network retries or user double-clicks.

```java
import org.springframework.web.bind.annotation.*;
import org.springframework.http.*;
import java.util.*;
import java.util.concurrent.*;
import java.time.*;

@RestController
@RequestMapping("/api/v1/payments")
public class PaymentController {
    private final Map<String, Payment> payments = new ConcurrentHashMap<>();
                                     // => Stores payments by ID
    private final Map<String, String> idempotencyKeys = new ConcurrentHashMap<>();
                                     // => Maps idempotency key -> payment ID
                                     // => Prevents duplicate processing

    @PostMapping
    public ResponseEntity<Payment> createPayment(
        @RequestBody CreatePaymentRequest request,
        @RequestHeader("Idempotency-Key") String idempotencyKey
                                     // => Client provides unique key
                                     // => Same key = same payment (no duplicate)
    ) {
        // Check if this key was already processed
        String existingPaymentId = idempotencyKeys.get(idempotencyKey);
                                     // => Lookup by idempotency key
        if (existingPaymentId != null) {
                                     // => Key exists = already processed
            Payment existingPayment = payments.get(existingPaymentId);
                                     // => Return same payment (idempotent behavior)
            return ResponseEntity.ok(existingPayment);
                                     // => 200 OK (not 201, resource already created)
        }

        // First time seeing this key - process payment
        String paymentId = UUID.randomUUID().toString();
                                     // => Generate new payment ID
        Payment payment = new Payment(
            paymentId,
            request.amount(),
            request.currency(),
            "COMPLETED",
            LocalDateTime.now()
        );
        payments.put(paymentId, payment);
                                     // => Store payment
        idempotencyKeys.put(idempotencyKey, paymentId);
                                     // => Record idempotency key -> payment mapping
                                     // => Future requests with same key return this payment

        return ResponseEntity
            .created(URI.create("/api/v1/payments/" + paymentId))
            .body(payment);          // => 201 Created (first time only)
    }

    @GetMapping("/{id}")
    public ResponseEntity<Payment> getPayment(@PathVariable String id) {
        Payment payment = payments.get(id);
        return payment != null
            ? ResponseEntity.ok(payment)
            : ResponseEntity.notFound().build();
    }
}

record CreatePaymentRequest(double amount, String currency) {}
record Payment(String id, double amount, String currency, String status, LocalDateTime createdAt) {}
```

**Output**:

```http
POST /api/v1/payments (1st call)
Idempotency-Key: client-abc-123
Request: {"amount":100.0,"currency":"USD"}
Response: 201 Created
{"id":"payment-xyz","amount":100.0,"currency":"USD","status":"COMPLETED","createdAt":"2025-01-20T10:30:00"}

POST /api/v1/payments (2nd call, network retry with same key)
Idempotency-Key: client-abc-123
Request: {"amount":100.0,"currency":"USD"}
Response: 200 OK (same payment returned, no duplicate)
{"id":"payment-xyz","amount":100.0,"currency":"USD","status":"COMPLETED","createdAt":"2025-01-20T10:30:00"}

POST /api/v1/payments (3rd call, new key)
Idempotency-Key: client-def-456
Request: {"amount":100.0,"currency":"USD"}
Response: 201 Created (NEW payment)
{"id":"payment-abc","amount":100.0,"currency":"USD","status":"COMPLETED","createdAt":"2025-01-20T10:31:00"}
```

**Key Takeaway**: Idempotency keys make POST operations safe to retry. Client provides unique key in header (Idempotency-Key). Server checks if key was already processed. If yes, return existing resource (200 OK). If no, process request and record key (201 Created). Same key always returns same result.

**Why It Matters**: Network failures, timeouts, and user errors cause duplicate requests. Without idempotency keys, duplicate POSTs create duplicate resources (double charges, duplicate orders). Financial systems REQUIRE idempotency for payment processing. Stripe, PayPal, Square all use idempotency keys. Implementation: store key -> resource mapping with TTL (expire after 24 hours). Critical for production REST APIs handling critical operations.

### Example 105: Pagination and Filtering

**Context**: Large datasets must be paginated to avoid memory issues and slow responses. Spring Data provides Pageable abstraction for offset-based pagination with sorting and filtering.

```java
import org.springframework.web.bind.annotation.*;
import org.springframework.http.*;
import org.springframework.data.domain.*;
import java.util.*;
import java.util.stream.*;

@RestController
@RequestMapping("/api/v1/products")
public class ProductPaginationController {
    private final List<Product> allProducts;
                                     // => Full dataset (1000 products)

    public ProductPaginationController() {
        allProducts = IntStream.range(1, 1001)
                                     // => Generate 1000 products
            .mapToObj(i -> new Product(
                "PROD-" + i,
                "Product " + i,
                Math.random() * 1000,
                i % 5 == 0 ? "ELECTRONICS" : "CLOTHING"
            ))
            .toList();
    }

    @GetMapping
    public ResponseEntity<PagedResponse<Product>> listProducts(
        @RequestParam(defaultValue = "0") int page,
                                     // => Page number (0-indexed)
                                     // => Default: first page
        @RequestParam(defaultValue = "20") int size,
                                     // => Page size (items per page)
                                     // => Default: 20 items
        @RequestParam(defaultValue = "name") String sortBy,
                                     // => Sort field
                                     // => Default: sort by name
        @RequestParam(defaultValue = "ASC") Sort.Direction sortDirection,
                                     // => Sort direction (ASC or DESC)
        @RequestParam(required = false) String category
                                     // => Optional filter by category
    ) {
        Pageable pageable = PageRequest.of(page, size, Sort.by(sortDirection, sortBy));
                                     // => Creates Pageable object
                                     // => page=0, size=20, sort=name ASC

        // Filter by category if provided
        List<Product> filtered = category == null
            ? allProducts            // => No filter: all products
            : allProducts.stream()
                .filter(p -> p.category().equals(category))
                                     // => Filter by category
                .toList();

        // Apply pagination
        int start = (int) pageable.getOffset();
                                     // => Offset: page * size
                                     // => page=2, size=20 => offset=40
        int end = Math.min(start + pageable.getPageSize(), filtered.size());
                                     // => End index (exclusive)
                                     // => Handles last page (may be smaller)

        List<Product> pageContent = filtered.subList(start, end);
                                     // => Extract page slice
                                     // => Page 0, size 20 => items [0, 20)

        // Apply sorting
        pageContent = pageContent.stream()
            .sorted(getComparator(sortBy, sortDirection))
                                     // => Sort by field and direction
            .toList();

        Page<Product> pageResult = new PageImpl<>(
            pageContent,             // => Current page items
            pageable,                // => Pageable metadata
            filtered.size()          // => Total filtered items
        );

        PagedResponse<Product> response = new PagedResponse<>(
            pageContent,
            new PageMetadata(
                pageResult.getNumber(),    // => Current page number
                pageResult.getSize(),      // => Page size
                pageResult.getTotalPages(),// => Total pages
                pageResult.getTotalElements() // => Total items
            )
        );

        return ResponseEntity.ok(response);
    }

    private Comparator<Product> getComparator(String sortBy, Sort.Direction dir) {
        Comparator<Product> comp = switch (sortBy) {
            case "name" -> Comparator.comparing(Product::name);
            case "price" -> Comparator.comparing(Product::price);
            case "category" -> Comparator.comparing(Product::category);
            default -> Comparator.comparing(Product::name);
        };
        return dir == Sort.Direction.DESC ? comp.reversed() : comp;
    }
}

record Product(String id, String name, double price, String category) {}
record PagedResponse<T>(List<T> content, PageMetadata page) {}
record PageMetadata(int number, int size, int totalPages, long totalElements) {}
```

**Output**:

```http
GET /api/v1/products?page=0&size=2&sortBy=price&sortDirection=DESC&category=ELECTRONICS
Response: 200 OK
{
  "content": [
    {"id":"PROD-500","name":"Product 500","price":998.5,"category":"ELECTRONICS"},
    {"id":"PROD-250","name":"Product 250","price":987.3,"category":"ELECTRONICS"}
  ],
  "page": {
    "number": 0,
    "size": 2,
    "totalPages": 100,
    "totalElements": 200
  }
}
```

**Key Takeaway**: Pagination prevents loading entire datasets into memory. Use @RequestParam for page, size, sortBy, sortDirection. Spring Data Pageable provides offset calculation. Return metadata (current page, total pages, total items) for client-side navigation. Combine pagination with filtering for dynamic queries.

**Why It Matters**: Loading 10,000 products into memory crashes clients and overwhelms networks. Pagination limits response size (20-100 items per page). Offset-based pagination (page \* size) is simple but inefficient for large datasets (cursor-based pagination better for millions of records). Sorting + filtering enables rich queries (e.g., "ELECTRONICS sorted by price DESC"). Industry standard for list endpoints.

### Example 106: GraphQL Schema and Resolvers

**Context**: GraphQL allows clients to request exactly the fields they need in a single query. Schema defines types and operations. Resolvers fetch data for each field.

```java
// GraphQL Schema (src/main/resources/graphql/schema.graphqls)
// type Query {
//   product(id: ID!): Product
//   products(category: String, limit: Int = 10): [Product!]!
// }
//
// type Product {
//   id: ID!
//   name: String!
//   price: Float!
//   category: String!
//   reviews: [Review!]!
// }
//
// type Review {
//   id: ID!
//   rating: Int!
//   comment: String!
// }

import org.springframework.graphql.data.method.annotation.*;
import org.springframework.stereotype.*;
import java.util.*;
import java.util.stream.*;

@Controller                          // => GraphQL controller (NOT @RestController)
public class ProductGraphQLController {
    private final Map<String, Product> products;
    private final Map<String, List<Review>> reviews;

    public ProductGraphQLController() {
        products = Map.of(
            "1", new Product("1", "Laptop", 999.99, "ELECTRONICS"),
            "2", new Product("2", "Shirt", 29.99, "CLOTHING")
        );
        reviews = Map.of(
            "1", List.of(
                new Review("r1", 5, "Excellent laptop!"),
                new Review("r2", 4, "Good performance")
            ),
            "2", List.of(
                new Review("r3", 3, "Average quality")
            )
        );
    }

    @QueryMapping                    // => Maps to Query type in schema
                                     // => Method name matches schema field: product(id: ID!)
    public Product product(@Argument String id) {
                                     // => @Argument extracts id from GraphQL query
                                     // => Type-safe: id is String
        return products.get(id);     // => Returns Product or null
                                     // => GraphQL handles null (returns null in response)
    }

    @QueryMapping                    // => Maps to Query.products
    public List<Product> products(
        @Argument String category,   // => Optional filter (nullable in schema)
        @Argument Integer limit      // => Default value: 10 (in schema)
    ) {
        Stream<Product> stream = products.values().stream();
        if (category != null) {
            stream = stream.filter(p -> p.category().equals(category));
                                     // => Filter by category if provided
        }
        return stream.limit(limit != null ? limit : 10)
                                     // => Limit results (default 10)
            .toList();
    }

    @SchemaMapping(typeName = "Product", field = "reviews")
                                     // => Field resolver for Product.reviews
                                     // => Called ONLY if client requests reviews field
    public List<Review> reviews(Product product) {
                                     // => Receives parent Product object
                                     // => Fetches related reviews
        return reviews.getOrDefault(product.id(), List.of());
                                     // => Returns reviews for this product
                                     // => Empty list if no reviews
    }
}

record Product(String id, String name, double price, String category) {}
record Review(String id, int rating, String comment) {}
```

**Output**:

```graphql
# GraphQL Query 1: Fetch only id and name (selective fields)
query {
  product(id: "1") {
    id
    name
  }
}
# Response:
# {
#   "data": {
#     "product": {"id":"1","name":"Laptop"}
#   }
# }

# GraphQL Query 2: Fetch product with nested reviews (single query)
query {
  product(id: "1") {
    id
    name
    price
    reviews {
      rating
      comment
    }
  }
}
# Response:
# {
#   "data": {
#     "product": {
#       "id":"1",
#       "name":"Laptop",
#       "price":999.99,
#       "reviews":[
#         {"rating":5,"comment":"Excellent laptop!"},
#         {"rating":4,"comment":"Good performance"}
#       ]
#     }
#   }
# }

# GraphQL Query 3: Filter products by category
query {
  products(category: "ELECTRONICS", limit: 5) {
    id
    name
    price
  }
}
# Response:
# {
#   "data": {
#     "products": [
#       {"id":"1","name":"Laptop","price":999.99}
#     ]
#   }
# }
```

**Key Takeaway**: GraphQL allows selective field queries (client specifies exact data needed). @QueryMapping maps to Query type fields. @SchemaMapping resolves nested fields (e.g., Product.reviews). Single endpoint handles all queries. Clients fetch multiple resources in one request.

**Why It Matters**: REST over-fetches (returns all fields) or under-fetches (requires multiple requests). GraphQL solves both: client requests exactly needed fields in one query. Mobile apps save bandwidth (no unused fields). Frontend teams iterate without backend changes. Schema provides type safety and introspection (self-documenting API). Trade-off: more complex server implementation, harder to cache. Best for client-diverse APIs (mobile, web, desktop with different data needs).

### Example 107: gRPC Service with Protocol Buffers

**Context**: gRPC uses binary Protocol Buffers for efficient serialization. Strongly-typed service contracts defined in .proto files. Faster and more compact than JSON/REST.

```protobuf
// product_service.proto
syntax = "proto3";

package com.example.product;

option java_multiple_files = true;
option java_package = "com.example.product.grpc";

service ProductService {
  rpc GetProduct(GetProductRequest) returns (Product);
  rpc ListProducts(ListProductsRequest) returns (stream Product);
}

message Product {
  string id = 1;
  string name = 2;
  double price = 3;
  string category = 4;
}

message GetProductRequest {
  string id = 1;
}

message ListProductsRequest {
  string category = 2;
  int32 limit = 3;
}
```

```java
// Generated by protoc: ProductServiceGrpc.java
// Manual implementation:

import io.grpc.*;
import io.grpc.stub.*;
import com.example.product.grpc.*;
import java.util.*;

public class ProductServiceImpl extends ProductServiceGrpc.ProductServiceImplBase {
                                     // => Extends generated base class
                                     // => Implements service methods

    private final Map<String, Product> products;

    public ProductServiceImpl() {
        products = Map.of(
            "1", Product.newBuilder()
                                     // => Protocol Buffers builder pattern
                .setId("1")
                .setName("Laptop")
                .setPrice(999.99)
                .setCategory("ELECTRONICS")
                .build(),            // => Immutable Product message
            "2", Product.newBuilder()
                .setId("2")
                .setName("Shirt")
                .setPrice(29.99)
                .setCategory("CLOTHING")
                .build()
        );
    }

    // Unary RPC: single request -> single response
    @Override
    public void getProduct(
        GetProductRequest request,   // => Request message (contains id)
        StreamObserver<Product> responseObserver
                                     // => Callback for sending response
    ) {
        String id = request.getId(); // => Extract id from request
        Product product = products.get(id);
                                     // => Fetch product

        if (product != null) {
            responseObserver.onNext(product);
                                     // => Send response message
            responseObserver.onCompleted();
                                     // => Signal completion (success)
        } else {
            responseObserver.onError(
                Status.NOT_FOUND     // => gRPC status code
                    .withDescription("Product not found: " + id)
                    .asRuntimeException()
                                     // => Convert to exception
            );                       // => Client receives NOT_FOUND error
        }
    }

    // Server streaming: single request -> stream of responses
    @Override
    public void listProducts(
        ListProductsRequest request,
        StreamObserver<Product> responseObserver
    ) {
        String category = request.getCategory();
        int limit = request.getLimit();

        products.values().stream()
            .filter(p -> category.isEmpty() || p.getCategory().equals(category))
                                     // => Filter by category
            .limit(limit)            // => Limit results
            .forEach(product -> {
                responseObserver.onNext(product);
                                     // => Stream each product to client
                                     // => Multiple onNext calls (streaming)
            });

        responseObserver.onCompleted();
                                     // => Signal end of stream
    }
}

// Server startup
public class GrpcServer {
    public static void main(String[] args) throws Exception {
        Server server = ServerBuilder.forPort(9090)
                                     // => Listen on port 9090
            .addService(new ProductServiceImpl())
                                     // => Register service implementation
            .build()
            .start();                // => Start server

        System.out.println("gRPC server started on port 9090");
        server.awaitTermination();   // => Block until shutdown
    }
}
```

**Output**:

```bash
# Client call (Java gRPC client):
# ManagedChannel channel = ManagedChannelBuilder.forAddress("localhost", 9090)
#     .usePlaintext()
#     .build();
# ProductServiceBlockingStub stub = ProductServiceGrpc.newBlockingStub(channel);
#
# GetProductRequest request = GetProductRequest.newBuilder().setId("1").build();
# Product product = stub.getProduct(request);
# Output: Product{id=1, name=Laptop, price=999.99, category=ELECTRONICS}

# Server streaming example:
# ListProductsRequest request = ListProductsRequest.newBuilder()
#     .setCategory("ELECTRONICS")
#     .setLimit(10)
#     .build();
# Iterator<Product> products = stub.listProducts(request);
# while (products.hasNext()) {
#     Product p = products.next();
#     System.out.println(p.getName());
# }
# Output:
# Laptop
```

**Key Takeaway**: gRPC uses Protocol Buffers for binary serialization (smaller and faster than JSON). Service contracts defined in .proto files. Code generation creates type-safe Java classes. Server streaming allows sending multiple responses for single request. Use for microservice communication.

**Why It Matters**: gRPC is 7x faster than REST/JSON (binary vs text). Strongly-typed contracts prevent version mismatches. Streaming enables efficient large dataset transfer. HTTP/2 multiplexing allows concurrent requests on single connection. Multi-language support (Java, Go, Python, C++) for polyglot microservices. Downsides: less human-readable, requires code generation, poor browser support. Ideal for internal microservice communication where performance matters.

### Example 108: Content Negotiation (JSON vs XML)

**Context**: Content negotiation allows single endpoint to return multiple formats (JSON, XML, CSV) based on Accept header. Spring Boot supports automatic serialization to different media types.

```java
import org.springframework.web.bind.annotation.*;
import org.springframework.http.*;
import com.fasterxml.jackson.dataformat.xml.annotation.*;
import java.util.*;

@RestController
@RequestMapping("/api/v1/products")
public class ContentNegotiationController {

    @GetMapping(produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
                                     // => Supports both JSON and XML
                                     // => Determined by Accept header
    public ResponseEntity<List<ProductDTO>> listProducts() {
        List<ProductDTO> products = List.of(
            new ProductDTO("1", "Laptop", 999.99),
            new ProductDTO("2", "Mouse", 29.99)
        );
        return ResponseEntity.ok(products);
                                     // => Spring auto-serializes based on Accept header
                                     // => Accept: application/json -> JSON
                                     // => Accept: application/xml -> XML
    }

    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_JSON_VALUE)
                                     // => Only JSON supported for this endpoint
    public ResponseEntity<ProductDTO> getProductJson(@PathVariable String id) {
        ProductDTO product = new ProductDTO("1", "Laptop", 999.99);
        return ResponseEntity.ok(product);
                                     // => Returns JSON only
                                     // => 406 Not Acceptable if client requests XML
    }

    @GetMapping(value = "/{id}", produces = MediaType.APPLICATION_XML_VALUE)
                                     // => Only XML supported for this endpoint
    public ResponseEntity<ProductDTO> getProductXml(@PathVariable String id) {
        ProductDTO product = new ProductDTO("1", "Laptop", 999.99);
        return ResponseEntity.ok(product);
                                     // => Returns XML only
    }

    @GetMapping(value = "/export", produces = "text/csv")
                                     // => Custom media type: CSV
    public ResponseEntity<String> exportProductsCsv() {
        String csv = "id,name,price\n1,Laptop,999.99\n2,Mouse,29.99";
                                     // => Manual CSV generation
        return ResponseEntity.ok()
            .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=products.csv")
                                     // => Triggers download in browser
            .contentType(MediaType.parseMediaType("text/csv"))
            .body(csv);
    }
}

@JacksonXmlRootElement(localName = "product")
                                     // => XML root element name
record ProductDTO(
    @JacksonXmlProperty(localName = "id")
    String id,                       // => XML element: <id>1</id>
    @JacksonXmlProperty(localName = "name")
    String name,                     // => XML element: <name>Laptop</name>
    @JacksonXmlProperty(localName = "price")
    double price                     // => XML element: <price>999.99</price>
) {}
```

**Output**:

```http
GET /api/v1/products HTTP/1.1
Accept: application/json
Response: 200 OK
Content-Type: application/json
[
  {"id":"1","name":"Laptop","price":999.99},
  {"id":"2","name":"Mouse","price":29.99}
]

GET /api/v1/products HTTP/1.1
Accept: application/xml
Response: 200 OK
Content-Type: application/xml
<?xml version="1.0" encoding="UTF-8"?>
<List>
  <item>
    <product>
      <id>1</id>
      <name>Laptop</name>
      <price>999.99</price>
    </product>
  </item>
  <item>
    <product>
      <id>2</id>
      <name>Mouse</name>
      <price>29.99</price>
    </product>
  </item>
</List>

GET /api/v1/products/export HTTP/1.1
Accept: text/csv
Response: 200 OK
Content-Type: text/csv
Content-Disposition: attachment; filename=products.csv
id,name,price
1,Laptop,999.99
2,Mouse,29.99
```

**Key Takeaway**: Content negotiation allows single endpoint to return multiple formats. Set produces attribute with supported media types. Spring auto-serializes based on Accept header. Use @JacksonXmlRootElement/@JacksonXmlProperty for XML mapping. Custom media types (CSV, PDF) require manual serialization.

**Why It Matters**: Different clients need different formats. Web apps prefer JSON. Legacy systems need XML. Reports export as CSV/PDF. Content negotiation eliminates duplicate endpoints (/products.json, /products.xml). Single codebase serves all formats. HTTP standard: clients specify preference via Accept header. Spring Boot handles negotiation automatically (406 Not Acceptable if format unsupported).

### Example 109: HATEOAS (Hypermedia as the Engine of Application State)

**Context**: HATEOAS provides links in API responses for discoverability. Clients follow links instead of hardcoding URLs. Richardson Maturity Model Level 3 REST.

```java
import org.springframework.web.bind.annotation.*;
import org.springframework.http.*;
import org.springframework.hateoas.*;
import static org.springframework.hateoas.server.mvc.WebMvcLinkBuilder.*;
import java.util.*;

@RestController
@RequestMapping("/api/v1/orders")
public class OrderHATEOASController {

    @GetMapping("/{id}")
    public ResponseEntity<EntityModel<OrderDTO>> getOrder(@PathVariable String id) {
        OrderDTO order = new OrderDTO("ORD-123", "PENDING", 199.99);
                                     // => Order in PENDING state

        // Create HATEOAS entity model with links
        EntityModel<OrderDTO> resource = EntityModel.of(order);
                                     // => Wraps DTO with hypermedia links

        // Self link (link to this resource)
        resource.add(linkTo(methodOn(OrderHATEOASController.class).getOrder(id))
            .withSelfRel());         // => rel="self"
                                     // => Link to GET /api/v1/orders/ORD-123

        // Conditional links based on state
        if (order.status().equals("PENDING")) {
            // Allow payment for pending orders
            resource.add(linkTo(methodOn(OrderHATEOASController.class).payOrder(id))
                .withRel("pay"));    // => rel="pay"
                                     // => Link to POST /api/v1/orders/ORD-123/pay

            // Allow cancellation for pending orders
            resource.add(linkTo(methodOn(OrderHATEOASController.class).cancelOrder(id))
                .withRel("cancel")); // => rel="cancel"
                                     // => Link to POST /api/v1/orders/ORD-123/cancel
        }

        if (order.status().equals("PAID")) {
            // Allow shipping for paid orders
            resource.add(linkTo(methodOn(OrderHATEOASController.class).shipOrder(id))
                .withRel("ship"));   // => rel="ship"
                                     // => Link to POST /api/v1/orders/ORD-123/ship
        }

        // Collection link (back to all orders)
        resource.add(linkTo(methodOn(OrderHATEOASController.class).listOrders())
            .withRel("orders"));     // => rel="orders"
                                     // => Link to GET /api/v1/orders

        return ResponseEntity.ok(resource);
    }

    @GetMapping
    public ResponseEntity<CollectionModel<EntityModel<OrderDTO>>> listOrders() {
        List<OrderDTO> orders = List.of(
            new OrderDTO("ORD-123", "PENDING", 199.99),
            new OrderDTO("ORD-124", "PAID", 99.99)
        );

        // Wrap each order with self link
        List<EntityModel<OrderDTO>> orderResources = orders.stream()
            .map(order -> EntityModel.of(order,
                linkTo(methodOn(OrderHATEOASController.class).getOrder(order.id()))
                    .withSelfRel())) // => Each order has self link
            .toList();

        // Wrap collection with self link
        CollectionModel<EntityModel<OrderDTO>> collection =
            CollectionModel.of(orderResources,
                linkTo(methodOn(OrderHATEOASController.class).listOrders())
                    .withSelfRel()); // => Collection self link

        return ResponseEntity.ok(collection);
    }

    @PostMapping("/{id}/pay")
    public ResponseEntity<EntityModel<OrderDTO>> payOrder(@PathVariable String id) {
        OrderDTO order = new OrderDTO(id, "PAID", 199.99);
                                     // => Status changed to PAID
        EntityModel<OrderDTO> resource = EntityModel.of(order);
        resource.add(linkTo(methodOn(OrderHATEOASController.class).getOrder(id))
            .withSelfRel());
        resource.add(linkTo(methodOn(OrderHATEOASController.class).shipOrder(id))
            .withRel("ship"));       // => Next available action: ship
        return ResponseEntity.ok(resource);
    }

    @PostMapping("/{id}/cancel")
    public ResponseEntity<EntityModel<OrderDTO>> cancelOrder(@PathVariable String id) {
        OrderDTO order = new OrderDTO(id, "CANCELLED", 199.99);
        EntityModel<OrderDTO> resource = EntityModel.of(order);
        resource.add(linkTo(methodOn(OrderHATEOASController.class).getOrder(id))
            .withSelfRel());
        // No further actions for cancelled orders
        return ResponseEntity.ok(resource);
    }

    @PostMapping("/{id}/ship")
    public ResponseEntity<EntityModel<OrderDTO>> shipOrder(@PathVariable String id) {
        OrderDTO order = new OrderDTO(id, "SHIPPED", 199.99);
        EntityModel<OrderDTO> resource = EntityModel.of(order);
        resource.add(linkTo(methodOn(OrderHATEOASController.class).getOrder(id))
            .withSelfRel());
        // No further actions after shipping
        return ResponseEntity.ok(resource);
    }
}

record OrderDTO(String id, String status, double total) {}
```

**Output**:

```http
GET /api/v1/orders/ORD-123 HTTP/1.1
Response: 200 OK
{
  "id": "ORD-123",
  "status": "PENDING",
  "total": 199.99,
  "_links": {
    "self": {
      "href": "http://localhost:8080/api/v1/orders/ORD-123"
    },
    "pay": {
      "href": "http://localhost:8080/api/v1/orders/ORD-123/pay"
    },
    "cancel": {
      "href": "http://localhost:8080/api/v1/orders/ORD-123/cancel"
    },
    "orders": {
      "href": "http://localhost:8080/api/v1/orders"
    }
  }
}

POST /api/v1/orders/ORD-123/pay HTTP/1.1
Response: 200 OK
{
  "id": "ORD-123",
  "status": "PAID",
  "total": 199.99,
  "_links": {
    "self": {
      "href": "http://localhost:8080/api/v1/orders/ORD-123"
    },
    "ship": {
      "href": "http://localhost:8080/api/v1/orders/ORD-123/ship"
    }
  }
}
```

**Key Takeaway**: HATEOAS embeds hypermedia links in responses for discoverability. Clients follow links instead of constructing URLs. Conditional links based on resource state (e.g., PENDING orders have "pay" link). Use Spring HATEOAS EntityModel and CollectionModel. Richardson Maturity Model Level 3.

**Why It Matters**: Hardcoded client URLs break when server paths change. HATEOAS provides self-documenting APIs: responses show available actions. State-driven navigation: links change based on resource state (workflow enforcement). Reduces client-server coupling. Downside: increased payload size, complex client implementation. Best for public APIs with diverse clients. Internal microservices often skip HATEOAS for simplicity.

### Example 110: API Versioning Strategies

**Context**: APIs evolve over time. Breaking changes require versioning to avoid breaking existing clients. Common strategies: URI versioning, header versioning, content negotiation.

```java
import org.springframework.web.bind.annotation.*;
import org.springframework.http.*;
import java.util.*;

// Strategy 1: URI Versioning (most common, explicit)
@RestController
@RequestMapping("/api/v1/products")  // => Version in URI path
                                     // => Example: /api/v1/products
public class ProductV1Controller {
    @GetMapping("/{id}")
    public ResponseEntity<ProductV1DTO> getProduct(@PathVariable String id) {
        ProductV1DTO product = new ProductV1DTO("1", "Laptop", 999.99);
                                     // => V1 format: simple structure
        return ResponseEntity.ok(product);
    }
}

@RestController
@RequestMapping("/api/v2/products")  // => New version: v2
                                     // => Example: /api/v2/products
public class ProductV2Controller {
    @GetMapping("/{id}")
    public ResponseEntity<ProductV2DTO> getProduct(@PathVariable String id) {
        ProductV2DTO product = new ProductV2DTO(
            "1",
            "Laptop",
            999.99,
            "USD",                   // => V2 adds currency field
            List.of("ELECTRONICS")   // => V2 adds categories (breaking change)
        );
        return ResponseEntity.ok(product);
    }
}

// Strategy 2: Header Versioning (cleaner URIs)
@RestController
@RequestMapping("/api/products")     // => No version in URI
public class ProductHeaderVersionController {

    @GetMapping(value = "/{id}", headers = "API-Version=1")
                                     // => Version in custom header
                                     // => Request: GET /api/products/1
                                     //            API-Version: 1
    public ResponseEntity<ProductV1DTO> getProductV1(@PathVariable String id) {
        ProductV1DTO product = new ProductV1DTO("1", "Laptop", 999.99);
        return ResponseEntity.ok(product);
    }

    @GetMapping(value = "/{id}", headers = "API-Version=2")
                                     // => Same URI, different header value
                                     // => Request: GET /api/products/1
                                     //            API-Version: 2
    public ResponseEntity<ProductV2DTO> getProductV2(@PathVariable String id) {
        ProductV2DTO product = new ProductV2DTO(
            "1", "Laptop", 999.99, "USD", List.of("ELECTRONICS")
        );
        return ResponseEntity.ok(product);
    }
}

// Strategy 3: Accept Header Versioning (content negotiation)
@RestController
@RequestMapping("/api/products")
public class ProductAcceptVersionController {

    @GetMapping(value = "/{id}", produces = "application/vnd.company.product.v1+json")
                                     // => Custom media type with version
                                     // => Request: GET /api/products/1
                                     //            Accept: application/vnd.company.product.v1+json
    public ResponseEntity<ProductV1DTO> getProductV1(@PathVariable String id) {
        ProductV1DTO product = new ProductV1DTO("1", "Laptop", 999.99);
        return ResponseEntity.ok(product);
    }

    @GetMapping(value = "/{id}", produces = "application/vnd.company.product.v2+json")
                                     // => Different media type for v2
                                     // => Request: GET /api/products/1
                                     //            Accept: application/vnd.company.product.v2+json
    public ResponseEntity<ProductV2DTO> getProductV2(@PathVariable String id) {
        ProductV2DTO product = new ProductV2DTO(
            "1", "Laptop", 999.99, "USD", List.of("ELECTRONICS")
        );
        return ResponseEntity.ok(product);
    }
}

// DTOs
record ProductV1DTO(String id, String name, double price) {}
record ProductV2DTO(String id, String name, double price, String currency, List<String> categories) {}
```

**Output**:

```http
# Strategy 1: URI Versioning
GET /api/v1/products/1 HTTP/1.1
Response: {"id":"1","name":"Laptop","price":999.99}

GET /api/v2/products/1 HTTP/1.1
Response: {"id":"1","name":"Laptop","price":999.99,"currency":"USD","categories":["ELECTRONICS"]}

# Strategy 2: Header Versioning
GET /api/products/1 HTTP/1.1
API-Version: 1
Response: {"id":"1","name":"Laptop","price":999.99}

GET /api/products/1 HTTP/1.1
API-Version: 2
Response: {"id":"1","name":"Laptop","price":999.99,"currency":"USD","categories":["ELECTRONICS"]}

# Strategy 3: Accept Header Versioning
GET /api/products/1 HTTP/1.1
Accept: application/vnd.company.product.v1+json
Response: {"id":"1","name":"Laptop","price":999.99}

GET /api/products/1 HTTP/1.1
Accept: application/vnd.company.product.v2+json
Response: {"id":"1","name":"Laptop","price":999.99,"currency":"USD","categories":["ELECTRONICS"]}
```

**Key Takeaway**: Three versioning strategies: URI versioning (/v1/, /v2/), header versioning (API-Version header), accept header versioning (custom media types). URI versioning is most explicit and common. Header versioning keeps URIs clean but less discoverable. Choose based on client needs and infrastructure.

**Why It Matters**: Breaking changes (removing fields, changing types) break existing clients. Versioning allows gradual migration: v1 clients continue working while v2 clients use new features. URI versioning is easiest (visible in URLs, works with HTTP caches). Header versioning is RESTful but harder to test (need header tools). Deprecation policy: support old versions for 6-12 months. Industry standard: semantic versioning (v1, v2, not v1.1, v1.2 for REST).

## Security Best Practices

Security is fundamental in modern Java applications. This section covers OWASP Top 10 protections, cryptographic operations, secure authentication, and input validation patterns.

## Example 92: Input Validation with Allowlist Pattern

**Demonstrates**: Preventing injection attacks through strict input validation using allowlist approach.

**Security principle**: Allowlist approach (define what IS permitted) provides stronger protection than blocklist (define what ISN'T permitted).

```java
import java.util.regex.Pattern;                    // => Pattern matching for validation
import java.math.BigDecimal;                        // => Precise decimal arithmetic

public class SecureDonationValidator {              // => Input validator using allowlist
    // Allowlist: Only permit valid formats
    private static final Pattern VALID_AMOUNT =     // => Regex pattern for currency
        Pattern.compile("^\\d{1,10}(\\.\\d{1,2})?$");
                                                    // => Matches: 100, 100.00, 1234567890.99
                                                    // => Rejects: negative, letters, SQL injection

    private static final BigDecimal MAX_AMOUNT =    // => Maximum allowed donation
        new BigDecimal("999999999.99");             // => $999M limit (business rule)

    private static final BigDecimal MIN_AMOUNT =    // => Minimum allowed donation
        BigDecimal.ZERO;                            // => No negative amounts

    public record ValidationResult<T>(              // => Result container (success/failure)
        boolean valid,                              // => true if validation passed
        String error,                               // => Error message if failed
        T value                                     // => Parsed value if successful
    ) {
        public static <T> ValidationResult<T> success(T value) {
            return new ValidationResult<>(true, null, value);
                                                    // => Success case: valid=true, no error
        }

        public static <T> ValidationResult<T> error(String message) {
            return new ValidationResult<>(false, message, null);
                                                    // => Failure case: valid=false, error message
        }
    }

    public ValidationResult<BigDecimal> validate(String input) {
        // Step 1: Null/blank check
        if (input == null || input.isBlank()) {     // => Reject null or empty input
            return ValidationResult.error("Amount cannot be blank");
        }                                           // => Prevents NullPointerException

        // Step 2: Format validation (allowlist pattern)
        if (!VALID_AMOUNT.matcher(input).matches()) {
                                                    // => Check against allowlist regex
            return ValidationResult.error("Invalid amount format");
                                                    // => Rejects: "100'; DROP TABLE--" (SQL injection)
                                                    // => Rejects: "<script>alert(1)</script>" (XSS)
        }

        // Step 3: Safe parsing
        BigDecimal amount;                          // => Declare amount variable
        try {
            amount = new BigDecimal(input);         // => Parse to BigDecimal (throws on error)
        } catch (NumberFormatException e) {         // => Catch parsing errors
            return ValidationResult.error("Cannot parse amount");
                                                    // => Should never reach (regex already validated)
        }

        // Step 4: Range validation
        if (amount.compareTo(MIN_AMOUNT) < 0) {     // => Check minimum bound
            return ValidationResult.error("Amount cannot be negative");
        }                                           // => Business rule: no negative donations

        if (amount.compareTo(MAX_AMOUNT) > 0) {     // => Check maximum bound
            return ValidationResult.error("Amount exceeds maximum");
        }                                           // => Business rule: prevent overflow/abuse

        return ValidationResult.success(amount);    // => All validations passed
    }                                               // => Returns parsed BigDecimal
}

// Usage example
var validator = new SecureDonationValidator();      // => Create validator instance
var result = validator.validate("100.50");          // => Validate input "100.50"
                                                    // => result.valid() is true
                                                    // => result.value() is BigDecimal(100.50)

result = validator.validate("'; DROP TABLE users--");
                                                    // => SQL injection attempt
                                                    // => result.valid() is false
                                                    // => result.error() is "Invalid amount format"
```

**Output**:

```
Valid input "100.50": ValidationResult[valid=true, error=null, value=100.50]
SQL injection attempt: ValidationResult[valid=false, error=Invalid amount format, value=null]
```

**Key takeaway**: Always use allowlist validation (define valid formats) rather than blocklist (block known attacks). Allowlists protect against unknown attack vectors.

**Why it matters**: Input validation is the foundation of security. The OWASP Top 10 lists injection attacks as #3. Allowlist validation prevents SQL injection, XSS, command injection, and path traversal by rejecting ANY input that doesn't match expected patterns, including novel attack patterns not yet discovered.

## Example 93: SQL Injection Prevention with Prepared Statements

**Demonstrates**: Preventing SQL injection through parameterized queries instead of string concatenation.

**Attack vector**: String concatenation allows attackers to manipulate SQL query logic by injecting malicious SQL fragments.

**Vulnerable code (DO NOT USE)**:

```java
import java.sql.*;                                  // => JDBC API for database access

// WRONG: SQL Injection vulnerability
public class InsecureRepository {                   // => Vulnerable repository (example of BAD code)
    private final DataSource dataSource;            // => Database connection pool

    public List<String> findByDonor(String donorName) throws SQLException {
        // DANGEROUS: String concatenation
        String query = "SELECT * FROM donations WHERE donor_name = '"
                     + donorName + "'";             // => Concatenates user input directly
                                                    // => Attack: donorName = "' OR '1'='1"
                                                    // => Result: SELECT * FROM donations WHERE donor_name = '' OR '1'='1'
                                                    // => Impact: Returns ALL donations (bypasses access control)

        try (Connection conn = dataSource.getConnection();
                                                    // => Opens database connection
             Statement stmt = conn.createStatement();
                                                    // => Creates statement (no protection)
             ResultSet rs = stmt.executeQuery(query)) {
                                                    // => Executes MALICIOUS query
            // Process results...
            return new ArrayList<>();               // => Returns data (including unauthorized records)
        }
    }
}
```

**Secure implementation**:

```java
import java.sql.*;                                  // => JDBC API
import java.math.BigDecimal;                        // => For money amounts
import java.time.LocalDate;                         // => For date filtering

// CORRECT: SQL Injection protected
public class SecureRepository {                     // => Secure repository using prepared statements
    private final DataSource dataSource;            // => Database connection pool

    public List<String> findByDonor(String donorName) throws SQLException {
        // Safe: Parameterized query
        String query = "SELECT * FROM donations WHERE donor_name = ?";
                                                    // => ? is placeholder for parameter
                                                    // => SQL logic separated from data

        try (Connection conn = dataSource.getConnection();
                                                    // => Opens database connection
             PreparedStatement pStmt = conn.prepareStatement(query)) {
                                                    // => PreparedStatement (parameterized)

            pStmt.setString(1, donorName);          // => Sets parameter 1 to donorName
                                                    // => Driver escapes special characters
                                                    // => Attack string treated as LITERAL text
                                                    // => "' OR '1'='1" becomes literal string

            try (ResultSet rs = pStmt.executeQuery()) {
                                                    // => Executes SAFE query
                List<String> results = new ArrayList<>();
                                                    // => Collect results
                while (rs.next()) {                 // => Iterate result set
                    results.add(rs.getString("donor_name"));
                                                    // => Extract donor name
                }
                return results;                     // => Returns only matching donors
            }                                       // => Attack prevented: no unauthorized data
        }
    }

    public BigDecimal calculateTotalByPurpose(      // => Calculate total donations
            String purpose,                         // => Filter by purpose
            LocalDate startDate,                    // => Date range start
            LocalDate endDate) throws SQLException {// => Date range end

        String query = """
            SELECT SUM(amount) as total
            FROM donations
            WHERE purpose = ?
              AND donation_date BETWEEN ? AND ?
            """;                                    // => Three placeholders (?, ?, ?)
                                                    // => All parameters safely bound

        try (Connection conn = dataSource.getConnection();
             PreparedStatement pStmt = conn.prepareStatement(query)) {

            pStmt.setString(1, purpose);            // => Bind parameter 1 (purpose)
            pStmt.setDate(2, Date.valueOf(startDate));
                                                    // => Bind parameter 2 (start date)
            pStmt.setDate(3, Date.valueOf(endDate));
                                                    // => Bind parameter 3 (end date)
                                                    // => All inputs escaped automatically

            try (ResultSet rs = pStmt.executeQuery()) {
                return rs.next()                    // => Check if result exists
                    ? rs.getBigDecimal("total")     // => Return sum (or null)
                    : BigDecimal.ZERO;              // => Default to zero if no results
            }
        }
    }
}

// Usage example
var repo = new SecureRepository(dataSource);        // => Create secure repository
var donors = repo.findByDonor("' OR '1'='1");      // => SQL injection attempt
                                                    // => Returns empty list (no donor with that name)
                                                    // => Attack prevented!

var total = repo.calculateTotalByPurpose(           // => Calculate donations
    "Education",                                    // => Purpose filter
    LocalDate.of(2025, 1, 1),                       // => Start date
    LocalDate.of(2025, 12, 31)                      // => End date
);                                                  // => Returns total for Education in 2025
```

**Output**:

```
SQL injection attempt blocked: donors = []
Legitimate query: total = 15000.00
```

**Key takeaway**: ALWAYS use PreparedStatement with parameter binding. NEVER concatenate user input into SQL strings. Prepared statements separate SQL logic from data, making injection impossible.

**Why it matters**: SQL injection is OWASP #3 and enables attackers to read sensitive data, modify records, execute admin operations, or delete entire databases. PreparedStatement automatically escapes special characters, ensuring user input is treated as data, not SQL code.

## Example 94: Cross-Site Scripting (XSS) Prevention with Output Encoding

**Demonstrates**: Preventing XSS attacks by encoding output based on context (HTML, JavaScript, URL).

**Attack vector**: Attackers inject malicious JavaScript that executes in victim browsers, stealing cookies, session tokens, or performing actions as the victim.

```java
import org.owasp.encoder.Encode;                    // => OWASP Java Encoder library

public class SecureOutputRenderer {                 // => XSS-safe output rendering

    // WRONG: Direct output (vulnerable to XSS)
    public String renderDescriptionUnsafe(String description) {
        return "<div class='description'>" + description + "</div>";
                                                    // => If description = "<script>alert('XSS')</script>"
                                                    // => Browser executes the script! (XSS attack)
    }

    // CORRECT: HTML context encoding
    public String renderDescription(String description) {
        String encoded = Encode.forHtml(description);
                                                    // => Encodes for HTML context
                                                    // => < becomes &lt;
                                                    // => > becomes &gt;
                                                    // => & becomes &amp;
                                                    // => " becomes &quot;
                                                    // => ' becomes &#x27;

        return "<div class='description'>" + encoded + "</div>";
                                                    // => <script> becomes &lt;script&gt;
                                                    // => Displayed as text, NOT executed
    }

    // CORRECT: JavaScript context encoding
    public String renderInlineScript(String donorName) {
        String encoded = Encode.forJavaScript(donorName);
                                                    // => Encodes for JavaScript context
                                                    // => Escapes quotes, backslashes, etc.

        return """
            <script>
            var donorName = '%s';
            console.log('Donor: ' + donorName);
            </script>
            """.formatted(encoded);                 // => Safe insertion into JS string
                                                    // => Attack: donorName = "'; alert('XSS'); '"
                                                    // => Encoded: \'; alert(\'XSS\'); \'
                                                    // => No code execution (treated as string)
    }

    // CORRECT: URL context encoding
    public String renderDonationLink(String campaignId) {
        String encoded = Encode.forUriComponent(campaignId);
                                                    // => Encodes for URL parameter
                                                    // => Escapes &, =, /, etc.

        return "/donate?campaign=" + encoded;       // => Safe URL construction
                                                    // => Attack: campaignId = "123&admin=true"
                                                    // => Encoded: 123%26admin%3Dtrue
                                                    // => Treated as single parameter value
    }

    // CORRECT: HTML attribute context encoding
    public String renderInputField(String userName) {
        String encoded = Encode.forHtmlAttribute(userName);
                                                    // => Encodes for HTML attribute

        return "<input type='text' value='" + encoded + "'/>";
                                                    // => Safe attribute value
                                                    // => Attack: userName = "' onclick='alert(1)"
                                                    // => Encoded: &#x27; onclick=&#x27;alert(1)
                                                    // => No event handler injection
    }
}

// Usage examples
var renderer = new SecureOutputRenderer();          // => Create renderer instance

String xssAttempt = "<script>alert('XSS')</script>";
                                                    // => Malicious script injection

String safeHtml = renderer.renderDescription(xssAttempt);
                                                    // => Returns: <div class='description'>&lt;script&gt;alert('XSS')&lt;/script&gt;</div>
                                                    // => Displayed as text: <script>alert('XSS')</script>
                                                    // => NOT executed as code

String safeJs = renderer.renderInlineScript("Alice'; alert('XSS'); '");
                                                    // => Attack attempt in JavaScript context
                                                    // => Returns: var donorName = 'Alice\'; alert(\'XSS\'); \'';
                                                    // => Quotes escaped, no code execution

String safeUrl = renderer.renderDonationLink("123&admin=true");
                                                    // => Parameter injection attempt
                                                    // => Returns: /donate?campaign=123%26admin%3Dtrue
                                                    // => Treated as single parameter value
```

**Output**:

```
HTML context: <div class='description'>&lt;script&gt;alert('XSS')&lt;/script&gt;</div>
(Displays as text: <script>alert('XSS')</script>)

JS context: var donorName = 'Alice\'; alert(\'XSS\'); \'';
(No alert popup, treated as string)

URL context: /donate?campaign=123%26admin%3Dtrue
(Single parameter, not two parameters)
```

**Key takeaway**: Use context-specific encoding for ALL user-controlled output. HTML context requires HTML encoding, JavaScript context requires JavaScript escaping, URLs require URL encoding. One-size-fits-all encoding doesn't work.

**Why it matters**: XSS is OWASP #3 and allows attackers to execute arbitrary JavaScript in victim browsers. This enables session hijacking (stealing cookies), credential theft (fake login forms), malware distribution, and defacement. Context-aware output encoding ensures user data is treated as data, never as code.

## Example 95: Password Hashing with bcrypt

**Demonstrates**: Secure password storage using bcrypt with automatic salt generation and configurable cost factor.

**Security principle**: NEVER store plaintext passwords. Use cryptographically strong one-way hashing with salt.

```java
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
                                                    // => Spring Security's bcrypt implementation
import org.springframework.security.crypto.password.PasswordEncoder;
                                                    // => Generic password encoder interface

public class SecureAuthenticationService {          // => Password management service
    private final PasswordEncoder passwordEncoder =
        new BCryptPasswordEncoder(12);              // => Cost factor 12 = 2^12 iterations (4096)
                                                    // => Higher cost = slower hashing = more secure
                                                    // => Cost 12 takes ~300ms (acceptable for login)
                                                    // => Cost 10 = 100ms, Cost 15 = 2.5s

    // Registration: Hash password before storage
    public User registerUser(String username, String plainPassword) {
        // Step 1: Validate password strength
        if (!isStrongPassword(plainPassword)) {     // => Enforce password policy
            throw new IllegalArgumentException(
                "Password must be 12+ chars with mixed case, digits, symbols"
            );                                      // => Prevent weak passwords
        }

        // Step 2: Hash password with bcrypt
        String hashedPassword = passwordEncoder.encode(plainPassword);
                                                    // => Generates random salt
                                                    // => Hashes password + salt with bcrypt
                                                    // => Format: $2a$12$[22-char salt][31-char hash]
                                                    // => Example: $2a$12$N9qo8uLOickgx2ZMRZoMyeIjZAgcfl7p92ldGxad68LJZdL17lhWy
                                                    // => Salt embedded in output (no separate storage)

        return new User(username, hashedPassword);  // => Store HASHED password (never plaintext)
    }                                               // => Original password discarded

    // Login: Verify password
    public boolean authenticateUser(String username, String plainPassword) {
        User user = findUserByUsername(username);   // => Retrieve user from database
        if (user == null) {                         // => User not found
            // Timing attack mitigation: Hash anyway
            passwordEncoder.encode(plainPassword);  // => Same duration as successful login
            return false;                           // => Prevents username enumeration
        }                                           // => Attacker can't tell user doesn't exist

        boolean matches = passwordEncoder.matches(  // => Compare plaintext with stored hash
            plainPassword,                          // => Password entered at login
            user.hashedPassword()                   // => Stored hash from database
        );                                          // => bcrypt extracts salt from hash
                                                    // => Hashes plaintext with same salt
                                                    // => Compares resulting hash (constant-time)

        return matches;                             // => true if password correct
    }

    // Password strength validation
    private boolean isStrongPassword(String password) {
        if (password == null || password.length() < 12) {
                                                    // => Minimum 12 characters (NIST guideline)
            return false;
        }

        boolean hasLower = password.matches(".*[a-z].*");
                                                    // => At least one lowercase letter
        boolean hasUpper = password.matches(".*[A-Z].*");
                                                    // => At least one uppercase letter
        boolean hasDigit = password.matches(".*\\d.*");
                                                    // => At least one digit
        boolean hasSymbol = password.matches(".*[!@#$%^&*()].*");
                                                    // => At least one special character

        return hasLower && hasUpper && hasDigit && hasSymbol;
                                                    // => All four categories required
    }

    private User findUserByUsername(String username) {
        // Database lookup (simplified)
        return null;                                // => Replace with actual DB query
    }

    record User(String username, String hashedPassword) {}
                                                    // => User record with hashed password
}

// Usage examples
var authService = new SecureAuthenticationService();
                                                    // => Create authentication service

// Registration
User alice = authService.registerUser("alice", "SecureP@ssw0rd123");
                                                    // => Registers user with strong password
                                                    // => alice.hashedPassword() = "$2a$12$..." (60 chars)
                                                    // => Salt + hash stored together

// Same password, different hash (random salt)
User bob = authService.registerUser("bob", "SecureP@ssw0rd123");
                                                    // => Same password as Alice
                                                    // => bob.hashedPassword() = "$2a$12$..." (different hash!)
                                                    // => Unique salt prevents rainbow table attacks

// Login attempt
boolean valid = authService.authenticateUser("alice", "SecureP@ssw0rd123");
                                                    // => valid = true (correct password)

valid = authService.authenticateUser("alice", "WrongPassword");
                                                    // => valid = false (incorrect password)

// Weak password rejected
try {
    authService.registerUser("charlie", "password");
                                                    // => Throws IllegalArgumentException
} catch (IllegalArgumentException e) {
    System.out.println(e.getMessage());             // => "Password must be 12+ chars..."
}
```

**Output**:

```
Alice registered: $2a$12$N9qo8uLOickgx2ZMRZoMyeIjZAgcfl7p92ldGxad68LJZdL17lhWy
Bob registered: $2a$12$5Zp6rBfLOtP9gZqJ7YM3.eMvYzPxR3cNd8FjH2WqTgL5VbXaUhKmS
(Same password, different hashes due to random salts)

Login with correct password: true
Login with wrong password: false
Weak password rejected: Password must be 12+ chars with mixed case, digits, symbols
```

**Key takeaway**: Use bcrypt (or Argon2) for password hashing. Cost factor 12 provides good security/performance balance. bcrypt automatically generates random salts, preventing rainbow table attacks. Never store plaintext passwords or use fast hashes (MD5, SHA-1).

**Why it matters**: Password database breaches expose millions of credentials. bcrypt's slow hashing (2^12 iterations) makes offline cracking attacks computationally expensive. Unique salts ensure identical passwords produce different hashes, preventing precomputed rainbow table attacks. Cost factor can be increased over time as hardware improves.

## Example 96: AES-256 Encryption for Data at Rest

**Demonstrates**: Encrypting sensitive data before database storage using AES-256-GCM with Google Tink library.

**Use case**: Protecting sensitive financial data (bank account numbers, credit cards, social security numbers) even if database is compromised.

```java
import com.google.crypto.tink.Aead;                 // => Authenticated Encryption with Associated Data
import com.google.crypto.tink.KeysetHandle;         // => Key management
import com.google.crypto.tink.aead.AeadConfig;      // => AEAD algorithm configuration
import com.google.crypto.tink.aead.AeadKeyTemplates; // => Predefined key templates
import java.nio.charset.StandardCharsets;           // => UTF-8 encoding
import java.security.GeneralSecurityException;      // => Crypto exceptions
import java.util.Arrays;                            // => Array utilities

public class DataEncryptionService {                // => Service for encrypting sensitive data
    private final Aead aead;                        // => AEAD primitive (encrypt/decrypt)

    public DataEncryptionService(KeysetHandle keysetHandle)
            throws GeneralSecurityException {
        AeadConfig.register();                      // => Register AEAD algorithms with Tink
        this.aead = keysetHandle.getPrimitive(Aead.class);
                                                    // => Get AEAD primitive from key
    }

    // Generate new encryption key
    public static DataEncryptionService create()
            throws GeneralSecurityException {
        KeysetHandle keysetHandle = KeysetHandle.generateNew(
            AeadKeyTemplates.AES256_GCM            // => AES-256 in GCM mode
        );                                          // => Authenticated encryption (integrity + confidentiality)
                                                    // => GCM mode: Galois/Counter Mode (parallelizable)
                                                    // => 256-bit key (32 bytes, 2^256 keyspace)

        return new DataEncryptionService(keysetHandle);
    }

    // Encrypt bank account number
    public byte[] encryptBankAccount(String accountNumber)
            throws GeneralSecurityException {
        byte[] plaintext = accountNumber.getBytes(StandardCharsets.UTF_8);
                                                    // => Convert string to bytes (UTF-8)

        // Associated data (authenticated but not encrypted)
        byte[] associatedData = "bank_account".getBytes(StandardCharsets.UTF_8);
                                                    // => Context identifier (prevents ciphertext reuse)
                                                    // => Authenticated with MAC (can't be tampered)
                                                    // => NOT encrypted (visible in ciphertext)

        byte[] ciphertext = aead.encrypt(plaintext, associatedData);
                                                    // => Encrypts plaintext with AES-256-GCM
                                                    // => Generates random nonce (12 bytes)
                                                    // => Computes authentication tag (16 bytes)
                                                    // => Returns: nonce + ciphertext + tag
                                                    // => Total size: 12 + plaintext.length + 16

        return ciphertext;                          // => Store ciphertext in database
    }

    // Decrypt bank account number
    public String decryptBankAccount(byte[] ciphertext)
            throws GeneralSecurityException {
        byte[] associatedData = "bank_account".getBytes(StandardCharsets.UTF_8);
                                                    // => MUST match encryption associatedData
                                                    // => Wrong value = decryption fails

        byte[] plaintext = aead.decrypt(ciphertext, associatedData);
                                                    // => Extracts nonce from ciphertext
                                                    // => Decrypts with AES-256-GCM
                                                    // => Verifies authentication tag (integrity)
                                                    // => Throws exception if tampered

        return new String(plaintext, StandardCharsets.UTF_8);
                                                    // => Convert bytes back to string
    }
}

// Usage examples
var encryptionService = DataEncryptionService.create();
                                                    // => Generate new encryption key

String accountNumber = "1234-5678-9012-3456";       // => Sensitive bank account number
System.out.println("Plaintext: " + accountNumber);  // => Original data

// Encrypt
byte[] encrypted = encryptionService.encryptBankAccount(accountNumber);
                                                    // => encrypted.length = 12 + 19 + 16 = 47 bytes
                                                    // => (nonce + ciphertext + tag)
System.out.println("Encrypted (" + encrypted.length + " bytes): "
    + Arrays.toString(encrypted));                  // => Binary data (not human-readable)

// Encrypt same data twice (different nonces = different ciphertexts)
byte[] encrypted2 = encryptionService.encryptBankAccount(accountNumber);
                                                    // => New random nonce generated
System.out.println("Same input, different ciphertext: "
    + !Arrays.equals(encrypted, encrypted2));       // => true (probabilistic encryption)

// Decrypt
String decrypted = encryptionService.decryptBankAccount(encrypted);
                                                    // => Decrypts to original plaintext
System.out.println("Decrypted: " + decrypted);      // => "1234-5678-9012-3456"
System.out.println("Match: " + accountNumber.equals(decrypted));
                                                    // => true (successful round-trip)

// Tampering detection
encrypted[0] ^= 1;                                  // => Flip one bit in ciphertext
try {
    encryptionService.decryptBankAccount(encrypted);
                                                    // => Decryption fails (authentication tag invalid)
} catch (GeneralSecurityException e) {
    System.out.println("Tampering detected: " + e.getMessage());
                                                    // => Authenticated encryption prevents tampering
}
```

**Output**:

```
Plaintext: 1234-5678-9012-3456
Encrypted (47 bytes): [123, -54, 78, 90, ..., -12, 45]
Same input, different ciphertext: true
Decrypted: 1234-5678-9012-3456
Match: true
Tampering detected: decryption failed
```

**Key takeaway**: Use AES-256-GCM (authenticated encryption) for sensitive data at rest. Google Tink provides safe defaults and prevents common crypto mistakes. Authenticated encryption ensures both confidentiality (encryption) and integrity (tampering detection).

**Why it matters**: Data breaches expose sensitive information even with access controls. Encryption at rest ensures stolen database dumps are useless without encryption keys. AES-256-GCM provides both confidentiality (attacker can't read data) and integrity (attacker can't modify data without detection). Random nonces prevent pattern analysis.

## Example 97: Secure Random Number Generation

**Demonstrates**: Generating cryptographically strong random numbers for tokens, keys, and nonces using SecureRandom.

**Security principle**: NEVER use Math.random() or java.util.Random for security purposes. They are predictable and unsuitable for cryptographic use.

```java
import java.security.SecureRandom;                  // => Cryptographically strong PRNG
import java.util.HexFormat;                         // => Hex encoding (Java 17+)
import java.util.Base64;                            // => Base64 encoding

public class SecureRandomService {                  // => Secure token generation service
    private final SecureRandom secureRandom;        // => CSPRNG instance

    public SecureRandomService() {
        this.secureRandom = new SecureRandom();     // => Uses OS entropy source
                                                    // => Linux: /dev/urandom
                                                    // => Windows: CryptGenRandom
                                                    // => Seed from hardware RNG if available
    }

    // Generate session token (32 bytes = 256 bits)
    public String generateSessionToken() {
        byte[] tokenBytes = new byte[32];           // => 32 bytes = 256-bit token
        secureRandom.nextBytes(tokenBytes);         // => Fill with cryptographically random bytes
                                                    // => Unpredictable (not seeded with time)
                                                    // => 2^256 possible values

        return Base64.getUrlEncoder()               // => URL-safe Base64 encoding
            .withoutPadding()                       // => Remove trailing = padding
            .encodeToString(tokenBytes);            // => 43 character token
                                                    // => Example: "8f3k9Js7Wp2Lm4Nq6Rs1Tv5Yx8Az3Bw7"
    }

    // Generate API key (16 bytes = 128 bits)
    public String generateApiKey() {
        byte[] keyBytes = new byte[16];             // => 16 bytes = 128-bit key
        secureRandom.nextBytes(keyBytes);           // => Random bytes

        return HexFormat.of().formatHex(keyBytes);  // => Hex encoding (32 characters)
                                                    // => Example: "a3f5c8e1b9d4f2a7c6e9b1d8f3a5c2e7"
    }

    // Generate cryptographic nonce (12 bytes for AES-GCM)
    public byte[] generateNonce() {
        byte[] nonce = new byte[12];                // => 12 bytes = 96-bit nonce
                                                    // => Standard size for AES-GCM
        secureRandom.nextBytes(nonce);              // => Random nonce
                                                    // => MUST be unique per encryption
        return nonce;                               // => Used once (number used once)
    }

    // Generate secure random integer in range [0, bound)
    public int generateRandomInt(int bound) {
        return secureRandom.nextInt(bound);         // => Unbiased random int
                                                    // => Range: 0 (inclusive) to bound (exclusive)
                                                    // => Example: bound=6 simulates dice roll
    }

    // Generate password reset token (URL-safe, 256 bits)
    public String generatePasswordResetToken() {
        byte[] tokenBytes = new byte[32];           // => 32 bytes for high security
        secureRandom.nextBytes(tokenBytes);         // => Random token

        return Base64.getUrlEncoder()               // => URL-safe (no +, /, =)
            .withoutPadding()
            .encodeToString(tokenBytes);            // => Can be used in URLs
                                                    // => Example: /reset?token=8f3k9Js7Wp...
    }

    // WRONG: Predictable random (DO NOT USE for security)
    public String generateInsecureToken() {
        // VULNERABLE: java.util.Random is predictable
        java.util.Random random = new java.util.Random();
                                                    // => Seeded with current time
                                                    // => Output can be predicted
                                                    // => NOT suitable for security

        byte[] bytes = new byte[32];
        random.nextBytes(bytes);                    // => Predictable bytes
        return Base64.getEncoder().encodeToString(bytes);
                                                    // => Attacker can predict future tokens
    }
}

// Usage examples
var randomService = new SecureRandomService();      // => Create secure random service

// Generate session token
String sessionToken = randomService.generateSessionToken();
                                                    // => 43-character URL-safe token
System.out.println("Session token: " + sessionToken);
                                                    // => Example: "8f3k9Js7Wp2Lm4Nq6Rs1Tv5Yx8Az3Bw7Cv2"

// Generate API key
String apiKey = randomService.generateApiKey();     // => 32-character hex key
System.out.println("API key: " + apiKey);           // => Example: "a3f5c8e1b9d4f2a7c6e9b1d8f3a5c2e7"

// Generate multiple tokens (all unique)
System.out.println("Token 1: " + randomService.generateSessionToken());
System.out.println("Token 2: " + randomService.generateSessionToken());
System.out.println("Token 3: " + randomService.generateSessionToken());
                                                    // => All different (extremely low collision probability)
                                                    // => Collision probability: 1 / 2^256

// Generate nonce for encryption
byte[] nonce = randomService.generateNonce();       // => 12-byte nonce for AES-GCM
System.out.println("Nonce (" + nonce.length + " bytes): "
    + HexFormat.of().formatHex(nonce));             // => Example: "a3f5c8e1b9d4f2a7c6e9b1d8"

// Generate random dice roll
int diceRoll = randomService.generateRandomInt(6) + 1;
                                                    // => Range: 1-6 (inclusive)
System.out.println("Dice roll: " + diceRoll);       // => Fair, unpredictable
```

**Output**:

```
Session token: 8f3k9Js7Wp2Lm4Nq6Rs1Tv5Yx8Az3Bw7Cv2Dm5
API key: a3f5c8e1b9d4f2a7c6e9b1d8f3a5c2e7
Token 1: 1Bx4Dz7Fy9Hw2Jv5Kx8Mz1Ny4Qw7Rx9Tz2Vx5
Token 2: 3Cy6Ez9Gx2Hv5Jy8Lz1Mx4Nw7Py9Rx2Tx5Vz8
Token 3: 5Dy8Fz1Gy4Hx7Jz9Lx2Mv5Nz8Py1Rz4Tx7Vx9
Nonce (12 bytes): a3f5c8e1b9d4f2a7c6e9b1d8
Dice roll: 4
```

**Key takeaway**: Always use SecureRandom for security-sensitive operations (tokens, keys, nonces, salts). Never use Math.random() or java.util.Random—they are predictable and unsuitable for cryptographic purposes.

**Why it matters**: Predictable random numbers enable session hijacking, token forgery, and key recovery attacks. SecureRandom uses OS entropy sources (hardware RNG, environmental noise) to generate unpredictable values. A 256-bit token has 2^256 possible values (more than atoms in the universe), making brute-force attacks infeasible.

## Example 98: JWT Token Validation

**Demonstrates**: Validating JSON Web Tokens (JWT) for authentication, checking signature, expiration, and claims.

**Security principle**: Validate ALL JWT components—signature (authenticity), expiration (freshness), and claims (authorization).

```java
import io.jsonwebtoken.*;                           // => JJWT library for JWT handling
import io.jsonwebtoken.security.Keys;               // => Key generation utilities
import javax.crypto.SecretKey;                      // => Symmetric key interface
import java.time.Instant;                           // => Time utilities
import java.util.Date;                              // => Legacy Date (required by JJWT)
import java.nio.charset.StandardCharsets;           // => UTF-8 encoding

public class JwtAuthenticationService {             // => JWT creation and validation
    private final SecretKey secretKey;              // => HMAC signing key (256-bit)
    private final int expirationSeconds = 3600;     // => Token lifetime: 1 hour

    public JwtAuthenticationService(String secret) {
        // Generate key from secret string
        this.secretKey = Keys.hmacShaKeyFor(        // => Derive HMAC-SHA256 key
            secret.getBytes(StandardCharsets.UTF_8)
        );                                          // => Key must be >= 256 bits (32 bytes)
                                                    // => Shorter keys throw WeakKeyException
    }

    // Create JWT token
    public String createToken(String userId, String role) {
        Instant now = Instant.now();                // => Current timestamp
        Instant expiration = now.plusSeconds(expirationSeconds);
                                                    // => Token expires in 1 hour

        return Jwts.builder()                       // => JWT builder
            .setSubject(userId)                     // => Subject claim: user identifier
                                                    // => Standard claim (registered)
            .claim("role", role)                    // => Custom claim: user role
                                                    // => Used for authorization
            .setIssuedAt(Date.from(now))            // => Issued-at claim: creation time
                                                    // => Standard claim
            .setExpiration(Date.from(expiration))   // => Expiration claim: validity period
                                                    // => Standard claim
            .signWith(secretKey, SignatureAlgorithm.HS256)
                                                    // => Sign with HMAC-SHA256
                                                    // => Signature prevents tampering
            .compact();                             // => Serialize to compact JWT string
                                                    // => Format: header.payload.signature
                                                    // => Example: eyJhbGc...eyJzdWI...SflKxw
    }

    // Validate and parse JWT token
    public record ValidationResult(                 // => Validation result container
        boolean valid,                              // => true if token valid
        String userId,                              // => Extracted user ID
        String role,                                // => Extracted role
        String error                                // => Error message if invalid
    ) {}

    public ValidationResult validateToken(String token) {
        try {
            // Parse and validate token
            Jws<Claims> jws = Jwts.parserBuilder()  // => JWT parser builder
                .setSigningKey(secretKey)           // => Provide signing key for verification
                                                    // => Verifies HMAC-SHA256 signature
                .build()                            // => Build parser
                .parseClaimsJws(token);             // => Parse token
                                                    // => Throws if signature invalid
                                                    // => Throws if expired
                                                    // => Throws if malformed

            Claims claims = jws.getBody();          // => Extract claims (payload)
            String userId = claims.getSubject();    // => Get subject claim
            String role = claims.get("role", String.class);
                                                    // => Get custom role claim

            return new ValidationResult(true, userId, role, null);
                                                    // => Token valid, return claims

        } catch (ExpiredJwtException e) {           // => Token expired
            return new ValidationResult(false, null, null,
                "Token expired: " + e.getMessage());
                                                    // => Reject expired tokens

        } catch (SignatureException e) {            // => Invalid signature
            return new ValidationResult(false, null, null,
                "Invalid signature: " + e.getMessage());
                                                    // => Token tampered or wrong key

        } catch (MalformedJwtException e) {         // => Malformed token
            return new ValidationResult(false, null, null,
                "Malformed token: " + e.getMessage());
                                                    // => Invalid JWT format

        } catch (JwtException e) {                  // => Other JWT errors
            return new ValidationResult(false, null, null,
                "Token validation failed: " + e.getMessage());
                                                    // => Catch-all for JWT errors
        }
    }

    // Check if user has required role
    public boolean hasRole(String token, String requiredRole) {
        ValidationResult result = validateToken(token);
                                                    // => Validate token first

        if (!result.valid()) {                      // => Token invalid
            return false;
        }

        return requiredRole.equals(result.role());  // => Check role match
    }
}

// Usage examples
String secret = "my-super-secret-key-at-least-32-bytes-long-for-hmac-sha256";
                                                    // => 256-bit secret (32+ bytes)
var jwtService = new JwtAuthenticationService(secret);
                                                    // => Create JWT service

// Create token for user
String token = jwtService.createToken("user123", "ADMIN");
                                                    // => Generate JWT for user123 with ADMIN role
System.out.println("Token: " + token);              // => eyJhbGc...eyJzdWI...SflKxw
                                                    // => Three Base64 parts separated by dots

// Validate token
var result = jwtService.validateToken(token);       // => Parse and verify token
System.out.println("Valid: " + result.valid());     // => true
System.out.println("User ID: " + result.userId());  // => user123
System.out.println("Role: " + result.role());       // => ADMIN

// Tamper with token (flip one character)
String tamperedToken = token.substring(0, token.length() - 5) + "XXXXX";
                                                    // => Modify signature
result = jwtService.validateToken(tamperedToken);   // => Validation fails
System.out.println("Tampered token valid: " + result.valid());
                                                    // => false
System.out.println("Error: " + result.error());     // => "Invalid signature: ..."

// Check role-based authorization
boolean isAdmin = jwtService.hasRole(token, "ADMIN");
                                                    // => true (user123 has ADMIN role)
boolean isDonor = jwtService.hasRole(token, "DONOR");
                                                    // => false (user123 does not have DONOR role)

System.out.println("Is admin: " + isAdmin);         // => true
System.out.println("Is donor: " + isDonor);         // => false
```

**Output**:

```
Token: eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyMTIzIiwicm9sZSI6IkFETUlOIiwiaWF0IjoxNzM3NTc2MDAwLCJleHAiOjE3Mzc1Nzk2MDB9.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c
Valid: true
User ID: user123
Role: ADMIN
Tampered token valid: false
Error: Invalid signature: JWT signature does not match locally computed signature
Is admin: true
Is donor: false
```

**Key takeaway**: Always validate JWT signature, expiration, and claims. Use strong HMAC keys (256+ bits) or RSA public key validation. JWT signature ensures token authenticity (not forged), expiration ensures freshness (not reused indefinitely).

**Why it matters**: JWTs enable stateless authentication (no server-side session storage). Signature validation prevents token forgery—attacker can't create valid tokens without the secret key. Expiration limits damage from stolen tokens. Proper JWT validation is critical for API security and prevents unauthorized access.

## Example 99: CSRF Protection with Synchronizer Token Pattern

**Demonstrates**: Preventing Cross-Site Request Forgery (CSRF) attacks using synchronizer tokens.

**Attack vector**: Attacker tricks victim into submitting authenticated requests to vulnerable site (e.g., money transfer, password change).

```java
import java.security.SecureRandom;                  // => Secure random generator
import java.util.Base64;                            // => Base64 encoding
import java.util.Map;                               // => Token storage
import java.util.concurrent.ConcurrentHashMap;      // => Thread-safe map

public class CsrfProtectionService {                // => CSRF token generation and validation
    private final SecureRandom secureRandom = new SecureRandom();
                                                    // => Crypto-strength random

    // Store session_id -> csrf_token mapping
    private final Map<String, String> csrfTokens = new ConcurrentHashMap<>();
                                                    // => Thread-safe token storage
                                                    // => Maps session ID to CSRF token

    // Generate CSRF token for session
    public String generateToken(String sessionId) {
        byte[] tokenBytes = new byte[32];           // => 32 bytes = 256-bit token
        secureRandom.nextBytes(tokenBytes);         // => Random token

        String csrfToken = Base64.getUrlEncoder()   // => URL-safe encoding
            .withoutPadding()
            .encodeToString(tokenBytes);            // => 43-character token

        csrfTokens.put(sessionId, csrfToken);       // => Store token for session
                                                    // => New token per session

        return csrfToken;                           // => Return token for form embedding
    }

    // Validate CSRF token from request
    public boolean validateToken(String sessionId, String submittedToken) {
        if (sessionId == null || submittedToken == null) {
                                                    // => Missing required parameters
            return false;
        }

        String expectedToken = csrfTokens.get(sessionId);
                                                    // => Lookup token for session

        if (expectedToken == null) {                // => No token for session
            return false;                           // => Session invalid or expired
        }

        // Constant-time comparison (prevents timing attacks)
        return java.security.MessageDigest.isEqual(
            expectedToken.getBytes(),
            submittedToken.getBytes()
        );                                          // => Compare tokens
                                                    // => true if match (valid CSRF token)
    }

    // Invalidate token (after logout or session expiration)
    public void invalidateToken(String sessionId) {
        csrfTokens.remove(sessionId);               // => Remove token from storage
    }                                               // => Forces new token on next login

    // Example: Protect money transfer endpoint
    public record TransferRequest(                  // => Money transfer request
        String fromAccount,                         // => Source account
        String toAccount,                           // => Destination account
        double amount,                              // => Transfer amount
        String csrfToken                            // => CSRF token from form
    ) {}

    public boolean processTransfer(String sessionId, TransferRequest request) {
        // Step 1: Validate CSRF token
        if (!validateToken(sessionId, request.csrfToken())) {
                                                    // => CSRF token validation
            System.out.println("CSRF token validation failed");
            return false;                           // => Reject forged request
        }                                           // => Prevents CSRF attack

        // Step 2: Process transfer (if CSRF valid)
        System.out.println("Transfer approved: $" + request.amount()
            + " from " + request.fromAccount()
            + " to " + request.toAccount());

        // Execute transfer...
        return true;                                // => Transfer successful
    }
}

// Usage example: Protecting web application

var csrfService = new CsrfProtectionService();      // => Create CSRF service

// User logs in (generate session)
String sessionId = "session-abc123";                // => User session ID (from cookie)
String csrfToken = csrfService.generateToken(sessionId);
                                                    // => Generate CSRF token for session
System.out.println("Session: " + sessionId);
System.out.println("CSRF token: " + csrfToken);     // => Embed in HTML form

// HTML form (server-rendered)
String htmlForm = """
    <form method="POST" action="/transfer">
        <input type="hidden" name="csrfToken" value="%s" />
        <input name="fromAccount" />
        <input name="toAccount" />
        <input name="amount" />
        <button type="submit">Transfer</button>
    </form>
    """.formatted(csrfToken);                       // => CSRF token embedded in hidden field
                                                    // => Same-origin form submission includes token

System.out.println("Form HTML:\n" + htmlForm);

// Legitimate request (includes CSRF token)
var legitimateRequest = new CsrfProtectionService.TransferRequest(
    "acct-001", "acct-002", 1000.00, csrfToken
);                                                  // => Request from legitimate form

boolean success = csrfService.processTransfer(sessionId, legitimateRequest);
                                                    // => CSRF token valid
System.out.println("Legitimate request: " + success);
                                                    // => true (transfer approved)

// Forged request (attacker doesn't know CSRF token)
var forgedRequest = new CsrfProtectionService.TransferRequest(
    "acct-001", "attacker-acct", 5000.00, "fake-token"
);                                                  // => Attacker guesses token (wrong)

success = csrfService.processTransfer(sessionId, forgedRequest);
                                                    // => CSRF token validation fails
System.out.println("Forged request: " + success);   // => false (transfer rejected)

// Attack scenario explanation
System.out.println("\nAttack scenario:");
System.out.println("1. Victim logged into bank.com (has session cookie)");
System.out.println("2. Attacker sends email with link to evil.com");
System.out.println("3. evil.com has hidden form:");
System.out.println("   <form action='https://bank.com/transfer' method='POST'>");
System.out.println("   <input name='toAccount' value='attacker-acct' />");
System.out.println("   <input name='amount' value='5000' />");
System.out.println("   </form>");
System.out.println("   <script>document.forms[0].submit();</script>");
System.out.println("4. Form auto-submits to bank.com with victim's cookies");
System.out.println("5. Without CSRF protection: Transfer succeeds! (victim loses $5000)");
System.out.println("6. With CSRF protection: Transfer rejected (no valid CSRF token)");
```

**Output**:

```
Session: session-abc123
CSRF token: 8f3k9Js7Wp2Lm4Nq6Rs1Tv5Yx8Az3Bw7Cv2Dm5
Form HTML:
<form method="POST" action="/transfer">
    <input type="hidden" name="csrfToken" value="8f3k9Js7Wp2Lm4Nq6Rs1Tv5Yx8Az3Bw7Cv2Dm5" />
    <input name="fromAccount" />
    <input name="toAccount" />
    <input name="amount" />
    <button type="submit">Transfer</button>
</form>

Transfer approved: $1000.0 from acct-001 to acct-002
Legitimate request: true
CSRF token validation failed
Forged request: false

Attack scenario:
1. Victim logged into bank.com (has session cookie)
2. Attacker sends email with link to evil.com
3. evil.com has hidden form:
   <form action='https://bank.com/transfer' method='POST'>
   <input name='toAccount' value='attacker-acct' />
   <input name='amount' value='5000' />
   </form>
   <script>document.forms[0].submit();</script>
4. Form auto-submits to bank.com with victim's cookies
5. Without CSRF protection: Transfer succeeds! (victim loses $5000)
6. With CSRF protection: Transfer rejected (no valid CSRF token)
```

**Key takeaway**: Protect state-changing operations (POST, PUT, DELETE) with CSRF tokens. Generate unique token per session, embed in forms, validate on submission. Cookies alone don't prevent CSRF (attacker tricks browser into sending cookies).

**Why it matters**: CSRF is OWASP #8 (2021) and enables attackers to perform unauthorized actions as authenticated users (money transfers, password changes, account deletions). Session cookies are automatically sent by browsers, making CSRF possible. Synchronizer tokens ensure requests originate from legitimate forms, not attacker-controlled sites.

## Example 100: Rate Limiting for API Protection

**Demonstrates**: Implementing rate limiting to prevent brute-force attacks, API abuse, and denial-of-service.

**Use case**: Protecting login endpoints, API routes, and password reset functionality from automated attacks.

```java
import java.time.Instant;                           // => Time utilities
import java.time.Duration;                          // => Time duration
import java.util.Map;                               // => Token bucket storage
import java.util.concurrent.ConcurrentHashMap;      // => Thread-safe map

public class RateLimitService {                     // => Token bucket rate limiter

    // Token bucket for each client (IP address or user ID)
    private record TokenBucket(                     // => Token bucket state
        int tokens,                                 // => Available tokens
        Instant lastRefill                          // => Last refill timestamp
    ) {}

    private final Map<String, TokenBucket> buckets = new ConcurrentHashMap<>();
                                                    // => Client ID -> token bucket

    private final int maxTokens;                    // => Bucket capacity (max requests)
    private final Duration refillInterval;          // => Refill rate (time per token)

    public RateLimitService(int maxTokens, Duration refillInterval) {
        this.maxTokens = maxTokens;                 // => Example: 10 tokens
        this.refillInterval = refillInterval;       // => Example: 1 second per token
                                                    // => Result: 10 requests per 10 seconds
    }

    // Attempt to consume a token (returns true if allowed)
    public boolean allowRequest(String clientId) {
        TokenBucket bucket = buckets.compute(clientId, (key, existingBucket) -> {
                                                    // => Atomic update of bucket
            Instant now = Instant.now();            // => Current timestamp

            if (existingBucket == null) {           // => First request from client
                return new TokenBucket(maxTokens - 1, now);
                                                    // => Create bucket with max-1 tokens
                                                    // => Consume 1 token for this request
            }

            // Calculate tokens to refill
            long secondsSinceRefill = Duration.between(
                existingBucket.lastRefill(), now
            ).getSeconds();                         // => Seconds elapsed

            int tokensToAdd = (int) (secondsSinceRefill / refillInterval.getSeconds());
                                                    // => Tokens earned since last refill
                                                    // => Example: 5 seconds / 1 second = 5 tokens

            int currentTokens = Math.min(           // => Refill bucket (capped at max)
                existingBucket.tokens() + tokensToAdd,
                maxTokens
            );                                      // => Don't exceed bucket capacity

            if (currentTokens > 0) {                // => Tokens available
                return new TokenBucket(             // => Consume 1 token
                    currentTokens - 1,              // => Decrease token count
                    now                             // => Update refill timestamp
                );
            } else {                                // => No tokens available
                return existingBucket;              // => Keep existing bucket (rate limited)
            }
        });

        return bucket.tokens() >= 0;                // => true if request allowed
    }                                               // => false if rate limited

    // Check remaining tokens (without consuming)
    public int getRemainingTokens(String clientId) {
        TokenBucket bucket = buckets.get(clientId); // => Get bucket for client
        if (bucket == null) {                       // => No requests yet
            return maxTokens;                       // => Full bucket
        }

        Instant now = Instant.now();                // => Current time
        long secondsSinceRefill = Duration.between(
            bucket.lastRefill(), now
        ).getSeconds();

        int tokensToAdd = (int) (secondsSinceRefill / refillInterval.getSeconds());

        return Math.min(                            // => Current tokens + refilled
            bucket.tokens() + tokensToAdd,
            maxTokens
        );
    }
}

// Usage example: Protecting login endpoint

var rateLimiter = new RateLimitService(             // => Create rate limiter
    5,                                              // => 5 tokens (max 5 requests)
    Duration.ofSeconds(1)                           // => Refill 1 token per second
);                                                  // => Result: 5 requests per 5 seconds

String clientIp = "192.168.1.100";                  // => Client IP address
                                                    // => Could also use user ID

// Simulate rapid login attempts (brute-force attack)
System.out.println("Rapid login attempts:");
for (int i = 1; i <= 10; i++) {                     // => 10 login attempts
    boolean allowed = rateLimiter.allowRequest(clientIp);
                                                    // => Check rate limit

    int remaining = rateLimiter.getRemainingTokens(clientIp);
                                                    // => Check remaining tokens

    System.out.printf("Attempt %d: %s (remaining: %d)%n",
        i,
        allowed ? "ALLOWED" : "RATE LIMITED",
        remaining
    );

    if (!allowed) {
        System.out.println("  → HTTP 429 Too Many Requests");
                                                    // => Return error to client
    }
}

// Wait for refill
System.out.println("\nWaiting 3 seconds for token refill...");
try {
    Thread.sleep(3000);                             // => Wait 3 seconds
} catch (InterruptedException e) {
    Thread.currentThread().interrupt();
}

// Try again after refill
System.out.println("\nAfter refill:");
for (int i = 1; i <= 5; i++) {
    boolean allowed = rateLimiter.allowRequest(clientIp);
    int remaining = rateLimiter.getRemainingTokens(clientIp);

    System.out.printf("Attempt %d: %s (remaining: %d)%n",
        i,
        allowed ? "ALLOWED" : "RATE LIMITED",
        remaining
    );
}

// Different client (independent rate limit)
String client2 = "192.168.1.200";                   // => Different IP
boolean allowed = rateLimiter.allowRequest(client2);
                                                    // => Has full bucket (5 tokens)
System.out.println("\nDifferent client: " +
    (allowed ? "ALLOWED" : "RATE LIMITED"));        // => ALLOWED
```

**Output**:

```
Rapid login attempts:
Attempt 1: ALLOWED (remaining: 4)
Attempt 2: ALLOWED (remaining: 3)
Attempt 3: ALLOWED (remaining: 2)
Attempt 4: ALLOWED (remaining: 1)
Attempt 5: ALLOWED (remaining: 0)
Attempt 6: RATE LIMITED (remaining: 0)
  → HTTP 429 Too Many Requests
Attempt 7: RATE LIMITED (remaining: 0)
  → HTTP 429 Too Many Requests
Attempt 8: RATE LIMITED (remaining: 0)
  → HTTP 429 Too Many Requests
Attempt 9: RATE LIMITED (remaining: 0)
  → HTTP 429 Too Many Requests
Attempt 10: RATE LIMITED (remaining: 0)
  → HTTP 429 Too Many Requests

Waiting 3 seconds for token refill...

After refill:
Attempt 1: ALLOWED (remaining: 2)
Attempt 2: ALLOWED (remaining: 1)
Attempt 3: ALLOWED (remaining: 0)
Attempt 4: RATE LIMITED (remaining: 0)
Attempt 5: RATE LIMITED (remaining: 0)

Different client: ALLOWED
```

**Key takeaway**: Implement rate limiting on all authentication endpoints, API routes, and sensitive operations. Token bucket algorithm provides smooth rate limiting with burst tolerance. Return HTTP 429 (Too Many Requests) when limit exceeded.

**Why it matters**: Rate limiting prevents brute-force password attacks (try millions of passwords), credential stuffing (stolen password lists), API abuse (excessive requests), and denial-of-service attacks. Without rate limiting, attackers can automate attacks at machine speed. Rate limiting slows attackers while allowing legitimate users normal access.

## Performance Optimization

Performance optimization is critical for production applications. This section covers JVM tuning, garbage collection configuration, profiling tools, allocation optimization, and micro-benchmarking with JMH.

## Example 101: JVM Heap Sizing

Heap sizing controls memory available for object allocation. Setting `-Xms` (initial) equal to `-Xmx` (maximum) prevents dynamic resizing and improves performance predictability.

**Heap Memory Layout:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Heap["Heap Memory<br/>-Xms -Xmx"]
    Heap --> Young["Young Generation<br/>Eden + Survivor"]
    Heap --> Old["Old Generation<br/>Tenured Objects"]

    Young --> Eden["Eden Space<br/>new allocations"]
    Young --> S0["Survivor 0"]
    Young --> S1["Survivor 1"]

    style Heap fill:#0173B2,color:#fff
    style Young fill:#DE8F05,color:#fff
    style Old fill:#029E73,color:#fff
    style Eden fill:#CC78BC,color:#000
    style S0 fill:#CC78BC,color:#000
    style S1 fill:#CC78BC,color:#000
```

**Code**:

```java
// JVM heap configuration via command-line flags
// Run with: java -Xms4g -Xmx4g -XX:+PrintFlagsFinal MyApp
//
// -Xms4g => Initial heap size: 4GB
//           => Allocated at JVM startup (physical memory reserved)
//           => Prevents heap expansion during runtime
//           => Reduces GC overhead (no dynamic resizing)
//
// -Xmx4g => Maximum heap size: 4GB
//           => Upper limit for heap growth
//           => Same as -Xms for stable performance
//           => Heap won't grow beyond 4GB
//
// Why -Xms = -Xmx?
// => Eliminates heap resizing overhead
// => Predictable memory footprint
// => Faster startup (allocates all memory upfront)
// => Prevents GC thrashing from resizing

// Check current heap configuration at runtime
Runtime runtime = Runtime.getRuntime();
                                 // => Runtime singleton instance
                                 // => Provides JVM runtime information

long maxMemory = runtime.maxMemory();
                                 // => Maximum heap size (-Xmx value)
                                 // => Returns bytes (convert to GB)
long totalMemory = runtime.totalMemory();
                                 // => Current allocated heap size
                                 // => Starts at -Xms, grows toward -Xmx
long freeMemory = runtime.freeMemory();
                                 // => Unused memory in current heap
                                 // => Available for allocations before GC

System.out.println("Max Heap: " + (maxMemory / 1024 / 1024 / 1024) + " GB");
                                 // => Example output: "Max Heap: 4 GB"
                                 // => maxMemory / 1024³ converts bytes to GB
System.out.println("Total Heap: " + (totalMemory / 1024 / 1024 / 1024) + " GB");
                                 // => Example output: "Total Heap: 4 GB" (if -Xms=4g)
System.out.println("Free Heap: " + (freeMemory / 1024 / 1024) + " MB");
                                 // => Example output: "Free Heap: 3072 MB"
                                 // => freeMemory / 1024² converts bytes to MB

// Recommended heap sizing
// Rule: Heap = 50-75% of physical RAM
// => 8GB RAM: -Xms4g -Xmx4g
// => 16GB RAM: -Xms8g -Xmx8g
// => 32GB RAM: -Xms16g -Xmx16g
//
// Leave headroom for:
// => Native memory (Metaspace, thread stacks, direct buffers)
// => OS processes and caching
// => JVM overhead (code cache, JIT compiler)
```

**Key Takeaway**: Set `-Xms` equal to `-Xmx` for production workloads to eliminate heap resizing overhead and ensure predictable memory allocation. Size heap to 50-75% of physical RAM, leaving headroom for native memory and OS.

**Why It Matters**: Dynamic heap resizing triggers frequent GCs and unpredictable performance. Fixed heap size provides stable latency and throughput. Undersized heaps cause OutOfMemoryErrors. Oversized heaps increase GC pause times and waste resources.

## Example 102: G1 Garbage Collector Configuration

G1GC balances throughput and pause times for most workloads. It divides heap into regions and targets pause time goals.

**G1GC Region Structure:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Heap["Heap<br/>divided into regions"]
    Heap --> E1["Eden Region"]
    Heap --> E2["Eden Region"]
    Heap --> S1["Survivor Region"]
    Heap --> O1["Old Region"]
    Heap --> O2["Old Region"]
    Heap --> O3["Old Region"]

    style Heap fill:#0173B2,color:#fff
    style E1 fill:#CC78BC,color:#000
    style E2 fill:#CC78BC,color:#000
    style S1 fill:#DE8F05,color:#000
    style O1 fill:#029E73,color:#fff
    style O2 fill:#029E73,color:#fff
    style O3 fill:#029E73,color:#fff
```

**Code**:

```bash
# G1GC configuration for 8GB heap, target 100ms pause time
# Run with: java @g1gc.conf MyApp

# Enable G1GC (default in Java 9+)
-XX:+UseG1GC                     # => Activate G1 Garbage Collector
                                 # => Replaces default GC (if overridden)
                                 # => Region-based, concurrent marking

# Heap sizing
-Xms8g                           # => Initial heap: 8GB
-Xmx8g                           # => Maximum heap: 8GB (no dynamic growth)

# Pause time goal
-XX:MaxGCPauseMillis=100         # => Target max pause: 100ms
                                 # => G1 GOAL, not guarantee
                                 # => Balances throughput vs latency
                                 # => Lower values: more frequent short pauses
                                 # => Higher values: less frequent long pauses

# Region size
-XX:G1HeapRegionSize=16m         # => Each region: 16MB
                                 # => Heap divided into ~512 regions (8192MB / 16MB)
                                 # => Valid sizes: 1m, 2m, 4m, 8m, 16m, 32m
                                 # => Formula: RegionCount = HeapSize / RegionSize
                                 # => Larger regions: fewer, faster scanning
                                 # => Smaller regions: more granular collection

# Concurrent marking trigger
-XX:InitiatingHeapOccupancyPercent=45
                                 # => Start concurrent marking at 45% heap usage
                                 # => 8GB heap: triggers at ~3.6GB used
                                 # => Lower value: more proactive (less risk of Full GC)
                                 # => Higher value: defers overhead (more risk of Full GC)
                                 # => Default: 45 (good starting point)

# Reserve space for evacuations
-XX:G1ReservePercent=10          # => Reserve 10% heap for to-space
                                 # => 8GB heap: 800MB reserved
                                 # => Prevents evacuation failures
                                 # => Buffer for copying live objects during GC

# GC logging
-Xlog:gc*:file=gc.log:time,uptime,level,tags
                                 # => Log all GC events to gc.log
                                 # => time: timestamp per entry
                                 # => uptime: JVM uptime
                                 # => level: INFO, DEBUG, TRACE
                                 # => tags: gc, gc+heap, gc+age, etc.
```

**Verification**:

```java
// Monitor G1GC behavior at runtime
import java.lang.management.*;
import java.util.*;

List<GarbageCollectorMXBean> gcBeans =
    ManagementFactory.getGarbageCollectorMXBeans();
                                 // => Retrieves GC management beans
                                 // => One bean per GC (young, old)

for (GarbageCollectorMXBean gcBean : gcBeans) {
                                 // => Iterate over GC beans
    String name = gcBean.getName();
                                 // => GC name: "G1 Young Generation", "G1 Old Generation"
    long count = gcBean.getCollectionCount();
                                 // => Number of collections performed
                                 // => Increments with each GC cycle
    long time = gcBean.getCollectionTime();
                                 // => Total time spent in GC (milliseconds)
                                 // => Cumulative across all collections

    System.out.println("GC: " + name);
                                 // => Example: "GC: G1 Young Generation"
    System.out.println("  Collections: " + count);
                                 // => Example: "  Collections: 150"
    System.out.println("  Time: " + time + " ms");
                                 // => Example: "  Time: 2500 ms"
    System.out.println("  Avg Pause: " + (count > 0 ? time / count : 0) + " ms");
                                 // => Example: "  Avg Pause: 16 ms"
                                 // => Average pause time per collection
                                 // => Should be < MaxGCPauseMillis (100ms goal)
}

// Check if pause time goal is met
// Parse gc.log for: [GC pause (G1 Evacuation Pause) 78.2ms]
// Verify p99 pause time < 100ms
```

**Key Takeaway**: G1GC targets pause time goals (`-XX:MaxGCPauseMillis`) while balancing throughput. Configure region size based on heap size (~1-2048 regions). Set `InitiatingHeapOccupancyPercent` to trigger concurrent marking early (prevent Full GCs).

**Why It Matters**: G1GC is the default GC in modern Java (9+). Proper configuration prevents long pause times and Full GCs. Region-based collection enables incremental cleaning of old generation. Pause time goals make latency predictable.

## Example 103: ZGC Ultra-Low Latency Configuration

ZGC provides sub-millisecond pause times for heaps up to 16TB. Generational ZGC (Java 21+) improves throughput for high-allocation workloads.

**ZGC vs G1GC Comparison:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    G1["G1GC<br/>50-200ms pauses"]
    ZGC["ZGC<br/><1ms pauses"]
    GenZGC["Generational ZGC<br/><1ms + better throughput"]

    G1 --> GC1["Stop-The-World<br/>young collections"]
    G1 --> GC2["Concurrent marking<br/>old generation"]

    ZGC --> ZGC1["Concurrent everything<br/>marking + relocation"]
    ZGC --> ZGC2["Colored pointers<br/>no stop-the-world"]

    GenZGC --> GZGC1["Young gen: fast<br/>short-lived objects"]
    GenZGC --> GZGC2["Old gen: concurrent<br/>long-lived objects"]

    style G1 fill:#0173B2,color:#fff
    style ZGC fill:#DE8F05,color:#000
    style GenZGC fill:#029E73,color:#fff
```

**Code**:

```bash
# ZGC configuration for 16GB heap, <1ms pause time
# Run with: java @zgc.conf MyApp

# Enable ZGC (Java 15+, production-ready Java 17+)
-XX:+UseZGC                      # => Activate Z Garbage Collector
                                 # => Low-latency collector (<10ms pauses)
                                 # => Concurrent: most work parallel with app
                                 # => Scalable: handles 8MB to 16TB heaps

# Enable Generational ZGC (Java 21+)
-XX:+ZGenerational               # => Separate young and old generations
                                 # => Young gen collected more frequently
                                 # => Improves throughput for high-allocation apps
                                 # => Reduces memory pressure

# Heap sizing
-Xms16g                          # => Initial heap: 16GB
-Xmx16g                          # => Maximum heap: 16GB

# Concurrent GC threads
-XX:ConcGCThreads=4              # => Threads for concurrent GC work
                                 # => Formula: ConcGCThreads = CPUs / 4
                                 # => 16-core CPU: 4 threads
                                 # => More threads: faster GC, higher CPU usage

# Allocation spike tolerance
-XX:ZAllocationSpikeTolerance=5  # => Tolerate 5x allocation rate spikes
                                 # => Prevents GC thrashing during bursts
                                 # => Higher value: more headroom (more memory)
                                 # => Lower value: tighter memory usage

# GC logging
-Xlog:gc*:file=zgc.log:time,uptime,level,tags
                                 # => Log ZGC events to zgc.log
                                 # => Monitor pause times and cycles
```

**Generational ZGC Performance**:

```java
// Compare ZGC vs Generational ZGC allocation performance
import java.util.*;
import java.math.*;

// High-allocation workload (simulates request processing)
public class AllocationBenchmark {
    public static void main(String[] args) {
        long start = System.currentTimeMillis();
        List<String> data = new ArrayList<>();

        // Allocate 1 million short-lived objects
        for (int i = 0; i < 1_000_000; i++) {
                                 // => Loop creates temporary objects
                                 // => High young-gen allocation pressure
            String temp = "Transaction_" + i + "_" + System.nanoTime();
                                 // => temp is String (short-lived)
                                 // => Concatenation creates StringBuilder (temporary)
                                 // => Most objects die young

            data.add(temp);      // => Add to list (temp survives to old gen eventually)
                                 // => Young gen fills quickly

            if (i % 10_000 == 0) {
                                 // => Every 10,000 iterations
                data.clear();    // => Clear list (make objects unreachable)
                                 // => Triggers young gen collection
                                 // => ZGC collects concurrently (<1ms pause)
                                 // => Gen ZGC optimizes young-gen collection
            }
        }

        long elapsed = System.currentTimeMillis() - start;
        System.out.println("Time: " + elapsed + " ms");
                                 // => Non-generational ZGC: ~1200ms
                                 // => Generational ZGC: ~800ms (33% faster)
                                 // => Improvement from young-gen optimization

        // Check pause times in zgc.log:
        // ZGC: [GC(123) Garbage Collection 0.8ms]
        // Gen ZGC: [GC(123) Young Collection 0.4ms]
        //          [GC(130) Old Collection 0.7ms]
    }
}
```

**Key Takeaway**: ZGC provides sub-millisecond pause times for latency-critical applications. Generational ZGC (Java 21+) adds young-gen optimization for better throughput with high allocation rates. Configure `ConcGCThreads` based on CPU cores and `ZAllocationSpikeTolerance` for bursty workloads.

**Why It Matters**: Traditional GCs pause applications for 10-200ms. ZGC pauses are <1ms regardless of heap size (8MB to 16TB). Generational ZGC combines low latency with improved throughput. Ideal for real-time systems, trading platforms, and low-latency APIs.

## Example 104: GC Logging and Analysis

GC logs provide insights into pause times, collection frequency, and heap usage. Analyze logs to tune GC parameters and detect memory leaks.

**Code**:

```bash
# Enable comprehensive GC logging
# Run with: java @gc-logging.conf MyApp

# Unified logging (Java 9+)
-Xlog:gc*:file=gc.log:time,uptime,level,tags:filecount=5,filesize=100m
                                 # => gc*: all GC events (gc, gc+heap, gc+age, etc.)
                                 # => file=gc.log: output to gc.log
                                 # => time: timestamp per log entry
                                 # => uptime: JVM uptime in seconds
                                 # => level: INFO (default), DEBUG, TRACE
                                 # => tags: event categories
                                 # => filecount=5: rotate logs (5 files max)
                                 # => filesize=100m: rotate when file exceeds 100MB

# Example GC log output (G1GC)
# [2026-02-03T10:30:00.123+0000][0.456s] GC(10) Pause Young (Normal) 256M->64M(512M) 23.456ms
# Breakdown:
# => [timestamp] = 2026-02-03T10:30:00.123+0000
# => [uptime] = 0.456s (JVM uptime)
# => GC(10) = 10th GC event
# => Pause Young (Normal) = Young generation collection
# => 256M->64M = Heap before GC -> after GC
# => (512M) = Total heap size
# => 23.456ms = Pause time duration

# Analyze gc.log with GCViewer (GUI tool)
# Download: https://github.com/chewiebug/GCViewer
java -jar gcviewer.jar gc.log
                                 # => Opens GUI with visualizations
                                 # => Charts: heap usage, pause times, throughput
                                 # => Statistics: avg/max pause, GC overhead

# Analyze gc.log with GCeasy (online service)
# Upload gc.log to https://gceasy.io
# => Automatic analysis and recommendations
# => Detects memory leaks, pause time trends
# => Suggests GC tuning parameters
```

**Parsing GC Logs Programmatically**:

```java
import java.io.*;
import java.util.*;
import java.util.regex.*;

// Parse gc.log to extract pause times
public class GCLogParser {
    public static void main(String[] args) throws IOException {
        List<Double> pauseTimes = new ArrayList<>();
                                 // => Stores pause times in milliseconds

        // Pattern: GC(N) Pause ... XXms
        Pattern pattern = Pattern.compile("GC\\(\\d+\\).*?([\\d.]+)ms");
                                 // => Regex matches pause time at end of line
                                 // => ([\\d.]+) captures decimal number
                                 // => ms literal matches "ms" suffix

        try (BufferedReader reader = new BufferedReader(new FileReader("gc.log"))) {
                                 // => Opens gc.log for reading
                                 // => try-with-resources auto-closes
            String line;
            while ((line = reader.readLine()) != null) {
                                 // => Read file line by line
                                 // => null when EOF reached
                Matcher matcher = pattern.matcher(line);
                                 // => Apply regex to line
                if (matcher.find()) {
                                 // => If pause time found
                    double pauseMs = Double.parseDouble(matcher.group(1));
                                 // => Extract pause time (group 1)
                                 // => Convert string to double
                    pauseTimes.add(pauseMs);
                                 // => Add to list
                }
            }
        }

        // Calculate statistics
        Collections.sort(pauseTimes);
                                 // => Sort ascending for percentiles
        int count = pauseTimes.size();
                                 // => Total number of pauses
        double sum = pauseTimes.stream().mapToDouble(Double::doubleValue).sum();
                                 // => Sum of all pause times
        double avg = sum / count;
                                 // => Average pause time
        double p50 = pauseTimes.get(count / 2);
                                 // => Median (50th percentile)
        double p95 = pauseTimes.get((int) (count * 0.95));
                                 // => 95th percentile (95% pauses below this)
        double p99 = pauseTimes.get((int) (count * 0.99));
                                 // => 99th percentile (99% pauses below this)
        double max = pauseTimes.get(count - 1);
                                 // => Maximum pause time

        System.out.println("GC Pause Time Analysis:");
        System.out.printf("  Total pauses: %d%n", count);
                                 // => Example: "  Total pauses: 1500"
        System.out.printf("  Average: %.2f ms%n", avg);
                                 // => Example: "  Average: 15.32 ms"
        System.out.printf("  p50 (median): %.2f ms%n", p50);
                                 // => Example: "  p50 (median): 12.50 ms"
        System.out.printf("  p95: %.2f ms%n", p95);
                                 // => Example: "  p95: 45.00 ms"
        System.out.printf("  p99: %.2f ms%n", p99);
                                 // => Example: "  p99: 78.50 ms"
                                 // => p99 is critical SLA metric
        System.out.printf("  Max: %.2f ms%n", max);
                                 // => Example: "  Max: 125.00 ms"

        // Check SLA compliance
        double slaTarget = 100.0; // => 100ms pause time SLA
        if (p99 > slaTarget) {
            System.out.printf("WARNING: p99 (%.2f ms) exceeds SLA (%2f ms)%n",
                p99, slaTarget);
                                 // => Alert if 99th percentile violates SLA
                                 // => Triggers GC tuning investigation
        }
    }
}
```

**Key Takeaway**: GC logging is essential for production monitoring. Analyze pause times (p50, p95, p99) to verify SLA compliance. Use tools like GCViewer or GCeasy for automated analysis. Monitor trends to detect memory leaks and GC degradation.

**Why It Matters**: GC pauses directly impact application latency. Without logging, performance regressions go unnoticed. Percentile analysis reveals tail latency (p99) that averages hide. Early detection prevents production incidents.

## Example 105: Java Flight Recorder (JFR) Profiling

JFR provides production-grade profiling with <2% overhead. Records allocation hotspots, CPU samples, GC events, and I/O operations.

**JFR Event Categories:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    JFR["Java Flight Recorder"]
    JFR --> Alloc["Memory Events<br/>ObjectAllocationInNewTLAB"]
    JFR --> CPU["CPU Events<br/>ExecutionSample"]
    JFR --> GC["GC Events<br/>GarbageCollection"]
    JFR --> IO["I/O Events<br/>FileRead/FileWrite"]
    JFR --> Lock["Lock Events<br/>JavaMonitorWait"]

    style JFR fill:#0173B2,color:#fff
    style Alloc fill:#CC78BC,color:#000
    style CPU fill:#DE8F05,color:#000
    style GC fill:#029E73,color:#fff
    style IO fill:#CA9161,color:#000
    style Lock fill:#CC78BC,color:#000
```

**Code**:

```bash
# Start JFR recording from command line
# Run with: java -XX:StartFlightRecording=... MyApp

# Command-line recording (60 seconds)
java -XX:StartFlightRecording=filename=app.jfr,duration=60s,settings=profile \
     -jar myapp.jar
                                 # => filename=app.jfr: output file
                                 # => duration=60s: record for 60 seconds
                                 # => settings=profile: detailed profiling
                                 #    (alternative: settings=default for lower overhead)
                                 # => JFR starts automatically at JVM startup

# Trigger recording dynamically (running JVM)
jcmd <pid> JFR.start duration=60s filename=app.jfr settings=profile
                                 # => <pid>: Java process ID (from jps)
                                 # => Starts recording without JVM restart
                                 # => Useful for production troubleshooting

# Check recording status
jcmd <pid> JFR.check             # => Lists active recordings
                                 # => Shows duration, filename, settings

# Stop recording manually
jcmd <pid> JFR.stop name=1       # => name=1: recording ID from JFR.check
                                 # => Flushes data to file immediately

# Dump recording without stopping
jcmd <pid> JFR.dump filename=dump.jfr
                                 # => Captures current state
                                 # => Recording continues after dump
```

**Analyze Allocation Hotspots**:

```bash
# Print allocation events (top allocators)
jfr print --events jdk.ObjectAllocationInNewTLAB app.jfr | head -50
                                 # => Lists methods allocating objects
                                 # => ObjectAllocationInNewTLAB: new objects in young gen
                                 # => Sorted by allocation size (bytes)
                                 # => head -50: top 50 allocators

# Example output:
# jdk.ObjectAllocationInNewTLAB {
#   startTime = 10:30:00.123
#   objectClass = java.lang.String
#   allocationSize = 48 bytes
#   tlabSize = 2048 bytes
#   stackTrace = [
#     com.example.ReportGenerator.generateReport(String) line: 42
#     com.example.Controller.handle(Request) line: 78
#   ]
# }
# => String allocated in generateReport method
# => 48 bytes per String (header + fields)
# => Identify StringBuilder usage needed
```

**Analyze CPU Hotspots**:

```bash
# Print execution samples (CPU-intensive methods)
jfr print --events jdk.ExecutionSample app.jfr | head -50
                                 # => Lists methods consuming CPU time
                                 # => ExecutionSample: periodic stack sampling
                                 # => Frequency: 20 samples/second (configurable)
                                 # => Sorted by sample count (CPU time proxy)

# Example output:
# jdk.ExecutionSample {
#   startTime = 10:30:05.789
#   sampledThread = "worker-thread-1"
#   state = STATE_RUNNABLE
#   stackTrace = [
#     com.example.Algorithm.sortData(List) line: 156
#     com.example.Processor.process(Data) line: 89
#   ]
# }
# => sortData method consumes significant CPU
# => Investigate algorithm complexity (O(n²)?)
```

**GUI Analysis with JDK Mission Control**:

```bash
# Open JFR file in Mission Control GUI
jmc app.jfr                      # => Launches JDK Mission Control
                                 # => Visual analysis: charts, tables, flame graphs
                                 # => Tabs: Memory, Code, Threads, I/O, System
                                 # => Interactive drill-down into events
```

**Programmatic JFR Control**:

```java
import jdk.jfr.*;
import java.nio.file.*;

// Control JFR programmatically in application code
public class ProfilingExample {
    public static void main(String[] args) throws Exception {
        // Create and configure recording
        Configuration config = Configuration.getConfiguration("profile");
                                 // => Load "profile" configuration (detailed)
                                 // => Alternative: "default" (low overhead)

        Recording recording = new Recording(config);
                                 // => Create new JFR recording
                                 // => Not started yet

        recording.setMaxAge(Duration.ofMinutes(10));
                                 // => Keep max 10 minutes of data
                                 // => Older data discarded (circular buffer)
        recording.setMaxSize(100_000_000);
                                 // => Max file size: 100MB
                                 // => Prevents disk exhaustion

        recording.start();       // => Start recording
                                 // => Events captured from this point
        System.out.println("JFR recording started");

        // Run application workload
        performWork();           // => Execute code to profile
                                 // => JFR captures events during this time

        // Stop and save recording
        recording.stop();        // => Stop capturing events
        Path path = Paths.get("programmatic.jfr");
        recording.dump(path);    // => Write events to file
                                 // => File ready for analysis
        System.out.println("Recording saved to: " + path);

        recording.close();       // => Release resources
    }

    static void performWork() {
        // Simulate allocation-heavy workload
        for (int i = 0; i < 1_000_000; i++) {
                                 // => High-allocation loop
            String temp = "Data_" + i;
                                 // => String allocations tracked by JFR
                                 // => ObjectAllocationInNewTLAB events
        }
    }
}
```

**Key Takeaway**: JFR provides low-overhead profiling (<2% CPU impact) for production systems. Analyze allocation hotspots (`ObjectAllocationInNewTLAB`), CPU samples (`ExecutionSample`), and GC events (`GarbageCollection`). Use `jcmd` for dynamic recording and JMC for visual analysis.

**Why It Matters**: Traditional profilers impose 20-50% overhead (unsuitable for production). JFR's continuous recording enables always-on profiling. Captures real production workload behavior. Essential for diagnosing memory leaks, CPU hotspots, and GC issues.

## Example 106: Allocation Profiling with async-profiler

async-profiler provides flame graphs for allocation and CPU profiling with minimal overhead. Uses OS signals for sampling (more accurate than JFR for CPU).

**Flame Graph Structure:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Main["main<br/>100% samples"]
    Main --> A["processRequests<br/>80% samples"]
    Main --> B["logging<br/>20% samples"]

    A --> A1["parseData<br/>50%"]
    A --> A2["validateData<br/>30%"]

    A1 --> A1a["String allocation<br/>40%"]
    A1 --> A1b["parsing<br/>10%"]

    style Main fill:#0173B2,color:#fff
    style A fill:#DE8F05,color:#000
    style B fill:#029E73,color:#fff
    style A1 fill:#CC78BC,color:#000
    style A2 fill:#CC78BC,color:#000
    style A1a fill:#CA9161,color:#000
    style A1b fill:#CA9161,color:#000
```

**Code**:

```bash
# Download async-profiler
# https://github.com/async-profiler/async-profiler/releases
wget https://github.com/async-profiler/async-profiler/releases/download/v2.9/async-profiler-2.9-linux-x64.tar.gz
tar -xzf async-profiler-2.9-linux-x64.tar.gz
cd async-profiler-2.9-linux-x64

# Profile CPU usage (60 seconds, flame graph output)
./profiler.sh -d 60 -f cpu-flamegraph.html <pid>
                                 # => -d 60: duration 60 seconds
                                 # => -f: output file (HTML flame graph)
                                 # => <pid>: Java process ID (from jps)
                                 # => Samples stack traces at 100Hz (configurable)
                                 # => Output: interactive HTML flame graph

# Profile allocations (60 seconds)
./profiler.sh -d 60 -e alloc -f alloc-flamegraph.html <pid>
                                 # => -e alloc: allocation profiling event
                                 # => Tracks object allocations (TLAB-based)
                                 # => Flame graph shows allocation hotspots
                                 # => Width = allocation size (bytes)

# Advanced: Profile specific event
./profiler.sh -d 60 -e cpu,alloc -f combined.html <pid>
                                 # => -e cpu,alloc: both CPU and allocation
                                 # => Combined flame graph (two tabs)

# Set sampling interval (default 10ms = 100Hz)
./profiler.sh -d 60 -i 5ms -f cpu-high-res.html <pid>
                                 # => -i 5ms: sample every 5ms (200Hz)
                                 # => Higher frequency: more accurate, higher overhead
                                 # => Lower frequency: less overhead, less detail

# Filter by thread name
./profiler.sh -d 60 -t --include 'worker-*' -f worker-threads.html <pid>
                                 # => -t: thread mode (per-thread flame graph)
                                 # => --include 'worker-*': only threads matching pattern
                                 # => Isolate specific thread pool behavior
```

**Flame Graph Interpretation**:

```bash
# Flame graph structure:
# - X-axis: alphabetical order (NOT time!)
# - Y-axis: stack depth (call hierarchy)
# - Width: sample count (CPU time or allocation size)
# - Color: random (distinguish frames, no meaning)
#
# Hot spot identification:
# 1. Wide frames at bottom = common entry points
# 2. Wide frames at top = CPU hotspots / allocation sources
# 3. Tall stacks = deep call chains (potential optimization)
# 4. Plateaus = method calling many sub-methods
#
# Example:
# main (100%)
#   ├─ processRequests (80%)
#   │    ├─ parseData (50%)    ← WIDEST: CPU hotspot
#   │    │    └─ String.split() (40%)  ← Allocation source
#   │    └─ validateData (30%)
#   └─ logging (20%)
#
# Optimization: Optimize parseData (50% of CPU time)
# => Replace String.split() with custom parser
# => Reduce String allocations (40% of allocations)
```

**Integration with Application**:

```java
// Trigger async-profiler programmatically (requires native agent)
// Add to JVM args: -agentpath:/path/to/libasyncProfiler.so=start,file=profile.html

// Or use jattach for dynamic profiling
// ./jattach <pid> start,event=cpu,file=profile.html
// ./jattach <pid> stop

public class ProfiledApplication {
    public static void main(String[] args) {
        // Run workload
        for (int i = 0; i < 1_000_000; i++) {
            processData("Sample input " + i);
                                 // => async-profiler samples this method
                                 // => Stack traces captured at 100Hz
        }

        // Profile captured in profile.html
        // Open in browser to view flame graph
    }

    static void processData(String input) {
        // Parse data (potential hotspot)
        String[] parts = input.split(" ");
                                 // => String.split() allocates String[] and Strings
                                 // => Shows up in allocation flame graph

        // Validate parts
        for (String part : parts) {
            if (part.isEmpty()) {
                throw new IllegalArgumentException("Empty part");
            }
        }
    }
}
```

**Key Takeaway**: async-profiler generates interactive flame graphs for CPU and allocation profiling. Sample at 100Hz for production profiling (low overhead). Flame graph width indicates hotspot severity. Identify optimization targets by finding widest top-level frames.

**Why It Matters**: Flame graphs visualize profiling data intuitively. Wide frames instantly highlight bottlenecks. async-profiler's OS-level sampling captures native code and JVM internals (JFR cannot). Essential for deep performance analysis.

## Example 107: Micro-Benchmarking with JMH

JMH (Java Microbenchmark Harness) provides reliable micro-benchmarks. Handles JIT warmup, dead code elimination, and loop optimizations.

**JMH Workflow:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    Code["Write Benchmark<br/>@Benchmark methods"]
    Code --> Warmup["Warmup Phase<br/>JIT compilation"]
    Warmup --> Measure["Measurement Phase<br/>collect samples"]
    Measure --> Stats["Statistical Analysis<br/>mean, p95, p99"]

    style Code fill:#0173B2,color:#fff
    style Warmup fill:#DE8F05,color:#000
    style Measure fill:#029E73,color:#fff
    style Stats fill:#CC78BC,color:#000
```

**Code**:

```xml
<!-- Add JMH dependencies to pom.xml -->
<dependency>
    <groupId>org.openjdk.jmh</groupId>
    <artifactId>jmh-core</artifactId>
    <version>1.37</version>
    <scope>test</scope>
</dependency>
<dependency>
    <groupId>org.openjdk.jmh</groupId>
    <artifactId>jmh-generator-annprocess</artifactId>
    <version>1.37</version>
    <scope>test</scope>
</dependency>
```

**Benchmark Example**:

```java
import org.openjdk.jmh.annotations.*;
import java.util.concurrent.TimeUnit;

// Benchmark: String concatenation vs StringBuilder
@State(Scope.Benchmark)          // => Benchmark-level state (shared across iterations)
                                 // => Scope.Thread: per-thread state
                                 // => Scope.Group: per-thread-group state
@BenchmarkMode(Mode.AverageTime) // => Measure average time per operation
                                 // => Alternative modes:
                                 // => Mode.Throughput: ops/second
                                 // => Mode.SampleTime: p50, p95, p99
                                 // => Mode.SingleShotTime: single invocation
@OutputTimeUnit(TimeUnit.MICROSECONDS)
                                 // => Report results in microseconds
                                 // => Alternatives: NANOSECONDS, MILLISECONDS, SECONDS
@Warmup(iterations = 3, time = 1)
                                 // => 3 warmup iterations, 1 second each
                                 // => Warmup triggers JIT compilation
                                 // => Discard warmup results (not measured)
@Measurement(iterations = 5, time = 1)
                                 // => 5 measurement iterations, 1 second each
                                 // => Collect performance data
                                 // => Average across iterations
@Fork(1)                         // => Run benchmark in 1 forked JVM
                                 // => Isolates from other benchmarks
                                 // => Prevents profile pollution
public class StringConcatenationBenchmark {

    private static final int ITERATIONS = 10_000;
                                 // => Loop iterations per benchmark invocation
                                 // => Balances measurement accuracy vs overhead

    @Benchmark
    public String stringConcatenation() {
                                 // => Benchmark method (annotated with @Benchmark)
                                 // => JMH calls repeatedly, measures time
        String result = "";      // => Empty string (immutable)
        for (int i = 0; i < ITERATIONS; i++) {
                                 // => Loop 10,000 times
            result += "data";    // => String concatenation (creates new String)
                                 // => Allocates ITERATIONS new Strings
                                 // => O(n²) complexity (copy entire string each time)
        }
        return result;           // => Return to prevent dead code elimination
                                 // => JMH ensures result is "used"
    }

    @Benchmark
    public String stringBuilder() {
                                 // => Alternative approach: StringBuilder
        StringBuilder sb = new StringBuilder(ITERATIONS * 4);
                                 // => Pre-size StringBuilder (capacity = 40,000 chars)
                                 // => Avoids internal array resizing
        for (int i = 0; i < ITERATIONS; i++) {
                                 // => Loop 10,000 times
            sb.append("data");   // => Append to existing StringBuilder
                                 // => Reuses same StringBuilder instance
                                 // => O(n) complexity (amortized constant time per append)
        }
        return sb.toString();    // => Convert to String at end
                                 // => Single String allocation
    }

    // Run benchmarks
    public static void main(String[] args) throws Exception {
        org.openjdk.jmh.Main.main(args);
                                 // => Executes JMH framework
                                 // => Discovers @Benchmark methods
                                 // => Runs warmup and measurement phases
                                 // => Prints results table
    }
}

// Example output:
// Benchmark                                Mode  Cnt     Score      Error  Units
// StringConcatenationBenchmark.stringConcatenation  avgt    5  1250.000 ±   15.000  us/op
// StringConcatenationBenchmark.stringBuilder        avgt    5     2.500 ±    0.200  us/op
//
// Analysis:
// => stringConcatenation: 1250 μs per operation (very slow)
// => stringBuilder: 2.5 μs per operation (500x faster!)
// => Error: standard deviation across iterations
// => StringBuilder is clear winner for repeated concatenation
```

**Advanced JMH Features**:

```java
@State(Scope.Benchmark)
@BenchmarkMode(Mode.Throughput)  // => Measure operations per second
@OutputTimeUnit(TimeUnit.SECONDS)
public class AdvancedBenchmark {

    private List<Integer> data;

    @Setup                       // => Setup method (runs before benchmarks)
                                 // => Initializes state once per benchmark run
    public void setup() {
        data = new ArrayList<>();
        for (int i = 0; i < 10_000; i++) {
            data.add(i);         // => Populate test data
        }
    }

    @Benchmark
    public int sequentialSum() {
                                 // => Sequential stream reduction
        return data.stream()
            .reduce(0, Integer::sum);
                                 // => Single-threaded summation
    }

    @Benchmark
    public int parallelSum() {
                                 // => Parallel stream reduction
        return data.parallelStream()
            .reduce(0, Integer::sum);
                                 // => Multi-threaded summation (ForkJoinPool)
    }

    @TearDown                    // => Teardown method (runs after benchmarks)
                                 // => Cleanup resources
    public void teardown() {
        data.clear();            // => Release memory
        data = null;
    }
}

// Example output (ops/sec, higher is better):
// Benchmark                          Mode  Cnt      Score       Error  Units
// AdvancedBenchmark.sequentialSum   thrpt    5   850.000 ±   10.000  ops/s
// AdvancedBenchmark.parallelSum     thrpt    5  1200.000 ±   20.000  ops/s
//
// => parallelSum: 41% faster (multi-core utilization)
```

**Key Takeaway**: JMH handles JIT warmup, dead code elimination, and statistical analysis automatically. Use `@Warmup` to trigger JIT compilation before measurement. Choose `BenchmarkMode` based on metric (throughput, latency, samples). Pre-size data structures in `@Setup` to isolate operation being benchmarked.

**Why It Matters**: Naive micro-benchmarks are unreliable due to JIT warmup, loop optimizations, and dead code elimination. JMH addresses these pitfalls. Essential for comparing algorithm implementations, data structure choices, and API alternatives. Provides statistically rigorous results with error bounds.

## Example 108: Memory Leak Detection

Memory leaks occur when objects remain reachable despite being logically dead. Use heap dumps and JFR to identify leak sources.

**Common Memory Leak Patterns:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    App["Application"]
    App --> Cache["Static Cache<br/>never cleared"]
    App --> Listener["Event Listeners<br/>not unregistered"]
    App --> ThreadLocal["ThreadLocal<br/>not removed"]

    Cache --> Leak1["Memory Leak<br/>unbounded growth"]
    Listener --> Leak2["Memory Leak<br/>references retained"]
    ThreadLocal --> Leak3["Memory Leak<br/>thread pool reuse"]

    style App fill:#0173B2,color:#fff
    style Cache fill:#DE8F05,color:#000
    style Listener fill:#DE8F05,color:#000
    style ThreadLocal fill:#DE8F05,color:#000
    style Leak1 fill:#029E73,color:#fff
    style Leak2 fill:#029E73,color:#fff
    style Leak3 fill:#029E73,color:#fff
```

**Code**:

```java
// LEAK EXAMPLE 1: Static cache without eviction
public class LeakyCacheExample {
    // Static map holds all entries forever
    private static final Map<String, byte[]> cache = new HashMap<>();
                                 // => Static: survives entire JVM lifetime
                                 // => No eviction policy (unbounded growth)
                                 // => Objects never garbage collected

    public static byte[] loadData(String key) {
        return cache.computeIfAbsent(key, k -> {
                                 // => If key absent, compute value
            byte[] data = new byte[1024 * 1024];
                                 // => 1MB byte array (large allocation)
            // Load data from disk/network...
            return data;         // => Stored in cache forever
                                 // => LEAK: never evicted, even if unused
        });
    }

    // FIX: Use bounded cache with LRU eviction
    private static final Map<String, byte[]> boundedCache =
        Collections.synchronizedMap(new LinkedHashMap<>(100, 0.75f, true) {
                                 // => LinkedHashMap with access-order
                                 // => 100 initial capacity, 0.75 load factor
                                 // => true: access-order (LRU)
            @Override
            protected boolean removeEldestEntry(Map.Entry<String, byte[]> eldest) {
                return size() > 100;
                                 // => Evict oldest when size exceeds 100
                                 // => LRU policy: least recently accessed removed
            }
        });

    public static byte[] loadDataFixed(String key) {
        return boundedCache.computeIfAbsent(key, k -> new byte[1024 * 1024]);
                                 // => Cache bounded at 100 entries
                                 // => Old entries evicted, eligible for GC
    }
}

// LEAK EXAMPLE 2: Event listeners not unregistered
public class LeakyListenerExample {
    private final List<EventListener> listeners = new ArrayList<>();
                                 // => Instance field (one per object)

    public void addEventListener(EventListener listener) {
        listeners.add(listener); // => Adds listener to list
                                 // => Listener referenced by list
                                 // => LEAK: if listener not removed, never GC'd
    }

    public void removeEventListener(EventListener listener) {
        listeners.remove(listener);
                                 // => Removes listener from list
                                 // => Listener now eligible for GC
    }

    // Common mistake: forget to remove listener
    public void problematicUsage() {
        EventListener temp = new EventListener() {
            @Override
            public void onEvent(Event e) {
                // Handle event
            }
        };
        addEventListener(temp);  // => Listener added

        // temp goes out of scope, but still referenced by listeners list
        // => LEAK: temp never garbage collected
    }

    // FIX: Use WeakReference for listeners
    private final List<WeakReference<EventListener>> weakListeners = new ArrayList<>();
                                 // => WeakReference doesn't prevent GC

    public void addEventListenerFixed(EventListener listener) {
        weakListeners.add(new WeakReference<>(listener));
                                 // => Listener wrapped in WeakReference
                                 // => GC can collect listener if no strong references
    }

    public void fireEvent(Event e) {
        // Clean up dead references while iterating
        Iterator<WeakReference<EventListener>> it = weakListeners.iterator();
        while (it.hasNext()) {
            EventListener listener = it.next().get();
                                 // => WeakReference.get() returns referent
                                 // => Returns null if GC'd
            if (listener == null) {
                it.remove();     // => Remove dead reference
            } else {
                listener.onEvent(e);
                                 // => Invoke listener
            }
        }
    }
}

// LEAK EXAMPLE 3: ThreadLocal not cleaned up
public class LeakyThreadLocalExample {
    private static final ThreadLocal<byte[]> threadData = new ThreadLocal<>();
                                 // => ThreadLocal: per-thread variable
                                 // => Stored in Thread's ThreadLocalMap

    public void processRequest() {
        byte[] buffer = new byte[10 * 1024 * 1024];
                                 // => 10MB buffer (large allocation)
        threadData.set(buffer);  // => Store in ThreadLocal
                                 // => Associated with current thread

        // Process request...

        // LEAK: ThreadLocal not removed after request
        // => In thread pool, thread reused for next request
        // => buffer referenced by ThreadLocalMap, not GC'd
        // => Each request accumulates 10MB leak
    }

    // FIX: Always remove ThreadLocal after use
    public void processRequestFixed() {
        try {
            byte[] buffer = new byte[10 * 1024 * 1024];
            threadData.set(buffer);

            // Process request...

        } finally {
            threadData.remove(); // => Remove ThreadLocal entry
                                 // => buffer eligible for GC
                                 // => Prevents leak in thread pools
        }
    }
}
```

**Detecting Memory Leaks with Heap Dumps**:

```bash
# Capture heap dump (running JVM)
jcmd <pid> GC.heap_dump /tmp/heap.hprof
                                 # => Dumps heap to /tmp/heap.hprof
                                 # => File size = heap size (can be large!)
                                 # => JVM pauses briefly during dump

# Alternative: OutOfMemoryError auto-dump
java -XX:+HeapDumpOnOutOfMemoryError \
     -XX:HeapDumpPath=/tmp/oom.hprof \
     -jar myapp.jar
                                 # => Auto-dump when OutOfMemoryError occurs
                                 # => Captures heap state at failure
                                 # => Critical for diagnosing OOM causes

# Analyze heap dump with Eclipse Memory Analyzer (MAT)
# Download: https://www.eclipse.org/mat/
# Open heap.hprof in MAT
# => Leak Suspects Report: identifies likely leaks
# => Dominator Tree: shows retained size by object
# => Histogram: object counts and sizes by class
```

**Analyzing Heap Dump**:

```java
// Example heap dump analysis findings:

// 1. Large HashMap retaining 2GB
// Class: java.util.HashMap
// Instances: 1
// Shallow Size: 48 bytes (object header + fields)
// Retained Size: 2,147,483,648 bytes (2GB)
// => HashMap holds 2GB of data (leak suspect!)
//
// Dominator tree shows:
// HashMap -> Entry[] -> Entry -> byte[1MB] (×2048 entries)
// => 2048 entries, each 1MB = 2GB total
//
// Shortest path to GC root:
// GC Root (Thread) -> MyClass (static) -> HashMap (field)
// => Static reference prevents GC
// => Fix: Make HashMap non-static or add eviction

// 2. ThreadLocal leak in thread pool
// Class: java.lang.ThreadLocal$ThreadLocalMap$Entry
// Instances: 200
// Retained Size per entry: 10MB
// Total: 2GB (200 threads × 10MB)
// => ThreadLocal not cleaned up
// => Each thread holds 10MB buffer
//
// Fix: Add threadLocal.remove() in finally block
```

**Key Takeaway**: Memory leaks occur when objects remain reachable despite being logically dead. Common patterns: unbounded caches, unregistered listeners, ThreadLocal in thread pools. Use heap dumps (`jcmd GC.heap_dump`) and Eclipse MAT to identify leaks. Fix by adding eviction policies, WeakReferences, or explicit cleanup.

**Why It Matters**: Memory leaks cause gradual performance degradation and OutOfMemoryErrors. Small leaks (10KB/request) become critical under load (1000 req/sec = 10MB/sec = 36GB/hour). Early detection prevents production outages.

## Example 109: Optimizing String Allocation

String concatenation in loops creates excessive temporary objects. Use StringBuilder to reduce allocations by 99%.

**String Allocation Patterns:**

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Concat["String + Concatenation"]
    Builder["StringBuilder.append"]

    Concat --> C1["Iteration 1<br/>new String"]
    Concat --> C2["Iteration 2<br/>new String"]
    Concat --> C3["Iteration N<br/>new String"]

    Builder --> B1["Iteration 1-N<br/>same StringBuilder"]

    C3 --> CResult["N String objects<br/>high allocation"]
    B1 --> BResult["1 StringBuilder<br/>low allocation"]

    style Concat fill:#DE8F05,color:#000
    style Builder fill:#029E73,color:#fff
    style C1 fill:#CC78BC,color:#000
    style C2 fill:#CC78BC,color:#000
    style C3 fill:#CC78BC,color:#000
    style B1 fill:#0173B2,color:#fff
    style CResult fill:#DE8F05,color:#000
    style BResult fill:#029E73,color:#fff
```

**Code**:

```java
import java.util.*;

// WRONG: String concatenation in loop
public class StringConcatWrong {
    public static String generateReport(List<String> items) {
        String report = "";      // => Empty string (immutable)
                                 // => Each concatenation creates new String

        for (String item : items) {
                                 // => Loop over items (assume 10,000 items)
            report += "Item: " + item + "\n";
                                 // => Three concatenations per iteration:
                                 // => "Item: " + item creates StringBuilder internally
                                 // => result + "\n" creates another StringBuilder
                                 // => report + result creates third String
                                 // => Total: 3 temporary objects per iteration
                                 // => 10,000 items × 3 = 30,000 temporary objects
                                 // => Plus 10,000 result Strings (cumulative)
                                 // => Grand total: 40,000+ allocations
        }

        return report;           // => Final report (huge String)
                                 // => Allocated 40,000+ temporary objects
                                 // => GC pressure: triggers young gen collections
    }
}

// CORRECT: StringBuilder for concatenation
public class StringConcatRight {
    public static String generateReport(List<String> items) {
        StringBuilder report = new StringBuilder(items.size() * 50);
                                 // => Pre-size StringBuilder
                                 // => Estimate: 50 chars per item
                                 // => 10,000 items × 50 = 500,000 chars capacity
                                 // => Avoids internal array resizing

        for (String item : items) {
                                 // => Loop over 10,000 items
            report.append("Item: ")
                                 // => Append to existing StringBuilder
                                 // => No new String allocated
                  .append(item)  // => Append item
                                 // => Reuses StringBuilder's char array
                  .append("\n"); // => Append newline
                                 // => Single StringBuilder instance used
                                 // => Total allocations: 1 StringBuilder + 1 final String
                                 // => 99.99% reduction vs concatenation (40,000 → 2)
        }

        return report.toString();// => Convert to String once at end
                                 // => Single String allocation (immutable result)
    }
}

// Benchmark: Measure allocation rate
import org.openjdk.jmh.annotations.*;
import java.util.concurrent.TimeUnit;

@State(Scope.Benchmark)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
public class StringAllocationBenchmark {

    private List<String> items;

    @Setup
    public void setup() {
        items = new ArrayList<>();
        for (int i = 0; i < 10_000; i++) {
            items.add("Data_" + i);
                                 // => 10,000 test items
        }
    }

    @Benchmark
    public String concatenation() {
                                 // => String + concatenation (wrong approach)
        return StringConcatWrong.generateReport(items);
                                 // => Expected: ~1,250ms (very slow)
                                 // => High allocation rate: ~40,000 objects
                                 // => Triggers multiple GCs during benchmark
    }

    @Benchmark
    public String stringBuilder() {
                                 // => StringBuilder.append (correct approach)
        return StringConcatRight.generateReport(items);
                                 // => Expected: ~2.5ms (500x faster!)
                                 // => Low allocation rate: 2 objects
                                 // => Minimal GC pressure
    }
}

// Example output:
// Benchmark                                      Mode  Cnt     Score    Error  Units
// StringAllocationBenchmark.concatenation        avgt    5  1250.000 ± 15.000  ms/op
// StringAllocationBenchmark.stringBuilder        avgt    5     2.500 ±  0.200  ms/op
//
// Analysis:
// => concatenation: 1250ms (allocation overhead dominates)
// => stringBuilder: 2.5ms (500x faster)
// => StringBuilder eliminates 99.95% of allocations
// => Critical for high-throughput systems (report generation, logging, etc.)

// Verify allocation reduction with JFR
// Run with: java -XX:StartFlightRecording=filename=alloc.jfr,duration=10s,settings=profile \
//                -jar benchmark.jar
//
// Analyze: jfr print --events jdk.ObjectAllocationInNewTLAB alloc.jfr
//
// concatenation method:
// => Top allocator: java.lang.String (40,000+ allocations)
// => Allocation rate: ~500MB/sec
//
// stringBuilder method:
// => Top allocator: java.lang.StringBuilder (2 allocations)
// => Allocation rate: ~5MB/sec (100x reduction)
```

**Key Takeaway**: String concatenation in loops (`report += item`) creates temporary StringBuilder objects and intermediate Strings. Use single StringBuilder with pre-sizing (`new StringBuilder(capacity)`) to reduce allocations by 99%. Measure allocation rate with JFR (`ObjectAllocationInNewTLAB`) to verify optimization.

**Why It Matters**: Excessive allocations trigger frequent young-gen GCs (pause times 10-50ms). High-throughput services (1000 req/sec) processing reports amplify the problem (40 million allocations/sec). StringBuilder optimization eliminates GC pressure, reduces latency, and improves throughput.
