---
title: "Advanced"
date: 2026-01-02T05:01:35+07:00
draft: false
weight: 10000003
description: "Master advanced Java through 25 examples: concurrency, JVM internals, design patterns, reflection, bytecode manipulation, and modern Java features"
tags:
  [
    "java",
    "tutorial",
    "by-example",
    "advanced",
    "concurrency",
    "jvm",
    "design-patterns",
    "reflection",
    "virtual-threads",
    "reactive",
  ]
---

Master advanced Java concepts through 25 annotated code examples. Build on intermediate foundations to explore advanced concurrency, JVM internals, design patterns, and modern Java features.

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
import java.sql.*;
import java.util.*;
import java.util.concurrent.*;

// Database connection interface
interface DatabaseConnection {
                                 // => Abstraction for different database types (Strategy pattern)
                                 // => Enables polymorphism: PostgresConnection, MySQLConnection
    void connect() throws SQLException;
                                 // => Opens database connection (expensive operation: 100-1000ms)
                                 // => May throw SQLException for connection failures
    void disconnect();           // => Closes connection and releases resources
    ResultSet executeQuery(String sql) throws SQLException;
                                 // => Executes SQL query, returns result set
    boolean isConnected();       // => Checks if connection is currently open
    String getConnectionString();// => Returns JDBC connection string
}

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
}

// MySQL connection implementation
class MySQLConnection implements DatabaseConnection {
                                 // => Concrete implementation for MySQL databases
                                 // => JDBC URL pattern: jdbc:mysql://host:port/database
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
            return connection != null && !connection.isClosed();
                                 // => true if connection valid and open
        } catch (SQLException e) {
            return false;        // => false on exception or if closed
        }
    }

    @Override
    public String getConnectionString() { return connString; }
                                 // => Returns MySQL JDBC connection string
}

// Factory for creating database connections
class ConnectionFactory {        // => Factory pattern: creates objects without exposing creation logic
                                 // => Centralizes connection creation, supports multiple database types
    public static DatabaseConnection create(String dbType, String host,
                                           int port, String database) {
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
    }
}

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
            this.poolSize = size;
                                 // => Sets number of connections (e.g., 20, 50)
            return this;         // => Returns this for chaining
        }

        public Builder timeout(int seconds) {
                                 // => Optional setter for connection timeout
            this.timeout = seconds;
                                 // => Sets timeout in seconds (e.g., 60, 120)
            return this;         // => Returns this for chaining
        }

        public ConnectionConfig build() {
                                 // => Terminal operation: creates ConnectionConfig instance
            return new ConnectionConfig(this);
                                 // => Calls private constructor with this Builder
                                 // => Returns immutable ConnectionConfig
        }
    }

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
}

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
        this.config = config;    // => Saves configuration
        this.availableConnections = new ConcurrentLinkedQueue<>();
                                 // => Initializes available connections queue (thread-safe)
        this.inUseConnections = ConcurrentHashMap.newKeySet();
                                 // => Initializes in-use connections set (thread-safe)

        // Pre-create pool connections
        for (int i = 0; i < config.getPoolSize(); i++) {
                                 // => Loops poolSize times (default: 10)
                                 // => Pre-creates connections to avoid lazy initialization delays
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
        }
        System.out.println("Connection pool initialized with " +
                         config.getPoolSize() + " connections");
                                 // => Output: "Connection pool initialized with 10 connections"
    }

    // Thread-safe Singleton with double-checked locking
    public static ConnectionPool getInstance(ConnectionConfig config) {
                                 // => Static factory method for Singleton instance
                                 // => Thread-safe: uses double-checked locking pattern
        if (instance == null) {  // => First check (no synchronization, fast path)
                                 // => Avoids synchronization overhead if already initialized
            synchronized (ConnectionPool.class) {
                                 // => Synchronizes on class object (ensures single instance)
                                 // => Lock acquired: other threads block here
                if (instance == null) {
                                 // => Second check inside synchronized block
                                 // => Prevents race condition: two threads pass first check
                    instance = new ConnectionPool(config);
                                 // => Creates Singleton instance (called once)
                }
            }
        }
        return instance;         // => Returns existing instance (all subsequent calls)
    }

    public DatabaseConnection acquire() throws InterruptedException {
                                 // => Acquires connection from pool (blocking if pool exhausted)
        DatabaseConnection conn = availableConnections.poll();
                                 // => poll(): removes and returns head of queue (non-blocking)
                                 // => Returns null if queue empty (no available connections)
        if (conn == null) {      // => Pool exhausted: no available connections
            System.out.println("Pool exhausted, creating new connection");
                                 // => Output: "Pool exhausted, creating new connection"
                                 // => Indicates pool undersized or connection leak
            conn = ConnectionFactory.create(
                config.getDbType(),
                config.getHost(),
                config.getPort(),
                config.getDatabase()
            );                   // => Creates new connection dynamically (overflow strategy)
                                 // => Alternative: block until connection available (HikariCP strategy)
        }
        inUseConnections.add(conn);
                                 // => Adds connection to in-use set
                                 // => Tracks borrowed connections for leak detection
        return conn; // => Connection from pool or newly created
                                 // => Caller must call release(conn) when done
                                 // => Connection leak if release() not called (pool exhaustion)
    }

    public void release(DatabaseConnection conn) {
                                 // => Returns connection to pool for reuse
        if (inUseConnections.remove(conn)) {
                                 // => Removes from in-use set
                                 // => Returns true if conn was in set (valid release)
            availableConnections.offer(conn);
                                 // => Adds connection back to available queue
                                 // => Connection now available for next acquire() call
            System.out.println("Connection returned to pool");
                                 // => Output: "Connection returned to pool"
        }
    }

    public void shutdown() {     // => Closes all connections and shuts down pool
        for (DatabaseConnection conn : availableConnections) {
                                 // => Iterates available connections
            conn.disconnect();   // => Closes each available connection
        }
        for (DatabaseConnection conn : inUseConnections) {
                                 // => Iterates in-use connections (may be leaked)
            conn.disconnect();   // => Closes each in-use connection (cleanup)
        }
        System.out.println("Connection pool shut down");
                                 // => Output: "Connection pool shut down"
    }

    public int getAvailableCount() { return availableConnections.size(); }
                                 // => Returns number of available connections (not in use)
    public int getInUseCount() { return inUseConnections.size(); }
                                 // => Returns number of in-use connections (borrowed)
}

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
    conn.connect();              // => Opens connection (TCP handshake + authentication)
                                 // => May throw SQLException
    ResultSet rs = conn.executeQuery("SELECT * FROM users");
                                 // => Executes SQL query, returns ResultSet
                                 // => rs contains all rows from users table
    // Process results...
} catch (SQLException e) {       // => Catches SQL exceptions (connection failures, query errors)
    e.printStackTrace();         // => Logs exception stack trace
} finally {
    pool.release(conn); // => Return to pool for reuse
                                 // => CRITICAL: always release in finally block
                                 // => Failure to release causes connection leak and pool exhaustion
                                 // => Connection returned to availableConnections queue
}

// Pool statistics
System.out.println("Available: " + pool.getAvailableCount());
                                 // => Output: "Available: 19" (20 total - 1 in use)
                                 // => Shows connections ready for reuse
System.out.println("In use: " + pool.getInUseCount());
                                 // => Output: "In use: 1" (conn acquired but not released yet)
                                 // => Shows connections currently borrowed

pool.shutdown(); // Cleanup
                                 // => Closes all connections (available + in-use)
                                 // => Call on application shutdown (cleanup resources)
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

// Bad - multiple responsibilities
class UserServiceBad {
    public void createUser(User user) {
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
    public boolean validate(User user) {
        // Only validation logic
                                 // => Single responsibility: validation
        return user.getName() != null && user.getEmail() != null;
                                 // => Returns true if user data valid
    }
}

class UserRepository {
    public void save(User user) {
        // Only database operations
                                 // => Single responsibility: persistence
                                 // => Database changes isolated to this class
    }
}

class EmailService {
    public void sendWelcomeEmail(User user) {
        // Only email sending   // => Single responsibility: email notifications
                                 // => Email format changes isolated to this class
    }
}

class UserService {
    private final UserValidator validator;
    private final UserRepository repository;
    private final EmailService emailService;
                                 // => Coordinates single-responsibility classes
                                 // => Delegates to specialized classes

    public void createUser(User user) {
        if (validator.validate(user)) {
                                 // => Delegates validation
            repository.save(user);
                                 // => Delegates persistence
            emailService.sendWelcomeEmail(user);
                                 // => Delegates notification
        }                        // => Orchestration: composing single-responsibility classes
    }
}

// Open/Closed Principle (OCP)
// Open for extension, closed for modification
                                 // => Classes open for extension (new features)
                                 // => Closed for modification (existing code unchanged)

// Bad - modifying class for new behavior
class AreaCalculatorBad {
    public double calculate(Object shape) {
        if (shape instanceof Circle) {
                                 // => Type check for each shape type
            Circle c = (Circle) shape;
            return Math.PI * c.radius * c.radius;
        } else if (shape instanceof Rectangle) {
            Rectangle r = (Rectangle) shape;
            return r.width * r.height;
        }
        // Adding new shape requires modifying this method
                                 // => Violates OCP: must modify class to add Triangle
                                 // => Each new shape = modify existing code
        return 0;
    }
}

// Good - extend via polymorphism
interface Shape {              // => Abstraction for all shapes
    double area();               // => Contract: all shapes calculate area
}

class Circle implements Shape {
    double radius;
    public double area() { return Math.PI * radius * radius; }
                                 // => Circle-specific area calculation
}

class Rectangle implements Shape {
    double width, height;
    public double area() { return width * height; }
                                 // => Rectangle-specific area calculation
}

class Triangle implements Shape {
    double base, height;
    public double area() { return 0.5 * base * height; }
                                 // => Triangle added WITHOUT modifying existing classes
}

class AreaCalculator {
    public double calculate(Shape shape) {
        return shape.area();     // => Polymorphism: delegates to shape implementation
                                 // => No modification needed for new shapes
                                 // => Open for extension (add Triangle), closed for modification
    }
}

// Liskov Substitution Principle (LSP)
// Subtypes must be substitutable for base types
                                 // => Subclass should be usable wherever parent expected
                                 // => Subclass must not break parent's contract

// Bad - violates LSP
class Bird {
    public void fly() { System.out.println("Flying"); }
                                 // => Contract: all Birds can fly
}

class Penguin extends Bird {
    @Override
    public void fly() {
        throw new UnsupportedOperationException("Penguins can't fly");
                                 // => Breaks Bird contract: fly() expected to work
        // Violates LSP: cannot substitute Penguin for Bird
                                 // => Code expecting Bird will fail with Penguin
                                 // => Subclass breaks parent's behavioral contract
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

// Bad - fat interface
interface WorkerBad {            // => Single interface with all capabilities
    void work();                 // => Method 1
    void eat();                  // => Method 2 (not all workers eat)
    void sleep();                // => Method 3 (not all workers sleep)
}

class RobotBad implements WorkerBad {
    public void work() { /* work */ }
                                 // => Robot can work
    public void eat() { /* robots don't eat - forced to implement */ }
                                 // => Forced to implement irrelevant method
    public void sleep() { /* robots don't sleep - forced to implement */ }
                                 // => Violates ISP: depends on methods it doesn't use
}

// Good - segregated interfaces
interface Workable {             // => Small, focused interface: working
    void work();                 // => Single capability
}

interface Eatable {              // => Small, focused interface: eating
    void eat();                  // => Single capability
}

interface Sleepable {            // => Small, focused interface: sleeping
    void sleep();                // => Single capability
}

class Human implements Workable, Eatable, Sleepable {
                                 // => Human implements all three capabilities
    public void work() { /* work */ }
                                 // => Human can work
    public void eat() { /* eat */ }
                                 // => Human can eat
    public void sleep() { /* sleep */ }
                                 // => Human can sleep
}

class Robot implements Workable {
                                 // => Robot implements ONLY what it needs
    public void work() { /* work */ }
                                 // => Robot can work
    // Only implements what it needs
                                 // => No forced implementation of irrelevant methods
                                 // => Respects ISP: depends only on Workable
}

// Dependency Inversion Principle (DIP)
// Depend on abstractions, not concretions
                                 // => High-level modules shouldn't depend on low-level modules
                                 // => Both should depend on abstractions

// Bad - depends on concrete class
class OrderProcessorBad {
    private MySQLDatabase database = new MySQLDatabase();
                                 // => Hardcoded dependency on concrete MySQLDatabase
                                 // => Tightly coupled: cannot switch to PostgreSQL

    public void process(Order order) {
        database.save(order);    // => Tightly coupled to MySQL
                                 // => Cannot use different database without code modification
    }
}

// Good - depends on abstraction
interface Database {             // => Abstraction: defines contract
    void save(Order order);      // => Contract: save method
}

class MySQLDatabase implements Database {
                                 // => Concrete implementation: MySQL
    public void save(Order order) { /* MySQL implementation */ }
                                 // => MySQL-specific persistence logic
}

class PostgreSQLDatabase implements Database {
                                 // => Alternative implementation: PostgreSQL
    public void save(Order order) { /* PostgreSQL implementation */ }
                                 // => PostgreSQL-specific persistence logic
}

class OrderProcessor {
    private final Database database;
                                 // => Depends on abstraction (Database interface)
                                 // => Not coupled to any specific implementation

    public OrderProcessor(Database database) {
        this.database = database;// => Depends on abstraction
                                 // => Injected: caller provides concrete implementation
    }

    public void process(Order order) {
        database.save(order);    // => Works with any Database implementation
                                 // => Polymorphism: MySQL, PostgreSQL, or any Database
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
Class<?> clazz = loader.loadClass("com.example.MyClass");
Object instance = clazz.getDeclaredConstructor().newInstance();

// Class identity determined by (class name + ClassLoader)
// Same class loaded by different loaders are different classes!
ClassLoader loader1 = new CustomClassLoader("/path1");
ClassLoader loader2 = new CustomClassLoader("/path2");
Class<?> class1 = loader1.loadClass("MyClass");
Class<?> class2 = loader2.loadClass("MyClass");
// class1 != class2 (different loaders)

// Hot reload use case
class HotReloadClassLoader extends ClassLoader {
    private String classPath;

    public HotReloadClassLoader(String classPath) {
        this.classPath = classPath;
    }

    public Class<?> reload(String name) throws Exception {
        // Create new loader for each reload (isolates class versions)
        CustomClassLoader freshLoader = new CustomClassLoader(classPath);
        return freshLoader.loadClass(name);
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
import org.objectweb.asm.*;

ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
// => Creates bytecode writer
cw.visit(Opcodes.V17, Opcodes.ACC_PUBLIC, "GeneratedClass", null, "java/lang/Object", null);
// => Defines class structure: Java 17, public, extends Object

MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "hello", "()Ljava/lang/String;", null, null);
// => Creates public String hello() method
mv.visitCode();
mv.visitLdcInsn("Hello");
// => Load constant "Hello" onto stack
mv.visitInsn(Opcodes.ARETURN);
// => Return string from stack
mv.visitMaxs(1, 1);
mv.visitEnd();

byte[] bytecode = cw.toByteArray();
// => Generates .class file bytes

CustomClassLoader loader = new CustomClassLoader();
Class<?> clazz = loader.defineClass("GeneratedClass", bytecode);
// => Loads generated class into JVM
Object instance = clazz.getDeclaredConstructor().newInstance();
Method method = clazz.getMethod("hello");
String result = (String) method.invoke(instance);
// => Result: "Hello"
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
import javax.ws.rs.core.*;   // => Core classes: Response, MediaType

@Path("/users")              // => Base path for all methods in this class
                                 // => Maps to /users endpoint
@Produces(MediaType.APPLICATION_JSON)
                                 // => All responses return JSON
@Consumes(MediaType.APPLICATION_JSON)
                                 // => All requests accept JSON
class UserResource {         // => JAX-RS resource class (REST controller)
    @GET                     // => HTTP GET method (retrieve resources)
    public Response getAllUsers() {
                                 // => GET /users - returns all users
        List<User> users = userService.findAll();
                                 // => Fetches users from service layer
        return Response.ok(users).build();
                                 // => Returns HTTP 200 OK with users JSON
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
    public Response createUser(User user) {
                                 // => POST /users - creates new user
                                 // => User deserialized from JSON request body
        userService.create(user);
                                 // => Persists user to database
        return Response.status(Response.Status.CREATED).entity(user).build();
                                 // => Returns HTTP 201 Created with created user
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
    public Response deleteUser(@PathParam("id") Long id) {
                                 // => Deletes user with specified ID
        userService.delete(id);
                                 // => Removes user from database
        return Response.noContent().build();
                                 // => Returns HTTP 204 No Content (successful deletion)
    }
}

// MicroProfile Config - externalized configuration
import org.eclipse.microprofile.config.inject.ConfigProperty;
                                 // => MicroProfile Config annotation for property injection

@ApplicationScoped           // => CDI scope: one instance per application
                                 // => Shared across all requests (singleton-like)
class ConfigExample {
    @Inject                  // => CDI dependency injection
    @ConfigProperty(name = "database.url")
                                 // => Injects value from config property "database.url"
                                 // => Checks system properties, env vars, config files
    String databaseUrl; // Injected from config source
                                 // => databaseUrl = value from DATABASE_URL env var or database.url property
                                 // => Example: "jdbc:postgresql://localhost:5432/mydb"

    @Inject
    @ConfigProperty(name = "database.timeout", defaultValue = "30")
                                 // => Injects "database.timeout" property with default 30
                                 // => Falls back to 30 if property not found
    int timeout;             // => timeout = 30 (default) or configured value
}

// Config sources: system properties, environment variables, microprofile-config.properties
                                 // => Config priority: system properties > env vars > config files
                                 // => DATABASE_URL env var overrides database.url property

// Health checks
import org.eclipse.microprofile.health.*;
                                 // => MicroProfile Health API for health check endpoints

@Health                      // => Marks class as health check (deprecated, use @Liveness/@Readiness)
@ApplicationScoped           // => One instance per application
class DatabaseHealthCheck implements HealthCheck {
                                 // => HealthCheck interface: single call() method
    @Override
    public HealthCheckResponse call() {
                                 // => Called by health check endpoint (GET /health)
        boolean isHealthy = checkDatabaseConnection();
                                 // => Checks if database connection is alive
        return HealthCheckResponse.named("database")
                                 // => Health check name: "database"
            .state(isHealthy)    // => UP if true, DOWN if false
            .withData("connection", "active")
                                 // => Additional metadata in health response
            .build();            // => Returns JSON: {"name":"database","state":"UP","data":{"connection":"active"}}
    }
}

// Liveness vs Readiness
@Liveness // Pod should be restarted if fails
                                 // => Liveness probe: checks if app is running
                                 // => Kubernetes restarts pod if fails (app deadlocked/crashed)
class LivenessCheck implements HealthCheck { /*...*/ }
                                 // => GET /health/live - returns UP if app alive

@Readiness // Pod should not receive traffic if fails
                                 // => Readiness probe: checks if app can handle requests
                                 // => Kubernetes stops sending traffic if fails (warming up, dependencies down)
class ReadinessCheck implements HealthCheck { /*...*/ }
                                 // => GET /health/ready - returns UP if ready for traffic

// Metrics
import org.eclipse.microprofile.metrics.annotation.*;
                                 // => MicroProfile Metrics annotations

@ApplicationScoped
class MetricsExample {
    @Counted(name = "user_requests", description = "Number of user requests")
                                 // => Counter metric: increments each time method called
                                 // => Exposed as Prometheus metric: user_requests_total
    @Timed(name = "user_request_time", description = "Time to process user requests")
                                 // => Timer metric: records execution duration
                                 // => Exposes p50, p99, max, mean latencies
    public void processUser() {
        // Method execution counted and timed
                                 // => Each call increments counter and records duration
                                 // => Metrics available at GET /metrics (Prometheus format)
    }

    @Gauge(name = "active_users", unit = MetricUnits.NONE)
                                 // => Gauge metric: current value (not cumulative)
                                 // => Polled periodically, returns current active users
    public long getActiveUsers() {
        return userService.countActive();
                                 // => Returns current count of active users
                                 // => Gauge value = return value
    }
}

// Fault Tolerance
import org.eclipse.microprofile.faultto tolerance.*;
                                 // => MicroProfile Fault Tolerance for resilience patterns

@ApplicationScoped
class FaultToleranceExample {
    @Retry(maxRetries = 3, delay = 1000) // Retry up to 3 times, 1 second delay
                                 // => Automatically retries on exception
                                 // => maxRetries = 3: tries up to 4 times total (initial + 3 retries)
                                 // => delay = 1000ms between retry attempts
    public String callExternalService() {
                                 // => Calls external service with automatic retry
        return externalService.getData();
                                 // => If fails, retries 3 times with 1 second delay
                                 // => Total max time: ~3 seconds (3 retries × 1 second)
    }

    @CircuitBreaker(
        requestVolumeThreshold = 4,
                                 // => Minimum 4 requests before circuit opens
                                 // => Circuit doesn't activate until 4 requests received
        failureRatio = 0.5,      // => Opens if 50% of requests fail
                                 // => Example: 2 of 4 requests fail → circuit opens
        delay = 5000             // => Circuit stays open for 5 seconds
                                 // => After 5 seconds, enters half-open state (allows 1 request)
    )
    public String callUnreliableService() {
        // Circuit opens if 50% of 4 requests fail
        // Opens for 5 seconds before half-open state
                                 // => While open: immediately throws CircuitBreakerOpenException
                                 // => Half-open: allows 1 request. Success → closes, failure → opens
        return unreliableService.getData();
                                 // => Returns data if circuit closed/half-open and request succeeds
    }

    @Timeout(1000) // Timeout after 1 second
                                 // => Throws TimeoutException if method exceeds 1000ms
                                 // => Interrupts long-running operations
    @Fallback(fallbackMethod = "fallbackMethod")
                                 // => Calls fallbackMethod if timeout or exception occurs
                                 // => fallbackMethod must have same signature
    public String callSlowService() {
                                 // => Calls service with 1 second timeout
        return slowService.getData();
                                 // => If > 1 second, throws TimeoutException → calls fallbackMethod
    }

    public String fallbackMethod() {
                                 // => Fallback method (same signature as callSlowService)
        return "Fallback data"; // => Returns default data when main method fails
                                 // => Called on timeout, exception, or circuit breaker open
    }

    @Bulkhead(value = 5, waitingTaskQueue = 10)
                                 // => Limits concurrent executions (semaphore pattern)
                                 // => value = 5: max 5 concurrent executions
                                 // => waitingTaskQueue = 10: max 10 waiting requests
    public String limitedConcurrency() {
        // Max 5 concurrent executions, 10 waiting
                                 // => 6th concurrent request waits in queue (up to 10)
                                 // => 16th concurrent request immediately rejected (BulkheadException)
        return service.getData();
                                 // => Returns data if semaphore acquired (not at limit)
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
    void subscribe(Subscriber<? super T> subscriber);
                                 // => Connects subscriber to this publisher
                                 // => Triggers data flow when subscriber ready
}

// Subscriber interface
interface Subscriber<T> {        // => Consumer of data (receives elements)
    void onSubscribe(Subscription subscription);
                                 // => Called first when subscription starts
                                 // => Receives Subscription for backpressure control
    void onNext(T item);         // => Called for each emitted item
                                 // => Receives one element at a time
    void onError(Throwable throwable);
                                 // => Called on error (terminal event)
                                 // => No more onNext calls after onError
    void onComplete();           // => Called when publisher finished (terminal event)
                                 // => No more onNext calls after onComplete
}

// Subscription interface
interface Subscription {         // => Controls data flow between Publisher and Subscriber
                                 // => Implements backpressure mechanism
    void request(long n); // Request n items (backpressure)
                                 // => Subscriber requests n items from Publisher
                                 // => Publisher sends ≤ n items (respects backpressure)
    void cancel(); // Cancel subscription
                                 // => Subscriber cancels subscription (no more items)
                                 // => Publisher stops emitting
}

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
