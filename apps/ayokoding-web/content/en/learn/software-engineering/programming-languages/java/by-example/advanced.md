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

## Example 61: Concurrent Collections

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

## Example 62: Atomic Variables

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

## Example 63: CountDownLatch and CyclicBarrier

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
CountDownLatch latch = new CountDownLatch(3); // Wait for 3 events

// Worker threads count down
for (int i = 0; i < 3; i++) {
    int taskId = i;
    new Thread(() -> {
        System.out.println("Task " + taskId + " starting");
        try { Thread.sleep(1000); } catch (InterruptedException e) {}
        System.out.println("Task " + taskId + " done");
        latch.countDown(); // Decrement count
    }).start();
}

// Main thread waits for all tasks
latch.await(); // Blocks until count reaches 0
System.out.println("All tasks completed!"); // Prints after all 3 tasks done

// CyclicBarrier - synchronize threads at barrier
CyclicBarrier barrier = new CyclicBarrier(3, () -> {
    System.out.println("All threads reached barrier!"); // Barrier action
});

// Threads wait at barrier
for (int i = 0; i < 3; i++) {
    int threadId = i;
    new Thread(() -> {
        try {
            System.out.println("Thread " + threadId + " working");
            Thread.sleep(1000);
            System.out.println("Thread " + threadId + " waiting at barrier");
            barrier.await(); // Wait for all threads
            System.out.println("Thread " + threadId + " continues after barrier");
        } catch (Exception e) {}
    }).start();
}

// Barrier can be reused (cyclic)
// After all threads pass, barrier resets for next use

// Semaphore - control access to limited resources
Semaphore semaphore = new Semaphore(2); // Allow 2 concurrent accesses

Runnable task = () -> {
    try {
        semaphore.acquire(); // Acquire permit (blocks if none available)
        System.out.println(Thread.currentThread().getName() + " acquired permit");
        Thread.sleep(2000); // Simulate work
        System.out.println(Thread.currentThread().getName() + " releasing permit");
        semaphore.release(); // Release permit
    } catch (InterruptedException e) {}
};

// 5 threads compete for 2 permits
for (int i = 0; i < 5; i++) {
    new Thread(task, "Thread-" + i).start();
}
```

**Key Takeaway**: `CountDownLatch` is one-time use for waiting on multiple events. `CyclicBarrier` is reusable for synchronizing threads at a common point. `Semaphore` controls access to limited resources with permits. Choose based on coordination pattern.

**Why It Matters**: CountDownLatch and CyclicBarrier enable thread coordination patterns. CountDownLatch waits for N events before proceeding—useful for phased initialization (start processing after all services ready) and parallel testing (wait for async operations). CyclicBarrier waits for N threads to reach a barrier then resets—ideal for iterative parallel algorithms. Understanding one-time (latch) vs reusable (barrier) determines appropriate choice. These primitives prevent busy-waiting and race conditions in concurrent coordination. They're common in production systems requiring synchronization across multiple threads without complex locking logic.

---

## Example 64: Fork/Join Framework

Fork/Join framework enables efficient parallel processing of recursive tasks. It uses work-stealing queues where idle threads steal work from busy threads. Powers parallel streams under the hood.

**Code**:

```java
import java.util.concurrent.*;

// RecursiveTask - returns a result
class SumTask extends RecursiveTask<Long> {
    private final long[] array;
    private final int start, end;
    private static final int THRESHOLD = 1000;

    public SumTask(long[] array, int start, int end) {
        this.array = array;
        this.start = start;
        this.end = end;
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
for (int i = 0; i < numbers.length; i++) {
    numbers[i] = i + 1;
}

ForkJoinPool pool = new ForkJoinPool(); // Default: Runtime.availableProcessors() threads
SumTask task = new SumTask(numbers, 0, numbers.length);
long result = pool.invoke(task); // => 50005000 (sum of 1 to 10000)

// RecursiveAction - no result (void)
class PrintTask extends RecursiveAction {
    private final int start, end;
    private static final int THRESHOLD = 10;

    public PrintTask(int start, int end) {
        this.start = start;
        this.end = end;
    }

    @Override
    protected void compute() {
        if (end - start <= THRESHOLD) {
            for (int i = start; i < end; i++) {
                System.out.println("Processing: " + i);
            }
        } else {
            int mid = start + (end - start) / 2;
            PrintTask left = new PrintTask(start, mid);
            PrintTask right = new PrintTask(mid, end);
            invokeAll(left, right); // Fork both and wait for completion
        }
    }
}

PrintTask printTask = new PrintTask(0, 100);
pool.invoke(printTask); // Prints 0 to 99 in parallel

// Parallel streams use ForkJoinPool.commonPool()
long parallelSum = java.util.stream.LongStream.range(1, 10001)
    .parallel() // Uses ForkJoinPool internally
    .sum(); // => 50005000
```

**Key Takeaway**: Fork/Join splits recursive tasks for parallel execution. `RecursiveTask<V>` returns results, `RecursiveAction` doesn't. Work-stealing balances load across threads. Use for divide-and-conquer algorithms. Parallel streams leverage this framework.

**Why It Matters**: Fork/Join framework parallelizes divide-and-conquer algorithms—merge sort, quicksort, tree processing. Work-stealing load balancing ensures even CPU utilization—idle threads steal tasks from busy threads. RecursiveTask/RecursiveAction hide thread pool complexity, enabling focus on problem decomposition. However, overhead limits effectiveness to large tasks—small tasks underperform sequential code. Understanding task granularity (split until work exceeds threshold) optimizes performance. Fork/Join powers parallel streams and is essential for CPU-bound parallel algorithms on multi-core hardware, enabling scalability without manual thread management.

---

## Example 65: Annotations and Reflection

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

## Example 66: Enums with Behavior

Enums are type-safe constants that can have fields, methods, and constant-specific behavior. They're more powerful than simple integer constants and integrate seamlessly with switch statements.

**Code**:

```java
// Basic enum
enum Day {
    MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, SATURDAY, SUNDAY
}

Day today = Day.MONDAY;
System.out.println(today); // => MONDAY
System.out.println(today.ordinal()); // => 0 (position in enum)

// Enum with fields and methods
enum Planet {
    MERCURY(3.303e23, 2.4397e6),
    VENUS(4.869e24, 6.0518e6),
    EARTH(5.976e24, 6.37814e6),
    MARS(6.421e23, 3.3972e6);

    private final double mass; // kg
    private final double radius; // meters

    // Constructor (always private)
    Planet(double mass, double radius) {
        this.mass = mass;
        this.radius = radius;
    }

    public double surfaceGravity() {
        final double G = 6.67300E-11;
        return G * mass / (radius * radius);
    }

    public double surfaceWeight(double otherMass) {
        return otherMass * surfaceGravity();
    }
}

double earthWeight = 75.0; // kg
double marsWeight = Planet.MARS.surfaceWeight(earthWeight); // => ~28.4 kg

// Enum with abstract methods (constant-specific behavior)
enum Operation {
    PLUS {
        @Override
        public double apply(double x, double y) { return x + y; }
    },
    MINUS {
        @Override
        public double apply(double x, double y) { return x - y; }
    },
    TIMES {
        @Override
        public double apply(double x, double y) { return x * y; }
    },
    DIVIDE {
        @Override
        public double apply(double x, double y) { return x / y; }
    };

    public abstract double apply(double x, double y);
}

double result = Operation.PLUS.apply(5, 3); // => 8.0
double product = Operation.TIMES.apply(4, 7); // => 28.0

// Enum methods
Day[] days = Day.values(); // => [MONDAY, TUESDAY, ...]
Day day = Day.valueOf("FRIDAY"); // => FRIDAY

// Switch with enum (exhaustive in modern Java)
String typeOfDay = switch (today) {
    case MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY -> "Weekday";
    case SATURDAY, SUNDAY -> "Weekend";
};

// EnumSet - efficient set implementation for enums
import java.util.*;
EnumSet<Day> weekend = EnumSet.of(Day.SATURDAY, Day.SUNDAY);
EnumSet<Day> weekdays = EnumSet.range(Day.MONDAY, Day.FRIDAY);

// EnumMap - efficient map with enum keys
EnumMap<Day, String> schedule = new EnumMap<>(Day.class);
schedule.put(Day.MONDAY, "Team meeting");
schedule.put(Day.WEDNESDAY, "Code review");
```

**Key Takeaway**: Enums are type-safe constants with fields, methods, and constructors. Constant-specific behavior via abstract methods enables polymorphism. `values()` and `valueOf()` provide iteration and lookup. `EnumSet` and `EnumMap` offer efficient enum-based collections.

**Why It Matters**: Enums provide type-safe constants with behavior, eliminating magic strings/numbers. Methods on enums encapsulate state-specific logic (Status.ACTIVE.canTransitionTo(INACTIVE)), making code self-documenting. Exhaustive switch statements with enums enable compiler-checked completeness. EnumSet and EnumMap are highly optimized for enum keys. Enums model finite state machines, configuration options, strategy patterns. Understanding enum construction (singletons, instance-specific behavior) enables sophisticated designs. Enums prevent stringly-typed code that's fragile and hard to refactor, making them essential for modeling closed sets of values in type-safe, maintainable systems.

---

## Example 67: Sealed Classes and Pattern Matching

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
    // Only Circle, Rectangle, Triangle can extend Shape
}

// Permitted subclasses must be final, sealed, or non-sealed
final class Circle extends Shape {
    private final double radius;
    public Circle(double radius) { this.radius = radius; }
    public double area() { return Math.PI * radius * radius; }
}

final class Rectangle extends Shape {
    private final double width, height;
    public Rectangle(double width, double height) {
        this.width = width;
        this.height = height;
    }
    public double area() { return width * height; }
}

non-sealed class Triangle extends Shape {
    // non-sealed: allows further subclassing
    private final double base, height;
    public Triangle(double base, double height) {
        this.base = base;
        this.height = height;
    }
    public double area() { return 0.5 * base * height; }
}

// Pattern matching for instanceof (Java 16+)
Object obj = "Hello";
if (obj instanceof String s) {
    // s is automatically cast to String in this scope
    System.out.println(s.toUpperCase()); // => "HELLO"
}

// Pattern matching with sealed classes
Shape shape = new Circle(5.0);

// Switch pattern matching (Java 17+, enhanced in Java 21)
double area = switch (shape) {
    case Circle c -> Math.PI * c.radius * c.radius;
    case Rectangle r -> r.width * r.height;
    case Triangle t -> 0.5 * t.base * t.height;
    // Exhaustive: compiler knows all possible subtypes
};

// Pattern matching with guards (Java 21+)
String description = switch (shape) {
    case Circle c when c.radius > 10 -> "Large circle";
    case Circle c -> "Small circle";
    case Rectangle r when r.width == r.height -> "Square";
    case Rectangle r -> "Rectangle";
    case Triangle t -> "Triangle";
};

// Record patterns (Java 19+, finalized in Java 21)
record Point(int x, int y) {}

Object point = new Point(10, 20);
if (point instanceof Point(int x, int y)) {
    System.out.println("x: " + x + ", y: " + y); // => x: 10, y: 20
}

// Nested record patterns
record ColoredPoint(Point point, String color) {}

Object cp = new ColoredPoint(new Point(5, 10), "red");
if (cp instanceof ColoredPoint(Point(int x, int y), String color)) {
    System.out.println("Point at (" + x + ", " + y + ") is " + color);
    // => Point at (5, 10) is red
}

// Sealed interfaces
sealed interface Result permits Success, Failure {}
record Success(String data) implements Result {}
record Failure(String error) implements Result {}

Result result = new Success("Data loaded");
String message = switch (result) {
    case Success(String data) -> "Success: " + data;
    case Failure(String error) -> "Error: " + error;
};
```

**Key Takeaway**: Sealed classes restrict inheritance with `sealed` and `permits`. Subclasses must be `final`, `sealed`, or `non-sealed`. Pattern matching for `instanceof` eliminates casts. Switch pattern matching enables exhaustive type checking. Record patterns destructure records in pattern matching.

**Why It Matters**: Sealed classes enable exhaustive hierarchies—all subtypes known at compile time, enabling complete case analysis. Combined with pattern matching (Java 17+), they provide type-safe destructuring without casts. Sealed classes complement enums (which lack per-variant data) and interfaces (which allow unlimited implementations). They're ideal for modeling closed domain concepts—Result<T, Error>, AST nodes, state machines—where all variants are known and fixed. Sealed classes prevent external extension, ensuring exhaustive handling and enabling better compiler errors, refactoring support, and reasoning about code correctness.

---

## Example 68: Modules (Java Platform Module System)

Modules provide stronger encapsulation than packages, enabling better dependency management and smaller runtime images. Defined via `module-info.java`, modules explicitly declare dependencies and exports.

**Code**:

```java
// module-info.java in com.example.myapp module
module com.example.myapp {
    // Require other modules
    requires java.base; // Implicit, always available
    requires java.sql; // Explicit dependency on SQL module
    requires transitive java.logging; // Transitive: consumers get logging too

    // Export packages (make them accessible to other modules)
    exports com.example.myapp.api; // Public API
    exports com.example.myapp.internal to com.example.test; // Qualified export

    // Open packages for reflection (for frameworks like Spring, Hibernate)
    opens com.example.myapp.model; // Deep reflection allowed
    opens com.example.myapp.entity to org.hibernate.orm; // Qualified open

    // Provide service implementation
    provides com.example.myapp.api.Service
        with com.example.myapp.impl.ServiceImpl;

    // Use service
    uses com.example.myapp.api.Service;
}

// Without modules (pre-Java 9), all public classes are globally accessible
// With modules, only exported packages are accessible

// Checking module from code
Module module = String.class.getModule();
System.out.println(module.getName()); // => "java.base"
System.out.println(module.isNamed()); // => true

// Unnamed module (classpath code)
// Code on classpath runs in unnamed module, can access all modules

// Module layers and layers
ModuleLayer bootLayer = ModuleLayer.boot();
Set<Module> modules = bootLayer.modules();
modules.forEach(m -> System.out.println(m.getName()));

// Creating custom runtime images with jlink
// jlink --module-path $JAVA_HOME/jmods:mods --add-modules com.example.myapp --output customjre
// Creates minimal JRE with only required modules

// Module visibility example
// In com.example.myapp.api package (exported):
package com.example.myapp.api;
public class PublicService {
    // Accessible to other modules
}

// In com.example.myapp.internal package (not exported):
package com.example.myapp.internal;
public class InternalUtil {
    // NOT accessible to other modules, even though public
    // Stronger encapsulation than package-private
}

// Module benefits:
// 1. Reliable configuration: missing dependencies detected at startup
// 2. Strong encapsulation: internal packages truly internal
// 3. Scalable: module graph prevents accidental dependencies
// 4. Smaller deployments: jlink creates custom runtime images
```

**Key Takeaway**: Modules provide stronger encapsulation via `module-info.java`. `requires` declares dependencies, `exports` makes packages accessible. `opens` allows deep reflection. `transitive` propagates dependencies. Modules enable reliable configuration and smaller runtime images with jlink.

**Why It Matters**: Modules (JPMS) provide explicit dependencies (requires/exports), strong encapsulation (internal packages hidden), and improved security (reduced attack surface). They enable faster startup (optimized module graph), smaller deployments (jlink custom runtimes), and compile-time dependency checking. Understanding unnamed modules, automatic modules, and split packages enables gradual migration from classpath. Modules are essential for large codebases and library development—enforcing clean boundaries and preventing internal API usage. However, migration complexity and ecosystem maturity issues make modules optional for many applications. Use them for explicit dependency management and API protection.

---

## Example 69: var and Type Inference

`var` enables local variable type inference, reducing boilerplate while preserving static typing. The compiler infers types from initializers. Use for readability when types are obvious, avoid when clarity suffers.

**Code**:

```java
// var for local variables (Java 10+)
var message = "Hello"; // Inferred as String
var count = 42; // Inferred as int
var price = 19.99; // Inferred as double

// Works with generics (reduces verbosity)
var list = new ArrayList<String>(); // Inferred as ArrayList<String>
var map = new HashMap<String, Integer>(); // HashMap<String, Integer>

// Diamond operator with var
var names = new ArrayList<>(); // Inferred as ArrayList<Object> (be careful!)
var scores = List.of(95, 87, 92); // Inferred as List<Integer>

// var in loops
var numbers = List.of(1, 2, 3, 4, 5);
for (var num : numbers) { // num inferred as Integer
    System.out.println(num);
}

for (var i = 0; i < 10; i++) { // i inferred as int
    System.out.println(i);
}

// var with streams
var stream = numbers.stream()
    .filter(n -> n > 2)
    .map(n -> n * 2); // Inferred as Stream<Integer>

// When var improves readability
var userRepository = new UserRepositoryImpl(); // Type obvious from right side
var configuration = ConfigurationLoader.load("config.json");

// When var reduces readability (avoid these)
var data = process(); // What type is data? Unclear!
var x = calculate(y); // What is x? Need to check calculate() return type

// var limitations
// Cannot use without initializer
// var x; // ERROR: cannot infer type

// Cannot use with null
// var name = null; // ERROR: cannot infer from null

// Cannot use for fields
class Example {
    // var field = "value"; // ERROR: var only for local variables
}

// Cannot use for method parameters
// void method(var param) {} // ERROR

// Cannot use for method return types
// var getValue() { return 42; } // ERROR

// var with method references
var comparator = Comparator.comparing(String::length); // Comparator<String>

// var doesn't change semantics, only reduces verbosity
var text = "Hello"; // Still statically typed as String
// text = 42; // ERROR: incompatible types

// Best practices
// ✅ Use var when type is obvious from right-hand side
var users = userService.getAllUsers();

// ✅ Use var for complex generic types
var result = new HashMap<String, List<Map<String, Object>>>();

// ❌ Avoid var when type isn't clear
var value = compute(); // What type? Check method signature

// ❌ Avoid var for primitives when literal type unclear
var flag = false; // Is it boolean or Boolean? Obvious here, but...
var number = 1; // int or Integer or long? Better: int number = 1;
```

**Key Takeaway**: `var` infers local variable types from initializers, reducing boilerplate while preserving static typing. Use when types are obvious from context. Limited to local variables—not fields, parameters, or return types. Doesn't change semantics, only syntax.

**Why It Matters**: var (Java 10+) reduces boilerplate through local variable type inference—eliminating redundant type declarations while maintaining compile-time type safety. It improves readability for obvious types (var list = new ArrayList<String>()) but can harm clarity when type isn't evident. Understanding where var works (local variables with initializers) vs where it doesn't (fields, parameters, method returns) prevents misuse. var doesn't make Java dynamically typed—it's purely compile-time sugar. Use it to reduce verbosity without sacrificing type safety, improving code maintainability by focusing on logic rather than repetitive type declarations.

---

## Example 70: Garbage Collection Basics

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

## Example 71: Memory Management and Reference Types

Java provides four reference types to control GC behavior. Strong references prevent collection. Soft references enable memory-sensitive caches. Weak references allow collection despite references. Phantom references enable pre-mortem cleanup.

**Code**:

```java
import java.lang.ref.*;
import java.util.*;

// Strong reference (default) - prevents GC
String strong = new String("Cannot be collected while referenced");
strong = null; // Now eligible for GC

// Soft reference - memory-sensitive caching
class ImageCache {
    private Map<String, SoftReference<byte[]>> cache = new HashMap<>();

    public void addImage(String key, byte[] image) {
        cache.put(key, new SoftReference<>(image));
    }

    public byte[] getImage(String key) {
        SoftReference<byte[]> ref = cache.get(key);
        if (ref != null) {
            byte[] image = ref.get();
            if (image != null) {
                return image; // Cache hit
            } else {
                cache.remove(key); // Was collected, remove entry
            }
        }
        return null; // Cache miss
    }
}

// Soft references cleared only when heap is nearly full
// Ideal for caches that can be regenerated

// Weak reference - collected at next GC
WeakReference<String> weak = new WeakReference<>(new String("Collected soon"));
String value = weak.get();
if (value != null) {
    System.out.println(value);
} else {
    System.out.println("Already collected");
}

// WeakHashMap - auto-remove entries when keys collected
WeakHashMap<Object, String> weakMap = new WeakHashMap<>();
Object key = new Object();
weakMap.put(key, "value");
System.out.println(weakMap.size()); // => 1

key = null; // Key becomes weakly reachable
System.gc(); // Suggest GC
Thread.sleep(100); // Give GC time
System.out.println(weakMap.size()); // => 0 (entry auto-removed)

// Phantom reference - for cleanup notification
class ResourceWithCleanup {
    private static ReferenceQueue<ResourceWithCleanup> queue =
        new ReferenceQueue<>();
    private static Set<PhantomReference<ResourceWithCleanup>> refs =
        new HashSet<>();

    private String resourceId;

    ResourceWithCleanup(String id) {
        this.resourceId = id;
        refs.add(new PhantomReference<>(this, queue));
    }

    static void cleanupThread() {
        new Thread(() -> {
            while (true) {
                try {
                    Reference<?> ref = queue.remove(); // Blocks until available
                    // Object has been finalized, perform cleanup
                    System.out.println("Cleanup triggered");
                    refs.remove(ref);
                } catch (InterruptedException e) {}
            }
        }).start();
    }
}

// Reference comparison
Object obj = new Object();

// Strong: obj -> Object (prevents GC)
SoftReference<Object> soft = new SoftReference<>(obj);
// Soft: obj -> SoftRef -> Object (GC if memory pressure)

WeakReference<Object> weak2 = new WeakReference<>(obj);
// Weak: obj -> WeakRef -> Object (GC at next cycle)

PhantomReference<Object> phantom = new PhantomReference<>(obj, new ReferenceQueue<>());
// Phantom: obj -> PhantomRef -> Object (get() always null, for cleanup)

// Reachability hierarchy (from strongest to weakest)
// 1. Strongly reachable: via strong reference chain from GC root
// 2. Softly reachable: only via soft references
// 3. Weakly reachable: only via weak references
// 4. Phantom reachable: finalized, only phantom refs remain
// 5. Unreachable: no references, ready for collection

// Practical: metadata cache with weak keys
class MetadataCache {
    private WeakHashMap<Class<?>, String> metadata = new WeakHashMap<>();

    public void register(Class<?> clazz, String meta) {
        metadata.put(clazz, meta);
    }

    public String get(Class<?> clazz) {
        return metadata.get(clazz);
    }

    // When Class<?> objects are unloaded (classloader gone),
    // entries auto-removed from map
}
```

**Key Takeaway**: Four reference types control GC: strong (default, prevents GC), soft (memory-sensitive caching), weak (GC-friendly caching), phantom (cleanup hooks). `WeakHashMap` auto-removes entries when keys collected. Soft references cleared under memory pressure, weak at next GC.

**Why It Matters**: Understanding reference types (strong, weak, soft, phantom) enables advanced memory management—caches that don't prevent GC, resource cleanup, memory-sensitive collections. Weak references enable caches that release entries under memory pressure. Soft references provide memory-sensitive caching—retain while memory available. Phantom references enable post-GC cleanup for native resources. Reference queues enable notification when objects are collected. These tools power memory-efficient caching, resource management, and preventing memory leaks. However, they're complex—use existing libraries (Guava caches) when possible. Understanding references enables building memory-efficient systems handling large datasets.

---

## Example 72: Performance Monitoring and Profiling

Java provides rich tools for monitoring and profiling applications. JMX exposes runtime metrics. JFR enables low-overhead production profiling. JMH provides accurate microbenchmarks. Profile before optimizing.

**Code**:

```java
import java.lang.management.*;
import javax.management.*;

// JMX - Java Management Extensions
ManagementFactory.getRuntimeMXBean().getName(); // => "12345@hostname" (PID@host)

// Memory monitoring
MemoryMXBean memoryBean = ManagementFactory.getMemoryMXBean();
MemoryUsage heapUsage = memoryBean.getHeapMemoryUsage();
System.out.println("Heap used: " + heapUsage.getUsed() / 1024 / 1024 + " MB");
System.out.println("Heap max: " + heapUsage.getMax() / 1024 / 1024 + " MB");

// Thread monitoring
ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
System.out.println("Live threads: " + threadBean.getThreadCount());
System.out.println("Peak threads: " + threadBean.getPeakThreadCount());

// Detect deadlocks
long[] deadlockedThreads = threadBean.findDeadlockedThreads();
if (deadlockedThreads != null) {
    System.out.println("Deadlock detected!");
}

// GC monitoring
List<GarbageCollectorMXBean> gcBeans = ManagementFactory.getGarbageCollectorMXBeans();
for (GarbageCollectorMXBean gcBean : gcBeans) {
    System.out.println("GC: " + gcBean.getName());
    System.out.println("  Collections: " + gcBean.getCollectionCount());
    System.out.println("  Time (ms): " + gcBean.getCollectionTime());
}

// CPU time for current thread
long cpuTime = threadBean.getCurrentThreadCpuTime();
long userTime = threadBean.getCurrentThreadUserTime();
System.out.println("CPU time (ns): " + cpuTime);

// Class loading monitoring
ClassLoadingMXBean classBean = ManagementFactory.getClassLoadingMXBean();
System.out.println("Loaded classes: " + classBean.getLoadedClassCount());
System.out.println("Total loaded: " + classBean.getTotalLoadedClassCount());

// Compilation monitoring (JIT)
CompilationMXBean compBean = ManagementFactory.getCompilationMXBean();
System.out.println("JIT compilation time (ms): " + compBean.getTotalCompilationTime());

// Simple performance measurement
long start = System.nanoTime();
// Code to measure
for (int i = 0; i < 1000000; i++) {
    Math.sqrt(i);
}
long end = System.nanoTime();
System.out.println("Elapsed (ms): " + (end - start) / 1_000_000);

// JMH (Java Microbenchmark Harness) - external library
// More accurate than System.nanoTime() due to warmup, JIT, etc.
/*
@Benchmark
public void benchmarkMethod() {
    // Code to benchmark
    doWork();
}
*/

// JFR (Java Flight Recorder) - low-overhead profiling
// Start: java -XX:StartFlightRecording=duration=60s,filename=recording.jfr MyApp
// Analyze with JDK Mission Control

// Common profiling tools (external)
// - jconsole: GUI for JMX monitoring
// - jvisualvm: Profiling, heap dumps, thread dumps
// - JDK Mission Control: JFR analysis
// - Async-profiler: Low-overhead CPU/memory profiling
// - YourKit, JProfiler: Commercial profilers

// Heap dump on OutOfMemoryError
// -XX:+HeapDumpOnOutOfMemoryError -XX:HeapDumpPath=/path/to/dump

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

## Example 73: Common Performance Patterns

Choosing appropriate data structures and algorithms dramatically impacts performance. StringBuilder for string building. ArrayList for indexed access. HashMap for lookups. Lazy initialization for expensive objects. Profile before optimizing.

**Code**:

```java
import java.util.*;

// String concatenation - avoid in loops
String bad = "";
for (int i = 0; i < 1000; i++) {
    bad += i; // Creates new String each iteration (O(n²) complexity)
}

// Use StringBuilder instead
StringBuilder good = new StringBuilder();
for (int i = 0; i < 1000; i++) {
    good.append(i); // O(n) complexity
}
String result = good.toString();

// ArrayList vs LinkedList
List<Integer> arrayList = new ArrayList<>();
List<Integer> linkedList = new LinkedList<>();

// ArrayList: O(1) random access, O(n) insertion in middle
arrayList.get(50); // Fast
arrayList.add(0, 100); // Slow (shifts elements)

// LinkedList: O(n) random access, O(1) insertion if iterator
linkedList.get(50); // Slow (traverses nodes)
Iterator<Integer> it = linkedList.iterator();
it.next();
((LinkedList<Integer>) linkedList).add(1, 100); // Still O(n) without iterator position

// Prefer ArrayList unless frequent insertions/deletions in middle

// HashMap resizing - set initial capacity if size known
Map<String, Integer> inefficient = new HashMap<>(); // Default capacity 16, resizes at 75% load
for (int i = 0; i < 1000; i++) {
    inefficient.put("key" + i, i); // Multiple resizes
}

Map<String, Integer> efficient = new HashMap<>(1000 * 4 / 3); // Avoid resizes
for (int i = 0; i < 1000; i++) {
    efficient.put("key" + i, i); // No resizes
}

// Stream vs for-loop performance
List<Integer> numbers = new ArrayList<>();
for (int i = 0; i < 1000000; i++) {
    numbers.add(i);
}

// Stream: more readable, potential parallelization, overhead
long streamSum = numbers.stream()
    .filter(n -> n % 2 == 0)
    .mapToLong(n -> n)
    .sum();

// For-loop: faster for simple operations, less overhead
long loopSum = 0;
for (int n : numbers) {
    if (n % 2 == 0) {
        loopSum += n;
    }
}

// Parallel stream for CPU-intensive work
long parallelSum = numbers.parallelStream()
    .filter(n -> n % 2 == 0)
    .mapToLong(n -> n)
    .sum(); // Faster for large datasets, CPU-bound tasks

// Lazy initialization - delay expensive object creation
class ExpensiveObject {
    private static ExpensiveObject instance;

    public static ExpensiveObject getInstance() {
        if (instance == null) {
            instance = new ExpensiveObject(); // Created only when needed
        }
        return instance;
    }
}

// Thread-safe lazy initialization (double-checked locking)
class ThreadSafeLazy {
    private static volatile ThreadSafeLazy instance;

    public static ThreadSafeLazy getInstance() {
        if (instance == null) { // First check (no locking)
            synchronized (ThreadSafeLazy.class) {
                if (instance == null) { // Second check (with lock)
                    instance = new ThreadSafeLazy();
                }
            }
        }
        return instance;
    }
}

// Object pooling (rarely beneficial in modern JVMs)
// Modern GC is fast; pooling adds complexity
// Only consider for:
// - JDBC connections
// - Thread pools
// - Expensive external resources

// Avoiding unnecessary object creation
// Bad
Integer badCount = new Integer(42); // Creates object

// Good
Integer goodCount = 42; // Uses Integer.valueOf() cache (-128 to 127)

// EnumSet/EnumMap - efficient for enums
enum Color { RED, GREEN, BLUE }
Set<Color> colors = EnumSet.of(Color.RED, Color.BLUE); // Bit vector internally

// Effective caching with appropriate eviction
Map<String, String> cache = new LinkedHashMap<>(100, 0.75f, true) {
    @Override
    protected boolean removeEldestEntry(Map.Entry<String, String> eldest) {
        return size() > 100; // LRU cache with max 100 entries
    }
};
```

**Key Takeaway**: Use `StringBuilder` for string concatenation in loops. Prefer `ArrayList` over `LinkedList` for most cases. Set HashMap initial capacity to avoid resizes. Streams for readability, loops for raw performance. Lazy initialization delays expensive object creation. Profile before optimizing.

**Why It Matters**: Common performance patterns (object pooling, lazy initialization, caching, batching) optimize hot paths in production code. Understanding when to apply each pattern prevents both premature optimization and performance bugs. Object pooling reduces allocation in high-throughput paths (connection pools, buffer pools). Lazy initialization defers expensive work until needed. Caching trades memory for speed—memoizing expensive computations. Batching reduces per-operation overhead (batch database inserts, network requests). However, patterns have costs—pooling adds complexity, caching adds memory pressure, batching adds latency. Measure before optimizing, and understand trade-offs.

---

## Example 74: Connection Pool Factory Pattern

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
    void connect() throws SQLException;
    void disconnect();
    ResultSet executeQuery(String sql) throws SQLException;
    boolean isConnected();
    String getConnectionString();
}

// PostgreSQL connection implementation
class PostgresConnection implements DatabaseConnection {
    private Connection connection;
    private final String connString;

    public PostgresConnection(String host, int port, String database) {
        this.connString = String.format("jdbc:postgresql://%s:%d/%s",
                                       host, port, database);
    }

    @Override
    public void connect() throws SQLException {
        connection = DriverManager.getConnection(connString);
        System.out.println("Connected to PostgreSQL: " + connString);
    }

    @Override
    public void disconnect() {
        try {
            if (connection != null) connection.close();
            System.out.println("Disconnected from PostgreSQL");
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public ResultSet executeQuery(String sql) throws SQLException {
        return connection.createStatement().executeQuery(sql);
    }

    @Override
    public boolean isConnected() {
        try {
            return connection != null && !connection.isClosed();
        } catch (SQLException e) {
            return false;
        }
    }

    @Override
    public String getConnectionString() { return connString; }
}

// MySQL connection implementation
class MySQLConnection implements DatabaseConnection {
    private Connection connection;
    private final String connString;

    public MySQLConnection(String host, int port, String database) {
        this.connString = String.format("jdbc:mysql://%s:%d/%s",
                                       host, port, database);
    }

    @Override
    public void connect() throws SQLException {
        connection = DriverManager.getConnection(connString);
        System.out.println("Connected to MySQL: " + connString);
    }

    @Override
    public void disconnect() {
        try {
            if (connection != null) connection.close();
            System.out.println("Disconnected from MySQL");
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public ResultSet executeQuery(String sql) throws SQLException {
        return connection.createStatement().executeQuery(sql);
    }

    @Override
    public boolean isConnected() {
        try {
            return connection != null && !connection.isClosed();
        } catch (SQLException e) {
            return false;
        }
    }

    @Override
    public String getConnectionString() { return connString; }
}

// Factory for creating database connections
class ConnectionFactory {
    public static DatabaseConnection create(String dbType, String host,
                                           int port, String database) {
        return switch (dbType.toLowerCase()) {
            case "postgres", "postgresql" ->
                new PostgresConnection(host, port, database);
            case "mysql" ->
                new MySQLConnection(host, port, database);
            default ->
                throw new IllegalArgumentException("Unsupported database: " + dbType);
        };
    }
}

// Connection configuration using Builder pattern
class ConnectionConfig {
    private final String dbType;
    private final String host;
    private final int port;
    private final String database;
    private final int poolSize;
    private final int timeout;

    private ConnectionConfig(Builder builder) {
        this.dbType = builder.dbType;
        this.host = builder.host;
        this.port = builder.port;
        this.database = builder.database;
        this.poolSize = builder.poolSize;
        this.timeout = builder.timeout;
    }

    // Fluent Builder
    public static class Builder {
        private final String dbType;
        private final String host;
        private final String database;
        private int port = 5432; // Default for Postgres
        private int poolSize = 10;
        private int timeout = 30;

        public Builder(String dbType, String host, String database) {
            this.dbType = dbType;
            this.host = host;
            this.database = database;
        }

        public Builder port(int port) {
            this.port = port;
            return this; // => Fluent API
        }

        public Builder poolSize(int size) {
            this.poolSize = size;
            return this;
        }

        public Builder timeout(int seconds) {
            this.timeout = seconds;
            return this;
        }

        public ConnectionConfig build() {
            return new ConnectionConfig(this);
        }
    }

    public String getDbType() { return dbType; }
    public String getHost() { return host; }
    public int getPort() { return port; }
    public String getDatabase() { return database; }
    public int getPoolSize() { return poolSize; }
    public int getTimeout() { return timeout; }
}

// Connection Pool using Singleton pattern
class ConnectionPool {
    private static volatile ConnectionPool instance; // Singleton instance
    private final Queue<DatabaseConnection> availableConnections;
    private final Set<DatabaseConnection> inUseConnections;
    private final ConnectionConfig config;

    private ConnectionPool(ConnectionConfig config) {
        this.config = config;
        this.availableConnections = new ConcurrentLinkedQueue<>();
        this.inUseConnections = ConcurrentHashMap.newKeySet();

        // Pre-create pool connections
        for (int i = 0; i < config.getPoolSize(); i++) {
            DatabaseConnection conn = ConnectionFactory.create(
                config.getDbType(),
                config.getHost(),
                config.getPort(),
                config.getDatabase()
            );
            availableConnections.offer(conn);
        }
        System.out.println("Connection pool initialized with " +
                         config.getPoolSize() + " connections");
    }

    // Thread-safe Singleton with double-checked locking
    public static ConnectionPool getInstance(ConnectionConfig config) {
        if (instance == null) {
            synchronized (ConnectionPool.class) {
                if (instance == null) {
                    instance = new ConnectionPool(config);
                }
            }
        }
        return instance;
    }

    public DatabaseConnection acquire() throws InterruptedException {
        DatabaseConnection conn = availableConnections.poll();
        if (conn == null) {
            System.out.println("Pool exhausted, creating new connection");
            conn = ConnectionFactory.create(
                config.getDbType(),
                config.getHost(),
                config.getPort(),
                config.getDatabase()
            );
        }
        inUseConnections.add(conn);
        return conn; // => Connection from pool or newly created
    }

    public void release(DatabaseConnection conn) {
        if (inUseConnections.remove(conn)) {
            availableConnections.offer(conn);
            System.out.println("Connection returned to pool");
        }
    }

    public void shutdown() {
        for (DatabaseConnection conn : availableConnections) {
            conn.disconnect();
        }
        for (DatabaseConnection conn : inUseConnections) {
            conn.disconnect();
        }
        System.out.println("Connection pool shut down");
    }

    public int getAvailableCount() { return availableConnections.size(); }
    public int getInUseCount() { return inUseConnections.size(); }
}

// Usage example combining all patterns
ConnectionConfig config = new ConnectionConfig.Builder("postgres", "localhost", "mydb")
    .port(5432)
    .poolSize(20)
    .timeout(60)
    .build(); // => Builder pattern

ConnectionPool pool = ConnectionPool.getInstance(config); // => Singleton

// Acquire connection from pool
DatabaseConnection conn = pool.acquire(); // => Factory creates connection
try {
    conn.connect();
    ResultSet rs = conn.executeQuery("SELECT * FROM users");
    // Process results...
} catch (SQLException e) {
    e.printStackTrace();
} finally {
    pool.release(conn); // => Return to pool for reuse
}

// Pool statistics
System.out.println("Available: " + pool.getAvailableCount());
System.out.println("In use: " + pool.getInUseCount());

pool.shutdown(); // Cleanup
```

**Key Takeaway**: This demonstrates all three creational patterns in production context. **Singleton** ensures one connection pool instance (thread-safe with double-checked locking). **Factory** creates database connections by type (Postgres, MySQL) enabling polymorphism. **Builder** constructs complex configuration with fluent API, avoiding telescoping constructors. Connection pooling is essential for production databases—creating connections is expensive (100-1000ms), reusing them is fast (1ms). This pattern appears in all production database libraries (HikariCP, C3P0, Apache DBCP).

**Why It Matters**: Connection pooling reuses database connections, eliminating expensive connection setup (TCP handshake, authentication, initialization). Pools prevent exhaustion by limiting max connections, and improve performance by maintaining warm connections. Understanding pool sizing (max connections, min idle, connection timeout) optimizes performance vs resource usage. HikariCP is the standard high-performance pool. Connection leaks (not returning connections) cause pool exhaustion and application failure—use try-with-resources. Pooling is essential for scalable database-backed applications—unpooled connections can't handle production load. Similar patterns apply to thread pools, object pools, and other expensive resources.

---

## Example 75: Strategy, Observer, Decorator

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

## Example 76: Dependency Injection Basics

Dependency Injection (DI) inverts control, allowing dependencies to be provided externally rather than created internally. Enhances testability, flexibility, and maintainability. Constructor injection preferred for required dependencies.

**Code**:

```java
// Without DI - tight coupling
class OrderServiceBad {
    private EmailService emailService = new EmailService(); // Hardcoded dependency

    public void placeOrder(Order order) {
        // Process order
        emailService.send("Order placed"); // Cannot mock for testing
    }
}

// With DI - loose coupling via interface
interface NotificationService {
    void send(String message);
}

class EmailService implements NotificationService {
    @Override
    public void send(String message) {
        System.out.println("Email: " + message);
    }
}

class SMSService implements NotificationService {
    @Override
    public void send(String message) {
        System.out.println("SMS: " + message);
    }
}

// Constructor injection (preferred for required dependencies)
class OrderService {
    private final NotificationService notificationService;

    public OrderService(NotificationService notificationService) {
        this.notificationService = notificationService; // Injected
    }

    public void placeOrder(Order order) {
        // Process order
        notificationService.send("Order placed");
    }
}

// Using the service
NotificationService emailService = new EmailService();
OrderService orderService = new OrderService(emailService);
orderService.placeOrder(new Order());

// Easily switch implementation
NotificationService smsService = new SMSService();
OrderService orderService2 = new OrderService(smsService);

// Setter injection (for optional dependencies)
class ReportService {
    private Logger logger;

    public void setLogger(Logger logger) {
        this.logger = logger; // Optional dependency
    }

    public void generateReport() {
        if (logger != null) {
            logger.log("Report generated");
        }
    }
}

// Benefits of DI
// 1. Testability - inject mocks/stubs
class MockNotificationService implements NotificationService {
    public List<String> sentMessages = new ArrayList<>();

    @Override
    public void send(String message) {
        sentMessages.add(message); // Track for assertions
    }
}

MockNotificationService mock = new MockNotificationService();
OrderService testService = new OrderService(mock);
testService.placeOrder(new Order());
assert mock.sentMessages.contains("Order placed");

// 2. Flexibility - change behavior without modifying code
// 3. Maintainability - dependencies explicit in constructor

// Manual DI container (simple example)
class DIContainer {
    private Map<Class<?>, Object> services = new HashMap<>();

    public <T> void register(Class<T> type, T instance) {
        services.put(type, instance);
    }

    @SuppressWarnings("unchecked")
    public <T> T resolve(Class<T> type) {
        return (T) services.get(type);
    }
}

DIContainer container = new DIContainer();
container.register(NotificationService.class, new EmailService());
NotificationService service = container.resolve(NotificationService.class);

// Spring Framework DI (conceptual)
/*
@Component
class OrderService {
    private final NotificationService notificationService;

    @Autowired // Spring injects dependency
    public OrderService(NotificationService notificationService) {
        this.notificationService = notificationService;
    }
}

@Component
class EmailService implements NotificationService {
    // Spring creates instance
}
*/

// DI principles
// - Depend on abstractions (interfaces), not concretions
// - Constructor injection for required dependencies
// - Setter injection for optional dependencies
// - Avoid service locator pattern (anti-pattern)
```

**Key Takeaway**: DI inverts control—dependencies injected, not created internally. Constructor injection for required dependencies, setter for optional. Enhances testability via mock injection. Depend on interfaces for flexibility. DI containers automate wiring in frameworks.

**Why It Matters**: Dependency injection decouples code from concrete dependencies, improving testability and flexibility. Constructor injection (preferred) makes dependencies explicit and enables immutability. Field injection (convenient) hides dependencies but prevents immutability. Method injection enables optional dependencies. Understanding DI enables effective framework usage (Spring, Guice, Dagger). DI inverts control—framework instantiates objects, not application code. This enables swapping implementations (mock for test, real for production), reduces coupling, and improves modularity. DI is foundational to modern Java development, enabling loosely coupled, testable, maintainable systems.

---

## Example 77: Immutability Patterns

Immutable objects cannot be modified after creation, providing inherent thread safety and simplicity. Use final fields, no setters, defensive copying for mutable components. Records automate immutable class creation.

**Code**:

```java
// Immutable class - traditional approach
final class ImmutablePoint {
    private final int x;
    private final int y;

    public ImmutablePoint(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() { return x; }
    public int getY() { return y; }

    // No setters - state cannot change

    // Operations return new instances
    public ImmutablePoint move(int dx, int dy) {
        return new ImmutablePoint(x + dx, y + dy);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ImmutablePoint other)) return false;
        return x == other.x && y == other.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
}

ImmutablePoint p1 = new ImmutablePoint(10, 20);
ImmutablePoint p2 = p1.move(5, 5); // => new ImmutablePoint(15, 25)
// p1 unchanged: (10, 20)

// Immutable class with mutable component - defensive copying
final class ImmutablePerson {
    private final String name;
    private final Date birthDate; // Date is mutable!

    public ImmutablePerson(String name, Date birthDate) {
        this.name = name;
        this.birthDate = new Date(birthDate.getTime()); // Defensive copy
    }

    public String getName() { return name; }

    public Date getBirthDate() {
        return new Date(birthDate.getTime()); // Return copy, not original
    }
}

Date date = new Date();
ImmutablePerson person = new ImmutablePerson("Alice", date);
date.setTime(0); // Doesn't affect person.birthDate (defensive copy)

// Records - immutable data carriers (Java 14+)
record Point(int x, int y) {
    // Automatically generates:
    // - private final fields
    // - constructor
    // - getters (x(), y())
    // - equals(), hashCode(), toString()
}

Point p = new Point(10, 20);
System.out.println(p.x()); // => 10
// No setters - immutable by default

// Compact constructor for validation
record PositivePoint(int x, int y) {
    public PositivePoint {
        if (x < 0 || y < 0) {
            throw new IllegalArgumentException("Coordinates must be positive");
        }
    }
}

// Immutable collections (Java 9+)
List<String> immutableList = List.of("A", "B", "C");
// immutableList.add("D"); // UnsupportedOperationException

Set<Integer> immutableSet = Set.of(1, 2, 3);
Map<String, Integer> immutableMap = Map.of("one", 1, "two", 2);

// Copying to immutable (Java 10+)
List<String> mutable = new ArrayList<>(List.of("X", "Y"));
List<String> copy = List.copyOf(mutable); // Immutable copy

// Benefits of immutability
// 1. Thread safety - no synchronization needed
final class SharedCounter {
    private final int count;

    public SharedCounter(int count) { this.count = count; }

    public SharedCounter increment() {
        return new SharedCounter(count + 1);
    }

    public int getCount() { return count; }

    // Safe to share across threads without locks
}

// 2. Cacheability - hash code never changes
Map<ImmutablePoint, String> cache = new HashMap<>();
ImmutablePoint key = new ImmutablePoint(5, 10);
cache.put(key, "Value"); // Safe - key cannot mutate

// 3. Simplicity - no defensive copying needed
public void processPoint(ImmutablePoint point) {
    // No worry about caller modifying point
}

// 4. Failure atomicity - partially constructed objects impossible
// (all fields set in constructor before object accessible)

// Persistent data structures (conceptual)
// Immutable collections that share structure for efficiency
// Example: adding to immutable list creates new list sharing most nodes
```

**Key Takeaway**: Immutable objects use final fields, no setters, and defensive copying for mutable components. Records automate immutable class creation. Immutable collections via `List.of()`, `Set.of()`, `Map.of()`. Benefits: thread safety, simplicity, cacheability, failure atomicity.

**Why It Matters**: Immutability prevents bugs from unintended mutations—immutable objects are thread-safe, easier to reason about, and prevent data races. Immutable collections (List.of, Set.of, Map.of) prevent modification bugs. Records provide immutable data carriers. Defensive copying prevents mutations through references. Understanding when immutability helps (shared data, concurrent access, caching) vs when mutability is needed (performance-critical updates, large data structures) determines appropriate design. Immutability reduces cognitive load—no need to track mutations. It's essential for thread-safe code without synchronization and prevents entire categories of bugs.

---

## Example 78: SOLID Principles in Java

SOLID principles guide maintainable object-oriented design. Single Responsibility (one reason to change). Open/Closed (open for extension, closed for modification). Liskov Substitution (subtypes substitutable). Interface Segregation (many specific interfaces). Dependency Inversion (depend on abstractions).

**Code**:

```java
// Single Responsibility Principle (SRP)
// A class should have one reason to change

// Bad - multiple responsibilities
class UserServiceBad {
    public void createUser(User user) {
        // Validate user
        // Save to database
        // Send email
        // Log activity
        // Multiple reasons to change!
    }
}

// Good - single responsibility per class
class UserValidator {
    public boolean validate(User user) {
        // Only validation logic
        return user.getName() != null && user.getEmail() != null;
    }
}

class UserRepository {
    public void save(User user) {
        // Only database operations
    }
}

class EmailService {
    public void sendWelcomeEmail(User user) {
        // Only email sending
    }
}

class UserService {
    private final UserValidator validator;
    private final UserRepository repository;
    private final EmailService emailService;

    public void createUser(User user) {
        if (validator.validate(user)) {
            repository.save(user);
            emailService.sendWelcomeEmail(user);
        }
    }
}

// Open/Closed Principle (OCP)
// Open for extension, closed for modification

// Bad - modifying class for new behavior
class AreaCalculatorBad {
    public double calculate(Object shape) {
        if (shape instanceof Circle) {
            Circle c = (Circle) shape;
            return Math.PI * c.radius * c.radius;
        } else if (shape instanceof Rectangle) {
            Rectangle r = (Rectangle) shape;
            return r.width * r.height;
        }
        // Adding new shape requires modifying this method
        return 0;
    }
}

// Good - extend via polymorphism
interface Shape {
    double area();
}

class Circle implements Shape {
    double radius;
    public double area() { return Math.PI * radius * radius; }
}

class Rectangle implements Shape {
    double width, height;
    public double area() { return width * height; }
}

class Triangle implements Shape {
    double base, height;
    public double area() { return 0.5 * base * height; }
}

class AreaCalculator {
    public double calculate(Shape shape) {
        return shape.area(); // No modification needed for new shapes
    }
}

// Liskov Substitution Principle (LSP)
// Subtypes must be substitutable for base types

// Bad - violates LSP
class Bird {
    public void fly() { System.out.println("Flying"); }
}

class Penguin extends Bird {
    @Override
    public void fly() {
        throw new UnsupportedOperationException("Penguins can't fly");
        // Violates LSP: cannot substitute Penguin for Bird
    }
}

// Good - respects LSP
interface Flyable {
    void fly();
}

class Sparrow implements Flyable {
    public void fly() { System.out.println("Sparrow flying"); }
}

class Penguin2 {
    public void swim() { System.out.println("Penguin swimming"); }
    // Doesn't implement Flyable - no LSP violation
}

// Interface Segregation Principle (ISP)
// Many specific interfaces better than one general

// Bad - fat interface
interface WorkerBad {
    void work();
    void eat();
    void sleep();
}

class RobotBad implements WorkerBad {
    public void work() { /* work */ }
    public void eat() { /* robots don't eat - forced to implement */ }
    public void sleep() { /* robots don't sleep - forced to implement */ }
}

// Good - segregated interfaces
interface Workable {
    void work();
}

interface Eatable {
    void eat();
}

interface Sleepable {
    void sleep();
}

class Human implements Workable, Eatable, Sleepable {
    public void work() { /* work */ }
    public void eat() { /* eat */ }
    public void sleep() { /* sleep */ }
}

class Robot implements Workable {
    public void work() { /* work */ }
    // Only implements what it needs
}

// Dependency Inversion Principle (DIP)
// Depend on abstractions, not concretions

// Bad - depends on concrete class
class OrderProcessorBad {
    private MySQLDatabase database = new MySQLDatabase();

    public void process(Order order) {
        database.save(order); // Tightly coupled to MySQL
    }
}

// Good - depends on abstraction
interface Database {
    void save(Order order);
}

class MySQLDatabase implements Database {
    public void save(Order order) { /* MySQL implementation */ }
}

class PostgreSQLDatabase implements Database {
    public void save(Order order) { /* PostgreSQL implementation */ }
}

class OrderProcessor {
    private final Database database;

    public OrderProcessor(Database database) {
        this.database = database; // Depends on abstraction
    }

    public void process(Order order) {
        database.save(order); // Works with any Database implementation
    }
}
```

**Key Takeaway**: SOLID principles enable maintainable design. SRP: one reason to change per class. OCP: extend via polymorphism, not modification. LSP: subtypes substitutable without breaking behavior. ISP: many specific interfaces over one fat interface. DIP: depend on abstractions via interfaces.

**Why It Matters**: SOLID principles guide object-oriented design toward maintainable systems. Single Responsibility (one reason to change) prevents god classes. Open/Closed (open for extension, closed for modification) enables adding features without changing existing code. Liskov Substitution (subclasses replaceable for parents) prevents inheritance misuse. Interface Segregation (many small interfaces vs few large) prevents forcing clients to depend on unused methods. Dependency Inversion (depend on abstractions) enables loose coupling. Understanding SOLID enables writing code that's easier to extend, test, and maintain. However, over-application causes over-engineering—apply principles judiciously based on concrete needs.

---

## Example 79: Custom ClassLoaders

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
System.out.println("System: " + systemClassLoader);
// => sun.misc.Launcher$AppClassLoader

ClassLoader platformClassLoader = systemClassLoader.getParent();
System.out.println("Platform: " + platformClassLoader);
// => sun.misc.Launcher$ExtClassLoader (Java 8) or PlatformClassLoader (Java 9+)

ClassLoader bootstrapClassLoader = platformClassLoader.getParent();
System.out.println("Bootstrap: " + bootstrapClassLoader);
// => null (bootstrap loader is native)

// Delegation model: child delegates to parent before loading
// Bootstrap -> Platform -> System -> Custom

// Custom ClassLoader
class CustomClassLoader extends ClassLoader {
    private String classPath;

    public CustomClassLoader(String classPath) {
        this.classPath = classPath;
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        try {
            // Convert class name to file path
            String fileName = name.replace('.', '/') + ".class";
            Path path = Paths.get(classPath, fileName);
            byte[] classBytes = Files.readAllBytes(path);

            // Define class from bytes
            return defineClass(name, classBytes, 0, classBytes.length);
        } catch (IOException e) {
            throw new ClassNotFoundException("Could not load class: " + name, e);
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

## Example 80: Bytecode Manipulation with ASM/ByteBuddy

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

## Example 81: JNI and Native Code

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
// 2. Compile Java class
// 3. Generate C header with javah (Java 8) or javac -h (Java 9+)
// 4. Implement native method in C/C++
// 5. Compile native code to shared library (.so, .dll, .dylib)
// 6. Load library and call native method

// Step 1: Declare native method
class NativeExample {
    // Native method declaration
    public native int add(int a, int b);
    public native String getMessage();

    // Load native library
    static {
        System.loadLibrary("native_example"); // Loads libnative_example.so/.dll/.dylib
    }

    public static void main(String[] args) {
        NativeExample example = new NativeExample();
        int result = example.add(5, 3); // => 8 (computed in C)
        System.out.println("Result: " + result);

        String message = example.getMessage(); // => "Hello from C"
        System.out.println(message);
    }
}

// Step 2 & 3: Generate header
// javac -h . NativeExample.java
// Creates: NativeExample.h

/*
Step 4: Implement in C (NativeExample.c)

#include <jni.h>
#include "NativeExample.h"

JNIEXPORT jint JNICALL Java_NativeExample_add(JNIEnv *env, jobject obj, jint a, jint b) {
    return a + b;
}

JNIEXPORT jstring JNICALL Java_NativeExample_getMessage(JNIEnv *env, jobject obj) {
    return (*env)->NewStringUTF(env, "Hello from C");
}
*/

// Step 5: Compile native code
// gcc -shared -fPIC -I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux -o libnative_example.so NativeExample.c

// JNI type mappings
// Java      -> C
// boolean   -> jboolean
// byte      -> jbyte
// char      -> jchar
// short     -> jshort
// int       -> jint
// long      -> jlong
// float     -> jfloat
// double    -> jdouble
// Object    -> jobject
// String    -> jstring
// array     -> jarray

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
// 2. Hardware access (device drivers, system calls)
// 3. Performance-critical operations (though JIT often matches C)
// 4. Platform-specific features (Windows API, POSIX)

// Alternatives to JNI
// 1. Java Native Access (JNA) - easier than JNI, no C code needed
// 2. Panama Foreign Function API (Java 19+) - modern replacement
// 3. Pure Java implementations - often sufficient with modern JIT

// Performance considerations
// - JNI calls have overhead (crossing JVM boundary)
// - Use for computationally intensive operations, not trivial calls
// - Batch operations to minimize crossings

// Memory management
// - Java GC doesn't manage native memory
// - Must manually free native allocations
// - Use try-finally or try-with-resources pattern

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

## Example 82: MicroProfile and Cloud-Native Java

MicroProfile standardizes enterprise Java microservices. Specifications for REST, configuration, health checks, metrics, fault tolerance, and JWT authentication. Enables cloud-native Java with containers and Kubernetes.

**Code**:

```java
// MicroProfile specifications overview
// - JAX-RS: RESTful web services
// - Config: Externalized configuration
// - Health: Health check endpoints
// - Metrics: Application metrics
// - Fault Tolerance: Circuit breaker, retry, bulkhead, timeout
// - JWT Auth: JSON Web Token authentication
// - OpenAPI: API documentation
// - Rest Client: Type-safe REST clients

// JAX-RS - REST endpoints
import javax.ws.rs.*;
import javax.ws.rs.core.*;

@Path("/users")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
class UserResource {
    @GET
    public Response getAllUsers() {
        List<User> users = userService.findAll();
        return Response.ok(users).build();
    }

    @GET
    @Path("/{id}")
    public Response getUser(@PathParam("id") Long id) {
        User user = userService.find(id);
        return Response.ok(user).build();
    }

    @POST
    public Response createUser(User user) {
        userService.create(user);
        return Response.status(Response.Status.CREATED).entity(user).build();
    }

    @PUT
    @Path("/{id}")
    public Response updateUser(@PathParam("id") Long id, User user) {
        userService.update(id, user);
        return Response.ok(user).build();
    }

    @DELETE
    @Path("/{id}")
    public Response deleteUser(@PathParam("id") Long id) {
        userService.delete(id);
        return Response.noContent().build();
    }
}

// MicroProfile Config - externalized configuration
import org.eclipse.microprofile.config.inject.ConfigProperty;

@ApplicationScoped
class ConfigExample {
    @Inject
    @ConfigProperty(name = "database.url")
    String databaseUrl; // Injected from config source

    @Inject
    @ConfigProperty(name = "database.timeout", defaultValue = "30")
    int timeout;
}

// Config sources: system properties, environment variables, microprofile-config.properties

// Health checks
import org.eclipse.microprofile.health.*;

@Health
@ApplicationScoped
class DatabaseHealthCheck implements HealthCheck {
    @Override
    public HealthCheckResponse call() {
        boolean isHealthy = checkDatabaseConnection();
        return HealthCheckResponse.named("database")
            .state(isHealthy)
            .withData("connection", "active")
            .build();
    }
}

// Liveness vs Readiness
@Liveness // Pod should be restarted if fails
class LivenessCheck implements HealthCheck { /*...*/ }

@Readiness // Pod should not receive traffic if fails
class ReadinessCheck implements HealthCheck { /*...*/ }

// Metrics
import org.eclipse.microprofile.metrics.annotation.*;

@ApplicationScoped
class MetricsExample {
    @Counted(name = "user_requests", description = "Number of user requests")
    @Timed(name = "user_request_time", description = "Time to process user requests")
    public void processUser() {
        // Method execution counted and timed
    }

    @Gauge(name = "active_users", unit = MetricUnits.NONE)
    public long getActiveUsers() {
        return userService.countActive();
    }
}

// Fault Tolerance
import org.eclipse.microprofile.faultto tolerance.*;

@ApplicationScoped
class FaultToleranceExample {
    @Retry(maxRetries = 3, delay = 1000) // Retry up to 3 times, 1 second delay
    public String callExternalService() {
        return externalService.getData();
    }

    @CircuitBreaker(
        requestVolumeThreshold = 4,
        failureRatio = 0.5,
        delay = 5000
    )
    public String callUnreliableService() {
        // Circuit opens if 50% of 4 requests fail
        // Opens for 5 seconds before half-open state
        return unreliableService.getData();
    }

    @Timeout(1000) // Timeout after 1 second
    @Fallback(fallbackMethod = "fallbackMethod")
    public String callSlowService() {
        return slowService.getData();
    }

    public String fallbackMethod() {
        return "Fallback data";
    }

    @Bulkhead(value = 5, waitingTaskQueue = 10)
    public String limitedConcurrency() {
        // Max 5 concurrent executions, 10 waiting
        return service.getData();
    }
}

// JWT Authentication
import org.eclipse.microprofile.jwt.*;

@ApplicationScoped
class SecureResource {
    @Inject
    JsonWebToken jwt;

    @GET
    @Path("/secure")
    @RolesAllowed("user")
    public String secureEndpoint() {
        String username = jwt.getName();
        return "Hello, " + username;
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

## Example 83: Reactive Programming with Reactive Streams

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

// Publisher interface
interface Publisher<T> {
    void subscribe(Subscriber<? super T> subscriber);
}

// Subscriber interface
interface Subscriber<T> {
    void onSubscribe(Subscription subscription);
    void onNext(T item);
    void onError(Throwable throwable);
    void onComplete();
}

// Subscription interface
interface Subscription {
    void request(long n); // Request n items (backpressure)
    void cancel(); // Cancel subscription
}

// Simple Publisher implementation
class SimplePublisher implements Publisher<Integer> {
    @Override
    public void subscribe(Subscriber<? super Integer> subscriber) {
        subscriber.onSubscribe(new Subscription() {
            private boolean cancelled = false;

            @Override
            public void request(long n) {
                if (cancelled) return;
                for (int i = 0; i < n; i++) {
                    subscriber.onNext(i);
                }
                subscriber.onComplete();
            }

            @Override
            public void cancel() {
                cancelled = true;
            }
        });
    }
}

// Simple Subscriber implementation
class SimpleSubscriber implements Subscriber<Integer> {
    private Subscription subscription;

    @Override
    public void onSubscribe(Subscription subscription) {
        this.subscription = subscription;
        subscription.request(5); // Request 5 items (backpressure control)
    }

    @Override
    public void onNext(Integer item) {
        System.out.println("Received: " + item);
    }

    @Override
    public void onError(Throwable throwable) {
        System.err.println("Error: " + throwable.getMessage());
    }

    @Override
    public void onComplete() {
        System.out.println("Complete");
    }
}

SimplePublisher publisher = new SimplePublisher();
publisher.subscribe(new SimpleSubscriber());

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
for (String url : urls) {
    String data = httpClient.get(url); // Blocks
    results.add(data);
}

// Reactive (non-blocking)
/*
Flux.fromIterable(urls)
    .flatMap(url -> webClient.get(url)) // Non-blocking, concurrent
    .collectList()
    .subscribe(results -> {
        // All results ready
    });
*/

// Use cases for Reactive Streams
// 1. High-throughput systems (millions of requests/sec)
// 2. Non-blocking I/O (database, HTTP, message queues)
// 3. Event streaming (real-time data pipelines)
// 4. Backpressure handling (prevent overwhelming slow consumers)

// When NOT to use reactive
// 1. Simple CRUD applications (overkill, complexity not justified)
// 2. CPU-bound operations (blocking acceptable)
// 3. Team unfamiliar with reactive paradigm
```

**Key Takeaway**: Reactive Streams enable async data processing with backpressure. Publisher emits, Subscriber consumes, Subscription controls flow with `request(n)`. Project Reactor provides Flux (0-N) and Mono (0-1) with rich operators. Ideal for high-throughput, non-blocking I/O. Backpressure prevents overwhelming slow consumers.

**Why It Matters**: Concurrent collections provide building blocks for scalable systems—ConcurrentHashMap for caches, CopyOnWriteArrayList for event listeners, BlockingQueue for producer-consumer patterns. Understanding performance characteristics (ConcurrentHashMap: fast reads/writes, CopyOnWrite: fast reads/slow writes, BlockingQueue: coordination) enables choosing appropriately. Weakly consistent iterators prevent ConcurrentModificationException during traversal. These collections prevent manual synchronization bugs (deadlocks, races) while providing better performance than synchronized wrappers. They're foundational to concurrent system design.

---

## Example 84: Virtual Threads (Project Loom, Java 21+)

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
Thread platformThread = new Thread(() -> {
    System.out.println("Platform thread: " + Thread.currentThread());
});
platformThread.start();

// Virtual thread (lightweight, Java 21+)
Thread virtualThread = Thread.ofVirtual().start(() -> {
    System.out.println("Virtual thread: " + Thread.currentThread());
});

// Create virtual thread with name
Thread namedVirtual = Thread.ofVirtual()
    .name("my-virtual-thread")
    .start(() -> {
        System.out.println(Thread.currentThread().getName());
    });

// Executor for virtual threads
ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();
executor.submit(() -> {
    System.out.println("Task in virtual thread");
});
executor.close(); // Auto-closes when all tasks complete

// Massive concurrency with virtual threads
// Platform threads: 1000s max (each ~1MB stack)
// Virtual threads: millions possible (each ~1KB)

try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
    for (int i = 0; i < 1_000_000; i++) {
        int taskId = i;
        executor.submit(() -> {
            // 1 million concurrent tasks!
            System.out.println("Task " + taskId);
        });
    }
} // Auto-waits for completion

// Virtual threads excel at I/O-bound work
ExecutorService ioExecutor = Executors.newVirtualThreadPerTaskExecutor();
ioExecutor.submit(() -> {
    // Blocking I/O doesn't block platform thread
    String data = httpClient.get("https://api.example.com");
    // Virtual thread unmounts from carrier while waiting
    System.out.println(data);
});

// Structured concurrency (preview feature)
/*
try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
    Future<String> user = scope.fork(() -> fetchUser(userId));
    Future<String> orders = scope.fork(() -> fetchOrders(userId));

    scope.join(); // Wait for both
    scope.throwIfFailed(); // Propagate exceptions

    return new UserData(user.resultNow(), orders.resultNow());
}
*/

// Virtual thread characteristics
// 1. Cheap to create (millions possible)
// 2. Unmount from carrier thread when blocking
// 3. No thread pooling needed (create per-task)
// 4. Same Thread API, different implementation

// When virtual threads excel
// - High concurrency I/O (web servers, database queries)
// - Blocking I/O operations (network, file)
// - Simplifies async code (no callbacks)

// When virtual threads DON'T help
// - CPU-bound work (same cores available)
// - Synchronized blocks (pins carrier thread)
// - Native code (JNI pins carrier thread)

// Pinning (carrier thread blocked)
synchronized (lock) {
    // Blocks carrier thread (bad for virtual threads)
    Thread.sleep(1000);
}

// Solution: use ReentrantLock instead
ReentrantLock lock = new ReentrantLock();
lock.lock();
try {
    // Doesn't pin carrier thread
    Thread.sleep(1000);
} finally {
    lock.unlock();
}

// Thread.sleep() with virtual threads
Thread.sleep(1000); // Virtual thread unmounts, doesn't block carrier

// Comparing platform vs virtual threads
// Platform: 1-1 mapping to OS thread, expensive, limited
// Virtual: M-N mapping to carrier threads, cheap, millions

// Migration from platform to virtual threads
// Old
ExecutorService old = Executors.newFixedThreadPool(10);

// New
ExecutorService newEx = Executors.newVirtualThreadPerTaskExecutor();

// Code unchanged, just swap executor

// Virtual thread debugging
// jcmd <pid> Thread.dump_to_file -format=json <file>
// Virtual threads visible in thread dumps
```

**Key Takeaway**: Virtual threads are lightweight (millions possible) with M:N mapping to platform threads. Ideal for I/O-bound workloads—unmount from carrier when blocking. Create per-task with `Thread.ofVirtual()` or `Executors.newVirtualThreadPerTaskExecutor()`. Avoid synchronized blocks (pins carrier), use `ReentrantLock`. Simplifies concurrency without callbacks.

**Why It Matters**: Phaser enables dynamic participant coordination—threads can register/deregister during execution. It generalizes CountDownLatch (one-time) and CyclicBarrier (fixed parties) with multi-phase support. Phasers enable complex coordination patterns (parallel algorithms with phases, test orchestration). Understanding termination (when parties reach zero) prevents resource leaks. Phasers are powerful but complex—use simpler primitives when possible. They shine in scenarios requiring flexible, multi-phase coordination across dynamic sets of threads.

---

## Example 85: Modern Java Best Practices

Modern Java emphasizes immutability, composition, type safety, and simplicity. Records for data, sealed classes for domain modeling, pattern matching for cleaner code. Testing and modularity are essential. Streams and Optional improve expressiveness.

**Code**:

```java
// Prefer composition over inheritance
// Bad - inheritance for code reuse
class Stack extends ArrayList {
    // Inherits methods that break stack semantics (add at index, etc.)
}

// Good - composition
class Stack {
    private List items = new ArrayList<>();

    public void push(Object item) { items.add(item); }
    public Object pop() { return items.remove(items.size() - 1); }
    // Only expose stack operations
}

// Use streams judiciously (readability vs performance)
// Stream: readable, potential parallelization
List<String> names = users.stream()
    .filter(u -> u.isActive())
    .map(User::getName)
    .collect(Collectors.toList());

// For-loop: faster for simple operations
List<String> names2 = new ArrayList<>();
for (User u : users) {
    if (u.isActive()) {
        names2.add(u.getName());
    }
}

// Leverage records for data carriers
record Point(int x, int y) {}
record Person(String name, int age) {}

Point p = new Point(10, 20);
// Auto-generated: constructor, getters, equals, hashCode, toString

// Sealed classes for domain modeling
sealed interface Result permits Success, Failure {}
record Success(String data) implements Result {}
record Failure(String error) implements Result {}

// Pattern matching for cleaner code
String message = switch (result) {
    case Success(String data) -> "Success: " + data;
    case Failure(String error) -> "Error: " + error;
};

// Immutability by default
// Bad - mutable
class MutablePoint {
    public int x, y;
}

// Good - immutable
record ImmutablePoint(int x, int y) {}

// Explicit nullability with Optional
// Bad - null return
public User findUser(Long id) {
    return null; // Implicit null
}

// Good - Optional
public Optional<User> findUser(Long id) {
    return Optional.ofNullable(repository.find(id));
}

user.ifPresent(u -> System.out.println(u.getName()));

// Use var for obvious types
var users = userRepository.findAll(); // Type obvious from method name
var count = users.size(); // Type obvious

// Testing as first-class activity
@Test
void shouldCreateUser() {
    // Arrange
    User user = new User("Alice", "alice@example.com");

    // Act
    userService.create(user);

    // Assert
    User saved = userRepository.find(user.getId());
    assertEquals("Alice", saved.getName());
}

// Modern module organization
// module-info.java
module com.example.app {
    requires java.sql;
    exports com.example.app.api;
    opens com.example.app.entity to org.hibernate.orm;
}

// Prefer method references over lambdas
// Lambda
users.forEach(u -> System.out.println(u));

// Method reference (cleaner)
users.forEach(System.out::println);

// Use try-with-resources for AutoCloseable
try (var reader = new BufferedReader(new FileReader("file.txt"))) {
    return reader.readLine();
} // Auto-closed

// Avoid premature optimization
// 1. Write clear code first
// 2. Profile to find bottlenecks
// 3. Optimize where measurements prove necessary

// Document public APIs
/**
 * Calculates the sum of two integers.
 *
 * @param a the first integer
 * @param b the second integer
 * @return the sum of a and b
 */
public int add(int a, int b) {
    return a + b;
}

// Use enums for type-safe constants
enum Status { PENDING, APPROVED, REJECTED }

// Modern exception handling
try {
    riskyOperation();
} catch (IOException | SQLException e) {
    // Multi-catch
    logger.error("Operation failed", e);
}

// Text blocks for multi-line strings (Java 15+)
String json = """
    {
        "name": "Alice",
        "age": 30
    }
    """;

// Switch expressions (Java 14+)
int numLetters = switch (day) {
    case MONDAY, FRIDAY, SUNDAY -> 6;
    case TUESDAY -> 7;
    default -> throw new IllegalArgumentException();
};

// Helpful NullPointerExceptions (Java 14+)
// Shows which variable was null in chain
// user.getAddress().getStreet() => "Cannot invoke getStreet() because getAddress() returned null"

// Modern Java philosophy
// - Immutability reduces bugs
// - Composition over inheritance
// - Explicit over implicit (Optional, sealed classes)
// - Type safety (records, sealed classes, pattern matching)
// - Simplicity (avoid over-engineering)
// - Testing and observability built-in
```

**Key Takeaway**: Modern Java emphasizes immutability (records, final), composition over inheritance, type safety (sealed classes, pattern matching), and explicitness (Optional). Use streams for readability, records for data, sealed classes for modeling. Testing essential. Avoid premature optimization—profile first. Simplicity over complexity.

**Why It Matters**: Semaphores limit concurrent access—rate limiting, connection pooling, resource bounding. They prevent resource exhaustion from too many concurrent operations (database connections, API calls, threads). Understanding fairness (FIFO vs non-FIFO acquisition) impacts performance vs latency. Semaphores enable throttling—control parallelism without rejecting requests. However, they don't prevent deadlocks (acquire multiple permits in different orders). Use them for resource limiting, not mutual exclusion (use locks). Semaphores are essential for building resilient systems with controlled concurrency.

---
