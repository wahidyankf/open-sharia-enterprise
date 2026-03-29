---
title: "Anti Patterns"
date: 2025-12-12T00:00:00+07:00
draft: false
description: Common Java mistakes and pitfalls to avoid - recognize and prevent problematic patterns in your codebase
weight: 10000030
tags: ["java", "anti-patterns", "best-practices", "code-quality", "pitfalls"]
---

## Understanding Anti-Patterns

Anti-patterns represent common but ineffective solutions to recurring problems. Unlike design patterns which show best practices, anti-patterns demonstrate what to avoid. They often emerge from well-intentioned code that seemed reasonable initially but proved problematic in production.

**Why study anti-patterns:**

- **Prevention**: Recognize patterns before introducing them
- **Detection**: Identify existing problems in codebases
- **Communication**: Shared vocabulary for discussing code issues
- **Learning**: Understanding why patterns fail deepens knowledge

This guide catalogs Java anti-patterns organized by category, explains their problems, and demonstrates solutions.

## Concurrency Anti-Patterns

### Thread Leakage

**Problem**: Creating threads without proper lifecycle management causes threads to accumulate and never terminate, leading to resource exhaustion.

**Recognition signals:**

- Thread count continuously increases
- Memory usage grows unbounded
- Application performance degrades over time
- Thread dumps show numerous idle threads

**Why this fails:**

- Consumes system resources (memory, CPU, file descriptors)
- Causes OutOfMemoryError or thread creation failures
- Makes debugging unpredictable
- Performance degrades with application uptime

**Solution approach:**

| Problematic Pattern                        | Better Approach                                    |
| ------------------------------------------ | -------------------------------------------------- |
| Manual thread creation with `new Thread()` | Use ExecutorService with thread pools              |
| Threads without shutdown logic             | Always shutdown executors properly                 |
| No cleanup in finally blocks               | Use try-with-resources for AutoCloseable executors |
| Uncontrolled thread proliferation          | Configure thread pool sizing limits                |

**Example transformation:**

```java
// PROBLEMATIC: Unbounded thread creation
public class ReportGenerator {
// => ANTI-PATTERN: creates new thread for every request
    public void generateReport(ReportRequest request) {
// => Called for each report: creates unbounded threads
        new Thread(() -> {
// => Manual thread creation: no pool, no lifecycle management
// => Lambda runnable: executes report processing asynchronously
            // No lifecycle management - thread leaks
// => Thread never joined or tracked: leaks after completion
// => No shutdown mechanism: threads accumulate over time
            processReport(request);
// => Processes report: blocking work on unmanaged thread
        }).start();
// => Starts thread immediately: no queuing, no back-pressure
// => Memory leak: Thread objects and stacks never garbage collected
    }
}

// SOLUTION: Managed thread pool
public class ReportGenerator {
// => SOLUTION: uses thread pool for bounded concurrency
    private final ExecutorService executor = Executors.newFixedThreadPool(10);
// => Fixed thread pool: reuses 10 threads for all tasks
// => Bounded concurrency: maximum 10 reports processed simultaneously
// => Queues excess work: prevents resource exhaustion

    public void generateReport(ReportRequest request) {
// => Submits work to pool: doesn't create new thread
        executor.submit(() -> processReport(request));
// => submit(): schedules task, returns Future for result tracking
// => Thread reuse: same 10 threads handle all reports
// => Automatic queuing: excess tasks wait in queue
    }

    public void shutdown() {
// => Cleanup method: must be called during application shutdown
        executor.shutdown();
// => Graceful shutdown: stops accepting new tasks, finishes queued work
// => Does not interrupt: running tasks complete normally
        try {
            if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
// => awaitTermination: waits up to 60 seconds for tasks to complete
// => Returns false: if timeout expires before all tasks finish
                executor.shutdownNow();
// => Force shutdown: interrupts running tasks immediately
// => Returns List<Runnable>: tasks that never started
            }
        } catch (InterruptedException e) {
// => Interrupted while waiting: shutdown process itself interrupted
            executor.shutdownNow();
// => Force immediate shutdown: don't wait any longer
            Thread.currentThread().interrupt();
// => Restore interrupt status: preserves interruption for caller
        }
    }
}
```

### Busy Waiting

**Problem**: Using loops to repeatedly check conditions instead of proper synchronization, wasting CPU cycles.

**Recognition signals:**

- Loops with `Thread.sleep()` checking conditions
- High CPU usage with minimal actual work
- Tight loops checking volatile variables
- Spin locks in application code

**Why this fails:**

- Wastes CPU resources unnecessarily
- Reduces performance for other threads
- Increases power consumption
- Causes timing issues across different hardware

**Solution approach:**

| Problematic Pattern                     | Better Approach                                     |
| --------------------------------------- | --------------------------------------------------- |
| `while` loops with sleep checking flags | Use `wait()` and `notify()` for coordination        |
| Polling volatile variables              | Use `CountDownLatch`, `Semaphore`, or synchronizers |
| Manual condition checking               | Use `CompletableFuture` for async operations        |
| Spin waiting                            | Leverage reactive programming for events            |

### Nested Monitor Lockout (Deadlock)

**Problem**: Two or more threads hold locks and wait for each other to release them, causing permanent blocking.

**Recognition signals:**

- Application stops responding completely
- Thread dumps show threads in BLOCKED state
- Multiple threads waiting for locks held by others
- Circular wait conditions in lock acquisition

**Why this fails:**

- Application hangs completely
- Requires restart to recover
- Difficult to diagnose in production
- Occurs intermittently under specific timing

**Solution strategy:**

**Deadlock prevention principles:**

1. **Consistent lock ordering**: Always acquire locks in same order
2. **Lock timeouts**: Use `tryLock()` with timeout
3. **Minimize scope**: Hold locks for shortest time possible
4. **Higher-level utilities**: Prefer concurrent collections
5. **Lock-free algorithms**: Consider atomic operations

**Example comparison:**

```java
// PROBLEMATIC: Inconsistent lock ordering causes deadlock
public class AccountManager {
// => ANTI-PATTERN: multiple locks acquired in inconsistent order
    private final Object accountLock = new Object();
// => First lock: protects account operations
    private final Object balanceLock = new Object();
// => Second lock: protects balance operations
// => Two locks: creates potential for deadlock

    // Thread 1: accountLock → balanceLock
    public void transfer(Account from, Account to, BigDecimal amount) {
// => Transfer method: acquires locks in order A → B
        synchronized (accountLock) {
// => Acquires accountLock first: locks account operations
            synchronized (balanceLock) {
// => Then acquires balanceLock: nested lock acquisition
// => Lock order: accountLock THEN balanceLock
                // Transfer logic
            }
        }
    }

    // Thread 2: balanceLock → accountLock (DEADLOCK!)
    public void updateBalance(Account account, BigDecimal amount) {
// => Update method: acquires locks in order B → A (OPPOSITE!)
        synchronized (balanceLock) {
// => Acquires balanceLock first: DIFFERENT ORDER than transfer()
            synchronized (accountLock) {
// => Then acquires accountLock: creates circular wait condition
// => Lock order: balanceLock THEN accountLock (REVERSED!)
// => DEADLOCK SCENARIO: Thread 1 holds A waiting for B, Thread 2 holds B waiting for A
                // Update logic
            }
        }
    }
}

// SOLUTION: Single lock or consistent ordering
public class AccountManager {
// => SOLUTION: eliminates deadlock with single lock
    private final Object lock = new Object();
// => Single lock: no lock ordering issues possible
// => Simpler reasoning: all synchronized blocks use same lock

    public void transfer(Account from, Account to, BigDecimal amount) {
// => Transfer uses single lock: no deadlock possible
        synchronized (lock) {
// => Acquires single lock: all operations synchronized on same object
            // All operations use same lock
// => No nested locks: eliminates circular wait conditions
        }
    }

    public void updateBalance(Account account, BigDecimal amount) {
// => Update uses same lock: consistent with transfer()
        synchronized (lock) {
// => Same lock as transfer(): ensures mutual exclusion
            // Consistent ordering
// => Lock order irrelevant: only one lock exists
        }
    }
}
```

### Race Conditions

**Problem**: Multiple threads access shared mutable state without proper synchronization, causing unpredictable results.

**Recognition signals:**

- Intermittent test failures
- Results vary between runs
- Data corruption under load
- Lost updates to shared state

**Why this fails:**

- Non-atomic operations on shared state
- Visibility issues across CPU caches
- Compiler/processor reordering
- Check-then-act race conditions

**Solution patterns:**

| Problematic Pattern                 | Better Approach                              |
| ----------------------------------- | -------------------------------------------- |
| Unsynchronized shared mutable state | Use `synchronized` blocks or methods         |
| `volatile` for compound operations  | Use `AtomicReference`, `AtomicInteger`, etc. |
| Check-then-act without locking      | Use atomic compare-and-swap operations       |
| Manual synchronization              | Use thread-safe collections                  |

### Ignoring InterruptedException

**Problem**: Catching `InterruptedException` without properly handling it suppresses the interruption signal.

**Recognition signals:**

- Empty catch blocks for `InterruptedException`
- Threads don't terminate during shutdown
- Application hangs during shutdown
- Not restoring interrupt status

**Why this fails:**

- Breaks thread cancellation mechanisms
- Makes threads unresponsive to shutdown
- Causes resource leaks during shutdown
- Violates interruption protocol

**Solution approach:**

**Three valid strategies:**

1. **Propagate**: Declare `throws InterruptedException`
2. **Restore status**: Call `Thread.currentThread().interrupt()`
3. **Clean exit**: Restore status then exit gracefully

```java
// PROBLEMATIC: Swallowing interruption
public void processQueue() {
// => ANTI-PATTERN: ignores thread interruption, runs forever
    while (true) {
// => Infinite loop: no exit condition, ignores interruption
        try {
            Task task = queue.take();
// => BlockingQueue.take(): waits for available task, throws InterruptedException
// => Interruptible: responds to Thread.interrupt() by throwing exception
            process(task);
// => Processes task: business logic
        } catch (InterruptedException e) {
// => Catches interruption: thread signaled to stop
            // WRONG: Ignoring interruption signal
// => ANTI-PATTERN: swallows exception, clears interrupt status
// => Loop continues: thread never terminates despite interrupt request
// => Breaks shutdown: application can't stop this thread gracefully
        }
    }
}

// SOLUTION: Restore interrupt status
public void processQueue() {
// => SOLUTION: properly handles interruption, allows graceful shutdown
    try {
        while (!Thread.currentThread().isInterrupted()) {
// => Loop condition: checks interrupt flag before each iteration
// => isInterrupted(): returns true if thread interrupted, exits loop
            try {
                Task task = queue.take();
// => Blocking call: throws InterruptedException if interrupted
                process(task);
// => Processes task: business logic runs only if not interrupted
            } catch (InterruptedException e) {
// => Catches interruption: thread signaled to terminate
                Thread.currentThread().interrupt();
// => Restores interrupt status: makes flag true again (take() cleared it)
// => Propagates signal: allows outer loop condition to detect interruption
                break;
// => Exits inner try: transfers control to outer loop condition check
// => Loop will exit: isInterrupted() returns true after restore
            }
        }
    } finally {
// => Always executed: cleanup runs whether loop exits normally or via exception
        cleanup();
// => Resource cleanup: releases resources before thread termination
    }
}
```

## Resource Management Anti-Patterns

### Not Closing Resources

**Problem**: Failing to close resources like streams, connections, or files causes resource leaks.

**Recognition signals:**

- Resources opened but never closed
- Close statements in try blocks (skipped on exception)
- No finally blocks for cleanup
- Not using try-with-resources

**Why this fails:**

- Exhausts file descriptors or database connections
- Causes memory leaks
- Prevents file deletion or modification
- Leads to "too many open files" errors

**Solution approach:**

**Resource management hierarchy:**

1. **Modern (Java 7+)**: Use try-with-resources
2. **Multiple resources**: Nest or semicolon-separate
3. **Legacy code**: Use try-finally with null checks
4. **Connection pools**: Configure timeouts and limits

```java
// PROBLEMATIC: Resources not closed on exception
public List<Record> getRecords(int year) {
// => ANTI-PATTERN: leaks resources when exception thrown
    List<Record> records = new ArrayList<>();
// => Result accumulator: collects database records
    try {
        Connection conn = dataSource.getConnection();
// => Acquires connection: resource #1, not in try-with-resources
        PreparedStatement stmt = conn.prepareStatement(sql);
// => Creates statement: resource #2, depends on connection
        ResultSet rs = stmt.executeQuery();
// => Executes query: resource #3, depends on statement

        while (rs.next()) {
// => Iterates rows: may throw SQLException during mapping
            records.add(mapRecord(rs));
// => Maps row: if this throws, resources never closed
        }

        rs.close();  // Skipped if exception occurs!
// => Manual close: only runs if no exception above
// => RESOURCE LEAK: if mapRecord() throws, rs never closed
        stmt.close();
// => Statement close: skipped if earlier exception
        conn.close();
// => Connection close: skipped if earlier exception
// => CRITICAL LEAK: connection not returned to pool
    } catch (SQLException e) {
// => Catches exception: but resources already leaked
        throw new RuntimeException(e);
// => Wraps exception: doesn't fix resource leak
    }
    return records;
}

// SOLUTION: try-with-resources guarantees cleanup
public List<Record> getRecords(int year) {
// => SOLUTION: guarantees resource cleanup even on exception
    List<Record> records = new ArrayList<>();
    try (Connection conn = dataSource.getConnection();
// => Try-with-resources: Connection implements AutoCloseable
// => Automatic close: close() called when exiting try block (normal or exception)
         PreparedStatement stmt = conn.prepareStatement(sql);
// => Semicolon separation: multiple resources in one try-with-resources
// => Close order: stmt.close() called BEFORE conn.close() (reverse declaration order)
         ResultSet rs = stmt.executeQuery()) {
// => ResultSet in resources: guaranteed close even if while loop throws
// => Automatic cleanup: all three resources closed in correct order

        while (rs.next()) {
// => Iterates rows: safe to throw here, cleanup guaranteed
            records.add(mapRecord(rs));
// => Maps row: if throws, try-with-resources closes all resources
        }
    } catch (SQLException e) {
// => Catches exception: resources already closed by try-with-resources
        throw new RuntimeException(e);
// => Wraps exception: resources properly cleaned up before propagation
    }
    return records;
// => Returns results: all resources guaranteed closed
}
```

### Connection Pool Exhaustion

**Problem**: Not returning connections to pool or creating connections without limits.

**Recognition signals:**

- "Connection pool exhausted" errors
- Increasing connection count over time
- Timeout exceptions acquiring connections
- Connections in use but idle

**Why this fails:**

- Database rejects new connections
- Application performance degrades
- Cascading failures across services
- Difficult to recover without restart

**Solution strategy:**

| Problematic Pattern          | Better Approach                             |
| ---------------------------- | ------------------------------------------- |
| Manual connection management | Use connection pool (HikariCP, Apache DBCP) |
| No connection timeouts       | Configure acquisition timeout               |
| Unbounded pool size          | Set maximum pool size                       |
| No connection validation     | Enable connection validation                |

### File Handle Leaks

**Problem**: Opening files without ensuring they're closed in all code paths.

**Recognition signals:**

- "Too many open files" system errors
- File descriptor count increasing
- Cannot delete or rename files
- Files locked by process

**Why this fails:**

- Operating system limits file descriptors
- Prevents file system operations
- Causes application crashes
- Difficult to diagnose root cause

## Design Anti-Patterns

### God Objects

**Problem**: Creating classes that know too much or do too much, violating Single Responsibility Principle.

**Recognition signals:**

- Classes with thousands of lines
- Many unrelated methods
- Names like "Manager", "Handler", "Util", "Helper"
- Everything depends on this class
- Frequent merge conflicts

**Why this fails:**

- Hard to understand and maintain
- Difficult to test in isolation
- High coupling to many system parts
- Changes ripple through entire class
- Violates separation of concerns

**Solution strategy:**

**Decomposition approach:**

1. **Identify responsibilities**: List all distinct concerns
2. **Create focused services**: One service per responsibility
3. **Define clear interfaces**: Each service has well-defined contract
4. **Use orchestration**: Application service coordinates domain services
5. **Apply dependency injection**: Wire services together

**Example transformation:**

```java
// PROBLEMATIC: God class handling everything
public class OrderManager {
// => ANTI-PATTERN: violates Single Responsibility Principle
    // Handles customers, products, payments, notifications, audit, risk, etc.
// => Multiple responsibilities: customer management, order processing, payment, notifications
// => Thousands of lines, dozens of dependencies
// => High coupling: changes to any concern affect entire class
// => Hard to test: requires mocking all dependencies for any test

    public Customer createCustomer(CustomerData data) { /* ... */ }
// => Customer responsibility: not related to orders
    public void updateCustomer(Customer customer) { /* ... */ }
// => Customer management: separate concern
    public Order createOrder(OrderData data) { /* ... */ }
// => Order responsibility: core but mixed with unrelated concerns
    public void processPayment(Payment payment) { /* ... */ }
// => Payment processing: different responsibility from orders
    public void sendNotification(String customerId, String message) { /* ... */ }
// => Notification delivery: infrastructure concern, not business logic
    public void auditAction(String action) { /* ... */ }
// => Audit logging: cross-cutting concern
    // 50+ more methods...
// => Unmaintainable: too many reasons to change
// => Team conflicts: multiple developers editing same massive file
}

// SOLUTION: Focused services with single responsibility
public class CustomerService {
// => SOLUTION: single responsibility (customer management only)
// => Cohesive: all methods relate to customer lifecycle
    public Customer createCustomer(CustomerData data) { /* ... */ }
// => Customer creation: focused responsibility
    public void updateCustomer(Customer customer) { /* ... */ }
// => Customer updates: same domain, same service
}

public class OrderService {
// => Single responsibility: order processing only
// => Clear boundaries: doesn't handle customers, payments, or notifications
    public Order createOrder(OrderData data) { /* ... */ }
// => Order creation: core responsibility of this service
    public Order getOrder(String orderId) { /* ... */ }
// => Order retrieval: focused on order operations
}

public class PaymentService {
// => Payment responsibility: isolated payment processing
    public void processPayment(Payment payment) { /* ... */ }
// => Payment logic: encapsulated in dedicated service
}

// Orchestrator coordinates services
public class OrderApplicationService {
// => Application service: orchestrates domain services (no business logic)
    private final CustomerService customerService;
// => Injected dependency: customer operations
    private final OrderService orderService;
// => Injected dependency: order operations
    private final PaymentService paymentService;
// => Injected dependency: payment operations
// => Composition: combines services without violating SRP

    public OrderResult processNewOrder(OrderRequest request) {
// => Workflow orchestration: coordinates services for complete use case
        Customer customer = customerService.getCustomer(request.getCustomerId());
// => Delegates to CustomerService: validates customer exists
        Order order = orderService.createOrder(request.toOrderData());
// => Delegates to OrderService: creates order domain object
        paymentService.processPayment(request.toPayment());
// => Delegates to PaymentService: processes payment transaction
        return OrderResult.success(order);
// => Returns result: workflow completed successfully
// => Each service focused: customer/order/payment separated, orchestrator coordinates
    }
}
```

### Primitive Obsession

**Problem**: Using primitive types instead of small value objects to represent domain concepts.

**Recognition signals:**

- Primitive parameters in method signatures
- Validation logic scattered across codebase
- Same validation duplicated everywhere
- Comments explaining what primitives mean
- Easy to mix up parameter order

**Why this fails:**

- No type safety for domain concepts
- Validation duplicated everywhere
- Easy to pass wrong values
- No encapsulation of business rules
- Difficult to refactor

**Solution approach:**

**Value object pattern:**

- Create small classes for domain concepts
- Encapsulate validation in constructor
- Make immutable (final fields)
- Provide domain-specific operations
- Use descriptive names

**Example comparison:**

```java
// PROBLEMATIC: Primitives with scattered validation
public class Account {
// => ANTI-PATTERN: uses primitives for domain concepts
    public void transfer(String fromAccountNumber, String toAccountNumber,
                        double amount, String currency) {
// => All primitives: no type safety, easy to swap parameters
// => String for account: could be any string, no validation at type level
// => double for money: floating-point precision errors (0.1 + 0.2 != 0.3)
        // Validation scattered everywhere
        if (fromAccountNumber == null || fromAccountNumber.length() != 10) {
// => Duplicate validation: same logic repeated in every method using account numbers
            throw new IllegalArgumentException("Invalid account number");
        }
        if (amount <= 0) {
// => Amount validation: duplicated wherever amounts used
            throw new IllegalArgumentException("Amount must be positive");
        }
        // Easy to swap parameters!
        // transfer("USD", "ACC123", "ACC456", 100.0) - compiles but wrong
// => Type system can't help: all strings look the same to compiler
// => Runtime errors: parameter swap compiles successfully, fails at runtime
    }
}

// SOLUTION: Value objects with encapsulated validation
public class AccountNumber {
// => Value object: represents domain concept with type safety
    private final String value;
// => Encapsulated value: hides representation details

    public AccountNumber(String value) {
// => Validates on construction: fails fast with invalid input
        if (value == null || value.length() != 10) {
// => Single validation point: centralized business rule
            throw new IllegalArgumentException("Invalid account number");
// => Constructor guarantee: impossible to create invalid AccountNumber
        }
        this.value = value;
// => Immutable: final field assigned once
    }

    public String getValue() { return value; }
// => Accessor: exposes value when needed (e.g., persistence)
}

public class Money {
// => Value object: encapsulates amount + currency
    private final BigDecimal amount;
// => BigDecimal: precise decimal arithmetic (no floating-point errors)
    private final Currency currency;
// => Currency type: validates ISO codes, type-safe

    public Money(BigDecimal amount, Currency currency) {
// => Constructor validation: ensures invariants before object creation
        if (amount.compareTo(BigDecimal.ZERO) <= 0) {
// => Business rule: enforces positive amounts
            throw new IllegalArgumentException("Amount must be positive");
// => Fail-fast: rejects invalid state immediately
        }
        this.amount = amount;
        this.currency = currency;
// => Immutable: both fields final, object cannot change after construction
    }
}

public class Account {
// => SOLUTION: uses value objects for type safety
    public void transfer(AccountNumber from, AccountNumber to, Money amount) {
// => Type-safe parameters: compiler prevents parameter swaps
// => Pre-validated: all values guaranteed valid by constructors
        // Type-safe, validated, can't swap parameters
// => No validation needed: value objects guarantee invariants
// => Parameter order enforced: AccountNumber vs Money distinguishable by compiler
    }
}
```

### Shotgun Surgery

**Problem**: Making changes requires modifying many different classes in many different locations.

**Recognition signals:**

- Single feature change touches dozens of files
- Related code scattered across packages
- Difficult to find all affected code
- High risk of missing updates
- Frequent regression bugs

**Why this fails:**

- Increases change complexity
- High risk of inconsistent updates
- Difficult to understand feature boundaries
- Makes refactoring dangerous
- Slows development velocity

**Solution strategy:**

| Problematic Pattern            | Better Approach                         |
| ------------------------------ | --------------------------------------- |
| Scattered business logic       | Group related functionality             |
| Duplicated code across classes | Extract to single location              |
| Mixed concerns                 | Apply Single Responsibility             |
| Tight coupling                 | Use interfaces and dependency injection |

## Performance Anti-Patterns

### N+1 Query Problem

**Problem**: Executing one query to get list, then additional query for each item in the list.

**Recognition signals:**

- Hundreds of database queries for single operation
- Query count scales with result set size
- Slow page loads with database queries
- Linear performance degradation

**Why this fails:**

- Multiplies database round trips
- Overwhelms connection pool
- Network latency accumulates
- Unacceptable user experience

**Solution approach:**

**Query optimization strategies:**

1. **Join fetching**: Single query with joins
2. **Batch loading**: Fetch all related data in one query
3. **Projection queries**: Select only needed columns
4. **Caching**: Cache frequently accessed data

**Example transformation:**

```java
// PROBLEMATIC: N+1 queries
public List<OrderDTO> getOrders() {
// => ANTI-PATTERN: executes 1 + N database queries
    List<Order> orders = orderRepository.findAll(); // 1 query
// => First query: SELECT * FROM orders (fetches all orders)
// => Returns 100 orders: triggers 100 additional queries below

    return orders.stream()
// => Streams over orders: processes each order individually
        .map(order -> {
// => Maps each order: executes for every order in list
            Customer customer = customerRepository.findById(order.getCustomerId()); // N queries!
// => N+1 PROBLEM: one query per order (if 100 orders, 100 customer queries)
// => Each query: SELECT * FROM customers WHERE id = ?
// => Database round trips: 1 + 100 = 101 total queries
// => Linear scaling: doubles queries when orders double
            return new OrderDTO(order, customer);
// => Creates DTO: combines order and customer data
        })
        .collect(Collectors.toList());
// => Collects DTOs: returns list after all 101 queries complete
}

// SOLUTION: Join fetch or batch loading
public List<OrderDTO> getOrders() {
// => SOLUTION: uses single query with JOIN
    // Single query with join
    List<Order> orders = orderRepository.findAllWithCustomers();
// => Single query: SELECT o.*, c.* FROM orders o JOIN customers c ON o.customer_id = c.id
// => Eager loading: fetches orders and customers in one database round trip
// => Returns orders: each Order already contains associated Customer (no lazy loading)
// => Performance: constant time regardless of result size (1 query for 100 or 1000 orders)

    return orders.stream()
// => Streams preloaded orders: all customer data already available
        .map(order -> new OrderDTO(order, order.getCustomer()))
// => No queries executed: customer already loaded from join
// => order.getCustomer(): returns customer from join, doesn't query database
        .collect(Collectors.toList());
// => Collects results: creates DTO list without additional database calls
}
```

### Premature Optimization

**Problem**: Optimizing code before knowing where actual bottlenecks exist.

**Recognition signals:**

- Complex code with unclear benefit
- Micro-optimizations without measurements
- Trading readability for speculative performance
- Optimizing code that's not in hot path

**Why this fails:**

- Wastes development time
- Makes code harder to maintain
- Often doesn't improve actual performance
- Optimization should be based on profiling

**Solution approach:**

**Optimization workflow:**

1. **Make it work**: Correct, readable implementation first
2. **Make it right**: Clean, maintainable code
3. **Make it fast**: Profile, identify bottlenecks, optimize
4. **Measure impact**: Verify optimization actually helps

### Excessive Object Creation

**Problem**: Creating unnecessary objects in loops or hot code paths.

**Recognition signals:**

- Creating objects in tight loops
- New objects on every method call
- Excessive garbage collection
- Memory pressure under load

**Why this fails:**

- Increases garbage collection pressure
- Causes GC pauses affecting latency
- Wastes CPU on allocation and collection
- Can trigger OutOfMemoryError

**Solution strategy:**

| Problematic Pattern           | Better Approach                      |
| ----------------------------- | ------------------------------------ |
| Objects in loops              | Reuse or pool objects                |
| Defensive copying everywhere  | Use immutable objects where possible |
| String concatenation in loops | Use `StringBuilder`                  |
| Temporary collections         | Use streams or iterators             |

## Security Anti-Patterns

### Hardcoded Credentials

**Problem**: Storing passwords, API keys, or secrets directly in source code.

**Recognition signals:**

- Passwords in string literals
- API keys in configuration files
- Credentials in version control
- Connection strings with passwords

**Why this fails:**

- Secrets exposed in version control history
- Cannot rotate credentials easily
- Different environments need different secrets
- Security audit failures
- Compliance violations

**Solution approach:**

**Credential management:**

1. **Environment variables**: Store in environment
2. **Secret management**: Use vault systems (HashiCorp Vault, AWS Secrets Manager)
3. **Configuration externalization**: Separate config from code
4. **IAM roles**: Use cloud provider identity management

### SQL Injection Vulnerabilities

**Problem**: Concatenating user input directly into SQL queries.

**Recognition signals:**

- String concatenation for SQL queries
- User input in WHERE clauses
- Dynamic table or column names from input
- No parameterized queries

**Why this fails:**

- Attackers can execute arbitrary SQL
- Data breach risk
- Data deletion or corruption
- Authentication bypass

**Solution approach:**

**SQL injection prevention:**

1. **Parameterized queries**: Always use prepared statements
2. **ORM frameworks**: Use JPA, Hibernate with proper API
3. **Input validation**: Whitelist validation
4. **Least privilege**: Database user with minimal permissions

**Example comparison:**

```java
// PROBLEMATIC: SQL injection vulnerability
public User findUser(String username) {
    String sql = "SELECT * FROM users WHERE username = '" + username + "'";
    // Attacker can use: admin' OR '1'='1
    return jdbcTemplate.queryForObject(sql, new UserMapper());
}

// SOLUTION: Parameterized query
public User findUser(String username) {
    String sql = "SELECT * FROM users WHERE username = ?";
    return jdbcTemplate.queryForObject(sql, new UserMapper(), username);
}
```

### Insufficient Input Validation

**Problem**: Not validating or sanitizing user input at system boundaries.

**Recognition signals:**

- Accepting any input without validation
- Blacklist-based validation
- No length limits on input
- Trusting client-side validation

**Why this fails:**

- XSS attacks possible
- Buffer overflow vulnerabilities
- Invalid data in database
- Application crashes on unexpected input

**Solution strategy:**

**Input validation principles:**

- **Validate at boundaries**: All external input
- **Whitelist validation**: Define allowed values
- **Type safety**: Convert to domain objects early
- **Fail securely**: Reject invalid input
- **Sanitize output**: Context-specific escaping

## Financial Calculation Anti-Patterns

### Using Float/Double for Money

**Problem**: Using floating-point types (`float`, `double`) for monetary calculations.

**Recognition signals:**

- `double` or `float` for currency amounts
- Rounding errors in calculations
- Incorrect totals that don't balance
- Precision loss in arithmetic

**Why this fails:**

- Binary floating-point cannot represent decimal exactly
- Rounding errors accumulate
- Violates financial accuracy requirements
- Audit failures

**Solution approach:**

**Monetary calculation rules:**

1. **Use BigDecimal**: Always for money calculations
2. **Set scale explicitly**: Define decimal places
3. **Specify rounding mode**: Choose appropriate rounding
4. **Use Money value objects**: Encapsulate currency and amount

**Example transformation:**

```java
// PROBLEMATIC: Floating-point money calculations
public double calculateInterest(double principal, double rate, int days) {
    return principal * rate * days / 365.0; // Precision loss!
}

// SOLUTION: BigDecimal with explicit scale and rounding
public BigDecimal calculateInterest(BigDecimal principal, BigDecimal rate, int days) {
    BigDecimal daysDecimal = new BigDecimal(days);
    BigDecimal daysInYear = new BigDecimal(365);

    return principal
        .multiply(rate)
        .multiply(daysDecimal)
        .divide(daysInYear, 2, RoundingMode.HALF_UP);
}
```

### Incorrect Rounding

**Problem**: Not specifying rounding mode or using inappropriate rounding for financial calculations.

**Recognition signals:**

- Using default rounding behavior
- Inconsistent rounding across codebase
- Rounding too early in calculations
- Not matching business rounding rules

**Why this fails:**

- Inconsistent results
- Regulatory compliance issues
- Audit trail problems
- Customer trust issues

**Solution strategy:**

**Rounding best practices:**

- Specify rounding mode explicitly (`HALF_UP`, `HALF_EVEN`, etc.)
- Round only at final step
- Document rounding policy
- Use same rounding consistently
- Match business requirements

## Testing Anti-Patterns

### Non-Deterministic Tests

**Problem**: Tests that pass or fail inconsistently due to external factors.

**Recognition signals:**

- Tests fail intermittently
- Different results on different machines
- Time-dependent test behavior
- Race conditions in tests

**Why this fails:**

- Cannot trust test results
- Difficult to debug failures
- Slows development velocity
- Erodes confidence in test suite

**Solution approach:**

| Problematic Pattern                 | Better Approach                 |
| ----------------------------------- | ------------------------------- |
| Tests depend on external services   | Use test doubles (mocks, stubs) |
| Time-dependent assertions           | Use clock abstraction           |
| Shared mutable state between tests  | Isolated test state             |
| Concurrent test execution conflicts | Proper test isolation           |

### Testing Implementation Details

**Problem**: Tests coupled to internal implementation rather than public behavior.

**Recognition signals:**

- Tests break on refactoring
- Testing private methods
- Mocking everything
- Tests know too much about internals

**Why this fails:**

- Tests fragile to refactoring
- False sense of test coverage
- Difficult to change implementation
- Tests don't verify actual behavior

**Solution approach:**

**Test design principles:**

- Test public API behavior
- Focus on observable outcomes
- Use integration tests for interactions
- Keep unit tests focused
- Minimize mocking

## Recognition and Prevention

### Static Analysis Tools

**Automated detection:**

- **SpotBugs**: Detects common bugs and anti-patterns
- **PMD**: Code quality and design issues
- **SonarQube**: Comprehensive code analysis
- **Error Prone**: Compile-time error detection
- **Checkstyle**: Coding standard violations

### Code Review Checklist

**During code review, check for:**

- [ ] Proper resource management (try-with-resources)
- [ ] Thread safety for shared state
- [ ] Parameterized SQL queries
- [ ] BigDecimal for monetary calculations
- [ ] Proper exception handling
- [ ] No hardcoded credentials
- [ ] Single Responsibility Principle adherence
- [ ] Proper input validation
- [ ] Explicit type safety
- [ ] Test isolation and determinism

### Prevention Strategies

**Organizational practices:**

1. **Education**: Regular training on anti-patterns
2. **Coding standards**: Document and enforce standards
3. **Code review**: Peer review catches issues early
4. **Automated checks**: CI/CD pipeline validation
5. **Refactoring culture**: Regular improvement sprints
6. **Architecture review**: Periodic design assessment

## Summary

Anti-patterns represent common mistakes that emerge from well-intentioned but ultimately problematic solutions. Understanding anti-patterns helps you:

**Recognize issues early** - Spot patterns before they become ingrained

**Prevent introduction** - Avoid creating new problems

**Communicate effectively** - Shared vocabulary for code issues

**Refactor confidently** - Know what to change and why

**Build quality code** - Apply lessons learned from common mistakes

The key to avoiding anti-patterns is understanding the principles they violate and applying proper alternatives from the start. When you encounter anti-patterns in existing code, systematic refactoring guided by these principles leads to more maintainable, reliable systems.

## Related Resources

- [Overview](/en/learn/software-engineering/programming-languages/java/in-the-field/overview) - In-practice content introduction
- [By Example](/en/learn/software-engineering/programming-languages/java/by-example) - Code-first tutorials
- [Quick Start](/en/learn/software-engineering/programming-languages/java/quick-start) - Get started with Java
