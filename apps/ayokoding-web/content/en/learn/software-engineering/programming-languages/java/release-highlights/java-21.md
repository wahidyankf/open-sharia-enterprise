---
title: "Java 21 LTS (2023)"
date: 2026-02-03T00:00:00+07:00
draft: false
description: "Key highlights from Java 21 LTS release - virtual threads, sequenced collections, and pattern matching finalization"
weight: 1000008
tags: ["java", "java-21", "lts", "virtual-threads", "concurrency", "pattern-matching"]
---

## Release Overview

**Java 21** was released on **September 19, 2023**, as the second LTS release following the two-year cadence. This release delivers **15 JEPs** including revolutionary concurrency features, finalized pattern matching, and significant API improvements.

**Key Metrics:**

- **Release Date:** September 19, 2023
- **Support Duration:** 8+ years (until 2031+)
- **Previous LTS:** Java 17 (September 2021, 2-year gap)
- **Next LTS:** Java 25 (September 2025, 2-year gap)
- **JEPs Delivered:** 15 enhancements

## Major Language Features

### 1. Virtual Threads (Finalized) 🚀

**JEP 444:** Lightweight, JVM-managed threads enabling high-throughput concurrent applications with simple thread-per-request model.

**Revolutionary Impact:** This is the **most significant concurrency feature** since Java 8 streams.

**Key Characteristics:**

- Managed by JVM, not OS
- Extremely lightweight (millions possible)
- Same programming model as platform threads
- Excel at I/O-bound workloads
- Not faster for CPU-intensive tasks

**Quick Example:**

```java
// Simple virtual thread creation
Thread.startVirtualThread(() -> {
    System.out.println("Running in virtual thread");
});

// Recommended: ExecutorService
try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
    tasks.forEach(task ->
        executor.submit(() -> processTask(task)));
    // Handles millions of concurrent I/O operations
}
```

**Finance Example - Donation Processing:**

```java
// Before (Java 17): Platform threads with pool
public class DonationProcessorOld {
    private final ExecutorService executor =
        Executors.newFixedThreadPool(200);  // Limited!

    public CompletableFuture<Receipt> processDonation(Donation d) {
        return CompletableFuture.supplyAsync(() -> {
            validateDonor(d);          // Database - blocks thread
            checkFraudRules(d);        // API call - blocks thread
            recordTransaction(d);      // Database - blocks thread
            sendConfirmation(d);       // Email - blocks thread
            return generateReceipt(d);
        }, executor);
    }
    // Problem: 200 thread limit = max 200 concurrent donations
}

// After (Java 21): Virtual threads - unlimited concurrency
public class DonationProcessorNew {
    private final ExecutorService executor =
        Executors.newVirtualThreadPerTaskExecutor();

    public CompletableFuture<Receipt> processDonation(Donation d) {
        return CompletableFuture.supplyAsync(() -> {
            validateDonor(d);          // Blocks virtual thread only
            checkFraudRules(d);        // Carrier thread available
            recordTransaction(d);      // Extremely efficient
            sendConfirmation(d);       // No tuning needed
            return generateReceipt(d);
        }, executor);
    }
    // Benefit: 100,000+ concurrent donations with same resources
}
```

**Structured Concurrency Example:**

```java
public class ZakatCalculationService {
    public ZakatReport calculateAnnualZakat(String userId) {
        try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
            // Launch parallel subtasks
            Subtask<BigDecimal> cash = scope.fork(() ->
                fetchCashBalance(userId));

            Subtask<BigDecimal> gold = scope.fork(() ->
                fetchGoldHoldings(userId));

            Subtask<BigDecimal> investments = scope.fork(() ->
                fetchInvestments(userId));

            Subtask<BigDecimal> debts = scope.fork(() ->
                fetchDebts(userId));

            // Wait for all to complete
            scope.join();
            scope.throwIfFailed();

            // Calculate net zakatable assets
            BigDecimal totalAssets = cash.get()
                .add(gold.get())
                .add(investments.get())
                .subtract(debts.get());

            return new ZakatReport(userId, totalAssets);
        } catch (InterruptedException | ExecutionException e) {
            throw new ZakatCalculationException("Failed", e);
        }
    }
}
```

**Performance Gains:**

- **Throughput:** 10-15% increase for I/O-bound services
- **Concurrency:** 10-100x more concurrent requests
- **Latency:** Reduced tail latencies (p95, p99)

**When NOT to Use Virtual Threads:**

1. **CPU-Intensive Operations** - Use platform threads sized to CPU cores
2. **Synchronized Blocks** - Pinning issue, migrate to `ReentrantLock`
3. **Sub-Millisecond Tasks** - Overhead dominates actual work
4. **Native Method Calls** - Pinning issue

**Migration Checklist:**

- Profile application (I/O vs CPU workload)
- Replace `synchronized` with `ReentrantLock`
- Remove thread pool size tuning code
- Enable pinning detection: `-Djdk.tracePinnedThreads=full`
- Measure performance before/after

### 2. Record Patterns (Finalized)

**JEP 440:** Deconstruct record values with concise, type-safe pattern matching.

**Example:**

```java
public record Point(int x, int y) {}
public record Address(String street, String city, String postal) {}
public record Donor(String name, String email, Address address) {}

public class DonorService {

    // Old approach - verbose
    public void processOld(Object obj) {
        if (obj instanceof Point) {
            Point p = (Point) obj;
            int x = p.x();
            int y = p.y();
            System.out.println("Point at (" + x + ", " + y + ")");
        }
    }

    // Record pattern - concise
    public void process(Object obj) {
        if (obj instanceof Point(int x, int y)) {
            System.out.println("Point at (" + x + ", " + y + ")");
        }
    }

    // Nested record patterns
    public void validate(Object obj) {
        if (obj instanceof Donor(String name, String email,
                                 Address(String street, String city, String postal))) {
            System.out.println("Donor: " + name);
            System.out.println("City: " + city);
        }
    }

    // Pattern matching in switch
    public String getInfo(Object obj) {
        return switch (obj) {
            case Donor(String name, String email,
                      Address(var street, var city, var postal)) ->
                String.format("Donor %s from %s", name, city);
            case null -> "No information";
            default -> "Unknown type";
        };
    }
}
```

### 3. Pattern Matching for Switch (Finalized)

**JEP 441:** Pattern matching in `switch` graduated to finalized feature with guards, null handling, and exhaustiveness checking.

**Example:**

```java
public class TransactionProcessor {

    public String process(Object tx) {
        return switch (tx) {
            case null ->
                "Null transaction";

            case ZakatTransaction z when z.getAmount().compareTo(BigDecimal.ZERO) > 0 ->
                "Processing valid Zakat: " + z.getAmount();

            case ZakatTransaction z ->
                "Invalid Zakat amount: " + z.getAmount();

            case Donation d when d.getAmount().compareTo(new BigDecimal("1000")) > 0 ->
                "Large donation: " + d.getAmount() + " - requires approval";

            case Donation d ->
                "Regular donation: " + d.getAmount();

            default ->
                "Unknown transaction type";
        };
    }

    // Exhaustive switch with sealed types
    public BigDecimal calculateFee(PaymentMethod payment) {
        return switch (payment) {
            case CreditCard card ->
                card.getAmount().multiply(new BigDecimal("0.029"));  // 2.9%

            case BankTransfer transfer ->
                new BigDecimal("5.00");  // Flat $5

            case Cash cash ->
                BigDecimal.ZERO;  // No fee

            // No default needed - compiler ensures exhaustiveness
        };
    }
}

sealed interface PaymentMethod permits CreditCard, BankTransfer, Cash {
    BigDecimal getAmount();
}

final class CreditCard implements PaymentMethod {
    public BigDecimal getAmount() { return BigDecimal.ZERO; }
}

final class BankTransfer implements PaymentMethod {
    public BigDecimal getAmount() { return BigDecimal.ZERO; }
}

final class Cash implements PaymentMethod {
    public BigDecimal getAmount() { return BigDecimal.ZERO; }
}
```

### 4. Sequenced Collections

**JEP 431:** New interfaces for collections with defined encounter order.

**New Interfaces:**

- `SequencedCollection` - Collection with defined order
- `SequencedSet` - Set with defined order
- `SequencedMap` - Map with defined order

**Key Methods:**

- `addFirst(E)` / `addLast(E)` - Add at beginning/end
- `getFirst()` / `getLast()` - Retrieve first/last
- `removeFirst()` / `removeLast()` - Remove first/last
- `reversed()` - Get reversed view (no copying!)

**Example:**

```java
public class DonationQueue {

    // Works with ArrayList, LinkedList, Deque
    public void process(SequencedCollection<Donation> donations) {
        // Add urgent donation at front
        Donation urgent = new Donation("Emergency", new BigDecimal("10000"));
        donations.addFirst(urgent);

        // Add regular donation at back
        Donation regular = new Donation("General", new BigDecimal("500"));
        donations.addLast(regular);

        // Process first donation
        Donation next = donations.getFirst();
        System.out.println("Processing: " + next.purpose());

        // Reversed view (no copying!)
        SequencedCollection<Donation> reversed = donations.reversed();
        System.out.println("Last: " + reversed.getFirst().purpose());
    }

    // SequencedSet example
    public void processUniqueDonors(SequencedSet<String> donors) {
        donors.addFirst("Ahmad");
        donors.addLast("Fatimah");

        String first = donors.getFirst();  // "Ahmad"
        String last = donors.getLast();    // "Fatimah"
    }

    // SequencedMap example
    public void processMonthly(SequencedMap<String, BigDecimal> monthly) {
        monthly.putFirst("January", new BigDecimal("5000"));
        monthly.putLast("December", new BigDecimal("8000"));

        var firstEntry = monthly.firstEntry();  // January=5000
        var lastEntry = monthly.lastEntry();    // December=8000

        var reversed = monthly.reversed();
    }

    private record Donation(String purpose, BigDecimal amount) {}
}
```

## Preview Features

### 5. String Templates (Preview)

**JEP 430:** Safer string composition with embedded expressions.

**Status:** Preview in Java 21 (requires `--enable-preview`)

**Example:**

```java
public class NotificationService {

    // STR processor - simple interpolation
    public void sendWelcome(String donor, BigDecimal amount) {
        String message = STR."Welcome \{donor}! Your donation of $\{amount} received.";
        System.out.println(message);
    }

    // FMT processor - formatted output
    public void sendReceipt(String donor, BigDecimal amount, LocalDate date) {
        String receipt = FMT."""
            Receipt
            -------
            Donor: %s\{donor}
            Amount: $%.2f\{amount}
            Date: %tF\{date}
            """;
        System.out.println(receipt);
    }

    // Multi-line with expressions
    public void generateReport(List<Donation> donations) {
        BigDecimal total = donations.stream()
            .map(Donation::amount)
            .reduce(BigDecimal.ZERO, BigDecimal::add);

        String report = STR."""
            Donation Report
            ===============
            Total Donations: \{donations.size()}
            Total Amount: $\{total}
            Average: $\{total.divide(BigDecimal.valueOf(donations.size()), 2, RoundingMode.HALF_UP)}
            """;
        System.out.println(report);
    }
}
```

### 6. Unnamed Patterns and Variables (Preview)

**JEP 443:** Use underscore `_` for unused variables.

**Example:**

```java
public class TransactionHandler {

    // Ignore exception when not needed
    public BigDecimal parseAmount(String str) {
        try {
            return new BigDecimal(str);
        } catch (NumberFormatException _) {
            return BigDecimal.ZERO;
        }
    }

    // Ignore record components
    public void process(Transaction tx) {
        switch (tx) {
            case Transaction(String id, _, _, BigDecimal amount) ->
                // Only care about id and amount
                System.out.println("TX " + id + ": $" + amount);
        }
    }
}
```

### 7. Scoped Values (Preview)

**JEP 446:** Modern alternative to thread-local variables for sharing immutable data.

**Finalized in:** Java 25

**Example:**

```java
public class UserContextService {

    public static final ScopedValue<User> CURRENT_USER =
        ScopedValue.newInstance();

    // Set value for scope
    public void executeAsUser(User user, Runnable action) {
        ScopedValue.where(CURRENT_USER, user)
            .run(action);
    }

    // Access scoped value
    public void processTransaction() {
        User current = CURRENT_USER.get();
        System.out.println("Processing for: " + current.name());
    }

    // Works with virtual threads
    public void handleRequest(User user) {
        Thread.startVirtualThread(() -> {
            ScopedValue.where(CURRENT_USER, user).run(() -> {
                processTransaction();
            });
        });
    }
}
```

### 8. Structured Concurrency (Preview)

**JEP 453:** Treat groups of related tasks as single unit of work.

**Example shown in Virtual Threads section above.**

## Core Library Enhancements

### 9. Generational ZGC

**JEP 439:** Extends Z Garbage Collector with generational mode.

**Benefits:**

- Lower memory overhead
- Reduced GC overhead
- Better performance
- Maintains ultra-low latency

### 10. Key Encapsulation Mechanism API

**JEP 452:** API for key encapsulation mechanisms (KEMs) for cryptographic security.

## Performance Improvements

Java 21 includes numerous optimizations:

- **Startup Time:** Faster application startup
- **Memory Efficiency:** Improved footprint
- **GC Performance:** Better with Generational ZGC
- **JIT Compilation:** Enhanced optimization
- **Virtual Threads:** Massive concurrency with minimal overhead

## Migration from Java 17

**Key Changes:**

1. Adopt virtual threads for I/O-bound applications
2. Refactor `instanceof` chains to pattern matching
3. Use sequenced collections for ordered data
4. Review deprecated APIs

**Migration Steps:**

1. Update dependencies to Java 21-compatible versions
2. Test preview features with `--enable-preview`
3. Refactor code to adopt new features gradually
4. Benchmark virtual threads vs platform threads
5. Update security practices

## Why Upgrade to Java 21?

**For Java 17 Applications:**

- **Virtual Threads:** Revolutionary concurrency
- **Pattern Matching:** More expressive code
- **Performance:** 5-10% better than Java 17
- **Sequenced Collections:** Cleaner APIs
- **Long-term Support:** 8+ years

**For New Projects:**

- **Modern Features:** Latest innovations
- **Ecosystem Support:** Spring Boot 3.2+, Jakarta EE 10+
- **Virtual Threads:** Perfect for microservices
- **Future-Ready:** Foundation for Java 25

## Feature Evolution

| Feature                   | Java 17    | Java 21      | Java 25      |
| ------------------------- | ---------- | ------------ | ------------ |
| **Virtual Threads**       | ❌ None    | ✅ Finalized | ✅ Optimized |
| **Pattern Matching**      | 🔬 Preview | ✅ Finalized | ✅ Enhanced  |
| **Record Patterns**       | ❌ None    | ✅ Finalized | ✅ Available |
| **Sequenced Collections** | ❌ None    | ✅ Finalized | ✅ Available |
| **String Templates**      | ❌ None    | 🔬 Preview   | ⏸️ Withdrawn |
| **Scoped Values**         | ❌ None    | 🔬 Preview   | ✅ Finalized |

## Summary

Java 21 LTS (2023) revolutionized Java concurrency and finalized key language features:

- **Virtual threads** for massive concurrency
- **Sequenced collections** for ordered data
- **Pattern matching finalized** for type-safe conditionals
- **Record patterns** for cleaner destructuring
- **Performance improvements** across the board

**Next Steps:**

- Review [Java 17 Highlights](/en/learn/software-engineering/programming-languages/java/release-highlights/java-17) for foundation features
- Explore [Java 25 Highlights](/en/learn/software-engineering/programming-languages/java/release-highlights/java-25) for latest optimizations
