---
title: "Java 17 Lts"
date: 2026-02-03T00:00:00+07:00
draft: false
description: "Key highlights from Java 17 LTS release - sealed classes, pattern matching preview, and platform improvements"
weight: 1000004
tags: ["java", "java-17", "lts", "sealed-classes", "pattern-matching"]
---

## Release Overview

**Java 17** was released on **September 15, 2021**, as the first LTS release following the new two-year cadence (replacing the previous three-year cycle). This release delivers **14 JEPs** focused on language enhancements, platform improvements, and developer productivity.

**Key Metrics:**

- **Release Date:** September 15, 2021
- **Support Duration:** 8+ years (until 2029+)
- **Previous LTS:** Java 11 (September 2018, 3-year gap)
- **Next LTS:** Java 21 (September 2023, 2-year gap)
- **JEPs Delivered:** 14 enhancements

## Major Language Features

### 1. Sealed Classes (Finalized)

**JEP 409:** Restrict which classes can extend or implement a type, enabling precise domain modeling and exhaustive pattern matching.

**Key Benefits:**

- Explicit type hierarchies
- Compiler exhaustiveness checking
- Better security and encapsulation
- Clear documentation of permitted subtypes

**Example:**

```java
// Sealed interface with permitted implementations
public sealed interface PaymentMethod
    permits CreditCard, BankTransfer, Cash {

    BigDecimal getAmount();
    boolean process();
}

public final class CreditCard implements PaymentMethod {
    private final String cardNumber;
    private final BigDecimal amount;

    public CreditCard(String cardNumber, BigDecimal amount) {
        this.cardNumber = cardNumber;
        this.amount = amount;
    }

    @Override
    public BigDecimal getAmount() { return amount; }

    @Override
    public boolean process() {
        // Credit card processing logic
        return true;
    }
}

public final class BankTransfer implements PaymentMethod {
    private final String accountNumber;
    private final BigDecimal amount;

    @Override
    public BigDecimal getAmount() { return amount; }

    @Override
    public boolean process() {
        // Bank transfer logic
        return true;
    }
}

public final class Cash implements PaymentMethod {
    private final BigDecimal amount;

    @Override
    public BigDecimal getAmount() { return amount; }

    @Override
    public boolean process() {
        // Cash processing logic
        return true;
    }
}

// Exhaustive switch - compiler ensures all cases covered
public class PaymentProcessor {
    public void process(PaymentMethod payment) {
        switch (payment) {
            case CreditCard card ->
                System.out.println("Credit: " + card.getAmount());
            case BankTransfer transfer ->
                System.out.println("Transfer: " + transfer.getAmount());
            case Cash cash ->
                System.out.println("Cash: " + cash.getAmount());
            // No default needed - compiler checks exhaustiveness
        }
    }
}
```

**Hierarchy Example:**

```java
// Sealed class with mixed modifiers
public abstract sealed class Transaction
    permits ZakatTransaction, GeneralDonation {

    protected final String id;
    protected final BigDecimal amount;
    protected final LocalDateTime timestamp;

    protected Transaction(String id, BigDecimal amount) {
        this.id = id;
        this.amount = amount;
        this.timestamp = LocalDateTime.now();
    }

    public abstract String getType();
}

// Final subclass (cannot be extended)
public final class ZakatTransaction extends Transaction {
    private final String donorId;
    private final ZakatCategory category;

    @Override
    public String getType() { return "ZAKAT"; }
}

// Sealed subclass (controlled extension)
public sealed class GeneralDonation extends Transaction
    permits EmergencyDonation, ProgramDonation {

    protected final String purpose;

    @Override
    public String getType() { return "DONATION"; }
}

public final class EmergencyDonation extends GeneralDonation {
    private final String emergencyType;
}

public final class ProgramDonation extends GeneralDonation {
    private final String programName;
}
```

### 2. Pattern Matching for Switch (Preview)

**JEP 406:** Enable pattern matching in `switch` statements and expressions with type patterns, guards, and null handling.

**Status:** Preview (requires `--enable-preview`)  
**Finalized in:** Java 21

**Example:**

```java
public class TransactionValidator {

    // Before Java 17 - verbose instanceof chain
    public String validateOld(Object tx) {
        String result;
        if (tx instanceof CreditCard) {
            CreditCard card = (CreditCard) tx;
            result = "Credit card: " + card.getAmount();
        } else if (tx instanceof BankTransfer) {
            BankTransfer transfer = (BankTransfer) tx;
            result = "Transfer: " + transfer.getAmount();
        } else {
            result = "Unknown";
        }
        return result;
    }

    // Java 17 - pattern matching for switch
    public String validate(Object tx) {
        return switch (tx) {
            case CreditCard card ->
                "Credit card: " + card.getAmount();
            case BankTransfer transfer ->
                "Transfer: " + transfer.getAmount();
            case Cash cash ->
                "Cash: " + cash.getAmount();
            case null ->
                "Null transaction";
            default ->
                "Unknown type";
        };
    }

    // Pattern matching with guards
    public String validateWithAmount(PaymentMethod payment) {
        return switch (payment) {
            case CreditCard card when card.getAmount().compareTo(BigDecimal.ZERO) > 0 ->
                "Valid credit card";
            case BankTransfer transfer when transfer.getAmount().compareTo(BigDecimal.ZERO) > 0 ->
                "Valid transfer";
            case Cash cash when cash.getAmount().compareTo(BigDecimal.ZERO) > 0 ->
                "Valid cash";
            default ->
                "Invalid amount";
        };
    }
}
```

### 3. Restore Always-Strict Floating-Point Semantics

**JEP 306:** All floating-point operations now strictly follow IEEE 754 standards. The `strictfp` keyword is no longer needed.

**Before Java 17:**

```java
// Had to use strictfp for consistent results
public strictfp class FinancialCalculator {
    public double calculateInterest(double principal, double rate) {
        return principal * rate;
    }
}
```

**Java 17 and Later:**

```java
// strictfp no longer needed - all operations strict by default
public class FinancialCalculator {
    public double calculateInterest(double principal, double rate) {
        return principal * rate;
        // Guaranteed IEEE 754 compliant on all platforms
    }
}
```

## Core Library Enhancements

### 4. Enhanced Pseudo-Random Number Generators

**JEP 356:** New interfaces and implementations for PRNGs with better stream support and algorithm flexibility.

**New Interfaces:**

- `RandomGenerator` - Base interface
- `RandomGenerator.SplittableGenerator` - For parallel streams
- `RandomGenerator.JumpableGenerator` - Skip-ahead capability
- `RandomGenerator.LeapableGenerator` - Large jumps
- `RandomGenerator.ArbitrarilyJumpableGenerator` - Arbitrary distance jumps

**Example:**

```java
import java.util.random.RandomGenerator;
import java.util.random.RandomGeneratorFactory;

public class DonationDistributor {

    // Use specific algorithm
    public void distributeRandomly() {
        RandomGenerator generator = RandomGeneratorFactory
            .of("L128X1024MixRandom")
            .create(12345L);  // Seed for reproducibility

        int beneficiaryIndex = generator.nextInt(100);
        double amount = generator.nextDouble(1000.0);

        System.out.println("Selected beneficiary: " + beneficiaryIndex);
        System.out.println("Amount: " + amount);
    }

    // Use default generator
    public void distributeWithDefault() {
        RandomGenerator generator = RandomGenerator.getDefault();

        // Stream-based operations
        generator.ints(10, 1, 101)
            .forEach(index ->
                System.out.println("Beneficiary " + index + " selected"));
    }

    // List available algorithms
    public void listAlgorithms() {
        RandomGeneratorFactory.all()
            .map(RandomGeneratorFactory::name)
            .sorted()
            .forEach(System.out::println);
    }
}
```

## Platform and Performance

### 5. macOS/AArch64 Port

**JEP 391:** Native support for macOS on Apple Silicon (M1, M2, and later chips).

**Benefits:**

- Native performance on Apple Silicon
- Better battery life
- Improved thermal efficiency
- Feature parity with x64 macOS

### 6. New macOS Rendering Pipeline

**JEP 382:** Java 2D rendering using Apple's Metal API instead of deprecated OpenGL.

## Security and Encapsulation

### 7. Strong Encapsulation of JDK Internals

**JEP 403:** Internal JDK APIs are strongly encapsulated by default (except for critical APIs like `sun.misc.Unsafe`).

**Impact:**

- Most internal APIs inaccessible via reflection
- Improves security and maintainability
- May require code changes if using internal APIs
- Use `--add-opens` flag if necessary (not recommended)

## Deprecations and Removals

**Deprecated for Removal:**

- **Security Manager** (JEP 411) - Will be removed in future versions
- **Applet API** (JEP 398) - Browser plugins obsolete

**Removed:**

- **RMI Activation** (JEP 407) - Obsolete since Java 8
- **Experimental AOT and JIT Compiler** (JEP 410) - Limited adoption

## Incubator and Preview Features

### Foreign Function & Memory API (Incubator)

**JEP 412:** API for interoperating with code and data outside the Java runtime, replacing JNI.

**Status:** Incubator in Java 17, finalized in Java 25

### Vector API (Second Incubator)

**JEP 414:** Express vector computations that compile to optimal hardware instructions.

**Use Cases:**

- Machine learning
- Cryptography
- Financial modeling
- Image processing

**Status:** Incubator in Java 17, continues evolving

### Context-Specific Deserialization Filters

**JEP 415:** Configure context-specific deserialization filters for better security.

## Performance Improvements

Java 17 includes thousands of optimizations:

- **Garbage Collection:** Better G1GC, ZGC, Shenandoah performance
- **JIT Compiler:** Faster warm-up, better peak performance
- **Startup Time:** Reduced application startup time
- **Memory Footprint:** Lower memory consumption
- **Security Updates:** Latest patches and cryptographic algorithms

## Migration Considerations

**From Java 11 to Java 17:**

**Breaking Changes:**

- Removed APIs (RMI Activation, Nashorn)
- Strong encapsulation of internal APIs
- Reflection changes may require `--add-opens`

**Migration Steps:**

1. Update dependencies to Java 17-compatible versions
2. Run comprehensive tests
3. Address deprecation warnings
4. Update build tools (Maven plugins, Gradle)
5. Performance test thoroughly

## Why Upgrade to Java 17?

**For Java 8/11 Applications:**

- **Long-term Support:** 8+ years of updates
- **Performance:** 10-25% better than Java 11
- **Security:** Latest security features
- **Language Features:** Sealed classes, pattern matching, records (Java 16)
- **Modern APIs:** Enhanced collections, streams, HTTP client

**For New Projects:**

- **Industry Standard:** Widely adopted LTS version
- **Rich Ecosystem:** Excellent framework support
- **Best Tooling:** IDE and build tool support
- **Future-Proof:** Foundation for Java 21/25 migration

## Feature Evolution

| Feature              | Java 17      | Java 21      | Java 25      |
| -------------------- | ------------ | ------------ | ------------ |
| **Sealed Classes**   | ✅ Finalized | ✅ Available | ✅ Available |
| **Pattern Matching** | 🔬 Preview   | ✅ Finalized | ✅ Enhanced  |
| **Records**          | ✅ Finalized | ✅ Available | ✅ Available |
| **Text Blocks**      | ✅ Finalized | ✅ Available | ✅ Available |

## Migration Path

Java 17 serves as foundation for future LTS migrations:

**Java 17 → Java 21:**

- Adopt virtual threads for I/O-bound services
- Finalize pattern matching usage
- Leverage sequenced collections

**Java 17 → Java 25:**

- Two-phase migration recommended (17 → 21 → 25)
- Direct migration possible but riskier
- Gain all concurrency and performance improvements

## Summary

Java 17 LTS (2021) established modern Java foundations with:

- **Sealed classes** for precise type hierarchies
- **Pattern matching preview** for cleaner conditionals
- **Enhanced PRNG** for better randomness
- **macOS/AArch64 support** for Apple Silicon
- **Strong encapsulation** for better security

**Next Steps:**

- Review [Java 21 Highlights](/en/learn/software-engineering/programming-languages/java/release-highlights/java-21) for virtual threads and sequenced collections
- Explore [Java 25 Highlights](/en/learn/software-engineering/programming-languages/java/release-highlights/java-25) for performance optimizations
