---
title: "Beginner"
weight: 10000003
date: 2026-05-15T00:00:00+07:00
draft: false
description: "Examples 1-20: The three zones, port interfaces, adapter classes, package structure, dependency direction, application service wiring, and the full request/response flow in Java 21+"
tags: ["hexagonal-architecture", "ports-and-adapters", "tutorial", "by-example", "oop", "java", "beginner"]
---

Examples 1–20 introduce hexagonal architecture (ports and adapters) using a procurement platform domain (`purchasing` context). Every code block is self-contained and runnable in Java 21+. Annotation density targets 1.0–2.25 comment lines per code line per example.

## The Three Zones (Examples 1–4)

### Example 1: The hexagon metaphor — three zones as packages

Hexagonal architecture divides every application into three concentric zones: the domain (pure business logic), the application (use-case orchestration), and the adapters (technology connectors). Each zone is a distinct Java package. The domain imports nothing from outside itself; the application imports only the domain; adapters import the application plus any framework.

```mermaid
%% Palette: Blue #0173B2, Teal #029E73, Orange #DE8F05
graph TD
    subgraph Adapter["Adapter Zone (outermost)"]
        WEB["HttpController\n#40;REST entry point#41;"]:::orange
        DB["PgPurchaseOrderRepository\n#40;Postgres adapter#41;"]:::orange
    end
    subgraph Application["Application Zone (middle)"]
        UC["IssuePurchaseOrderUseCase\n#40;input port#41;"]:::teal
        REPO["PurchaseOrderRepository\n#40;output port#41;"]:::teal
        SVC["IssuePurchaseOrderService\n#40;application service#41;"]:::teal
    end
    subgraph Domain["Domain Zone (innermost)"]
        PO["PurchaseOrder\n#40;aggregate root#41;"]:::blue
        POID["PurchaseOrderId\n#40;value object#41;"]:::blue
        MONEY["Money\n#40;value object#41;"]:::blue
    end

    WEB -->|"calls"| UC
    SVC -->|"implements"| UC
    SVC -->|"calls"| REPO
    DB -->|"implements"| REPO
    SVC -->|"uses"| PO

    classDef blue fill:#0173B2,stroke:#000,color:#fff,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000,color:#fff,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000,color:#fff,stroke-width:2px
```

{{< tabs items="Java,Kotlin,C#" >}}
{{< tab >}}

```java
// Zone 1: Domain — zero framework imports allowed
// => Package: com.example.procurement.purchasing.domain
// => Only JDK standard library (java.util, java.math) permitted
package com.example.procurement.purchasing.domain;

// PurchaseOrder: pure Java record — no @Entity, no @JsonProperty, no Spring annotations
// => Compiles and runs without Spring, JPA, or any framework on the classpath
public record PurchaseOrder(
    PurchaseOrderId id,      // => strongly-typed identity value object (format: po_<uuid>)
    SupplierId supplierId,   // => typed; cannot accidentally swap with PurchaseOrderId
    Money total,             // => Money carries both amount and ISO 4217 currency
    POStatus status          // => domain enum; no framework dependency
) {}

// Zone 2: Application — imports domain only
// => Package: com.example.procurement.purchasing.application
// => May import domain.*; must not import adapter packages

// Zone 3: Adapter — imports application and framework
// => Package: com.example.procurement.purchasing.adapter.in.web
// => May import org.springframework.*, application.*
package com.example.procurement.purchasing.adapter.in.web;
```

{{< /tab >}}
{{< tab >}}

```kotlin
// Zone 1: Domain — zero framework imports allowed
// => Package: com.example.procurement.purchasing.domain
// => Only Kotlin stdlib permitted; no framework on the classpath

package com.example.procurement.purchasing.domain

// PurchaseOrder: Kotlin data class — no @Entity, no @JsonProperty, no Spring annotations
// => data class: compiler generates equals, hashCode, copy, toString automatically
// => compiles and runs without Spring, JPA, or any external framework
data class PurchaseOrder(
    val id: PurchaseOrderId,        // => strongly-typed identity value object (format: po_<uuid>)
    val supplierId: SupplierId,     // => distinct type; compiler prevents accidental swaps
    val total: Money,               // => Money carries both amount and ISO 4217 currency code
    val status: POStatus            // => sealed class or enum; exhaustive when enforced by compiler
)
// => immutable by convention: all properties are val (read-only); use copy() to derive new state

// Zone 2: Application — imports domain only
// => Package: com.example.procurement.purchasing.application
// => May import domain.*; must not reference adapter packages

// Zone 3: Adapter — imports application and framework
// => Package: com.example.procurement.purchasing.adapter.in.web
// => May import org.springframework.*, application.*
```

{{< /tab >}}
{{< tab >}}

```csharp
// Zone 1: Domain — zero framework imports allowed
// => Namespace: Procurement.Purchasing.Domain
// => Only .NET BCL (System, System.Collections) permitted; no EF Core, no ASP.NET

namespace Procurement.Purchasing.Domain;

// PurchaseOrder: C# record — no [Entity], no [JsonProperty], no framework attributes
// => record: compiler generates Equals, GetHashCode, ToString, and with-expressions
// => immutable by default: init-only properties cannot be reassigned after construction
public sealed record PurchaseOrder(
    PurchaseOrderId Id,        // => strongly-typed identity value object (format: po_<uuid>)
    SupplierId SupplierId,     // => distinct type; compiler prevents accidental argument swaps
    Money Total,               // => Money record carrying Amount + ISO 4217 CurrencyCode
    POStatus Status            // => enum or discriminated-union; exhaustive switch enforced
);
// => immutable: all constructor parameters become init-only properties automatically
// => with-expression: var approved = po with { Status = POStatus.AwaitingApproval };

// Zone 2: Application — references Domain only
// => Namespace: Procurement.Purchasing.Application
// => May use Domain.*; must not reference Adapter namespaces

// Zone 3: Adapter — references Application and framework
// => Namespace: Procurement.Purchasing.Adapter.In.Web
// => May use Microsoft.AspNetCore.*, Application.*
```

{{< /tab >}}
{{< /tabs >}}

**Key Takeaway**: Domain knows nothing, application knows domain, adapters know application and frameworks.

**Why It Matters**: When the domain zone has zero framework imports, every domain test runs with no server, no database, and no container — feedback loop collapses from minutes to milliseconds. Swapping any persistence or web framework becomes a one-zone change confined to the adapter layer.

---

### Example 2: Domain entity — pure Java record, no framework annotations

A domain entity contains only business state and behaviour. Framework annotations (`@Entity`, `@Table`, `@JsonProperty`) are infrastructure concerns that belong in adapter-layer mapping classes. Placing them in the domain couples the domain to a specific framework and forces recompilation whenever that framework changes.

**Anti-pattern — framework annotation in the domain**:

{{< tabs items="Java,Kotlin,C#" >}}
{{< tab >}}

```java
// WRONG: @Entity and @JsonProperty leak infrastructure into the domain
// => domain class now requires JPA at test time; cannot run without Hibernate
import jakarta.persistence.Entity;               // => infrastructure import in domain zone
import com.fasterxml.jackson.annotation.JsonProperty; // => JSON framework in business class

@Entity                                          // => couples PurchaseOrder to JPA container
public class PurchaseOrder {
    @JsonProperty("supplier_id")                 // => JSON naming belongs in an adapter DTO
    private String supplierId;                   // => raw String; typed safety lost
}
// Problem: every unit test must bootstrap a JPA context and Jackson
// => coupling cost: framework deps forced into all domain tests
```

{{< /tab >}}
{{< tab >}}

```kotlin
// WRONG: @Entity and @JsonProperty leak infrastructure into the domain
// => Kotlin data class now requires JPA at test time; cannot run without Hibernate
import jakarta.persistence.Entity               // => infrastructure import pulled into domain zone
import com.fasterxml.jackson.annotation.JsonProperty // => JSON serialisation concern in business class

@Entity                                         // => couples PurchaseOrder to JPA container lifecycle
data class PurchaseOrder(
    @JsonProperty("supplier_id")                // => serialisation naming belongs in an adapter DTO
    val supplierId: String                      // => raw String; typed safety and null-safety lost
)
// Problem: every unit test must load a JPA context and the Jackson runtime
// => coupling cost: Hibernate + Jackson forced onto the domain test classpath
```

{{< /tab >}}
{{< tab >}}

```csharp
// WRONG: [Table] and [JsonPropertyName] leak infrastructure into the domain
// => record now requires EF Core at test time; cannot instantiate without the EF runtime
using Microsoft.EntityFrameworkCore;            // => infrastructure namespace in domain zone
using System.Text.Json.Serialization;           // => JSON serialisation concern in business class

[Table("purchase_orders")]                      // => couples PurchaseOrder to EF Core schema
public record PurchaseOrder(
    [property: JsonPropertyName("supplier_id")] // => JSON naming belongs in an adapter DTO
    string SupplierId                           // => raw string; typed safety and nullability lost
);
// Problem: every unit test must configure a DbContext and System.Text.Json
// => coupling cost: EF Core + JSON runtime forced onto the domain test dependency graph
```

{{< /tab >}}
{{< /tabs >}}

**Correct — clean domain record**:

{{< tabs items="Java,Kotlin,C#" >}}
{{< tab >}}

```java
// PurchaseOrder: zero framework imports; compiles with only the JDK
// => No @Entity, no @Id, no @JsonProperty, no Spring, no Jackson
package com.example.procurement.purchasing.domain;

public record PurchaseOrder(       // => record = immutable; equals/hashCode/toString generated
    PurchaseOrderId id,            // => domain value object, not raw String
    SupplierId supplierId,         // => distinct type; prevents id-kind confusion at compile time
    Money total,                   // => Money(amount, currency); richer than BigDecimal alone
    POStatus status                // => domain enum; exhaustive switch enforced by compiler
) {
    // Domain behaviour: pure function, no I/O, no framework calls
    public PurchaseOrder submit() {           // => returns new PurchaseOrder; this unchanged
        if (status != POStatus.DRAFT) {       // => guard: only DRAFT can be submitted
            throw new IllegalStateException("Can only submit a DRAFT PO"); // => domain rule
        }
        return new PurchaseOrder(id, supplierId, total, POStatus.AWAITING_APPROVAL);
        // => state transition: DRAFT → AWAITING_APPROVAL
    }
}
// Test: new PurchaseOrder(id, sup, money, DRAFT) — no framework; sub-millisecond
```

{{< /tab >}}
{{< tab >}}

```kotlin
// PurchaseOrder: zero framework imports; compiles with only the Kotlin stdlib
// => No @Entity, no @Id, no @JsonProperty, no Spring, no Jackson
package com.example.procurement.purchasing.domain

data class PurchaseOrder(          // => data class: equals/hashCode/copy/toString generated
    val id: PurchaseOrderId,       // => domain value object, not raw String
    val supplierId: SupplierId,    // => distinct type; prevents id-kind mix-up at compile time
    val total: Money,              // => Money(amount, currency); richer than BigDecimal alone
    val status: POStatus           // => sealed class or enum; exhaustive when enforced by compiler
) {
    // Domain behaviour: pure function — no I/O, no framework calls
    fun submit(): PurchaseOrder {              // => returns new PurchaseOrder via copy(); this unchanged
        require(status == POStatus.DRAFT) {    // => Kotlin stdlib guard: throws IllegalArgumentException
            "Can only submit a DRAFT PO"       // => domain rule expressed as lambda message
        }
        return copy(status = POStatus.AWAITING_APPROVAL)
        // => copy() creates new instance; only status field changes; all others preserved
        // => state transition: DRAFT → AWAITING_APPROVAL
    }
}
// Test: PurchaseOrder(id, sup, money, DRAFT) — no framework; sub-millisecond
```

{{< /tab >}}
{{< tab >}}

```csharp
// PurchaseOrder: zero framework references; compiles with only the .NET BCL
// => No [Table], no [Key], no [JsonPropertyName], no EF Core, no ASP.NET
namespace Procurement.Purchasing.Domain;

public sealed record PurchaseOrder( // => sealed record: immutable; Equals/GetHashCode/ToString generated
    PurchaseOrderId Id,             // => domain value object, not raw string
    SupplierId SupplierId,          // => distinct type; compiler rejects accidental argument swaps
    Money Total,                    // => Money(Amount, CurrencyCode); richer than decimal alone
    POStatus Status                 // => enum; exhaustive switch expression enforced by compiler
) {
    // Domain behaviour: pure method — no I/O, no framework calls
    public PurchaseOrder Submit()
    {
        if (Status != POStatus.Draft)           // => guard: only Draft orders may be submitted
            throw new InvalidOperationException("Can only submit a Draft PO"); // => domain rule
        return this with { Status = POStatus.AwaitingApproval };
        // => with-expression: creates new record; only Status changes; all other fields preserved
        // => state transition: Draft → AwaitingApproval
    }
}
// Test: new PurchaseOrder(id, sup, money, Draft) — no framework; sub-millisecond
```

{{< /tab >}}
{{< /tabs >}}

**Key Takeaway**: Domain records carry only business state and rules. Zero framework annotations means zero framework test dependencies.

**Why It Matters**: A domain class free of infrastructure annotations can be instantiated in a plain JUnit test in under a millisecond. Teams that enforce this boundary report that switching ORMs (e.g., JPA to jOOQ) touches only adapter files — thousands of lines of domain logic remain untouched.

---

### Example 3: Value object — PurchaseOrderId and Money

Value objects encapsulate a primitive value plus its invariants. They make illegal states unrepresentable at the type level and prevent the billion-dollar mistake of mixing up raw strings.

{{< tabs items="Java,Kotlin,C#" >}}
{{< tab >}}

```java
// Domain value objects: PurchaseOrderId and Money
// => package com.example.procurement.purchasing.domain
package com.example.procurement.purchasing.domain;

import java.math.BigDecimal;
import java.util.Objects;

// PurchaseOrderId: wraps a String but enforces the "po_<uuid>" format invariant
// => record: compiler generates equals, hashCode, toString automatically
// => value(): generated accessor — no need to hand-write getter
public record PurchaseOrderId(String value) {
    // Compact canonical constructor: runs validation before the implicit record constructor
    // => compact form omits parameter list; implicit this.value = value assignment still runs
    public PurchaseOrderId {
        Objects.requireNonNull(value, "PurchaseOrderId must not be null");
        // => null guard: fails fast at construction; no null PurchaseOrderId can exist
        if (!value.startsWith("po_") || value.length() < 39) {
            // => format invariant: "po_" prefix + 36-char UUID = minimum 39 chars
            throw new IllegalArgumentException("Invalid PurchaseOrderId: " + value);
            // => construction fails: caller sees the invalid value in the error message
        }
    }
    // => PurchaseOrderId("po_550e8400-e29b-41d4-a716-446655440000") — succeeds; 39 chars
    // => PurchaseOrderId("abc") — throws IllegalArgumentException; does not start with "po_"
    // => PurchaseOrderId(null)  — throws NullPointerException from requireNonNull above
}

// Money: immutable value object combining amount + ISO 4217 currency code
// => richer than BigDecimal alone: prevents silently mixing USD and EUR amounts
// => record: generated equals() compares both amount and currency fields
public record Money(BigDecimal amount, String currency) {
    // Compact canonical constructor: validates both fields before the record is built
    public Money {
        Objects.requireNonNull(amount, "amount required");
        // => null guard: amount field cannot be null; BigDecimal.ZERO is valid
        Objects.requireNonNull(currency, "currency required");
        // => null guard: currency field cannot be null; empty string caught below
        if (amount.compareTo(BigDecimal.ZERO) < 0) {
            // => invariant: negative money is not meaningful in the P2P domain
            throw new IllegalArgumentException("Money amount must be >= 0");
            // => caller sees clear message: "Money amount must be >= 0"
        }
        if (currency.length() != 3) {
            // => ISO 4217: all currency codes are exactly 3 uppercase letters (USD, EUR, IDR)
            throw new IllegalArgumentException("Currency must be 3-letter ISO 4217 code");
            // => "US" (2 chars) and "USDD" (4 chars) both fail here
        }
    }
    // => Money(new BigDecimal("1000.00"), "USD") — succeeds; amount >= 0, currency = 3 chars
    // => Money(new BigDecimal("-1"), "USD")       — throws; amount < 0 violates invariant
    // => Money(new BigDecimal("500"), "US")       — throws; currency must be 3 letters
}
```

{{< /tab >}}
{{< tab >}}

```kotlin
// Domain value objects: PurchaseOrderId and Money
// => package com.example.procurement.purchasing.domain
package com.example.procurement.purchasing.domain

import java.math.BigDecimal

// PurchaseOrderId: wraps a String but enforces the "po_<uuid>" format invariant
// => value class: zero-overhead wrapper; JVM erases to plain String at call sites
// => value property: generated by @JvmInline; no hand-written accessor needed
@JvmInline
value class PurchaseOrderId(val value: String) {
    // init block: Kotlin's canonical place for constructor-time validation
    // => runs after the primary constructor assigns the field; ideal for invariant checks
    init {
        requireNotNull(value) { "PurchaseOrderId must not be null" }
        // => null guard: Kotlin non-nullable type already prevents null at call site;
        // => explicit check documents the invariant and guards Java interop callers
        require(value.startsWith("po_") && value.length >= 39) {
            // => format invariant: "po_" prefix + 36-char UUID = minimum 39 chars
            "Invalid PurchaseOrderId: $value"
            // => string template: caller sees the invalid value in the error message
        }
    }
    // => PurchaseOrderId("po_550e8400-e29b-41d4-a716-446655440000") — succeeds; 39 chars
    // => PurchaseOrderId("abc") — throws IllegalArgumentException; fails startsWith check
}

// Money: immutable value object combining amount + ISO 4217 currency code
// => data class: compiler generates equals(), hashCode(), copy(), toString()
// => richer than BigDecimal alone: prevents silently mixing USD and EUR at compile time
data class Money(val amount: BigDecimal, val currency: String) {
    // init block: validates both fields before the data class is considered constructed
    init {
        require(amount >= BigDecimal.ZERO) {
            // => invariant: negative money has no meaning in a P2P procurement domain
            "Money amount must be >= 0"
            // => caller sees clear message; amount value in scope for string template if needed
        }
        require(currency.length == 3) {
            // => ISO 4217: all currency codes are exactly 3 uppercase letters (USD, EUR, IDR)
            "Currency must be 3-letter ISO 4217 code"
            // => "US" (2 chars) and "USDD" (4 chars) both fail this check
        }
    }
    // => Money(BigDecimal("1000.00"), "USD") — succeeds; amount >= 0, currency = 3 chars
    // => Money(BigDecimal("-1"), "USD")       — throws IllegalArgumentException; amount < 0
    // => Money(BigDecimal("500"), "US")       — throws IllegalArgumentException; currency length != 3
}
```

{{< /tab >}}
{{< tab >}}

```csharp
// Domain value objects: PurchaseOrderId and Money
// => Namespace: Procurement.Purchasing.Domain
// => Only System and System.Diagnostics.CodeAnalysis from the BCL
namespace Procurement.Purchasing.Domain;

// PurchaseOrderId: wraps a string but enforces the "po_<uuid>" format invariant
// => readonly record struct: value semantics + structural equality + zero heap allocation
// => Value property: generated by the record; no hand-written getter needed
public readonly record struct PurchaseOrderId(string Value)
{
    // Primary constructor guard via static factory — idiomatic for C# records
    // => records cannot have instance constructors with validation in a compact form;
    // => use a static Create method or a custom constructor as the validation entry point
    public static PurchaseOrderId Create(string value)
    {
        ArgumentNullException.ThrowIfNull(value, nameof(value));
        // => null guard: fails fast at creation; no null PurchaseOrderId can exist
        if (!value.StartsWith("po_", StringComparison.Ordinal) || value.Length < 39)
        {
            // => format invariant: "po_" prefix + 36-char UUID = minimum 39 chars
            throw new ArgumentException($"Invalid PurchaseOrderId: {value}", nameof(value));
            // => caller sees the invalid value interpolated into the exception message
        }
        return new PurchaseOrderId(value);
        // => only valid instances are returned; invalid input never produces an instance
    }
    // => PurchaseOrderId.Create("po_550e8400-e29b-41d4-a716-446655440000") — succeeds; 39 chars
    // => PurchaseOrderId.Create("abc") — throws ArgumentException; does not start with "po_"
    // => PurchaseOrderId.Create(null)  — throws ArgumentNullException from ThrowIfNull above
}

// Money: immutable value object combining Amount + ISO 4217 CurrencyCode
// => readonly record struct: structural equality compares both fields automatically
// => richer than decimal alone: prevents silently mixing USD and EUR at compile time
public readonly record struct Money(decimal Amount, string CurrencyCode)
{
    public static Money Create(decimal amount, string currencyCode)
    {
        ArgumentNullException.ThrowIfNull(currencyCode, nameof(currencyCode));
        // => null guard: currency field cannot be null; empty string caught below
        if (amount < 0)
        {
            // => invariant: negative money has no meaning in a P2P procurement domain
            throw new ArgumentOutOfRangeException(nameof(amount), "Money amount must be >= 0");
            // => caller sees clear message: "Money amount must be >= 0"
        }
        if (currencyCode.Length != 3)
        {
            // => ISO 4217: all currency codes are exactly 3 uppercase letters (USD, EUR, IDR)
            throw new ArgumentException("CurrencyCode must be 3-letter ISO 4217 code", nameof(currencyCode));
            // => "US" (2 chars) and "USDD" (4 chars) both fail this guard
        }
        return new Money(amount, currencyCode);
        // => only valid instances reach callers; invalid state cannot be constructed
    }
    // => Money.Create(1000.00m, "USD") — succeeds; amount >= 0, currencyCode = 3 chars
    // => Money.Create(-1m, "USD")       — throws ArgumentOutOfRangeException; amount < 0
    // => Money.Create(500m, "US")       — throws ArgumentException; currencyCode length != 3
}
```

{{< /tab >}}
{{< /tabs >}}

**Key Takeaway**: Value objects enforce invariants at construction, making invalid states impossible to represent downstream.

**Why It Matters**: When `PurchaseOrderId` and `SupplierId` are distinct types, the compiler catches accidental argument swaps that code review misses. Encoding the `po_` prefix in the type means no controller, service, or repository needs to re-validate format — the object simply cannot exist in an invalid state.

---

### Example 4: The dependency rule — what can import what

The dependency rule is the single most important invariant in hexagonal architecture: dependencies always point inward. Outer zones depend on inner zones; inner zones never depend on outer zones. This is enforced by package visibility, module boundaries, or architectural tests.

**Legal imports (inward dependencies only)**:

```java
// Application zone: may import domain types — inward dependency is always legal
// => package com.example.procurement.purchasing.application
import com.example.procurement.purchasing.domain.PurchaseOrder;   // => ok: inward dependency
import com.example.procurement.purchasing.domain.PurchaseOrderId; // => ok: inward direction
import com.example.procurement.purchasing.domain.Money;            // => ok: domain value object

// Adapter zone: may import application types — one step further outward
// => package com.example.procurement.purchasing.adapter.in.web
import com.example.procurement.purchasing.application.IssuePurchaseOrderUseCase; // => ok: inward
```

**Illegal imports (outward dependencies — architecture violations)**:

```java
// Domain zone importing Application: FORBIDDEN
// => package com.example.procurement.purchasing.domain
// import com.example.procurement.purchasing.application.IssuePurchaseOrderService; // => NEVER
// => domain would depend on orchestration; creates circular compile dependency

// Application zone importing Adapter: FORBIDDEN
// => package com.example.procurement.purchasing.application
// import com.example.procurement.purchasing.adapter.in.web.PurchaseOrderHttpController; // => NEVER
// => application logic coupled to Spring; cannot test without web container
```

**ArchUnit test enforcing the dependency rule in CI**:

```java
// ArchUnit: dependency rule as an executable test — runs in < 500ms
// => package com.example.procurement.architecture
package com.example.procurement.architecture;

import com.tngtech.archunit.core.importer.ClassFileImporter;
import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.noClasses;
import org.junit.jupiter.api.Test;

class HexagonDependencyTest {

    private static final var classes =
        new ClassFileImporter().importPackages("com.example.procurement");
    // => classes: all compiled .class files in the procurement package tree

    @Test void domain_must_not_depend_on_application() {
        noClasses().that().resideInAPackage("..domain..")
            .should().dependOnClassesThat().resideInAPackage("..application..")
            .check(classes);
        // => fails: any domain class that imports an application type → CI build broken
        // => passes: domain imports only java.* and sibling domain types
    }

    @Test void domain_must_not_depend_on_adapters() {
        noClasses().that().resideInAPackage("..domain..")
            .should().dependOnClassesThat().resideInAPackage("..adapter..")
            .check(classes);
        // => fails: @Entity or @RestController imported in domain → caught immediately
    }

    @Test void application_must_not_depend_on_adapters() {
        noClasses().that().resideInAPackage("..application..")
            .should().dependOnClassesThat().resideInAPackage("..adapter..")
            .check(classes);
        // => fails: application service imports PgPurchaseOrderRepository directly
        // => passes: application imports only domain types and port interfaces
    }
}
// => all three ArchUnit tests run as part of test:unit; sub-second; zero infrastructure
```

**Key Takeaway**: Dependencies always point inward — domain ← application ← adapters. The reverse direction is always an architectural violation.

**Why It Matters**: Enforcing the dependency rule with ArchUnit turns a convention into a compile-like gate. Any future commit that accidentally imports a Spring class into the domain will fail the CI build immediately, before it reaches code review. In a growing P2P platform, this protection becomes increasingly valuable — as the team scales from 5 to 50 developers, manual enforcement of architectural rules is unreliable. ArchUnit makes the hexagon self-defending: the architecture test suite rejects rule violations the same way a compiler rejects type errors.

---

## Output Ports (Examples 5–8)

### Example 5: Output port interface — PurchaseOrderRepository

An output port is a Java interface placed in the `domain.application` package. It expresses what the application needs from the outside world (e.g., persistence) using domain language. The interface has no knowledge of databases, SQL, or frameworks — it speaks only in domain types.

```java
// Output port: lives in application package; speaks domain language only
// => package com.example.procurement.purchasing.application
package com.example.procurement.purchasing.application;

import com.example.procurement.purchasing.domain.PurchaseOrder;
import com.example.procurement.purchasing.domain.PurchaseOrderId;
import java.util.Optional;

// PurchaseOrderRepository: output port for PO persistence
// => This is an interface — no implementation details here
// => Adapters implement this; domain/application code only calls this
public interface PurchaseOrderRepository {

    // save: persist a PurchaseOrder; return the saved instance (may have generated id)
    // => takes domain type; returns domain type; no JPA/JDBC types visible
    PurchaseOrder save(PurchaseOrder purchaseOrder);
    // => caller: repository.save(po) — does not know if storage is Postgres or in-memory

    // findById: retrieve a PurchaseOrder by its typed identity
    // => Optional<> makes the "not found" case explicit; no null returns
    Optional<PurchaseOrder> findById(PurchaseOrderId id);
    // => returns Optional.empty() when PO does not exist — caller handles absence explicitly

    // existsById: lightweight existence check without loading the full aggregate
    // => useful for duplicate-check guard before saving a new PO
    boolean existsById(PurchaseOrderId id);
    // => returns true if PO with given id is present; false otherwise
}
// => Application service calls this interface; zero coupling to Postgres/JPA
```

**Key Takeaway**: Output ports are Java interfaces in the application package that speak only in domain types — no SQL, no JPA, no HTTP.

**Why It Matters**: Because `PurchaseOrderRepository` is an interface, the application service can be tested with an in-memory implementation that runs in microseconds. Swapping from JPA to jOOQ later means writing one new adapter class — the application service and every test remain unchanged.

---

### Example 6: Clock output port — making time testable

Time is an implicit dependency. Code that calls `LocalDateTime.now()` directly is non-deterministic and cannot be tested without mocking the JVM clock. Wrapping time behind a `Clock` output port makes the dependency explicit and swappable.

```java
// Clock output port: time as an explicit dependency
// => package com.example.procurement.purchasing.application
package com.example.procurement.purchasing.application;

import java.time.Instant;

// Clock: output port; returns current time as a domain-neutral Instant
// => Adapter in production: returns Instant.now() from system clock
// => Adapter in tests: returns a fixed Instant — deterministic, no sleep() needed
public interface Clock {
    // now: returns the current point in time
    // => caller never calls Instant.now() directly; always goes through this port
    Instant now();
    // => test adapter: () -> Instant.parse("2026-01-01T00:00:00Z") — always same value
}

// Usage inside an application service:
// => clock.now() instead of Instant.now() — the port is injected by the composition root
public class IssuePurchaseOrderService {
    private final Clock clock; // => injected; production vs test adapter determined at wiring
    // => field clock: Clock — injected dependency; implementation chosen at wiring time

    public IssuePurchaseOrderService(Clock clock) { // => constructor injection; no @Autowired
        this.clock = clock; // => stored for use in business methods below
    }

    public PurchaseOrder issue(PurchaseOrder po) {
        Instant issuedAt = clock.now();  // => explicit; testable; no hidden System.currentTimeMillis
        // => issuedAt: the timestamp will be embedded in the issued PO or an event
        return po.issue(issuedAt);       // => delegates state transition to domain method
    }
}
```

**Key Takeaway**: Wrapping `Instant.now()` behind a `Clock` port makes time an explicit, swappable dependency — test adapters return fixed timestamps.

**Why It Matters**: Time-dependent business rules (e.g., "PO must be issued within 30 days of approval") become deterministically testable. Tests run at the same speed regardless of wall-clock time, and the CI build produces the same result at 3 AM as at 3 PM.

---

### Example 7: In-memory adapter implementing PurchaseOrderRepository

An in-memory adapter implements the output port using a `HashMap`. It has no external dependencies, starts instantly, and is the default adapter for unit and integration tests. It is a first-class production artifact — not a test utility — that lives in an `adapter.out.persistence` package.

```java
// In-memory adapter: implements PurchaseOrderRepository with a HashMap
// => package com.example.procurement.purchasing.adapter.out.persistence
package com.example.procurement.purchasing.adapter.out.persistence;

import com.example.procurement.purchasing.application.PurchaseOrderRepository;
import com.example.procurement.purchasing.domain.PurchaseOrder;
import com.example.procurement.purchasing.domain.PurchaseOrderId;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

// InMemoryPurchaseOrderRepository: adapter implementing the output port
// => No JPA, no Postgres; HashMap is the store
// => Thread-safety omitted for clarity; use ConcurrentHashMap in shared test contexts
public class InMemoryPurchaseOrderRepository implements PurchaseOrderRepository {

    private final Map<PurchaseOrderId, PurchaseOrder> store = new HashMap<>();
    // => store: HashMap<PurchaseOrderId, PurchaseOrder> — in-memory map as the backing store

    @Override
    public PurchaseOrder save(PurchaseOrder po) {
        store.put(po.id(), po);  // => key = typed PurchaseOrderId; value = PurchaseOrder record
        return po;               // => return the same instance; consistent with repository contract
    }

    @Override
    public Optional<PurchaseOrder> findById(PurchaseOrderId id) {
        return Optional.ofNullable(store.get(id)); // => absent = Optional.empty(); never null
        // => caller handles absence with Optional.orElseThrow() or Optional.isEmpty()
    }

    @Override
    public boolean existsById(PurchaseOrderId id) {
        return store.containsKey(id); // => O(1) lookup; returns true/false
    }
}
// Usage in test:
// var repo = new InMemoryPurchaseOrderRepository();
// var service = new IssuePurchaseOrderService(repo, new FixedClock());
// => wired in 2 lines; no Spring context; starts in < 1ms
```

**Key Takeaway**: The in-memory adapter implements the port with a `HashMap` — no framework, no database, instantiates in microseconds.

**Why It Matters**: Because the in-memory adapter satisfies the same interface as the Postgres adapter, every application service test can run without Docker. A suite of 500 service tests completes in under a second. The same tests run identically in CI without a database container.

---

### Example 8: JPA adapter implementing PurchaseOrderRepository

The JPA adapter implements `PurchaseOrderRepository` using Spring Data JPA. It lives in the adapter layer and contains all JPA-specific code: `@Entity`, `@Repository`, ORM mapping. The domain record remains annotation-free — the adapter maps between the domain record and a JPA entity class.

```java
// JPA adapter: persistence adapter implementing the output port
// => package com.example.procurement.purchasing.adapter.out.persistence
package com.example.procurement.purchasing.adapter.out.persistence;

import com.example.procurement.purchasing.application.PurchaseOrderRepository;
import com.example.procurement.purchasing.domain.*;
import jakarta.persistence.*;
import org.springframework.stereotype.Repository;
import java.math.BigDecimal;
import java.util.Optional;

// JPA entity: lives in adapter layer; domain record stays annotation-free
// => @Entity tells JPA to manage this class as a persistent entity
// => @Table maps the class to the "purchase_orders" table — adapter concern only
@Entity @Table(name = "purchase_orders")
class PurchaseOrderJpaEntity {
    @Id String id;               // => @Id: JPA primary key column; raw String ok (adapter layer)
    String supplierId;           // => raw String; reconstructed to typed SupplierId on load
    BigDecimal totalAmount;      // => stored as DECIMAL(19,4); currency stored in separate column
    String totalCurrency;        // => ISO 4217 code e.g. "USD"; reconstructed into Money on load
    String status;               // => stored as enum name string e.g. "AWAITING_APPROVAL"
    // => no domain annotations (@Entity, @Table) leak to the domain record — adapter boundary holds
}

// Spring Data JPA repository interface — adapter-internal; never exposed to application layer
// => extends JpaRepository: Spring generates all CRUD methods at boot time via reflection
// => @Repository: Spring marks this as a persistence component; exception translation enabled
@Repository
interface JpaPoRepository extends JpaRepository<PurchaseOrderJpaEntity, String> {}
// => Spring creates a proxy at startup; no handwritten SQL for basic CRUD operations

// PgPurchaseOrderRepository: the actual output port implementation used in production
// => implements PurchaseOrderRepository (domain-facing interface) using JPA internally
// => application service depends on PurchaseOrderRepository; never sees this class
public class PgPurchaseOrderRepository implements PurchaseOrderRepository {

    private final JpaPoRepository jpa; // => Spring Data JPA — adapter-internal detail only

    // Constructor injection: jpa is injected by the composition root (Spring @Configuration)
    // => no @Autowired on fields; domain and application zones never use @Autowired
    public PgPurchaseOrderRepository(JpaPoRepository jpa) {
        this.jpa = jpa; // => stored for use in save(), findById(), existsById()
    }

    @Override
    public PurchaseOrder save(PurchaseOrder po) {
        jpa.save(toEntity(po));  // => toEntity(): domain record → JPA entity mapping
        // => jpa.save(): Spring Data issues INSERT or UPDATE; returns managed entity (ignored)
        return po;               // => return original domain record; caller sees domain type only
        // => JPA entity never leaks past this method; boundary maintained
    }

    @Override
    public Optional<PurchaseOrder> findById(PurchaseOrderId id) {
        return jpa.findById(id.value())  // => id.value(): unwrap typed id to raw String for JPA
                  .map(this::toDomain);  // => toDomain(): JPA entity → domain record on the way out
        // => Optional.empty() when row not found; Optional<PurchaseOrder> propagated to caller
    }

    @Override
    public boolean existsById(PurchaseOrderId id) {
        return jpa.existsById(id.value());
        // => Spring Data: SELECT COUNT(*) WHERE id=?; returns boolean; O(1) cost
        // => id.value(): raw String unwrapped from typed PurchaseOrderId for JPA query
    }

    // toEntity: translate domain record → JPA entity (adapter-internal; domain never calls this)
    // => all field assignments are primitive unwrappings from typed value objects
    private PurchaseOrderJpaEntity toEntity(PurchaseOrder po) {
        var e = new PurchaseOrderJpaEntity();
        e.id = po.id().value();            // => PurchaseOrderId → raw String for @Id column
        e.supplierId = po.supplierId().value(); // => SupplierId → raw String for supplier_id column
        e.totalAmount = po.total().amount();    // => Money.amount() → BigDecimal for DECIMAL column
        e.totalCurrency = po.total().currency(); // => Money.currency() → String for currency column
        e.status = po.status().name();          // => POStatus enum → String name for status column
        return e;
        // => e: fully populated JPA entity; passed to jpa.save(); adapter-internal only
    }

    // toDomain: translate JPA entity → domain record (adapter-internal; called only from findById)
    // => reconstructs each typed value object from the raw DB column values
    private PurchaseOrder toDomain(PurchaseOrderJpaEntity e) {
        return new PurchaseOrder(
            new PurchaseOrderId(e.id),              // => raw String → typed PurchaseOrderId
            new SupplierId(e.supplierId),            // => raw String → typed SupplierId
            new Money(e.totalAmount, e.totalCurrency), // => BigDecimal + String → Money value object
            POStatus.valueOf(e.status)               // => String name → domain enum constant
        );
        // => domain record: fully reconstructed with typed value objects; no raw Strings visible
    }
}
// => domain record PurchaseOrder has zero JPA annotations; compiles without jakarta.persistence
// => swapping to jOOQ: replace toEntity/toDomain + JpaPoRepository; application service unchanged
```

**Key Takeaway**: The JPA adapter maps between the domain record and a JPA entity. All `@Entity` annotations stay in the adapter layer; the domain record remains annotation-free.

**Why It Matters**: Keeping JPA entities in the adapter layer means the domain is never recompiled when a column name changes or an index is added. The mapping methods are the only translation boundary — a single place to change when the schema evolves.

---

## Input Ports and Application Services (Examples 9–12)

### Example 9: Input port interface — use-case contract

An input port is a Java interface that defines a use case the application exposes to the outside world. Primary adapters (HTTP controllers, CLI, event consumers) call input ports; they never call application service classes directly. This keeps the adapter decoupled from service implementation details.

```java
// Input port: use-case interface in the application package
// => package com.example.procurement.purchasing.application
package com.example.procurement.purchasing.application;

import com.example.procurement.purchasing.domain.PurchaseOrder;
import com.example.procurement.purchasing.domain.PurchaseOrderId;

// IssuePurchaseOrderUseCase: input port; defines the contract for issuing a PO
// => HTTP controller calls this interface; never the concrete service class
public interface IssuePurchaseOrderUseCase {

    // IssuePOCommand: immutable command DTO carrying everything the use case needs
    // => record = compact; compiler-generated equals/hashCode; no Lombok required
    record IssuePOCommand(
        String supplierId,     // => raw String from HTTP layer; validated inside use case
        String totalAmount,    // => String from JSON; parsed to BigDecimal in service
        String totalCurrency   // => ISO 4217 currency code
    ) {}

    // execute: the single method of this use case
    // => takes a command (inbound DTO); returns the resulting domain object
    PurchaseOrder execute(IssuePOCommand command);
    // => throwing unchecked exception on domain violation; caller maps to HTTP 422
}
// => Controller wires to this interface; can be replaced with CLI or test double
```

**Key Takeaway**: Input ports are Java interfaces in the application package. Primary adapters depend on the interface, not the concrete service class.

**Why It Matters**: When a controller depends on `IssuePurchaseOrderUseCase` (an interface), a test can swap in a stub returning a known `PurchaseOrder` — no Spring context needed. Adding a CLI adapter that calls the same use case requires zero changes to the service or the interface.

---

### Example 10: Application service implementing the input port

The application service is the orchestration layer. It implements the input port, coordinates the domain objects, and calls output ports. It contains no business rules itself — those live in the domain. It contains no framework annotations — those live in the adapter layer.

```java
// Application service: implements input port; orchestrates domain + output ports
// => package com.example.procurement.purchasing.application
package com.example.procurement.purchasing.application;

import com.example.procurement.purchasing.domain.*;
import java.math.BigDecimal;
import java.util.UUID;

// IssuePurchaseOrderService: the concrete use-case orchestrator
// => No @Service, no @Transactional — framework annotations belong in the adapter layer
public class IssuePurchaseOrderService implements IssuePurchaseOrderUseCase {

    private final PurchaseOrderRepository repository; // => output port; injected at wiring time
    private final Clock clock;                        // => output port; testable time source

    // Constructor injection: explicit, no reflection, no framework needed for wiring
    // => composition root (main or Spring config) calls this constructor
    public IssuePurchaseOrderService(
        PurchaseOrderRepository repository,  // => injected: InMemory or Postgres adapter
        Clock clock                          // => injected: FixedClock or SystemClock adapter
    ) {
        this.repository = repository; // => stored for use in execute() below
        this.clock = clock;           // => stored for use in execute() below
    }

    @Override
    public PurchaseOrder execute(IssuePOCommand command) {
        // 1. Build typed domain value objects from the raw command fields
        var id = new PurchaseOrderId("po_" + UUID.randomUUID());
        // => generate a new ID; format "po_<uuid>" satisfies PurchaseOrderId invariant
        var supplierId = new SupplierId("sup_" + command.supplierId());
        // => wrap supplier id; SupplierId constructor validates format
        var total = new Money(
            new BigDecimal(command.totalAmount()), // => parse String → BigDecimal
            command.totalCurrency()                // => pass ISO code; Money validates length
        );

        // 2. Construct domain object in initial DRAFT state
        var po = new PurchaseOrder(id, supplierId, total, POStatus.DRAFT);
        // => new PurchaseOrder: immutable record in DRAFT state

        // 3. Apply domain transition: DRAFT → AWAITING_APPROVAL
        var submitted = po.submit(); // => domain method enforces guard; throws if not DRAFT
        // => submitted: new PurchaseOrder with status = AWAITING_APPROVAL

        // 4. Persist via output port
        return repository.save(submitted); // => output port call; adapter handles actual storage
        // => returns persisted PurchaseOrder; caller (controller) maps to response DTO
    }
}
```

**Key Takeaway**: The application service orchestrates domain objects and output ports. No business rules here; no framework annotations here.

**Why It Matters**: Because the service has no `@Service` or `@Transactional` annotation, it can be instantiated with `new` in a plain JUnit test. The entire use case — domain logic + port interactions — is testable without a Spring context, Postgres container, or HTTP server.

---

### Example 11: Naming conventions — ports and adapters

Consistent naming is the map that makes the hexagonal codebase navigable. When every developer follows the same suffix conventions, the role of any class is immediately obvious from its name alone.

```java
// Naming conventions for ports and adapters in the purchasing context
// => consistent naming makes architecture visible from class names alone

// OUTPUT PORTS (interfaces in application package):
// => Suffix: "Repository" for persistence, "Port" for other output concerns
interface PurchaseOrderRepository {}  // => output port: persistence
interface Clock {}                    // => output port: time (single-method; no suffix needed)
interface EventPublisher {}           // => output port: domain event publishing (future intermediate)

// INPUT PORTS (interfaces in application package):
// => Suffix: "UseCase" signals a driving-side port
interface IssuePurchaseOrderUseCase {}  // => input port: action verb + aggregate + UseCase
interface ApprovePurchaseOrderUseCase {}// => input port: different use case, same aggregate

// APPLICATION SERVICES (application package, no suffix confusion):
// => Suffix: "Service" implements an input port
class IssuePurchaseOrderService implements IssuePurchaseOrderUseCase {}
// => class name mirrors the use case it implements; easy grep

// ADAPTERS (adapter package):
// => Prefix with technology or direction; suffix: "Adapter" or "Controller"
class InMemoryPurchaseOrderRepository implements PurchaseOrderRepository {}
// => InMemory prefix signals the adapter technology
class PgPurchaseOrderRepository implements PurchaseOrderRepository {}
// => Pg prefix signals Postgres; implements the same port

class PurchaseOrderHttpController {}   // => primary adapter; HTTP in-bound
// => no "Impl" suffix anywhere — that suffix hides rather than reveals intent
```

**Key Takeaway**: Output ports end in `Repository`/`Port`, input ports end in `UseCase`, services end in `Service`, adapters are prefixed with their technology (`InMemory`, `Pg`, `Http`).

**Why It Matters**: When the naming convention is universally applied, a developer can locate the Postgres persistence adapter for `PurchaseOrder` by searching for `PgPurchaseOrder` — no IDE navigation required. Onboarding a new engineer to the codebase takes hours, not days. Consistent suffixes also make grep-based audits possible: finding all use cases is `grep -r "UseCase" src/`, and all adapters is `grep -r "implements.*Repository" src/`. Convention consistency pays compound interest as the team grows.

---

### Example 12: Package/namespace structure for hexagonal

The package structure enforces the three-zone separation at the filesystem level. A flat or disorganized package layout makes the dependency rule unenforceable and the architecture invisible to tooling.

```java
// Canonical package layout for the purchasing bounded context
// => each level of the hierarchy maps to a hexagonal zone

// com.example.procurement.purchasing.domain
//   PurchaseOrder.java          — aggregate root (record)
//   PurchaseOrderId.java        — value object (record)
//   SupplierId.java             — value object (record)
//   Money.java                  — value object (record)
//   POStatus.java               — domain enum (DRAFT, AWAITING_APPROVAL, APPROVED, ISSUED, ...)
//   DomainException.java        — base unchecked exception for domain violations

// com.example.procurement.purchasing.application
//   PurchaseOrderRepository.java — output port interface
//   Clock.java                   — output port interface
//   IssuePurchaseOrderUseCase.java — input port interface (+ IssuePOCommand record inside)
//   IssuePurchaseOrderService.java — application service (implements input port)

// com.example.procurement.purchasing.adapter.in.web
//   PurchaseOrderHttpController.java — primary adapter (Spring @RestController)
//   CreatePORequest.java             — inbound DTO (adapter-layer record)
//   CreatePOResponse.java            — outbound DTO (adapter-layer record)

// com.example.procurement.purchasing.adapter.out.persistence
//   InMemoryPurchaseOrderRepository.java — in-memory adapter (implements output port)
//   PgPurchaseOrderRepository.java       — JPA adapter (implements output port)
//   PurchaseOrderJpaEntity.java          — JPA entity class (adapter-internal)

// com.example.procurement
//   ProcurementApplication.java         — composition root (Spring @SpringBootApplication)

// => "in" packages = primary (driving) adapters; "out" packages = secondary (driven) adapters
// => the package tree makes the dependency direction readable without opening any file
```

**Key Takeaway**: Package paths encode the hexagonal zone (`domain`, `application`, `adapter.in.*`, `adapter.out.*`). The directory tree is the architecture diagram.

**Why It Matters**: Tools like ArchUnit can assert dependency rules by package path pattern — `noClasses().that().resideIn("..domain..").should().dependOn("..adapter..")`. Making the package structure match the architecture prevents silent violations that accumulate over months. When a new developer joins, the directory layout itself communicates the architecture without needing a whiteboard session. The package tree is the cheapest always-up-to-date architectural documentation in the codebase.

---

## Primary Adapters (Examples 13–15)

### Example 13: HTTP input adapter — thin controller

The HTTP controller is a primary (driving) adapter. Its job is to translate HTTP concepts (requests, status codes, error responses) into domain concepts (commands, domain objects, domain errors). It delegates all logic to an input port and must not contain any business rules.

```java
// Primary adapter: HTTP controller translating HTTP ↔ application layer
// => package com.example.procurement.purchasing.adapter.in.web
package com.example.procurement.purchasing.adapter.in.web;

import com.example.procurement.purchasing.application.*;
import com.example.procurement.purchasing.domain.PurchaseOrder;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;

// CreatePORequest: inbound DTO — adapter-layer record; never crosses into domain
// => all fields are String: raw JSON values; application service parses and validates them
// => record: immutable; generated equals/hashCode; Jackson deserialises from JSON body
record CreatePORequest(String supplierId, String totalAmount, String totalCurrency) {}
// => JSON example: {"supplierId":"550e...","totalAmount":"1500.00","totalCurrency":"USD"}

// CreatePOResponse: outbound DTO — adapter-layer record; built from domain types
// => domain typed fields are unwrapped to primitives for JSON serialisation
// => record: Spring's Jackson converter serialises all accessor methods to JSON fields
record CreatePOResponse(String id, String supplierId, String status) {}
// => JSON output: {"id":"po_...","supplierId":"sup_...","status":"AWAITING_APPROVAL"}

// PurchaseOrderHttpController: primary adapter — thin; zero business logic
// => @RestController: Spring annotation; marks this class as an HTTP adapter
// => @RequestMapping: all endpoints in this class are under /api/v1/purchase-orders
@RestController
@RequestMapping("/api/v1/purchase-orders")
public class PurchaseOrderHttpController {

    private final IssuePurchaseOrderUseCase useCase; // => input port interface; not the concrete service
    // => depends on interface: controller can be tested with a stub without Spring context

    public PurchaseOrderHttpController(IssuePurchaseOrderUseCase useCase) {
        this.useCase = useCase; // => constructor injection; Spring DI populates this from @Bean
        // => no @Autowired on field; explicit constructor dependency is visible and testable
    }

    // create: handles POST /api/v1/purchase-orders
    // => @PostMapping: maps HTTP POST to this method; @RequestBody: deserialises JSON body
    @PostMapping
    public ResponseEntity<CreatePOResponse> create(@RequestBody CreatePORequest req) {
        // Step 1: Translate HTTP inbound DTO → application command (no business logic)
        var command = new IssuePurchaseOrderUseCase.IssuePOCommand(
            req.supplierId(), req.totalAmount(), req.totalCurrency()
        );
        // => command: adapter-layer fields forwarded to application-layer command record

        // Step 2: Delegate to use case — all business logic lives in the service
        PurchaseOrder po = useCase.execute(command);
        // => execute(): port call; returns domain PurchaseOrder; adapter never sees service impl

        // Step 3: Translate domain result → HTTP outbound DTO
        var response = new CreatePOResponse(
            po.id().value(),          // => PurchaseOrderId → raw String for JSON "id" field
            po.supplierId().value(),  // => SupplierId → raw String for JSON "supplierId" field
            po.status().name()        // => POStatus enum → String name for JSON "status" field
        );
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
        // => HTTP 201 Created; JSON body: {"id":"po_...","supplierId":"sup_...","status":"AWAITING_APPROVAL"}
    }
}
```

**Key Takeaway**: The controller translates HTTP ↔ application layer. It holds `@RestController` but zero business logic — all logic lives in the use case.

**Why It Matters**: A controller that delegates immediately to a use case can be replaced by a CLI adapter with minimal effort. Both adapters share the same application service — the logic does not duplicate. When business rules change, only the service changes; the controller is unaffected.

---

### Example 14: DTO at the adapter boundary — inbound and outbound

DTOs (Data Transfer Objects) at the adapter boundary prevent domain objects from leaking into the HTTP response and prevent HTTP concepts from leaking into the domain. The mapping is explicit and happens at the adapter boundary.

```java
// Inbound DTO: adapter receives this from HTTP; converts to command before entering application
// => package com.example.procurement.purchasing.adapter.in.web
package com.example.procurement.purchasing.adapter.in.web;

// CreatePORequest: raw HTTP body; all fields are nullable Strings from JSON
// => adapter validates and converts to typed command; domain never sees this record
public record CreatePORequest(
    String supplierId,      // => raw supplier UUID from client; validated in service
    String totalAmount,     // => String from JSON; "1500.00"; service parses BigDecimal
    String totalCurrency    // => "USD"; service validates 3-letter ISO 4217
) {}
// => If a field is null, the application service throws a domain validation error
// => HTTP adapter catches it and returns 422 Unprocessable Entity

// Outbound DTO: adapter builds this from domain result before writing HTTP response
// => domain types are unwrapped to JSON-serialisable primitives
public record CreatePOResponse(
    String id,           // => po.id().value() — strip the typed wrapper for JSON
    String supplierId,   // => po.supplierId().value() — strip SupplierId wrapper
    String totalAmount,  // => po.total().amount().toPlainString() — BigDecimal to String
    String currency,     // => po.total().currency() — already a String
    String status        // => po.status().name() — domain enum to String
) {}
// => JSON output: {"id":"po_...","supplierId":"sup_...","totalAmount":"1500.00","currency":"USD","status":"AWAITING_APPROVAL"}

// Mapping in controller (explicit, no magic):
// CreatePOResponse toResponse(PurchaseOrder po) {
//     return new CreatePOResponse(
//         po.id().value(),
//         po.supplierId().value(),
//         po.total().amount().toPlainString(),
//         po.total().currency(),
//         po.status().name()
//     );
// } // => one method; no reflection; no framework annotation; trivially testable
```

**Key Takeaway**: Inbound DTOs carry raw HTTP data into the adapter boundary; outbound DTOs carry serialisable data out. Domain objects never cross either boundary.

**Why It Matters**: When the API response schema changes (e.g., adding a field), only the outbound DTO and the mapping method change — not the domain. When the domain model changes (e.g., renaming a field), only the mapping method changes — not the JSON contract. The boundary decouples both directions of change.

---

### Example 15: Multi-adapter — HTTP and CLI calling the same use case

Because the application service implements a Java interface (the input port), any number of primary adapters can call it without the service knowing. This is the "multiple ports" promise of hexagonal architecture.

```java
// Two primary adapters calling the same use case
// => HTTP adapter and CLI adapter both call IssuePurchaseOrderUseCase

// --- Adapter 1: HTTP (shown in Example 13) ---
// PurchaseOrderHttpController depends on IssuePurchaseOrderUseCase
// => receives HTTP POST; builds command; calls useCase.execute(command)

// --- Adapter 2: CLI ---
// => package com.example.procurement.purchasing.adapter.in.cli
package com.example.procurement.purchasing.adapter.in.cli;

import com.example.procurement.purchasing.application.IssuePurchaseOrderUseCase;
import com.example.procurement.purchasing.application.IssuePurchaseOrderUseCase.IssuePOCommand;

// PurchaseOrderCliAdapter: secondary primary adapter — reads from CLI args instead of HTTP
// => No @RestController; no Spring MVC; pure Java main-method adapter
public class PurchaseOrderCliAdapter {

    private final IssuePurchaseOrderUseCase useCase; // => same interface as HTTP controller
    // => depends on interface, not concrete class: same wiring, different caller

    public PurchaseOrderCliAdapter(IssuePurchaseOrderUseCase useCase) {
        this.useCase = useCase; // => injected at composition root alongside HTTP controller
    }

    // run: parse CLI args and call the use case
    // => args[0] = supplierId, args[1] = totalAmount, args[2] = currency
    public void run(String[] args) {
        if (args.length < 3) {
            System.err.println("Usage: issue-po <supplierId> <amount> <currency>");
            // => usage error; no business logic here; adapter responsibility only
            return;
        }
        var command = new IssuePOCommand(args[0], args[1], args[2]);
        // => build command from CLI args; same command type as HTTP adapter uses
        var po = useCase.execute(command); // => same use case, same application logic
        System.out.println("PO issued: " + po.id().value() + " status=" + po.status());
        // => output to stdout; CLI-specific presentation; domain unchanged
    }
}
// => IssuePurchaseOrderService.execute() is called by both adapters
// => Application logic runs once; adapters multiply without logic duplication
```

**Key Takeaway**: Multiple primary adapters can call the same input port. Adding a CLI or Kafka consumer adapter requires zero changes to the application service.

**Why It Matters**: The multi-adapter pattern is the practical payoff of hexagonal architecture. When a batch job needs the same PO issuance logic as the REST API, the team adds one adapter class — not a duplicated service method. All the business rules are tested once, against the single service.

---

## Composition Root and Wiring (Examples 16–17)

### Example 16: Composition root — wiring the hexagon at startup

The composition root is the single place in the application where adapters are selected and wired to ports. In a Spring Boot application, this is a `@Configuration` class. Outside Spring, it is a `main` method. The composition root is the only place that knows both sides of every port boundary.

```java
// Composition root: wires adapters to ports
// => package com.example.procurement (application root)
package com.example.procurement;

import com.example.procurement.purchasing.adapter.out.persistence.*;
import com.example.procurement.purchasing.application.*;
import org.springframework.context.annotation.*;

// HexagonConfiguration: Spring @Configuration as composition root
// => This class knows both the interface (port) and the implementation (adapter)
// => Domain and application classes know neither; only the composition root knows both
@Configuration
public class HexagonConfiguration {

    // clockBean: wires the system clock adapter to the Clock output port
    // => production: SystemClock adapter returns Instant.now()
    // => tests: FixedClock adapter returns a hard-coded Instant (see Example 7 wiring)
    @Bean
    public Clock clock() {
        return Instant::now; // => lambda implements the single-method Clock interface
        // => equivalent to: new SystemClock() that calls Instant.now()
    }

    // purchaseOrderRepository: wires the JPA adapter to the output port
    // => PgPurchaseOrderRepository implements PurchaseOrderRepository (the port)
    @Bean
    public PurchaseOrderRepository purchaseOrderRepository(JpaPoRepository jpa) {
        return new PgPurchaseOrderRepository(jpa); // => adapter selected here; port receives it
        // => swapping to InMemoryPurchaseOrderRepository is a one-line change here
    }

    // issuePurchaseOrderUseCase: wires the application service to the input port
    // => IssuePurchaseOrderService implements IssuePurchaseOrderUseCase (the port)
    @Bean
    public IssuePurchaseOrderUseCase issuePurchaseOrderUseCase(
        PurchaseOrderRepository repository, // => Spring injects the bean above
        Clock clock                         // => Spring injects the clock bean above
    ) {
        return new IssuePurchaseOrderService(repository, clock);
        // => application service constructed with explicit dependencies; no @Autowired inside
    }
}
// => PurchaseOrderHttpController receives IssuePurchaseOrderUseCase via constructor injection
// => Spring autowires the @Bean; controller never knows if it is Pg or InMemory adapter
```

**Key Takeaway**: The composition root is the only class that knows both port interfaces and their adapter implementations. All other classes are unaware of the wiring.

**Why It Matters**: Swapping from a Postgres adapter to an in-memory adapter (e.g., for a test profile) is a one-line change in the composition root — `return new InMemoryPurchaseOrderRepository()` instead of `return new PgPurchaseOrderRepository(jpa)`. No application service, no domain class, and no test double is modified.

---

### Example 17: Dependency injection wiring without Spring

For teams not using Spring, the hexagon can be wired manually in a `main` method. Constructor injection makes the wiring explicit and the dependency graph readable at a glance.

```java
// Manual composition root: wires the hexagon without a DI framework
// => package com.example.procurement
package com.example.procurement;

import com.example.procurement.purchasing.adapter.in.web.*;
import com.example.procurement.purchasing.adapter.out.persistence.*;
import com.example.procurement.purchasing.application.*;
import java.time.Instant;

public class Main {
    public static void main(String[] args) {
        // 1. Construct output port adapters (innermost dependencies first)
        Clock clock = Instant::now;
        // => clock: lambda adapter; Instant::now satisfies the single-method Clock port

        PurchaseOrderRepository repository = new InMemoryPurchaseOrderRepository();
        // => InMemoryPurchaseOrderRepository: adapter; implements PurchaseOrderRepository port

        // 2. Construct application service (depends on output ports only)
        IssuePurchaseOrderUseCase useCase =
            new IssuePurchaseOrderService(repository, clock);
        // => IssuePurchaseOrderService: implements the input port; depends on two output ports

        // 3. Construct primary adapter (depends on input port only)
        PurchaseOrderHttpController controller =
            new PurchaseOrderHttpController(useCase);
        // => controller: primary adapter; depends on interface, not concrete service

        // 4. Register controller with HTTP server (framework-specific step)
        // => e.g., Javalin.create(config -> config.addRoute(controller::create)).start(8080);
        // => or: HttpServer.create(new InetSocketAddress(8080), 0); etc.

        System.out.println("Procurement platform started");
        // => Output: Procurement platform started
        // => All dependencies resolved without a DI framework or reflection
    }
}
// Dependency graph visible in main():
// Main → controller → useCase → repository
//                            → clock
// => outermost (adapter) depends on inner (use case); inner depends on innermost (ports)
// => no dependency points outward; hexagonal rule enforced in plain Java
```

**Key Takeaway**: The hexagon can be fully wired without a DI framework — constructor injection makes the dependency graph explicit and readable.

**Why It Matters**: A manually-wired composition root makes the full dependency graph visible in one method. Teams that start without a DI framework avoid magic and annotation scanning. When they later adopt Spring, the DI annotations are additive — the constructor injection pattern remains unchanged.

---

## Testing and Error Handling (Examples 18–20)

### Example 18: Testing with the in-memory adapter — fast, no DB

The in-memory adapter enables the application service to be tested in a plain JUnit test without any framework bootstrap. This is the fastest, most reliable test level for business logic verification.

```java
// Application service test: uses in-memory adapter; no Spring, no Postgres, no Docker
// => package com.example.procurement.purchasing.application
package com.example.procurement.purchasing.application;

import com.example.procurement.purchasing.adapter.out.persistence.InMemoryPurchaseOrderRepository;
import com.example.procurement.purchasing.domain.*;
import org.junit.jupiter.api.*;
import static org.assertj.core.api.Assertions.*;

class IssuePurchaseOrderServiceTest {

    // Fixed clock: returns the same Instant every call — deterministic
    // => lambda implements the single-method Clock interface
    private static final Clock FIXED_CLOCK = () -> java.time.Instant.parse("2026-01-01T00:00:00Z");
    // => every test runs at 2026-01-01T00:00:00Z; no flakiness from wall-clock time

    private IssuePurchaseOrderService service;
    private InMemoryPurchaseOrderRepository repository;

    @BeforeEach void setUp() {
        repository = new InMemoryPurchaseOrderRepository(); // => fresh store for each test
        service = new IssuePurchaseOrderService(repository, FIXED_CLOCK);
        // => wired with two lines; no Spring context; starts in < 1ms
    }

    @Test void issues_purchase_order_and_persists_it() {
        // Arrange: build a valid command
        var command = new IssuePurchaseOrderUseCase.IssuePOCommand(
            "550e8400-e29b-41d4-a716-446655440000", // => supplierId raw value
            "1500.00",                               // => amount as String
            "USD"                                    // => ISO 4217 currency
        );

        // Act: call the use case
        PurchaseOrder result = service.execute(command);
        // => result: newly created PurchaseOrder in AWAITING_APPROVAL state

        // Assert: domain state
        assertThat(result.status()).isEqualTo(POStatus.AWAITING_APPROVAL);
        // => state transition DRAFT → AWAITING_APPROVAL completed by submit()
        assertThat(result.id().value()).startsWith("po_");
        // => id satisfies PurchaseOrderId invariant format "po_<uuid>"
        assertThat(result.total()).isEqualTo(new Money(new java.math.BigDecimal("1500.00"), "USD"));
        // => Money value equality from record equals()

        // Assert: persisted in repository
        var found = repository.findById(result.id());
        assertThat(found).isPresent();              // => Optional not empty
        assertThat(found.get().status()).isEqualTo(POStatus.AWAITING_APPROVAL); // => correct status
    }
}
// => Test runs in < 10ms; no Docker, no Spring, no Postgres; 100% deterministic
```

**Key Takeaway**: The in-memory adapter makes application service tests framework-free, instant, and 100% deterministic.

**Why It Matters**: A suite of 200 application service tests that runs in under two seconds enables fearless refactoring. Developers run the full suite on every save. When a domain rule changes, the relevant test fails immediately — not after a 90-second Docker boot. In a P2P platform context, this means that adding a new approval level or changing a PO state machine rule produces CI feedback in seconds rather than minutes, reducing the cycle time for every developer on the team.

---

### Example 19: Domain error hierarchy at port boundaries

Domain violations should be expressed as typed exceptions, not raw strings or generic `RuntimeException`. A domain error hierarchy allows the adapter layer to catch specific types and map them to appropriate HTTP status codes without knowing what caused them.

```java
// Domain error hierarchy: typed exceptions for business rule violations
// => package com.example.procurement.purchasing.domain
package com.example.procurement.purchasing.domain;

// DomainException: base unchecked exception for all domain violations
// => extends RuntimeException: no checked-exception pollution in use cases
public class DomainException extends RuntimeException {
    public DomainException(String message) { super(message); }
    // => message carries the domain-language description; logged as-is
}

// InvalidStateTransitionException: PO state machine violation
// => thrown when submit(), approve(), issue() called in wrong state
public class InvalidStateTransitionException extends DomainException {
    private final POStatus current;   // => the state the PO was in
    private final String attempted;   // => the transition that was attempted

    public InvalidStateTransitionException(POStatus current, String attempted) {
        super("Cannot " + attempted + " a PurchaseOrder in state " + current);
        // => message: "Cannot issue a PurchaseOrder in state DRAFT"
        this.current = current;   // => stored for adapter to inspect if needed
        this.attempted = attempted;
    }
    public POStatus current() { return current; }
    // => adapter can read current() to decide HTTP status code (e.g., 409 Conflict)
}

// Domain method throwing typed exception:
// inside PurchaseOrder.java
public PurchaseOrder submit() {
    if (status != POStatus.DRAFT) {
        throw new InvalidStateTransitionException(status, "submit");
        // => adapter catches InvalidStateTransitionException → HTTP 409 Conflict
    }
    return new PurchaseOrder(id, supplierId, total, POStatus.AWAITING_APPROVAL);
    // => returns new immutable record; this unchanged
}

// Adapter mapping domain error to HTTP status:
// inside PurchaseOrderHttpController.java
// @ExceptionHandler(InvalidStateTransitionException.class)
// ResponseEntity<String> handleInvalidTransition(InvalidStateTransitionException ex) {
//     return ResponseEntity.status(409).body(ex.getMessage());
//     // => 409 Conflict: "Cannot submit a PurchaseOrder in state AWAITING_APPROVAL"
// }
```

**Key Takeaway**: Domain errors are typed exceptions. The adapter layer catches specific types and maps them to HTTP status codes — no domain logic in the error handler.

**Why It Matters**: Typed domain exceptions make the domain's error contract explicit. A new adapter (CLI, GraphQL, gRPC) can map the same exception to its own protocol-specific error format without changing the domain or the application service. In a P2P platform, the same `InvalidStateTransitionException` maps to HTTP 409 in the REST adapter, an exit code 2 in the CLI adapter, and a `BAD_USER_INPUT` GraphQL error in the GraphQL adapter — one exception class, multiple protocol mappings, zero duplication of the domain rule that triggered it.

---

### Example 20: Full hexagonal flow — HTTP request to domain to response

This final example traces the complete request lifecycle from HTTP POST through every layer: adapter → input port → application service → domain → output port → response.

```java
// Full flow: HTTP POST /api/v1/purchase-orders
// => traces every layer; each comment shows which zone handles that step

// STEP 1 — HTTP request arrives at primary adapter
// => zone: adapter.in.web (PurchaseOrderHttpController)
// POST /api/v1/purchase-orders  body: {"supplierId":"550e...","totalAmount":"1500.00","currency":"USD"}
CreatePORequest req = new CreatePORequest("550e...", "1500.00", "USD");
// => HTTP body deserialised to adapter-layer inbound DTO; no domain type used here

// STEP 2 — Adapter translates request → command (boundary crossing)
// => zone: adapter.in.web → application (command is application-layer record)
IssuePurchaseOrderUseCase.IssuePOCommand command =
    new IssuePurchaseOrderUseCase.IssuePOCommand(
        req.supplierId(), req.totalAmount(), req.totalCurrency()
    );
// => command crosses from adapter zone into application zone; DTO stays in adapter zone

// STEP 3 — Application service orchestrates (no business rules here)
// => zone: application (IssuePurchaseOrderService)
// service.execute(command) runs:
PurchaseOrderId id = new PurchaseOrderId("po_" + UUID.randomUUID());
// => new id generated inside service; domain rule: format "po_<uuid>"
SupplierId supplierId = new SupplierId("sup_" + command.supplierId());
// => wrap to typed value object; SupplierId validates format
Money total = new Money(new BigDecimal(command.totalAmount()), command.totalCurrency());
// => Money validates: amount >= 0, currency is 3-letter ISO code

// STEP 4 — Domain object created and transition applied (business logic lives here)
// => zone: domain (PurchaseOrder.submit())
PurchaseOrder po = new PurchaseOrder(id, supplierId, total, POStatus.DRAFT);
// => initial state: DRAFT; immutable record
PurchaseOrder submitted = po.submit();
// => domain method: guards state = DRAFT; returns new PurchaseOrder(AWAITING_APPROVAL)
// => throws InvalidStateTransitionException if state != DRAFT — caught by adapter error handler

// STEP 5 — Output port call persists result
// => zone: application calls output port; adapter.out.persistence handles it
PurchaseOrder saved = repository.save(submitted);
// => repository: PurchaseOrderRepository (port); InMemory or Pg adapter selected at wiring
// => adapter stores record; returns same record; domain never sees SQL or HashMap internals

// STEP 6 — Response mapped back to adapter DTO and HTTP 201 returned
// => zone: adapter.in.web (PurchaseOrderHttpController)
CreatePOResponse response = new CreatePOResponse(
    saved.id().value(),         // => unwrap typed id → String for JSON
    saved.supplierId().value(), // => unwrap SupplierId → String
    saved.total().amount().toPlainString(), // => BigDecimal → String
    saved.total().currency(),   // => already String
    saved.status().name()       // => domain enum → String
);
// => HTTP 201 Created; body: {"id":"po_...","supplierId":"sup_...","totalAmount":"1500.00","currency":"USD","status":"AWAITING_APPROVAL"}
// => flow complete: HTTP → adapter → application → domain → output port → adapter → HTTP
```

**Key Takeaway**: The full flow crosses three zone boundaries (adapter → application → domain → output port → adapter), each crossing made explicit by a type transformation.

**Why It Matters**: Tracing the full flow reveals that every zone change is accompanied by a data translation — inbound DTO → command, command → domain types, domain types → persisted record, domain record → outbound DTO. These explicit translations are the system's natural seams: each can be tested in isolation, each can be modified without touching the others. This is the practical definition of a loosely coupled system.
