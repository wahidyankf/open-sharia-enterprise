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

**Key Takeaway**: Domain knows nothing, application knows domain, adapters know application and frameworks.

**Why It Matters**: When the domain zone has zero framework imports, every domain test runs with no server, no database, and no container — feedback loop collapses from minutes to milliseconds. Swapping any persistence or web framework becomes a one-zone change confined to the adapter layer.

---

### Example 2: Domain entity — pure Java record, no framework annotations

A domain entity contains only business state and behaviour. Framework annotations (`@Entity`, `@Table`, `@JsonProperty`) are infrastructure concerns that belong in adapter-layer mapping classes. Placing them in the domain couples the domain to a specific framework and forces recompilation whenever that framework changes.

**Anti-pattern — framework annotation in the domain**:

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

**Correct — clean domain record**:

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

**Key Takeaway**: Domain records carry only business state and rules. Zero framework annotations means zero framework test dependencies.

**Why It Matters**: A domain class free of infrastructure annotations can be instantiated in a plain JUnit test in under a millisecond. Teams that enforce this boundary report that switching ORMs (e.g., JPA to jOOQ) touches only adapter files — thousands of lines of domain logic remain untouched.

---

### Example 3: Value object — PurchaseOrderId and Money

Value objects encapsulate a primitive value plus its invariants. They make illegal states unrepresentable at the type level and prevent the billion-dollar mistake of mixing up raw strings.

```java
package com.example.procurement.purchasing.domain;

import java.math.BigDecimal;
import java.util.Objects;

// PurchaseOrderId: wraps a String but enforces the "po_<uuid>" format invariant
// => using a record eliminates the need to hand-write equals/hashCode
public record PurchaseOrderId(String value) {
    // Compact canonical constructor — runs before the implicit one
    public PurchaseOrderId {
        Objects.requireNonNull(value, "PurchaseOrderId must not be null"); // => null guard
        if (!value.startsWith("po_") || value.length() < 39) { // => format: "po_" + 36-char UUID
            throw new IllegalArgumentException("Invalid PurchaseOrderId: " + value);
            // => invariant enforced at construction; caller cannot create a bad id
        }
    }
    // => PurchaseOrderId("po_550e8400-...") succeeds; PurchaseOrderId("abc") throws
}

// Money: amount + ISO 4217 currency — richer than a raw BigDecimal
// => prevents mixing USD amounts with EUR amounts silently
public record Money(BigDecimal amount, String currency) {
    public Money {
        Objects.requireNonNull(amount, "amount required");   // => null guard
        Objects.requireNonNull(currency, "currency required"); // => null guard
        if (amount.compareTo(BigDecimal.ZERO) < 0) {         // => invariant: amount >= 0
            throw new IllegalArgumentException("Money amount must be >= 0");
        }
        if (currency.length() != 3) {                        // => ISO 4217 is exactly 3 letters
            throw new IllegalArgumentException("Currency must be 3-letter ISO 4217 code");
        }
    }
    // => Money(new BigDecimal("1000.00"), "USD") succeeds
    // => Money(new BigDecimal("-1"), "USD") throws at construction time
}
```

**Key Takeaway**: Value objects enforce invariants at construction, making invalid states impossible to represent downstream.

**Why It Matters**: When `PurchaseOrderId` and `SupplierId` are distinct types, the compiler catches accidental argument swaps that code review misses. Encoding the `po_` prefix in the type means no controller, service, or repository needs to re-validate format — the object simply cannot exist in an invalid state.

---

### Example 4: The dependency rule — what can import what

The dependency rule is the single most important invariant in hexagonal architecture: dependencies always point inward. Outer zones depend on inner zones; inner zones never depend on outer zones. This is enforced by package visibility, module boundaries, or architectural tests.

```java
// Dependency direction: Adapter → Application → Domain (never reversed)
// => if Domain imports an Adapter class, the architecture is broken

// LEGAL: Application imports Domain
// => package com.example.procurement.purchasing.application
import com.example.procurement.purchasing.domain.PurchaseOrder; // => ok: inward dependency
import com.example.procurement.purchasing.domain.PurchaseOrderId; // => ok: inward

// LEGAL: Adapter imports Application
// => package com.example.procurement.purchasing.adapter.in.web
import com.example.procurement.purchasing.application.IssuePurchaseOrderUseCase; // => ok

// ILLEGAL: Domain importing Application (outward dependency)
// => package com.example.procurement.purchasing.domain
// import com.example.procurement.purchasing.application.*; // => NEVER — breaks hexagon
// => domain would depend on orchestration layer — circular when service imports domain

// ILLEGAL: Application importing an Adapter (outward dependency)
// => package com.example.procurement.purchasing.application
// import com.example.procurement.purchasing.adapter.in.web.*; // => NEVER
// => application logic would be coupled to Spring/HTTP — untestable without server

// Enforced with ArchUnit in CI:
// noClasses().that().resideInPackage("..domain..")
//     .should().dependOnClassesThat()
//     .resideInPackage("..application..")  // => ArchUnit test fails if rule violated
// => run in test:unit; acts as compile-time-equivalent guard on import graph
```

**Key Takeaway**: Dependencies always point inward — domain ← application ← adapters. The reverse direction is always an architectural violation.

**Why It Matters**: Enforcing the dependency rule with ArchUnit turns a convention into a compile-like gate. Any future commit that accidentally imports a Spring class into the domain will fail the CI build immediately, before it reaches code review.

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
// => @Entity: JPA maps this class to the "purchase_orders" table — adapter concern only
@Entity @Table(name = "purchase_orders")
class PurchaseOrderJpaEntity {
    @Id String id;               // => JPA primary key; raw String ok here (adapter layer)
    String supplierId;           // => raw String; typed domain value object mapped on load
    BigDecimal totalAmount;      // => stored as DECIMAL; currency stored separately
    String totalCurrency;        // => ISO 4217 code; reconstructed into Money on load
    String status;               // => stored as string enum name
}

// Spring Data JPA repository interface — adapter-internal, not the output port
// => JpaPoRepository is NOT exposed to the application layer
@Repository
interface JpaPoRepository extends JpaRepository<PurchaseOrderJpaEntity, String> {}
// => Spring generates implementation at boot time

// PgPurchaseOrderRepository: the actual output port implementation
// => Implements the domain-facing interface; maps to/from JPA entity internally
public class PgPurchaseOrderRepository implements PurchaseOrderRepository {

    private final JpaPoRepository jpa; // => Spring Data JPA — adapter-internal detail

    public PgPurchaseOrderRepository(JpaPoRepository jpa) {
        this.jpa = jpa; // => constructor injection; no field @Autowired in domain/application
    }

    @Override
    public PurchaseOrder save(PurchaseOrder po) {
        jpa.save(toEntity(po));   // => maps domain record → JPA entity; JPA persists it
        return po;                // => return domain record; caller never sees JPA entity
    }

    @Override
    public Optional<PurchaseOrder> findById(PurchaseOrderId id) {
        return jpa.findById(id.value()) // => JPA finds by raw String id
                  .map(this::toDomain); // => maps JPA entity → domain record on the way out
    }

    @Override
    public boolean existsById(PurchaseOrderId id) {
        return jpa.existsById(id.value()); // => delegates to Spring Data; O(1) COUNT query
    }

    // Mapping helpers: adapter-internal; domain never sees these methods
    private PurchaseOrderJpaEntity toEntity(PurchaseOrder po) {
        var e = new PurchaseOrderJpaEntity();
        e.id = po.id().value();                         // => unwrap typed id to raw String
        e.supplierId = po.supplierId().value();         // => unwrap SupplierId
        e.totalAmount = po.total().amount();            // => extract BigDecimal
        e.totalCurrency = po.total().currency();        // => extract ISO code
        e.status = po.status().name();                  // => enum to String
        return e;
    }

    private PurchaseOrder toDomain(PurchaseOrderJpaEntity e) {
        return new PurchaseOrder(
            new PurchaseOrderId(e.id),           // => reconstruct typed value object
            new SupplierId(e.supplierId),         // => reconstruct typed value object
            new Money(e.totalAmount, e.totalCurrency), // => reconstruct Money
            POStatus.valueOf(e.status)            // => String back to domain enum
        );
    }
}
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

**Why It Matters**: When the naming convention is universally applied, a developer can locate the Postgres persistence adapter for `PurchaseOrder` by searching for `PgPurchaseOrder` — no IDE navigation required. Onboarding a new engineer to the codebase takes hours, not days.

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

**Why It Matters**: Tools like ArchUnit can assert dependency rules by package path pattern — `noClasses().that().resideIn("..domain..").should().dependOn("..adapter..")`. Making the package structure match the architecture prevents silent violations that accumulate over months.

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
// => Fields are Strings; the application service parses/validates them into domain types
record CreatePORequest(String supplierId, String totalAmount, String totalCurrency) {}

// CreatePOResponse: outbound DTO — adapter-layer record; built from domain object
// => Domain types are unwrapped to primitives for JSON serialisation
record CreatePOResponse(String id, String supplierId, String status) {}

// PurchaseOrderHttpController: primary adapter — thin; zero business logic
// => @RestController: Spring annotation lives here, not in domain or application
@RestController
@RequestMapping("/api/v1/purchase-orders")
public class PurchaseOrderHttpController {

    private final IssuePurchaseOrderUseCase useCase; // => input port; not the concrete service
    // => depends on interface: swappable adapter in tests without Spring context

    public PurchaseOrderHttpController(IssuePurchaseOrderUseCase useCase) {
        this.useCase = useCase; // => constructor injection; Spring populates from DI container
    }

    @PostMapping
    public ResponseEntity<CreatePOResponse> create(@RequestBody CreatePORequest req) {
        // Translate HTTP request → command (no business logic here)
        var command = new IssuePurchaseOrderUseCase.IssuePOCommand(
            req.supplierId(), req.totalAmount(), req.totalCurrency()
        ); // => command: inbound DTO → application-layer command record

        // Delegate to use case (all logic lives there)
        PurchaseOrder po = useCase.execute(command); // => port call; adapter does not care about impl

        // Translate domain result → HTTP response
        var response = new CreatePOResponse(
            po.id().value(),          // => unwrap typed id to String for JSON
            po.supplierId().value(),  // => unwrap SupplierId
            po.status().name()        // => domain enum to String
        );
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
        // => 201 Created; body is the outbound DTO
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

**Why It Matters**: A suite of 200 application service tests that runs in under two seconds enables fearless refactoring. Developers run the full suite on every save. When a domain rule changes, the relevant test fails immediately — not after a 90-second Docker boot.

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

**Why It Matters**: Typed domain exceptions make the domain's error contract explicit. A new adapter (CLI, GraphQL, gRPC) can map the same exception to its own protocol-specific error format without changing the domain or the application service.

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
