---
title: "Advanced"
weight: 10000014
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Advanced Cases guides (Guides 16–27) — Testcontainers database integration test, schema migration adapter with Flyway, banking port and Spring RestClient adapter, retry and circuit-breaker with Resilience4j, observability adapter with Micrometer Tracing, domain event flow end-to-end, hexagonal anti-patterns, Kubernetes deployment topology, Micrometer + OTLP + Prometheus observability stack, failure-mode HealthIndicator wiring, Flyway at deploy time, and configuration adapter"
tags:
  [
    "ddd",
    "hexagonal-architecture",
    "java",
    "spring-boot",
    "cases",
    "testcontainers",
    "flyway",
    "resilience4j",
    "micrometer",
    "opentelemetry",
    "anti-patterns",
    "advanced",
  ]
---

## Guide 16 — Database Integration Test via Testcontainers

### Why It Matters

Unit tests with an in-memory adapter (Guide 9) prove port correctness but cannot catch SQL schema mistakes, PostgreSQL-specific constraint behavior, or migration ordering bugs. A database integration test that spins up a real PostgreSQL instance inside Docker closes this gap without requiring a persistent database on developer machines. In `procurement-platform-be`, the `@Testcontainers`-annotated test class manages the full container lifecycle — start, health-check, stop — through JUnit 5 lifecycle hooks. The adapter under test receives a `DataSource` configured to point at the ephemeral container rather than any static URL.

### Standard Library First

`java.sql.DriverManager` can open a connection to any JDBC URL — but you manage container startup, health-check polling, and teardown manually outside the test:

```java
// Standard library: raw JDBC connection to a pre-running test database
// Demonstrates the manual approach that Testcontainers supersedes.

import java.sql.Connection;
// => Connection: JDBC connection — must be closed after use or connection pool leaks
import java.sql.DriverManager;
// => DriverManager: JDBC entry point — finds a driver matching the URL scheme
import java.sql.SQLException;
// => SQLException: checked exception on every JDBC operation — callers must handle or declare

public class ManualJdbcSmokeTest {
    public static void main(String[] args) throws SQLException {
        // => Assumes the database is already running on localhost:5432 — manual setup required
        // => If the database is not ready, DriverManager throws immediately — no health-check polling
        String url = System.getenv("TEST_DATABASE_URL");
        // => Reads the JDBC URL from an environment variable — CI must set this before the test runs
        // => No automatic container provisioning — the database must be started in a separate step
        try (Connection conn = DriverManager.getConnection(url, "test_user", "test_pass")) {
            // => try-with-resources: Connection implements AutoCloseable — conn.close() is guaranteed
            var stmt = conn.createStatement();
            // => createStatement: plain statement, no parameters — suitable for smoke queries only
            var rs = stmt.executeQuery("SELECT 1");
            // => SELECT 1: minimal smoke query — verifies the connection is live
            // => No schema migration here: the developer must run migrations manually before this test
            if (rs.next()) {
                System.out.println("Connected: " + rs.getInt(1));
                // => Diagnostic output only — this is not an assertion in a test framework
            }
        }
    }
}
```

**Limitation for production**: raw JDBC requires a running database before the test starts, manual health-check polling, and manual teardown. Container startup is not coordinated with JUnit lifecycle hooks — if the JVM exits unexpectedly, the container is orphaned.

### Production Framework

Testcontainers integrates with JUnit 5 via `@Testcontainers` and `@Container`. The `PostgreSQLContainer` manages the full container lifecycle — start, wait for the PostgreSQL health probe, expose a random host port, and stop after the test class finishes:

```mermaid
flowchart LR
    jvm["JUnit 5\ntest runner"]:::blue
    tc["@Testcontainers\nextension"]:::orange
    pg["PostgreSQLContainer\n(postgres:17-alpine)"]:::teal
    probe["waitingFor:\nlog message\n'ready to accept'"]:::purple
    adapter["JdbcPurchaseOrderRepository\n(adapter under test)"]:::brown
    jvm -->|"@BeforeAll\nstarts"| tc
    tc -->|"container.start()"| pg
    pg -->|"health-check"| probe
    probe -->|"ready → exposes\nJDBC URL"| adapter
    adapter -->|"SQL via JDBC URL"| pg

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    classDef purple fill:#CC78BC,color:#fff,stroke:#CC78BC
    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
```

```java
// Testcontainers integration test for the JDBC adapter
package com.procurement.platform.purchasing.infrastructure;
// => infrastructure package: integration tests live here — they test the adapter, not the domain

import com.procurement.platform.purchasing.application.PurchaseOrderRepository;
// => Application-layer port — the test exercises the adapter through the interface
import com.procurement.platform.purchasing.domain.PurchaseOrder;
import com.procurement.platform.purchasing.domain.PurchaseOrderId;
import com.procurement.platform.purchasing.domain.SupplierId;
import com.procurement.platform.purchasing.domain.Money;
import com.procurement.platform.purchasing.domain.ApprovalLevel;
import com.procurement.platform.purchasing.domain.PurchaseOrderStatus;
// => Domain types only — the test speaks in domain terms, not JDBC terms
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
// => JUnit 5: @Test marks test methods; @BeforeEach runs before each test method
import org.springframework.jdbc.datasource.DriverManagerDataSource;
// => DriverManagerDataSource: Spring's simple DataSource — wires the container JDBC URL
import org.testcontainers.containers.PostgreSQLContainer;
// => PostgreSQLContainer: the Testcontainers wrapper for postgres Docker images
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
// => @Testcontainers: JUnit 5 extension — manages @Container lifecycle automatically

import javax.sql.DataSource;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

@Testcontainers
// => @Testcontainers: registers the JUnit 5 extension that starts @Container fields
// => The extension calls container.start() before any test in this class and container.stop() after all
public class JdbcPurchaseOrderRepositoryIntegrationTest {

    @Container
    // => @Container: Testcontainers manages lifecycle — container starts before @BeforeEach, stops after @AfterAll
    // => static field: container is shared across all test methods in this class — one start, one stop
    static final PostgreSQLContainer<?> postgres =
        new PostgreSQLContainer<>("postgres:17-alpine");
        // => postgres:17-alpine: matches the production target — Alpine keeps the image small
        // => waitingFor defaults to waiting for the "ready to accept connections" log message

    private PurchaseOrderRepository repository;
    // => Port interface declared — the test never imports JdbcPurchaseOrderRepository directly
    // => Swapping the adapter requires no change to the test body

    @BeforeEach
    // => @BeforeEach: runs before each @Test method — creates a fresh adapter backed by the container
    void setUp() {
        DataSource dataSource = new DriverManagerDataSource(
            postgres.getJdbcUrl(),
            // => getJdbcUrl(): returns "jdbc:postgresql://localhost:<random-port>/test"
            // => Random port assigned by Docker — no port conflicts on CI runners
            postgres.getUsername(),
            postgres.getPassword()
            // => Default credentials: "test" / "test" — the default PostgreSQLContainer credentials
        );
        repository = new JdbcPurchaseOrderRepository(
            org.springframework.jdbc.core.simple.JdbcClient.create(dataSource));
        // => JdbcPurchaseOrderRepository: the infrastructure adapter under test
        // => Constructed fresh before each test — no state carries between tests
    }

    @Test
    // => @Test: JUnit 5 test method — discovered by the JUnit Platform and executed by the engine
    void save_thenFindById_roundTripsSuccessfully() {
        // => Test name describes the observable contract — save then find returns the same aggregate
        var id = new PurchaseOrderId(UUID.randomUUID());
        // => PurchaseOrderId: strongly-typed identity — wraps a random UUID for this test run
        var supplierId = new SupplierId(UUID.randomUUID());
        // => SupplierId: strongly-typed supplier identity — wraps a random UUID
        var po = new PurchaseOrder(id, supplierId, List.of(),
            new Money(new java.math.BigDecimal("500.00"), "USD"),
            ApprovalLevel.L1, PurchaseOrderStatus.Draft);
        // => Domain aggregate: built with the smart constructor — invariants validated at construction

        repository.save(po);
        // => Write path: persists the aggregate via JdbcPurchaseOrderRepository to the real PostgreSQL container

        Optional<PurchaseOrder> found = repository.findById(id);
        // => Read path: queries the real database — finds the row committed by save()

        assertTrue(found.isPresent(), "PurchaseOrder must be found after save");
        // => isPresent(): the row must exist — if the INSERT failed silently, this assertion fails
        assertEquals(po.supplierId(), found.get().supplierId());
        // => supplierId round-trip: the persisted supplier identity must match the domain record's value
        assertEquals(PurchaseOrderStatus.Draft, found.get().status());
        // => Status round-trip: verifies the enum column is mapped and retrieved correctly
    }

    @Test
    // => Second test: exercises the not-found path — no setup, no prior save
    void findById_returnsEmpty_whenNotFound() {
        var missingId = new PurchaseOrderId(UUID.randomUUID());
        // => A UUID that was never saved — the database has no row for this identity

        Optional<PurchaseOrder> result = repository.findById(missingId);
        // => Port contract: absence must be returned as Optional.empty(), never null

        assertTrue(result.isEmpty(), "Unknown PurchaseOrderId must return Optional.empty()");
    }
}
```

Add the Testcontainers dependency to the `pom.xml` for `procurement-platform-be`:

```xml
<!-- Testcontainers BOM import — manage all testcontainers module versions consistently -->
<dependency>
    <!-- => <dependency> in <dependencyManagement>: pins the version for all transitive consumers -->
    <groupId>org.testcontainers</groupId>
    <artifactId>testcontainers-bom</artifactId>
    <!-- => Spring Boot 4.0.6 BOM does NOT manage org.testcontainers core modules — explicit BOM import required -->
    <version>1.21.3</version>
    <type>pom</type>
    <scope>import</scope>
</dependency>

<!-- Then in <dependencies>: -->
<dependency>
    <groupId>org.testcontainers</groupId>
    <!-- => postgresql module: wraps postgres Docker image, exposes JDBC URL -->
    <artifactId>postgresql</artifactId>
    <!-- => Version managed by org.testcontainers:testcontainers-bom — no explicit version needed here -->
    <scope>test</scope>
</dependency>
<dependency>
    <groupId>org.testcontainers</groupId>
    <!-- => junit-jupiter: provides @Testcontainers and @Container annotations -->
    <artifactId>junit-jupiter</artifactId>
    <!-- => Without this module, the JUnit 5 lifecycle extension is not registered automatically -->
    <scope>test</scope>
</dependency>
```

**Trade-offs**: Testcontainers tests are slower than in-memory tests — PostgreSQL startup typically adds 5–15 seconds. They require Docker on the developer machine and CI runner. They are not cacheable by Nx because the external container is non-deterministic. Run them on the `test:integration` Nx target, not `test:quick`. The payoff is that they catch schema drift, PostgreSQL-specific constraint behavior, and migration bugs that no in-memory stub can surface.

---

## Guide 17 — Schema Migration Adapter with Flyway

### Why It Matters

Every database integration test and every production deployment depends on the schema matching the application's expectations. Without a migration tool, schema changes require manual SQL execution coordinated across every developer machine, CI runner, and production server. In `procurement-platform-be`, Flyway runs embedded SQL migration scripts in versioned order at application startup. The migration adapter is a first-class hexagonal concern: it runs before any domain port is called, and the Testcontainers integration test (Guide 16) can invoke it against the fresh container database before running assertions.

### Standard Library First

`java.io` and plain JDBC can execute SQL files in order — but you manage ordering, idempotency, and error recovery manually:

```java
// Standard library: manual SQL file execution without a migration library
// Demonstrates the raw JDBC migration approach that the Flyway adapter supersedes.

import java.io.IOException;
// => IOException: thrown by Files.readString if the file does not exist or cannot be read
import java.nio.file.Files;
// => Files: NIO utility class — readString reads a file's entire content in one call
import java.nio.file.Path;
// => Files.readString: reads a .sql file from the filesystem — no classpath scanning
import java.sql.Connection;
// => Connection: JDBC connection — must be provided by the caller; not managed here
import java.sql.SQLException;
// => SQLException: thrown by createStatement().execute() on any SQL error

public class ManualMigrationRunner {
    public static void runMigration(Connection conn, Path sqlFile)
            throws IOException, SQLException {
        // => Two checked exceptions declared: I/O for file reading, SQL for execution
        // => No ordering enforcement — the caller must sort files by name manually
        // => No version tracking: impossible to determine which migrations have already run
        String sql = Files.readString(sqlFile);
        // => Reads the entire SQL file as a string — no templating, no parameter binding
        // => Large migration files are read entirely into memory — no streaming
        try (var stmt = conn.createStatement()) {
            // => try-with-resources: Statement implements AutoCloseable — closed after execute
            stmt.execute(sql);
            // => execute: runs the entire file as one batch — DDL errors mid-file leave partial schema
            // => No journal table: if the script runs twice, CREATE TABLE throws a duplicate-object error
            // => No transaction wrapping: if stmt.execute throws partway through, schema is partially applied
        }
    }
}
```

**Limitation for production**: no journal table means migrations run again on every restart. No ordering enforcement means naming conventions must be manually enforced. No embedded-resource support means SQL files must be on the filesystem at a known path.

### Production Framework

Flyway reads versioned SQL scripts from the classpath (`db/migration/V1__*.sql`, `V2__*.sql`, …), maintains an applied-scripts journal table (`flyway_schema_history`) in the database, and applies only unapplied scripts in order. Spring Boot 4 auto-configures Flyway when `spring-boot-starter-flyway` is on the classpath:

```java
// ApplicationRunner invoking Flyway migration at startup
package com.procurement.platform.shared.config;

import org.flywaydb.core.Flyway;
// => Flyway: the migration engine — configured with dataSource and migration locations
import org.flywaydb.core.api.output.MigrateResult;
// => MigrateResult: result record — carries migrationsExecuted count and success flag
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationRunner;
// => ApplicationRunner: Spring Boot hook — runs after ApplicationContext is ready
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.sql.DataSource;

@Configuration
public class MigrationConfiguration {

    private static final Logger log = LoggerFactory.getLogger(MigrationConfiguration.class);

    @Bean
    public ApplicationRunner migrationRunner(DataSource dataSource) {
        // => DataSource: injected from the Spring context — backed by HikariCP in production
        return args -> {
            // => ApplicationRunner lambda — runs once at startup, before any HTTP request is served
            Flyway flyway = Flyway.configure()
                .dataSource(dataSource)
                // => dataSource: Flyway uses the same pool as the application — no second connection pool
                .locations("classpath:db/migration")
                // => locations: Flyway scans src/main/resources/db/migration for V*.sql files
                // => Scripts named V1__create_purchasing_schema.sql, V2__create_supplier_schema.sql, etc.
                .load();
            MigrateResult result = flyway.migrate();
            // => migrate(): applies all unapplied scripts in version order within individual transactions
            // => Flyway wraps each script in a transaction — a failed script leaves no partial schema
            log.info("Flyway applied {} migration(s)", result.migrationsExecuted);
            if (!result.success) {
                throw new IllegalStateException("Flyway migration failed — aborting startup");
                // => Throwing here causes Spring Boot to exit with a non-zero code
                // => Kubernetes readiness probe fails — pod does not receive traffic before schema is ready
            }
        };
    }
}
```

The migration SQL script follows Flyway's naming scheme and uses one schema per bounded context:

```sql
-- src/main/resources/db/migration/V1__create_purchasing_schema.sql
-- Creates the purchasing bounded context schema

CREATE SCHEMA IF NOT EXISTS purchasing;
-- => purchasing schema: isolates purchasing context tables from supplier and receiving schemas
-- => One schema per bounded context: prevents accidental cross-context table joins in raw SQL

CREATE TABLE IF NOT EXISTS purchasing.purchase_orders (
    -- => purchasing.purchase_orders: one table per aggregate in the purchasing context
    id             UUID         PRIMARY KEY,
    -- => UUID primary key: matches PurchaseOrderId.value() — no auto-increment sequences needed
    supplier_id    UUID         NOT NULL,
    -- => supplier_id: foreign reference to the supplier — UUID only, no FK to a suppliers table
    total_amount   NUMERIC(19,4) NOT NULL,
    -- => NUMERIC(19,4): sufficient precision for monetary amounts — 4 decimal places
    currency       CHAR(3)      NOT NULL,
    -- => CHAR(3): ISO 4217 currency code — "USD", "EUR", "IDR"
    approval_level TEXT         NOT NULL,
    -- => approval_level: stores the ApprovalLevel enum name: "L1", "L2", "L3"
    status         TEXT         NOT NULL DEFAULT 'Draft'
    -- => DEFAULT 'Draft': new rows start as Draft — matches the domain aggregate's initial state
);

CREATE INDEX IF NOT EXISTS po_supplier_id_idx ON purchasing.purchase_orders(supplier_id);
-- => Index by supplier_id: findBySupplierId() is a hot read path — makes it O(log n)

CREATE TABLE IF NOT EXISTS purchasing.outbox_events (
    -- => outbox_events: one outbox table per bounded context schema
    id           TEXT         PRIMARY KEY,
    -- => id: idempotency key generated by OutboxEventPublisher — UUID string form
    event_type   TEXT         NOT NULL,
    -- => event_type: string name of the domain event class — used by the relay worker for routing
    payload      JSONB        NOT NULL,
    -- => JSONB: binary JSON format — enables GIN index on payload fields for relay filtering
    created_at   TIMESTAMPTZ  NOT NULL,
    processed_at TIMESTAMPTZ
    -- => NULL until relayed: the relay worker sets this when the event has been delivered
);
```

Add the Flyway dependency to `pom.xml`:

```xml
<!-- Flyway starter — triggers FlywayAutoConfiguration in Spring Boot 4 -->
<dependency>
    <groupId>org.springframework.boot</groupId>
    <!-- => required in Spring Boot 4: standalone flyway-core no longer triggers FlywayAutoConfiguration -->
    <artifactId>spring-boot-starter-flyway</artifactId>
</dependency>
<dependency>
    <groupId>org.flywaydb</groupId>
    <!-- => PostgreSQL dialect module: required for PG-specific DDL and schema support -->
    <artifactId>flyway-database-postgresql</artifactId>
    <!-- => Without this, Flyway falls back to a generic JDBC dialect -->
</dependency>
```

**Trade-offs**: Flyway's versioned migration model requires naming discipline (`V1__`, `V2__`) — a mislabeled script that should run after `V10__` but is named `V2__` runs second and breaks. For `procurement-platform-be`, Flyway's embedded SQL approach keeps the migration language as plain SQL, which is more portable and easier to review in pull requests.

---

## Guide 18 — Banking Port + Spring `RestClient` Adapter

### Why It Matters

External bank API calls are an I/O boundary: the payments context sends a disbursement instruction and receives a confirmation from the bank's REST API. Like the database boundary, this I/O must sit behind a port so the application service is testable without a live banking API, and so the provider can be swapped without touching business logic. In `procurement-platform-be`, the `payments` bounded context introduces a `BankingPort` interface in its `application` package. Spring Boot 4 ships `RestClient` — already on the classpath via `spring-boot-starter-web` — which provides type-safe, builder-configured HTTP calls with timeout control.

### Standard Library First

`java.net.http.HttpClient` (JDK 11+) can call any HTTP endpoint without any Spring dependency. You manage timeout configuration, error discrimination, and JSON mapping manually:

```java
// Standard library: java.net.http.HttpClient calling a bank disbursement endpoint
// Demonstrates the stdlib HttpClient approach that the Spring RestClient adapter supersedes.

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
// => java.net.http: JDK 11+ — no external dependency, full HTTP/2 support
import java.time.Duration;
// => Duration: used for connect and request timeouts — no third-party type needed

public class RawBankHttpClient {
    private final HttpClient httpClient = HttpClient.newBuilder()
        .connectTimeout(Duration.ofSeconds(5))
        // => connectTimeout: how long to wait for the TCP handshake — no retry on timeout
        .build();

    public String initiateDisbursement(String apiKey, String iban, String amount, String currency)
            throws Exception {
        // => checked Exception: no typed error discrimination between insufficient-funds, auth failure, or timeout
        String body = """
            {"iban":"%s","amount":"%s","currency":"%s"}
            """.formatted(iban, amount, currency);
        // => JSON string built manually — no type safety on field names
        var request = HttpRequest.newBuilder()
            .uri(URI.create("https://bank-api.example.com/v1/disbursements"))
            // => Hardcoded URL: the base URL is not externalized — changing providers requires a code change
            .header("Authorization", "Bearer " + apiKey)
            .header("Content-Type", "application/json")
            .POST(HttpRequest.BodyPublishers.ofString(body))
            .timeout(Duration.ofSeconds(30))
            .build();
        var response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() != 200) {
            throw new Exception("Bank API call failed: " + response.statusCode());
            // => Undifferentiated exception: insufficient-funds (422) and auth failure (401) both throw the same type
        }
        return response.body();
        // => Returns the raw JSON body string — caller must parse it manually with Jackson
    }
}
```

**Limitation for production**: no typed error discrimination between insufficient-funds errors, authentication failures, and server errors. No retry logic. The application layer must import `HttpClient` to call this function — the banking boundary is not behind a port.

### Production Framework

The hexagonal approach declares a `BankingPort` in the `payments` application package and implements the HTTP adapter in infrastructure using Spring `RestClient`:

```java
// BankingPort.java — output port interface in the payments application package
package com.procurement.platform.payments.application;
// => application/ package: port interfaces live here — no Spring, no HTTP imports

import com.procurement.platform.payments.domain.Payment;
// => Payment: the domain aggregate — passed to initiateDisbursement for bank instructions
import com.procurement.platform.payments.domain.DisbursementConfirmation;
// => DisbursementConfirmation: value object wrapping the bank confirmation reference

public interface BankingPort {
    // => Output port: declares what the application needs from a bank API
    // => No mention of RestClient or HTTP — those are adapter concerns

    DisbursementConfirmation initiateDisbursement(Payment payment) throws BankingException;
    // => initiateDisbursement: called during a payment run — submits disbursement to the bank
    // => BankingException: typed checked exception — the caller can distinguish bank API failure

    boolean confirmDisbursement(String bankReference) throws BankingException;
    // => confirmDisbursement: polls for settlement status — returns true when bank confirms settlement
    // => Throws BankingException on connection failure, not on "pending" status
}

// BankingException.java — typed exception in the application package
class BankingException extends Exception {
    // => Checked exception: callers must handle or declare it — no silent swallowing
    public BankingException(String message, Throwable cause) {
        super(message, cause);
        // => Wraps the underlying RestClient exception with a typed domain-layer exception
    }
}
```

```java
// RestClientBankingAdapter.java — RestClient adapter implementing BankingPort
package com.procurement.platform.payments.infrastructure;
// => payments/infrastructure/ package: the RestClient adapter lives here

import com.procurement.platform.payments.application.BankingPort;
import com.procurement.platform.payments.application.BankingException;
import com.procurement.platform.payments.domain.Payment;
import com.procurement.platform.payments.domain.DisbursementConfirmation;
// => Domain value objects: Payment carries the input, DisbursementConfirmation carries the output
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestClientException;
// => RestClient: Spring Boot 4 fluent HTTP client — replaces RestTemplate for new code
import org.springframework.stereotype.Component;
import java.time.Duration;

@Component
public class RestClientBankingAdapter implements BankingPort {
    // => implements BankingPort: the adapter satisfies the output port contract

    private final RestClient restClient;
    // => RestClient: Spring Boot 4's modern HTTP client — builder-configured at construction
    private final String bankApiKey;
    // => apiKey: externalized in application.properties — never hardcoded

    public RestClientBankingAdapter(
            RestClient.Builder restClientBuilder,
            // => RestClient.Builder: Spring Boot 4 auto-configures one per application context
            @org.springframework.beans.factory.annotation.Value("${banking.api.base-url}")
            // => @Value: injects the property value at construction time — reads from application.properties
            String baseUrl,
            @org.springframework.beans.factory.annotation.Value("${banking.api.key}")
            // => @Value: injected from the Kubernetes Secret via BANKING_API_KEY environment variable
            String bankApiKey
    ) {
        this.restClient = restClientBuilder
            .baseUrl(baseUrl)
            // => baseUrl: all requests from this client use this prefix
            // => Externalised via BANKING_API_BASE_URL env var in ConfigMap — no hardcoded URLs
            .defaultHeader("Authorization", "Bearer " + bankApiKey)
            // => defaultHeader: the Authorization header is set once — not repeated per request
            // => Bearer token: the bank API uses OAuth2 bearer token authentication
            .requestFactory(factory -> factory.setConnectTimeout(Duration.ofSeconds(5)))
            // => connectTimeout: TCP handshake must complete within 5 seconds
            // => No read timeout configured here — Guide 19 Resilience4j adds the overall deadline
            .build();
        this.bankApiKey = bankApiKey;
        // => bankApiKey stored: used for logging or re-authentication if needed in future extensions
    }

    @Override
    public DisbursementConfirmation initiateDisbursement(Payment payment) throws BankingException {
        // => Implements BankingPort.initiateDisbursement — called by the payments application service
        try {
            var requestBody = new DisbursementRequest(
                payment.bankAccount().iban(),
                // => iban: extracted from the BankAccount value object — format-validated in the domain
                payment.amount().amount().toPlainString(),
                // => toPlainString(): avoids scientific notation — "1234.56" not "1.23456E3"
                payment.amount().currency()
                // => currency: ISO 4217 code from the Money value object — "USD", "EUR", "IDR"
            );
            var response = restClient.post()
                // => post(): opens an HTTP POST request builder
                .uri("/v1/disbursements")
                // => uri: relative path — resolved against the base URL configured at construction
                .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                // => contentType: sets Content-Type: application/json — required by the bank API
                .body(requestBody)
                // => body: Jackson serialises DisbursementRequest to JSON automatically
                .retrieve()
                // => retrieve(): executes the request and checks the HTTP status code
                .body(DisbursementResponse.class);
            // => body(Class): deserializes the response JSON into DisbursementResponse — Jackson mapping
            return new DisbursementConfirmation(response.reference(), response.status());
            // => DisbursementConfirmation: immutable value object returned to the application service
        } catch (RestClientException ex) {
            throw new BankingException("Bank disbursement API call failed: " + ex.getMessage(), ex);
            // => Wrap in BankingException to keep the application layer free of Spring imports
            // => RestClientException hierarchy: non-2xx responses throw HttpStatusCodeException
        }
    }

    @Override
    public boolean confirmDisbursement(String bankReference) throws BankingException {
        try {
            var response = restClient.get()
                // => get(): opens an HTTP GET request builder
                .uri("/v1/disbursements/{reference}", bankReference)
                // => URI template: bankReference is URL-encoded by RestClient automatically
                .retrieve()
                // => retrieve(): executes the GET and asserts 2xx — 404 throws RestClientException
                .body(DisbursementStatusResponse.class);
            // => body(Class): Jackson deserialises the bank's status response JSON
            return "SETTLED".equalsIgnoreCase(response.status());
            // => Returns true only when status is SETTLED — "PENDING" and "PROCESSING" return false
        } catch (RestClientException ex) {
            throw new BankingException("Bank confirmation API call failed: " + ex.getMessage(), ex);
            // => BankingException: the application layer catches this typed exception — no Spring leakage
        }
    }

    // Response records — private to the adapter, not exposed to the application layer
    private record DisbursementRequest(String iban, String amount, String currency) {}
    // => Jackson serializes this to {"iban":"...","amount":"...","currency":"..."}
    private record DisbursementResponse(String reference, String status) {}
    // => Jackson deserializes the bank API response — reference is the idempotency key
    private record DisbursementStatusResponse(String status) {}
    // => Status string: "PENDING", "PROCESSING", "SETTLED", "FAILED"
}
```

**Trade-offs**: `RestClient` requires Spring MVC on the classpath — which is always present for `spring-boot-starter-web`. The typed response records (private to the adapter) couple the adapter to the bank API's JSON schema — if the bank changes its response shape, only the adapter changes; the application service and port are untouched.

---

## Guide 19 — Retry + Circuit-Breaker via Resilience4j

### Why It Matters

External ports — the banking adapter from Guide 18, the JDBC adapter from Guide 8 — fail transiently. A timeout does not mean the downstream service is permanently unavailable; a retry after a brief pause often succeeds. Conversely, an adapter that retries indefinitely against a service that is genuinely down floods the downstream with traffic and keeps threads occupied. Resilience4j (the Spring Boot 4 resilience starter) wraps port adapters with configurable retry and circuit-breaker policies via a decorator pattern over the port interface. The application service code is unchanged — the decorator is wired in the composition root.

### Standard Library First

`java.util.concurrent` provides `CompletableFuture` and thread pools for async retry — but you write the retry loop, backoff calculation, and open-circuit logic yourself:

```java
// Standard library: manual retry loop with exponential backoff
// Demonstrates the manual retry approach that Resilience4j supersedes.

import java.util.function.Supplier;
// => Supplier<T>: functional interface — wraps the call that may throw

public class ManualRetry {
    public static <T> T withRetry(Supplier<T> supplier, int maxAttempts, long initialDelayMs)
            throws Exception {
        Exception lastException = null;
        for (int attempt = 0; attempt < maxAttempts; attempt++) {
            // => Linear retry loop: no built-in jitter, no backoff cap, no circuit-breaker state
            try {
                return supplier.get();
            } catch (Exception ex) {
                lastException = ex;
                if (attempt < maxAttempts - 1) {
                    long delay = initialDelayMs * (long) Math.pow(2, attempt);
                    // => Exponential backoff: 100ms, 200ms, 400ms — no jitter
                    Thread.sleep(delay);
                    // => Thread.sleep: blocks the calling thread — no async option without CompletableFuture
                }
            }
        }
        throw lastException;
        // => All attempts exhausted — rethrow the last exception
        // => No circuit-breaker: the retry loop always tries, even if the last 100 calls failed
    }
}
```

**Limitation for production**: no circuit-breaker state — the retry loop hammers a down service on every call. No jitter — simultaneous callers retry in lock-step. No metrics integration — failures are not counted for observability.

### Production Framework

Resilience4j provides `Retry` and `CircuitBreaker` decorators that wrap any `Supplier`, `Function`, or `Callable`. The decorator is applied in the composition root `@Configuration` class — the application service constructor receives a `PurchaseOrderRepository` that already has retry and circuit-breaker wiring applied:

```java
// ResilientPurchaseOrderRepository.java — Resilience4j decorator wrapping the output port
package com.procurement.platform.purchasing.infrastructure;

import com.procurement.platform.purchasing.application.PurchaseOrderRepository;
// => Port interface: the decorator implements the same interface as the underlying adapter
import com.procurement.platform.purchasing.domain.PurchaseOrder;
import com.procurement.platform.purchasing.domain.PurchaseOrderId;
// => Domain types only — the decorator speaks in domain terms, delegates to the real adapter
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
// => CircuitBreaker: state machine — CLOSED, OPEN, HALF_OPEN — stops calls when open
import io.github.resilience4j.retry.Retry;
// => Retry: configures max attempts, wait duration, and which exceptions trigger a retry
import io.github.resilience4j.retry.RetryConfig;
import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import java.time.Duration;
import java.util.Optional;

public class ResilientPurchaseOrderRepository implements PurchaseOrderRepository {
    // => Implements PurchaseOrderRepository: the decorator is a drop-in replacement for the adapter
    // => The application service cannot tell whether it has the raw adapter or the decorator

    private final PurchaseOrderRepository delegate;
    // => delegate: the real adapter (JdbcPurchaseOrderRepository) — receives calls after retry/CB evaluation
    private final Retry retry;
    private final CircuitBreaker circuitBreaker;

    public ResilientPurchaseOrderRepository(PurchaseOrderRepository delegate) {
        this.delegate = delegate;
        this.retry = Retry.of(
            "purchase-order-repository",
            RetryConfig.custom()
                .maxAttempts(3)
                // => maxAttempts: 3 — the initial call plus two retries
                .waitDuration(Duration.ofMillis(200))
                // => waitDuration: 200ms between attempts — avoid hammering a recovering database
                .retryOnException(ex -> ex instanceof java.sql.SQLException)
                // => retryOnException: only retry on SQLExceptions — not on domain exceptions
                .build()
        );
        this.circuitBreaker = CircuitBreaker.of(
            "purchase-order-repository",
            CircuitBreakerConfig.custom()
                .failureRateThreshold(50)
                // => failureRateThreshold: 50% failure rate in the sliding window opens the circuit
                .slidingWindowSize(10)
                // => slidingWindowSize: 10 calls — the CB evaluates the last 10 attempts
                .waitDurationInOpenState(Duration.ofSeconds(30))
                // => waitDurationInOpenState: CB stays open for 30 seconds before transitioning to HALF_OPEN
                .build()
        );
    }

    @Override
    public PurchaseOrder save(PurchaseOrder po) {
        // => Wraps save() with retry + circuit-breaker: transient DB failures are retried
        return Retry.decorateSupplier(retry,
            CircuitBreaker.decorateSupplier(circuitBreaker,
                () -> delegate.save(po)
                // => Lambda delegates to the real adapter — retry fires if the lambda throws
            )
        ).get();
    }

    @Override
    public Optional<PurchaseOrder> findById(PurchaseOrderId id) {
        return Retry.decorateSupplier(retry,
            CircuitBreaker.decorateSupplier(circuitBreaker,
                () -> delegate.findById(id)
                // => findById is also retried: transient DB timeouts on reads benefit from retry
                // => CircuitBreaker counts read failures too — opens circuit if DB is fully down
            )
        ).get();
        // => .get(): executes the decorated supplier — Retry and CircuitBreaker apply transparently
    }

    @Override
    public boolean existsById(PurchaseOrderId id) {
        return Retry.decorateSupplier(retry,
            CircuitBreaker.decorateSupplier(circuitBreaker,
                () -> delegate.existsById(id)
                // => existsById retried: the existence check uses a lightweight COUNT query — worth retrying
            )
        ).get();
        // => .get(): returns the boolean result from delegate.existsById() after retry + CB evaluation
    }
}
```

The composition root wires the decorator transparently:

```java
// PurchasingContextConfiguration.java — composition root wiring with resilience decorator

@Bean
public PurchaseOrderRepository purchaseOrderRepository(JdbcClient jdbc) {
    var jdbcAdapter = new JdbcPurchaseOrderRepository(jdbc);
    // => JdbcPurchaseOrderRepository: the real database adapter — receives SQL calls
    return new ResilientPurchaseOrderRepository(jdbcAdapter);
    // => ResilientPurchaseOrderRepository: decorator wrapping the adapter with retry + circuit-breaker
    // => The application service injects PurchaseOrderRepository — it never sees either concrete class
}
```

Add the Resilience4j Spring Boot starter to `pom.xml`:

```xml
<!-- Resilience4j Spring Boot 4 starter -->
<dependency>
    <groupId>io.github.resilience4j</groupId>
    <artifactId>resilience4j-spring-boot4</artifactId>
    <!-- => resilience4j-spring-boot4: dedicated Spring Boot 4 starter — published to Maven Central as of v2.4.0 -->
    <version>2.4.0</version>
    <!-- => 2.4.0: explicit pin — Spring Boot 4 BOM does not manage Resilience4j transitively -->
</dependency>
```

**Trade-offs**: the decorator pattern adds two layers of wrapping over every port method call. For high-frequency read operations, the decorator overhead is measurable — profile before adding circuit-breakers to read paths. The payoff is that every port adapter automatically participates in the resilience policy without modifying the application service.

---

## Guide 20 — Observability Adapter via Micrometer Tracing

### Why It Matters

A hexagonal application whose port calls are not traced is a black box in production. When a purchasing manager reports that the PO issuance endpoint is slow, the only way to diagnose it without tracing is to reproduce the slowness in development — expensive and error-prone. Micrometer Tracing (the observability layer in Spring Boot 4) wraps port calls with OpenTelemetry-compatible spans. The observability adapter follows the same decorator pattern as the resilience decorator in Guide 19: it implements the port interface, wraps each method with a span, delegates to the real adapter, and is wired transparently in the composition root.

### Standard Library First

`java.lang.System.nanoTime()` can measure wall-clock duration — but it gives you no distributed trace context, no parent-child span relationship, and no integration with any observability backend:

```java
// Standard library: manual timing with System.nanoTime()
// Demonstrates the manual timing approach that Micrometer Tracing supersedes.

public class ManualTimingExample {
    // => Illustrative class: wraps a PurchaseOrderRepository with manual nanoTime-based timing
    public void save(PurchaseOrder po) {
        long start = System.nanoTime();
        // => nanoTime(): monotonic clock — suitable for elapsed time, not wall-clock time
        // => No trace context: this duration cannot be correlated with upstream HTTP spans
        try {
            delegate.save(po);
        } finally {
            long elapsed = System.nanoTime() - start;
            System.out.printf("save() took %.2f ms%n", elapsed / 1_000_000.0);
            // => Console output: no structured format, no trace ID, no span ID
            // => No backend integration — the duration is not exported to Jaeger or Grafana Tempo
        }
    }
}
```

**Limitation for production**: no distributed trace context — spans from different services cannot be stitched into a single trace. No parent-child relationship. No backend integration.

### Production Framework

Micrometer Tracing with the OpenTelemetry bridge wraps port calls with spans that carry a trace ID and are exported to the configured backend. The decorator pattern keeps the tracing concern entirely in the infrastructure layer:

```java
// TracedPurchaseOrderRepository.java — Micrometer Tracing decorator wrapping the output port
package com.procurement.platform.purchasing.infrastructure;

import com.procurement.platform.purchasing.application.PurchaseOrderRepository;
// => Port interface: the decorator implements the same interface — application service is unaware of tracing
import com.procurement.platform.purchasing.domain.PurchaseOrder;
import com.procurement.platform.purchasing.domain.PurchaseOrderId;
import io.micrometer.tracing.Tracer;
// => Tracer: Micrometer Tracing API — creates spans and manages the trace context
import io.micrometer.tracing.Span;
// => Span: represents a single unit of work — carries trace ID, span ID, and tags
import java.util.Optional;

public class TracedPurchaseOrderRepository implements PurchaseOrderRepository {
    // => Implements PurchaseOrderRepository: transparent decorator
    // => Stacks with ResilientPurchaseOrderRepository: wire as Traced(Resilient(Jdbc(...)))

    private final PurchaseOrderRepository delegate;
    private final Tracer tracer;

    public TracedPurchaseOrderRepository(PurchaseOrderRepository delegate, Tracer tracer) {
        this.delegate = delegate;
        this.tracer = tracer;
        // => Both fields are final: immutable after construction — thread-safe for concurrent requests
    }

    @Override
    public PurchaseOrder save(PurchaseOrder po) {
        Span span = tracer.nextSpan().name("purchase-order-repository.save").start();
        // => nextSpan(): creates a child span if a parent trace context is active
        // => name: span name visible in Jaeger / Zipkin — describes the operation
        try (var ignored = tracer.withSpan(span)) {
            span.tag("purchase_order.id", po.id().value().toString());
            // => tag: adds a key-value attribute to the span — visible in trace viewers
            return delegate.save(po);
        } catch (Exception ex) {
            span.error(ex);
            // => error: marks the span as failed and records the exception
            throw ex;
        } finally {
            span.end();
            // => end(): closes the span and reports it to the OpenTelemetry exporter
        }
    }

    @Override
    public Optional<PurchaseOrder> findById(PurchaseOrderId id) {
        Span span = tracer.nextSpan().name("purchase-order-repository.findById").start();
        // => nextSpan(): creates a child span under the current trace if one is active
        try (var ignored = tracer.withSpan(span)) {
            // => withSpan: sets the active span on the current thread for the duration of the try block
            span.tag("purchase_order.id", id.value().toString());
            // => Tag the queried ID: lets operators filter traces for a specific PO
            return delegate.findById(id);
            // => Delegates to the resilient adapter: retry + circuit-breaker apply before the JDBC call
        } catch (Exception ex) {
            span.error(ex);
            // => error(): marks the span as failed and records the exception — visible in Jaeger
            throw ex;
        } finally {
            span.end();
            // => end(): closes the span and flushes it to the OTLP exporter — always called via finally
        }
    }

    @Override
    public boolean existsById(PurchaseOrderId id) {
        Span span = tracer.nextSpan().name("purchase-order-repository.existsById").start();
        // => Span name: "purchase-order-repository.existsById" — distinguishes it from save and findById
        try (var ignored = tracer.withSpan(span)) {
            span.tag("purchase_order.id", id.value().toString());
            // => Tag the checked ID: trace viewers can filter all existsById calls for a given PO
            return delegate.existsById(id);
            // => Delegates to the resilient adapter — the existence check benefits from retry too
        } catch (Exception ex) {
            span.error(ex);
            // => error(): marks the span as errored — the circuit-breaker counts this failure
            throw ex;
        } finally {
            span.end();
            // => end(): span always closed — no leak even when the delegate throws
        }
    }
}
```

The composition root stacks all three decorators in order:

```java
// PurchasingContextConfiguration.java — stacked decorators in the composition root

@Bean
public PurchaseOrderRepository purchaseOrderRepository(JdbcClient jdbc, Tracer tracer) {
    // => Parameters injected by Spring: JdbcClient from auto-config, Tracer from Micrometer auto-config
    var jdbcAdapter = new JdbcPurchaseOrderRepository(jdbc);
    // => Layer 1: the real adapter — performs SQL operations against the PostgreSQL DataSource
    var resilientAdapter = new ResilientPurchaseOrderRepository(jdbcAdapter);
    // => Layer 2: retry + circuit-breaker — retries transient failures, opens circuit on sustained failure
    return new TracedPurchaseOrderRepository(resilientAdapter, tracer);
    // => Layer 3: tracing — wraps the full resilience + adapter stack with a span
    // => Span duration includes retry wait time — visible in trace timelines
    // => Application service injects PurchaseOrderRepository — sees none of the concrete layers
}
```

Add Micrometer Tracing to `pom.xml`:

```xml
<!-- Micrometer Tracing + OpenTelemetry bridge -->
<dependency>
    <groupId>io.micrometer</groupId>
    <!-- => OTel bridge: translates Micrometer Tracing to OTel spans -->
    <artifactId>micrometer-tracing-bridge-otel</artifactId>
    <!-- => Version managed by spring-boot-starter-parent 4.0.6 BOM -->
</dependency>
<dependency>
    <groupId>io.opentelemetry</groupId>
    <!-- => OTLP exporter: sends spans to Collector, Jaeger, or Tempo -->
    <artifactId>opentelemetry-exporter-otlp</artifactId>
    <!-- => Configure the endpoint in application.properties: management.otlp.tracing.endpoint -->
</dependency>
```

**Trade-offs**: the decorator stack — trace → resilience → JDBC adapter — adds method-call overhead for every port operation. For high-frequency read paths under strict latency constraints, consider sampling via `management.tracing.sampling.probability=0.01` in `application.properties`. The payoff is full distributed trace visibility across services with no changes to the application service or domain layer.

---

## Guide 21 — Domain Event Flow End-to-End

### Why It Matters

Guides 10 and 11 introduced the domain event publisher port and its adapters. This guide traces the complete flow in a single context: the aggregate state change occurs during a command, the application service captures and publishes the event through the publisher port, and a second downstream handler — modeled as a Spring `@EventListener` in the same process — consumes the event and triggers a side-effect. Understanding this flow end-to-end is a prerequisite for cross-context event routing (Guide 14) and for deciding when to upgrade from the in-memory adapter to the outbox adapter.

### Standard Library First

Java `Consumer<T>` and a mutable listener registry both require manual registration and synchronous dispatch with no lifecycle management:

```java
// Standard library: java.util.function for an in-process event handler
// Demonstrates the stdlib Consumer pattern that the Spring @EventListener supersedes.

import java.util.ArrayList;
// => ArrayList: mutable, unsynchronised — concurrent subscribe() calls corrupt the internal array
import java.util.List;
// => List: the handler registry type — parameterised on Consumer<T>
import java.util.function.Consumer;
// => Consumer<T>: functional interface with accept(T) — the callback invoked on each publish

public class InMemoryBus<T> {
    private final List<Consumer<T>> handlers = new ArrayList<>();
    // => handlers: mutable list — not thread-safe if handlers are registered after startup
    // => No synchronisation: concurrent subscribe() from multiple threads corrupts the list

    public void subscribe(Consumer<T> handler) {
        handlers.add(handler);
        // => Registration is synchronous — all subscribers must register before the first publish
        // => No deduplication: subscribing the same handler twice causes it to run twice per event
    }

    public void publish(T event) {
        for (var handler : handlers) {
            handler.accept(event);
            // => Synchronous dispatch — slow handlers block the event publisher
            // => If a handler throws, remaining handlers are skipped silently
            // => No retry, no dead-letter queue — failed handlers lose the event permanently
        }
    }
}
```

**Limitation for production**: no transaction coordination — the event is published before the aggregate is saved, which means the handler acts on uncommitted data. No retry on handler failure.

### Production Framework

The Spring `ApplicationEventPublisher` is the in-process publisher; `@EventListener` annotated methods are the consumers. The application service publishes after saving the aggregate — inside the same transaction boundary if `@TransactionalEventListener` is used:

```java
// IssuePurchaseOrderServiceImpl.java — publishes event after saving the aggregate
package com.procurement.platform.purchasing.infrastructure;
// => infrastructure/ package: @Service beans live here — they import ports, not vice versa

import com.procurement.platform.purchasing.application.IssuePurchaseOrderService;
// => IssuePurchaseOrderService: application port interface — the @Service must implement it
import com.procurement.platform.purchasing.application.PurchaseOrderIssued;
// => Domain event record: constructed here after the aggregate is saved and the transaction commits
import com.procurement.platform.purchasing.application.PurchaseOrderRepository;
// => Repository output port: the service never imports JdbcPurchaseOrderRepository directly
import com.procurement.platform.purchasing.application.EventPublisher;
// => EventPublisher output port: wired to the outbox or in-process adapter at the composition root
import com.procurement.platform.purchasing.domain.PurchaseOrder;
// => Domain aggregate: all state lives here — the service constructs and passes it to the repository
import com.procurement.platform.purchasing.domain.PurchaseOrderId;
// => Strongly-typed identity: wraps a UUID — prevents passing SupplierId where PurchaseOrderId is expected
import com.procurement.platform.purchasing.domain.PurchaseOrderLine;
// => Line value object: one ordered item — quantity, unit price, product reference
import com.procurement.platform.purchasing.domain.SupplierId;
// => Supplier identity: passed in from the controller — never constructed here from a raw String
import com.procurement.platform.purchasing.domain.PurchaseOrderStatus;
// => Status enum: Draft → AwaitingApproval → Approved → … → Closed — drives all state transitions
import com.procurement.platform.purchasing.domain.ApprovalLevel;
// => ApprovalLevel enum: derived from totalAmount — determines the approver tier for this PO
import org.springframework.stereotype.Service;
// => @Service: Spring registers this bean during component scan; synonym of @Component with semantics
import org.springframework.transaction.annotation.Transactional;
// => @Transactional: Spring wraps the annotated method in a database transaction boundary
import java.math.BigDecimal;
// => BigDecimal: arbitrary-precision arithmetic for currency totals — avoids floating-point rounding
import java.time.Instant;
// => Instant.now(): UTC timestamp attached to domain events — monotonically increasing across replicas
import java.util.List;
// => List<PurchaseOrderLine>: ordered line items passed in from the controller DTO
import java.util.Optional;
// => Optional: return type for findById — absence is a valid outcome, not an error
import java.util.UUID;
// => UUID.randomUUID(): identity generation strategy — client-generated, not database-generated

@Service
// => @Service: Spring discovers this bean via the root-package component scan
public class IssuePurchaseOrderServiceImpl implements IssuePurchaseOrderService {
    // => implements IssuePurchaseOrderService: the compiler verifies all port methods are present

    private final PurchaseOrderRepository repository;
    // => Port interface: wired to the decorated adapter stack (traced → resilient → JDBC)
    private final EventPublisher eventPublisher;
    // => EventPublisher port: wired to the outbox adapter in production

    public IssuePurchaseOrderServiceImpl(PurchaseOrderRepository repository,
            EventPublisher eventPublisher) {
        this.repository = repository;
        this.eventPublisher = eventPublisher;
        // => Constructor injection: Spring injects both beans automatically — no @Autowired needed
        // => Both fields are final: immutable after construction — thread-safe for concurrent requests
    }

    @Override
    @Transactional
    // => @Transactional: Spring wraps this method in a database transaction
    // => save() and publish() execute within the same commit boundary when using OutboxEventPublisher
    public PurchaseOrder issue(SupplierId supplierId, List<PurchaseOrderLine> lines)
            throws com.procurement.platform.purchasing.application.DuplicatePurchaseOrderException {
        var id = new PurchaseOrderId(UUID.randomUUID());
        // => PurchaseOrderId: new random UUID — generated in the application service, not in the database
        var totalAmount = new com.procurement.platform.purchasing.domain.Money(
            lines.stream()
                .map(l -> l.unitPrice().amount().multiply(new BigDecimal(l.quantity().value())))
                .reduce(BigDecimal.ZERO, BigDecimal::add),
            lines.isEmpty() ? "USD" : lines.get(0).unitPrice().currency());
        // => totalAmount: sum of all line totals — derived from line items at issuance time
        // => Currency: carried from the first line; if no lines, defaults to "USD"
        var approvalLevel = ApprovalLevel.fromAmount(totalAmount);
        // => approvalLevel: derived from totalAmount — L1 (≤ $1k), L2 (≤ $10k), L3 (> $10k)
        var po = new PurchaseOrder(id, supplierId, lines, totalAmount, approvalLevel, PurchaseOrderStatus.Draft);
        // => Draft status: all new POs begin in Draft — the approval workflow transitions the status
        repository.save(po);
        // => Persist the aggregate first — the event carries the committed state
        // => If save() throws, the transaction rolls back and publish() is never reached
        eventPublisher.publish(new PurchaseOrderIssued(po.id(), po.supplierId(), Instant.now()));
        // => Publish after save: the event payload reflects the committed aggregate
        // => With OutboxEventPublisher: the outbox row and the aggregate row commit atomically
        return po;
        // => Returns the created aggregate: the controller maps it to the response DTO
    }

    @Override
    public Optional<PurchaseOrder> findById(PurchaseOrderId id) {
        return repository.findById(id);
        // => Delegate to the repository port — no event needed for a read operation
        // => Optional.empty() returned when the PO does not exist — the controller maps this to 404
    }

    @Override
    @Transactional
    // => @Transactional: cancel() modifies state — must run within a transaction boundary
    public PurchaseOrder cancel(PurchaseOrderId id)
            throws com.procurement.platform.purchasing.application.PurchaseOrderNotFoundException,
                   com.procurement.platform.purchasing.application.InvalidPurchaseOrderStateException {
        var po = repository.findById(id)
            .orElseThrow(() -> new com.procurement.platform.purchasing.application.PurchaseOrderNotFoundException(id));
        // => orElseThrow: absence maps to a typed domain exception — the controller maps to HTTP 404
        if (po.status() == PurchaseOrderStatus.Paid || po.status() == PurchaseOrderStatus.Closed) {
            throw new com.procurement.platform.purchasing.application.InvalidPurchaseOrderStateException(
                "PurchaseOrder " + id + " cannot be cancelled in status " + po.status());
            // => InvalidPurchaseOrderStateException: typed domain exception — mapped to HTTP 422 globally
        }
        // => Business rule: POs in Paid or Closed state cannot be cancelled
        var cancelled = new PurchaseOrder(po.id(), po.supplierId(), po.lines(),
            po.totalAmount(), po.approvalLevel(), PurchaseOrderStatus.Cancelled);
        // => Immutable update: create a new record with updated status — the original is not mutated
        repository.save(cancelled);
        // => Persist the cancelled aggregate — the save() upserts on conflict
        eventPublisher.publish(new PurchaseOrderCancelled(id, Instant.now()));
        // => PurchaseOrderCancelled: event signals downstream contexts (e.g., receiving) to void GRN expectations
        return cancelled;
        // => Returns the cancelled aggregate: the controller maps it to the response DTO
    }
}
```

```java
// PurchaseOrderIssuedEventHandler.java — consumer in the receiving context
package com.procurement.platform.receiving.infrastructure;
// => receiving infrastructure/: the handler lives here — it accesses purchasing via the event record only

import com.procurement.platform.purchasing.application.PurchaseOrderIssued;
// => Import from the purchasing application package only — the handler accesses domain via the event
// => No import from purchasing.domain: the event record carries the data the handler needs
import org.slf4j.Logger;
// => Logger: SLF4J API — decoupled from the underlying implementation (Logback, Log4j2)
import org.slf4j.LoggerFactory;
// => LoggerFactory.getLogger: creates a logger named after this class — appears in log output
import org.springframework.context.event.EventListener;
// => @EventListener: Spring registers this method as a synchronous in-process event consumer
import org.springframework.stereotype.Component;
// => @Component: Spring registers this bean during the receiving context component scan

@Component
// => @Component: Spring discovers this handler — no explicit registration in @Configuration needed
public class PurchaseOrderIssuedEventHandler {

    private static final Logger log = LoggerFactory.getLogger(PurchaseOrderIssuedEventHandler.class);
    // => static final: one logger per class — not one per method call

    @EventListener
    // => @EventListener: Spring calls this method when PurchaseOrderIssued is published
    // => The method parameter type determines which event type triggers this handler
    public void onPurchaseOrderIssued(PurchaseOrderIssued event) {
        log.info("Processing PurchaseOrderIssued: poId={} supplierId={}",
            event.purchaseOrderId().value(), event.supplierId().value());
        // => Structured log: PO ID and supplier ID are queryable fields in log aggregation
        // => This handler opens a GRN expectation in the receiving context
        // => A slow handler here blocks the purchasing service HTTP request
        // => Use @Async or upgrade to outbox + relay for async dispatch
    }
}
```

**Trade-offs**: synchronous `@EventListener` dispatch is simple but blocks the application service until all handlers complete. Slow handlers block the HTTP request. Use `@Async` on the handler method to dispatch on a Spring-managed thread pool. For cross-process reliability, replace the in-process publisher with the outbox adapter from Guide 11.

### Domain Event Flow — End-to-End Sequence

```mermaid
sequenceDiagram
    autonumber
    participant Controller as Primary Adapter<br/>(@RestController)
    participant AppService as Application Service<br/>(IssuePurchaseOrderServiceImpl)
    participant Repository as Repository Port<br/>(PurchaseOrderRepository)
    participant Publisher as Event Publisher Port<br/>(EventPublisher)
    participant Handler as Event Handler<br/>(PurchaseOrderIssuedEventHandler)

    Controller->>AppService: issue(supplierId, lines)
    note over AppService: @Transactional — DB transaction opens

    AppService->>AppService: new PurchaseOrder(id, supplierId, lines, ...)
    note over AppService: Aggregate constructed in application layer

    AppService->>Repository: save(purchaseOrder)
    note over Repository: Decorated adapter stack:<br/>traced → resilient → JDBC

    Repository-->>AppService: (PurchaseOrder — saved aggregate)

    AppService->>Publisher: publish(PurchaseOrderIssued)
    note over Publisher: OutboxEventPublisher writes outbox row<br/>in same JDBC transaction

    Publisher->>Handler: onPurchaseOrderIssued(event)
    Handler-->>Publisher: (side-effect complete — GRN expectation opened)

    Publisher-->>AppService: (all handlers finished)
    note over AppService: @Transactional commits — aggregate + outbox row atomically

    AppService-->>Controller: PurchaseOrder (created aggregate)
    Controller-->>Controller: map PurchaseOrder → PurchaseOrderResponse DTO

    note over Controller,Handler: Replace @EventListener with @TransactionalEventListener(AFTER_COMMIT)<br/>to defer handler dispatch until after the DB commit
```

---

## Guide 22 — Hexagonal Anti-Patterns

> **Note on structure**: This guide intentionally omits the Standard Library First → Limitation → Production Framework progression used in all other guides. An anti-patterns catalogue has no meaningful "standard library first" path — the patterns themselves are the subject. The guide uses ANTI-PATTERN → Correction pairs instead, which is the pedagogically appropriate structure for this topic.

### Why It Matters

Anti-patterns accumulate silently in Java codebases because the compiler does not enforce hexagonal boundaries. A JPA annotation on a domain record, a Spring service that handles HTTP requests and database writes in one class, and an aggregate that holds no behavior — each of these feels harmless in isolation and becomes a migration nightmare at scale. Recognizing these three anti-patterns in `procurement-platform-be` before they take root saves significant refactoring cost later.

### The Leaky Hexagon

The leaky hexagon places framework annotations on domain types. The moment a domain record carries `@Entity`, `@Column`, or `@Table`, the domain layer depends on the ORM framework. Domain unit tests require the ORM on the classpath; switching ORMs touches domain files:

```java
// ANTI-PATTERN: JPA @Entity on a domain record — do not do this
// Demonstrates the leaky-hexagon pattern that the domain records in Guide 3 avoid.

import jakarta.persistence.*;
// => ANTI-PATTERN: ORM import in the domain package
// => jakarta.persistence is a framework dependency — domain tests now require JPA on classpath

@Entity
// => ANTI-PATTERN: @Entity couples the domain aggregate to JPA's entity lifecycle
@Table(name = "purchase_orders", schema = "purchasing")
// => ANTI-PATTERN: the table name is now hardcoded in the domain — changing the schema requires touching domain code
public class PurchaseOrder {
    // => ANTI-PATTERN: class not record — JPA requires a no-arg constructor, preventing record immutability

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    // => ANTI-PATTERN: UUID generation moved to the database — the domain aggregate cannot construct itself
    // => Domain-first design: the aggregate creates its own identity (new PurchaseOrderId(UUID.randomUUID()))
    private java.util.UUID id;

    @Column(nullable = false, precision = 19, scale = 4)
    // => ANTI-PATTERN: column constraint duplicated in the domain — it already lives in V1__create_purchasing_schema.sql
    private java.math.BigDecimal totalAmount;
}
```

**Correction**: the domain layer contains pure Java records with no ORM annotations. The JDBC mapping lives in the adapter in `infrastructure/` with explicit SQL. The domain record can be constructed, validated, and tested with zero ORM dependency.

### The God Adapter

The god adapter is a single Spring `@Service` that performs HTTP request parsing, database operations, domain event publishing, and supplier notification — all in one class:

```java
// ANTI-PATTERN: god adapter — one class doing everything
// Demonstrates the god-adapter pattern that the layered composition root avoids.

import jakarta.servlet.http.HttpServletRequest;
// => ANTI-PATTERN: HTTP concern (presentation layer) imported into a @Service (application layer)
import org.springframework.jdbc.core.simple.JdbcClient;
// => ANTI-PATTERN: infrastructure interface imported directly into the @Service

@org.springframework.stereotype.Service
public class GodPurchaseOrderService {

    private final JdbcClient jdbc;
    // => ANTI-PATTERN: infrastructure adapter (JdbcClient) injected directly into the service
    // => The service now depends on a concrete adapter, not a port interface

    public void issuePurchaseOrder(HttpServletRequest req) {
        // => ANTI-PATTERN: HttpServletRequest in the application layer — HTTP is a presentation concern
        String supplierId = req.getParameter("supplierId");
        // => Parsing HTTP parameters in the application service: the service is now untestable without Servlet API
        jdbc.sql("INSERT INTO purchasing.purchase_orders (id, supplier_id, status) VALUES (:id, :supplierId, 'Draft')")
            .param("id", java.util.UUID.randomUUID())
            .param("supplierId", java.util.UUID.fromString(supplierId))
            .update();
        // => ANTI-PATTERN: the service calls JdbcClient directly — no port interface between them
        // => Direct JDBC access: the application service cannot be tested without a database
        notifySupplier(supplierId);
        // => ANTI-PATTERN: supplier notification in the application service — belongs in an event handler
    }

    private void notifySupplier(String supplierId) { /* ... */ }
    // => ANTI-PATTERN: notification logic inline — no SupplierNotifierPort, no stub, no test coverage
}
```

**Correction**: the controller handles HTTP request parsing and delegates a command record to the application service. The application service calls the repository through a port interface. Supplier notification is a side-effect triggered by a `PurchaseOrderIssued` event handler (Guide 21). The god adapter becomes three lean classes: one controller, one application service, one event handler.

### The Anemic Domain

The anemic domain places all business logic in application services while domain classes carry only data fields. The domain object becomes a DTO with a class name, and the application service duplicates invariant checks that should live in the aggregate:

```java
// ANTI-PATTERN: anemic domain — data class with zero behavior
// Demonstrates the anemic pattern that the invariant-enforcing PurchaseOrder record in Guide 3 avoids.

public class AnemicPurchaseOrder {
    // => ANTI-PATTERN: plain data class — no validation, no behavior, no invariants
    public java.util.UUID id;
    // => public field: any code can set id to null — no constructor guard
    public java.util.UUID supplierId;
    // => No null guard: supplierId can be null — the PO becomes an orphan with no supplier
    public java.math.BigDecimal totalAmount;
    // => Nullable totalAmount: no domain invariant that total must be non-negative
    public String status;
    // => Plain String status: any value is legal — "PENDING", "pending", "" all compile
    // => AnemicPurchaseOrder can be constructed with all nulls — invalid domain state
}

// The application service carries all logic that the aggregate should own:
public class AnemicPurchaseOrderService {
    // => ANTI-PATTERN: service holds all invariant checks — each check must be repeated in every service method
    public AnemicPurchaseOrder issue(java.util.UUID supplierId, java.math.BigDecimal totalAmount) {
        if (totalAmount == null || totalAmount.compareTo(java.math.BigDecimal.ZERO) < 0) {
            // => ANTI-PATTERN: domain invariant check duplicated in every service method that creates a PO
            // => If a second service method also creates POs, it must repeat this check
            throw new IllegalArgumentException("totalAmount must not be negative");
        }
        var po = new AnemicPurchaseOrder();
        po.id = java.util.UUID.randomUUID();
        // => Manual field assignment: no constructor enforces invariants at object creation
        po.supplierId = supplierId;
        // => supplierId not validated — if null, the PO is silently saved as an orphan
        po.totalAmount = totalAmount;
        po.status = "Draft";
        // => Initial state: not validated by the aggregate — any caller could set "INVALID" before save
        return po;
    }
}
```

**Correction**: domain records declare invariant-enforcing compact constructors (Guide 3). The application service constructs the aggregate by passing raw values; the aggregate validates them. No service method repeats the invariant check — the compiler guarantees that a `PurchaseOrder` record in scope is always valid.

**Trade-offs**: all three anti-patterns feel pragmatic under time pressure. The leaky hexagon saves writing a mapper. The god adapter saves defining port interfaces. The anemic domain saves writing a compact constructor. Each shortcut defers a refactoring cost that compounds with every feature added on top of it. In `procurement-platform-be`, catching these patterns before any bounded context feature is fully implemented is the cheapest moment to correct them.

## Guide 23 — Kubernetes Deployment Topology for `procurement-platform-be`

### Why It Matters

A Kubernetes manifest is not a deployment detail you bolt on after the code works — it is the composition root for the entire hexagonal stack at runtime. The `Deployment` object determines how many adapter instances run concurrently; the `ConfigMap` holds the non-secret wiring that tells the Spring DataSource adapter which PostgreSQL host to connect to; the `Secret` holds the credentials that make the adapter authenticate. If these three resources are misaligned, the adapter throws at startup rather than at test time — you find out at 3 AM during a rolling restart rather than during the pre-merge integration test. Writing the manifest before the first production deploy makes the configuration contract explicit, reviewable, and portable across environments.

Spring Boot Actuator adds `/actuator/health`, `/actuator/liveness`, and `/actuator/readiness` without any manifest-level change. Kubernetes reads those endpoints through liveness and readiness probes. A misconfigured probe means Kubernetes either never routes traffic to a healthy pod or restarts a pod that is actually busy finishing a long-running database migration — both outcomes land the on-call engineer in a painful rollback.

### Standard Library First

`System.getenv` is the Java SE mechanism for reading runtime configuration. You can start `procurement-platform-be` on any machine by exporting environment variables manually before running the JAR:

```bash
# Standard library: running procurement-platform-be with environment variables only
# Demonstrates the manual environment variable approach that Kubernetes supersedes.

export SPRING_DATASOURCE_URL="jdbc:postgresql://localhost:5432/procurement_dev"
# => SPRING_DATASOURCE_URL: Spring Boot auto-configuration reads this key for the DataSource bean
# => Hardcoding the host/port/database in a script works locally but cannot be committed to version control

export SPRING_DATASOURCE_USERNAME="procurement"
# => SPRING_DATASOURCE_USERNAME: credential read by HikariCP at DataSource construction time
# => Each developer sets this individually — no central secret store, no rotation

export SPRING_DATASOURCE_PASSWORD="procurement"
# => SPRING_DATASOURCE_PASSWORD: plaintext in the shell environment — visible to every child process

java -jar apps/procurement-platform-be/target/procurement-platform-be.jar
# => Starts the Spring Boot application on the default port (8080)
# => No orchestration: one process, one database, no health checks, no pod restart on failure
```

**Limitation for production**: manual environment variables must be set on every machine, are not versioned with the application, and offer no secret rotation. A single missing variable causes the adapter to fail at connection time — `HikariPool-1 - Exception during pool initialization`. No liveness or readiness probe means Kubernetes cannot detect a crashed or overloaded JVM.

### Production Framework

A Kubernetes manifest for `procurement-platform-be` wires the Deployment, Service, ConfigMap, and Secret into a self-documenting topology:

```yaml
# apps/procurement-platform-be/deploy/k8s/configmap.yaml
apiVersion: v1
# => apiVersion: v1 is the stable core API group — ConfigMap is a v1 resource since Kubernetes 1.0
kind: ConfigMap
# => ConfigMap: holds non-secret key-value pairs injected into pods as environment variables
metadata:
  name: procurement-platform-be-config
  # => name: referenced by envFrom.configMapRef.name in the Deployment spec
  namespace: procurement-platform
  # => namespace: isolates procurement-platform-be resources from other services in the cluster
data:
  SPRING_DATASOURCE_URL: "jdbc:postgresql://postgres-svc.procurement-platform:5432/procurement"
  # => Spring Boot reads this key and wires it into the DataSource auto-configuration
  # => postgres-svc.procurement-platform: cluster-internal DNS — <service>.<namespace>.svc.cluster.local
  SERVER_PORT: "8080"
  # => SERVER_PORT: Spring Boot listens on this port inside the container
  MANAGEMENT_SERVER_PORT: "8081"
  # => Separate Actuator port: isolates health and metrics endpoints from application traffic
  # => Access policy: internal load balancer routes 8081 only within the cluster
  SPRING_APPLICATION_NAME: "procurement-platform-be"
  # => service.name: attached to every span in Micrometer Tracing — visible in Jaeger / Tempo
```

```yaml
# apps/procurement-platform-be/deploy/k8s/secret.yaml
# IMPORTANT: Never commit real secret values. Use Sealed Secrets or External Secrets Operator.
apiVersion: v1
# => apiVersion: v1 — Secret is a core resource; same API group as ConfigMap
kind: Secret
metadata:
  name: procurement-platform-be-secrets
  # => name: referenced by envFrom.secretRef.name in the Deployment — must match exactly
  namespace: procurement-platform
type: Opaque
# => Opaque: generic secret type — no schema validation; all values treated as arbitrary bytes
stringData:
  # => stringData: plain-text input; Kubernetes base64-encodes and stores under .data automatically
  SPRING_DATASOURCE_USERNAME: "REPLACE_ME"
  # => REPLACE_ME is a placeholder — a CI linter catches literal "REPLACE_ME" before deployment
  # => In production, populate via Sealed Secrets: kubeseal --raw --from-file=...
  SPRING_DATASOURCE_PASSWORD: "REPLACE_ME"
  # => DataSource password: HikariCP reads this at pool initialization time
  # => Rotate by updating the Secret and performing a rolling restart — no code change required
  BANKING_API_KEY: "REPLACE_ME"
  # => Bank API key: read by the RestClientBankingAdapter at construction time (Guide 18)
  # => Never hardcode API keys — always inject from a Secret at the deployment seam
```

```yaml
# apps/procurement-platform-be/deploy/k8s/deployment.yaml
apiVersion: apps/v1
# => apps/v1: the stable Deployment API group — required for Deployments since Kubernetes 1.9
kind: Deployment
metadata:
  name: procurement-platform-be
  namespace: procurement-platform
spec:
  replicas: 2
  # => 2 replicas: zero-downtime rolling update — one pod serves traffic while the other restarts
  selector:
    matchLabels:
      app: procurement-platform-be
  template:
    metadata:
      labels:
        app: procurement-platform-be
      annotations:
        prometheus.io/scrape: "true"
        # => Prometheus scrape annotation: the Prometheus operator discovers this pod for metric scraping
        prometheus.io/port: "8081"
        # => Actuator port: Prometheus scrapes /actuator/prometheus on the management port
        prometheus.io/path: "/actuator/prometheus"
    spec:
      containers:
        - name: procurement-platform-be
          image: ghcr.io/wahidyankf/procurement-platform-be:latest
          # => OCI image: built by the CI workflow and pushed to GitHub Container Registry
          # => In production, pin to an immutable SHA digest: image: ghcr.io/...@sha256:<digest>
          ports:
            - containerPort: 8080
              name: http
            - containerPort: 8081
              name: management
          envFrom:
            # => envFrom: injects all keys from a ConfigMap or Secret as environment variables
            - configMapRef:
                name: procurement-platform-be-config
            - secretRef:
                name: procurement-platform-be-secrets
          livenessProbe:
            # => livenessProbe: kubelet restarts the container if this probe fails
            httpGet:
              path: /actuator/liveness
              port: 8081
            initialDelaySeconds: 30
            # => 30 s delay: allows Flyway migrations (Guide 26) and Spring context to fully start
            periodSeconds: 15
            failureThreshold: 3
            # => 3 failures × 15 s = 45 s of grace before restart — prevents flapping during GC pauses
          readinessProbe:
            # => readinessProbe: kubelet removes the pod from Service endpoints if this probe fails
            httpGet:
              path: /actuator/readiness
              port: 8081
            initialDelaySeconds: 10
            periodSeconds: 10
          resources:
            requests:
              memory: "256Mi"
              # => 256Mi: conservative heap floor for a Spring Boot JVM at idle
              cpu: "250m"
            limits:
              memory: "512Mi"
              # => Kubernetes OOM-kills the pod if it exceeds 512Mi
              cpu: "1000m"
```

```yaml
# apps/procurement-platform-be/deploy/k8s/service.yaml
apiVersion: v1
# => apiVersion: v1 — Service is a core Kubernetes resource
kind: Service
# => Service: stable network endpoint for the pod replicas — DNS-resolvable cluster-internal address
metadata:
  name: procurement-platform-be-svc
  # => name: DNS name for in-cluster callers — procurement-platform-be-svc.procurement-platform.svc.cluster.local
  namespace: procurement-platform
  # => namespace: same as the Deployment — Service routes to pods in the same namespace
spec:
  selector:
    app: procurement-platform-be
    # => selector: matches pods with label app=procurement-platform-be from the Deployment template
  ports:
    - name: http
      port: 80
      # => port 80: the cluster-internal port — callers use port 80, pods receive on 8080
      targetPort: 8080
      # => targetPort 8080: routes cluster port 80 to container port 8080
    - name: management
      port: 8081
      targetPort: 8081
      # => management port exposed separately — Prometheus scrapes 8081, not 80
  type: ClusterIP
  # => ClusterIP: reachable within the cluster only — the Ingress resource handles external traffic
```

```mermaid
flowchart LR
    ci["CI workflow\n(build + push OCI image)"]:::orange
    secret["Kubernetes Secret\n(DB password, bank API key)"]:::purple
    cm["Kubernetes ConfigMap\n(DATASOURCE_URL, ports)"]:::teal
    dep["Deployment\n(2 replicas)"]:::blue
    svc["Service\nClusterIP :80 → :8080"]:::brown
    pg["PostgreSQL\n(postgres-svc)"]:::teal
    prom["Prometheus\n(/actuator/prometheus :8081)"]:::orange

    ci -->|"OCI image pull"| dep
    secret -->|"envFrom secretRef"| dep
    cm -->|"envFrom configMapRef"| dep
    dep -->|"HikariCP adapter\nDATASOURCE_URL"| pg
    dep -->|"Prometheus scrape\n:8081"| prom
    svc -->|"routes :80 → pod :8080"| dep

    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    classDef purple fill:#CC78BC,color:#fff,stroke:#CC78BC
    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
```

**Trade-offs**: `envFrom` with `secretRef` exposes all Secret keys as environment variables — any process inside the container can read them. For stricter isolation, mount the Secret as a volume and read files from `/run/secrets/`; Spring Boot supports file-based property sources via `spring.config.import=optional:file:/run/secrets/`. Kubernetes Secrets are base64-encoded, not encrypted at rest by default; enable etcd encryption at rest and use Sealed Secrets or External Secrets Operator before moving to production.

---

## Guide 24 — Observability Stack at the Deploy Seam: Micrometer Tracing + OTLP + Prometheus

### Why It Matters

Guide 20 showed how Micrometer Tracing decorates individual port calls with spans. At the deployment seam, the concern shifts: where does the collected telemetry go, and which sources does the SDK export? A misconfigured OTLP exporter means you pay the span creation overhead on every request but see nothing in Jaeger or Grafana Tempo. A missing Prometheus scrape configuration means P95 latency regressions on the PO issuance path are invisible until a procurement manager files a support ticket. Getting observability wired correctly before the first production deploy makes the difference between reacting to incidents in seconds and debugging in the dark.

The deployment seam also determines the resource attributes attached to every span — `service.name`, `service.version`, and `service.instance.id`. Without them, traces from two pod replicas collide in the trace UI, making it impossible to diagnose which replica produced a slow span.

### Standard Library First

`java.lang.management.ManagementFactory` provides JVM-level instrumentation that ships with the JDK. You can print heap usage and thread counts to stdout without any framework:

```java
// Standard library: JVM instrumentation via ManagementFactory
// Demonstrates the JDK management API that Micrometer supersedes for production observability.

import java.lang.management.ManagementFactory;
// => ManagementFactory: JDK entry point for platform MXBeans — no Maven dependency required
import java.lang.management.MemoryMXBean;
// => MemoryMXBean: heap and non-heap usage in bytes — useful but not a trace
import java.lang.management.ThreadMXBean;
// => ThreadMXBean: thread count, peak thread count, daemon threads

public class JvmMetricsDump {
    public static void printMetrics() {
        MemoryMXBean memory = ManagementFactory.getMemoryMXBean();
        long heapUsedMb = memory.getHeapMemoryUsage().getUsed() / (1024 * 1024);
        // => getUsed(): bytes of heap currently used — divided by 1M for readability

        ThreadMXBean threads = ManagementFactory.getThreadMXBean();
        int threadCount = threads.getThreadCount();

        System.out.printf("heap=%dMB threads=%d%n", heapUsedMb, threadCount);
        // => stdout: visible in kubectl logs — no aggregation, no trace correlation
    }
}
```

**Limitation for production**: `ManagementFactory` metrics are snapshots, not time-series — you cannot compute rate, P95, or trend. No spans means you cannot correlate a slow database query with the HTTP request that triggered it.

### Production Framework

`procurement-platform-be` wires Micrometer Tracing with the OTLP exporter and the Prometheus Micrometer registry via Spring Boot auto-configuration:

```java
// TracingConfig.java — enables Micrometer Tracing sources for procurement-platform-be contexts
package com.procurement.platform.shared.observability;
// => shared/observability/ package: cross-cutting tracing configuration lives here
// => One @Configuration for all bounded contexts — tracing is a shared infrastructure concern

import io.micrometer.tracing.Tracer;
// => Tracer: Micrometer abstraction over the underlying tracing backend (Brave or OTel)
// => Application code imports Tracer — never imports Brave or OpenTelemetry directly
import org.springframework.context.annotation.Bean;
// => @Bean: Spring calls this method and registers the returned object as a singleton bean
import org.springframework.context.annotation.Configuration;
// => @Configuration: Spring registers this class as a bean factory — all @Bean methods are called at startup
import io.micrometer.observation.ObservationRegistry;
// => ObservationRegistry: Micrometer 1.11+ unified observation API — spans and metrics share one entry point

@Configuration
// => @Configuration: Spring discovers this class during the root package component scan
public class TracingConfig {

    @Bean
    public io.micrometer.tracing.brave.bridge.BraveBaggageManager braveBaggageManager() {
        // => BraveBaggageManager: bridges Micrometer baggage API to Brave — required for W3C baggage propagation
        // => W3C baggage: carries trace context across HTTP calls to downstream adapters (bank API)
        // => Without this bean, baggage headers are not propagated through RestClient calls
        return new io.micrometer.tracing.brave.bridge.BraveBaggageManager();
        // => Returns a singleton: Spring Boot injects this into Micrometer's baggage propagator
    }
}
```

```yaml
# apps/procurement-platform-be/src/main/resources/application.yml (observability section)
management:
  # => management: Spring Boot Actuator configuration section
  endpoints:
    web:
      exposure:
        include: "health,info,prometheus,liveness,readiness"
        # => include: restricts which Actuator endpoints are exposed — principle of least privilege
        # => prometheus: exposes /actuator/prometheus in Prometheus text format
        # => liveness,readiness: the named probe groups consumed by the Kubernetes probes in Guide 23
  metrics:
    export:
      prometheus:
        enabled: true
        # => enabled: activates the Prometheus registry — metrics available at /actuator/prometheus
  tracing:
    sampling:
      probability: 1.0
      # => 1.0 in development: all traces recorded — reduce to 0.1 in production for high-traffic services
      # => Override via MANAGEMENT_TRACING_SAMPLING_PROBABILITY environment variable in ConfigMap

spring:
  application:
    name: "procurement-platform-be"
    # => name: the resource attribute attached to every span — visible in Jaeger and Grafana Tempo

management:
  otlp:
    tracing:
      endpoint: "http://localhost:4318/v1/traces"
      # => localhost fallback: overridden by MANAGEMENT_OTLP_TRACING_ENDPOINT in the Kubernetes ConfigMap
      # => Port 4318: OTLP/HTTP — use 4317 for OTLP/gRPC; HTTP is preferred when TLS is not configured
```

```yaml
# Extend apps/procurement-platform-be/deploy/k8s/configmap.yaml with observability keys
data:
  MANAGEMENT_OTLP_TRACING_ENDPOINT: "http://otel-collector-svc.observability:4318/v1/traces"
  # => otel-collector-svc.observability: service in the "observability" namespace
  MANAGEMENT_TRACING_SAMPLING_PROBABILITY: "0.1"
  # => 0.1: sample 10% of traces in production — reduces storage and CPU overhead under load
  OTEL_RESOURCE_ATTRIBUTES: "deployment.environment=production"
  # => Additional resource attribute: filter production vs staging traces in the trace UI
```

```mermaid
flowchart LR
    req["HTTP request\n(Spring @RestController)"]:::blue
    obs["ObservationRegistry\n(Micrometer)"]:::orange
    trace["Micrometer Tracing\n(Brave bridge)"]:::teal
    metrics["Micrometer Metrics\n(Prometheus registry)"]:::purple
    otlp["OTLP Collector\n(otel-collector-svc :4318)"]:::brown
    prom["Prometheus\n(/actuator/prometheus)"]:::orange

    req -->|"observation.start()"| obs
    obs -->|"span emitted"| trace
    obs -->|"timer recorded"| metrics
    trace -->|"OTLP/HTTP export"| otlp
    metrics -->|"scrape"| prom

    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    classDef purple fill:#CC78BC,color:#fff,stroke:#CC78BC
    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
```

**Trade-offs**: `management.tracing.sampling.probability: 1.0` captures every span during development but adds measurable overhead above 1000 req/s in production. Reduce to 0.1 and use tail-based sampling in the collector for high-traffic services.

---

## Guide 25 — Failure-Mode Wiring: Degraded Adapters and `HealthIndicator`

### Why It Matters

When the PostgreSQL pod is unhealthy during a rolling restart, you have two choices: fail every request immediately with a 500, or serve degraded responses from a fallback adapter. The hexagonal architecture makes the second choice tractable — because the application service depends on a port interface, not a concrete adapter, you can swap in a degraded adapter at the composition root without touching the domain or application layers. The cached read-model adapter returns the last known state; the null event publisher silently drops events when the broker is unavailable. A Spring `HealthIndicator` drives the liveness and readiness probes from Guide 23, so Kubernetes removes a degraded pod from rotation rather than routing live traffic to it.

### Standard Library First

A plain `try-catch` at the controller layer is the minimal fallback Java SE provides without a framework:

```java
// Standard library: try-catch fallback at the @RestController level
// Demonstrates the handler-level catch approach that HealthIndicator and degraded adapters supersede.

import org.springframework.http.ResponseEntity;
// => ResponseEntity: wraps HTTP status code + body — used here to return 503 on DB failure
import org.springframework.web.bind.annotation.GetMapping;
// => @GetMapping: maps HTTP GET to listPurchaseOrders()
import org.springframework.web.bind.annotation.RestController;
// => @RestController: Spring bean that serialises return values to JSON

@RestController
// => @RestController: Spring discovers this bean and maps @GetMapping to the URL
public class PurchaseOrderControllerFallback {

    private final com.procurement.platform.purchasing.application.IssuePurchaseOrderService service;
    // => Application service interface — the controller never sees the @Service implementation

    public PurchaseOrderControllerFallback(com.procurement.platform.purchasing.application.IssuePurchaseOrderService service) {
        this.service = service;
        // => Constructor injection: Spring wires the service bean at context startup
    }

    @GetMapping("/api/v1/purchase-orders")
    // => @GetMapping: maps HTTP GET /api/v1/purchase-orders to this method
    public ResponseEntity<?> listPurchaseOrders() {
        try {
            var po = service.findById(null);
            // => findById(null): illustrative only — null ID causes a NullPointerException in production
            return ResponseEntity.ok(po);
        } catch (org.springframework.dao.DataAccessException ex) {
            // => DataAccessException: Spring's DB exception hierarchy — catches all SQL/JDBC failures
            // => Problem: every @RestController method must duplicate this catch block
            return ResponseEntity.status(503).body("Service temporarily unavailable");
            // => No health signal: Kubernetes keeps routing traffic to this pod even when every request fails
            // => No cached response: the caller receives an error, not stale data
        }
    }
}
```

**Limitation for production**: the fallback logic lives in the controller — every controller method must duplicate the catch block. No caching: the fallback returns an error, not stale data. No health signal: Kubernetes keeps routing traffic to the pod even when all requests fail.

### Production Framework

The degraded-mode pattern introduces a `CachedPurchaseOrderReadAdapter` that returns the last-known PO list when the real repository port fails, and a `NullEventPublisher` that silently drops events when the broker is unavailable. A `PurchasingHealthIndicator` bean exposes the degraded flag to the Spring Actuator readiness probe:

```java
// CachedPurchaseOrderReadAdapter.java — returns cached PO list when the DB port fails
package com.procurement.platform.purchasing.infrastructure;
// => infrastructure/ package: adapters with framework dependencies live here

import com.procurement.platform.purchasing.application.PurchaseOrderRepository;
// => PurchaseOrderRepository: output port interface — CachedPurchaseOrderReadAdapter satisfies the same interface
import com.procurement.platform.purchasing.domain.PurchaseOrder;
// => PurchaseOrder domain aggregate: stored in the cache snapshot, returned on degraded reads
import com.procurement.platform.purchasing.domain.PurchaseOrderId;
// => PurchaseOrderId: used to filter the cache on findById and existsById calls
import org.springframework.stereotype.Component;
// => @Component: Spring registers this bean — wired at the composition root only in degraded mode
import java.util.List;
// => List<PurchaseOrder>: the full snapshot passed to populateCache by the health-check thread
import java.util.Optional;
// => Optional: return type for findById — absence communicated without null
import java.util.concurrent.CopyOnWriteArrayList;
// => CopyOnWriteArrayList: thread-safe list — cache written by health-check thread, read by request threads
import java.util.concurrent.atomic.AtomicBoolean;
// => AtomicBoolean: lock-free degraded flag — updated by the circuit-breaker callback, read per request

@Component
// => @Component: Spring discovers this bean; the composition root wires it behind the port in degraded mode
public class CachedPurchaseOrderReadAdapter implements PurchaseOrderRepository {
    // => implements PurchaseOrderRepository: satisfies the output port contract

    private final List<PurchaseOrder> cache = new CopyOnWriteArrayList<>();
    // => CopyOnWriteArrayList: snapshot semantics — reads never block writes from the health-check thread

    private final AtomicBoolean degraded = new AtomicBoolean(false);
    // => AtomicBoolean: lock-free — the circuit-breaker callback sets this to true

    public void populateCache(List<PurchaseOrder> snapshot) {
        cache.clear();
        // => clear() first: replaces stale entries atomically before addAll
        cache.addAll(snapshot);
        // => Replaces all entries with the latest snapshot from PostgreSQL
        // => CopyOnWriteArrayList: concurrent readers see the new snapshot after addAll completes
    }

    public void setDegraded(boolean value) {
        degraded.set(value);
        // => AtomicBoolean.set: visible to all threads immediately — no synchronisation block needed
    }

    public boolean isDegraded() {
        return degraded.get();
        // => isDegraded(): read by PurchasingHealthIndicator to determine the Actuator readiness state
    }

    @Override
    public PurchaseOrder save(PurchaseOrder po) {
        if (degraded.get()) {
            throw new com.procurement.platform.purchasing.application.RepositoryException(
                "Writes unavailable in degraded mode", null);
            // => Write operations are not supported in degraded mode — callers receive RepositoryException
            // => The application service propagates this exception to the controller → HTTP 503
        }
        throw new UnsupportedOperationException("CachedPurchaseOrderReadAdapter is not the active write path");
        // => Normal operation: the JDBC adapter handles writes; this adapter is only active in degraded mode
    }

    @Override
    public Optional<PurchaseOrder> findById(PurchaseOrderId id) {
        if (degraded.get()) {
            return cache.stream().filter(p -> p.id().equals(id)).findFirst();
            // => Degraded mode: return from the cached snapshot without touching the database
            // => findFirst(): returns Optional.empty() when no match — port contract preserved
        }
        throw new UnsupportedOperationException("CachedPurchaseOrderReadAdapter is not the active read path");
        // => Normal operation: the JDBC adapter handles reads; this adapter is only active in degraded mode
    }

    @Override
    public boolean existsById(PurchaseOrderId id) {
        if (degraded.get()) {
            return cache.stream().anyMatch(p -> p.id().equals(id));
            // => anyMatch: returns false for unknown IDs — callers can still perform existence checks
        }
        throw new UnsupportedOperationException("CachedPurchaseOrderReadAdapter is not the active read path");
    }
}
```

```java
// NullEventPublisher.java — silently drops events when the broker port is unavailable
package com.procurement.platform.purchasing.infrastructure;
// => infrastructure/ package: the null adapter lives here — same package as OutboxEventPublisher

import com.procurement.platform.purchasing.application.EventPublisher;
// => EventPublisher: the output port interface — NullEventPublisher satisfies it with a no-op body
import com.procurement.platform.purchasing.application.PurchaseOrderIssued;
// => Domain event record: the publisher receives it but does not relay it to the broker
import com.procurement.platform.purchasing.application.PurchaseOrderCancelled;
// => Second event type: also dropped silently — the null adapter handles all EventPublisher overloads
import org.slf4j.Logger;
// => SLF4J Logger API — decoupled from the logging backend (Logback, Log4j2)
import org.slf4j.LoggerFactory;
// => LoggerFactory.getLogger: creates a logger bound to this class name
import org.springframework.stereotype.Component;
// => @Component: Spring registers this bean — wired at the composition root only in degraded mode

@Component
// => @Component: Spring discovers this bean; the composition root wires it instead of OutboxEventPublisher
public class NullEventPublisher implements EventPublisher {
    // => Null object pattern: replaces the real adapter without changing the application service

    private static final Logger log = LoggerFactory.getLogger(NullEventPublisher.class);
    // => static final: one logger per class — not one per method invocation

    @Override
    public void publish(PurchaseOrderIssued event) {
        log.warn("Null event publisher: dropping PurchaseOrderIssued {} — outbox unavailable",
            event.purchaseOrderId().value());
        // => WARN level: not an error (the system degrades gracefully), but not silent — visible in the trace
        // => Silent drop: the application service proceeds as if the event was published
        // => At-least-once guarantee is lost — switch to an outbox adapter to preserve delivery
        // => Downstream contexts (receiving) will not receive the PurchaseOrderIssued signal during outage
    }

    @Override
    public void publish(PurchaseOrderCancelled event) {
        log.warn("Null event publisher: dropping PurchaseOrderCancelled {} — outbox unavailable",
            event.purchaseOrderId().value());
        // => Same null-object behaviour for the cancellation event — both overloads must be implemented
        // => WARN logged for observability: log aggregation shows the volume of dropped events during outage
    }
}
```

```java
// PurchasingHealthIndicator.java — drives liveness/readiness probes via the degraded flag
package com.procurement.platform.purchasing.infrastructure;

import org.springframework.boot.actuate.health.Health;
// => Health: Spring Actuator result object — UP/DOWN with optional detail map
import org.springframework.boot.actuate.health.HealthIndicator;
// => HealthIndicator: Spring Actuator interface — implementations appear in /actuator/health response
import org.springframework.stereotype.Component;

@Component("purchasingHealth")
// => "purchasingHealth": the bean name determines the key under "components" in /actuator/health JSON
public class PurchasingHealthIndicator implements HealthIndicator {

    private final CachedPurchaseOrderReadAdapter cachedAdapter;
    // => CachedPurchaseOrderReadAdapter: the degraded flag lives here — the indicator reads it

    public PurchasingHealthIndicator(CachedPurchaseOrderReadAdapter cachedAdapter) {
        this.cachedAdapter = cachedAdapter;
        // => Same bean: both PurchasingHealthIndicator and the composition root share the AtomicBoolean state
    }

    @Override
    public Health health() {
        // => Called by Actuator for /actuator/health, /actuator/readiness, and /actuator/liveness
        if (cachedAdapter.isDegraded()) {
            return Health.down()
                // => Health.down(): Actuator returns HTTP 503 for the readiness group — Kubernetes removes pod
                .withDetail("reason", "DataSource health check failed — serving from cache")
                .build();
        }
        return Health.up().build();
        // => Health.up(): Actuator returns HTTP 200 — Kubernetes routes traffic to pod
    }
}
```

**Trade-offs**: the cached read adapter serves stale data — clients receive a response that may be minutes old during a PostgreSQL outage. For a PO listing, staleness is acceptable; for a financial payment ledger it is not. The null event publisher silently drops domain events — if at-least-once delivery is a hard requirement, replace it with an in-memory buffer that replays to the outbox when the broker recovers.

---

## Guide 26 — Flyway Migration at Deploy Time: Kubernetes Job vs `ApplicationRunner`

### Why It Matters

Guide 17 introduced Flyway as the schema-migration adapter. At the deployment seam, the wiring question is: when does the migration run relative to pod startup, and which mechanism owns the migration lifecycle? Running Flyway inside `SpringApplication.run` means every replica races to apply migrations during a rolling restart — a potential for migration conflicts on `ALTER TABLE` statements. Running Flyway as a Kubernetes `Job` before the `Deployment` rolls means the schema is stable before any pod starts, but a failed job blocks the entire rollout. Choosing the wrong strategy causes a database-level lock that holds the deployment in progress for ten minutes while the on-call engineer investigates.

### Standard Library First

`java.sql.Connection` can execute DDL directly without a migration framework — the raw JDBC approach:

```java
// Standard library: DDL execution via JDBC Connection
// Demonstrates the manual DDL approach that Flyway supersedes.

import java.sql.Connection;
// => Connection: JDBC connection to the database — must be closed after use or the pool leaks
import java.sql.DriverManager;
// => DriverManager: JDBC entry point — finds a registered driver matching the URL scheme
import java.sql.SQLException;
// => SQLException: checked exception on every JDBC operation — callers must handle or declare throws

public class ManualSchemaMigration {
    public static void main(String[] args) throws SQLException {
        String url = System.getenv("SPRING_DATASOURCE_URL");
        // => Reads the JDBC URL from an environment variable — must be set before this runs
        // => Returns null if the variable is absent — DriverManager.getConnection throws NullPointerException
        try (Connection conn = DriverManager.getConnection(url, "procurement", "procurement")) {
            // => try-with-resources: Connection implements AutoCloseable — conn.close() is guaranteed
            conn.createStatement().execute(
                // => createStatement: plain statement, no parameters — suitable for DDL only
                "CREATE SCHEMA IF NOT EXISTS purchasing;" +
                "CREATE TABLE IF NOT EXISTS purchasing.purchase_orders (" +
                "  id UUID PRIMARY KEY," +
                "  supplier_id UUID NOT NULL," +
                "  total_amount NUMERIC(19,4) NOT NULL," +
                "  currency CHAR(3) NOT NULL," +
                "  status TEXT NOT NULL DEFAULT 'Draft'" +
                ")"
                // => No version number: impossible to determine which state the schema is in
                // => No rollback: if a second migration fails after the first succeeds, schema is partially upgraded
                // => Running this twice: CREATE TABLE IF NOT EXISTS is idempotent, but ALTER TABLE is not
            );
        }
    }
}
```

**Limitation for production**: no version tracking means running the migration twice executes the DDL twice — dangerous for `ALTER TABLE` or `DROP COLUMN` statements that are not idempotent. No rollback mechanism.

### Production Framework

`procurement-platform-be` includes Flyway via `spring-boot-starter-flyway` (Guide 17). This guide makes the deploy-time migration strategy explicit: a dedicated `Job` runs `flyway migrate` before the application containers start. Spring Boot's `spring.flyway.enabled=false` disables the in-process Flyway so only one path owns the migration:

```yaml
# apps/procurement-platform-be/deploy/k8s/migration-job.yaml
apiVersion: batch/v1
# => batch/v1: the stable Jobs API group — Kubernetes guarantees at-least-once execution semantics
kind: Job
metadata:
  name: procurement-platform-be-migrate
  namespace: procurement-platform
  annotations:
    helm.sh/hook: pre-upgrade,pre-install
    # => Helm hook: runs this Job before the Deployment rolls — schema is ready before pods start
    helm.sh/hook-delete-policy: hook-succeeded
    # => Deletes the Job after it succeeds — prevents accumulation of completed migration Jobs
spec:
  backoffLimit: 3
  # => backoffLimit: Kubernetes retries the pod up to 3 times on failure before marking the Job failed
  template:
    spec:
      restartPolicy: OnFailure
      containers:
        - name: flyway-migrate
          image: flyway/flyway:10-alpine
          # => Official Flyway CLI image: applies migrations without starting the Spring Boot application
          args: ["migrate"]
          # => migrate: the Flyway command — scans /flyway/sql and applies pending versioned scripts
          env:
            - name: FLYWAY_URL
              valueFrom:
                configMapKeyRef:
                  name: procurement-platform-be-config
                  key: SPRING_DATASOURCE_URL
            - name: FLYWAY_USER
              valueFrom:
                secretKeyRef:
                  name: procurement-platform-be-secrets
                  key: SPRING_DATASOURCE_USERNAME
            - name: FLYWAY_PASSWORD
              valueFrom:
                secretKeyRef:
                  name: procurement-platform-be-secrets
                  key: SPRING_DATASOURCE_PASSWORD
          volumeMounts:
            - name: migrations
              mountPath: /flyway/sql
      volumes:
        - name: migrations
          configMap:
            name: procurement-platform-be-migrations
            # => ConfigMap holding SQL migration files (V1__create_purchasing_schema.sql, etc.)
```

```yaml
# Disable in-process Flyway so the Kubernetes Job is the only migration path
spring:
  flyway:
    enabled: false
    # => disabled: prevents Spring Boot from running Flyway at ApplicationContext startup
    # => The Kubernetes Job above owns the migration lifecycle — two migration paths would race
```

The `ApplicationRunner` strategy — running Flyway inside `SpringApplication.run` — is the simpler alternative for teams not using Helm or Kubernetes Jobs:

```java
// FlywayMigrationRunner.java — ApplicationRunner strategy (alternative to Kubernetes Job)
package com.procurement.platform.shared.config;
// => shared/config/: cross-cutting configuration lives here — not in a single context package

import org.flywaydb.core.Flyway;
// => Flyway: the migration engine — scans classpath:db/migration for versioned SQL scripts
import org.springframework.boot.ApplicationArguments;
// => ApplicationArguments: command-line arguments passed to run() — unused here but required by the interface
import org.springframework.boot.ApplicationRunner;
// => ApplicationRunner: Spring lifecycle hook — run() is called after the ApplicationContext starts
import org.springframework.context.annotation.Profile;
// => @Profile: activates this runner only in specified profiles — suppressed in the k8s profile
import org.springframework.stereotype.Component;
// => @Component: Spring discovers this bean during component scan — active only in non-k8s profiles

import javax.sql.DataSource;
// => DataSource: the HikariCP connection pool — Flyway uses it to acquire a migration-lock connection

@Component
// => @Component: Spring registers this bean — run() is called once at startup
@Profile("!k8s")
// => @Profile("!k8s"): disabled in the k8s Spring profile — the Kubernetes Job owns migration in that profile
// => Activate the k8s profile in application.yml: spring.profiles.active: k8s
public class FlywayMigrationRunner implements ApplicationRunner {

    private final DataSource dataSource;
    // => DataSource: HikariCP pool configured from SPRING_DATASOURCE_* environment variables

    public FlywayMigrationRunner(DataSource dataSource) {
        this.dataSource = dataSource;
        // => Constructor injection: Spring wires the DataSource bean — no @Autowired annotation needed
    }

    @Override
    public void run(ApplicationArguments args) {
        // => run(): called by Spring Boot once the ApplicationContext is fully started
        // => Runs synchronously before HTTP server accepts traffic — no race with request handlers
        Flyway flyway = Flyway.configure()
            .dataSource(dataSource)
            // => dataSource: the same HikariCP pool — Flyway acquires a connection for the migration lock
            // => One migration runs at a time: Flyway uses a database-level advisory lock
            .locations("classpath:db/migration")
            // => classpath:db/migration: directory of versioned SQL files — V1__create_schema.sql, V2__...
            .validateOnMigrate(true)
            // => validateOnMigrate: Flyway checksums applied migrations — detects edits to already-run scripts
            // => Fails fast if a past migration file is modified — prevents silent schema drift
            .load();
        flyway.migrate();
        // => migrate(): applies all pending versioned migrations in order
        // => Acquires a database-level advisory lock — only one JVM migrates at a time
        // => Returns MigrateResult with counts of applied, pending, and failed migrations
    }
}
```

**Trade-offs**: the Kubernetes Job strategy decouples migration from pod startup — a failed migration blocks the rollout before any pod is replaced, which is the correct failure mode. The ApplicationRunner strategy is simpler but requires tuning `initialDelaySeconds` to cover the migration time. For small schemas (< 50 migrations, < 5 s total), the ApplicationRunner is acceptable; for large schemas or additive migrations running concurrently across 10+ replicas, the Kubernetes Job is required.

---

## Guide 27 — Configuration Adapter at the Deploy Seam: Secret to Typed `@ConfigurationProperties` Record

### Why It Matters

`procurement-platform-be` reads database credentials and the banking API key from environment variables that Kubernetes injects from a `Secret`. The journey of a credential from a Kubernetes Secret object to a strongly-typed Java record crosses four boundaries: Kubernetes decodes the base64-encoded Secret value and injects it as an environment variable; the Spring Environment property source reads the environment variable; the `@ConfigurationProperties` binding maps it to a typed record; the composition root reads the record and passes it to the adapter constructor. A break at any boundary — a renamed key, a missing prefix, a wrong casing — silently produces a `null` or empty string. Spring Boot `@ConfigurationProperties` with `@Validated` detects the break at startup rather than at the first database call, which turns a `3 AM NullPointerException in HikariPool` into a `ContextLoad failure: datasource.username must not be blank` during the Kubernetes pod `Init:0/1` phase — a much easier debugging session.

### Standard Library First

`System.getenv` reads a single key directly — the manual approach before `@ConfigurationProperties`:

```java
// Standard library: reading DataSource credentials manually from environment variables
// Demonstrates the manual approach that @ConfigurationProperties supersedes.

import org.springframework.jdbc.datasource.DriverManagerDataSource;

public class ManualDataSourceFactory {

    public static DriverManagerDataSource create() {
        String url = System.getenv("SPRING_DATASOURCE_URL");
        // => Reads the JDBC URL from the environment — returns null if the variable is not set
        // => getenv returns null, not empty string: callers must null-check every variable individually
        String username = System.getenv("SPRING_DATASOURCE_USERNAME");
        // => Reads the username — may be null if the Secret key name has a typo
        String password = System.getenv("SPRING_DATASOURCE_PASSWORD");

        if (url == null || username == null || password == null) {
            throw new IllegalStateException("DataSource environment variables not fully set");
            // => Fail-fast: better than NullPointerException deep in HikariCP initialization
            // => But: the error fires at the point of first use, not at startup — after health probes pass
        }
        // => No validation: an empty string passes the null check — "username=" is not caught here
        var ds = new DriverManagerDataSource();
        ds.setUrl(url);
        ds.setUsername(username);
        // => empty string is silently accepted — causes auth failure at connect time
        ds.setPassword(password);
        return ds;
    }
}
```

**Limitation for production**: `getenv` returns `null` for a missing variable and an empty string for an env var set to `""` — both cases pass a naive null-check but cause HikariCP to fail at connection time. Changes to the key names in the Kubernetes Secret must be manually mirrored in every `getenv` call.

### Production Framework

Spring Boot `@ConfigurationProperties` with `@Validated` maps the environment variables to a typed record and runs Jakarta Bean Validation at startup — before any request is handled and before the liveness probe first fires:

```java
// DataSourceProperties.java — typed record bound to SPRING_DATASOURCE_* environment variables
package com.procurement.platform.shared.config;
// => shared/config/ package: @ConfigurationProperties records live here — bound at startup

import jakarta.validation.constraints.NotBlank;
// => @NotBlank: fails validation if the bound value is null, empty, or whitespace-only
// => Catches env vars set to "" — the silent failure mode that @NotNull misses
import jakarta.validation.constraints.Pattern;
// => @Pattern: validates the format of the JDBC URL at startup — detects transposed host names
import org.springframework.boot.context.properties.ConfigurationProperties;
// => @ConfigurationProperties: Spring Boot binds properties with the given prefix to this record's fields
import org.springframework.validation.annotation.Validated;

@ConfigurationProperties(prefix = "spring.datasource")
// => prefix = "spring.datasource": binds SPRING_DATASOURCE_URL, SPRING_DATASOURCE_USERNAME,
//    SPRING_DATASOURCE_PASSWORD — Spring converts underscore-separated env vars to dotted property names
@Validated
// => @Validated: activates constraint checking at ApplicationContext startup — before pods report ready
public record DataSourceProperties(

    @NotBlank(message = "spring.datasource.url must not be blank")
    // => @NotBlank: catches null and empty-string SPRING_DATASOURCE_URL from the Kubernetes ConfigMap
    @Pattern(regexp = "^jdbc:postgresql://.*",
             message = "spring.datasource.url must be a PostgreSQL JDBC URL")
    // => @Pattern: ensures the URL starts with jdbc:postgresql:// — catches MySQL URL typos in the ConfigMap
    String url,

    @NotBlank(message = "spring.datasource.username must not be blank")
    // => @NotBlank: catches a Secret key named SPRING_DATASOURCE_USER instead of SPRING_DATASOURCE_USERNAME
    String username,

    @NotBlank(message = "spring.datasource.password must not be blank")
    // => @NotBlank: catches an empty Sealed Secret placeholder that was not replaced before deployment
    String password

) {}
// => record: immutable — values are set once by Spring Boot binding; no mutability risk after startup
```

```java
// AppConfig.java — registers @ConfigurationProperties beans and wires them to the DataSource
package com.procurement.platform.shared.config;

import com.zaxxer.hikari.HikariDataSource;
// => HikariCP: the production-grade JDBC connection pool — auto-configured by Spring Boot
import org.springframework.boot.context.properties.EnableConfigurationProperties;
// => @EnableConfigurationProperties: registers the DataSourceProperties bean and triggers binding + validation
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.sql.DataSource;

@Configuration
@EnableConfigurationProperties(DataSourceProperties.class)
// => @EnableConfigurationProperties: triggers binding and @Validated constraint checking at startup
// => If SPRING_DATASOURCE_URL is blank, Spring throws BindValidationException before any @Bean runs
public class AppConfig {

    @Bean
    public DataSource dataSource(DataSourceProperties props) {
        // => DataSourceProperties: bound and validated before this method is called — url is guaranteed non-blank
        var config = new com.zaxxer.hikari.HikariConfig();
        config.setJdbcUrl(props.url());
        // => url(): non-blank JDBC URL from the ConfigMap — validated by @Pattern at startup
        config.setUsername(props.username());
        // => username(): non-blank username from the Secret — validated by @NotBlank at startup
        config.setPassword(props.password());
        // => password(): non-blank password from the Secret — validated by @NotBlank at startup
        config.setMaximumPoolSize(10);
        // => Maximum pool size: 10 connections — sized for 2 replicas × 5 connections each
        config.setConnectionTimeout(3000);
        // => 3000 ms: connection acquisition timeout — bounded wait avoids thread starvation
        return new HikariDataSource(config);
        // => HikariDataSource: starts the connection pool and validates connectivity at construction time
        // => If the PostgreSQL pod is not reachable, HikariDataSource throws here — the Actuator readiness
        //    probe from Guide 25 returns DOWN and Kubernetes does not route traffic until the pool is healthy
    }
}
```

```mermaid
flowchart LR
    k8s["Kubernetes Secret\n(base64-encoded)"]:::purple
    env["JVM environment\n(SPRING_DATASOURCE_*)"]:::orange
    props["DataSourceProperties\n(@ConfigurationProperties)"]:::teal
    valid["@Validated\n(Jakarta Bean Validation)"]:::brown
    ds["HikariDataSource\n(AppConfig @Bean)"]:::blue
    port["JdbcPurchaseOrderRepository\n(repository port)"]:::teal

    k8s -->|"envFrom secretRef\n(Kubernetes decodes base64)"| env
    env -->|"Spring Environment\nbinds prefix"| props
    props -->|"@NotBlank + @Pattern\nchecked at startup"| valid
    valid -->|"passes at startup"| ds
    ds -->|"DataSource injected"| port

    classDef purple fill:#CC78BC,color:#fff,stroke:#CC78BC
    classDef orange fill:#DE8F05,color:#fff,stroke:#DE8F05
    classDef teal fill:#029E73,color:#fff,stroke:#029E73
    classDef brown fill:#CA9161,color:#fff,stroke:#CA9161
    classDef blue fill:#0173B2,color:#fff,stroke:#0173B2
```

**Trade-offs**: `@ConfigurationProperties` with `@Validated` adds one extra class per configuration group. The startup validation overhead is measured in milliseconds — negligible compared to HikariCP pool initialization. The `@Pattern` constraint on the JDBC URL is a double-edged sword: it catches URL typos early, but it also rejects valid non-PostgreSQL JDBC URLs if `procurement-platform-be` ever migrates to a different database — update the pattern when changing the database vendor. For `spring.config.import` with SSM or Vault, add the dependency and set `spring.config.import=optional:aws-ssm:/procurement/` in `application.yml`; the `@ConfigurationProperties` binding is identical — no code change, only a new property source.
