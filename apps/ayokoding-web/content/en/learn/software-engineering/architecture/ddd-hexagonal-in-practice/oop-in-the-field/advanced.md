---
title: "Advanced"
weight: 10000014
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Advanced DDD + Hexagonal in Practice guides (Guides 16–22) — Testcontainers database integration test, schema migration adapter with Flyway, AI orchestration port and Spring RestClient adapter, retry and circuit-breaker with Resilience4j, observability adapter with Micrometer Tracing, domain event flow end-to-end, and hexagonal anti-patterns"
tags:
  [
    "ddd",
    "hexagonal-architecture",
    "java",
    "spring-boot",
    "in-the-field",
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

Unit tests with an in-memory adapter (Guide 9) prove port correctness but cannot
catch SQL schema mistakes, PostgreSQL-specific constraint behavior, or migration
ordering bugs. A database integration test that spins up a real PostgreSQL
instance inside Docker closes this gap without requiring a persistent database on
developer machines. In `talks-platform-be`, the `@Testcontainers`-annotated test
class manages the full container lifecycle — start, health-check, stop — through
JUnit 5 lifecycle hooks. The adapter under test receives a `DataSource` configured
to point at the ephemeral container rather than any static URL.

### Standard Library First

`java.sql.DriverManager` can open a connection to any JDBC URL — but you manage
container startup, health-check polling, and teardown manually outside the test:

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
            // => Connection does not retry if the database is not yet ready — throws at once
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
        // => conn.close() called here by try-with-resources
        // => The container must be stopped manually — no JUnit lifecycle hook manages it
    }
}
```

**Limitation for production**: raw JDBC requires a running database before the
test starts, manual health-check polling, and manual teardown. The harness logic
duplicates across every project that needs integration tests against PostgreSQL.
Container startup is not coordinated with JUnit lifecycle hooks — if the JVM exits
unexpectedly, the container is orphaned.

### Production Framework

Testcontainers integrates with JUnit 5 via `@Testcontainers` and `@Container`.
The `PostgreSQLContainer` manages the full container lifecycle — start, wait for
the PostgreSQL health probe, expose a random host port, and stop after the test
class finishes. The adapter receives a `DataSource` built from the container's
JDBC URL, username, and password:

```mermaid
flowchart LR
    jvm["JUnit 5\ntest runner"]:::blue
    tc["@Testcontainers\nextension"]:::orange
    pg["PostgreSQLContainer\n(postgres:17-alpine)"]:::teal
    probe["waitingFor:\nlog message\n'ready to accept'"]:::purple
    adapter["JdbcTalkRepository\n(adapter under test)"]:::brown

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
package com.talksplatform.submission.infrastructure;
// => infrastructure package: integration tests live here — they test the adapter, not the domain

import com.talksplatform.submission.application.TalkRepository;
// => Application-layer port — the test exercises the adapter through the interface
import com.talksplatform.submission.domain.Talk;
import com.talksplatform.submission.domain.TalkId;
import com.talksplatform.submission.domain.SpeakerId;
import com.talksplatform.submission.domain.Abstract;
import com.talksplatform.submission.domain.TalkStatus;
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
// => DataSource: JDBC abstraction — the adapter receives this, not a raw connection
import java.util.Optional;
import java.util.UUID;
// => UUID: raw UUID type — wrapped in TalkId for the domain aggregate

import static org.junit.jupiter.api.Assertions.*;
// => Static import: assertEquals, assertNotNull, assertTrue — avoids verbose assertion calls

@Testcontainers
// => @Testcontainers: registers the JUnit 5 extension that starts @Container fields
// => The extension calls container.start() before any test in this class and container.stop() after all
public class JdbcTalkRepositoryIntegrationTest {

    @Container
    // => @Container: Testcontainers manages lifecycle — container starts before @BeforeEach, stops after @AfterAll
    // => static field: container is shared across all test methods in this class — one start, one stop
    static final PostgreSQLContainer<?> postgres =
        new PostgreSQLContainer<>("postgres:17-alpine");
        // => postgres:17-alpine: matches the production target — Alpine keeps the image small
        // => Testcontainers pulls the image on first run, caches it for subsequent runs
        // => waitingFor defaults to waiting for the "ready to accept connections" log message

    private TalkRepository talkRepository;
    // => Port interface declared — the test never imports JdbcTalkRepository directly
    // => Swapping the adapter requires no change to the test body

    @BeforeEach
    // => @BeforeEach: runs before each @Test method — creates a fresh adapter backed by the container
    void setUp() {
        DataSource dataSource = new DriverManagerDataSource(
            postgres.getJdbcUrl(),
            // => getJdbcUrl(): returns "jdbc:postgresql://localhost:<random-port>/test"
            // => Random port assigned by Docker — no port conflicts on CI runners
            postgres.getUsername(),
            // => getUsername(): returns "test" — the default PostgreSQLContainer credential
            postgres.getPassword()
            // => getPassword(): returns "test" — paired with the default username
        );
        // => DriverManagerDataSource: non-pooling DataSource — sufficient for integration tests
        // => Production uses HikariCP DataSource configured in application.properties
        talkRepository = new JdbcTalkRepository(
            org.springframework.jdbc.core.simple.JdbcClient.create(dataSource));
        // => JdbcTalkRepository: the infrastructure adapter under test — receives the container DataSource
        // => The adapter is constructed fresh before each test — no state carries between tests
    }

    @Test
    // => @Test: JUnit 5 test method — discovered by the JUnit Platform and executed by the engine
    void save_thenFindById_roundTripsSuccessfully() {
        // => Test name describes the observable contract — save then find returns the same aggregate
        var id = new TalkId(UUID.randomUUID());
        // => TalkId: strongly-typed identity — wraps a random UUID for this test run
        var speakerId = new SpeakerId(UUID.randomUUID());
        // => SpeakerId: strongly-typed speaker identity — wraps a random UUID
        var talk = new Talk(id, speakerId, new Abstract("Hexagonal Architecture in Practice"),
            new Format.Standard(), TalkStatus.Draft);
        // => Domain aggregate: built with the smart constructor — invariants validated at construction
        // => No Spring context needed — the domain record is a pure Java record

        talkRepository.save(talk);
        // => Write path: persists the aggregate via the JdbcTalkRepository to the real PostgreSQL container
        // => No mock, no in-memory stub — real SQL INSERT to the containerized database

        Optional<Talk> found = talkRepository.findById(id);
        // => Read path: queries the real database — finds the row committed by save()
        // => Optional: absence is a valid outcome; the test asserts presence explicitly below

        assertTrue(found.isPresent(), "Talk must be found after save");
        // => isPresent(): the row must exist — if the INSERT failed silently, this assertion fails
        assertEquals(talk.speakerId(), found.get().speakerId());
        // => speakerId round-trip: the persisted speaker identity must match the domain record's value
        assertEquals("Hexagonal Architecture in Practice", found.get().abstract_().value());
        // => Abstract round-trip: the persisted abstract text must match exactly
        assertEquals(TalkStatus.Draft, found.get().status());
        // => Status round-trip: verifies the enum column is mapped and retrieved correctly
    }

    @Test
    // => Second test: exercises the not-found path — no setup, no prior save
    void findById_returnsEmpty_whenNotFound() {
        var missingId = new TalkId(UUID.randomUUID());
        // => A UUID that was never saved — the database has no row for this identity

        Optional<Talk> result = talkRepository.findById(missingId);
        // => Port contract: absence must be returned as Optional.empty(), never null

        assertTrue(result.isEmpty(), "Unknown TalkId must return Optional.empty()");
        // => Verifies the adapter handles the not-found case correctly
    }
}
```

Add the Testcontainers dependency to the `pom.xml` for `talks-platform-be`:

```xml
<!-- Testcontainers BOM import — manage all testcontainers module versions consistently -->
<!-- Add to <dependencyManagement> section: -->
<dependency>
    <!-- => <dependency> in <dependencyManagement>: pins the version for all transitive consumers -->
    <groupId>org.testcontainers</groupId>
    <!-- => testcontainers-bom: the BOM that pins all org.testcontainers module versions -->
    <artifactId>testcontainers-bom</artifactId>
    <!-- => Spring Boot 4.0.6 BOM does NOT manage org.testcontainers core modules — explicit BOM import required -->
    <version>1.21.3</version>
    <!-- => type pom: this entry is a BOM import, not a jar — Maven reads it as a bill of materials -->
    <type>pom</type>
    <!-- => scope import: imports version constraints from this BOM into the current pom's dependencyManagement -->
    <scope>import</scope>
</dependency>

<!-- Then in <dependencies>: -->
<dependency>
    <!-- => org.testcontainers: official Testcontainers Maven group -->
    <groupId>org.testcontainers</groupId>
    <!-- => postgresql module: wraps postgres Docker image, exposes JDBC URL -->
    <artifactId>postgresql</artifactId>
    <!-- => Version managed by org.testcontainers:testcontainers-bom — no explicit version needed here -->
    <!-- => test scope: Testcontainers classes are never on the production classpath -->
    <scope>test</scope>
</dependency>
<dependency>
    <!-- => JUnit 5 lifecycle extension for @Testcontainers and @Container -->
    <groupId>org.testcontainers</groupId>
    <!-- => junit-jupiter: provides @Testcontainers and @Container annotations -->
    <artifactId>junit-jupiter</artifactId>
    <!-- => Without this module, the JUnit 5 lifecycle extension is not registered automatically -->
    <!-- => Version managed by org.testcontainers:testcontainers-bom -->
    <scope>test</scope>
</dependency>
```

**Trade-offs**: Testcontainers tests are slower than in-memory tests — PostgreSQL
startup typically adds 5–15 seconds. They require Docker on the developer machine
and CI runner. They are not cacheable by Nx because the external container is
non-deterministic. Run them on the `test:integration` Nx target, not
`test:quick`. The payoff is that they catch schema drift, PostgreSQL-specific
constraint behavior (unique index violations, check constraint ordering), and
migration bugs that no in-memory stub can surface.

---

## Guide 17 — Schema Migration Adapter with Flyway

### Why It Matters

Every database integration test and every production deployment depends on the
schema matching the application's expectations. Without a migration tool, schema
changes require manual SQL execution coordinated across every developer machine,
CI runner, and production server. In `talks-platform-be`, Flyway runs embedded SQL
migration scripts in versioned order at application startup. The migration adapter
is a first-class hexagonal concern: it runs before any domain port is called, and
the Testcontainers integration test (Guide 16) can invoke it against the fresh
container database before running assertions.

### Standard Library First

`java.io` and plain JDBC can execute SQL files in order — but you manage
ordering, idempotency, and error recovery manually:

```java
// Standard library: manual SQL file execution without a migration library
// Demonstrates the raw JDBC migration approach that the Flyway adapter supersedes.

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
// => Files.readString: reads a .sql file from the filesystem — no classpath scanning
import java.sql.Connection;
import java.sql.SQLException;
// => Connection: JDBC connection — execute the SQL directly on this connection

public class ManualMigrationRunner {
    public static void runMigration(Connection conn, Path sqlFile)
            throws IOException, SQLException {
        // => Two checked exceptions declared: I/O for file reading, SQL for execution
        // => No ordering enforcement — the caller must sort files by name manually
        String sql = Files.readString(sqlFile);
        // => Reads the entire SQL file as a string — no templating, no parameter binding
        // => If the file is missing, IOException propagates — no embedded-resource fallback
        try (var stmt = conn.createStatement()) {
            // => createStatement: plain statement — suitable for DDL, not parameterized DML
            stmt.execute(sql);
            // => execute: runs the entire file as one batch — DDL errors mid-file leave partial schema
            // => No journal table: if the script runs twice, CREATE TABLE throws a duplicate-object error
            // => The caller must track which scripts were already applied — no automatic idempotency
        }
        // => stmt.close() called here by try-with-resources
    }
}
```

**Limitation for production**: no journal table means migrations run again on
every restart. No ordering enforcement means naming conventions must be manually
enforced. No error recovery means a failed migration leaves the schema in a
partial state. No embedded-resource support means SQL files must be on the
filesystem at a known path.

### Production Framework

Flyway reads versioned SQL scripts from the classpath (`db/migration/V1__*.sql`,
`V2__*.sql`, …), maintains an applied-scripts journal table (`flyway_schema_history`)
in the database, and applies only unapplied scripts in order. Spring Boot 4 auto-
configures Flyway when `spring-boot-starter-flyway` (and `flyway-database-postgresql`
for PostgreSQL) are on the classpath:

```java
// ApplicationRunner invoking Flyway migration at startup
package com.talksplatform.shared.config;

import org.flywaydb.core.Flyway;
// => Flyway: the migration engine — configured with dataSource and migration locations
import org.flywaydb.core.api.output.MigrateResult;
// => MigrateResult: result record — carries migrationsExecuted count and success flag
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
// => SLF4J: structured logging — logs which scripts were applied on each run
import org.springframework.boot.ApplicationRunner;
// => ApplicationRunner: Spring Boot hook — runs after ApplicationContext is ready
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
// => @Configuration + @Bean: registers the ApplicationRunner as a Spring-managed bean

import javax.sql.DataSource;
// => DataSource: injected by Spring Boot — backed by HikariCP in production

@Configuration
// => @Configuration: Spring discovers this class during component scan
public class MigrationConfiguration {

    private static final Logger log = LoggerFactory.getLogger(MigrationConfiguration.class);
    // => Logger: SLF4J — logs migration activity at startup before any request arrives

    @Bean
    // => @Bean: Spring registers the returned ApplicationRunner as a singleton bean
    public ApplicationRunner migrationRunner(DataSource dataSource) {
        // => DataSource: injected from the Spring context — backed by HikariCP in production
        return args -> {
            // => ApplicationRunner lambda — runs once at startup, before any HTTP request is served
            Flyway flyway = Flyway.configure()
                .dataSource(dataSource)
                // => dataSource: Flyway uses the same pool as the application — no second connection pool
                .locations("classpath:db/migration")
                // => locations: Flyway scans src/main/resources/db/migration for V*.sql files
                // => Scripts named V1__create_submission_schema.sql, V2__create_review_schema.sql, etc.
                // => The double underscore separator is required by Flyway's default naming scheme
                .load();
            // => load(): builds the Flyway instance — no migration runs yet
            MigrateResult result = flyway.migrate();
            // => migrate(): applies all unapplied scripts in version order within individual transactions
            // => Flyway wraps each script in a transaction — a failed script leaves no partial schema
            log.info("Flyway applied {} migration(s)", result.migrationsExecuted);
            // => migrationsExecuted: 0 on subsequent restarts when schema is up to date
            if (!result.success) {
                // => success: false when any script fails — throw to abort startup
                throw new IllegalStateException("Flyway migration failed — aborting startup");
                // => Throwing here causes Spring Boot to exit with a non-zero code
                // => Kubernetes readiness probe fails — pod does not receive traffic before schema is ready
            }
        };
    }
}
```

The migration SQL script follows Flyway's naming scheme and uses one schema per
bounded context:

```sql
-- src/main/resources/db/migration/V1__create_submission_schema.sql
-- Creates the submission bounded context schema

CREATE SCHEMA IF NOT EXISTS submission;
-- => submission schema: isolates submission context tables from review and scheduling schemas
-- => One schema per bounded context: prevents accidental cross-context table joins in raw SQL

CREATE TABLE IF NOT EXISTS submission.talks (
    -- => submission.talks: one table per aggregate in the submission context
    id          UUID        PRIMARY KEY,
    -- => UUID primary key: matches TalkId.value() — no auto-increment sequences needed
    -- => UUID generation happens in the domain layer (TalkId constructor), not in the database
    speaker_id  UUID        NOT NULL,
    -- => speaker_id: foreign reference to the speaker — UUID only, no FK to a speakers table
    abstract    TEXT        NOT NULL,
    -- => abstract: stores the Abstract value object's String content
    -- => NOT NULL: enforces the domain invariant that every talk has a non-empty abstract
    format      TEXT        NOT NULL,
    -- => format: stores the sealed interface class simple name: "Lightning", "Standard", "Workshop"
    status      TEXT        NOT NULL DEFAULT 'Draft'
    -- => DEFAULT 'Draft': new rows start as Draft — matches the domain aggregate's initial state
);

CREATE INDEX IF NOT EXISTS talks_speaker_id_idx ON submission.talks(speaker_id);
-- => Index by speaker_id: findBySpeakerId() is the hot read path — makes it O(log n)

CREATE TABLE IF NOT EXISTS submission.outbox_events (
    -- => outbox_events: one outbox table per bounded context schema
    id          TEXT        PRIMARY KEY,
    -- => id: idempotency key generated by the OutboxEventPublisher — UUID string form
    event_type  TEXT        NOT NULL,
    -- => event_type: string name of the domain event class — used by the relay worker for routing
    payload     JSONB       NOT NULL,
    -- => JSONB: binary JSON format — enables GIN index on payload fields for relay filtering
    created_at  TIMESTAMPTZ NOT NULL,
    -- => TIMESTAMPTZ: timezone-aware timestamp — stores UTC instants from the application
    processed_at TIMESTAMPTZ
    -- => NULL until relayed: the relay worker sets this when the event has been delivered
);
```

Add the Flyway dependency to `pom.xml`:

```xml
<!-- Flyway starter — triggers FlywayAutoConfiguration in Spring Boot 4 -->
<dependency>
    <!-- => spring-boot starter group — triggers auto-configuration -->
    <groupId>org.springframework.boot</groupId>
    <!-- => required in Spring Boot 4: standalone flyway-core no longer triggers FlywayAutoConfiguration -->
    <artifactId>spring-boot-starter-flyway</artifactId>
    <!-- => Version managed by spring-boot-starter-parent 4.0.6 BOM — no explicit version needed -->
</dependency>
<dependency>
    <!-- => org.flywaydb: Flyway's Maven group — distinct from spring-boot starters -->
    <groupId>org.flywaydb</groupId>
    <!-- => PostgreSQL dialect module: required for PG-specific DDL and schema support -->
    <artifactId>flyway-database-postgresql</artifactId>
    <!-- => Version managed by spring-boot-starter-parent 4.0.6 BOM — no explicit version needed -->
    <!-- => Without this, Flyway falls back to a generic JDBC dialect — some Postgres features are not supported -->
</dependency>
```

**Trade-offs**: Flyway's versioned migration model requires naming discipline
(`V1__`, `V2__`) — a mislabeled script that should run after `V10__` but is named
`V2__` runs second and breaks. For `talks-platform-be`, Flyway's embedded SQL
approach keeps the migration language as plain SQL, which is more portable and
easier to review in pull requests.

---

## Guide 18 — AI Orchestration Port + Spring `RestClient` Adapter

### Why It Matters

AI inference is an I/O boundary: the application service sends a prompt and
receives a generated response from an external model provider. Like the database
boundary, this I/O must sit behind a port so the application service is testable
without a live API key, and so the provider can be swapped without touching
business logic. In `talks-platform-be`, the `ai-assist` bounded context introduces
an `AiProvider` interface in its `application` package. Spring Boot 4 ships
`RestClient` — already in the `pom.xml` via `spring-boot-starter-web` — which
provides type-safe, builder-configured HTTP calls with timeout control.

### Standard Library First

`java.net.http.HttpClient` (JDK 11+) can call any HTTP endpoint without any
Spring dependency. You manage timeout configuration, error discrimination, and
JSON mapping manually:

```java
// Standard library: java.net.http.HttpClient calling an AI inference endpoint
// Demonstrates the stdlib HttpClient approach that the Spring RestClient adapter supersedes.

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
// => java.net.http: JDK 11+ — no external dependency, full HTTP/2 support
import java.time.Duration;
// => Duration: used for connect and request timeouts — no third-party type needed

public class RawAiHttpClient {
    private final HttpClient httpClient = HttpClient.newBuilder()
        .connectTimeout(Duration.ofSeconds(5))
        // => connectTimeout: how long to wait for the TCP handshake — no retry on timeout
        .build();
    // => HttpClient is thread-safe and should be shared — one instance per application

    public String generateTags(String apiKey, String model, String abstract_)
            throws Exception {
        // => checked Exception: no typed error discrimination between rate-limit, auth failure, or timeout
        String body = """
            {"model":"%s","messages":[{"role":"user","content":"Tag this talk abstract: %s"}]}
            """.formatted(model, abstract_.replace("\"", "\\\""));
        // => Text block: JSON string built manually — no type safety on field names
        // => String escaping: abstract must have its double quotes escaped — fragile
        var request = HttpRequest.newBuilder()
            .uri(URI.create("https://openrouter.ai/api/v1/chat/completions"))
            // => Hardcoded URL: the base URL is not externalized — changing providers requires a code change
            .header("Authorization", "Bearer " + apiKey)
            // => API key passed directly to the method — the caller must manage secret retrieval
            .header("Content-Type", "application/json")
            // => Content-Type set manually — RestClient.contentType(MediaType.APPLICATION_JSON) is declarative
            .POST(HttpRequest.BodyPublishers.ofString(body))
            .timeout(Duration.ofSeconds(30))
            // => timeout: per-request timeout — independent of the connect timeout
            .build();
        var response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        // => send: synchronous — blocks the calling thread for up to 30 seconds
        if (response.statusCode() != 200) {
            // => Hard-coded 200 check: treats every non-200 as a failure — 201 and 202 are also rejected
            throw new Exception("AI call failed: " + response.statusCode());
            // => Undifferentiated exception: 429 (rate limit) and 401 (auth) both throw the same type
        }
        return response.body();
        // => Returns the raw JSON body string — caller must parse it manually with Jackson
    }
}
```

**Limitation for production**: no typed error discrimination between rate-limit
errors (429), authentication failures (401), and server errors (503). No retry
logic. The application layer must import `HttpClient` to call this function — the
AI boundary is not behind a port.

### Production Framework

The hexagonal approach declares an `AiProvider` port in the `ai-assist` application
package and implements the OpenRouter HTTP adapter in infrastructure using Spring
`RestClient`:

```java
// AiProvider.java — output port interface in the ai-assist application package
package com.talksplatform.aiassist.application;

// => application/ package: port interfaces live here — no Spring, no HTTP imports
// => The application service declares a dependency on AiProvider, not on RestClient

import com.talksplatform.submission.domain.Abstract;
// => Abstract: the value object from submission context — shared via the domain event payload
import com.talksplatform.submission.domain.TagSet;
// => TagSet: the value object wrapping an immutable Set<String> — port returns this type

public interface AiProvider {
    // => Output port: declares what the application needs from an AI provider
    // => No mention of OpenRouter, RestClient, or HTTP — those are adapter concerns

    TagSet autoTag(Abstract abstract_) throws AiProviderException;
    // => autoTag: called on TalkSubmitted event — generates tags for the talk abstract
    // => AiProviderException: typed checked exception — the caller can distinguish AI failure

    String summarize(Abstract abstract_) throws AiProviderException;
    // => summarize: called on TalkSubmitted event — produces a short summary for reviewer packs
    // => Throws AiProviderException on 5xx, timeout, or connection failure — not on 4xx (caller error)
}

// AiProviderException.java — typed exception in the application package
class AiProviderException extends Exception {
    // => Checked exception: callers must handle or declare it — no silent swallowing
    // => Lives in the application package: infrastructure and presentation can import it
    public AiProviderException(String message, Throwable cause) {
        super(message, cause);
        // => Wraps the underlying RestClient exception with a typed domain-layer exception
    }
}
```

```java
// OpenRouterAiProvider.java — RestClient adapter implementing AiProvider
package com.talksplatform.aiassist.infrastructure;
// => ai-assist/infrastructure/ package: the RestClient adapter lives here

import com.talksplatform.aiassist.application.AiProvider;
import com.talksplatform.aiassist.application.AiProviderException;
// => Import from application/ package only — no domain imports in the adapter
import com.talksplatform.submission.domain.Abstract;
import com.talksplatform.submission.domain.TagSet;
// => Domain value objects: Abstract carries the input, TagSet carries the output
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestClientException;
// => RestClient: Spring Boot 4 fluent HTTP client — replaces RestTemplate for new code
// => RestClientException: base exception for all RestClient errors — subtypes carry HTTP status
import org.springframework.stereotype.Component;
// => @Component: Spring registers this adapter in the ApplicationContext automatically

import java.time.Duration;
import java.util.Set;
// => Duration: explicit timeout values; Set: the underlying collection for TagSet

@Component
// => @Component: Spring discovers this class during component scan
public class OpenRouterAiProvider implements AiProvider {
    // => implements AiProvider: the adapter satisfies the output port contract
    // => The application service injects AiProvider — it never imports OpenRouterAiProvider

    private final RestClient restClient;
    // => RestClient: Spring Boot 4's modern HTTP client — builder-configured at construction
    // => final: immutable after construction — thread-safe

    private final String model;
    // => model: e.g., "anthropic/claude-3-5-haiku" — externalized in application.properties
    // => Swapping models requires only a config change, no code change

    public OpenRouterAiProvider(
            RestClient.Builder restClientBuilder,
            // => RestClient.Builder: Spring Boot auto-configures this bean
            @org.springframework.beans.factory.annotation.Value("${ai.openrouter.api-key}")
            String apiKey,
            // => @Value: reads ai.openrouter.api-key from application.properties or environment variable
            @org.springframework.beans.factory.annotation.Value("${ai.openrouter.model}")
            String model
            // => @Value: reads ai.openrouter.model — swappable without code change
    ) {
        this.restClient = restClientBuilder
            .baseUrl("https://openrouter.ai/api/v1")
            // => baseUrl: all requests from this client use this prefix — no hardcoded paths in methods
            .defaultHeader("Authorization", "Bearer " + apiKey)
            // => defaultHeader: the Authorization header is set once — not repeated per request
            .requestFactory(factory -> factory.setConnectTimeout(Duration.ofSeconds(5)))
            // => connectTimeout: TCP handshake must complete within 5 seconds
            .build();
        this.model = model;
        // => Store the resolved model name — used in ChatRequest when autoTag or summarize is called
    }

    @Override
    public TagSet autoTag(Abstract abstract_) throws AiProviderException {
        // => Implements AiProvider.autoTag — called by the ai-assist application service on TalkSubmitted
        try {
            var requestBody = new ChatRequest(
                model,
                new Message[]{ new Message("user",
                    "Return a JSON array of 3-5 tags for this talk abstract: " + abstract_.value()) }
            );
            var rawResponse = restClient.post()
                // => post(): configures an HTTP POST
                .uri("/chat/completions")
                // => uri: relative to baseUrl
                .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                .body(requestBody)
                .retrieve()
                .body(ChatResponse.class)
                // => body(Class): deserializes the response JSON into ChatResponse — Jackson mapping
                .choices()[0].message().content();
            // => Extract the first completion's content — parse as a JSON array of tag strings
            var tags = java.util.Arrays.stream(rawResponse.strip()
                    .replaceAll("[\\[\\]\"]", "").split(","))
                .map(String::strip)
                .collect(java.util.stream.Collectors.toSet());
            return new TagSet(Set.copyOf(tags));
            // => TagSet: immutable Set wrapper — returned to the application service
        } catch (RestClientException ex) {
            throw new AiProviderException("OpenRouter autoTag call failed: " + ex.getMessage(), ex);
            // => Wrap in AiProviderException to keep the application layer free of Spring imports
        }
    }

    @Override
    public String summarize(Abstract abstract_) throws AiProviderException {
        // => Implements AiProvider.summarize — called on TalkSubmitted to generate reviewer summaries
        try {
            var requestBody = new ChatRequest(
                model,
                new Message[]{ new Message("user",
                    "Summarize this talk abstract in one sentence: " + abstract_.value()) }
            );
            return restClient.post()
                .uri("/chat/completions")
                .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                .body(requestBody)
                .retrieve()
                .body(ChatResponse.class)
                // => ChatResponse: Jackson deserializes the response body into this record
                .choices()[0].message().content();
            // => Returns the summarized text as a plain String for the reviewer pack
        } catch (RestClientException ex) {
            throw new AiProviderException("OpenRouter summarize call failed: " + ex.getMessage(), ex);
        }
    }

    // Response records — private to the adapter, not exposed to the application layer
    private record ChatRequest(String model, Message[] messages) {}
    // => ChatRequest: Jackson serializes this record to {"model":"...","messages":[...]}
    private record Message(String role, String content) {}
    // => Message: {"role":"user","content":"..."} — OpenRouter chat completions schema
    private record ChatResponse(Choice[] choices) {}
    // => ChatResponse: Jackson deserializes the response body into this record
    private record Choice(MessageContent message) {}
    private record MessageContent(String content) {}
    // => MessageContent: the "message" object inside each choice — carries the generated text
}
```

**Trade-offs**: `RestClient` requires Spring MVC on the classpath — which is always
present for `spring-boot-starter-web`. The typed response records (private to the
adapter) couple the adapter to OpenRouter's JSON schema — if OpenRouter changes its
response shape, only the adapter changes; the application service and port are
untouched.

---

## Guide 19 — Retry + Circuit-Breaker via Resilience4j

### Why It Matters

External ports — the AI provider adapter from Guide 18, the JDBC adapter from Guide
8, any HTTP adapter calling a third-party service — fail transiently. A timeout does
not mean the downstream service is permanently unavailable; a retry after a brief
pause often succeeds. Conversely, an adapter that retries indefinitely against a
service that is genuinely down floods the downstream with traffic and keeps threads
occupied. Resilience4j (the Spring Boot 4 resilience starter) wraps port adapters
with configurable retry and circuit-breaker policies via a decorator pattern over
the port interface. The application service code is unchanged — the decorator is
wired in the composition root.

### Standard Library First

`java.util.concurrent` provides `CompletableFuture` and thread pools for async
retry — but you write the retry loop, backoff calculation, and open-circuit logic
yourself:

```java
// Standard library: manual retry loop with exponential backoff
// Demonstrates the manual retry approach that Resilience4j supersedes.

import java.util.function.Supplier;
// => Supplier<T>: functional interface — wraps the call that may throw

public class ManualRetry {
    public static <T> T withRetry(Supplier<T> supplier, int maxAttempts, long initialDelayMs)
            throws Exception {
        // => Generic method: works with any return type and any exception from the supplier
        Exception lastException = null;
        // => lastException: holds the most recent failure — rethrown if all attempts fail
        for (int attempt = 0; attempt < maxAttempts; attempt++) {
            // => Linear retry loop: no built-in jitter, no backoff cap, no circuit-breaker state
            try {
                return supplier.get();
                // => On success: return immediately — skip remaining attempts
            } catch (Exception ex) {
                lastException = ex;
                // => Record the exception — rethrow if no attempts remain
                if (attempt < maxAttempts - 1) {
                    long delay = initialDelayMs * (long) Math.pow(2, attempt);
                    // => Exponential backoff: 100ms, 200ms, 400ms, 800ms, ...
                    // => No jitter: simultaneous retries from multiple threads hit the same backoff window
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

**Limitation for production**: no circuit-breaker state — the retry loop hammers a
down service on every call. No jitter — simultaneous callers retry in lock-step.
No fallback. No metrics integration — failures are not counted for observability.

### Production Framework

Resilience4j provides `Retry` and `CircuitBreaker` decorators that wrap any
`Supplier`, `Function`, or `Callable`. The decorator is applied in the composition
root `@Configuration` class — the application service constructor receives a
`TalkRepository` that already has retry and circuit-breaker wiring applied:

```java
// ResilientTalkRepository.java — Resilience4j decorator wrapping the output port adapter
package com.talksplatform.submission.infrastructure;
// => infrastructure package: the decorator lives here — between the application port and the real adapter

import com.talksplatform.submission.application.TalkRepository;
// => Port interface: the decorator implements the same interface as the underlying adapter
import com.talksplatform.submission.domain.Talk;
import com.talksplatform.submission.domain.TalkId;
import com.talksplatform.submission.domain.SpeakerId;
// => Domain types only — the decorator speaks in domain terms, delegates to the real adapter
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
// => CircuitBreaker: state machine — CLOSED, OPEN, HALF_OPEN — stops calls when open
import io.github.resilience4j.retry.Retry;
// => Retry: configures max attempts, wait duration, and which exceptions trigger a retry
import io.github.resilience4j.retry.RetryConfig;
// => RetryConfig: builder for retry policy — maxAttempts, waitDuration, retryOnException predicate
import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
// => CircuitBreakerConfig: builder for circuit-breaker policy — failureRateThreshold, slidingWindowSize

import java.time.Duration;
// => Duration: timeout/wait values — avoids magic number literals in the policy builders
import java.util.List;
import java.util.Optional;

public class ResilientTalkRepository implements TalkRepository {
    // => Implements TalkRepository: the decorator is a drop-in replacement for the adapter
    // => The application service cannot tell whether it has the raw adapter or the decorator

    private final TalkRepository delegate;
    // => delegate: the real adapter (e.g., JdbcTalkRepository) — receives calls after retry/CB evaluation
    private final Retry retry;
    // => Retry: Resilience4j retry policy — applied before the circuit-breaker check
    private final CircuitBreaker circuitBreaker;
    // => CircuitBreaker: Resilience4j CB — opens when failure rate exceeds threshold

    public ResilientTalkRepository(TalkRepository delegate) {
        // => Constructor injection: the delegate adapter is passed in — no new adapter instantiation here
        this.delegate = delegate;
        // => Store the delegate — every port method calls delegate.<method> inside the decorator wrapping
        this.retry = Retry.of(
            "talk-repository",
            // => Instance name: used for metrics and logging
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
            "talk-repository",
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
    // => @Override: compiler verifies save() signature matches TalkRepository — no silent signature drift
    public void save(Talk talk) {
        // => Wraps save() with retry + circuit-breaker: transient DB failures are retried
        Retry.decorateRunnable(retry,
            // => decorateRunnable: wraps a Runnable — used for void operations like save()
            CircuitBreaker.decorateRunnable(circuitBreaker,
                // => decorateRunnable: wraps with circuit-breaker check — throws CallNotPermittedException if open
                () -> delegate.save(talk)
                // => Lambda delegates to the real adapter — retry fires if the lambda throws
            )
        ).run();
        // => .run(): executes the wrapped operation — throws if all retries exhausted or CB is open
    }

    @Override
    // => @Override: verifies findById matches the port interface
    public Optional<Talk> findById(TalkId id) {
        // => Wraps findById() with retry + circuit-breaker
        return Retry.decorateSupplier(retry,
            // => decorateSupplier: wraps a Supplier<Optional<Talk>> — used for value-returning operations
            CircuitBreaker.decorateSupplier(circuitBreaker,
                () -> delegate.findById(id)
            )
        ).get();
        // => .get(): executes the supplier — returns Optional<Talk> on success
    }

    @Override
    // => @Override: verifies findBySpeakerId matches the port interface
    public List<Talk> findBySpeakerId(SpeakerId speakerId) {
        return Retry.decorateSupplier(retry,
            CircuitBreaker.decorateSupplier(circuitBreaker,
                () -> delegate.findBySpeakerId(speakerId)
            )
        ).get();
    }

    @Override
    // => @Override: verifies delete matches the port interface — all four methods decorated consistently
    public void delete(TalkId id) {
        Retry.decorateRunnable(retry,
            CircuitBreaker.decorateRunnable(circuitBreaker,
                () -> delegate.delete(id)
                // => Delegate: the real adapter's delete — performs the hard-delete
            )
        ).run();
    }
}
```

The composition root wires the decorator transparently:

```java
// SubmissionContextConfiguration.java — composition root wiring with resilience decorator

@Bean
public TalkRepository talkRepository(JdbcClient jdbc) {
    var jdbcAdapter = new JdbcTalkRepository(jdbc);
    // => JdbcTalkRepository: the real database adapter — receives SQL calls
    return new ResilientTalkRepository(jdbcAdapter);
    // => ResilientTalkRepository: decorator wrapping the adapter with retry + circuit-breaker
    // => The application service injects TalkRepository — it never sees either concrete class
    // => Swap the decorator in tests by returning jdbcAdapter directly from @TestConfiguration
}
```

Add the Resilience4j Spring Boot starter to `pom.xml`:

```xml
<!-- Resilience4j Spring Boot 4 starter -->
<dependency>
    <groupId>io.github.resilience4j</groupId>
    <!-- => groupId: io.github.resilience4j is the official Resilience4j Maven group -->
    <artifactId>resilience4j-spring-boot4</artifactId>
    <!-- => resilience4j-spring-boot4: dedicated Spring Boot 4 starter — published to Maven Central as of v2.4.0 -->
    <!-- => Includes auto-configuration for Retry, CircuitBreaker, RateLimiter, Bulkhead, and TimeLimiter -->
    <version>2.4.0</version>
    <!-- => 2.4.0: explicit pin — Spring Boot 4 BOM does not manage Resilience4j transitively -->
</dependency>
```

**Trade-offs**: the decorator pattern adds two layers of wrapping over every port
method call. For high-frequency read operations, the decorator overhead is
measurable — profile before adding circuit-breakers to read paths. The payoff is
that every port adapter automatically participates in the resilience policy without
modifying the application service.

---

## Guide 20 — Observability Adapter via Micrometer Tracing

### Why It Matters

A hexagonal application whose port calls are not traced is a black box in
production. When a reviewer reports that the talk retrieval endpoint is slow, the
only way to diagnose it without tracing is to reproduce the slowness in development
— expensive and error-prone. Micrometer Tracing (the observability layer in Spring
Boot 4) wraps port calls with OpenTelemetry-compatible spans. The observability
adapter follows the same decorator pattern as the resilience decorator in Guide 19:
it implements the port interface, wraps each method with a span, delegates to the
real adapter, and is wired transparently in the composition root.

### Standard Library First

`java.lang.System.nanoTime()` can measure wall-clock duration — but it gives you
no distributed trace context, no parent-child span relationship, and no integration
with any observability backend:

```java
// Standard library: manual timing with System.nanoTime()
// Demonstrates the manual timing approach that Micrometer Tracing supersedes.

public class ManualTimingExample {
    // => Illustrative class: wraps a TalkRepository with manual nanoTime-based timing
    public void save(Talk talk) {
        long start = System.nanoTime();
        // => nanoTime(): monotonic clock — suitable for elapsed time, not wall-clock time
        // => No trace context: this duration cannot be correlated with upstream HTTP spans
        try {
            delegate.save(talk);
            // => delegate: the real adapter — the timing wraps its execution
        } finally {
            long elapsed = System.nanoTime() - start;
            // => Elapsed nanoseconds — convert to milliseconds for human-readable logging
            System.out.printf("save() took %.2f ms%n", elapsed / 1_000_000.0);
            // => Console output: no structured format, no trace ID, no span ID
        }
    }
}
```

**Limitation for production**: no distributed trace context — spans from different
services cannot be stitched into a single trace. No parent-child relationship. No
backend integration — the duration is logged to stdout, not exported to Jaeger or
an OpenTelemetry Collector.

### Production Framework

Micrometer Tracing with the OpenTelemetry bridge wraps port calls with spans that
carry a trace ID and are exported to the configured backend. The decorator pattern
keeps the tracing concern entirely in the infrastructure layer:

```java
// TracedTalkRepository.java — Micrometer Tracing decorator wrapping the output port
package com.talksplatform.submission.infrastructure;
// => infrastructure package: the tracing decorator lives alongside the JDBC adapter

import com.talksplatform.submission.application.TalkRepository;
// => Port interface: the decorator implements the same interface — application service is unaware of tracing
import com.talksplatform.submission.domain.Talk;
import com.talksplatform.submission.domain.TalkId;
import com.talksplatform.submission.domain.SpeakerId;
// => Domain types only — the decorator speaks in domain terms
import io.micrometer.tracing.Tracer;
// => Tracer: Micrometer Tracing API — creates spans and manages the trace context
import io.micrometer.tracing.Span;
// => Span: represents a single unit of work — carries trace ID, span ID, and tags

import java.util.List;
import java.util.Optional;

public class TracedTalkRepository implements TalkRepository {
    // => Implements TalkRepository: transparent decorator — application service sees only the interface
    // => Stacks with ResilientTalkRepository: wire as TracedTalkRepository(ResilientTalkRepository(...))

    private final TalkRepository delegate;
    // => delegate: the underlying adapter or resilience decorator — receives calls after span start
    private final Tracer tracer;
    // => Tracer: Micrometer Tracing — provided by Spring Boot auto-configuration

    public TracedTalkRepository(TalkRepository delegate, Tracer tracer) {
        this.delegate = delegate;
        // => Store the delegate — the span wraps every call to delegate.<method>
        this.tracer = tracer;
        // => Store the Tracer — used in each port method to create and end spans
    }

    @Override
    // => @Override: compiler verifies save() signature matches TalkRepository
    public void save(Talk talk) {
        Span span = tracer.nextSpan().name("talk-repository.save").start();
        // => nextSpan(): creates a child span if a parent trace context is active
        // => name: span name visible in Jaeger / Zipkin — describes the operation
        try (var ignored = tracer.withSpan(span)) {
            // => withSpan: scopes the span to the current thread
            span.tag("talk.id", talk.id().value().toString());
            // => tag: adds a key-value attribute to the span — visible in trace viewers
            delegate.save(talk);
            // => Delegate: the real adapter call — the span wraps its entire execution time
        } catch (Exception ex) {
            span.error(ex);
            // => error: marks the span as failed and records the exception
            throw ex;
        } finally {
            span.end();
            // => end(): closes the span and reports it to the OpenTelemetry exporter
            // => Always in finally: ensures spans are exported even when exceptions are thrown
        }
    }

    @Override
    // => @Override: verifies findById signature matches the port
    public Optional<Talk> findById(TalkId id) {
        Span span = tracer.nextSpan().name("talk-repository.findById").start();
        try (var ignored = tracer.withSpan(span)) {
            span.tag("talk.id", id.value().toString());
            // => Tag the queried ID: lets operators filter traces for a specific talk
            return delegate.findById(id);
        } catch (Exception ex) {
            span.error(ex);
            throw ex;
        } finally {
            span.end();
        }
    }

    @Override
    // => @Override: verifies findBySpeakerId signature — all port methods traced consistently
    public List<Talk> findBySpeakerId(SpeakerId speakerId) {
        Span span = tracer.nextSpan().name("talk-repository.findBySpeakerId").start();
        try (var ignored = tracer.withSpan(span)) {
            span.tag("talk.speaker_id", speakerId.value().toString());
            // => speakerId tag: allows filtering traces by speaker — useful for tenant-level diagnostics
            return delegate.findBySpeakerId(speakerId);
        } catch (Exception ex) {
            span.error(ex);
            throw ex;
        } finally {
            span.end();
        }
    }

    @Override
    // => @Override: verifies delete signature — compiler confirms the decorator covers the full port contract
    public void delete(TalkId id) {
        Span span = tracer.nextSpan().name("talk-repository.delete").start();
        try (var ignored = tracer.withSpan(span)) {
            span.tag("talk.id", id.value().toString());
            delegate.delete(id);
        } catch (Exception ex) {
            span.error(ex);
            throw ex;
        } finally {
            span.end();
            // => end(): exports the span regardless of success or failure — every delete is traceable
        }
    }
}
```

The composition root stacks all three decorators in order:

```java
// SubmissionContextConfiguration.java — stacked decorators in the composition root

@Bean
// => @Bean: Spring calls this method once at startup and registers the returned object as a singleton
public TalkRepository talkRepository(JdbcClient jdbc, Tracer tracer) {
    // => Parameters injected by Spring: JdbcClient from auto-config, Tracer from Micrometer auto-config
    var jdbcAdapter = new JdbcTalkRepository(jdbc);
    // => Layer 1: the real adapter — performs SQL operations against the PostgreSQL DataSource
    var resilientAdapter = new ResilientTalkRepository(jdbcAdapter);
    // => Layer 2: retry + circuit-breaker — retries transient failures, opens circuit on sustained failure
    return new TracedTalkRepository(resilientAdapter, tracer);
    // => Layer 3: tracing — wraps the full resilience + adapter stack with a span
    // => Span duration includes retry wait time — visible in trace timelines
    // => Application service injects TalkRepository — sees none of the concrete layers
}
```

Add Micrometer Tracing to `pom.xml`:

```xml
<!-- Micrometer Tracing + OpenTelemetry bridge -->
<dependency>
    <!-- => io.micrometer: Micrometer project group — includes metrics and tracing -->
    <groupId>io.micrometer</groupId>
    <!-- => OTel bridge: translates Micrometer Tracing to OTel spans -->
    <artifactId>micrometer-tracing-bridge-otel</artifactId>
    <!-- => Version managed by spring-boot-starter-parent 4.0.6 BOM -->
</dependency>
<dependency>
    <!-- => io.opentelemetry: OpenTelemetry project group -->
    <groupId>io.opentelemetry</groupId>
    <!-- => OTLP exporter: sends spans to Collector, Jaeger, or Tempo -->
    <artifactId>opentelemetry-exporter-otlp</artifactId>
    <!-- => Configure the endpoint in application.properties: management.otlp.tracing.endpoint -->
</dependency>
```

**Trade-offs**: the decorator stack — trace → resilience → JDBC adapter — adds
method-call overhead for every port operation. For high-frequency read paths under
strict latency constraints, consider sampling (export only 1% of spans) via
`management.tracing.sampling.probability=0.01` in `application.properties`. The
payoff is full distributed trace visibility across services with no changes to the
application service or domain layer.

---

## Guide 21 — Domain Event Flow End-to-End

### Why It Matters

Guides 10 and 11 introduced the domain event publisher port and its adapters. This
guide traces the complete flow in a single context: the aggregate state change
occurs during a command, the application service captures and publishes the event
through the publisher port, and a second downstream handler — modeled as a Spring
`@EventListener` in the same process — consumes the event and triggers a side-effect.
Understanding this flow end-to-end is a prerequisite for cross-context event routing
(Guide 14) and for deciding when to upgrade from the in-memory adapter to the outbox
adapter.

### Standard Library First

Java `Consumer<T>` and a mutable listener registry both require manual registration
and synchronous dispatch with no lifecycle management:

```java
// Standard library: java.util.function for an in-process event handler
// Demonstrates the stdlib Consumer pattern that the Spring @EventListener supersedes.

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
// => Consumer<T>: functional interface — a single-method handler for events of type T
// => ArrayList: the listener registry — not thread-safe under concurrent registration

public class InMemoryBus<T> {
    // => Generic class: parameterized by event type T — one bus instance per event type
    private final List<Consumer<T>> handlers = new ArrayList<>();
    // => handlers: mutable list — not thread-safe if handlers are registered after startup

    public void subscribe(Consumer<T> handler) {
        handlers.add(handler);
        // => add: appends to the list — no deduplication, no ordering guarantee
    }

    public void publish(T event) {
        for (var handler : handlers) {
            // => for-each: iterates the handler list in registration order
            handler.accept(event);
            // => Synchronous dispatch: each handler runs before the next starts
            // => A slow handler blocks the event publisher — no async dispatch
            // => If a handler throws, remaining handlers are skipped silently
        }
    }
}
```

**Limitation for production**: no transaction coordination — the event is published
before the aggregate is saved, which means the handler acts on uncommitted data.
No retry on handler failure. No async dispatch option without explicit thread pools.

### Production Framework

The Spring `ApplicationEventPublisher` is the in-process publisher; `@EventListener`
annotated methods are the consumers. The application service publishes after saving
the aggregate — inside the same transaction boundary if `@TransactionalEventListener`
is used:

```java
// SubmitTalkServiceImpl.java — publishes event after saving the aggregate
package com.talksplatform.submission.infrastructure;
// => infrastructure package: the @Service implementation lives here — not in application/

import com.talksplatform.submission.application.SubmitTalkService;
import com.talksplatform.submission.application.TalkSubmitted;
import com.talksplatform.submission.application.TalkRepository;
import com.talksplatform.submission.application.EventPublisher;
// => Application-layer types: port interfaces and event record — no infrastructure imports in application
import com.talksplatform.submission.domain.Talk;
import com.talksplatform.submission.domain.TalkId;
import com.talksplatform.submission.domain.SpeakerId;
import com.talksplatform.submission.domain.Abstract;
import com.talksplatform.submission.domain.Format;
import com.talksplatform.submission.domain.TalkStatus;
// => Domain types used to build and return the aggregate
import org.springframework.stereotype.Service;
// => @Service: Spring stereotype — registers the class as a singleton bean
import org.springframework.transaction.annotation.Transactional;
// => @Transactional: Spring wraps this method in a database transaction

import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

@Service
// => @Service: Spring registers this as a singleton bean — component scan discovers it
public class SubmitTalkServiceImpl implements SubmitTalkService {
    // => implements SubmitTalkService: the compiler verifies all declared port methods are present

    private final TalkRepository talkRepository;
    // => Port interface: wired to the decorated adapter stack (traced → resilient → JDBC)
    private final EventPublisher eventPublisher;
    // => EventPublisher port: wired to the outbox adapter in production

    public SubmitTalkServiceImpl(TalkRepository talkRepository, EventPublisher eventPublisher) {
        this.talkRepository = talkRepository;
        this.eventPublisher = eventPublisher;
        // => Constructor injection: Spring injects both beans automatically — no @Autowired needed
        // => Fields are final: immutable after construction — thread-safe for concurrent requests
    }

    @Override
    @Transactional
    // => @Transactional: Spring wraps this method in a database transaction
    // => save() and publish() execute within the same commit boundary when using OutboxEventPublisher
    public Talk submit(SpeakerId speakerId, Abstract abstract_, Format format)
            throws com.talksplatform.submission.application.DuplicateTalkException {
        var id = new TalkId(UUID.randomUUID());
        // => TalkId: new random UUID — generated in the application service, not in the database
        var talk = new Talk(id, speakerId, abstract_, format, TalkStatus.Draft);
        // => Domain aggregate: built with all domain value objects — invariants validated at construction
        talkRepository.save(talk);
        // => Persist the aggregate first — the event carries the committed state
        // => If save() throws, the transaction rolls back and publish() is never reached
        eventPublisher.publish(new TalkSubmitted(
            talk.id(), talk.speakerId(), talk.abstract_(), talk.format(), Instant.now()));
        // => Publish after save: the event payload reflects the committed aggregate
        // => With OutboxEventPublisher: the outbox row and the aggregate row commit atomically
        // => @TransactionalEventListener(phase=AFTER_COMMIT) defers dispatch until after commit
        return talk;
        // => Return the created aggregate — the controller maps it to a TalkResponse DTO
    }

    @Override
    // => @Override: compiler verifies findById signature matches SubmitTalkService
    public Optional<Talk> findById(TalkId id) {
        return talkRepository.findById(id);
        // => Delegate to the repository port — no event needed for a read operation
    }

    @Override
    @Transactional
    // => @Transactional: wraps withdraw in a transaction — read + write are atomic
    public Talk withdraw(TalkId id)
            throws com.talksplatform.submission.application.TalkNotFoundException,
                   com.talksplatform.submission.application.InvalidTalkStateException {
        var talk = talkRepository.findById(id)
            .orElseThrow(() -> new com.talksplatform.submission.application.TalkNotFoundException(id));
        // => orElseThrow: absence is a domain error here — throws typed exception
        if (talk.status() != TalkStatus.Draft && talk.status() != TalkStatus.Submitted) {
            throw new com.talksplatform.submission.application.InvalidTalkStateException(
                "Talk " + id + " cannot be withdrawn in status " + talk.status());
        }
        // => Business rule: only Draft or Submitted talks can be withdrawn
        var withdrawn = new Talk(talk.id(), talk.speakerId(), talk.abstract_(), talk.format(),
            TalkStatus.Rejected);
        // => Immutable update: create a new record with updated status — the original is not mutated
        talkRepository.save(withdrawn);
        eventPublisher.publish(new TalkWithdrawn(id, Instant.now()));
        // => TalkWithdrawn event: notifies downstream contexts that the talk is no longer active
        return withdrawn;
    }
}
```

```java
// TalkSubmittedEventHandler.java — consumer in the ai-assist context
package com.talksplatform.aiassist.infrastructure;

import com.talksplatform.submission.application.TalkSubmitted;
// => Import from the submission application package only — the handler accesses domain via the event
import com.talksplatform.aiassist.application.AiProvider;
// => AiProvider port: the ai-assist context calls this port to auto-tag on submission
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
// => SLF4J: structured logging — the handler logs the event for audit purposes
import org.springframework.context.event.EventListener;
// => @EventListener: Spring registers this method as a synchronous in-process event consumer
import org.springframework.stereotype.Component;
// => @Component: Spring registers this class as a singleton bean

@Component
// => @Component: Spring scans and registers this handler — no manual registration needed
public class TalkSubmittedEventHandler {

    private static final Logger log = LoggerFactory.getLogger(TalkSubmittedEventHandler.class);
    // => Logger: SLF4J — structured log entry for each event received

    private final AiProvider aiProvider;
    // => AiProvider port: the handler calls autoTag after receiving the TalkSubmitted event
    // => final: immutable after construction — thread-safe

    public TalkSubmittedEventHandler(AiProvider aiProvider) {
        this.aiProvider = aiProvider;
        // => Constructor injection: Spring provides the AiProvider adapter at startup
    }

    @EventListener
    // => @EventListener: Spring calls this method when TalkSubmitted is published
    // => The method parameter type determines which event type triggers this handler
    public void onTalkSubmitted(TalkSubmitted event) {
        // => event: the TalkSubmitted record — carries talkId, speakerId, abstract_, format
        log.info("Processing TalkSubmitted: talkId={} format={}",
            event.talkId().value(), event.format().getClass().getSimpleName());
        // => Structured log: talk ID and format are queryable fields in log aggregation
        try {
            var tags = aiProvider.autoTag(event.abstract_());
            // => autoTag: calls the OpenRouter AI provider via the AiProvider port
            log.info("Auto-tagged talk {}: tags={}", event.talkId().value(), tags.values());
            // => tags.values(): the Set<String> from the TagSet value object
        } catch (com.talksplatform.aiassist.application.AiProviderException ex) {
            log.warn("AI auto-tagging failed for talk {}: {}", event.talkId().value(), ex.getMessage());
            // => WARN: non-critical failure — the talk is still submitted, just without auto-tags
        }
    }
}
```

**Trade-offs**: synchronous `@EventListener` dispatch is simple but blocks the
application service until all handlers complete. Slow handlers (e.g., calling the
AI provider) block the HTTP request. Use `@Async` on the handler method to dispatch
on a Spring-managed thread pool. For cross-process reliability, replace the
in-process publisher with the outbox adapter from Guide 11.

### Domain Event Flow — End-to-End Sequence

```mermaid
sequenceDiagram
    autonumber
    participant Controller as Primary Adapter<br/>(@RestController)
    participant AppService as Application Service<br/>(SubmitTalkServiceImpl)
    participant Repository as Repository Port<br/>(TalkRepository)
    participant Publisher as Event Publisher Port<br/>(EventPublisher)
    participant Handler as Event Handler<br/>(TalkSubmittedEventHandler)

    Controller->>AppService: submit(speakerId, abstract_, format)
    note over AppService: @Transactional — DB transaction opens

    AppService->>AppService: new Talk(id, speakerId, abstract_, format, Draft)
    note over AppService: Aggregate constructed in application layer

    AppService->>Repository: save(talk)
    note over Repository: Decorated adapter stack:<br/>traced → resilient → JDBC

    Repository-->>AppService: (void — save complete)

    AppService->>Publisher: publish(TalkSubmitted)
    note over Publisher: OutboxEventPublisher writes outbox row<br/>in same JDBC transaction

    Publisher->>Handler: onTalkSubmitted(event)
    Handler-->>Publisher: (side-effect complete — e.g., AI auto-tagging)

    Publisher-->>AppService: (all handlers finished)
    note over AppService: @Transactional commits — aggregate + outbox row atomically

    AppService-->>Controller: Talk (created aggregate)
    Controller-->>Controller: map Talk → TalkResponse DTO

    note over Controller,Handler: Replace @EventListener with @TransactionalEventListener(AFTER_COMMIT)<br/>to defer handler dispatch until after the DB commit
```

---

## Guide 22 — Hexagonal Anti-Patterns

### Why It Matters

Anti-patterns accumulate silently in Java codebases because the compiler does not
enforce hexagonal boundaries. A JPA annotation on a domain record, a Spring service
that handles HTTP requests and database writes in one class, and an aggregate that
holds no behavior — each of these feels harmless in isolation and becomes a migration
nightmare at scale. Recognizing these three anti-patterns in `talks-platform-be`
before they take root saves significant refactoring cost later.

### The Leaky Hexagon

The leaky hexagon places framework annotations on domain types. The moment a
domain record carries `@Entity`, `@Column`, or `@Table`, the domain layer depends
on the ORM framework. Domain unit tests require the ORM on the classpath; switching
ORMs touches domain files:

```java
// ANTI-PATTERN: JPA @Entity on a domain record — do not do this
// Demonstrates the leaky-hexagon pattern that the domain records in Guide 3 avoid.

import jakarta.persistence.*;
// => ANTI-PATTERN: ORM import in the domain package
// => jakarta.persistence is a framework dependency — domain tests now require JPA on classpath

@Entity
// => ANTI-PATTERN: @Entity couples the domain aggregate to JPA's entity lifecycle
// => Domain records should be pure Java — no lifecycle states, no lazy-loading proxies
@Table(name = "talks", schema = "submission")
// => ANTI-PATTERN: the table name is now hardcoded in the domain — changing the schema requires touching domain code
public class Talk {
    // => ANTI-PATTERN: class not record — JPA requires a no-arg constructor, preventing record immutability

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    // => ANTI-PATTERN: UUID generation moved to the database — the domain aggregate cannot construct itself
    // => Domain-first design: the aggregate creates its own identity (new TalkId(UUID.randomUUID()))
    private UUID id;
    // => ANTI-PATTERN: mutable private field — JPA requires a setter to set the ID after INSERT

    @Column(nullable = false, length = 2000)
    // => ANTI-PATTERN: column constraint duplicated in the domain — it already lives in V1__create_submission_schema.sql
    private String abstract_;
    // => ANTI-PATTERN: mutable private field — JPA uses setter injection; records use compact constructors
}
```

**Correction**: the domain layer contains pure Java records with no ORM annotations.
The JPA entity (or `@Table`-annotated Spring Data JDBC entity) lives in the
`infrastructure` package. A mapper translates between the domain record and the
infrastructure entity. The domain record can be constructed, validated, and tested
with zero ORM dependency.

### The God Adapter

The god adapter is a single Spring `@Service` that performs HTTP request parsing,
database operations, domain event publishing, and email sending — all in one class:

```java
// ANTI-PATTERN: god adapter — one class doing everything
// Demonstrates the god-adapter pattern that the layered composition root avoids.

import jakarta.servlet.http.HttpServletRequest;
// => ANTI-PATTERN: HTTP concern (presentation layer) imported into a @Service (application layer)
import org.springframework.jdbc.core.simple.JdbcClient;
// => ANTI-PATTERN: infrastructure interface imported directly into the @Service

@org.springframework.stereotype.Service
// => @Service annotation is not the problem — what the class does is
public class GodTalkService {

    private final JdbcClient jdbc;
    // => ANTI-PATTERN: infrastructure adapter (JdbcClient) injected directly into the service
    // => The service now depends on a concrete adapter, not a port interface
    // => Swapping the adapter requires modifying GodTalkService

    public void submitTalk(HttpServletRequest req) {
        // => ANTI-PATTERN: HttpServletRequest in the application layer — HTTP is a presentation concern
        // => The controller should translate HttpServletRequest to a command record before calling the service
        String abstract_ = req.getParameter("abstract");
        // => Parsing HTTP parameters in the application service: the service is now untestable without Servlet API
        jdbc.sql("INSERT INTO submission.talks (id, abstract) VALUES (:id, :abstract)")
            .param("id", java.util.UUID.randomUUID())
            .param("abstract", abstract_)
            .update();
        // => ANTI-PATTERN: the service calls JdbcClient directly — no port interface between them
        // => Direct JDBC access: the application service cannot be tested without a database
        sendSpeakerConfirmation(abstract_);
        // => ANTI-PATTERN: email sending in the application service — a side-effect that belongs in an event handler
    }

    private void sendSpeakerConfirmation(String abstract_) { /* ... */ }
    // => ANTI-PATTERN: email logic inline — no email port, no stub, no test coverage without SMTP
}
```

**Correction**: the controller handles HTTP request parsing and delegates a
command record to the application service. The application service calls the
repository through a port interface. Email sending is a side-effect triggered by an
event handler (Guide 21). The god adapter becomes three lean classes: one controller,
one application service, one event handler.

### The Anemic Domain

The anemic domain places all business logic in application services while domain
classes carry only data fields. The domain object becomes a DTO with a class name,
and the application service duplicates invariant checks that should live in the
aggregate:

```java
// ANTI-PATTERN: anemic domain — data class with zero behavior
// Demonstrates the anemic pattern that the invariant-enforcing Talk record in Guide 3 avoids.

public class AnemicTalk {
    // => ANTI-PATTERN: plain data class — no validation, no behavior, no invariants
    // => Every service method that creates a talk must re-validate abstract, speakerId, etc.
    public UUID id;
    // => public field: any code can set id to null — no constructor guard
    // => No UUID wrapper: a raw UUID can be confused with any other UUID-typed field
    public String abstract_;
    // => Nullable abstract: no domain invariant that a talk must have content
    public String speakerId;
    // => No null guard: speakerId can be null — the talk becomes an orphan with no speaker
    public String status;
    // => Plain String status: any value is legal — "PENDING", "pending", "" all compile
    // => AnemicTalk can be constructed with all nulls — invalid domain state
}

// The application service carries all logic that the aggregate should own:
public class AnemicTalkService {
    // => ANTI-PATTERN: service holds all invariant checks — each check must be repeated in every service method
    public AnemicTalk submit(String abstract_, String speakerId) {
        // => submit: invariant validation is the caller's responsibility, not the aggregate's
        if (abstract_ == null || abstract_.isBlank() || abstract_.length() > 2000) {
            // => ANTI-PATTERN: domain invariant check duplicated in every service method that creates a talk
            // => If a second service method also creates talks, it must repeat this check
            throw new IllegalArgumentException("abstract must not be blank and ≤ 2000 chars");
        }
        var talk = new AnemicTalk();
        // => new AnemicTalk(): default constructor — all fields are null
        talk.id = java.util.UUID.randomUUID();
        // => Manual field assignment: no constructor enforces invariants at object creation
        talk.abstract_ = abstract_;
        talk.speakerId = speakerId;
        // => speakerId not validated here — if speakerId is null, the talk is silently saved as an orphan
        talk.status = "Draft";
        // => Initial state: not validated by the aggregate — any caller could set "INVALID_STATUS" before save
        return talk;
        // => Returns a mutably-held AnemicTalk — callers can modify fields before the service sees it again
    }
}
```

**Correction**: domain records declare invariant-enforcing compact constructors
(Guide 3). The application service constructs the aggregate by passing raw values;
the aggregate validates them. No service method repeats the invariant check — the
compiler guarantees that a `Talk` record in scope is always valid.

**Trade-offs**: all three anti-patterns feel pragmatic under time pressure. The
leaky hexagon saves writing a mapper. The god adapter saves defining port interfaces.
The anemic domain saves writing a compact constructor. Each shortcut defers a
refactoring cost that compounds with every feature added on top of it. In
`talks-platform-be`, catching these patterns before any bounded context feature is
fully implemented is the cheapest moment to correct them.
