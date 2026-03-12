# demo-be-javx

Java + Vert.x REST API backend — a functional twin of `demo-be-jasb` (Java/Spring Boot),
`demo-be-exph` (Elixir/Phoenix), and other demo-be backends using Java and Eclipse Vert.x.

## Tech Stack

| Concern     | Choice                               |
| ----------- | ------------------------------------ |
| Language    | Java 25                              |
| Framework   | Eclipse Vert.x 4.x (Vert.x Web)      |
| Build       | Maven                                |
| Database    | PostgreSQL (via Vert.x SQL Client)   |
| JWT         | Vert.x Auth JWT                      |
| Passwords   | jBCrypt                              |
| BDD Tests   | Cucumber JVM + JUnit 5 + Vert.x Test |
| Coverage    | JaCoCo (XML) + rhino-cli validate    |
| Linting     | Checkstyle                           |
| Null Safety | JSpecify @NullMarked + NullAway      |
| Port        | 8201                                 |

## Local Development

### Prerequisites

- JDK 25+
- PostgreSQL (or use Docker Compose)

### Environment Variables

| Variable            | Default                                         | Description         |
| ------------------- | ----------------------------------------------- | ------------------- |
| `PORT`              | `8201`                                          | HTTP port           |
| `DATABASE_URL`      | `jdbc:postgresql://localhost:5432/demo_be_javx` | JDBC connection URL |
| `DATABASE_USER`     | `demo_be_javx`                                  | Database username   |
| `DATABASE_PASSWORD` | `demo_be_javx`                                  | Database password   |
| `JWT_SECRET`        | (dev default)                                   | JWT signing secret  |

### Run locally

```bash
# Start PostgreSQL
docker compose -f ../../infra/dev/demo-be-javx/docker-compose.yml up -d demo-be-javx-db

# Run dev server
mvn compile exec:java

# Health check
curl http://localhost:8201/health
```

## Nx Targets

```bash
nx build demo-be-javx          # Compile with Maven
nx dev demo-be-javx            # Start development server
nx run demo-be-javx:test:quick # Unit + integration tests + coverage gate + lint
nx run demo-be-javx:test:unit  # Unit tests only
nx run demo-be-javx:test:integration  # Integration (Cucumber) tests only
nx lint demo-be-javx           # Run Checkstyle
```

## API Endpoints

See [plan README](../../plans/done/2026-03-11__demo-be-javx/README.md) for the full API surface.

## Test Architecture

Integration tests use Cucumber JVM with Vert.x Test and in-memory store implementations
(ConcurrentHashMap). No external services required. Cucumber reads feature files from
`specs/apps/demo-be/gherkin/` copied into the test classpath.
