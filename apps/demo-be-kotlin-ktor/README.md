# demo-be-kotlin-ktor

Kotlin + Ktor REST API backend — a functional twin of `demo-be-java-springboot` (Java/Spring Boot),
`demo-be-elixir-phoenix` (Elixir/Phoenix), and `demo-be-fsharp-giraffe` (F#/Giraffe) using Kotlin, Ktor, and Exposed.

## Tech Stack

| Concern      | Choice                                    |
| ------------ | ----------------------------------------- |
| Language     | Kotlin 2.1 (JVM)                          |
| Framework    | Ktor 3.x (Netty engine)                   |
| Build        | Gradle 8.14 (Kotlin DSL)                  |
| Database ORM | Exposed + PostgreSQL                      |
| JWT          | com.auth0:java-jwt + Ktor JWT auth plugin |
| Passwords    | jBCrypt                                   |
| DI           | Koin 4.x                                  |
| BDD Tests    | Cucumber JVM + JUnit 5 Platform           |
| Coverage     | Kover (JaCoCo XML) + rhino-cli validate   |
| Linting      | detekt                                    |
| Formatting   | ktfmt (Google style)                      |
| Port         | 8201                                      |

## Local Development

### Prerequisites

- JDK 21+
- PostgreSQL (or use Docker Compose)

### Environment Variables

| Variable            | Default                                                | Description         |
| ------------------- | ------------------------------------------------------ | ------------------- |
| `PORT`              | `8201`                                                 | HTTP port           |
| `DATABASE_URL`      | `jdbc:postgresql://localhost:5432/demo_be_kotlin_ktor` | JDBC connection URL |
| `DATABASE_USER`     | `demo_be_kotlin_ktor`                                  | Database username   |
| `DATABASE_PASSWORD` | `demo_be_kotlin_ktor`                                  | Database password   |
| `JWT_SECRET`        | (dev default)                                          | JWT signing secret  |

### Run locally

```bash
# Start PostgreSQL
docker compose -f ../../infra/dev/demo-be-kotlin-ktor/docker-compose.yml up -d demo-be-db

# Run dev server
./gradlew run

# Health check
curl http://localhost:8201/health
```

## Nx Targets

```bash
nx build demo-be-kotlin-ktor          # Compile and package fat JAR
nx dev demo-be-kotlin-ktor            # Start development server
nx start demo-be-kotlin-ktor          # Start production JAR
nx run demo-be-kotlin-ktor:test:quick # Unit + integration tests + coverage gate + lint
nx run demo-be-kotlin-ktor:test:unit  # Unit tests only
nx run demo-be-kotlin-ktor:test:integration  # Integration (Cucumber) tests only
nx lint demo-be-kotlin-ktor           # Run detekt linter
```

## API Endpoints

See [plan README](../../plans/done/2026-03-11__demo-be-kotlin-ktor/README.md) for the full API
surface.

## Test Architecture

Integration tests use real Ktor Netty server on a random port with in-memory repository
implementations (ConcurrentHashMap). No external services required. Cucumber JVM reads feature
files from `specs/apps/demo-be/gherkin/` copied into the test classpath via `processTestResources`.
