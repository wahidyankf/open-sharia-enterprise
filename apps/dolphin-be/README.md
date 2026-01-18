# Dolphin Backend (dolphin-be)

Learning Management System (LMS) backend built with Spring Boot and Java 25.

## Overview

Dolphin Backend provides backend services for the Learning Management System platform, including user management, course management, and progress tracking. The name "dolphin" was chosen because dolphins are known for their intelligence and love of learning, symbolizing the LMS's purpose of facilitating education and knowledge growth.

## Prerequisites

Required software:

- Java 25 (LTS) or Java 17+ (minimum required by Spring Boot 4.0)
- Maven 3.8+
- Node.js 24.11.1 (for Nx monorepo)
- npm 11.6.3

To verify Java installation:

```bash
java --version
# Expected: openjdk 25.0.1 or higher
```

To verify Maven installation:

```bash
mvn --version
# Expected: Apache Maven 3.8+ with Java 25
```

## Installation

From the repository root, install all dependencies:

```bash
npm install
```

Maven dependencies will be downloaded automatically on first build.

## Development

### Start Development Server

Using Nx:

```bash
nx run dolphin-be:serve
```

Using Maven directly:

```bash
cd apps/dolphin-be
mvn spring-boot:run -Dspring-boot.run.profiles=dev
```

The application will start at `http://localhost:8080`.

### Available Endpoints

Health check:

```bash
curl http://localhost:8080/actuator/health
```

Expected response:

```json
{
  "status": "UP",
  "components": {
    "diskSpace": {
      "status": "UP"
    },
    "ping": {
      "status": "UP"
    }
  }
}
```

Application info:

```bash
curl http://localhost:8080/actuator/info
```

## Available Nx Commands

### Build

Build the application and create JAR file:

```bash
nx run dolphin-be:build
```

Output: `apps/dolphin-be/target/dolphin-be-1.0.0.jar`

### Test

Run all tests:

```bash
nx run dolphin-be:test
```

Using Maven directly:

```bash
cd apps/dolphin-be
mvn test
```

### Lint

Run code style checks:

```bash
nx run dolphin-be:lint
```

## Testing

### Unit Tests

Run unit tests:

```bash
nx run dolphin-be:test
```

### Integration Tests

Integration tests are included in the test suite and use `@SpringBootTest`.

### Manual Testing

Start the application and test health check endpoint:

```bash
# Terminal 1: Start application
nx run dolphin-be:serve

# Terminal 2: Test health check
curl http://localhost:8080/actuator/health
```

Expected output: `{"status":"UP"}`

## Production Deployment

### Build for Production

```bash
nx run dolphin-be:build
```

### Run Production Build

```bash
cd apps/dolphin-be
java -jar target/dolphin-be-1.0.0.jar --spring.profiles.active=prod
```

### Environment Variables

Production configuration via environment variables:

- `PORT` - Server port (default: 8080)
- `SPRING_PROFILES_ACTIVE` - Active Spring profile (dev/prod)

Example:

```bash
export PORT=9000
export SPRING_PROFILES_ACTIVE=prod
java -jar target/dolphin-be-1.0.0.jar
```

### Docker Deployment (Future)

```dockerfile
FROM eclipse-temurin:25-jre-alpine
COPY target/dolphin-be-1.0.0.jar app.jar
ENTRYPOINT ["java", "-jar", "app.jar"]
```

## Project Structure

```
apps/dolphin-be/
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/opencode/dolphin/
│   │   │       └── DolphinApplication.java    # Main application class
│   │   └── resources/
│   │       ├── application.yml                # Main configuration
│   │       ├── application-dev.yml            # Development profile
│   │       ├── application-prod.yml           # Production profile
│   │       └── logback-spring.xml             # Logging configuration
│   └── test/
│       └── java/
│           └── com/opencode/dolphin/
│               └── DolphinApplicationTests.java
├── target/                                     # Build output (gitignored)
├── pom.xml                                     # Maven configuration
├── project.json                                # Nx integration
├── .gitignore                                  # Git ignore rules
└── README.md                                   # This file
```

## Configuration

### Profiles

Two Spring profiles available:

- `dev` - Development profile (DEBUG logging, detailed errors)
- `prod` - Production profile (INFO logging, JSON logs, restricted actuator)

Activate profile:

```bash
# Development (default)
mvn spring-boot:run -Dspring-boot.run.profiles=dev

# Production
java -jar target/dolphin-be-1.0.0.jar --spring.profiles.active=prod
```

### Logging

Development logging (text format):

```
2026-01-16 09:21:15.123 DEBUG 12345 --- [main] c.o.d.DolphinApplication : Starting...
```

Production logging (JSON format):

```json
{
  "timestamp": "2026-01-16T09:21:15.123Z",
  "level": "INFO",
  "thread": "main",
  "logger": "com.opencode.dolphin.DolphinApplication",
  "message": "Starting..."
}
```

## Troubleshooting

### Application fails to start

Check Java version:

```bash
java --version
# Must be Java 17+ (Java 25 recommended)
```

Check Maven can resolve dependencies:

```bash
cd apps/dolphin-be
mvn dependency:resolve
```

### Port already in use

Change port via environment variable:

```bash
PORT=9000 mvn spring-boot:run
```

### Tests fail

Clean and rebuild:

```bash
nx run dolphin-be:build
nx run dolphin-be:test
```

### Maven dependencies not resolving

Clear Maven cache:

```bash
rm -rf ~/.m2/repository
mvn clean install
```

## Technology Stack

- Java 25 (LTS)
- Spring Boot 4.0.x
- Spring Framework 7.0
- Maven 3.8+
- Jakarta EE 10
- Tomcat 11.x (embedded)
- JUnit 6.0 (testing)
- Mockito 5.21 (mocking)

## Next Steps

Future enhancements:

1. User authentication and authorization (Spring Security)
2. Database integration (Spring Data JPA, PostgreSQL)
3. REST API endpoints for users and courses
4. API documentation (OpenAPI/Swagger)
5. Caching layer (Redis)
6. Rate limiting
7. Monitoring (Prometheus, Grafana)
8. CI/CD pipeline

## License

MIT License - See repository root for details.

## Support

For issues or questions, refer to the repository documentation or create an issue.
