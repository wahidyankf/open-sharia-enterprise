# Technical Documentation for Organic Lever Backend Initialization

## Project Structure

```
apps/organic-lever-be/
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/
│   │   │       └── opencode/
│   │   │           └── organiclever/
│   │   │               ├── OrganicLeverApplication.java  # Main application class
│   │   │               ├── controller/
│   │   │               │   └── HelloController.java      # /api/v1/hello endpoint
│   │   │               ├── dto/
│   │   │               │   └── MessageResponse.java      # Response DTO
│   │   │               ├── service/                      # Business logic (future)
│   │   │               ├── repository/                   # Data access (future)
│   │   │               └── model/                        # Domain models (future)
│   │   └── resources/
│   │       ├── application.yml                            # Main configuration (port 8100)
│   │       ├── application-dev.yml                        # Dev profile
│   │       ├── application-prod.yml                       # Production profile
│   │       └── logback-spring.xml                         # Logging config
│   └── test/
│       └── java/
│           └── com/
│               └── opencode/
│                   └── organiclever/
│                       ├── OrganicLeverApplicationTests.java
│                       └── controller/
│                           └── HelloControllerTest.java
├── pom.xml                                                # Maven configuration
├── project.json                                           # Nx integration
├── .gitignore                                            # Git ignore rules
└── README.md                                             # Documentation
```

## Maven Dependencies

### Complete pom.xml Configuration

**Location**: `apps/organic-lever-be/pom.xml`

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <parent>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter-parent</artifactId>
        <version>4.0.1</version>
        <relativePath/>
    </parent>

    <groupId>com.opencode</groupId>
    <artifactId>organic-lever-be</artifactId>
    <version>1.0.0</version>
    <name>organic-lever-be</name>
    <description>Organic Lever Backend - REST API Service</description>

    <properties>
        <java.version>25</java.version>
        <maven.compiler.source>25</maven.compiler.source>
        <maven.compiler.target>25</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <!-- Note: Spring Boot 4.0 supports Java 17-25, with 17 as minimum baseline -->
    </properties>

    <dependencies>
        <!-- Core Spring Boot Dependencies -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-web</artifactId>
        </dependency>

        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-validation</artifactId>
        </dependency>

        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-actuator</artifactId>
        </dependency>

        <!-- Logging: Logstash encoder for JSON logging in production -->
        <dependency>
            <groupId>net.logstash.logback</groupId>
            <artifactId>logstash-logback-encoder</artifactId>
            <version>9.0</version>
        </dependency>

        <!-- Test Dependencies -->
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-test</artifactId>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.springframework.boot</groupId>
                <artifactId>spring-boot-maven-plugin</artifactId>
            </plugin>

            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <configuration>
                    <source>25</source>
                    <target>25</target>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
```

### Spring Boot Parent

**Version**: 4.0.1 (latest stable as of February 2026)

The Spring Boot parent POM manages all dependency versions, ensuring compatibility with Java 25.

### Core Dependencies

**spring-boot-starter-web**

- REST API support with embedded Tomcat 11.x
- Jackson for JSON serialization
- Spring MVC for web layer
- Built on Spring Framework 7.0 GA

**spring-boot-starter-validation**

- Bean validation (Jakarta Bean Validation 3.x)
- Hibernate Validator 8.x
- Automatic validation of @Valid annotated parameters
- Fully Jakarta EE 10 compliant (jakarta.validation.*)

**spring-boot-starter-actuator**

- Health check endpoints
- Metrics collection
- Application information endpoints
- Enhanced observability with Micrometer 2.0+

### Testing Dependencies

**spring-boot-starter-test**

- JUnit 6.0 testing framework
- Mockito 5.21+ for mocking
- Spring Boot Test 4.0.x for integration tests
- AssertJ 3.27+ for fluent assertions
- Support for virtual threads (Java 21+ feature)

### Spring Boot 4.0 and Java 25 Considerations

**Jakarta EE 10 Migration**

- Spring Boot 4.0.x uses Jakarta EE 10 (jakarta.* namespace instead of javax.*)
- All imports must use `jakarta.persistence.*`, `jakarta.validation.*`, `jakarta.servlet.*`, etc.
- This is a breaking change from Spring Boot 2.x and Spring Boot 3.0-3.2
- No migration needed if starting fresh with Spring Boot 4.0

**Java 25 Features**

- Virtual Threads (Project Loom) - improved concurrency
- Pattern Matching for switch - enhanced pattern matching
- Record Patterns - pattern matching with records
- String Templates - string interpolation
- Sequenced Collections - ordered collection APIs
- Performance improvements in garbage collection (ZGC)

**Spring Boot 4.0 Changes**

- Built on Spring Framework 7.0 GA
- Tomcat 11.x (upgraded from 10.x in 3.x)
- Hibernate 7.1 (if database is added later)
- Enhanced observability with Micrometer 2.0+
- Improved virtual thread support
- Better AOT compilation support

## Source Code

### Main Application Class

**Location**: `src/main/java/com/opencode/organiclever/OrganicLeverApplication.java`

```java
package com.opencode.organiclever;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Main application class for Organic Lever Backend.
 * This is a simple REST API service running on port 8100.
 */
@SpringBootApplication
public class OrganicLeverApplication {

    public static void main(String[] args) {
        SpringApplication.run(OrganicLeverApplication.class, args);
    }
}
```

### Hello Controller

**Location**: `src/main/java/com/opencode/organiclever/controller/HelloController.java`

```java
package com.opencode.organiclever.controller;

import com.opencode.organiclever.dto.MessageResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Controller for hello endpoint.
 * Provides a simple API endpoint to verify the service is running.
 */
@RestController
@RequestMapping("/api/v1")
public class HelloController {

    /**
     * Returns a simple hello message.
     *
     * @return MessageResponse with "world" message
     */
    @GetMapping("/hello")
    public ResponseEntity<MessageResponse> hello() {
        MessageResponse response = new MessageResponse("world");
        return ResponseEntity.ok(response);
    }
}
```

### Message Response DTO

**Location**: `src/main/java/com/opencode/organiclever/dto/MessageResponse.java`

```java
package com.opencode.organiclever.dto;

/**
 * Response DTO for message endpoints.
 * Uses Java record for immutable data transfer object.
 *
 * @param message the message content
 */
public record MessageResponse(String message) {
}
```

**Note**: Using Java `record` (available since Java 14, stable in 16+) provides:

- Immutable data structure
- Automatic equals(), hashCode(), toString()
- Compact syntax
- Built-in JSON serialization with Jackson

## Configuration Details

### application.yml (Main Configuration)

**Location**: `src/main/resources/application.yml`

```yaml
spring:
  application:
    name: organic-lever-be
  profiles:
    active: dev

server:
  port: 8100
  error:
    include-message: always
    include-binding-errors: always

management:
  endpoints:
    web:
      exposure:
        include: health,info,metrics
      base-path: /actuator
  endpoint:
    health:
      show-details: when-authorized
  info:
    app:
      name: organic-lever-be
      description: Organic Lever Backend - REST API Service
      version: 1.0.0
```

**Configuration Notes**:

- `spring.profiles.active: dev` - Activate development profile by default
- `server.port: 8100` - Run on port 8100 (different from default 8080)
- `server.error.include-message: always` - Show error messages in response
- `management.endpoints.web.exposure.include` - Expose only necessary actuator endpoints
- `management.endpoint.health.show-details: when-authorized` - Hide details from unauthorized users in production
- `management.info.app` - Application metadata for /actuator/info endpoint

### application-dev.yml (Development Profile)

**Location**: `src/main/resources/application-dev.yml`

```yaml
logging:
  level:
    com.opencode.organiclever: DEBUG
    org.springframework.web: INFO

server:
  error:
    include-stacktrace: always
```

**Configuration Notes**:

- DEBUG logging for application code
- INFO logging for Spring framework
- Include stack traces in error responses (dev only)

### application-prod.yml (Production Profile)

**Location**: `src/main/resources/application-prod.yml`

```yaml
logging:
  level:
    com.opencode.organiclever: INFO
    org.springframework.web: WARN

server:
  error:
    include-stacktrace: never

management:
  endpoint:
    health:
      show-details: never
```

**Configuration Notes**:

- INFO logging for application
- WARN logging for Spring framework
- Never include stack traces in production
- Hide health check details in production

## Nx Integration (project.json)

**Location**: `apps/organic-lever-be/project.json`

```json
{
  "name": "organic-lever-be",
  "$schema": "../../node_modules/nx/schemas/project-schema.json",
  "sourceRoot": "apps/organic-lever-be/src",
  "projectType": "application",
  "targets": {
    "build": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn clean package -DskipTests",
        "cwd": "apps/organic-lever-be"
      }
    },
    "test": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn test",
        "cwd": "apps/organic-lever-be"
      }
    },
    "serve": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn spring-boot:run -Dspring-boot.run.profiles=dev",
        "cwd": "apps/organic-lever-be"
      }
    },
    "lint": {
      "executor": "nx:run-commands",
      "options": {
        "command": "mvn checkstyle:check",
        "cwd": "apps/organic-lever-be"
      }
    }
  },
  "tags": ["type:app", "platform:jvm", "framework:spring-boot"]
}
```

**Nx Targets**:

- `nx run organic-lever-be:build` - Build the JAR file
- `nx run organic-lever-be:test` - Run all tests
- `nx run organic-lever-be:serve` - Start development server on port 8100
- `nx run organic-lever-be:lint` - Run code style checks

## API Design

### Hello Endpoint

#### GET /api/v1/hello

**Description**: Simple hello endpoint that returns a JSON message.

**Request**:

```http
GET /api/v1/hello HTTP/1.1
Host: localhost:8100
Accept: application/json
```

**Response (200 OK)**:

```http
HTTP/1.1 200 OK
Content-Type: application/json
Content-Length: 19

{
  "message": "world"
}
```

**cURL Example**:

```bash
curl -X GET http://localhost:8100/api/v1/hello
```

### Health Check Endpoints

#### GET /actuator/health

**Response (200 OK)**:

```json
{
  "status": "UP",
  "components": {
    "diskSpace": {
      "status": "UP",
      "details": {
        "total": 500000000000,
        "free": 400000000000,
        "threshold": 10485760,
        "path": "/home/wkf/wkf-repos/."
      }
    },
    "ping": {
      "status": "UP"
    }
  },
  "groups": ["liveness", "readiness"]
}
```

#### GET /actuator/info

**Response (200 OK)**:

```json
{
  "app": {
    "name": "organic-lever-be",
    "description": "Organic Lever Backend - REST API Service",
    "version": "1.0.0"
  }
}
```

## Security Considerations

### Environment Variables

Sensitive configuration should be injected via environment variables:

- PORT - Server port (default: 8100)
- SPRING_PROFILES_ACTIVE - Active profile (dev/prod)

### Actuator Security

- Development: All actuator endpoints exposed
- Production: Only `/actuator/health` and `/actuator/info` exposed
- Future: Add authentication for actuator endpoints

### Git Protection

Files with sensitive data should be in `.gitignore`:

- `.env` files - Environment-specific secrets
- `target/` - Build artifacts
- IDE configuration files

## Logging Configuration

### logback-spring.xml Configuration

**Location**: `src/main/resources/logback-spring.xml`

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration>
    <!-- Development Profile: Text logging with colors -->
    <springProfile name="dev">
        <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
            <encoder>
                <pattern>%d{yyyy-MM-dd HH:mm:ss.SSS} %5p ${PID:- } --- [%15.15t] %-40.40logger{39} : %m%n</pattern>
            </encoder>
        </appender>
        <root level="INFO">
            <appender-ref ref="CONSOLE"/>
        </root>
        <logger name="com.opencode.organiclever" level="DEBUG"/>
        <logger name="org.springframework.web" level="INFO"/>
    </springProfile>

    <!-- Production Profile: JSON logging -->
    <springProfile name="prod">
        <appender name="JSON" class="ch.qos.logback.core.ConsoleAppender">
            <encoder class="net.logstash.logback.encoder.LogstashEncoder">
                <includeMdc>true</includeMdc>
                <includeContext>true</includeContext>
            </encoder>
        </appender>
        <root level="INFO">
            <appender-ref ref="JSON"/>
        </root>
        <logger name="com.opencode.organiclever" level="INFO"/>
        <logger name="org.springframework.web" level="WARN"/>
    </springProfile>
</configuration>
```

### Development Logging

**Format**: Text with color codes
**Level**: DEBUG for application, INFO for frameworks
**Output**: Console

**Sample Output**:

```
2026-02-11 10:15:30.123 DEBUG 12345 --- [main] c.o.o.OrganicLeverApplication : Starting OrganicLeverApplication...
2026-02-11 10:15:31.456  INFO 12345 --- [main] o.s.b.w.embedded.tomcat.TomcatWebServer : Tomcat started on port(s): 8100
```

### Production Logging

**Format**: JSON (structured logging)
**Level**: INFO for application, WARN for frameworks
**Output**: Console (for container logs)

**Sample Output**:

```json
{
  "timestamp": "2026-02-11T10:15:30.123Z",
  "level": "INFO",
  "thread": "http-nio-8100-exec-1",
  "logger": "com.opencode.organiclever.controller.HelloController",
  "message": "Hello endpoint accessed",
  "mdc": {
    "requestId": "abc123"
  }
}
```

## Testing Strategy

### Unit Tests

#### OrganicLeverApplicationTests.java

**Location**: `src/test/java/com/opencode/organiclever/OrganicLeverApplicationTests.java`

```java
package com.opencode.organiclever;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class OrganicLeverApplicationTests {

    @Test
    void contextLoads() {
        // Verify Spring Boot context loads successfully
    }
}
```

#### HelloControllerTest.java

**Location**: `src/test/java/com/opencode/organiclever/controller/HelloControllerTest.java`

```java
package com.opencode.organiclever.controller;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureMockMvc
class HelloControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    void helloEndpointReturnsExpectedMessage() throws Exception {
        mockMvc.perform(get("/api/v1/hello"))
                .andExpect(status().isOk())
                .andExpect(content().contentType("application/json"))
                .andExpect(jsonPath("$.message").value("world"));
    }
}
```

### Integration Tests

- Use `@SpringBootTest` for full context tests
- Use `MockMvc` for API endpoint testing
- Verify JSON response structure and content
- Test health check endpoints

## Build and Deployment

### Build Process

```bash
# Using Maven directly
mvn clean package -DskipTests

# Using Nx
nx run organic-lever-be:build
```

**Output**: `target/organic-lever-be-1.0.0.jar`

### Running the Application

**Development**:

```bash
# Using Maven
mvn spring-boot:run -Dspring-boot.run.profiles=dev

# Using Nx
nx run organic-lever-be:serve

# Access endpoints
curl http://localhost:8100/api/v1/hello
curl http://localhost:8100/actuator/health
```

**Production**:

```bash
java -jar target/organic-lever-be-1.0.0.jar --spring.profiles.active=prod
```

### Docker Deployment (Future)

```dockerfile
FROM eclipse-temurin:25-jre-alpine
WORKDIR /app
COPY target/organic-lever-be-1.0.0.jar app.jar
EXPOSE 8100
ENTRYPOINT ["java", "-jar", "app.jar"]
```

**Build and run**:

```bash
docker build -t organic-lever-be:1.0.0 .
docker run -p 8100:8100 organic-lever-be:1.0.0
```

**Note**: Eclipse Temurin is the recommended production-ready OpenJDK distribution.

## Performance Considerations

### Startup Time

Target: < 10 seconds

Optimizations:

- Use Spring Boot 4.0.x with built-in performance improvements
- Enable AOT compilation for native image (future)
- Lazy initialization (future)
- Exclude unnecessary auto-configurations (future)

### Memory Usage

Target: < 512 MB heap

Optimizations:

- Tune JVM heap size (`-Xms256m -Xmx512m`)
- Enable ZGC garbage collector (default for Java 21+)
- Consider virtual threads for high concurrency

### Response Time

Target: < 100ms for /api/v1/hello endpoint

The simple endpoint should have minimal latency as it:

- Has no database queries
- Has no external API calls
- Returns a static response

## Monitoring and Observability

### Actuator Metrics

Spring Boot Actuator provides built-in metrics:

- JVM memory usage
- HTTP request metrics
- Custom metrics (future)

### Future Enhancements

- Prometheus metrics export
- Grafana dashboards
- Distributed tracing (OpenTelemetry)
- Application Performance Monitoring (APM)

## Git Configuration

### .gitignore

**Location**: `apps/organic-lever-be/.gitignore`

```gitignore
# Maven
target/
pom.xml.tag
pom.xml.releaseBackup
pom.xml.versionsBackup
pom.xml.next
release.properties
dependency-reduced-pom.xml
buildNumber.properties
.mvn/timing.properties
.mvn/wrapper/maven-wrapper.jar

# Maven wrapper (optional, can be committed)
mvnw
mvnw.cmd

# IDE
.idea/
*.iml
*.iws
*.ipr
.vscode/
.classpath
.project
.settings/

# Environment
.env
.env.local
.env.*.local

# Logs
*.log

# OS
.DS_Store
Thumbs.db
```

## Docker Compose Integration

### Overview

The organic-lever-be service includes Docker Compose configuration for reproducible local development. This ensures all developers work in identical environments.

**Location**: `infra/local/organic-lever/`

### Configuration

The Docker Compose setup provides:

- **Containerized Environment**: Eclipse Temurin 25 JRE Alpine
- **Volume Mounting**: JAR file from build output
- **Port Mapping**: Service accessible on port 8100
- **Health Checks**: Automatic monitoring via actuator
- **Environment Configuration**: Profile selection (dev/prod)
- **Auto-Restart**: Service resilience

### Usage

The application README should include a "Docker Development" section:

````markdown
## Docker Development

For reproducible local development using Docker:

### Prerequisites

- Docker 20.10+
- Docker Compose 2.0+

### Quick Start

1. **Build the application**:
   ```bash
   mvn clean package -DskipTests
   # or
   nx run organic-lever-be:build
   ```

2. **Start with Docker Compose**:
   ```bash
   cd ../../infra/local/organic-lever
   docker-compose up -d
   ```

3. **Verify service**:
   ```bash
   curl http://localhost:8100/api/v1/hello
   # Expected: {"message":"world"}
   ```

4. **View logs**:
   ```bash
   docker-compose logs -f organic-lever-be
   ```

5. **Stop services**:
   ```bash
   docker-compose down
   ```

### Configuration

Copy `.env.example` to `.env` to customize:

```bash
cd ../../infra/local/organic-lever
cp .env.example .env
nano .env  # Edit as needed
```

For detailed Docker operations, see [Infrastructure Documentation](../../infra/local/organic-lever/README.md).
````

### Integration with Application README

The `apps/organic-lever-be/README.md` should include:

1. **Development Modes Section**:
   - Local development (Maven/Nx)
   - Docker development (Docker Compose)

2. **Prerequisites Section**:
   - Add Docker and Docker Compose to prerequisites
   - Mark as optional for non-Docker development

3. **Quick Reference Table**:

```markdown
| Method | Command | Port |
|--------|---------|------|
| Maven Direct | `mvn spring-boot:run` | 8100 |
| Nx | `nx run organic-lever-be:serve` | 8100 |
| Docker Compose | See [Docker Docs](../../infra/local/organic-lever/README.md) | 8100 |
```

### Benefits

- **Consistency**: All developers use identical environment
- **Isolation**: No conflicts with other local services
- **Production Parity**: Closer to production deployment
- **Easy Onboarding**: New developers get started faster
- **Clean Setup**: No need to install Java/Maven locally (optional)
