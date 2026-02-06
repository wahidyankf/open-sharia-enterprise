---
title: "Packaging and Deployment"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000070
description: "WAR deployment → executable JAR → layered JAR for optimized Docker deployment"
tags: ["spring-boot", "in-the-field", "production", "deployment", "packaging"]
---

## Why Packaging Matters

Spring Boot executable JARs eliminate WAR deployment complexity by packaging all dependencies in single runnable artifact. In production CI/CD pipelines, layered JARs optimize Docker rebuilds—separating rarely-changing dependencies (Spring Boot, libraries) from frequently-changing application code, reducing Docker push time from 5 minutes (150MB full image) to 30 seconds (5MB application layer only).

**Solution**: Spring Boot Maven Plugin with layered JAR configuration.

```xml
<build>
    <plugins>
        <plugin>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-maven-plugin</artifactId>
            <configuration>
                <layers>
                    <enabled>true</enabled>  <!-- Enable layered JAR -->
                </layers>
            </configuration>
        </plugin>
    </plugins>
</build>
```

```bash
mvn clean package
# => Produces target/zakat-api-1.0.0.jar (executable JAR)

java -jar target/zakat-api-1.0.0.jar
# => Runs application with embedded Tomcat
```

**Layered JAR structure**:

- **dependencies**: External libraries (Spring Boot, PostgreSQL driver) - rarely change
- **spring-boot-loader**: Boot loader classes - never change
- **snapshot-dependencies**: SNAPSHOT dependencies - change per build (dev only)
- **application**: Your application code - changes frequently

**Docker multi-stage build**:

```dockerfile
FROM eclipse-temurin:17-jdk as builder
WORKDIR /app
COPY target/zakat-api-1.0.0.jar app.jar
RUN java -Djarmode=layertools -jar app.jar extract

FROM eclipse-temurin:17-jre-alpine
WORKDIR /app
COPY --from=builder /app/dependencies/ ./
COPY --from=builder /app/spring-boot-loader/ ./
COPY --from=builder /app/snapshot-dependencies/ ./
COPY --from=builder /app/application/ ./
ENTRYPOINT ["java", "org.springframework.boot.loader.JarLauncher"]
```

**Rebuild optimization**:

- Dependencies layer cached (changes rarely)
- Only application layer rebuilt (changes frequently)
- **Result**: 5MB push vs 150MB full image

**Trade-offs**: Layered JARs essential for Docker deployment. Standard executable JAR sufficient for non-container deployment.

## Next Steps

- [Docker Containerization](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/docker-containerization) - Production Docker patterns
- [Embedded Servers](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/embedded-servers) - Server configuration
