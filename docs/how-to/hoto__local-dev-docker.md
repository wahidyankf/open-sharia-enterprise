# How to Set Up Reproducible Local Development with Docker

## Overview

This guide explains how to set up a reproducible local development environment using Docker and Docker Compose for all services in the open-sharia-enterprise platform.

**Goal**: Ensure all developers work in identical environments, regardless of their host operating system or installed tools.

## Prerequisites

- **Docker**: 20.10+ ([Install Docker](https://docs.docker.com/get-docker/))
- **Docker Compose**: 2.0+ (included with Docker Desktop)
- **Git**: For cloning the repository

**Optional** (only needed if building applications):

- Language-specific tools (Java, Node.js, etc.) for building before containerization
- Or use Docker multi-stage builds (future enhancement)

## Benefits of Docker-Based Development

1. **Environment Consistency**: All developers use identical runtime environments
2. **Isolation**: Services don't conflict with host system or each other
3. **Production Parity**: Development environment matches production deployment
4. **Easy Onboarding**: New developers get started in minutes
5. **Clean System**: No need to install multiple language runtimes
6. **Network Simulation**: Test service-to-service communication locally

## Repository Structure

Docker Compose configurations are organized by deployment target:

```
infra/
├── local/                    # Local development environments
│   ├── organic-lever/       # Organic Lever services ecosystem
│   │   ├── docker-compose.yml
│   │   ├── .env.example
│   │   └── README.md
│   └── [other-service]/     # Other service ecosystems
├── cloud/                    # Cloud deployment configs (future)
└── k8s/                      # Kubernetes configs (future)
```

## Quick Start

### 1. Clone Repository

```bash
git clone https://github.com/wahidyankf/open-sharia-enterprise.git
cd open-sharia-enterprise
```

### 2. Choose Your Service Ecosystem

Each service ecosystem has its own Docker Compose setup:

```bash
# Example: Organic Lever services
cd infra/local/organic-lever
```

### 3. Configure Environment (Optional)

```bash
# Copy environment template
cp .env.example .env

# Edit configuration (optional, defaults work)
nano .env
```

### 4. Build Application (If Needed)

Some services require building before containerization:

```bash
# Example: Spring Boot application
cd ../../../apps/organic-lever-be
mvn clean package -DskipTests

# Or using Nx
nx run organic-lever-be:build

# Return to Docker Compose directory
cd ../../infra/local/organic-lever
```

### 5. Start Services

```bash
# Start all services in the ecosystem
docker-compose up -d

# View logs
docker-compose logs -f

# Check status
docker-compose ps
```

### 6. Verify Services

```bash
# Example: Test organic-lever-be
curl http://localhost:8100/api/v1/hello
# Expected: {"message":"world"}

curl http://localhost:8100/actuator/health
# Expected: {"status":"UP"}
```

### 7. Stop Services

```bash
# Stop services
docker-compose down

# Stop and remove volumes
docker-compose down -v
```

## Available Service Ecosystems

### Organic Lever (`infra/local/organic-lever/`)

**Services**:

- `organic-lever-be` - Spring Boot backend (port 8100)

**Quick Start**:

```bash
cd infra/local/organic-lever
docker-compose up -d
```

**Documentation**: [Organic Lever Infrastructure README](../../infra/local/organic-lever/README.md)

### Future Ecosystems

Additional service ecosystems will be added as the platform grows.

## Common Operations

### View Logs

```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f organic-lever-be

# Last 100 lines
docker-compose logs --tail=100 organic-lever-be
```

### Restart Services

```bash
# Restart all
docker-compose restart

# Restart specific service
docker-compose restart organic-lever-be
```

### Rebuild After Code Changes

```bash
# 1. Rebuild the application
cd ../../apps/organic-lever-be
mvn clean package -DskipTests

# 2. Restart the service
cd ../../infra/local/organic-lever
docker-compose restart organic-lever-be
```

### Check Resource Usage

```bash
# Docker stats
docker stats

# Compose service status
docker-compose ps
```

### Clean Up

```bash
# Stop and remove containers
docker-compose down

# Remove volumes (caution: deletes data)
docker-compose down -v

# Remove unused Docker resources
docker system prune
```

## Development Workflows

### Workflow 1: Pure Docker Development

**Best for**: Frontend developers, new team members, quick testing

```bash
# 1. Build application (one time or after changes)
nx run organic-lever-be:build

# 2. Start services
cd infra/local/organic-lever
docker-compose up -d

# 3. Make changes to code
# 4. Rebuild and restart
nx run organic-lever-be:build
docker-compose restart organic-lever-be

# 5. Test changes
curl http://localhost:8100/api/v1/hello
```

### Workflow 2: Hybrid Development

**Best for**: Backend developers, active development

```bash
# Run service directly for faster iteration
cd apps/organic-lever-be
mvn spring-boot:run

# Run dependent services in Docker
cd ../../infra/local/organic-lever
# Comment out the service you're developing
# Keep database, cache, etc. in Docker
docker-compose up -d
```

### Workflow 3: Full Local Development

**Best for**: System integration testing

```bash
# Run all services in Docker
cd infra/local/organic-lever
docker-compose up -d

# Test inter-service communication
# All services on same network
```

## Environment Configuration

### Using .env Files

Each service ecosystem supports environment configuration:

```bash
# Copy template
cp .env.example .env

# Edit variables
nano .env
```

**Common variables**:

```bash
# Spring Boot Profile
SPRING_PROFILES_ACTIVE=dev  # or prod

# JVM Options
JAVA_OPTS=-Xms512m -Xmx1024m -XX:+UseZGC

# Database (future)
DB_HOST=organic-lever-db
DB_PORT=5432
```

### Profile Selection

Services support multiple profiles:

- **dev**: Development mode with verbose logging
- **prod**: Production mode with JSON logging

```bash
# Set in .env file
SPRING_PROFILES_ACTIVE=dev
```

## Networking

### Service Communication

Services in the same Docker Compose network can communicate by service name:

```bash
# Example: Frontend calling backend
http://organic-lever-be:8100/api/v1/hello

# Example: Backend connecting to database
jdbc:postgresql://organic-lever-db:5432/organic_lever
```

### Port Mapping

Services expose ports to the host:

| Service | Internal Port | Host Port | Purpose |
|---------|--------------|-----------|---------|
| organic-lever-be | 8100 | 8100 | Backend API |
| (future) organic-lever-fe | 80 | 3100 | Frontend |
| (future) organic-lever-db | 5432 | 5432 | Database |

## Health Checks

Services include health checks for monitoring:

```bash
# Check health via Docker
docker-compose ps

# Check health via endpoint
curl http://localhost:8100/actuator/health
```

**Health check features**:

- Automatic restarts on failure
- Startup grace period
- Configurable intervals and retries

## Troubleshooting

### Service Won't Start

```bash
# Check logs
docker-compose logs service-name

# Check if port is in use
lsof -i :8100

# Verify application was built
ls -lh ../../apps/organic-lever-be/target/*.jar
```

### Port Already in Use

```bash
# Find what's using the port
lsof -i :8100

# Kill the process
kill -9 <PID>

# Or change port in docker-compose.yml
```

### Out of Memory

```bash
# Increase Docker memory limit (Docker Desktop)
# Settings > Resources > Memory

# Or adjust JVM heap in .env
JAVA_OPTS=-Xms256m -Xmx512m -XX:+UseZGC
```

### Cannot Connect to Service

```bash
# Verify service is running
docker-compose ps

# Check if service is healthy
docker inspect organic-lever-be | grep -A 10 Health

# Test from inside container
docker exec organic-lever-be wget -O- http://localhost:8100/actuator/health
```

### Build Failed

```bash
# Check build logs
mvn clean package

# Verify Java version
java -version

# Clear Maven cache
rm -rf ~/.m2/repository/com/opencode
```

## Best Practices

### 1. Always Use Version Pinning

```yaml
# Good: Specific version
image: eclipse-temurin:25-jre-alpine

# Bad: Latest tag (unpredictable)
image: eclipse-temurin:latest
```

### 2. Use .env Files, Never Commit Secrets

```bash
# Good: Use .env.example as template
cp .env.example .env

# Bad: Hardcode secrets in docker-compose.yml
```

### 3. Mount Volumes Read-Only When Possible

```yaml
volumes:
  - ../../apps/organic-lever-be/target/app.jar:/app/app.jar:ro
```

### 4. Include Health Checks

```yaml
healthcheck:
  test: ["CMD", "wget", "--spider", "http://localhost:8100/actuator/health"]
  interval: 30s
  timeout: 10s
  retries: 3
```

### 5. Use Named Networks

```yaml
networks:
  organic-lever-network:
    driver: bridge
    name: organic-lever-network
```

### 6. Document Port Assignments

Maintain a central registry of ports to avoid conflicts:

- 8100: organic-lever-be
- 3100: organic-lever-fe (future)
- 5432: PostgreSQL databases

## Performance Tips

### 1. Use BuildKit

```bash
# Enable Docker BuildKit for faster builds
export DOCKER_BUILDKIT=1
```

### 2. Optimize Image Size

- Use Alpine-based images
- Multi-stage builds for compiled languages
- Remove unnecessary files

### 3. Cache Dependencies

```bash
# For Maven, cache .m2 directory
volumes:
  - ~/.m2:/root/.m2:cached
```

### 4. Limit Resource Usage

```yaml
deploy:
  resources:
    limits:
      cpus: '1.0'
      memory: 1024M
    reservations:
      cpus: '0.5'
      memory: 512M
```

## Migration from Local Development

### Before: Local Development

```bash
# Install Java 25
# Install Maven
# Install Node.js
# Configure environment variables
# Run application
mvn spring-boot:run
```

### After: Docker Development

```bash
# Install Docker (one time)
# Build application
mvn clean package
# Start with Docker Compose
docker-compose up -d
```

### Advantages

- No Java/Maven installation needed (optional)
- Consistent environment across team
- Easier onboarding for new developers
- Production parity

## CI/CD Integration

Docker Compose can be used in CI/CD pipelines:

```bash
# GitHub Actions example
- name: Start services
  run: docker-compose up -d

- name: Run tests
  run: docker-compose exec organic-lever-be mvn test

- name: Stop services
  run: docker-compose down
```

## Related Documentation

- [Organic Lever Infrastructure README](../../infra/local/organic-lever/README.md)
- [Docker Documentation](https://docs.docker.com/)
- [Docker Compose Documentation](https://docs.docker.com/compose/)
- [Reproducible Environments Convention](../explanation/software-engineering/ex-soen__reproducible-environments.md)

## Support

For issues or questions:

1. Check service-specific README in `infra/local/[service]/`
2. Review troubleshooting section above
3. Check Docker logs: `docker-compose logs`
4. Consult main repository documentation
