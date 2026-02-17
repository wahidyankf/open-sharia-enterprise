# organic-lever-be

Organic Lever Platform Backend - Spring Boot REST API

## Overview

- **Framework**: Spring Boot 4.0.2
- **Language**: Java 25
- **Build Tool**: Maven
- **Port**: 8100
- **API Base**: `/api/v1`
- **Client Apps**:
  - [`organic-lever-app`](../organic_lever_app/) - Flutter mobile and web application (port 3100)

**CORS Configuration**: The backend includes CORS configuration to allow Flutter web app on `http://localhost:*` (see `config/CorsConfig.java`).

## Prerequisites

- **Java 25** (managed via Volta or SDKMAN)
- **Maven 3.9+**
- **Docker & Docker Compose** (for containerized development)

## Development Modes

### Option 1: npm Scripts (Easiest - Recommended)

Use the convenient npm scripts from the repository root:

```bash
# Start development environment
npm run organic-lever:dev

# Restart (clean restart with fresh state)
npm run organic-lever:dev:restart
```

**Benefits**:

- ✅ Single command from anywhere in the repository
- ✅ No need to navigate to infra directory
- ✅ Consistent with other project scripts
- ✅ Auto-reload enabled (1-2 second restarts)

**Auto-reload workflow**:

1. Edit source files in `apps/organic-lever-be/src/`
2. Save changes (Ctrl+S)
3. Watch logs for restart message (1-2 seconds)
4. Test changes immediately

**First startup**: Takes 2-3 minutes for Maven to download dependencies
**Subsequent restarts**: 1-2 seconds via DevTools intelligent classloader

### Option 2: Docker Compose (Direct Control)

If you prefer direct Docker Compose control:

```bash
cd infra/dev/organic-lever

# First-time only: Build the custom dev image
docker compose build

# Start development environment
docker compose up
```

**What's happening**:

- Uses custom Docker image with Maven pre-installed (see `Dockerfile.dev`)
- Mounts source code into container (read-write)
- Runs `mvn spring-boot:run` with DevTools enabled
- DevTools watches for file changes and triggers fast restarts
- **Note**: Maven is NOT installed at runtime, saving 10-20 seconds on each startup

### Option 3: Local Maven (Fastest)

Run directly on host machine (0.5-1 second restarts):

```bash
# From repository root
nx serve organic-lever-be

# Or from app directory
cd apps/organic-lever-be
mvn spring-boot:run -Dspring-boot.run.profiles=dev
```

**Best for**: Rapid iteration, debugging with IDE

## Testing Auto-Reload

### Step 1: Start application

```bash
npm run organic-lever:dev
```

Wait for: `Started OrganicLeverApplication in X seconds`

### Step 2: Test baseline

```bash
curl http://localhost:8100/api/v1/hello
```

Expected: `{"message":"world"}`

### Step 3: Modify code

Edit `apps/organic-lever-be/src/main/java/com/organiclever/be/controller/HelloController.java`:

```java
return Map.of("message", "auto-reload works!");
```

Save file (Ctrl+S)

### Step 4: Verify reload

Watch Docker logs for:

```
Restarting due to 1 class path change
```

Test again (within 2 seconds):

```bash
curl http://localhost:8100/api/v1/hello
```

Expected: `{"message":"auto-reload works!"}`

## Production Deployment

### Step 1: Build JAR

```bash
# From repository root
nx build organic-lever-be

# Or from app directory
cd apps/organic-lever-be
mvn clean package -DskipTests
```

Output: `target/organic-lever-be-1.0.0.jar`

### Step 2: Run with production config

```bash
cd infra/dev/organic-lever
docker-compose -f docker-compose.yml up
```

**Production mode differences**:

- Uses JRE image (smaller, ~80MB vs 300MB JDK)
- No source mount (pre-built JAR only)
- No DevTools (excluded from JAR)
- No auto-reload (manual rebuild required)
- Faster startup (~10 seconds)

## Nx Commands

```bash
# Build JAR
nx build organic-lever-be

# Run tests
nx test organic-lever-be

# Start dev server (local Maven)
nx serve organic-lever-be

# Lint code
nx lint organic-lever-be
```

## Available Endpoints

### Application

- `GET /api/v1/hello` - Hello world endpoint

### Actuator (Monitoring)

- `GET /actuator/health` - Health check
- `GET /actuator/info` - Application info
- `GET /actuator/metrics` - Metrics

## Configuration Profiles

### dev (Development)

- **File**: `application-dev.yml`
- **Location**: Local machine
- **Deployment**: Docker Compose
- **DevTools**: Enabled (auto-reload)
- **Logging**: DEBUG level
- **Health**: Full details exposed

### staging (Staging Environment)

- **File**: `application-staging.yml`
- **Location**: Kubernetes cluster
- **Deployment**: Kubernetes
- **DevTools**: Excluded
- **Logging**: INFO level
- **Health**: Details when authorized

### prod (Production)

- **File**: `application-prod.yml`
- **Location**: Kubernetes cluster
- **Deployment**: Kubernetes
- **DevTools**: Excluded
- **Logging**: INFO level
- **Health**: Details hidden

## Troubleshooting

### Auto-reload not working

**Check DevTools is running**:

```bash
# Look for this in logs:
LiveReload server is running on port 35729
```

**Verify file changes are detected**:

```bash
# Watch container logs while editing files
docker-compose logs -f organic-lever-be
```

**Force restart**:

```bash
docker-compose restart organic-lever-be
```

### Port 8100 already in use

```bash
# Find process using port
lsof -i :8100

# Kill process
kill -9 <PID>
```

### Maven dependency download slow

First startup downloads ~100MB of dependencies. Subsequent starts are fast due to local Maven cache.

**Speed up next time**:

```bash
# Pre-download dependencies
cd apps/organic-lever-be
mvn dependency:go-offline
```

### Application won't start

**Check Java version**:

```bash
java -version
# Should show Java 25
```

**Check Maven version**:

```bash
mvn -version
# Should show Maven 3.9+
```

**Clear Maven cache**:

```bash
rm -rf ~/.m2/repository
mvn clean install
```

## Development Workflow

### 1. Feature Development

```bash
# Start dev environment
cd infra/dev/organic-lever
docker-compose up

# Edit code in apps/organic-lever-be/src/
# Save → Auto-reload (1-2 seconds)
# Test changes immediately
```

### 2. Running Tests

```bash
# From repository root
nx test organic-lever-be

# Or from app directory
cd apps/organic-lever-be
mvn test
```

### 3. Production Build

```bash
nx build organic-lever-be
cd infra/dev/organic-lever
docker-compose -f docker-compose.yml up
```

## Docker Development Image

The Docker-based development environment uses a custom image built from `infra/dev/organic-lever/Dockerfile.dev`:

**Key Features**:

- ✅ **Pre-installed Maven**: Maven 3.9.11 installed during image build, not at runtime
- ✅ **Faster startup**: Saves 10-20 seconds on each `docker compose up`
- ✅ **Consistent environment**: Same tooling for all developers
- ✅ **Isolated**: Maven installation contained to organic-lever-be service only

**Building the image** (first-time only):

```bash
cd infra/dev/organic-lever
docker compose build
```

**Image details**:

- Base: eclipse-temurin:25-jdk-alpine
- Maven: 3.9.11 (pre-installed via Alpine package manager)
- Size: ~666MB
- Build time: ~60-90 seconds

**When to rebuild**:

- Only if `Dockerfile.dev` changes
- Otherwise, the image persists and is reused automatically

## Architecture

```
apps/organic-lever-be/
├── src/main/java/com/organiclever/be/
│   ├── OrganicLeverApplication.java      # Main entry point
│   ├── config/
│   │   └── CorsConfig.java                # CORS configuration for Flutter web
│   └── controller/
│       └── HelloController.java           # REST endpoints
├── src/main/resources/
│   ├── application.yml                    # Base config
│   ├── application-dev.yml                # Dev config (DevTools)
│   └── application-prod.yml               # Prod config
└── src/test/java/
    └── OrganicLeverApplicationTests.java  # Integration tests
```

## Testing with Flutter Client

### 1. Start Backend

```bash
# From repository root
npm run organic-lever:dev
```

Wait for: `Started OrganicLeverApplication`

### 2. Start Flutter App

```bash
# From repository root
nx dev organic-lever-app
```

Open browser at `http://localhost:3100`

### 3. Test Integration

1. Click "Fetch Hello" button in Flutter app
2. Should display: ✅ Green checkmark + "world!" message
3. Check browser console for no CORS errors

**Note**: CORS is configured in `config/CorsConfig.java` to allow `http://localhost:*` for development.

## Next Steps

- Add database integration (PostgreSQL)
- Add security (Spring Security, JWT)
- Add API documentation (Swagger/OpenAPI)
- Add integration tests
- Add CI/CD pipeline
- Expand Flutter app with more features
