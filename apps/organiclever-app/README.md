# OrganicLever App

Flutter mobile and web application for the OrganicLever Platform.

## Overview

This is a multi-platform Flutter application that provides a mobile and web client for the organiclever-be backend API. The app is integrated into the Nx monorepo using vanilla Flutter (no plugins), following the explicit over implicit principle.

**Current Status**: MVP - Basic API integration with /api/v1/hello endpoint

## Tech Stack

- **Flutter**: 3.41.1 (stable)
- **Dart**: 3.11.0
- **State Management**: Provider (official Flutter recommendation)
- **HTTP Client**: http package (official Dart package)
- **JSON Serialization**: json_annotation + build_runner
- **Platforms**: Web (primary), Android, iOS, Linux

## Architecture

```
lib/
├── config/
│   └── environment.dart        # Environment configuration
├── models/
│   ├── hello_response.dart     # Data models
│   └── hello_response.g.dart   # Generated serialization code
├── providers/
│   └── hello_provider.dart     # State management
├── services/
│   ├── api_client.dart         # HTTP client wrapper
│   └── api_service.dart        # Business logic for API endpoints
├── screens/
│   └── home_screen.dart        # UI screens
└── main.dart                   # App entry point

test/
├── unit/                       # Unit tests
│   └── hello_response_test.dart
└── widget/                     # Widget tests
    └── home_screen_test.dart
```

### Key Design Decisions

1. **Environment Configuration**: Uses `--dart-define` for compile-time configuration
2. **API Client**: Generic wrapper with error handling and type-safe JSON deserialization
3. **State Management**: Provider pattern with ChangeNotifier
4. **No Shared Libraries**: Everything in app for now (YAGNI principle)

## Development Commands

All commands should be run from the repository root using Nx:

```bash
# Install dependencies
nx install organiclever-app

# Run development server (web on port 3201)
nx dev organiclever-app

# Type check and lint (flutter analyze — combined pass)
nx run organiclever-app:typecheck

# Run fast quality gate (pre-push standard)
nx run organiclever-app:test:quick

# Run unit tests
nx run organiclever-app:test:unit

# Build for web (production)
nx build:web organiclever-app
```

### Direct Flutter Commands

If you need to run Flutter commands directly:

```bash
cd apps/organiclever_app

# Install dependencies
flutter pub get

# Run on web server (port 3201)
flutter run -d web-server --web-port=3201 --dart-define=API_BASE_URL=http://localhost:8201/api/v1

# Run on Chrome (requires Chrome installation)
flutter run -d chrome --dart-define=API_BASE_URL=http://localhost:8201/api/v1

# Run tests
flutter test

# Analyze code
flutter analyze

# Build for web
flutter build web --dart-define=API_BASE_URL=https://api.organiclever.com/api/v1
```

### Code Generation

When adding or modifying models with JSON serialization:

```bash
flutter pub run build_runner build --delete-conflicting-outputs
```

## Environment Configuration

The app uses `--dart-define` for environment-specific configuration:

**Development** (default):

```bash
API_BASE_URL=http://localhost:8201/api/v1
```

**Production**:

```bash
API_BASE_URL=https://api.organiclever.com/api/v1
```

Configuration is centralized in `lib/config/environment.dart`.

## Backend Connectivity

The app connects to the organiclever-be Spring Boot backend:

**Prerequisites**:

1. Backend must be running on `http://localhost:8201`
2. CORS must be configured to allow the Flutter web origin

**Starting the Backend**:

```bash
# From repository root
npm run organiclever:dev
```

**IMPORTANT**: The backend must have CORS configured to allow `http://localhost:*` origin for development.

## Testing

Three-tier testing strategy:

1. **Unit Tests** (`test/unit/`): Model serialization, business logic
2. **Widget Tests** (`test/widget/`): UI components and state changes
3. **Integration Tests** (`test/integration/`): End-to-end flows (future)

**Run tests**:

```bash
# Fast quality gate (pre-push standard)
nx run organiclever-app:test:quick

# Isolated unit tests
nx run organiclever-app:test:unit
```

## Nx Integration

This app uses vanilla Flutter with `nx:run-commands` executor (no Nx plugins):

- Follows existing workspace pattern (Maven for Java, native tooling for Go/Hugo)
- Explicit commands in `project.json`
- Direct Flutter CLI usage
- Tags: `type:app`, `platform:flutter`, `domain:organiclever`

**View in dependency graph**:

```bash
nx graph
```

## Platform Support

**Web** (Primary):

- Development: `http://localhost:3201`
- No browser installation required (uses web-server)
- CORS configuration needed in backend

**Android**:

```bash
flutter run -d android --dart-define=API_BASE_URL=http://10.0.2.2:8201/api/v1
```

Note: Use `10.0.2.2` for Android emulator's localhost

**iOS** (macOS only):

```bash
flutter run -d iPhone
```

**Linux Desktop**:

Requires: clang++, cmake, ninja, pkg-config

```bash
flutter run -d linux
```

## E2E Testing

The [`organiclever-app-web-e2e`](../organiclever-app-web-e2e/) project provides Playwright-based
browser E2E tests for this Flutter web app. Run them after starting both services:

```bash
# Start backend
npm run organiclever:dev

# Start Flutter web app (separate terminal)
nx dev organiclever-app

# Run browser E2E tests
nx run organiclever-app-web-e2e:test:e2e
```

Tests cover:

- HomeScreen initial state — app title and "Fetch Hello" button visible
- Fetch interaction — "API Response:" label and `world!` message displayed after clicking

## Future Scalability

**When app grows beyond 10 screens**:

- Add routing with `go_router` package
- Add dependency injection with `get_it`
- Migrate to Riverpod for better state management
- Extract API client to `libs/dart-api-client`
- Implement feature-based folder structure

**When backend adds authentication**:

- Migrate to `dio` for interceptors
- Add `secure_storage` for JWT tokens
- Implement token refresh logic
- Add login/logout screens

## Troubleshooting

### CORS Errors in Web

**Symptom**: `XMLHttpRequest error` in browser console

**Solution**: Ensure backend has CORS configured:

```java
@Configuration
public class CorsConfig implements WebMvcConfigurer {
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/api/**")
                .allowedOrigins("http://localhost:*")
                .allowedMethods("GET", "POST", "PUT", "DELETE")
                .allowedHeaders("*");
    }
}
```

### Flutter Not Found

**Symptom**: `flutter: command not found`

**Solution**: Add Flutter to PATH:

```bash
export PATH="$PATH:$HOME/flutter/bin"
```

Or add to `~/.zshrc`:

```bash
echo 'export PATH="$PATH:$HOME/flutter/bin"' >> ~/.zshrc
source ~/.zshrc
```

### Build Runner Errors

**Symptom**: `*.g.dart` file not generated

**Solution**: Run code generation:

```bash
flutter pub run build_runner build --delete-conflicting-outputs
```

### Test Failures

**Symptom**: HTTP-related test failures

**Solution**: Tests use mock HTTP client. Network requests in tests return 400. Provide mock implementations in tests.

## License

MIT
