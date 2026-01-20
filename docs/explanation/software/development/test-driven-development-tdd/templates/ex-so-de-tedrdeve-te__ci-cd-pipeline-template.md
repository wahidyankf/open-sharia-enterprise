# CI/CD Pipeline Template

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [Integration Test Template](./ex-so-de-tedrdeve-te__integration-test-template.md)
  - [Test Organization Template](./ex-so-de-tedrdeve-te__test-organization-template.md)
  - [Coverage Planning Canvas](./ex-so-de-tedrdeve-te__coverage-planning-canvas.md)
- **Use Case**: Automating test execution in CI/CD pipelines
- **Template Size**: ~18 KB
- **Complexity**: Intermediate to Advanced

## Overview

This template provides CI/CD pipeline configurations for running automated tests on every commit, pull request, and deployment. It includes examples for GitHub Actions, GitLab CI, and Nx-specific optimizations.

## GitHub Actions Template for Nx Monorepo

### Complete Workflow File

```yaml
# .github/workflows/ci.yml
name: CI

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

env:
  NX_CLOUD_ACCESS_TOKEN: ${{ secrets.NX_CLOUD_ACCESS_TOKEN }}
  NODE_VERSION: "24.11.1"

jobs:
  # Job 1: Install dependencies and cache
  setup:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4
        with:
          fetch-depth: 0 # Full history for Nx affected commands

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Cache node_modules
        uses: actions/cache@v3
        with:
          path: node_modules
          key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}
          restore-keys: |
            ${{ runner.os }}-node-

  # Job 2: Linting
  lint:
    needs: setup
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: "npm"

      - name: Restore dependencies
        uses: actions/cache@v3
        with:
          path: node_modules
          key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}

      - name: Lint affected projects
        run: npx nx affected:lint --base=origin/main --parallel=3

  # Job 3: Unit tests (fast)
  unit-tests:
    needs: setup
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: "npm"

      - name: Restore dependencies
        uses: actions/cache@v3
        with:
          path: node_modules
          key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}

      - name: Run unit tests (affected)
        run: |
          npx nx affected:test \
            --base=origin/main \
            --parallel=3 \
            --ci \
            --code-coverage \
            --testPathIgnorePatterns=integration.spec.ts

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/**/coverage-final.json
          flags: unittests
          name: unit-test-coverage

  # Job 4: Integration tests (slower)
  integration-tests:
    needs: setup
    runs-on: ubuntu-latest
    services:
      postgres:
        image: postgres:16-alpine
        env:
          POSTGRES_USER: test_user
          POSTGRES_PASSWORD: test_password
          POSTGRES_DB: test_db
        options: >-
          --health-cmd pg_isready
          --health-interval 10s
          --health-timeout 5s
          --health-retries 5
        ports:
          - 5432:5432

    steps:
      - name: Checkout repository
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: "npm"

      - name: Restore dependencies
        uses: actions/cache@v3
        with:
          path: node_modules
          key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}

      - name: Run integration tests (affected)
        env:
          DATABASE_URL: postgresql://test_user:test_password@localhost:5432/test_db
        run: |
          npx nx affected:test \
            --base=origin/main \
            --parallel=2 \
            --ci \
            --testMatch='**/*.integration.spec.ts' \
            --maxWorkers=2

      - name: Upload integration test results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: integration-test-results
          path: test-results/

  # Job 5: Build affected projects
  build:
    needs: [lint, unit-tests]
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: "npm"

      - name: Restore dependencies
        uses: actions/cache@v3
        with:
          path: node_modules
          key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}

      - name: Build affected projects
        run: npx nx affected:build --base=origin/main --parallel=3

      - name: Upload build artifacts
        uses: actions/upload-artifact@v3
        with:
          name: build-artifacts
          path: dist/

  # Job 6: E2E tests (slowest, runs last)
  e2e-tests:
    needs: build
    runs-on: ubuntu-latest
    if: github.event_name == 'push' || github.event.pull_request.base.ref == 'main'
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: "npm"

      - name: Restore dependencies
        uses: actions/cache@v3
        with:
          path: node_modules
          key: ${{ runner.os }}-node-${{ hashFiles('**/package-lock.json') }}

      - name: Run E2E tests
        run: npx nx affected:e2e --base=origin/main --parallel=1

  # Job 7: Report test results
  report:
    needs: [unit-tests, integration-tests, e2e-tests]
    runs-on: ubuntu-latest
    if: always()
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Download test results
        uses: actions/download-artifact@v3
        with:
          name: integration-test-results
          path: test-results/

      - name: Publish test results
        uses: dorny/test-reporter@v1
        if: always()
        with:
          name: Test Results
          path: "test-results/**/*.xml"
          reporter: jest-junit
```

## Matrix Testing Strategy

Run tests across multiple Node.js versions:

```yaml
# .github/workflows/matrix-tests.yml
name: Matrix Tests

on:
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        node-version: [20.x, 22.x, 24.x]

    steps:
      - uses: actions/checkout@v4

      - name: Setup Node.js ${{ matrix.node-version }}
        uses: actions/setup-node@v4
        with:
          node-version: ${{ matrix.node-version }}
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run tests
        run: npm test
```

## GitLab CI Template

```yaml
# .gitlab-ci.yml
image: node:24.11.1

stages:
  - setup
  - lint
  - test
  - build
  - deploy

variables:
  NX_CLOUD_ACCESS_TOKEN: $NX_CLOUD_ACCESS_TOKEN
  POSTGRES_DB: test_db
  POSTGRES_USER: test_user
  POSTGRES_PASSWORD: test_password
  DATABASE_URL: postgresql://test_user:test_password@postgres:5432/test_db

cache:
  key:
    files:
      - package-lock.json
  paths:
    - node_modules/
    - .npm/

# Stage 1: Setup
install:
  stage: setup
  script:
    - npm ci --cache .npm --prefer-offline
  artifacts:
    paths:
      - node_modules/
    expire_in: 1 hour

# Stage 2: Linting
lint:
  stage: lint
  needs: [install]
  script:
    - npx nx affected:lint --base=origin/main --parallel=3
  only:
    - merge_requests
    - main

# Stage 3: Tests
unit-tests:
  stage: test
  needs: [install]
  script:
    - npx nx affected:test --base=origin/main --parallel=3 --ci --code-coverage --testPathIgnorePatterns=integration.spec.ts
  coverage: '/All files[^|]*\|[^|]*\s+([\d\.]+)/'
  artifacts:
    when: always
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage/cobertura-coverage.xml
      junit: test-results/**/junit.xml
    paths:
      - coverage/
    expire_in: 30 days

integration-tests:
  stage: test
  needs: [install]
  services:
    - name: postgres:16-alpine
      alias: postgres
  script:
    - npx nx affected:test --base=origin/main --parallel=2 --ci --testMatch='**/*.integration.spec.ts'
  artifacts:
    when: always
    paths:
      - test-results/
    expire_in: 7 days

property-tests:
  stage: test
  needs: [install]
  script:
    - npx nx affected:test --base=origin/main --parallel=2 --ci --testMatch='**/*.property.spec.ts'
  only:
    - merge_requests
    - main

# Stage 4: Build
build:
  stage: build
  needs: [lint, unit-tests]
  script:
    - npx nx affected:build --base=origin/main --parallel=3
  artifacts:
    paths:
      - dist/
    expire_in: 1 week
  only:
    - main
    - merge_requests

# Stage 5: Deploy
deploy-staging:
  stage: deploy
  needs: [build, integration-tests]
  script:
    - echo "Deploy to staging"
    # Add deployment commands here
  environment:
    name: staging
  only:
    - main

deploy-production:
  stage: deploy
  needs: [build, integration-tests]
  script:
    - echo "Deploy to production"
    # Add deployment commands here
  environment:
    name: production
  when: manual
  only:
    - main
```

## Test Parallelization

### Nx Affected Commands

```bash
# Run only tests for affected projects
npx nx affected:test --base=origin/main

# Parallel execution (3 projects at a time)
npx nx affected:test --base=origin/main --parallel=3

# Run all tests (not just affected)
npx nx run-many --target=test --all --parallel=3

# Specific project
npx nx test ts-zakat --parallel
```

### Jest Configuration for CI

```typescript
// jest.config.ci.ts
export default {
  preset: "./jest.preset.js",
  maxWorkers: "50%", // Use 50% of available CPUs
  ci: true,
  collectCoverage: true,
  coverageReporters: ["json", "lcov", "text", "clover", "cobertura"],
  reporters: [
    "default",
    [
      "jest-junit",
      {
        outputDirectory: "test-results",
        outputName: "junit.xml",
        classNameTemplate: "{classname}",
        titleTemplate: "{title}",
      },
    ],
  ],
};
```

## Coverage Reporting

### Codecov Configuration

```yaml
# codecov.yml
coverage:
  status:
    project:
      default:
        target: 80% # Require 80% coverage
        threshold: 1% # Allow 1% drop
    patch:
      default:
        target: 80%

comment:
  layout: "header, diff, files"
  behavior: default

ignore:
  - "**/*.spec.ts"
  - "**/*.integration.spec.ts"
  - "**/__test-helpers__/**"
  - "**/test-utils.ts"
```

### SonarQube Integration

```yaml
# .github/workflows/sonarqube.yml
name: SonarQube Analysis

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  sonarqube:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: "24.11.1"

      - name: Install dependencies
        run: npm ci

      - name: Run tests with coverage
        run: npm run test:coverage

      - name: SonarQube Scan
        uses: SonarSource/sonarqube-scan-action@master
        env:
          SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
          SONAR_HOST_URL: ${{ secrets.SONAR_HOST_URL }}
```

```properties
# sonar-project.properties
sonar.projectKey=open-sharia-enterprise
sonar.organization=your-org
sonar.sources=libs,apps
sonar.tests=libs,apps
sonar.test.inclusions=**/*.spec.ts,**/*.integration.spec.ts
sonar.exclusions=**/node_modules/**,**/dist/**,**/*.spec.ts
sonar.javascript.lcov.reportPaths=coverage/**/lcov.info
sonar.testExecutionReportPaths=test-results/**/test-report.xml
```

## Pre-commit Hooks with Husky

```json
// package.json
{
  "scripts": {
    "prepare": "husky install"
  },
  "lint-staged": {
    "*.ts": ["eslint --fix", "prettier --write"],
    "*.md": ["prettier --write"]
  }
}
```

```bash
# .husky/pre-commit
#!/usr/bin/env sh
. "$(dirname -- "$0")/_/husky.sh"

# Run lint-staged
npx lint-staged

# Run tests for affected files
npx nx affected:test --uncommitted --parallel=3
```

```bash
# .husky/pre-push
#!/usr/bin/env sh
. "$(dirname -- "$0")/_/husky.sh"

# Run all affected tests before push
npx nx affected:test --base=origin/main --parallel=3
```

## Docker-based Testing

```dockerfile
# Dockerfile.test
FROM node:24.11.1-alpine

WORKDIR /app

# Install dependencies
COPY package*.json ./
RUN npm ci

# Copy source
COPY . .

# Run tests
CMD ["npm", "test"]
```

```yaml
# docker-compose.test.yml
version: "3.8"

services:
  test:
    build:
      context: .
      dockerfile: Dockerfile.test
    environment:
      - NODE_ENV=test
      - DATABASE_URL=postgresql://test_user:test_password@postgres:5432/test_db
    depends_on:
      - postgres
    volumes:
      - ./coverage:/app/coverage

  postgres:
    image: postgres:16-alpine
    environment:
      POSTGRES_USER: test_user
      POSTGRES_PASSWORD: test_password
      POSTGRES_DB: test_db
    ports:
      - "5432:5432"
```

```bash
# Run tests in Docker
docker-compose -f docker-compose.test.yml up --abort-on-container-exit
```

## Performance Optimization

### 1. Test Sharding

```yaml
# GitHub Actions - shard tests across multiple runners
jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        shard: [1, 2, 3, 4]
    steps:
      - uses: actions/checkout@v4
      - name: Run tests (shard ${{ matrix.shard }}/4)
        run: npx jest --shard=${{ matrix.shard }}/4
```

### 2. Nx Cloud Caching

```bash
# Enable Nx Cloud for distributed caching
npx nx connect-to-nx-cloud

# CI will automatically use cached results
npx nx affected:test --base=origin/main
```

### 3. Selective Test Execution

```yaml
# Only run integration tests on main branch
integration-tests:
  if: github.ref == 'refs/heads/main'
  runs-on: ubuntu-latest
  steps:
    - name: Run integration tests
      run: npm run test:integration
```

## Test Reporting and Dashboards

### Jest HTML Reporter

```typescript
// jest.config.ts
export default {
  reporters: [
    "default",
    [
      "jest-html-reporters",
      {
        publicPath: "./test-reports",
        filename: "test-report.html",
        expand: true,
      },
    ],
  ],
};
```

### Allure Report

```bash
# Install Allure
npm install --save-dev jest-allure

# Generate report
npm run test
allure generate allure-results --clean -o allure-report
allure open allure-report
```

## Best Practices

### 1. Fast Feedback Loop

```yaml
# Run fast tests first, slow tests last
jobs:
  lint: # Fastest
  unit-tests: # Fast
  build:
  integration-tests: # Slower
  e2e-tests: # Slowest
```

### 2. Fail Fast

```yaml
# Stop on first failure for pull requests
jobs:
  test:
    strategy:
      fail-fast: true # Stop other jobs if one fails
```

### 3. Conditional Execution

```yaml
# Skip E2E tests for documentation changes
e2e-tests:
  if: |
    !contains(github.event.head_commit.message, '[skip e2e]') &&
    !contains(github.event.head_commit.message, 'docs:')
```

### 4. Artifact Management

```yaml
# Keep test results for debugging
- name: Upload test results
  if: always() # Upload even if tests fail
  uses: actions/upload-artifact@v3
  with:
    name: test-results
    path: test-results/
    retention-days: 30
```

## Checklist

- [ ] CI pipeline triggers on push and pull request
- [ ] Dependencies cached for faster builds
- [ ] Nx affected commands used to run only changed projects
- [ ] Tests run in parallel
- [ ] Unit tests run separately from integration tests
- [ ] Coverage reports uploaded (Codecov, SonarQube)
- [ ] Test results published for easy review
- [ ] Failed tests provide clear error messages
- [ ] Build artifacts saved for deployment
- [ ] Pre-commit hooks prevent broken code
- [ ] Matrix testing for multiple environments (optional)
- [ ] Proper secrets management (API keys, tokens)

## Summary

**Key Principles**:

1. **Fast Feedback**: Run fast tests first
2. **Affected Testing**: Only test changed code
3. **Parallel Execution**: Utilize multiple workers
4. **Comprehensive Coverage**: Unit, integration, E2E
5. **Caching**: Cache dependencies and Nx results
6. **Clear Reports**: Easy-to-read test results
7. **Fail Fast**: Stop early on failures

A well-configured CI/CD pipeline ensures tests run automatically, catch issues early, and provide fast feedback to developers.
