---
title: Build Configuration Template
description: Copy-paste ready Python build configuration template with pyproject.toml, Docker, CI/CD, and development tools for Sharia-compliant financial applications
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - build-configuration
  - pyproject-toml
  - docker
  - ci-cd
related:
  - ../ex-so-stla-py__best-practices.md
  - ../ex-so-stla-py__linting-and-formatting.md
principles:
  - automation-over-manual
  - explicit-over-implicit
last_updated: 2025-01-23
---

# Build Configuration Template

## Overview

Complete Python project configuration with pyproject.toml, Docker, CI/CD, and development tools. Use for standardizing project setup.

**Use this template when**:

- Starting new Python project
- Standardizing build process
- Setting up CI/CD pipeline
- Configuring development tools

**Examples**: OSE Platform Python services, Zakat calculation service

## Complete pyproject.toml Template

```toml
[project]
name = "ose-zakat-service"
version = "1.0.0"
description = "Zakat calculation and management service"
authors = [{name = "OSE Platform Team", email = "team@oseplatform.com"}]
readme = "README.md"
requires-python = ">=3.11"
license = {text = "MIT"}

dependencies = [
    "fastapi>=0.115.0",
    "pydantic>=2.10.0",
    "sqlalchemy[asyncio]>=2.0.0",
    "aiosqlite>=0.20.0",
    "httpx>=0.28.0",
    "python-jose[cryptography]>=3.3.0",
    "passlib[bcrypt]>=1.7.4",
]

[project.optional-dependencies]
dev = [
    "pytest>=8.3.0",
    "pytest-asyncio>=0.24.0",
    "pytest-cov>=6.0.0",
    "mypy>=1.13.0",
    "ruff>=0.8.0",
    "black>=24.10.0",
    "pre-commit>=4.0.0",
]

[build-system]
requires = ["setuptools>=75.0.0", "wheel"]
build-backend = "setuptools.build_meta"

# ============================================================================
# RUFF CONFIGURATION
# ============================================================================

[tool.ruff]
target-version = "py311"
line-length = 88

select = [
    "E",   # pycodestyle errors
    "F",   # Pyflakes
    "I",   # isort
    "N",   # pep8-naming
    "W",   # pycodestyle warnings
    "UP",  # pyupgrade
    "B",   # flake8-bugbear
    "C4",  # flake8-comprehensions
    "SIM", # flake8-simplify
]

ignore = [
    "E501",  # Line too long (Black handles this)
]

exclude = [
    ".git",
    ".venv",
    "__pycache__",
    "build",
    "dist",
]

[tool.ruff.per-file-ignores]
"__init__.py" = ["F401"]  # Allow unused imports

# ============================================================================
# BLACK CONFIGURATION
# ============================================================================

[tool.black]
line-length = 88
target-version = ['py311', 'py312', 'py313']
include = '\\.pyi?$'

# ============================================================================
# MYPY CONFIGURATION
# ============================================================================

[tool.mypy]
python_version = "3.11"
strict = true
warn_return_any = true
warn_unused_configs = true
disallow_untyped_defs = true
disallow_incomplete_defs = true

[[tool.mypy.overrides]]
module = "tests.*"
disallow_untyped_defs = false

# ============================================================================
# PYTEST CONFIGURATION
# ============================================================================

[tool.pytest.ini_options]
minversion = "8.0"
testpaths = ["tests"]
asyncio_mode = "auto"
addopts = [
    "--strict-markers",
    "--cov=src",
    "--cov-report=term-missing",
    "--cov-report=html",
]

# ============================================================================
# COVERAGE CONFIGURATION
# ============================================================================

[tool.coverage.run]
source = ["src"]
omit = ["tests/*", "**/__init__.py"]

[tool.coverage.report]
exclude_lines = [
    "pragma: no cover",
    "def __repr__",
    "raise AssertionError",
    "raise NotImplementedError",
    "if __name__ == .__main__.:",
]
```

## Dockerfile Template

```dockerfile
# Multi-stage build for Python service
FROM python:3.11-slim AS base

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gcc \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements
COPY pyproject.toml ./

# Install Python dependencies
RUN pip install --no-cache-dir -e .

# Development stage
FROM base AS development
RUN pip install --no-cache-dir -e ".[dev]"
COPY . .
CMD ["python", "-m", "uvicorn", "src.main:app", "--host", "0.0.0.0", "--port", "8000", "--reload"]

# Production stage
FROM base AS production
COPY src ./src
CMD ["python", "-m", "uvicorn", "src.main:app", "--host", "0.0.0.0", "--port", "8000"]
```

## Docker Compose Template

```yaml
version: "3.8"

services:
  api:
    build:
      context: .
      target: development
    ports:
      - "8000:8000"
    volumes:
      - ./src:/app/src
      - ./tests:/app/tests
    environment:
      - DATABASE_URL=postgresql://user:password@db:5432/zakat
      - SECRET_KEY=your-secret-key-here
    depends_on:
      - db

  db:
    image: postgres:16-alpine
    environment:
      - POSTGRES_USER=user
      - POSTGRES_PASSWORD=password
      - POSTGRES_DB=zakat
    volumes:
      - postgres_data:/var/lib/postgresql/data

volumes:
  postgres_data:
```

## GitHub Actions CI/CD Template

```yaml
# .github/workflows/ci.yml
name: CI/CD

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    strategy:
      matrix:
        python-version: ["3.11", "3.12", "3.13"]

    steps:
      - uses: actions/checkout@v4

      - name: Set up Python ${{ matrix.python-version }}
        uses: actions/setup-python@v5
        with:
          python-version: ${{ matrix.python-version }}

      - name: Install dependencies
        run: |
          pip install -e ".[dev]"

      - name: Run Ruff
        run: ruff check .

      - name: Run Black
        run: black --check .

      - name: Run mypy
        run: mypy src/

      - name: Run tests
        run: pytest tests/ --cov

      - name: Upload coverage
        uses: codecov/codecov-action@v4
        with:
          file: ./coverage.xml
```

## Pre-commit Configuration

```yaml
# .pre-commit-config.yaml
repos:
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.8.0
    hooks:
      - id: ruff
        args: [--fix]
      - id: ruff-format

  - repo: https://github.com/psf/black
    rev: 24.10.0
    hooks:
      - id: black

  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.13.0
    hooks:
      - id: mypy
        additional_dependencies: [pydantic]
```

## Makefile Template

```makefile
.PHONY: install test lint format clean

install:
 pip install -e ".[dev]"
 pre-commit install

test:
 pytest tests/ --cov --cov-report=html

lint:
 ruff check .
 black --check .
 mypy src/

format:
 ruff check --fix .
 black .

clean:
 find . -type d -name __pycache__ -exec rm -rf {} +
 find . -type f -name "*.pyc" -delete
 rm -rf .pytest_cache .mypy_cache .ruff_cache htmlcov .coverage

docker-build:
 docker build -t ose-zakat-service:latest .

docker-run:
 docker-compose up

docker-test:
 docker-compose run --rm api pytest tests/
```

## Usage

### Initial Setup

```bash
# Create project structure
mkdir ose-zakat-service
cd ose-zakat-service

# Copy pyproject.toml
cp /path/to/template/pyproject.toml .

# Install dependencies
pip install -e ".[dev]"

# Setup pre-commit
pre-commit install

# Run tests
pytest tests/
```

### Development Workflow

```bash
# Format code
make format

# Run linters
make lint

# Run tests
make test

# Docker development
docker-compose up
```

## Best Practices

### Do: Pin Dependencies

```toml
# GOOD: Pinned versions
dependencies = [
    "fastapi>=0.115.0",
    "pydantic>=2.10.0",
]
```

### Do: Separate Dev Dependencies

```toml
# GOOD: Separate dev dependencies
[project.optional-dependencies]
dev = [
    "pytest>=8.3.0",
    "mypy>=1.13.0",
]
```

## References

- [Best Practices](../ex-so-stla-py__best-practices.md)
- [Linting and Formatting](../ex-so-stla-py__linting-and-formatting.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.13.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
