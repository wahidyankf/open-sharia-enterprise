---
title: "How to Manage Dependencies and Environments"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 506
description: "Set up reproducible Python environments with virtual environments and modern dependency management"
tags:
  ["python", "dependencies", "venv", "pip", "poetry", "virtual-environments"]
categories: ["learn"]
---

## Problem

Python projects need isolated dependencies to avoid conflicts between different projects and ensure reproducible builds. Without proper dependency management, you face "works on my machine" problems and unpredictable behavior in production.

This guide shows how to manage Python dependencies and environments effectively.

## Virtual Environments

Virtual environments isolate project dependencies from system Python and other projects.

### Creating Virtual Environments

```bash
# ✅ Create virtual environment (Python 3.3+)
python -m venv venv

# Or specify Python version
python3.10 -m venv venv

# Alternative name
python -m venv .venv  # Hidden directory

# ✅ Activate environment (Unix/macOS)
source venv/bin/activate

# ✅ Activate environment (Windows)
venv\Scripts\activate

# ✅ Verify activation
which python  # Should show venv/bin/python
pip list      # Shows only venv packages

# ✅ Deactivate
deactivate
```

### Why Virtual Environments Matter

```bash
# ❌ Without venv - global pollution
pip install requests==2.25.0  # Project A needs 2.25.0
pip install requests==2.28.0  # Project B needs 2.28.0 - breaks A!

# ✅ With venv - isolation
# Project A
python -m venv venv-project-a
source venv-project-a/bin/activate
pip install requests==2.25.0  # Isolated to this venv

# Project B
python -m venv venv-project-b
source venv-project-b/bin/activate
pip install requests==2.28.0  # Separate environment
```

## Managing Dependencies with pip

### Requirements Files

```bash
# ✅ Install dependencies
pip install -r requirements.txt

# ✅ Generate requirements from current environment
pip freeze > requirements.txt

# ✅ Upgrade all packages
pip install --upgrade -r requirements.txt
```

### Requirements File Structure

```python
# requirements.txt - version ranges
requests>=2.28.0,<3.0.0
flask>=2.3.0
python-dateutil>=2.8.0

# Optional: Comments and grouping
# Web framework
flask>=2.3.0
flask-cors>=4.0.0

# Database
sqlalchemy>=2.0.0
psycopg2-binary>=2.9.0

# Testing (better in requirements-dev.txt)
pytest>=7.4.0
pytest-cov>=4.1.0
```

### Pinned Dependencies

```python
# requirements-lock.txt - exact versions (from pip freeze)
requests==2.31.0
certifi==2023.7.22
charset-normalizer==3.2.0
idna==3.4
urllib3==2.0.4
flask==2.3.3
click==8.1.7
itsdangerous==2.1.2
jinja2==3.1.2
markupsafe==2.1.3
werkzeug==2.3.7

# ✅ Workflow
# 1. Development: pip install -r requirements.txt
# 2. Lock: pip freeze > requirements-lock.txt
# 3. Production: pip install -r requirements-lock.txt
```

### Development Dependencies

```python
# requirements-dev.txt - development tools
-r requirements.txt  # Include base requirements

# Testing
pytest>=7.4.0
pytest-cov>=4.1.0
pytest-mock>=3.11.0

# Linting and formatting
black>=23.0.0
flake8>=6.0.0
mypy>=1.5.0
isort>=5.12.0

# Development tools
ipython>=8.14.0
ipdb>=0.13.0

# ✅ Install dev dependencies
pip install -r requirements-dev.txt
```

## Modern Dependency Management with Poetry

Poetry provides better dependency resolution and project management.

### Installing Poetry

```bash
# Install Poetry
curl -sSL https://install.python-poetry.org | python3 -

# Verify installation
poetry --version
```

### Using Poetry

```bash
# ✅ Create new project
poetry new my-project
cd my-project

# ✅ Initialize existing project
poetry init

# ✅ Add dependencies
poetry add requests
poetry add flask ">=2.3.0,<3.0.0"

# ✅ Add dev dependencies
poetry add --group dev pytest black mypy

# ✅ Install dependencies
poetry install

# ✅ Update dependencies
poetry update

# ✅ Show dependency tree
poetry show --tree

# ✅ Run commands in virtual environment
poetry run python script.py
poetry run pytest

# ✅ Activate shell in venv
poetry shell
```

### pyproject.toml Structure

```toml
[tool.poetry]
name = "my-project"
version = "0.1.0"
description = "My awesome project"
authors = ["Your Name <you@example.com>"]

[tool.poetry.dependencies]
python = "^3.10"
requests = "^2.31.0"
flask = "^2.3.0"

[tool.poetry.group.dev.dependencies]
pytest = "^7.4.0"
black = "^23.0.0"
mypy = "^1.5.0"

[build-system]
requires = ["poetry-core"]
build-backend = "poetry.core.masonry.api"
```

### Poetry Lock File

```bash
# poetry.lock - automatically generated
# Contains exact versions of all dependencies and subdependencies

# ✅ Workflow
# 1. Add dependency: poetry add package
# 2. Commit pyproject.toml and poetry.lock
# 3. Others run: poetry install (uses lock file)
# 4. Update: poetry update (updates lock file)
```

## Dependency Version Specifications

### Version Specifiers

```python
# requirements.txt version specifiers

# Exact version
requests==2.31.0

# Minimum version
requests>=2.28.0

# Compatible release (same major.minor)
requests~=2.31.0  # Allows 2.31.x, not 2.32.0

# Version range
requests>=2.28.0,<3.0.0

# Any version (not recommended)
requests

# Exclude specific versions
requests!=2.30.0

# Multiple specifiers
requests>=2.28.0,<3.0.0,!=2.30.0
```

### Semantic Versioning

```python
# Understanding version numbers: MAJOR.MINOR.PATCH

# MAJOR: Breaking changes (1.0.0 -> 2.0.0)
# MINOR: New features, backwards compatible (1.0.0 -> 1.1.0)
# PATCH: Bug fixes, backwards compatible (1.0.0 -> 1.0.1)

# ✅ Safe version ranges
requests>=2.28.0,<3.0.0  # Accept new features, reject breaking changes
flask~=2.3.0              # Accept patches only
```

## Reproducible Builds

### Best Practices

```bash
# ✅ 1. Always use virtual environments
python -m venv venv
source venv/bin/activate

# ✅ 2. Pin Python version
# .python-version (for pyenv)
3.10.12

# runtime.txt (for Heroku)
python-3.10.12

# pyproject.toml (for Poetry)
[tool.poetry.dependencies]
python = "^3.10"

# ✅ 3. Lock dependencies
pip freeze > requirements-lock.txt
# Or use Poetry (automatic)

# ✅ 4. Include lock file in version control
git add requirements-lock.txt poetry.lock

# ✅ 5. Install from lock file in production
pip install -r requirements-lock.txt
# Or: poetry install --only main
```

### Docker for Reproducibility

```dockerfile
# Dockerfile for reproducible builds
FROM python:3.10-slim

WORKDIR /app

# Install dependencies
COPY requirements-lock.txt .
RUN pip install --no-cache-dir -r requirements-lock.txt

# Copy application
COPY . .

CMD ["python", "app.py"]
```

## Dependency Security

### Checking for Vulnerabilities

```bash
# ✅ Use pip-audit to check for vulnerabilities
pip install pip-audit
pip-audit

# ✅ Use safety
pip install safety
safety check

# ✅ Upgrade vulnerable packages
pip install --upgrade package-name

# ✅ With Poetry
poetry add package-name@latest
poetry update
```

### Dependency Scanning in CI

```yaml
# .github/workflows/security.yml
name: Security Check

on: [push, pull_request]

jobs:
  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-python@v4
        with:
          python-version: "3.10"
      - name: Install dependencies
        run: |
          pip install pip-audit
      - name: Run security scan
        run: pip-audit
```

## Managing Multiple Python Versions

### Using pyenv

```bash
# Install pyenv (macOS)
brew install pyenv

# Install Python versions
pyenv install 3.10.12
pyenv install 3.11.5

# Set global version
pyenv global 3.10.12

# Set project-specific version
cd my-project
pyenv local 3.10.12  # Creates .python-version

# List installed versions
pyenv versions

# ✅ Workflow with pyenv + venv
pyenv local 3.10.12
python -m venv venv
source venv/bin/activate
```

## Common Patterns

### Project Structure

```
my-project/
├── .venv/                 # Virtual environment
├── src/
│   └── my_package/
│       └── __init__.py
├── tests/
│   └── test_example.py
├── requirements.txt       # Base dependencies
├── requirements-dev.txt   # Dev dependencies
├── requirements-lock.txt  # Locked versions
├── pyproject.toml        # Modern config
├── .python-version       # Python version (pyenv)
├── README.md
└── .gitignore
```

### .gitignore for Python

```gitignore
# Virtual environments
venv/
.venv/
env/
ENV/

# Python cache
__pycache__/
*.py[cod]
*$py.class
.pytest_cache/

# Distribution / packaging
dist/
build/
*.egg-info/

# IDEs
.vscode/
.idea/
*.swp

# Environment variables
.env
.env.local
```

## Troubleshooting

### Common Issues

```bash
# ❌ Package conflicts
# ERROR: package-a requires package-b<2.0, but package-c requires package-b>=2.0

# ✅ Check dependency tree
pip install pipdeptree
pipdeptree

# ✅ Use Poetry for better conflict resolution
poetry add package-a package-c  # Resolves automatically

# ❌ Broken virtual environment
# ✅ Delete and recreate
rm -rf venv
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# ❌ Cache issues
# ✅ Clear pip cache
pip cache purge

# ❌ Outdated pip
# ✅ Upgrade pip
pip install --upgrade pip
```

## Summary

Virtual environments isolate project dependencies from system Python and other projects, preventing version conflicts and ensuring reproducibility. Always create a virtual environment before installing packages, activate it for development, and never install project dependencies globally. The venv module built into Python provides everything needed for basic isolation.

Requirements files manage dependencies with pip. Use requirements.txt for flexible version ranges during development, and requirements-lock.txt (from pip freeze) for exact versions in production. Separate development dependencies into requirements-dev.txt to keep production installs lean. Version specifiers like >= and < define acceptable version ranges based on semantic versioning.

Poetry provides modern dependency management with automatic lock files and better conflict resolution than pip. It combines dependency management, virtual environment creation, and package building in one tool. The pyproject.toml file replaces multiple configuration files, and poetry.lock ensures reproducible installs across environments. Poetry's dependency resolver prevents common version conflicts.

Pin Python versions with .python-version files or runtime specifications to ensure consistent behavior across development, testing, and production. Use pyenv to manage multiple Python versions on one machine, setting project-specific versions that activate automatically. Docker provides the ultimate reproducibility by packaging both Python and dependencies in a container.

Security scanning with tools like pip-audit and safety identifies vulnerable dependencies before they reach production. Run security checks in CI pipelines to catch vulnerabilities early. Keep dependencies updated, but test thoroughly before upgrading in production.

Lock files are crucial for reproducibility. They record exact versions of all packages including transitive dependencies, ensuring that installations produce identical environments. Commit lock files to version control so team members and deployments use the same versions. Update lock files deliberately rather than on every install.

The investment in proper dependency management prevents debugging sessions caused by version mismatches and "works on my machine" problems. Isolated environments, locked dependencies, and version control create predictable, reproducible builds across all environments.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [How to Organize Packages Properly](/en/learn/swe/prog-lang/python/how-to/organize-packages-properly)
- [How to Write Pythonic Code](/en/learn/swe/prog-lang/python/how-to/write-pythonic-code)
