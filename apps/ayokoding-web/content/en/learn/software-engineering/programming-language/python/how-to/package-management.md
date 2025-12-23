---
title: "How to Manage Packages and Dependencies"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000023
description: "Practical techniques for managing Python packages with pip, virtual environments, Poetry, dependency resolution, and reproducible builds"
---

## Problem

Python projects require dependency isolation, version control, and reproducible installations. System-wide package installations cause conflicts and security risks. Manual dependency management becomes unmaintainable.

## Solution

### 1. Virtual Environments with venv

```bash
# Create virtual environment
python -m venv venv

# Activate virtual environment
# On Linux/macOS:
source venv/bin/activate

# On Windows:
venv\Scripts\activate

# Install packages (isolated from system)
pip install requests pandas numpy

# Freeze dependencies
pip freeze > requirements.txt

# Install from requirements
pip install -r requirements.txt

# Deactivate environment
deactivate
```

**Python script to create venv programmatically**:

```python
import venv
import subprocess
from pathlib import Path

def create_virtual_environment(path: str) -> None:
    """Create virtual environment at specified path."""
    venv_path = Path(path)

    # Create venv
    venv.create(venv_path, with_pip=True)

    # Get pip path
    if venv_path.joinpath("Scripts").exists():  # Windows
        pip_path = venv_path / "Scripts" / "pip"
    else:  # Unix
        pip_path = venv_path / "bin" / "pip"

    # Install packages
    subprocess.run([str(pip_path), "install", "requests", "pytest"])

# Usage
create_virtual_environment("./my_venv")
```

### 2. Modern Package Management with Poetry

```bash
# Install Poetry
curl -sSL https://install.python-poetry.org | python3 -

# Create new project
poetry new my-project
cd my-project

# Or initialize in existing project
poetry init

# Add dependencies
poetry add requests pandas
poetry add --group dev pytest black mypy

# Install dependencies
poetry install

# Update dependencies
poetry update

# Show dependency tree
poetry show --tree

# Run scripts in venv
poetry run python script.py
poetry run pytest

# Build distribution
poetry build

# Publish to PyPI
poetry publish
```

**pyproject.toml structure**:

```toml
[tool.poetry]
name = "my-project"
version = "0.1.0"
description = "My awesome project"
authors = ["Your Name <you@example.com>"]
readme = "README.md"

[tool.poetry.dependencies]
python = "^3.11"
requests = "^2.31.0"
pandas = "^2.1.0"

[tool.poetry.group.dev.dependencies]
pytest = "^7.4.0"
black = "^23.7.0"
mypy = "^1.5.0"
ruff = "^0.0.287"

[build-system]
requires = ["poetry-core"]
build-backend = "poetry.core.masonry.api"

[tool.poetry.scripts]
my-cli = "my_project.cli:main"
```

### 3. Dependency Version Pinning

```python
# requirements.txt - Different pinning strategies

# Exact version (reproducible but inflexible)
requests==2.31.0
pandas==2.1.0

# Compatible version (minor/patch updates)
requests~=2.31.0  # >=2.31.0, <2.32.0
pandas~=2.1       # >=2.1.0, <2.2.0

# Minimum version (flexible but risky)
requests>=2.31.0
pandas>=2.1.0

# Version range
numpy>=1.24.0,<2.0.0

# Platform-specific dependencies
pywin32==306 ; sys_platform == 'win32'
uvloop==0.17.0 ; sys_platform != 'win32'

# Environment markers
cryptography>=41.0.0 ; python_version >= '3.11'

# Git dependencies
# git+https://github.com/user/repo.git@main#egg=package-name

# Local dependencies
# -e /path/to/local/package
```

**requirements-dev.txt for development dependencies**:

```txt
-r requirements.txt

# Development tools
pytest>=7.4.0
pytest-cov>=4.1.0
black>=23.7.0
ruff>=0.0.287
mypy>=1.5.0

# Documentation
sphinx>=7.2.0
sphinx-rtd-theme>=1.3.0
```

### 4. Dependency Resolution and Lock Files

```python
# Use pip-tools for deterministic builds
# Install pip-tools
# pip install pip-tools

# requirements.in - High-level dependencies
"""
requests
pandas
numpy
"""

# Generate locked requirements.txt
# pip-compile requirements.in

# This creates requirements.txt with exact versions:
"""
certifi==2023.7.22
charset-normalizer==3.2.0
idna==3.4
numpy==1.25.2
pandas==2.1.0
python-dateutil==2.8.2
pytz==2023.3
requests==2.31.0
six==1.16.0
tzdata==2023.3
urllib3==2.0.4
"""

# Upgrade dependencies
# pip-compile --upgrade requirements.in

# Sync environment with lock file
# pip-sync requirements.txt
```

**Poetry automatically creates poetry.lock**:

```bash
# poetry.lock is automatically generated
poetry install  # Uses poetry.lock for exact versions
poetry lock --no-update  # Regenerate lock without updating
poetry lock  # Update lock file with latest compatible versions
```

### 5. Creating Distributable Packages

**Project structure**:

```
my-package/
├── src/
│   └── my_package/
│       ├── __init__.py
│       ├── core.py
│       └── utils.py
├── tests/
│   ├── __init__.py
│   └── test_core.py
├── README.md
├── LICENSE
├── pyproject.toml
└── setup.py (optional, for backwards compatibility)
```

**pyproject.toml for packaging**:

```toml
[build-system]
requires = ["setuptools>=68.0", "wheel"]
build-backend = "setuptools.build_meta"

[project]
name = "my-package"
version = "0.1.0"
description = "A helpful package"
readme = "README.md"
authors = [
    {name = "Your Name", email = "you@example.com"}
]
license = {text = "MIT"}
classifiers = [
    "Development Status :: 3 - Alpha",
    "Intended Audience :: Developers",
    "License :: OSI Approved :: MIT License",
    "Programming Language :: Python :: 3",
    "Programming Language :: Python :: 3.11",
]
requires-python = ">=3.11"
dependencies = [
    "requests>=2.31.0",
    "pandas>=2.1.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=7.4.0",
    "black>=23.7.0",
    "mypy>=1.5.0",
]

[project.urls]
Homepage = "https://github.com/user/my-package"
Documentation = "https://my-package.readthedocs.io"
Repository = "https://github.com/user/my-package"

[project.scripts]
my-cli = "my_package.cli:main"

[tool.setuptools.packages.find]
where = ["src"]
```

**Building and publishing**:

```bash
# Build distribution
python -m build

# This creates:
# dist/my-package-0.1.0.tar.gz (source)
# dist/my_package-0.1.0-py3-none-any.whl (wheel)

# Install locally for testing
pip install -e .

# Upload to PyPI
python -m twine upload dist/*

# Upload to Test PyPI first
python -m twine upload --repository testpypi dist/*
```

### 6. Dependency Security Scanning

```bash
# Install safety
pip install safety

# Check for known vulnerabilities
safety check

# Check specific requirements file
safety check -r requirements.txt

# Generate JSON report
safety check --json

# Use pip-audit (more comprehensive)
pip install pip-audit

# Audit installed packages
pip-audit

# Audit requirements file
pip-audit -r requirements.txt

# Fix vulnerabilities automatically
pip-audit --fix
```

**Automated security scanning with GitHub Actions**:

```yaml
name: Security Scan

on: [push, pull_request]

jobs:
  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: "3.11"

      - name: Install dependencies
        run: |
          pip install safety pip-audit

      - name: Run safety check
        run: safety check

      - name: Run pip-audit
        run: pip-audit
```

## How It Works

```mermaid
graph TD
    A[Project Requirements] --> B{Package Manager}
    B -->|pip| C[requirements.txt]
    B -->|Poetry| D[pyproject.toml]
    C --> E[Dependency Resolver]
    D --> E
    E --> F{Conflicts?}
    F -->|Yes| G[Resolution Error]
    F -->|No| H[Lock File]
    H --> I[Virtual Environment]
    I --> J[Isolated Installation]

    style A fill:#0173B2
    style E fill:#DE8F05
    style H fill:#029E73
    style J fill:#CC78BC
```

**Dependency Resolution Process:**

1. **Requirements**: Define package dependencies with version constraints
2. **Resolution**: Solver finds compatible versions satisfying all constraints
3. **Lock File**: Record exact versions for reproducibility
4. **Installation**: Download and install packages in isolated environment
5. **Isolation**: Virtual environment prevents conflicts with other projects

## Variations

### Conda for Scientific Computing

```bash
# Create conda environment
conda create -n myenv python=3.11

# Activate environment
conda activate myenv

# Install packages
conda install numpy pandas scikit-learn

# Install from conda-forge
conda install -c conda-forge xgboost

# Mix conda and pip
conda install numpy
pip install custom-package

# Export environment
conda env export > environment.yml

# Create from environment file
conda env create -f environment.yml

# Deactivate
conda deactivate
```

### Docker for Complete Isolation

```dockerfile
# Dockerfile
FROM python:3.11-slim

WORKDIR /app

# Install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application
COPY . .

CMD ["python", "app.py"]
```

### pipx for CLI Tools

```bash
# Install pipx
python -m pip install --user pipx
python -m pipx ensurepath

# Install CLI tools in isolated environments
pipx install black
pipx install pytest
pipx install poetry

# Run tool
black myfile.py

# Upgrade tool
pipx upgrade black

# Uninstall
pipx uninstall black
```

## Common Pitfalls

### 1. Not Using Virtual Environments

**Problem**: Global package installations cause conflicts.

```bash
# ❌ Bad: Installing globally
pip install requests  # Affects system Python!

# ✅ Good: Use virtual environment
python -m venv venv
source venv/bin/activate
pip install requests  # Isolated
```

### 2. Committing Virtual Environments to Git

**Problem**: Large venv directories bloat repository.

```gitignore
# ✅ Always add to .gitignore
venv/
env/
.venv/
__pycache__/
*.pyc
.pytest_cache/
.mypy_cache/
dist/
build/
*.egg-info/
```

### 3. Not Pinning Dependencies

**Problem**: Builds become non-reproducible.

```txt
# ❌ Bad: No version constraints
requests
pandas
numpy

# ✅ Good: Pin versions
requests==2.31.0
pandas==2.1.0
numpy==1.25.2

# ✅ Better: Use lock file (poetry.lock or pip-compile)
```

### 4. Mixing Package Managers

**Problem**: Dependency conflicts between pip and conda.

```bash
# ❌ Bad: Mixing managers carelessly
conda install numpy
pip install pandas  # May conflict with conda's numpy

# ✅ Good: Choose one primary manager
# If using conda, prefer conda packages
conda install numpy pandas

# Only use pip for packages not in conda
conda install numpy
pip install custom-package-not-in-conda
```

### 5. Ignoring Dependency Vulnerabilities

**Problem**: Outdated packages have security issues.

```bash
# ❌ Bad: Never updating or checking security
pip install old-package==1.0.0

# ✅ Good: Regular security audits
pip install pip-audit
pip-audit  # Check for vulnerabilities
pip-audit --fix  # Auto-upgrade vulnerable packages

# Schedule regular dependency updates
```

## Related Patterns

**Related Tutorial**: See [Beginner Tutorial - Setup](/en/learn/software-engineering/programming-language/python/tutorials/beginner#setup).
**Related How-To**: See [Implement Security Best Practices](/en/learn/software-engineering/programming-language/python/how-to/security-best-practices).
**Related Cookbook**: See Cookbook recipe "Dependency Management".
