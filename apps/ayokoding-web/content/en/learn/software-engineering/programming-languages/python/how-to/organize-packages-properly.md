---
title: "Organize Packages Properly"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 1000011
description: "Structure Python packages with proper imports and clear module organization"
tags: ["python", "packages", "imports", "modules", "project-structure"]
categories: ["learn"]
---

## Problem

Python's import system is flexible but can lead to confusion with circular imports, relative vs absolute imports, and package structure. Proper package organization makes code maintainable and prevents import issues.

This guide shows how to structure Python packages effectively.

## Package Structure Basics

```
myproject/
├── src/
│   └── mypackage/
│       ├── __init__.py      # Package initialization
│       ├── module1.py
│       ├── module2.py
│       └── subpackage/
│           ├── __init__.py
│           └── module3.py
├── tests/
│   ├── __init__.py
│   ├── test_module1.py
│   └── test_module2.py
├── setup.py or pyproject.toml
└── README.md
```

## Import Styles

### Absolute Imports (Preferred)

```python
from mypackage import module1
from mypackage.subpackage import module3
from mypackage.module2 import SomeClass

```

### Relative Imports

```python
from . import module2         # Same level
from .subpackage import module3  # Subdirectory
from ..otherpackage import module4  # Parent directory

```

### Import Patterns

```python
import mypackage.module1
result = mypackage.module1.function()

from mypackage.module1 import function, SomeClass
result = function()
obj = SomeClass()

import mypackage.very_long_module_name as vlmn
vlmn.function()

from mypackage.module1 import VeryLongClassName as VLCN
obj = VLCN()

from mypackage.module1 import *  # Pollutes namespace
```

## **init**.py Usage

### Empty **init**.py

```python

```

### Exposing Package API

```python
from .module1 import PublicClass, public_function
from .module2 import AnotherClass


__all__ = ["PublicClass", "public_function", "AnotherClass"]
```

### Package Initialization

```python
import logging

logger = logging.getLogger(__name__)
logger.addHandler(logging.NullHandler())

__version__ = "1.0.0"

def _import_heavy_module():
    from . import heavy_module
    return heavy_module

```

## Avoiding Circular Imports

### The Problem

```python
from module_b import function_b

def function_a():
    return function_b()

from module_a import function_a  # Circular!

def function_b():
    return function_a()
```

### Solutions

```python
def shared_function():
    return "shared"

from shared import shared_function
def function_a():
    return shared_function()

from shared import shared_function
def function_b():
    return shared_function()

def function_a():
    from module_b import function_b  # Delayed import
    return function_b()

def function_a(dependency):
    return dependency()

from module_b import function_b
result = function_a(function_b)
```

## Project Layout Patterns

### Flat Layout

```
myproject/
├── mypackage/
│   ├── __init__.py
│   ├── core.py
│   ├── utils.py
│   └── config.py
├── tests/
├── setup.py
└── README.md
```

### Src Layout (Recommended)

```
myproject/
├── src/
│   └── mypackage/
│       ├── __init__.py
│       ├── core.py
│       └── utils.py
├── tests/
├── setup.py or pyproject.toml
└── README.md

```

### Application Layout

```
myapp/
├── src/
│   └── myapp/
│       ├── __init__.py
│       ├── __main__.py  # Entry point
│       ├── api/
│       │   ├── __init__.py
│       │   ├── routes.py
│       │   └── handlers.py
│       ├── core/
│       │   ├── __init__.py
│       │   ├── models.py
│       │   └── services.py
│       ├── utils/
│       │   ├── __init__.py
│       │   └── helpers.py
│       └── config.py
├── tests/
├── requirements.txt
└── README.md
```

## **main**.py for Entry Points

```python

def main():
    print("Running mypackage")
    # Application logic

if __name__ == "__main__":
    main()

```

## Managing Dependencies

### Import Order (PEP 8)

```python
import os
import sys
from pathlib import Path

import numpy as np
import requests
from flask import Flask

from mypackage import module1
from mypackage.utils import helper

```

### Conditional Imports

```python
try:
    import pandas as pd
    HAS_PANDAS = True
except ImportError:
    HAS_PANDAS = False

def process_data(data):
    if HAS_PANDAS:
        return process_with_pandas(data)
    else:
        return process_without_pandas(data)

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    # Only imported for type checkers, not at runtime
    from mypackage.heavy_module import HeavyClass

def function(obj: 'HeavyClass'):  # String annotation
    pass
```

## Namespace Packages

```python



from mynamespace.package1 import module
from mynamespace.package2 import module
```

## Best Practices

### Keep **init**.py Minimal

```python
from .core import MainClass, main_function
from .utils import helper_function

__version__ = "1.0.0"
__all__ = ["MainClass", "main_function", "helper_function"]

```

### Use Explicit Exports

```python
__all__ = ["public_function", "PublicClass"]

def public_function():
    return _private_function()

class PublicClass:
    pass

def _private_function():  # Leading underscore = private
    pass

class _PrivateClass:
    pass
```

### Organize by Feature, Not Type

```python
myapp/
├── models/
│   ├── user.py
│   └── order.py
├── views/
│   ├── user.py
│   └── order.py
└── controllers/
    ├── user.py
    └── order.py

myapp/
├── users/
│   ├── __init__.py
│   ├── models.py
│   ├── views.py
│   └── controllers.py
└── orders/
    ├── __init__.py
    ├── models.py
    ├── views.py
    └── controllers.py
```

## Testing Package Structure

```
myproject/
├── src/
│   └── mypackage/
│       └── ...
└── tests/
    ├── __init__.py
    ├── conftest.py        # pytest fixtures
    ├── test_module1.py
    └── integration/
        ├── __init__.py
        └── test_api.py

```

## Package Distribution

### setup.py (Traditional)

```python
from setuptools import setup, find_packages

setup(
    name="mypackage",
    version="1.0.0",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    install_requires=[
        "requests>=2.28.0",
        "click>=8.0.0",
    ],
    extras_require={
        "dev": ["pytest>=7.0.0", "black>=23.0.0"],
    },
    entry_points={
        "console_scripts": [
            "myapp=mypackage.cli:main",
        ],
    },
)
```

### pyproject.toml (Modern)

```toml
[build-system]
requires = ["setuptools>=61.0"]
build-backend = "setuptools.build_meta"

[project]
name = "mypackage"
version = "1.0.0"
dependencies = [
    "requests>=2.28.0",
    "click>=8.0.0",
]

[project.optional-dependencies]
dev = ["pytest>=7.0.0", "black>=23.0.0"]

[project.scripts]
myapp = "mypackage.cli:main"
```

## Summary

Package organization in Python centers on clear import paths and preventing circular dependencies. Absolute imports provide explicit, unambiguous paths that work everywhere, while relative imports work only within packages and fail when modules are run directly. Prefer absolute imports for clarity and maintainability.

The src layout separates source code from project metadata and tests, forcing proper installation before use. This prevents accidentally importing from the wrong location and makes testing more reliable. Place your package inside src/ and import it like any third-party package.

The **init**.py file marks directories as Python packages and controls package initialization. Keep it minimal - expose the public API through targeted imports and avoid placing business logic there. Use **all** to explicitly define what star imports should include, making the package's public interface clear.

Circular imports indicate design issues more than import problems. Restructure code to extract shared functionality, use dependency injection, or move imports inside functions when necessary. Circular dependencies usually disappear when responsibilities are properly separated.

Organize modules by feature rather than type. Group related models, views, and controllers together rather than separating all models into one directory and all views into another. Feature-based organization scales better and keeps related code together.

Import order follows PEP 8 conventions: standard library, third-party packages, then local imports, with blank lines separating groups. This consistency makes imports scannable and prevents ambiguity about dependencies.

Package distribution through setup.py or pyproject.toml defines dependencies, entry points, and metadata. Modern Python uses pyproject.toml for configuration, consolidating multiple config files into one standard format. Entry points enable installing command-line tools that users can run directly.

Proper package organization makes code maintainable as projects grow. Clear import paths, minimal circular dependencies, and feature-based organization prevent common Python project problems.

## Related Content

- [Python Best Practices](/en/learn/software-engineering/programming-languages/python/explanation/best-practices)
- [How to Manage Dependencies and Environments](/en/learn/software-engineering/programming-languages/python/how-to/manage-dependencies-and-environments)
- [How to Write Pythonic Code](/en/learn/software-engineering/programming-languages/python/how-to/write-pythonic-code)
