---
title: "How to Document Code Effectively"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 617
description: "Write clear docstrings, meaningful comments, and comprehensive documentation with Sphinx"
tags: ["python", "documentation", "docstrings", "sphinx", "type-hints"]
---

## Problem

Python's dynamic nature makes documentation critical for understanding code behavior. Docstrings follow conventions but style varies (Google, NumPy, Sphinx). Type hints provide documentation but require understanding annotation syntax. Balancing thoroughness with brevity is challenging.

This guide shows effective documentation in Python.

## Docstring Conventions

### Module Docstrings

```python
"""User management and authentication module.

This module provides user registration, authentication, password management,
and session handling. All functions are designed for concurrent use with
proper locking mechanisms.

Example:
    Basic user registration and authentication:

    >>> from myapp import user
    >>> user.register("alice@example.com", "password123")
    User(email='alice@example.com')
    >>> user.authenticate("alice@example.com", "password123")
    True

Attributes:
    MAX_LOGIN_ATTEMPTS (int): Maximum failed login attempts before lockout.
    SESSION_TIMEOUT (int): Session timeout in seconds.
"""

MAX_LOGIN_ATTEMPTS = 3
SESSION_TIMEOUT = 3600
```

### Function Docstrings (Google Style)

```python
def register(email, password, name=None):
    """Register a new user account.

    Creates a new user with the specified email and password. Passwords are
    hashed using bcrypt before storage. A verification email is sent to the
    provided address.

    Args:
        email (str): User's email address (must be unique).
        password (str): User's password (minimum 8 characters).
        name (str, optional): User's display name. Defaults to None.

    Returns:
        User: The newly created user object with generated ID.

    Raises:
        ValueError: If email is invalid or password too short.
        DuplicateEmailError: If email already registered.
        EmailSendError: If verification email fails to send.

    Example:
        >>> user = register("alice@example.com", "securepass123", "Alice")
        >>> user.email
        'alice@example.com'
    """
    validate_email(email)
    validate_password(password)

    user = User(email=email, name=name)
    user.set_password(password)

    send_verification_email(user)
    return user
```

### Class Docstrings

```python
class User:
    """Represents a registered user account.

    Users have a unique email address, hashed password, and optional profile
    information. Accounts may be active or inactive based on email verification.

    Attributes:
        email (str): User's email address (unique, validated).
        name (str): User's display name.
        active (bool): Whether email is verified.
        created_at (datetime): Account creation timestamp.

    Example:
        >>> user = User("alice@example.com", "Alice")
        >>> user.set_password("password123")
        >>> user.validate()
        True
    """

    def __init__(self, email, name=None):
        """Initialize a new user.

        Args:
            email (str): User's email address.
            name (str, optional): User's display name.
        """
        self.email = email
        self.name = name or email.split('@')[0]
        self.active = False
        self.created_at = datetime.now()

    def set_password(self, password):
        """Hash and store the user's password.

        Args:
            password (str): Plain text password (min 8 characters).

        Raises:
            ValueError: If password is too short.
        """
        if len(password) < 8:
            raise ValueError("Password must be at least 8 characters")
        self._password_hash = bcrypt.hashpw(password.encode(), bcrypt.gensalt())
```

### NumPy Style Docstrings

```python
def calculate_discount(total, customer_type, coupon_code=None):
    """Calculate discount for an order.

    Parameters
    ----------
    total : float
        Order total before discount.
    customer_type : {'regular', 'premium', 'vip'}
        Customer membership tier.
    coupon_code : str, optional
        Promotional coupon code.

    Returns
    -------
    float
        Discount amount to subtract from total.

    Raises
    ------
    ValueError
        If total is negative or customer_type invalid.

    Examples
    --------
    >>> calculate_discount(100.0, 'premium')
    10.0
    >>> calculate_discount(100.0, 'vip', 'SAVE20')
    30.0
    """
    if total < 0:
        raise ValueError("Total cannot be negative")

    # Implementation...
```

## Type Hints as Documentation

### Function Type Hints

```python
from typing import List, Dict, Optional, Union

def process_items(
    items: List[str],
    max_count: int = 10,
    filter_fn: Optional[callable] = None
) -> Dict[str, int]:
    """Process items and return counts.

    Args:
        items: List of items to process.
        max_count: Maximum items to process.
        filter_fn: Optional filter function.

    Returns:
        Dictionary mapping items to counts.
    """
    result = {}
    for item in items[:max_count]:
        if filter_fn is None or filter_fn(item):
            result[item] = result.get(item, 0) + 1
    return result

# Type hints show:
# - items must be list of strings
# - max_count is integer with default
# - filter_fn is optional callable
# - Returns dict with string keys and int values
```

### Class Type Hints

```python
from dataclasses import dataclass
from typing import Optional
from datetime import datetime

@dataclass
class User:
    """User account data.

    Attributes:
        email: User's email address.
        name: User's display name.
        age: User's age (must be >= 18).
        created_at: Account creation timestamp.
    """
    email: str
    name: str
    age: int
    created_at: datetime = datetime.now()
    metadata: Optional[Dict[str, str]] = None

    def __post_init__(self):
        """Validate user data after initialization."""
        if self.age < 18:
            raise ValueError("User must be 18 or older")
```

## Writing Effective Comments

### When to Comment

```python
# ✅ Explain WHY, not WHAT
def calculate_price(base_price, customer_type):
    # Apply 20% premium discount to match competitor pricing
    if customer_type == "premium":
        return base_price * 0.8

    # New customers get 10% to encourage first purchase
    if customer_type == "new":
        return base_price * 0.9

    return base_price

# ✅ Document complex algorithms
def hash_password(password):
    # Use bcrypt with cost factor 12 for strong hashing.
    # Higher cost = slower but more secure against brute force.
    # Cost 12 takes ~0.3s on modern hardware.
    return bcrypt.hashpw(password.encode(), bcrypt.gensalt(rounds=12))

# ✅ Warn about gotchas
def process_list(items):
    # Note: This function modifies the input list!
    # Pass a copy if you need to preserve the original.
    items.sort()
    items.pop(0)
    return items

# ❌ Don't state the obvious
x = 5  # Set x to 5
user.name = "Alice"  # Set user name to Alice

# ❌ Don't leave outdated comments
# TODO: Add caching (implemented 6 months ago!)
def fetch_data():
    return cached_fetch()
```

### TODO and FIXME Comments

```python
# ✅ TODO for future enhancements
def authenticate(email, password):
    # TODO: Add rate limiting to prevent brute force
    # TODO: Implement OAuth2 support (issue #123)
    user = find_user(email)
    return user.check_password(password)

# ✅ FIXME for known bugs
def process_concurrent(items):
    # FIXME: Race condition when multiple threads access cache
    # See issue #456
    for item in items:
        update_cache(item)

# ✅ HACK for temporary workarounds
def parse_date(date_str):
    # HACK: Workaround for library bug #789
    # Remove when library updates to v2.0
    date_str = date_str.replace('Z', '+00:00')
    return datetime.fromisoformat(date_str)

# ✅ NOTE for important information
def save_user(user):
    # NOTE: This function assumes user is already validated.
    # Caller must call user.validate() first.
    db.save(user)
```

## README Files

### Package README

````markdown
# MyPackage

User management and authentication for Python applications.

## Features

- User registration with email verification
- Secure password hashing with bcrypt
- Session management
- Type-safe with full type hints

## Installation

```bash
pip install mypackage
```
````

## Quick Start

```python
from mypackage import User, register, authenticate

# Register new user
user = register("alice@example.com", "password123")

# Authenticate
if authenticate("alice@example.com", "password123"):
    print("Login successful")
```

## Documentation

Full documentation available at [docs.example.com](https://docs.example.com).

## License

MIT License

````

## Sphinx Documentation

### Sphinx Setup

```bash
# Install Sphinx
pip install sphinx

# Initialize Sphinx
cd docs
sphinx-quickstart

# Build HTML documentation
make html
````

### conf.py Configuration

```python
# docs/conf.py
project = 'MyProject'
author = 'Your Name'
release = '1.0.0'

# Add extensions
extensions = [
    'sphinx.ext.autodoc',      # Auto-generate from docstrings
    'sphinx.ext.napoleon',     # Google/NumPy style docstrings
    'sphinx.ext.viewcode',     # Add source code links
    'sphinx.ext.intersphinx',  # Link to other projects
]

# Napoleon settings
napoleon_google_docstring = True
napoleon_numpy_docstring = True

# Theme
html_theme = 'sphinx_rtd_theme'
```

### Sphinx Directives

```rst
.. MyProject Documentation
   ========================

User Module
-----------

.. automodule:: myproject.user
   :members:
   :undoc-members:
   :show-inheritance:

.. autoclass:: myproject.user.User
   :members:
   :special-members: __init__

.. autofunction:: myproject.user.register
```

## Deprecation Warnings

### Marking Deprecated Code

```python
import warnings

def old_function(x):
    """Deprecated function.

    .. deprecated:: 2.0
       Use :func:`new_function` instead.
    """
    warnings.warn(
        "old_function is deprecated, use new_function instead",
        DeprecationWarning,
        stacklevel=2
    )
    return new_function(x)

# With decorator
from functools import wraps

def deprecated(message):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            warnings.warn(
                f"{func.__name__} is deprecated: {message}",
                DeprecationWarning,
                stacklevel=2
            )
            return func(*args, **kwargs)
        return wrapper
    return decorator

@deprecated("Use process_v2() instead")
def process(data):
    """Process data (deprecated)."""
    # Implementation...
```

## Examples in Docstrings

### Doctests

```python
def add(a, b):
    """Add two numbers.

    >>> add(2, 3)
    5
    >>> add(-1, 1)
    0
    >>> add(0, 0)
    0
    """
    return a + b

# Run doctests
if __name__ == "__main__":
    import doctest
    doctest.testmod()
```

## Summary

Python documentation centers on docstrings following PEP 257 conventions. Module docstrings explain module purpose and usage, class docstrings describe class behavior and attributes, function docstrings detail parameters, returns, and exceptions. Choose Google, NumPy, or Sphinx style consistently.

Type hints provide self-documenting code through annotations. Specify parameter types, return types, and optional/union types. Type checkers like mypy validate code against type hints, catching errors before runtime.

Comments explain why code exists, not what it does. Document rationale, complex algorithms, and non-obvious constraints. Use TODO for future work, FIXME for known bugs, HACK for temporary workarounds. Remove outdated comments immediately.

README files target package users with installation instructions, quick start examples, and feature overview. Link to full documentation for details. Keep examples working and tested.

Sphinx generates documentation from docstrings automatically. Use autodoc extension to extract docstrings, napoleon for Google/NumPy styles, intersphinx for cross-references. Build HTML, PDF, or EPUB output.

Deprecation warnings notify users about removed functionality. Use warnings.warn with DeprecationWarning, document in docstrings with deprecated directive. Provide migration path to replacement functions.

Doctests embed examples in docstrings and verify they execute correctly. Write examples showing typical usage, edge cases, and error conditions. Run with doctest.testmod() to validate.

Self-documenting code reduces comment needs through clear naming and structure. Well-named functions, variables, and classes explain what code does. Comments explain why decisions were made and important constraints.

Documentation serves different audiences - docstrings for API users, comments for code maintainers, README for new users, Sphinx for comprehensive reference. Write for each audience appropriately.

Effective documentation balances completeness with maintainability. Document public APIs thoroughly, internal implementation lightly. Keep documentation synchronized with code through automatic generation and regular reviews.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [How to Write Pythonic Code](/en/learn/swe/prog-lang/python/how-to/write-pythonic-code)
- [How to Write Effective Tests](/en/learn/swe/prog-lang/python/how-to/write-effective-tests)
