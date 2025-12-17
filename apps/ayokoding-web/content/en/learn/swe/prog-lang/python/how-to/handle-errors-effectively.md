---
title: "How to Handle Errors Effectively"
date: 2025-12-17T10:54:44+07:00
draft: false
weight: 504
description: "Master Python's exception system with proper error handling patterns"
tags: ["python", "exceptions", "error-handling", "eafp"]
categories: ["learn"]
---

## Problem

Error handling in Python differs from languages with checked exceptions. Python's exception system supports the EAFP (Easier to Ask for Forgiveness than Permission) philosophy, but misusing exceptions creates bugs and performance issues.

This guide shows how to handle errors effectively in Python with practical patterns.

## Understanding Python's Exception Hierarchy

Python organizes exceptions in a hierarchy. Understanding it helps you catch the right exceptions.

```mermaid
%%{ Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161 }%%
graph TD
    BaseException --> SystemExit
    BaseException --> KeyboardInterrupt
    BaseException --> Exception
    Exception --> ValueError
    Exception --> TypeError
    Exception --> KeyError
    Exception --> AttributeError
    Exception --> IOError
    IOError --> FileNotFoundError

    style BaseException fill:#0173B2,stroke:#000,color:#fff
    style Exception fill:#DE8F05,stroke:#000,color:#000
    style ValueError fill:#029E73,stroke:#000,color:#fff
    style TypeError fill:#029E73,stroke:#000,color:#fff
    style KeyError fill:#029E73,stroke:#000,color:#fff
```

### Exception Categories

```python
# System exceptions (don't catch)
BaseException
├── SystemExit      # sys.exit() was called
├── KeyboardInterrupt  # Ctrl+C pressed
└── GeneratorExit   # Generator closed

# Application exceptions (catch these)
Exception
├── ValueError      # Invalid value (wrong type OK, value bad)
├── TypeError       # Wrong type
├── KeyError        # Missing dict key
├── AttributeError  # Missing attribute
├── IOError         # I/O operation failed
│   └── FileNotFoundError
├── RuntimeError    # Generic runtime error
└── ...
```

## EAFP vs LBYL

Python favors EAFP (Easier to Ask for Forgiveness than Permission) over LBYL (Look Before You Leap).

### EAFP Approach

```python
# ✅ EAFP - Try operation, handle failure
def get_user_email(users, user_id):
    try:
        return users[user_id].email
    except KeyError:
        return None  # User not found
    except AttributeError:
        return None  # User has no email

# Clean code focused on happy path
email = get_user_email(users, 123)
if email:
    send_notification(email)
```

### LBYL Approach

```python
# ❌ LBYL - Check before operating (verbose and racy)
def get_user_email(users, user_id):
    if user_id in users:
        user = users[user_id]
        if hasattr(user, 'email'):
            return user.email
    return None

# Still vulnerable to race conditions in concurrent code
# Between check and use, state might change
```

### When to Use Each

```python
# ✅ Use EAFP for normal operations
def read_config(filename):
    try:
        with open(filename) as f:
            return json.load(f)
    except FileNotFoundError:
        return default_config()
    except json.JSONDecodeError as e:
        raise ConfigError(f"Invalid JSON in {filename}: {e}")

# ✅ Use LBYL when exceptions are expensive in tight loops
def process_large_dataset(items):
    # Pre-validate to avoid exception overhead
    if not all(isinstance(item, dict) for item in items):
        raise TypeError("All items must be dicts")

    # Now safe to process without try/except in loop
    for item in items:
        process_item(item)  # No exceptions expected
```

## Catching Specific Exceptions

Always catch specific exceptions, never use bare except.

```python
# ❌ Bare except catches everything (including KeyboardInterrupt!)
def load_data():
    try:
        return json.load(open('data.json'))
    except:  # NEVER do this
        return None

# ❌ Catching Exception is too broad
def load_data():
    try:
        return json.load(open('data.json'))
    except Exception:  # Catches too much
        return None

# ✅ Catch specific exceptions
def load_data():
    try:
        with open('data.json') as f:
            return json.load(f)
    except FileNotFoundError:
        logger.warning("Config file not found, using defaults")
        return default_config()
    except json.JSONDecodeError as e:
        logger.error(f"Invalid JSON in config: {e}")
        raise ConfigError("Corrupted configuration file") from e
    except PermissionError:
        raise ConfigError("Cannot read config file - permission denied")

# ✅ Multiple exceptions with same handler
def process_user_input(value):
    try:
        return int(value)
    except (ValueError, TypeError) as e:
        logger.debug(f"Invalid input: {e}")
        return 0
```

## Creating Custom Exceptions

Custom exceptions provide semantic context and enable targeted catching.

```python
# ✅ Custom exception hierarchy
class ApplicationError(Exception):
    """Base exception for all application errors."""
    pass

class ConfigError(ApplicationError):
    """Configuration-related errors."""
    pass

class ValidationError(ApplicationError):
    """Input validation errors."""
    def __init__(self, field, message):
        self.field = field
        super().__init__(f"{field}: {message}")

class AuthenticationError(ApplicationError):
    """Authentication failures."""
    pass

# Usage
def validate_email(email):
    if '@' not in email:
        raise ValidationError('email', 'Must contain @')
    if not email.endswith('.com'):
        raise ValidationError('email', 'Must end with .com')
    return email

# Caller can catch specific errors
try:
    email = validate_email(user_input)
except ValidationError as e:
    print(f"Validation failed: {e}")
    print(f"Field: {e.field}")
except ApplicationError as e:
    print(f"Application error: {e}")
```

## Exception Chaining

Preserve exception context when wrapping or re-raising.

```python
# ❌ Lost exception context
def fetch_user(user_id):
    try:
        response = requests.get(f'/users/{user_id}')
        return response.json()
    except requests.RequestException:
        raise UserNotFoundError(f"User {user_id} not found")
        # Original exception lost!

# ✅ Chain exceptions with 'from'
def fetch_user(user_id):
    try:
        response = requests.get(f'/users/{user_id}')
        return response.json()
    except requests.RequestException as e:
        raise UserNotFoundError(f"User {user_id} not found") from e
        # Preserves original exception as __cause__

# ✅ Implicit chaining (exception raised while handling another)
def process_order(order_id):
    try:
        order = fetch_order(order_id)
    except OrderNotFoundError:
        send_alert("Order not found")  # If this raises, chained automatically
        raise

# ✅ Suppress chaining when not relevant
def parse_config(text):
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        raise ConfigError("Invalid config format") from None
        # Explicitly suppress cause
```

## Try/Except/Else/Finally

Python's try statement has four clauses with specific purposes.

```python
# ✅ Complete try statement structure
def process_file(filename):
    try:
        f = open(filename)
        data = f.read()
    except FileNotFoundError:
        logger.error(f"File not found: {filename}")
        return None
    except PermissionError:
        logger.error(f"Permission denied: {filename}")
        return None
    else:
        # Runs only if no exception occurred
        logger.info(f"Successfully read {filename}")
        return process_data(data)
    finally:
        # Always runs (even with return/exception)
        try:
            f.close()
        except:
            pass  # Ignore errors when cleaning up

# ✅ Else clause for operations after try
def update_cache(key, value):
    try:
        lock.acquire()
    except LockError:
        logger.warning("Could not acquire lock")
        return False
    else:
        # Only runs if lock acquired successfully
        cache[key] = value
        return True
    finally:
        # Always release lock if acquired
        if lock.is_locked():
            lock.release()
```

## Context Managers for Automatic Cleanup

Context managers guarantee cleanup even with exceptions.

```python
# ✅ Built-in context managers
def process_file(filename):
    with open(filename) as f:
        return f.read()  # File closed even if exception occurs

# ✅ Multiple resources
def copy_file(source, dest):
    with open(source, 'rb') as src, open(dest, 'wb') as dst:
        dst.write(src.read())

# ✅ Custom context manager (class-based)
class DatabaseTransaction:
    def __init__(self, connection):
        self.connection = connection

    def __enter__(self):
        self.connection.begin()
        return self.connection

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type is None:
            self.connection.commit()
        else:
            self.connection.rollback()
        return False  # Don't suppress exceptions

with DatabaseTransaction(conn) as db:
    db.execute("INSERT INTO users ...")
    db.execute("UPDATE accounts ...")
    # Commits if successful, rolls back if exception

# ✅ Custom context manager (generator-based)
from contextlib import contextmanager

@contextmanager
def timer(name):
    import time
    start = time.time()
    try:
        yield
    finally:
        elapsed = time.time() - start
        print(f"{name} took {elapsed:.2f}s")

with timer("Database query"):
    results = database.query("SELECT * FROM users")
```

## Logging Exceptions

Proper logging preserves stack traces for debugging.

```python
import logging

logger = logging.getLogger(__name__)

# ❌ Lost stack trace
def process_request():
    try:
        result = perform_operation()
    except Exception as e:
        logger.error(f"Operation failed: {e}")  # Only message, no stack trace
        return None

# ✅ Log with stack trace
def process_request():
    try:
        result = perform_operation()
    except Exception:
        logger.exception("Operation failed")  # Logs full stack trace
        return None

# ✅ Log with context
def process_request(request_id):
    try:
        result = perform_operation()
    except Exception:
        logger.exception(f"Operation failed for request {request_id}")
        raise  # Re-raise after logging

# ✅ Different log levels for different exceptions
def fetch_data(url):
    try:
        response = requests.get(url, timeout=5)
        return response.json()
    except requests.Timeout:
        logger.warning(f"Timeout fetching {url}")  # Expected, just warning
        return None
    except requests.RequestException:
        logger.exception(f"Failed to fetch {url}")  # Unexpected, full trace
        raise
```

## Validation Patterns

Different patterns for different validation scenarios.

```python
# ✅ Validate and raise for invalid input
def create_user(name, age, email):
    if not name:
        raise ValueError("Name is required")
    if age < 0:
        raise ValueError("Age must be non-negative")
    if '@' not in email:
        raise ValueError("Invalid email format")

    return User(name, age, email)

# ✅ Collect multiple validation errors
def validate_user_data(data):
    errors = []

    if not data.get('name'):
        errors.append("Name is required")
    if data.get('age', 0) < 0:
        errors.append("Age must be non-negative")
    if '@' not in data.get('email', ''):
        errors.append("Invalid email format")

    if errors:
        raise ValidationError(errors)

    return data

# ✅ Return result object for validation
from dataclasses import dataclass
from typing import Optional

@dataclass
class ValidationResult:
    is_valid: bool
    errors: list[str]
    value: Optional[dict] = None

def validate_user(data):
    errors = []

    if not data.get('name'):
        errors.append("Name is required")
    if data.get('age', 0) < 0:
        errors.append("Age must be non-negative")

    if errors:
        return ValidationResult(False, errors)

    return ValidationResult(True, [], data)

# Usage
result = validate_user(user_data)
if result.is_valid:
    create_user(**result.value)
else:
    display_errors(result.errors)
```

## Retry Patterns

Handle transient failures with retry logic.

```python
import time
from functools import wraps

# ✅ Simple retry decorator
def retry(max_attempts=3, delay=1, exceptions=(Exception,)):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except exceptions as e:
                    if attempt == max_attempts - 1:
                        raise  # Last attempt, re-raise
                    logger.warning(
                        f"{func.__name__} failed (attempt {attempt + 1}): {e}"
                    )
                    time.sleep(delay)
        return wrapper
    return decorator

@retry(max_attempts=3, delay=2, exceptions=(requests.RequestException,))
def fetch_api_data(url):
    response = requests.get(url, timeout=5)
    response.raise_for_status()
    return response.json()

# ✅ Exponential backoff
def retry_with_backoff(max_attempts=5, base_delay=1):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_attempts - 1:
                        raise
                    delay = base_delay * (2 ** attempt)
                    logger.warning(f"Retry in {delay}s: {e}")
                    time.sleep(delay)
        return wrapper
    return decorator
```

## Summary

Effective error handling in Python starts with understanding the exception hierarchy and catching specific exceptions rather than broad categories. Exception inherits from BaseException but so do SystemExit and KeyboardInterrupt - catching Exception avoids intercepting system exceptions that should propagate. Always catch the most specific exception that matches your recovery strategy, and use multiple except clauses to handle different failures differently.

The EAFP philosophy (Easier to Ask for Forgiveness than Permission) leads to cleaner code that focuses on the happy path. Try the operation and handle exceptions rather than checking conditions upfront. LBYL (Look Before You Leap) makes sense in tight loops where exception overhead matters, but for most code EAFP produces more readable and race-condition-free logic.

Custom exceptions provide semantic meaning beyond built-in exceptions. Create a hierarchy rooted in a base ApplicationError class, then derive specific exceptions for different failure categories. Include relevant context in exception instances through custom attributes, making debugging and targeted catching easier.

Exception chaining with the `from` keyword preserves the original exception as context when wrapping or translating errors. This maintains debugging information while presenting application-appropriate exceptions to callers. Use `from None` to explicitly suppress chaining when the original exception isn't relevant.

The else clause in try/except/else/finally runs only when no exception occurs, providing a clean place for code that should run after successful try blocks. The finally clause always runs regardless of exceptions or return statements, making it ideal for cleanup. Context managers provide a cleaner abstraction for try/finally patterns, automatically handling cleanup through **enter** and **exit** methods.

Log exceptions with logger.exception() to capture full stack traces, not just exception messages. Different exception types warrant different log levels - expected exceptions like timeouts might be warnings while unexpected failures deserve error logs with full traces. Validation can raise immediately for single errors, collect errors for batch validation, or return result objects for flexible handling.

Retry patterns handle transient failures in external systems. Simple retry loops with fixed delays work for basic scenarios, while exponential backoff prevents overwhelming failing services. Wrap retry logic in decorators to keep business logic clean and retry behavior configurable.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [Common Python Anti-Patterns](/en/learn/swe/prog-lang/python/explanation/anti-patterns)
- [How to Write Pythonic Code](/en/learn/swe/prog-lang/python/how-to/write-pythonic-code)
- [How to Avoid Common Pitfalls](/en/learn/swe/prog-lang/python/how-to/avoid-common-pitfalls)
