---
title: "How to Debug and Log Effectively"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000014
description: "Master Python debugging with pdb, logging module, and effective debugging strategies"
tags: ["python", "debugging", "logging", "pdb", "ipdb"]
categories: ["learn"]
---

## Problem

Python's dynamic nature makes debugging essential for catching runtime errors. Print debugging clutters code and lacks context. The logging module offers powerful features but requires understanding handlers, formatters, and log levels. pdb provides interactive debugging but has a learning curve.

This guide shows effective debugging and logging in Python.

## Using pdb Debugger

### Basic pdb Commands

```python
import pdb

def calculate_total(items):
    total = 0
    for item in items:
        # ✅ Set breakpoint
        pdb.set_trace()  # Legacy (Python < 3.7)
        breakpoint()     # Python 3.7+ (preferred)

        total += item['price'] * item['quantity']
    return total

# When breakpoint hits:
# (Pdb) p item           # Print variable
# (Pdb) pp items         # Pretty print
# (Pdb) l                # List source code
# (Pdb) n                # Next line
# (Pdb) s                # Step into function
# (Pdb) c                # Continue execution
# (Pdb) q                # Quit debugger
```

### Common pdb Commands

```python
def process_order(order):
    breakpoint()

    # Inside debugger:
    # p order              # Print variable
    # pp order             # Pretty print (formatted)
    # type(order)          # Check type
    # dir(order)           # List attributes
    # help n               # Help on command

    # Navigation:
    # n (next)             # Execute current line
    # s (step)             # Step into function
    # r (return)           # Continue until function returns
    # c (continue)         # Continue execution
    # u (up)               # Move up stack frame
    # d (down)             # Move down stack frame

    # Breakpoints:
    # b 25                 # Set breakpoint at line 25
    # b function_name      # Break at function entry
    # b                    # List breakpoints
    # cl 1                 # Clear breakpoint 1

    # Execution:
    # !variable = value    # Set variable
    # !some_function()     # Call function
```

### Conditional Breakpoints

```python
def process_items(items):
    for item in items:
        # ✅ Conditional breakpoint in code
        if item['price'] > 1000:
            breakpoint()

        process_item(item)

# ✅ Conditional breakpoint in pdb
# (Pdb) b 10, item['price'] > 1000
# Breakpoint 1 at line 10
```

## ipdb for Enhanced Debugging

### Using ipdb

```python
# Install: pip install ipdb
import ipdb

def complex_function(data):
    # ✅ Enhanced debugger with syntax highlighting
    ipdb.set_trace()

    # Tab completion works
    # Syntax highlighting
    # Better output formatting

    result = process_data(data)
    return result

# ✅ Post-mortem debugging
try:
    risky_operation()
except Exception:
    ipdb.post_mortem()  # Debug at exception point
```

## Logging Module

### Basic Logging

```python
import logging

# ✅ Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

logger = logging.getLogger(__name__)

# ✅ Log messages
def process_payment(amount):
    logger.debug(f"Processing payment: ${amount}")
    logger.info(f"Payment initiated: ${amount}")

    try:
        charge_card(amount)
        logger.info(f"Payment successful: ${amount}")
    except PaymentError as e:
        logger.error(f"Payment failed: {e}")
        raise
    except Exception as e:
        logger.exception("Unexpected error during payment")
        raise

# Log levels (lowest to highest):
# DEBUG: Detailed diagnostic info
# INFO: General informational messages
# WARNING: Warning messages
# ERROR: Error messages
# CRITICAL: Critical errors
```

### Logging Configuration

```python
import logging
import logging.handlers

# ✅ Advanced configuration
def setup_logging():
    # Create logger
    logger = logging.getLogger('myapp')
    logger.setLevel(logging.DEBUG)

    # Console handler (INFO and above)
    console_handler = logging.StreamHandler()
    console_handler.setLevel(logging.INFO)
    console_format = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    console_handler.setFormatter(console_format)

    # File handler (DEBUG and above)
    file_handler = logging.FileHandler('app.log')
    file_handler.setLevel(logging.DEBUG)
    file_format = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(filename)s:%(lineno)d - %(message)s'
    )
    file_handler.setFormatter(file_format)

    # Rotating file handler
    rotating_handler = logging.handlers.RotatingFileHandler(
        'app.log',
        maxBytes=10485760,  # 10MB
        backupCount=5
    )
    rotating_handler.setFormatter(file_format)

    # Add handlers
    logger.addHandler(console_handler)
    logger.addHandler(file_handler)

    return logger

logger = setup_logging()
```

### Structured Logging

```python
import logging

logger = logging.getLogger(__name__)

# ✅ Include context in logs
def process_user(user_id, action):
    logger.info(
        "Processing user action",
        extra={
            'user_id': user_id,
            'action': action,
            'timestamp': datetime.now().isoformat()
        }
    )

# ✅ With structlog for better structure
import structlog

structlog.configure(
    processors=[
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.JSONRenderer()
    ]
)

log = structlog.get_logger()
log.info("user_action", user_id="123", action="login", success=True)
# Output: {"event": "user_action", "user_id": "123", "action": "login", "success": true, "timestamp": "2025-12-17T13:19:07"}
```

### Logging Best Practices

```python
import logging

logger = logging.getLogger(__name__)

# ✅ Log at appropriate levels
def process_order(order_id):
    logger.debug(f"Starting order processing: {order_id}")

    try:
        order = fetch_order(order_id)
        logger.info(f"Processing order {order_id} for ${order.total}")

        validate_order(order)
        charge_payment(order)

        logger.info(f"Order {order_id} completed successfully")

    except ValidationError as e:
        logger.warning(f"Order {order_id} validation failed: {e}")
        raise
    except PaymentError as e:
        logger.error(f"Payment failed for order {order_id}: {e}")
        raise
    except Exception as e:
        logger.exception(f"Unexpected error processing order {order_id}")
        raise

# ✅ Use logger.exception for exceptions
try:
    risky_operation()
except Exception:
    logger.exception("Operation failed")  # Includes stack trace

# ❌ Don't log sensitive data
logger.info(f"User login: {username}")  # OK
# logger.debug(f"Password: {password}")  # NEVER!

# ✅ Use lazy formatting
logger.debug("Processing %d items", item_count)  # Formatted only if logged
# logger.debug(f"Processing {item_count} items")  # Formatted always
```

## Debugging Strategies

### Print Debugging

```python
# ✅ Temporary debug prints (remove before commit)
def calculate_discount(price, customer_type):
    print(f"DEBUG: price={price}, customer_type={customer_type}")

    if customer_type == "premium":
        discount = price * 0.20
        print(f"DEBUG: premium discount={discount}")
        return discount

    print(f"DEBUG: no discount")
    return 0

# ✅ Better: Use logging for debug prints
logger.debug(f"price={price}, customer_type={customer_type}")
```

### Assert for Sanity Checks

```python
def calculate_total(items):
    total = sum(item['price'] for item in items)

    # ✅ Assert expected conditions
    assert total >= 0, f"Total cannot be negative: {total}"
    assert isinstance(total, (int, float)), f"Total must be numeric: {type(total)}"

    return total

# Assertions disabled with python -O (optimized mode)
# Use for development checks, not production validation
```

### Debugging Decorators

```python
import functools
import logging

logger = logging.getLogger(__name__)

# ✅ Debug decorator
def debug(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        args_repr = [repr(a) for a in args]
        kwargs_repr = [f"{k}={v!r}" for k, v in kwargs.items()]
        signature = ", ".join(args_repr + kwargs_repr)

        logger.debug(f"Calling {func.__name__}({signature})")

        result = func(*args, **kwargs)

        logger.debug(f"{func.__name__} returned {result!r}")
        return result

    return wrapper

@debug
def calculate_total(price, quantity):
    return price * quantity

# Logs:
# Calling calculate_total(10, 5)
# calculate_total returned 50
```

## Profiling

### Time Profiling

```python
import cProfile
import pstats

# ✅ Profile function
def profile_function():
    cProfile.run('expensive_operation()', 'profile_stats')

    # Analyze results
    stats = pstats.Stats('profile_stats')
    stats.strip_dirs()
    stats.sort_stats('cumulative')
    stats.print_stats(10)  # Top 10 functions

# ✅ Profile with decorator
import functools
import time

def timing(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        start = time.perf_counter()
        result = func(*args, **kwargs)
        end = time.perf_counter()
        print(f"{func.__name__} took {end - start:.4f}s")
        return result
    return wrapper

@timing
def slow_function():
    time.sleep(1)
    return "done"

# ✅ Line profiling with line_profiler
# Install: pip install line_profiler
# Usage: kernprof -l -v script.py

@profile  # Added by line_profiler
def function_to_profile():
    result = []
    for i in range(1000):
        result.append(i * 2)
    return result
```

### Memory Profiling

```python
# Install: pip install memory_profiler
from memory_profiler import profile

@profile
def memory_intensive():
    big_list = [i for i in range(1000000)]
    return sum(big_list)

# Run: python -m memory_profiler script.py
# Shows memory usage line by line
```

## Exception Debugging

### Post-Mortem Debugging

```python
import pdb
import sys

def risky_operation():
    x = 10
    y = 0
    return x / y  # Raises ZeroDivisionError

# ✅ Automatic post-mortem on uncaught exception
def main():
    try:
        risky_operation()
    except Exception:
        import traceback
        traceback.print_exc()
        pdb.post_mortem()  # Debug at exception point

# ✅ Global exception handler
def debug_hook(type, value, tb):
    if hasattr(sys, 'ps1') or not sys.stderr.isatty():
        # Interactive mode or no terminal
        sys.__excepthook__(type, value, tb)
    else:
        import traceback
        import pdb
        traceback.print_exception(type, value, tb)
        pdb.post_mortem(tb)

sys.excepthook = debug_hook
```

## Testing and Debugging

### Capturing Logs in Tests

```python
import logging
import pytest

def test_logging_output(caplog):
    with caplog.at_level(logging.INFO):
        logger.info("Test message")

    assert "Test message" in caplog.text
    assert len(caplog.records) == 1
    assert caplog.records[0].levelname == "INFO"

# ✅ Test specific logger
def test_app_logger(caplog):
    with caplog.at_level(logging.DEBUG, logger='myapp'):
        app_logger = logging.getLogger('myapp')
        app_logger.debug("Debug message")

    assert "Debug message" in caplog.text
```

## Summary

Python debugging combines pdb interactive debugger, logging for production insights, and strategic print debugging. pdb sets breakpoints with breakpoint(), inspects variables with p and pp, navigates with n/s/c commands. ipdb enhances pdb with syntax highlighting and tab completion.

Logging module provides structured, level-based output superior to print statements. Configure handlers for console and file output, formatters for message structure, and levels for severity filtering. Use logger.exception() for exceptions to include stack traces automatically.

Log levels communicate severity and audience. DEBUG for development diagnostics, INFO for business events, WARNING for unexpected but handled situations, ERROR for failures, CRITICAL for severe errors requiring immediate attention. Choose levels based on who needs the information.

Structured logging with extra parameters or structlog adds context beyond message text. Include user IDs, request IDs, timestamps, and relevant business data. JSON output enables parsing by log aggregation tools.

Never log sensitive data - passwords, API keys, credit card numbers, or personal information. Log user IDs and email addresses for correlation, but omit secrets. Use lazy formatting with % style to avoid formatting overhead when log level is disabled.

Print debugging suits quick investigations but should be removed before committing. Use assertions for sanity checks in development - disabled in optimized mode. Debug decorators log function calls and returns automatically.

Profiling with cProfile identifies performance bottlenecks. Profile expensive operations, analyze with pstats, focus optimization on top time consumers. line_profiler shows line-by-line timings, memory_profiler tracks memory usage.

Post-mortem debugging with pdb.post_mortem() investigates crashes at exception point. Set sys.excepthook for automatic post-mortem on uncaught exceptions. Inspect variables, stack frames, and execution state.

Testing captures logs with pytest's caplog fixture. Assert expected log messages appear, verify log levels, check logger names. Test both success and error logging paths.

Effective debugging combines tools based on problem type. pdb for logic errors, logging for production issues, profiling for performance problems, asserts for development checks. Master all approaches for efficient debugging.

## Related Content

- [Python Best Practices](/en/learn/swe/prog-lang/python/explanation/best-practices)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/python/how-to/handle-errors-effectively)
- [How to Write Effective Tests](/en/learn/swe/prog-lang/python/how-to/write-effective-tests)
