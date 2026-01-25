---
name: swe-programming-python
description: Python coding standards from authoritative docs/explanation/software/prog-lang/python/ documentation
---

# Python Coding Standards

## Purpose

Progressive disclosure of Python coding standards for agents writing Python code.

**Authoritative Source**: [docs/explanation/software/prog-lang/python/README.md](../../../docs/explanation/software/prog-lang/python/README.md)

**Usage**: Auto-loaded for agents when writing Python code. Provides quick reference to idioms, best practices, and antipatterns.

## Quick Standards Reference

### Naming Conventions

**Modules and Packages**: lowercase_with_underscores

- `user_account.py`, `payment_processor.py`

**Classes**: PascalCase

- `UserAccount`, `PaymentProcessor`

**Functions and Variables**: lowercase_with_underscores

- Functions: `calculate_total()`, `find_user_by_id()`
- Variables: `user_name`, `total_amount`

**Constants**: UPPER_CASE_WITH_UNDERSCORES

- `MAX_RETRIES`, `DEFAULT_TIMEOUT`, `API_ENDPOINT`

**Private**: Single leading underscore

- `_internal_function()`, `_private_var`

### Modern Python Features (3.11+)

**Type Hints**: Use for all function signatures

```python
def calculate_total(items: list[Item]) -> Decimal:
    return sum(item.price for item in items)
```

**Dataclasses**: Use for data containers

```python
from dataclasses import dataclass

@dataclass(frozen=True)
class UserAccount:
    id: str
    name: str
    created_at: datetime
```

**Pattern Matching**: Use for complex conditionals

```python
match payment:
    case CreditCard(number=n):
        process_credit_card(n)
    case BankTransfer(account=a):
        process_bank_transfer(a)
```

**f-strings**: Preferred for string formatting

```python
message = f"User {name} has {count} items"
```

### Error Handling

**Specific Exceptions**: Catch specific exceptions

```python
try:
    result = process_payment(amount)
except ValueError as e:
    logger.error(f"Invalid amount: {e}")
except NetworkError as e:
    logger.error(f"Network error: {e}")
```

**Custom Exceptions**: Define for domain errors

```python
class ValidationError(Exception):
    def __init__(self, field: str, message: str):
        self.field = field
        super().__init__(message)
```

**Context Managers**: Use for resource management

```python
with open('file.txt', 'r') as f:
    content = f.read()
```

### Testing Standards

**pytest**: Primary testing framework

- Use `def test_*` for test functions
- Use fixtures for setup
- Use parametrize for data-driven tests

```python
import pytest

@pytest.mark.parametrize('input,expected', [
    (5, 10),
    (0, 0),
    (-5, -10),
])
def test_double(input, expected):
    assert double(input) == expected
```

**Type Checking**: Use mypy for static analysis

```python
# Run: mypy src/
```

### Security Practices

**Input Validation**: Validate all external input

- Use Pydantic for data validation
- Check types and bounds

**SQL Injection**: Use parameterized queries

```python
cursor.execute("SELECT * FROM users WHERE id = ?", (user_id,))
```

**Secrets Management**: Never hardcode secrets

- Use environment variables
- Use python-dotenv for local development

```python
import os
from dotenv import load_dotenv

load_dotenv()
api_key = os.getenv('API_KEY')
```

## Comprehensive Documentation

For detailed guidance, refer to:

- **[Idioms](../../../docs/explanation/software/prog-lang/python/ex-so-prla-py__idioms.md)** - Python-specific patterns
- **[Best Practices](../../../docs/explanation/software/prog-lang/python/ex-so-prla-py__best-practices.md)** - Clean code standards
- **[Anti-Patterns](../../../docs/explanation/software/prog-lang/python/ex-so-prla-py__anti-patterns.md)** - Common mistakes

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [Python README](../../../docs/explanation/software/prog-lang/python/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
