---
title: "Write Effective Tests"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000013
description: "Master Python testing with pytest, fixtures, mocking, and effective test organization"
tags: ["python", "testing", "pytest", "fixtures", "mocking"]
categories: ["learn"]
---

## Problem

Python's dynamic nature makes testing critical for catching bugs that static typing would prevent in other languages. unittest provides basic testing but requires verbose boilerplate. pytest offers powerful features but requires understanding fixtures, parametrization, and assertion introspection.

This guide shows effective testing patterns in Python.

## pytest Basics

### Simple Test Functions

```python
# test_calculator.py
def add(a, b):
    return a + b

# ✅ Test functions start with test_
def test_add_positive_numbers():
    assert add(2, 3) == 5

def test_add_negative_numbers():
    assert add(-1, -2) == -3

def test_add_zero():
    assert add(5, 0) == 5

# ✅ Descriptive assertions with messages
def test_add_floats():
    result = add(0.1, 0.2)
    assert result == pytest.approx(0.3), f"Expected 0.3, got {result}"
```

**Running tests:**

```bash
# Run all tests
pytest

# Run specific file
pytest test_calculator.py

# Run specific test
pytest test_calculator.py::test_add_positive_numbers

# Verbose output
pytest -v

# Show print statements
pytest -s
```

### Test Classes

```python
# ✅ Group related tests in classes
class TestUser:
    def test_create_user(self):
        user = User("alice@example.com")
        assert user.email == "alice@example.com"

    def test_user_validation(self):
        with pytest.raises(ValueError):
            User("invalid-email")

    def test_user_age_validation(self):
        user = User("bob@example.com")
        user.age = -1
        with pytest.raises(ValueError):
            user.validate()
```

## Fixtures

### Basic Fixtures

```python
import pytest

# ✅ Setup fixture
@pytest.fixture
def user():
    return User("alice@example.com", age=25)

# ✅ Use fixture in tests
def test_user_email(user):
    assert user.email == "alice@example.com"

def test_user_age(user):
    assert user.age == 25

# ✅ Fixture with setup and teardown
@pytest.fixture
def database():
    db = Database()
    db.connect()
    yield db  # Test runs here
    db.disconnect()  # Cleanup

def test_query_users(database):
    users = database.query("SELECT * FROM users")
    assert len(users) > 0
```

### Fixture Scope

```python
# ✅ Function scope (default) - new instance per test
@pytest.fixture
def user():
    return User("test@example.com")

# ✅ Class scope - shared within test class
@pytest.fixture(scope="class")
def database():
    db = Database()
    db.connect()
    yield db
    db.disconnect()

# ✅ Module scope - shared across module
@pytest.fixture(scope="module")
def app_config():
    return load_config("test_config.yaml")

# ✅ Session scope - shared across all tests
@pytest.fixture(scope="session")
def browser():
    driver = webdriver.Chrome()
    yield driver
    driver.quit()
```

### conftest.py for Shared Fixtures

```python
# conftest.py - fixtures available to all tests in directory
import pytest

@pytest.fixture
def db_connection():
    """Database connection used by all tests"""
    conn = create_connection()
    yield conn
    conn.close()

@pytest.fixture
def sample_user():
    """Sample user for testing"""
    return User("test@example.com", age=25)

# test_models.py
def test_save_user(db_connection, sample_user):
    # Fixtures automatically available
    db_connection.save(sample_user)
    assert db_connection.find_by_email("test@example.com") is not None
```

## Parametrized Tests

### Basic Parametrization

```python
# ✅ Test multiple inputs
@pytest.mark.parametrize("a,b,expected", [
    (2, 3, 5),
    (-1, 1, 0),
    (0, 0, 0),
    (10, -5, 5),
])
def test_add(a, b, expected):
    assert add(a, b) == expected

# ✅ Named parameters for clarity
@pytest.mark.parametrize("email,valid", [
    ("alice@example.com", True),
    ("invalid.email", False),
    ("", False),
    ("test@", False),
    ("@example.com", False),
])
def test_email_validation(email, valid):
    assert is_valid_email(email) == valid

# ✅ Multiple parameters
@pytest.mark.parametrize("age", [16, 17, -1, 0])
@pytest.mark.parametrize("email", ["valid@example.com", ""])
def test_user_validation(age, email):
    # Runs 8 tests (4 ages × 2 emails)
    if age < 18 or not email:
        with pytest.raises(ValidationError):
            User(email, age)
```

### Parametrize with IDs

```python
# ✅ Custom test IDs for readability
@pytest.mark.parametrize("input,expected", [
    ("hello", "HELLO"),
    ("World", "WORLD"),
    ("123", "123"),
], ids=["lowercase", "mixed", "numbers"])
def test_uppercase(input, expected):
    assert input.upper() == expected

# Test output:
# test_uppercase[lowercase] PASSED
# test_uppercase[mixed] PASSED
# test_uppercase[numbers] PASSED
```

## Mocking

### unittest.mock Basics

```python
from unittest.mock import Mock, patch, MagicMock

# ✅ Mock object
def test_api_call():
    mock_api = Mock()
    mock_api.get_user.return_value = {"name": "Alice", "age": 25}

    service = UserService(mock_api)
    user = service.fetch_user("123")

    assert user.name == "Alice"
    mock_api.get_user.assert_called_once_with("123")

# ✅ Mock with side effects
def test_retry_logic():
    mock_api = Mock()
    # First call fails, second succeeds
    mock_api.fetch_data.side_effect = [ConnectionError(), {"data": "success"}]

    service = Service(mock_api)
    result = service.fetch_with_retry()

    assert result == {"data": "success"}
    assert mock_api.fetch_data.call_count == 2

# ✅ Patch function
@patch('myapp.send_email')
def test_user_registration(mock_send_email):
    register_user("alice@example.com", "password")

    mock_send_email.assert_called_once()
    args, kwargs = mock_send_email.call_args
    assert "alice@example.com" in args

# ✅ Patch as context manager
def test_external_api():
    with patch('requests.get') as mock_get:
        mock_response = Mock()
        mock_response.json.return_value = {"data": "test"}
        mock_get.return_value = mock_response

        result = fetch_data_from_api()

        assert result == {"data": "test"}
        mock_get.assert_called_once()
```

### Mock Classes and Methods

```python
# ✅ Mock entire class
@patch('myapp.database.Database')
def test_save_user(MockDatabase):
    mock_db_instance = MockDatabase.return_value
    mock_db_instance.save.return_value = True

    service = UserService()
    result = service.create_user("alice@example.com")

    assert result is True
    mock_db_instance.save.assert_called_once()

# ✅ Patch object method
def test_user_method():
    user = User("test@example.com")

    with patch.object(user, 'send_email') as mock_send:
        user.register()

        mock_send.assert_called_once()
```

## Testing Exceptions

### Asserting Exceptions

```python
import pytest

# ✅ Assert exception is raised
def test_divide_by_zero():
    with pytest.raises(ZeroDivisionError):
        divide(10, 0)

# ✅ Check exception message
def test_invalid_email():
    with pytest.raises(ValueError, match="Invalid email"):
        User("invalid-email")

# ✅ Capture exception for inspection
def test_validation_error():
    with pytest.raises(ValidationError) as exc_info:
        validate_age(-5)

    assert "age" in str(exc_info.value)
    assert exc_info.value.field == "age"

# ✅ Test no exception raised
def test_valid_input():
    # No assertion needed - test passes if no exception
    process_data({"valid": "data"})
```

## Test Organization

### File Structure

```
tests/
├── conftest.py              # Shared fixtures
├── test_models.py           # Model tests
├── test_services.py         # Service tests
├── test_api.py              # API tests
└── integration/
    ├── conftest.py          # Integration fixtures
    └── test_database.py     # Integration tests
```

### Marking Tests

```python
import pytest

# ✅ Mark slow tests
@pytest.mark.slow
def test_large_dataset():
    # Long-running test
    pass

# ✅ Mark integration tests
@pytest.mark.integration
def test_database_connection():
    pass

# ✅ Skip tests conditionally
@pytest.mark.skipif(sys.platform == "win32", reason="Unix only")
def test_unix_specific():
    pass

# ✅ Expected failure
@pytest.mark.xfail(reason="Known bug #123")
def test_known_issue():
    pass
```

**Run specific marks:**

```bash
# Run only fast tests
pytest -m "not slow"

# Run integration tests
pytest -m integration

# Run specific marks
pytest -m "slow or integration"
```

## Coverage

### Running with Coverage

```bash
# Install pytest-cov
pip install pytest-cov

# Run with coverage
pytest --cov=myapp

# Generate HTML report
pytest --cov=myapp --cov-report=html

# Show missing lines
pytest --cov=myapp --cov-report=term-missing

# Fail if coverage below threshold
pytest --cov=myapp --cov-fail-under=80
```

### Coverage Configuration

```ini
# .coveragerc or pyproject.toml
[coverage:run]
omit =
    */tests/*
    */migrations/*
    */venv/*

[coverage:report]
exclude_lines =
    pragma: no cover
    def __repr__
    raise AssertionError
    raise NotImplementedError
    if __name__ == .__main__.:
```

## Summary

Effective Python testing centers on pytest's powerful features over unittest's verbose class-based approach. Test functions start with test\_, use plain assert statements, and pytest's introspection provides helpful failure messages without boilerplate.

Fixtures provide test setup and teardown with automatic dependency injection. Define fixtures with @pytest.fixture, use yield for cleanup, and control scope (function, class, module, session) based on sharing needs. conftest.py makes fixtures available across test files.

Parametrized tests eliminate duplication when testing multiple inputs. @pytest.mark.parametrize supplies test data as tuples, pytest generates separate tests for each input set. Use ids parameter for readable test names.

Mocking with unittest.mock isolates code under test from dependencies. Mock() creates mock objects, patch() replaces functions or classes, side_effect simulates multiple calls or exceptions. Verify interactions with assert_called_once(), assert_called_with(), and call_count.

Exception testing uses pytest.raises context manager. Assert exceptions are raised, check exception messages with match parameter, inspect exception attributes through exc_info. Missing exception causes test failure.

Test organization follows project structure with tests/ directory mirroring source layout. Mark tests with @pytest.mark for selective execution - slow, integration, skip conditions. Run subsets with -m flag.

Coverage measurement with pytest-cov shows which code paths tests execute. Generate reports identifying untested lines, fail builds below coverage thresholds. Configure omit patterns to exclude test code and generated files.

Pytest's simple syntax, powerful fixtures, parametrization, and assertion introspection make tests easier to write and maintain than unittest. Combined with coverage measurement and marking, pytest enables comprehensive, organized test suites.

## Related Content

- [Python Best Practices](/en/learn/software-engineering/programming-language/python/explanation/best-practices)
- [How to Handle Errors Effectively](/en/learn/software-engineering/programming-language/python/how-to/handle-errors-effectively)
- [How to Write Pythonic Code](/en/learn/software-engineering/programming-language/python/how-to/write-pythonic-code)
