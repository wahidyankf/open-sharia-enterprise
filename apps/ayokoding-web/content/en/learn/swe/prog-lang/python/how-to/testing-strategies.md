---
title: "Testing Strategies"
date: 2025-12-19T00:00:00+07:00
draft: false
description: "Comprehensive guide to testing Python code with pytest, mocking, fixtures, integration tests, and coverage strategies"
weight: 621
tags: ["python", "testing", "pytest", "mocking", "tdd", "how-to"]
---

**Need to write better tests for your Python code?** This guide covers comprehensive testing strategies from unit tests to integration tests, mocking, fixtures, parametrization, and achieving meaningful code coverage.

For testing basics, see the [Beginner Tutorial](/en/learn/swe/prog-lang/python/tutorials/beginner). For related patterns, check [Write Effective Tests](/en/learn/swe/prog-lang/python/how-to/write-effective-tests).

## 🎯 Testing Philosophy

### The Testing Pyramid

```
        /\
       /E2E\      ← Few, slow, expensive (10%)
      /------\
     /Integration\ ← Some, moderate speed (20%)
    /------------\
   /  Unit Tests  \ ← Many, fast, cheap (70%)
  /----------------\
```

**Unit Tests**: Test individual functions/classes in isolation
**Integration Tests**: Test components working together
**End-to-End (E2E)**: Test complete user workflows

## 🧪 Unit Testing Strategies

### Strategy 1: Test One Thing at a Time

```python
# calculator.py
def add(a, b):
    return a + b

def divide(a, b):
    if b == 0:
        raise ValueError("Cannot divide by zero")
    return a / b

# test_calculator.py
import pytest
from calculator import add, divide

# ✅ Good: Each test tests one specific behavior
def test_add_positive_numbers():
    assert add(2, 3) == 5

def test_add_negative_numbers():
    assert add(-1, -1) == -2

def test_add_zero():
    assert add(5, 0) == 5

def test_divide_normal():
    assert divide(6, 2) == 3

def test_divide_by_zero():
    with pytest.raises(ValueError, match="Cannot divide by zero"):
        divide(10, 0)

# ❌ Bad: One test testing multiple things
def test_calculator_all():
    assert add(2, 3) == 5
    assert divide(6, 2) == 3
    # If first fails, rest don't run
```

### Strategy 2: Use Parametrize for Multiple Cases

```python
import pytest

@pytest.mark.parametrize("a,b,expected", [
    (2, 3, 5),
    (-1, 1, 0),
    (0, 0, 0),
    (100, 200, 300),
    (-5, -10, -15),
])
def test_add_multiple_cases(a, b, expected):
    assert add(a, b) == expected

# With test IDs for clarity
@pytest.mark.parametrize("a,b,expected", [
    pytest.param(2, 3, 5, id="positive"),
    pytest.param(-1, 1, 0, id="negative"),
    pytest.param(0, 0, 0, id="zeros"),
])
def test_add_with_ids(a, b, expected):
    assert add(a, b) == expected
```

### Strategy 3: Fixtures for Setup/Teardown

```python
import pytest
from database import Database

@pytest.fixture
def db():
    """Create database connection."""
    database = Database("test.db")
    database.connect()
    yield database  # Test runs here
    database.disconnect()
    database.cleanup()

def test_insert_user(db):
    user_id = db.insert_user("Alice", "alice@example.com")
    assert user_id > 0

def test_fetch_user(db):
    user_id = db.insert_user("Bob", "bob@example.com")
    user = db.fetch_user(user_id)
    assert user["name"] == "Bob"

# Fixture scope control
@pytest.fixture(scope="module")  # Once per module
def expensive_resource():
    resource = ExpensiveResource()
    resource.initialize()
    yield resource
    resource.cleanup()

@pytest.fixture(scope="session")  # Once per test session
def global_config():
    return load_config()
```

## 🎭 Mocking Strategies

### Strategy 4: Mock External Dependencies

```python
from unittest.mock import Mock, patch, MagicMock
import requests

# Code to test
def fetch_user_data(user_id):
    response = requests.get(f"https://api.example.com/users/{user_id}")
    response.raise_for_status()
    return response.json()

# Test with mocking
@patch('requests.get')
def test_fetch_user_data(mock_get):
    # Setup mock
    mock_response = Mock()
    mock_response.json.return_value = {"id": 1, "name": "Alice"}
    mock_response.status_code = 200
    mock_get.return_value = mock_response

    # Test
    result = fetch_user_data(1)

    # Assertions
    assert result["name"] == "Alice"
    mock_get.assert_called_once_with("https://api.example.com/users/1")

# Mock with side effects
@patch('requests.get')
def test_fetch_user_not_found(mock_get):
    mock_get.side_effect = requests.HTTPError("404 Not Found")

    with pytest.raises(requests.HTTPError):
        fetch_user_data(999)
```

### Strategy 5: Mock with pytest-mock

```python
# Install: pip install pytest-mock

def test_with_pytest_mock(mocker):
    # mocker is a pytest-mock fixture
    mock_get = mocker.patch('requests.get')
    mock_get.return_value.json.return_value = {"id": 1}

    result = fetch_user_data(1)
    assert result["id"] == 1
```

### Strategy 6: Dependency Injection for Testability

```python
# ❌ Hard to test (tight coupling)
class UserService:
    def get_user(self, user_id):
        # Directly uses requests - hard to test
        response = requests.get(f"https://api.example.com/users/{user_id}")
        return response.json()

# ✅ Easy to test (dependency injection)
class UserService:
    def __init__(self, http_client):
        self.client = http_client

    def get_user(self, user_id):
        response = self.client.get(f"https://api.example.com/users/{user_id}")
        return response.json()

# Test with mock client
def test_user_service():
    mock_client = Mock()
    mock_client.get.return_value.json.return_value = {"id": 1, "name": "Alice"}

    service = UserService(mock_client)
    user = service.get_user(1)

    assert user["name"] == "Alice"
```

## 🔗 Integration Testing Strategies

### Strategy 7: Test Database Integration

```python
import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from models import Base, User

@pytest.fixture(scope="module")
def test_db():
    """Create test database."""
    engine = create_engine("sqlite:///:memory:")
    Base.metadata.create_all(engine)
    Session = sessionmaker(bind=engine)
    session = Session()

    yield session

    session.close()
    Base.metadata.drop_all(engine)

def test_create_user(test_db):
    user = User(name="Alice", email="alice@example.com")
    test_db.add(user)
    test_db.commit()

    fetched = test_db.query(User).filter_by(name="Alice").first()
    assert fetched.email == "alice@example.com"

def test_user_relationships(test_db):
    user = User(name="Bob")
    post = Post(title="Hello", author=user)
    test_db.add_all([user, post])
    test_db.commit()

    assert post.author.name == "Bob"
    assert user.posts[0].title == "Hello"
```

### Strategy 8: Test API Endpoints

```python
import pytest
from fastapi.testclient import TestClient
from main import app

@pytest.fixture
def client():
    return TestClient(app)

def test_create_user(client):
    response = client.post(
        "/users/",
        json={"name": "Alice", "email": "alice@example.com"}
    )
    assert response.status_code == 201
    data = response.json()
    assert data["name"] == "Alice"
    assert "id" in data

def test_get_user(client):
    # Create user first
    create_response = client.post(
        "/users/",
        json={"name": "Bob", "email": "bob@example.com"}
    )
    user_id = create_response.json()["id"]

    # Fetch user
    response = client.get(f"/users/{user_id}")
    assert response.status_code == 200
    assert response.json()["name"] == "Bob"

def test_user_not_found(client):
    response = client.get("/users/9999")
    assert response.status_code == 404
```

## 📊 Coverage Strategies

### Strategy 9: Measure and Improve Coverage

```bash
# Install coverage
pip install pytest-cov

# Run tests with coverage
pytest --cov=mymodule --cov-report=html

# View HTML report
open htmlcov/index.html

# Terminal report
pytest --cov=mymodule --cov-report=term-missing

# Fail if coverage below threshold
pytest --cov=mymodule --cov-fail-under=80
```

**Coverage Configuration** (.coveragerc):

```ini
[run]
source = mymodule
omit =
    */tests/*
    */venv/*
    */__init__.py

[report]
exclude_lines =
    pragma: no cover
    def __repr__
    raise AssertionError
    raise NotImplementedError
    if __name__ == .__main__.:
```

### Strategy 10: Focus on Critical Paths

```python
# ✅ Prioritize testing critical business logic
def calculate_price(base_price, discount, tax_rate):
    """Critical: Affects revenue."""
    discounted = base_price * (1 - discount)
    return discounted * (1 + tax_rate)

def test_price_calculation():
    # Test various scenarios
    assert calculate_price(100, 0.1, 0.05) == 94.5
    assert calculate_price(100, 0, 0) == 100
    assert calculate_price(100, 0.5, 0.1) == 55

# ⚠️ Less critical: Simple getters/setters
class Product:
    def __init__(self, name):
        self._name = name

    @property
    def name(self):
        return self._name  # Simple getter, low priority
```

## 🎯 Test Organization

### Strategy 11: Mirror Source Structure

```
myproject/
├── mymodule/
│   ├── __init__.py
│   ├── users.py
│   ├── products.py
│   └── orders.py
└── tests/
    ├── __init__.py
    ├── test_users.py
    ├── test_products.py
    └── test_orders.py
```

### Strategy 12: Use conftest.py for Shared Fixtures

```python
# tests/conftest.py
import pytest

@pytest.fixture
def db_session():
    """Shared database session fixture."""
    session = create_test_session()
    yield session
    session.close()

@pytest.fixture
def sample_user():
    """Shared user fixture."""
    return User(name="Test User", email="test@example.com")

# Available in all test files automatically
```

## 🔍 Advanced Patterns

### Strategy 13: Property-Based Testing

```python
# Install: pip install hypothesis
from hypothesis import given, strategies as st

@given(st.integers(), st.integers())
def test_add_commutative(a, b):
    """Addition is commutative: a + b = b + a."""
    assert add(a, b) == add(b, a)

@given(st.lists(st.integers()))
def test_reverse_twice(lst):
    """Reversing twice returns original list."""
    assert list(reversed(list(reversed(lst)))) == lst
```

### Strategy 14: Snapshot Testing

```python
# Install: pip install pytest-snapshot
def test_api_response_snapshot(snapshot):
    """Ensure API response structure doesn't change."""
    response = fetch_api_data()
    snapshot.assert_match(response)
    # First run: creates snapshot
    # Subsequent runs: compares against snapshot
```

### Strategy 15: Async Testing

```python
import pytest
import asyncio

@pytest.mark.asyncio
async def test_async_function():
    result = await async_fetch_data()
    assert result is not None

@pytest.fixture
async def async_client():
    client = AsyncClient()
    await client.connect()
    yield client
    await client.disconnect()

@pytest.mark.asyncio
async def test_with_async_fixture(async_client):
    data = await async_client.fetch_data()
    assert data["status"] == "ok"
```

## 🚫 Common Mistakes

### Mistake 1: Testing Implementation, Not Behavior

```python
# ❌ Bad: Tests internal implementation
def test_sort_uses_quicksort():
    data = [3, 1, 2]
    sorter = Sorter(algorithm="quicksort")
    sorter.sort(data)
    assert sorter.algorithm == "quicksort"  # Implementation detail

# ✅ Good: Tests behavior
def test_sort_orders_correctly():
    data = [3, 1, 2]
    result = sort(data)
    assert result == [1, 2, 3]  # Outcome matters
```

### Mistake 2: Fragile Tests with Unnecessary Mocking

```python
# ❌ Bad: Over-mocking
@patch('datetime.datetime')
def test_get_timestamp(mock_datetime):
    mock_datetime.now.return_value = datetime(2025, 1, 1)
    # Tests become fragile

# ✅ Good: Test logic, not datetime
def test_format_timestamp():
    dt = datetime(2025, 1, 1, 12, 30)
    result = format_timestamp(dt)
    assert result == "2025-01-01 12:30:00"
```

### Mistake 3: Interdependent Tests

```python
# ❌ Bad: Tests depend on each other
def test_create_user():
    global user_id
    user_id = create_user("Alice")

def test_update_user():
    update_user(user_id, name="Bob")  # Depends on previous test!

# ✅ Good: Independent tests
@pytest.fixture
def user_id():
    return create_user("Alice")

def test_update_user(user_id):
    update_user(user_id, name="Bob")
    # Each test is self-contained
```

## 🎯 Best Practices

1. **AAA Pattern**: Arrange, Act, Assert in every test
2. **Fast Tests**: Unit tests should run in milliseconds
3. **Isolated Tests**: Each test can run independently
4. **Clear Names**: Test names describe what they test
5. **One Assertion Per Test**: Or very related assertions
6. **Mock External Dependencies**: Don't hit real APIs/databases in unit tests
7. **Meaningful Coverage**: 80% meaningful coverage > 100% superficial
8. **Test Edge Cases**: Empty inputs, null values, boundaries
9. **Fail Fast**: Stop on first failure with `pytest -x`
10. **CI Integration**: Run tests automatically on every commit

## 🛠️ Testing Tools

**Testing Frameworks**:

- pytest (recommended)
- unittest (built-in)

**Mocking**:

- unittest.mock (built-in)
- pytest-mock

**Coverage**:

- pytest-cov
- coverage.py

**Special Testing**:

- hypothesis (property-based)
- pytest-asyncio (async)
- pytest-benchmark (performance)
- pytest-snapshot (snapshot)

**Web Testing**:

- pytest-django
- pytest-flask
- TestClient (FastAPI)

## 📚 See Also

- [Write Effective Tests](/en/learn/swe/prog-lang/python/how-to/write-effective-tests) - Basic testing patterns
- [Beginner Tutorial](/en/learn/swe/prog-lang/python/tutorials/beginner) - Testing fundamentals
- [pytest Documentation](https://docs.pytest.org/) - Official pytest docs
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development) - TDD methodology
