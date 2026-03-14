"""Unit BDD step definitions for expense management feature."""

import json

import pytest
from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, when

from tests.unit.conftest import GHERKIN_ROOT

pytestmark = pytest.mark.unit

scenarios(str(GHERKIN_ROOT / "expenses" / "expense-management.feature"))

_PASSWORD = "Str0ng#Pass1"


@given(
    '"alice" has logged in and stored the access token',
    target_fixture="alice_tokens",
)
def alice_login_expense(client: TestClient, registered_user: dict) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 200
    return resp.json()


@given(
    parsers.parse("alice has created an entry with body {body}"),
    target_fixture="created_expense",
)
def alice_create_entry(client: TestClient, alice_tokens: dict, body: str) -> dict:
    data = json.loads(body)
    resp = client.post(
        "/api/v1/expenses",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )
    assert resp.status_code == 201, f"Create expense failed: {resp.text}"
    return resp.json()


@given("alice has created 3 entries", target_fixture="created_expenses")
def alice_create_3_entries(client: TestClient, alice_tokens: dict) -> list:
    expenses = []
    for i in range(3):
        resp = client.post(
            "/api/v1/expenses",
            json={
                "amount": f"{10 + i}.00",
                "currency": "USD",
                "category": "food",
                "description": f"Entry {i}",
                "date": f"2025-01-{10 + i:02d}",
                "type": "expense",
            },
            headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
        )
        assert resp.status_code == 201
        expenses.append(resp.json())
    return expenses


# --- When steps ---


@when(
    parsers.parse("alice sends POST /api/v1/expenses with body {body}"),
    target_fixture="response",
)
def alice_post_expense(client: TestClient, alice_tokens: dict, body: str):  # type: ignore[no-untyped-def]
    data = json.loads(body)
    return client.post(
        "/api/v1/expenses",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when(
    'the client sends POST /api/v1/expenses with body { "amount": "10.00", "currency": "USD", "category": "food", "description": "Coffee", "date": "2025-01-01", "type": "expense" }',  # noqa: E501
    target_fixture="response",
)
def unauth_post_expense(client: TestClient):  # type: ignore[no-untyped-def]
    return client.post(
        "/api/v1/expenses",
        json={
            "amount": "10.00",
            "currency": "USD",
            "category": "food",
            "description": "Coffee",
            "date": "2025-01-01",
            "type": "expense",
        },
    )


@when("alice sends GET /api/v1/expenses/{expenseId}", target_fixture="response")
def alice_get_expense(client: TestClient, alice_tokens: dict, created_expense: dict):  # type: ignore[no-untyped-def]
    return client.get(
        f"/api/v1/expenses/{created_expense['id']}",
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when("alice sends GET /api/v1/expenses", target_fixture="response")
def alice_list_expenses(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.get(
        "/api/v1/expenses",
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when(
    parsers.parse("alice sends PUT /api/v1/expenses/{{expenseId}} with body {body}"),
    target_fixture="response",
)
def alice_update_expense(client: TestClient, alice_tokens: dict, created_expense: dict, body: str):  # type: ignore[no-untyped-def]
    data = json.loads(body)
    return client.put(
        f"/api/v1/expenses/{created_expense['id']}",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when("alice sends DELETE /api/v1/expenses/{expenseId}", target_fixture="response")
def alice_delete_expense(client: TestClient, alice_tokens: dict, created_expense: dict):  # type: ignore[no-untyped-def]
    return client.delete(
        f"/api/v1/expenses/{created_expense['id']}",
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )
