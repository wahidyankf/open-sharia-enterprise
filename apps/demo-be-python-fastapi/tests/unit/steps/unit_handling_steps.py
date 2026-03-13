"""Unit BDD step definitions for unit handling feature."""

import json

import pytest
from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, then, when

from tests.unit.conftest import GHERKIN_ROOT

pytestmark = pytest.mark.unit

scenarios(str(GHERKIN_ROOT / "expenses" / "unit-handling.feature"))

_PASSWORD = "Str0ng#Pass1"


@given(
    '"alice" has logged in and stored the access token',
    target_fixture="alice_tokens",
)
def alice_login_units(client: TestClient, registered_user: dict) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 200
    return resp.json()


@given(
    parsers.parse("alice has created an expense with body {body}"),
    target_fixture="created_expense",
)
def alice_create_unit_expense(client: TestClient, alice_tokens: dict, body: str) -> dict:
    data = json.loads(body)
    resp = client.post(
        "/api/v1/expenses",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )
    assert resp.status_code == 201, f"Create expense failed: {resp.text}"
    return resp.json()


# --- When steps ---


@when("alice sends GET /api/v1/expenses/{expenseId}", target_fixture="response")
def alice_get_unit_expense(client: TestClient, alice_tokens: dict, created_expense: dict):  # type: ignore[no-untyped-def]
    return client.get(
        f"/api/v1/expenses/{created_expense['id']}",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


@when(
    parsers.parse("alice sends POST /api/v1/expenses with body {body}"),
    target_fixture="response",
)
def alice_post_unit_expense(client: TestClient, alice_tokens: dict, body: str):  # type: ignore[no-untyped-def]
    data = json.loads(body)
    return client.post(
        "/api/v1/expenses",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


# --- Then steps ---


@then(parsers.parse('the response body should contain "quantity" equal to {value}'))
def check_quantity(response, value: str) -> None:
    body = response.json()
    assert "quantity" in body, f"'quantity' not in response: {body}"
    actual = body["quantity"]
    expected = float(value)
    assert float(actual) == expected, f"Expected quantity={expected}, got {actual}"


@then(parsers.parse('the response body should contain "unit" equal to "{value}"'))
def check_unit(response, value: str) -> None:
    body = response.json()
    assert "unit" in body, f"'unit' not in response: {body}"
    assert body["unit"] == value, f"Expected unit={value!r}, got {body['unit']!r}"
