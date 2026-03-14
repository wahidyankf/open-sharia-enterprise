"""Unit BDD step definitions for financial reporting feature."""

import json

import pytest
from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, then, when

from tests.unit.conftest import GHERKIN_ROOT

pytestmark = pytest.mark.unit

scenarios(str(GHERKIN_ROOT / "expenses" / "reporting.feature"))

_PASSWORD = "Str0ng#Pass1"


@given(
    '"alice" has logged in and stored the access token',
    target_fixture="alice_tokens",
)
def alice_login_reporting(client: TestClient, registered_user: dict) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 200
    return resp.json()


@given(
    parsers.parse("alice has created an entry with body {body}"),
    target_fixture="reporting_entry",
)
def alice_create_reporting_entry(client: TestClient, alice_tokens: dict, body: str) -> dict:
    data = json.loads(body)
    resp = client.post(
        "/api/v1/expenses",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )
    assert resp.status_code == 201, f"Create entry failed: {resp.text}"
    return resp.json()


# --- When steps ---


@when(
    parsers.parse("alice sends GET /api/v1/reports/pl?from={from_}&to={to}&currency={currency}"),
    target_fixture="response",
)
def alice_get_pl_report(client: TestClient, alice_tokens: dict, from_: str, to: str, currency: str):  # type: ignore[no-untyped-def]
    return client.get(
        "/api/v1/reports/pl",
        params={"from": from_, "to": to, "currency": currency},
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


# --- Then steps ---


@then(parsers.parse('the response body should contain "totalIncome" equal to "{value}"'))
def check_income_total(response, value: str) -> None:
    body = response.json()
    assert "totalIncome" in body, f"totalIncome not in: {body}"
    assert str(body["totalIncome"]) == value, (
        f"Expected totalIncome={value}, got {body['totalIncome']}"
    )


@then(parsers.parse('the response body should contain "totalExpense" equal to "{value}"'))
def check_expense_total(response, value: str) -> None:
    body = response.json()
    assert "totalExpense" in body, f"totalExpense not in: {body}"
    assert str(body["totalExpense"]) == value, (
        f"Expected totalExpense={value}, got {body['totalExpense']}"
    )


@then(parsers.parse('the response body should contain "net" equal to "{value}"'))
def check_net(response, value: str) -> None:
    body = response.json()
    assert "net" in body, f"net not in: {body}"
    assert str(body["net"]) == value, f"Expected net={value}, got {body['net']}"


@then(parsers.parse('the income breakdown should contain "{category}" with amount "{amount}"'))
def check_income_breakdown(response, category: str, amount: str) -> None:
    body = response.json()
    breakdown = body.get("income_breakdown", {})
    assert category in breakdown, f"Category '{category}' not in income_breakdown: {breakdown}"
    assert str(breakdown[category]) == amount, (
        f"Expected income_breakdown[{category}]={amount}, got {breakdown[category]}"
    )


@then(parsers.parse('the expense breakdown should contain "{category}" with amount "{amount}"'))
def check_expense_breakdown(response, category: str, amount: str) -> None:
    body = response.json()
    breakdown = body.get("expense_breakdown", {})
    assert category in breakdown, f"Category '{category}' not in expense_breakdown: {breakdown}"
    assert str(breakdown[category]) == amount, (
        f"Expected expense_breakdown[{category}]={amount}, got {breakdown[category]}"
    )
