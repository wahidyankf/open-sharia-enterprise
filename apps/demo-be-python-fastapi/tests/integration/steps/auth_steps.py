"""BDD step definitions for registration and login features."""

from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, then

from tests.integration.conftest import GHERKIN_ROOT

scenarios(str(GHERKIN_ROOT / "user-lifecycle" / "registration.feature"))
scenarios(str(GHERKIN_ROOT / "authentication" / "password-login.feature"))

_STRONG_PASSWORD = "Str0ng#Pass1"


def _login_user(client: TestClient, username: str, password: str = _STRONG_PASSWORD) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": username, "password": password},
    )
    assert resp.status_code == 200, f"Login failed: {resp.text}"
    return resp.json()


# --- Background steps only needed by these scenarios ---


@given(
    parsers.parse('a user "{username}" is registered and deactivated'),
    target_fixture="deactivated_user",
)
def register_and_deactivate_user(client: TestClient, username: str, registered_user) -> dict:
    tokens = _login_user(client, username)
    resp = client.post(
        "/api/v1/users/me/deactivate",
        headers={"Authorization": f"Bearer {tokens['access_token']}"},
    )
    assert resp.status_code == 200
    return registered_user


# --- Then steps ---


@then('the response body should not contain a "password" field')
def check_no_password_field(response) -> None:  # type: ignore[no-untyped-def]
    body = response.json()
    assert "password" not in body, f"password field found in response: {body}"
    assert "password_hash" not in body


@then("the response body should contain an error message about duplicate username")
def check_duplicate_username(response) -> None:  # type: ignore[no-untyped-def]
    body = response.json()
    assert "message" in body
    msg = body["message"].lower()
    assert any(kw in msg for kw in ["duplicate", "taken", "exists", "already"]), (
        f"Expected duplicate username message, got: {body['message']}"
    )
