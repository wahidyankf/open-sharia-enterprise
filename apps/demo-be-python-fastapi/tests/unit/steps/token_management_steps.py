"""Unit BDD step definitions for token management feature."""

import base64
import json

import pytest
from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, then, when

from tests.unit.conftest import GHERKIN_ROOT
from tests.unit.steps._helpers import _ADMIN_PASSWORD, register_and_promote_admin

pytestmark = pytest.mark.unit

scenarios(str(GHERKIN_ROOT / "token-management" / "tokens.feature"))

_PASSWORD = "Str0ng#Pass1"


@given(
    '"alice" has logged in and stored the access token',
    target_fixture="alice_tokens",
)
def alice_login_for_tokens(client: TestClient, registered_user: dict) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 200
    return resp.json()


@given("alice has logged out and her access token is blacklisted")
def alice_logout_blacklisted(client: TestClient, alice_tokens: dict) -> None:
    client.post(
        "/api/v1/auth/logout",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


@given(
    parsers.parse('an admin user "{username}" is registered and logged in'),
    target_fixture="admin_tokens",
)
def admin_login_for_tokens(client: TestClient, username: str) -> dict:
    user_data = register_and_promote_admin(client, username, _ADMIN_PASSWORD)
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": username, "password": _ADMIN_PASSWORD},
    )
    assert resp.status_code == 200
    return {**resp.json(), "id": user_data["id"]}


@given("the admin has disabled alice's account via POST /api/v1/admin/users/{alice_id}/disable")
def admin_disable_alice(client: TestClient, registered_user: dict, admin_tokens: dict) -> None:
    resp = client.post(
        f"/api/v1/admin/users/{registered_user['id']}/disable",
        json={"reason": "Test"},
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )
    assert resp.status_code == 200, f"Disable failed: {resp.text}"


# --- When steps ---


@when("alice decodes her access token payload", target_fixture="decoded_payload")
def alice_decode_token(alice_tokens: dict) -> dict:
    token = alice_tokens["access_token"]
    parts = token.split(".")
    assert len(parts) == 3, "Invalid JWT structure"
    payload_b64 = parts[1]
    padding = 4 - len(payload_b64) % 4
    if padding != 4:
        payload_b64 += "=" * padding
    payload = json.loads(base64.urlsafe_b64decode(payload_b64).decode("utf-8"))
    return payload


@when("the client sends GET /.well-known/jwks.json", target_fixture="response")
def get_jwks(client: TestClient):  # type: ignore[no-untyped-def]
    return client.get("/.well-known/jwks.json")


@when("alice sends POST /api/v1/auth/logout with her access token", target_fixture="response")
def alice_logout_tokens(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.post(
        "/api/v1/auth/logout",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


@when(
    "the client sends GET /api/v1/users/me with alice's access token",
    target_fixture="response",
)
def get_me_with_alice_token(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.get(
        "/api/v1/users/me",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


# --- Then steps ---


@then(parsers.parse('the token should contain a non-null "{claim}" claim'))
def check_token_claim(decoded_payload: dict, claim: str) -> None:
    assert claim in decoded_payload, f"Claim '{claim}' not in token: {decoded_payload}"
    assert decoded_payload[claim] is not None, f"Claim '{claim}' is null"


@then('the response body should contain at least one key in the "keys" array')
def check_jwks_keys(response) -> None:  # type: ignore[no-untyped-def]
    body = response.json()
    assert "keys" in body, f"No 'keys' field in JWKS response: {body}"
    assert len(body["keys"]) > 0, "JWKS keys array is empty"


@then("alice's access token should be recorded as revoked")
def check_alice_token_revoked(client: TestClient, alice_tokens: dict) -> None:
    resp = client.get(
        "/api/v1/users/me",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )
    assert resp.status_code == 401, f"Expected 401 for revoked token, got {resp.status_code}"
