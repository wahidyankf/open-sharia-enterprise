"""Unit BDD step definitions for attachment feature."""

import io
import json

import pytest
from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, then, when

from tests.unit.conftest import GHERKIN_ROOT

pytestmark = pytest.mark.unit

scenarios(str(GHERKIN_ROOT / "expenses" / "attachments.feature"))

_PASSWORD = "Str0ng#Pass1"
_PASSWORD2 = "Str0ng#Pass2"


@given(
    '"alice" has logged in and stored the access token',
    target_fixture="alice_tokens",
)
def alice_login_attach(client: TestClient, registered_user: dict) -> dict:
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
def alice_create_attachment_entry(client: TestClient, alice_tokens: dict, body: str) -> dict:
    data = json.loads(body)
    resp = client.post(
        "/api/v1/expenses",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )
    assert resp.status_code == 201, f"Create entry failed: {resp.text}"
    return resp.json()


@given(
    parsers.parse("bob has created an entry with body {body}"),
    target_fixture="bob_expense",
)
def bob_create_entry(client: TestClient, registered_user: dict, body: str) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "bob", "password": _PASSWORD2},
    )
    assert resp.status_code == 200, f"Bob login failed: {resp.text}"
    bob_tokens = resp.json()
    data = json.loads(body)
    resp2 = client.post(
        "/api/v1/expenses",
        json=data,
        headers={"Authorization": f"Bearer {bob_tokens['accessToken']}"},
    )
    assert resp2.status_code == 201
    return resp2.json()


@given(
    parsers.parse(
        'alice has uploaded file "{filename}" with content type "{content_type}" to the entry'
    ),
    target_fixture="uploaded_attachment",
)
def alice_upload_attachment_given(
    client: TestClient, alice_tokens: dict, created_expense: dict, filename: str, content_type: str
) -> dict:
    file_content = b"dummy file content"
    resp = client.post(
        f"/api/v1/expenses/{created_expense['id']}/attachments",
        files={"file": (filename, io.BytesIO(file_content), content_type)},
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )
    assert resp.status_code == 201, f"Upload failed: {resp.text}"
    return resp.json()


# --- When steps ---


@when(
    parsers.parse(
        'alice uploads file "{filename}" with content type "{content_type}" to POST /api/v1/expenses/{{expenseId}}/attachments'  # noqa: E501
    ),
    target_fixture="response",
)
def alice_upload_file(
    client: TestClient, alice_tokens: dict, created_expense: dict, filename: str, content_type: str
):  # type: ignore[no-untyped-def]
    file_content = b"dummy file content"
    return client.post(
        f"/api/v1/expenses/{created_expense['id']}/attachments",
        files={"file": (filename, io.BytesIO(file_content), content_type)},
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when(
    parsers.parse(
        'alice uploads file "{filename}" with content type "{content_type}" to POST /api/v1/expenses/{{bobExpenseId}}/attachments'  # noqa: E501
    ),
    target_fixture="response",
)
def alice_upload_to_bob_expense(
    client: TestClient,
    alice_tokens: dict,
    bob_expense: dict,
    filename: str,
    content_type: str,
):  # type: ignore[no-untyped-def]
    file_content = b"dummy file content"
    return client.post(
        f"/api/v1/expenses/{bob_expense['id']}/attachments",
        files={"file": (filename, io.BytesIO(file_content), content_type)},
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when(
    "alice uploads an oversized file to POST /api/v1/expenses/{expenseId}/attachments",
    target_fixture="response",
)
def alice_upload_oversized(client: TestClient, alice_tokens: dict, created_expense: dict):  # type: ignore[no-untyped-def]
    big_content = b"x" * (11 * 1024 * 1024)
    return client.post(
        f"/api/v1/expenses/{created_expense['id']}/attachments",
        files={"file": ("large.pdf", io.BytesIO(big_content), "application/pdf")},
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when("alice sends GET /api/v1/expenses/{expenseId}/attachments", target_fixture="response")
def alice_list_attachments(client: TestClient, alice_tokens: dict, created_expense: dict):  # type: ignore[no-untyped-def]
    return client.get(
        f"/api/v1/expenses/{created_expense['id']}/attachments",
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when("alice sends GET /api/v1/expenses/{bobExpenseId}/attachments", target_fixture="response")
def alice_list_bob_attachments(client: TestClient, alice_tokens: dict, bob_expense: dict):  # type: ignore[no-untyped-def]
    return client.get(
        f"/api/v1/expenses/{bob_expense['id']}/attachments",
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when(
    "alice sends DELETE /api/v1/expenses/{expenseId}/attachments/{attachmentId}",
    target_fixture="response",
)
def alice_delete_attachment(
    client: TestClient, alice_tokens: dict, created_expense: dict, uploaded_attachment: dict
):  # type: ignore[no-untyped-def]
    return client.delete(
        f"/api/v1/expenses/{created_expense['id']}/attachments/{uploaded_attachment['id']}",
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when(
    "alice sends DELETE /api/v1/expenses/{bobExpenseId}/attachments/{attachmentId}",
    target_fixture="response",
)
def alice_delete_bob_attachment(
    client: TestClient, alice_tokens: dict, bob_expense: dict, uploaded_attachment: dict
):  # type: ignore[no-untyped-def]
    return client.delete(
        f"/api/v1/expenses/{bob_expense['id']}/attachments/{uploaded_attachment['id']}",
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


@when(
    "alice sends DELETE /api/v1/expenses/{expenseId}/attachments/{randomAttachmentId}",
    target_fixture="response",
)
def alice_delete_nonexistent_attachment(
    client: TestClient, alice_tokens: dict, created_expense: dict
):  # type: ignore[no-untyped-def]
    import uuid

    random_id = str(uuid.uuid4())
    return client.delete(
        f"/api/v1/expenses/{created_expense['id']}/attachments/{random_id}",
        headers={"Authorization": f"Bearer {alice_tokens['accessToken']}"},
    )


# --- Then steps ---


@then(parsers.parse('the response body should contain 2 items in the "attachments" array'))
def check_two_attachments(response) -> None:
    body = response.json()
    attachments = body.get("attachments", [])
    assert len(attachments) == 2, f"Expected 2 attachments, got {len(attachments)}: {body}"


@then(
    parsers.parse(
        'the response body should contain an attachment with "filename" equal to "{filename}"'
    )
)
def check_attachment_filename(response, filename: str) -> None:
    body = response.json()
    attachments = body.get("attachments", [])
    assert any(a.get("filename") == filename for a in attachments), (
        f"Attachment '{filename}' not found in: {attachments}"
    )


@then(parsers.parse('the response body should contain "content_type" equal to "{content_type}"'))
def check_attachment_content_type(response, content_type: str) -> None:
    body = response.json()
    assert "content_type" in body, f"'content_type' not in response: {body}"
    assert body["content_type"] == content_type, (
        f"Expected content_type={content_type!r}, got {body['content_type']!r}"
    )


@then(parsers.parse('the response body should contain "filename" equal to "{filename}"'))
def check_filename(response, filename: str) -> None:
    body = response.json()
    assert "filename" in body, f"'filename' not in response: {body}"
    assert body["filename"] == filename, f"Expected filename={filename!r}, got {body['filename']!r}"


@then('the response body should contain a validation error for "file"')
def check_file_validation_error(response) -> None:
    body = response.json()
    body_str = json.dumps(body).lower()
    assert any(kw in body_str for kw in ["file", "media", "type", "unsupported"]), (
        f"Expected file validation error, got: {body}"
    )
