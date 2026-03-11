"""BDD step definitions for health check feature."""

from fastapi.testclient import TestClient
from pytest_bdd import scenarios, then, when

from tests.integration.conftest import GHERKIN_ROOT

scenarios(str(GHERKIN_ROOT / "health" / "health-check.feature"))


@when("an operations engineer sends GET /health", target_fixture="response")
def get_health(client: TestClient):  # type: ignore[no-untyped-def]
    return client.get("/health")


@when("an unauthenticated engineer sends GET /health", target_fixture="response")
def get_health_unauthenticated(client: TestClient):  # type: ignore[no-untyped-def]
    return client.get("/health")


@then('the health status should be "UP"')
def check_health_status(response) -> None:  # type: ignore[no-untyped-def]
    body = response.json()
    assert body.get("status") == "UP", f"Expected status=UP, got: {body}"


@then("the response should not include detailed component health information")
def check_no_component_details(response) -> None:  # type: ignore[no-untyped-def]
    body = response.json()
    assert "components" not in body
    assert "details" not in body
    assert set(body.keys()) == {"status"}, f"Unexpected keys: {set(body.keys())}"
