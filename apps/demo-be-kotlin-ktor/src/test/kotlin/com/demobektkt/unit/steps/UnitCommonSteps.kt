package com.demobektkt.unit.steps

import io.cucumber.java.en.Given
import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue

class UnitCommonSteps {

  @Given("the API is running")
  fun theApiIsRunning() {
    // No-op: Ktor testApplication starts in-process for each request
  }

  @When("an operations engineer sends GET \\/health")
  fun operationsEngineerSendsGetHealth() {
    val (status, body) = UnitHttpHelper.get("/health")
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @When("an unauthenticated engineer sends GET \\/health")
  fun unauthenticatedEngineerSendsGetHealth() {
    val (status, body) = UnitHttpHelper.get("/health")
    UnitTestWorld.lastResponseStatus = status
    UnitTestWorld.lastResponseBody = body
  }

  @Then("the response status code should be {int}")
  fun theResponseStatusCodeShouldBe(code: Int) {
    assertEquals(
      code,
      UnitTestWorld.lastResponseStatus,
      "Expected status $code but got ${UnitTestWorld.lastResponseStatus}. Body: ${UnitTestWorld.lastResponseBody}",
    )
  }

  @Then("the health status should be {string}")
  fun theHealthStatusShouldBe(status: String) {
    val actual = UnitJsonHelper.getString(UnitTestWorld.lastResponseBody, "status")
    assertEquals(
      status,
      actual,
      "Expected health status '$status' in: ${UnitTestWorld.lastResponseBody}",
    )
  }

  @Then("the response should not include detailed component health information")
  fun theResponseShouldNotIncludeDetailedComponentHealthInformation() {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      !body.contains("components") && !body.contains("db"),
      "Should not include component details: $body",
    )
  }

  @Then("the response body should contain a non-null {string} field")
  fun theResponseBodyShouldContainNonNullField(field: String) {
    UnitJsonHelper.assertNonNull(UnitTestWorld.lastResponseBody, field)
  }

  @Then("the response body should contain {string} equal to {string}")
  fun theResponseBodyShouldContainFieldEqualTo(field: String, expected: String) {
    UnitJsonHelper.assertStringField(UnitTestWorld.lastResponseBody, field, expected)
  }

  @Then("the response body should not contain a {string} field")
  fun theResponseBodyShouldNotContainField(field: String) {
    UnitJsonHelper.assertNotPresent(UnitTestWorld.lastResponseBody, field)
  }

  @Then("the response body should contain an error message about invalid credentials")
  fun theResponseBodyShouldContainInvalidCredentialsError() {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains("Invalid credentials") ||
        body.contains("invalid") ||
        body.contains("Unauthorized"),
      "Expected invalid credentials message in: $body",
    )
  }

  @Then("the response body should contain an error message about account deactivation")
  fun theResponseBodyShouldContainAccountDeactivationError() {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains("deactivated") || body.contains("INACTIVE") || body.contains("inactive"),
      "Expected account deactivation message in: $body",
    )
  }

  @Then("the response body should contain a validation error for {string}")
  fun theResponseBodyShouldContainValidationErrorFor(field: String) {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(body.contains(field), "Expected validation error for '$field' in: $body")
  }

  @Then("the response body should contain an error message about duplicate username")
  fun theResponseBodyShouldContainDuplicateUsernameError() {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains("already exists") || body.contains("duplicate") || body.contains("Conflict"),
      "Expected duplicate username message in: $body",
    )
  }

  @Then("the response body should contain an error message about token expiration")
  fun theResponseBodyShouldContainTokenExpirationError() {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains("expired") || body.contains("Unauthorized") || body.contains("token"),
      "Expected token expiration message in: $body",
    )
  }

  @Then("the response body should contain an error message about invalid token")
  fun theResponseBodyShouldContainInvalidTokenError() {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains("invalid") || body.contains("Invalid") || body.contains("Unauthorized"),
      "Expected invalid token message in: $body",
    )
  }

  @Then("the response body should contain an error message about file size")
  fun theResponseBodyShouldContainFileSizeError() {
    val body = UnitTestWorld.lastResponseBody
    assertTrue(
      body.contains("size") || body.contains("limit") || body.contains("large"),
      "Expected file size message in: $body",
    )
  }
}
