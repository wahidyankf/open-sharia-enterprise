package com.organiclever.demoktkt.integration.steps

import io.cucumber.java.en.Given
import io.cucumber.java.en.Then
import io.cucumber.java.en.When
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue

class CommonSteps {

    @Given("the API is running")
    fun theApiIsRunning() {
        TestDatabase.init()
    }

    @When("an operations engineer sends GET \\/health")
    fun operationsEngineerSendsGetHealth() {
        val (status, body) = ServiceDispatcher.health()
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @When("an unauthenticated engineer sends GET \\/health")
    fun unauthenticatedEngineerSendsGetHealth() {
        val (status, body) = ServiceDispatcher.health()
        TestWorld.lastResponseStatus = status
        TestWorld.lastResponseBody = body
    }

    @Then("the response status code should be {int}")
    fun theResponseStatusCodeShouldBe(code: Int) {
        assertEquals(
            code,
            TestWorld.lastResponseStatus,
            "Expected status $code but got ${TestWorld.lastResponseStatus}. Body: ${TestWorld.lastResponseBody}",
        )
    }

    @Then("the health status should be {string}")
    fun theHealthStatusShouldBe(status: String) {
        val actual = JsonHelper.getString(TestWorld.lastResponseBody, "status")
        assertEquals(
            status,
            actual,
            "Expected health status '$status' in: ${TestWorld.lastResponseBody}",
        )
    }

    @Then("the response should not include detailed component health information")
    fun theResponseShouldNotIncludeDetailedComponentHealthInformation() {
        val body = TestWorld.lastResponseBody
        assertTrue(
            !body.contains("components") && !body.contains("db"),
            "Should not include component details: $body",
        )
    }

    @Then("the response body should contain a non-null {string} field")
    fun theResponseBodyShouldContainNonNullField(field: String) {
        JsonHelper.assertNonNull(TestWorld.lastResponseBody, field)
    }

    @Then("the response body should contain {string} equal to {string}")
    fun theResponseBodyShouldContainFieldEqualTo(field: String, expected: String) {
        JsonHelper.assertStringField(TestWorld.lastResponseBody, field, expected)
    }

    @Then("the response body should not contain a {string} field")
    fun theResponseBodyShouldNotContainField(field: String) {
        JsonHelper.assertNotPresent(TestWorld.lastResponseBody, field)
    }

    @Then("the response body should contain an error message about invalid credentials")
    fun theResponseBodyShouldContainInvalidCredentialsError() {
        val body = TestWorld.lastResponseBody
        assertTrue(
            body.contains("Invalid credentials") ||
                body.contains("invalid") ||
                body.contains("Unauthorized"),
            "Expected invalid credentials message in: $body",
        )
    }

    @Then("the response body should contain an error message about account deactivation")
    fun theResponseBodyShouldContainAccountDeactivationError() {
        val body = TestWorld.lastResponseBody
        assertTrue(
            body.contains("deactivated") || body.contains("INACTIVE") || body.contains("inactive"),
            "Expected account deactivation message in: $body",
        )
    }

    @Then("the response body should contain a validation error for {string}")
    fun theResponseBodyShouldContainValidationErrorFor(field: String) {
        val body = TestWorld.lastResponseBody
        assertTrue(body.contains(field), "Expected validation error for '$field' in: $body")
    }

    @Then("the response body should contain an error message about duplicate username")
    fun theResponseBodyShouldContainDuplicateUsernameError() {
        val body = TestWorld.lastResponseBody
        assertTrue(
            body.contains("already exists") ||
                body.contains("duplicate") ||
                body.contains("Conflict"),
            "Expected duplicate username message in: $body",
        )
    }

    @Then("the response body should contain an error message about token expiration")
    fun theResponseBodyShouldContainTokenExpirationError() {
        val body = TestWorld.lastResponseBody
        assertTrue(
            body.contains("expired") || body.contains("Unauthorized") || body.contains("token"),
            "Expected token expiration message in: $body",
        )
    }

    @Then("the response body should contain an error message about invalid token")
    fun theResponseBodyShouldContainInvalidTokenError() {
        val body = TestWorld.lastResponseBody
        assertTrue(
            body.contains("invalid") || body.contains("Invalid") || body.contains("Unauthorized"),
            "Expected invalid token message in: $body",
        )
    }

    @Then("the response body should contain an error message about file size")
    fun theResponseBodyShouldContainFileSizeError() {
        val body = TestWorld.lastResponseBody
        assertTrue(
            body.contains("size") || body.contains("limit") || body.contains("large"),
            "Expected file size message in: $body",
        )
    }
}
