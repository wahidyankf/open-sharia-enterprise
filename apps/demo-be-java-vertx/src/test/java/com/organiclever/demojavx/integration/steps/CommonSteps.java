package com.organiclever.demojavx.integration.steps;

import com.organiclever.demojavx.support.AppFactory;
import com.organiclever.demojavx.support.ScenarioState;
import io.cucumber.java.After;
import io.cucumber.java.Before;
import io.cucumber.java.BeforeAll;
import io.cucumber.java.AfterAll;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import org.junit.jupiter.api.Assertions;

public class CommonSteps {

    private final ScenarioState state;

    public CommonSteps(ScenarioState state) {
        this.state = state;
    }

    @BeforeAll
    public static void deployApp() throws Exception {
        AppFactory.deploy();
    }

    @AfterAll
    public static void closeApp() {
        AppFactory.close();
    }

    @Before
    public void resetState() {
        AppFactory.reset();
        state.reset();
    }

    @After
    public void afterScenario() {
        // no-op — cleanup done in @Before
    }

    @Given("the API is running")
    public void theApiIsRunning() {
        // Vert.x server is deployed in @BeforeAll
    }

    @Then("the response status code should be {int}")
    public void theResponseStatusCodeShouldBe(int expected) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response, "No response stored in state");
        Assertions.assertEquals(expected, response.statusCode(),
                "Expected status " + expected + " but got " + response.statusCode()
                        + ". Body: " + response.bodyAsString());
    }

    @Then("the response body should contain a non-null {string} field")
    public void theResponseBodyShouldContainNonNullField(String field) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body, "Response body is null");
        String mapped = mapFieldName(field);
        Assertions.assertNotNull(body.getValue(mapped),
                "Expected non-null field '" + mapped + "' in body: " + body.encode());
    }

    @Then("the response body should contain {string} equal to {string}")
    public void theResponseBodyShouldContainStringEqualTo(String field, String value) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body);
        String mapped = mapFieldName(field);
        Assertions.assertEquals(value, body.getString(mapped),
                "Field '" + mapped + "' expected '" + value + "' but was '"
                        + body.getString(mapped) + "'");
    }

    @Then("the response body should not contain a {string} field")
    public void theResponseBodyShouldNotContainField(String field) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body);
        String mapped = mapFieldName(field);
        Assertions.assertNull(body.getValue(mapped),
                "Expected no field '" + mapped + "' but found: " + body.getValue(mapped));
    }

    @Then("the response body should contain an error message about invalid credentials")
    public void responseContainsInvalidCredentials() {
        checkErrorResponse("Invalid credentials", "deactivated", "disabled", "locked");
    }

    @Then("the response body should contain an error message about account deactivation")
    public void responseContainsAccountDeactivation() {
        checkErrorResponse("deactivated", "Account deactivated", "disabled", "Disabled");
    }

    @Then("the response body should contain an error message about token expiration")
    public void responseContainsTokenExpiration() {
        checkErrorResponse("expired", "Token expired");
    }

    @Then("the response body should contain an error message about invalid token")
    public void responseContainsInvalidToken() {
        checkErrorResponse("invalid", "Token invalid", "Invalid");
    }

    @Then("the response body should contain a validation error for {string}")
    public void theResponseBodyShouldContainValidationError(String field) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body, "Expected JSON body for validation error");
        // Either message contains the field name or field property matches
        String msg = body.getString("message", "");
        String fieldProp = body.getString("field", "");
        boolean containsField = msg.toLowerCase().contains(field.toLowerCase())
                || field.equalsIgnoreCase(fieldProp);
        Assertions.assertTrue(containsField,
                "Expected validation error for '" + field + "' but got: " + body.encode());
    }

    @Then("the response body should contain an error message about duplicate username")
    public void responseContainsDuplicateUsername() {
        checkErrorResponse("already", "Username");
    }

    @Then("the response body should contain an error message about file size")
    public void responseContainsFileSizeError() {
        checkErrorResponse("size", "maximum", "10MB");
    }

    private void checkErrorResponse(String... fragments) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        if (body == null) {
            return;
        }
        String message = body.getString("message", "").toLowerCase();
        boolean found = false;
        for (String fragment : fragments) {
            if (message.contains(fragment.toLowerCase())) {
                found = true;
                break;
            }
        }
        Assertions.assertTrue(found,
                "Expected error message containing one of " + java.util.Arrays.toString(fragments)
                        + " but got: " + message);
    }

    private String mapFieldName(String field) {
        return switch (field) {
            case "access_token" -> "access_token";
            case "refresh_token" -> "refresh_token";
            default -> field;
        };
    }
}
