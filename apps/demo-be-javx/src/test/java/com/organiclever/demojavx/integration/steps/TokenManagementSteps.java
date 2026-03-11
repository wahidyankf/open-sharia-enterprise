package com.organiclever.demojavx.integration.steps;

import com.organiclever.demojavx.support.AppFactory;
import com.organiclever.demojavx.support.ScenarioState;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.Assertions;

public class TokenManagementSteps {

    private final ScenarioState state;

    public TokenManagementSteps(ScenarioState state) {
        this.state = state;
    }

    @When("alice decodes her access token payload")
    public void aliceDecodesHerAccessTokenPayload() throws Exception {
        String token = state.getAccessToken();
        Assertions.assertNotNull(token, "Access token must be set");
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/tokens/claims")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @Then("the token should contain a non-null {string} claim")
    public void theTokenShouldContainNonNullClaim(String claim) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response, "Response must be set");
        Assertions.assertEquals(200, response.statusCode(),
                "Expected 200 from claims endpoint but got " + response.statusCode());
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body, "Claims response body must not be null");
        Object value = body.getValue(claim);
        Assertions.assertNotNull(value, "Expected non-null claim '" + claim + "' in: " + body.encode());
    }

    @When("^the client sends GET /\\.well-known/jwks\\.json$")
    public void clientSendsGetJwks() throws Exception {
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/.well-known/jwks.json")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @Then("the response body should contain at least one key in the {string} array")
    public void responseBodyContainsAtLeastOneKeyInArray(String field) {
        HttpResponse<Buffer> response = state.getLastResponse();
        Assertions.assertNotNull(response);
        JsonObject body = response.bodyAsJsonObject();
        Assertions.assertNotNull(body);
        io.vertx.core.json.JsonArray keys = body.getJsonArray(field);
        Assertions.assertNotNull(keys, "Expected '" + field + "' array in response");
        Assertions.assertTrue(keys.size() > 0,
                "Expected at least one key in '" + field + "' array");
    }

    @Then("alice's access token should be recorded as revoked")
    public void alicesAccessTokenShouldBeRecordedAsRevoked() throws Exception {
        String token = state.getAccessToken();
        Assertions.assertNotNull(token);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/users/me")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        Assertions.assertEquals(401, response.statusCode());
    }

    @Given("alice has logged out and her access token is blacklisted")
    public void aliceHasLoggedOutAndTokenIsBlacklisted() throws Exception {
        String token = state.getAccessToken();
        Assertions.assertNotNull(token);
        AppFactory.getClient()
                .post("/api/v1/auth/logout")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
    }

    @Given("^the admin has disabled alice's account via POST /api/v1/admin/users/\\{alice_id\\}/disable$")
    public void adminHasDisabledAlice() throws Exception {
        String adminToken = state.getAdminAccessToken();
        Assertions.assertNotNull(adminToken);

        HttpResponse<Buffer> listResp = AppFactory.getClient()
                .get("/api/v1/admin/users")
                .bearerTokenAuthentication(adminToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String aliceId = SecuritySteps.findUserIdByUsername(
                listResp.bodyAsJsonObject().getJsonArray("data"), "alice");

        AppFactory.getClient()
                .post("/api/v1/admin/users/" + aliceId + "/disable")
                .bearerTokenAuthentication(adminToken)
                .sendJsonObject(new JsonObject().put("reason", "test"))
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
    }
}
