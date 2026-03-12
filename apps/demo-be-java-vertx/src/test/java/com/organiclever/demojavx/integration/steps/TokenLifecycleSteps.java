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

public class TokenLifecycleSteps {

    private final ScenarioState state;

    public TokenLifecycleSteps(ScenarioState state) {
        this.state = state;
    }

    @When("^alice sends POST /api/v1/auth/refresh with her refresh token$")
    public void aliceSendsRefresh() throws Exception {
        String refreshToken = state.getRefreshToken();
        Assertions.assertNotNull(refreshToken, "Refresh token must be set");
        JsonObject body = new JsonObject().put("refresh_token", refreshToken);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/auth/refresh")
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @Given("alice's refresh token has expired")
    public void alicesRefreshTokenHasExpired() throws Exception {
        // Find alice's user object by looking up via login, then generate expired token
        // We need to get the user id first
        // Get alice's user id from the access token
        String accessToken = state.getAccessToken();
        if (accessToken != null) {
            HttpResponse<Buffer> meResp = AppFactory.getClient()
                    .get("/api/v1/users/me")
                    .bearerTokenAuthentication(accessToken)
                    .send()
                    .toCompletionStage()
                    .toCompletableFuture()
                    .get(5, TimeUnit.SECONDS);
            String userId = meResp.bodyAsJsonObject().getString("id");
            // Create a user object to generate expired token
            com.organiclever.demojavx.domain.model.User fakeUser =
                    new com.organiclever.demojavx.domain.model.User(
                            userId, "alice", "alice@example.com", "alice",
                            "hash", "USER", "ACTIVE", 0, java.time.Instant.now());
            String expiredToken = AppFactory.getJwtService().generateExpiredRefreshToken(fakeUser);
            state.setRefreshToken(expiredToken);
        }
    }

    @Given("alice has used her refresh token to get a new token pair")
    public void aliceHasUsedRefreshToken() throws Exception {
        String originalRefreshToken = state.getRefreshToken();
        JsonObject body = new JsonObject().put("refresh_token", originalRefreshToken);
        // Don't update state.refreshToken — keep original for the test assertion
        AppFactory.getClient()
                .post("/api/v1/auth/refresh")
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
    }

    @When("^alice sends POST /api/v1/auth/refresh with her original refresh token$")
    public void aliceSendsRefreshWithOriginalToken() throws Exception {
        aliceSendsRefresh();
    }

    @When("^alice sends POST /api/v1/auth/logout with her access token$")
    public void aliceSendsLogout() throws Exception {
        String accessToken = state.getAccessToken();
        Assertions.assertNotNull(accessToken, "Access token must be set");
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/auth/logout")
                .bearerTokenAuthentication(accessToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends POST /api/v1/auth/logout-all with her access token$")
    public void aliceSendsLogoutAll() throws Exception {
        String accessToken = state.getAccessToken();
        Assertions.assertNotNull(accessToken, "Access token must be set");
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/auth/logout-all")
                .bearerTokenAuthentication(accessToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @Then("alice's access token should be invalidated")
    public void alicesAccessTokenShouldBeInvalidated() throws Exception {
        String accessToken = state.getAccessToken();
        Assertions.assertNotNull(accessToken);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/users/me")
                .bearerTokenAuthentication(accessToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        Assertions.assertEquals(401, response.statusCode(),
                "Expected 401 after logout but got " + response.statusCode());
    }

    @Given("alice has already logged out once")
    public void aliceHasAlreadyLoggedOut() throws Exception {
        aliceSendsLogout();
    }
}
