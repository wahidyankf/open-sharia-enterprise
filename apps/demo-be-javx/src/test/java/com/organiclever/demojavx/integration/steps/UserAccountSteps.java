package com.organiclever.demojavx.integration.steps;

import com.organiclever.demojavx.support.AppFactory;
import com.organiclever.demojavx.support.ScenarioState;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import java.util.concurrent.TimeUnit;

public class UserAccountSteps {

    private final ScenarioState state;

    public UserAccountSteps(ScenarioState state) {
        this.state = state;
    }

    @When("^alice sends GET /api/v1/users/me$")
    public void aliceSendsGetMe() throws Exception {
        String token = state.getAccessToken();
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/users/me")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends PATCH /api/v1/users/me with body \\{ \"display_name\": \"([^\"]*)\" \\}$")
    public void aliceSendsPatchMe(String displayName) throws Exception {
        String token = state.getAccessToken();
        JsonObject body = new JsonObject().put("display_name", displayName);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .patch("/api/v1/users/me")
                .bearerTokenAuthentication(token)
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends POST /api/v1/users/me/password with body \\{ \"old_password\": \"([^\"]*)\", \"new_password\": \"([^\"]*)\" \\}$")
    public void aliceSendsChangePassword(String oldPassword, String newPassword) throws Exception {
        String token = state.getAccessToken();
        JsonObject body = new JsonObject()
                .put("old_password", oldPassword)
                .put("new_password", newPassword);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/users/me/password")
                .bearerTokenAuthentication(token)
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^alice sends POST /api/v1/users/me/deactivate$")
    public void aliceSendsDeactivate() throws Exception {
        String token = state.getAccessToken();
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/users/me/deactivate")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @Given("^alice has deactivated her own account via POST /api/v1/users/me/deactivate$")
    public void aliceHasDeactivatedHerAccount() throws Exception {
        aliceSendsDeactivate();
    }

    @When("^the client sends GET /api/v1/users/me with alice's access token$")
    public void clientSendsGetMeWithAlicesToken() throws Exception {
        String token = state.getAccessToken();
        HttpResponse<Buffer> response = AppFactory.getClient()
                .get("/api/v1/users/me")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }
}
