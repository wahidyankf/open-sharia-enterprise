package com.organiclever.demojavx.unit.steps;

import com.organiclever.demojavx.support.AppFactory;
import com.organiclever.demojavx.support.ScenarioState;
import io.cucumber.java.en.Given;
import io.cucumber.java.en.When;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import java.util.concurrent.TimeUnit;

public class UnitAuthSteps {

    private final ScenarioState state;

    public UnitAuthSteps(ScenarioState state) {
        this.state = state;
    }

    @Given("a user {string} is registered with password {string}")
    public void aUserIsRegisteredWithPassword(String username, String password) throws Exception {
        state.setPassword(password);
        registerUser(username, username + "@example.com", password);
    }

    @Given("a user {string} is registered with email {string} and password {string}")
    public void aUserIsRegisteredWithEmailAndPassword(String username, String email,
            String password) throws Exception {
        state.setPassword(password);
        registerUser(username, email, password);
        if ("bob".equals(username)) {
            HttpResponse<Buffer> loginResp = login(username, password);
            String bobToken = loginResp.bodyAsJsonObject().getString("access_token");
            state.setBobAccessToken(bobToken);
        }
    }

    @Given("a user {string} is registered and deactivated")
    public void aUserIsRegisteredAndDeactivated(String username) throws Exception {
        String password = "Str0ng#Pass1";
        state.setPassword(password);
        registerUser(username, username + "@example.com", password);
        HttpResponse<Buffer> loginResp = login(username, password);
        String token = loginResp.bodyAsJsonObject().getString("access_token");
        AppFactory.getClient()
                .post("/api/v1/users/me/deactivate")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
    }

    @Given("{string} has logged in and stored the access token and refresh token")
    public void hasLoggedInAndStoredBothTokens(String username) throws Exception {
        String password = state.getPassword() != null ? state.getPassword() : "Str0ng#Pass1";
        HttpResponse<Buffer> resp = login(username, password);
        state.setAccessToken(resp.bodyAsJsonObject().getString("access_token"));
        state.setRefreshToken(resp.bodyAsJsonObject().getString("refresh_token"));
    }

    @Given("{string} has logged in and stored the access token")
    public void hasLoggedInAndStoredAccessToken(String username) throws Exception {
        String password = state.getPassword() != null ? state.getPassword() : "Str0ng#Pass1";
        HttpResponse<Buffer> resp = login(username, password);
        JsonObject body = resp.bodyAsJsonObject();
        state.setAccessToken(body.getString("access_token"));
        if ("alice".equals(username)) {
            state.setRefreshToken(body.getString("refresh_token"));
        }
        String userId = extractUserId(username);
        if (userId != null && !userId.isEmpty()) {
            state.setUserId(userId);
        }
    }

    @Given("the user {string} has been deactivated")
    public void theUserHasBeenDeactivated(String username) throws Exception {
        String token = state.getAccessToken();
        if (token != null) {
            AppFactory.getClient()
                    .post("/api/v1/users/me/deactivate")
                    .bearerTokenAuthentication(token)
                    .send()
                    .toCompletionStage()
                    .toCompletableFuture()
                    .get(5, TimeUnit.SECONDS);
        }
    }

    @When("^the client sends POST /api/v1/auth/register with body \\{ \"username\": \"([^\"]*)\", \"email\": \"([^\"]*)\", \"password\": \"([^\"]*)\" \\}$")
    public void clientSendsRegister(String username, String email, String password) throws Exception {
        JsonObject body = new JsonObject()
                .put("username", username)
                .put("email", email)
                .put("password", password);
        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/auth/register")
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    @When("^the client sends POST /api/v1/auth/login with body \\{ \"username\": \"([^\"]*)\", \"password\": \"([^\"]*)\" \\}$")
    public void clientSendsLogin(String username, String password) throws Exception {
        HttpResponse<Buffer> response = login(username, password);
        state.setLastResponse(response);
    }

    @Given("a user {string} is registered with email {string} and password {string} for registration conflict")
    public void registeredForConflict(String username, String email, String password) throws Exception {
        registerUser(username, email, password);
    }

    public HttpResponse<Buffer> registerUser(String username, String email,
            String password) throws Exception {
        JsonObject body = new JsonObject()
                .put("username", username)
                .put("email", email)
                .put("password", password);
        HttpResponse<Buffer> resp = AppFactory.getClient()
                .post("/api/v1/auth/register")
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        if (resp.statusCode() == 201) {
            String id = resp.bodyAsJsonObject().getString("id");
            if ("alice".equals(username)) {
                state.setUserId(id);
            }
        }
        return resp;
    }

    public HttpResponse<Buffer> login(String username, String password) throws Exception {
        JsonObject body = new JsonObject()
                .put("username", username)
                .put("password", password);
        return AppFactory.getClient()
                .post("/api/v1/auth/login")
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
    }

    private String extractUserId(String username) throws Exception {
        String token = state.getAccessToken();
        if (token == null) {
            return "";
        }
        HttpResponse<Buffer> resp = AppFactory.getClient()
                .get("/api/v1/users/me")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        return resp.bodyAsJsonObject().getString("id", "");
    }
}
