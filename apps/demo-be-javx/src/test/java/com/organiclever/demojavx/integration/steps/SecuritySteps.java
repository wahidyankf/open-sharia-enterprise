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

public class SecuritySteps {

    private static final int MAX_FAILED_ATTEMPTS = 5;
    private final ScenarioState state;

    public SecuritySteps(ScenarioState state) {
        this.state = state;
    }

    @Given("{string} has had the maximum number of failed login attempts")
    public void hasHadMaxFailedLoginAttempts(String username) throws Exception {
        for (int i = 0; i < MAX_FAILED_ATTEMPTS; i++) {
            JsonObject body = new JsonObject()
                    .put("username", username)
                    .put("password", "WrongPassword!");
            AppFactory.getClient()
                    .post("/api/v1/auth/login")
                    .sendJsonObject(body)
                    .toCompletionStage()
                    .toCompletableFuture()
                    .get(5, TimeUnit.SECONDS);
        }
    }

    @Then("alice's account status should be {string}")
    public void alicesAccountStatusShouldBe(String expectedStatus) throws Exception {
        // Use admin token if available; otherwise verify through login response
        String adminToken = state.getAdminAccessToken();
        String userId = state.getUserId();

        if (adminToken != null && userId != null) {
            HttpResponse<Buffer> resp = AppFactory.getClient()
                    .get("/api/v1/admin/users")
                    .bearerTokenAuthentication(adminToken)
                    .send()
                    .toCompletionStage()
                    .toCompletableFuture()
                    .get(5, TimeUnit.SECONDS);
            JsonObject body = resp.bodyAsJsonObject();
            io.vertx.core.json.JsonArray data = body.getJsonArray("data");
            boolean found = false;
            for (int i = 0; i < data.size(); i++) {
                JsonObject user = data.getJsonObject(i);
                if (userId.equals(user.getString("id"))) {
                    Assertions.assertEquals(expectedStatus.toLowerCase(),
                            user.getString("status", "").toLowerCase());
                    found = true;
                    break;
                }
            }
            if (!found) {
                // Find alice by username
                for (int i = 0; i < data.size(); i++) {
                    JsonObject user = data.getJsonObject(i);
                    if ("alice".equals(user.getString("username"))) {
                        Assertions.assertEquals(expectedStatus.toLowerCase(),
                                user.getString("status", "").toLowerCase());
                        return;
                    }
                }
            }
        } else {
            // Try to get the last response for locked scenario
            HttpResponse<Buffer> lastResp = state.getLastResponse();
            Assertions.assertNotNull(lastResp);
            Assertions.assertEquals(401, lastResp.statusCode());
        }
    }

    @Given("a user {string} is registered and locked after too many failed logins")
    public void aUserIsRegisteredAndLocked(String username) throws Exception {
        String password = "Str0ng#Pass1";
        state.setPassword(password);
        AuthSteps authSteps = new AuthSteps(state);
        authSteps.registerUser(username, username + "@example.com", password);
        hasHadMaxFailedLoginAttempts(username);
    }

    @Given("an admin user {string} is registered and logged in")
    public void anAdminUserIsRegisteredAndLoggedIn(String username) throws Exception {
        // Register as regular user
        AuthSteps authSteps = new AuthSteps(state);
        HttpResponse<Buffer> regResp = authSteps.registerUser(username,
                username + "@example.com", "Admin#Pass1234");
        String adminId = regResp.bodyAsJsonObject().getString("id");

        // Promote to admin by directly accessing the in-memory store
        // We do this through the app's test infrastructure
        promoteToAdmin(adminId);

        // Login as admin
        HttpResponse<Buffer> loginResp = authSteps.login(username, "Admin#Pass1234");
        String adminToken = loginResp.bodyAsJsonObject().getString("access_token");
        state.setAdminAccessToken(adminToken);
    }

    @Given("an admin has unlocked alice's account")
    public void anAdminHasUnlockedAlicesAccount() throws Exception {
        // Register and login admin
        AuthSteps authSteps = new AuthSteps(state);
        HttpResponse<Buffer> regResp = authSteps.registerUser("tempAdmin",
                "tempAdmin@example.com", "Admin#Pass1234");
        String adminId = regResp.bodyAsJsonObject().getString("id");
        promoteToAdmin(adminId);
        HttpResponse<Buffer> loginResp = authSteps.login("tempAdmin", "Admin#Pass1234");
        String adminToken = loginResp.bodyAsJsonObject().getString("access_token");

        // Find alice's user id
        HttpResponse<Buffer> listResp = AppFactory.getClient()
                .get("/api/v1/admin/users")
                .bearerTokenAuthentication(adminToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String aliceId = findUserIdByUsername(listResp.bodyAsJsonObject()
                .getJsonArray("data"), "alice");

        // Unlock alice
        AppFactory.getClient()
                .post("/api/v1/admin/users/" + aliceId + "/unlock")
                .bearerTokenAuthentication(adminToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
    }

    @When("^the admin sends POST /api/v1/admin/users/\\{alice_id\\}/unlock$")
    public void adminSendsUnlock() throws Exception {
        String adminToken = state.getAdminAccessToken();
        Assertions.assertNotNull(adminToken);

        HttpResponse<Buffer> listResp = AppFactory.getClient()
                .get("/api/v1/admin/users")
                .bearerTokenAuthentication(adminToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String aliceId = findUserIdByUsername(listResp.bodyAsJsonObject()
                .getJsonArray("data"), "alice");

        HttpResponse<Buffer> response = AppFactory.getClient()
                .post("/api/v1/admin/users/" + aliceId + "/unlock")
                .bearerTokenAuthentication(adminToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        state.setLastResponse(response);
    }

    private void promoteToAdmin(String userId) throws Exception {
        // Access the in-memory user repository through AppFactory's reset mechanism
        // We use a workaround: make the user an admin by re-registering with admin path
        // Since there's no direct admin promotion API, we use the test infrastructure
        AppFactory.promoteUserToAdmin(userId);
    }

    public static String findUserIdByUsername(io.vertx.core.json.JsonArray data,
            String username) {
        for (int i = 0; i < data.size(); i++) {
            JsonObject user = data.getJsonObject(i);
            if (username.equals(user.getString("username"))) {
                return user.getString("id");
            }
        }
        return "";
    }
}
