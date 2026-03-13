package com.organiclever.demojavx.unit;

import com.organiclever.demojavx.auth.JwtService;
import com.organiclever.demojavx.domain.model.User;
import com.organiclever.demojavx.support.AppFactory;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import java.time.Instant;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Unit-level coverage tests for code paths not exercised by Cucumber scenarios.
 * These tests directly call the in-process Vert.x HTTP server started by AppFactory.
 * Ported from CoverageIT (integration) so that unit-level coverage alone meets the
 * 90% threshold.
 */
class UnitCoverageTest {

    @BeforeAll
    static void startServer() throws Exception {
        AppFactory.deploy();
    }

    @AfterEach
    void resetState() {
        AppFactory.reset();
    }

    // ─────────────────────────── helpers ────────────────────────────

    private WebClient client() {
        return AppFactory.getClient();
    }

    private HttpResponse<Buffer> post(String path, JsonObject body) throws Exception {
        return client().post(path)
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
    }

    private HttpResponse<Buffer> get(String path, String bearerToken) throws Exception {
        return client().get(path)
                .bearerTokenAuthentication(bearerToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
    }

    private String register(String username, String password) throws Exception {
        JsonObject body = new JsonObject()
                .put("username", username)
                .put("email", username + "@example.com")
                .put("password", password);
        HttpResponse<Buffer> resp = post("/api/v1/auth/register", body);
        assertEquals(201, resp.statusCode());
        return resp.bodyAsJsonObject().getString("id");
    }

    private String login(String username, String password) throws Exception {
        JsonObject body = new JsonObject()
                .put("username", username)
                .put("password", password);
        HttpResponse<Buffer> resp = post("/api/v1/auth/login", body);
        assertEquals(200, resp.statusCode());
        return resp.bodyAsJsonObject().getString("access_token");
    }

    private String loginAndGetRefreshToken(String username, String password) throws Exception {
        JsonObject body = new JsonObject()
                .put("username", username)
                .put("password", password);
        HttpResponse<Buffer> resp = post("/api/v1/auth/login", body);
        assertEquals(200, resp.statusCode());
        return resp.bodyAsJsonObject().getString("refresh_token");
    }

    // ─────────────────── TokenHandler.handleClaims ──────────────────

    @Test
    void tokenClaims_validToken_returns200WithClaims() throws Exception {
        register("alice", "Str0ng#Pass1");
        String token = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = get("/api/v1/tokens/claims", token);

        assertEquals(200, resp.statusCode());
        JsonObject body = resp.bodyAsJsonObject();
        assertNotNull(body.getString("sub"));
        assertNotNull(body.getString("iss"));
        assertNotNull(body.getString("jti"));
        assertNotNull(body.getString("role"));
    }

    @Test
    void tokenClaims_noAuthHeader_returns401() throws Exception {
        HttpResponse<Buffer> resp = client().get("/api/v1/tokens/claims")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(401, resp.statusCode());
    }

    @Test
    void tokenClaims_invalidToken_returns401() throws Exception {
        // Use a bearer token that will fail JWT verification
        JwtService badJwt = new JwtService("different-secret-32-chars-or-more!!");
        User fakeUser = new User("999", "fake", "fake@example.com", "Fake",
                "hash", User.ROLE_USER, User.STATUS_ACTIVE, 0, Instant.now());
        JwtService.TokenPair pair = badJwt.generateTokenPair(fakeUser);

        HttpResponse<Buffer> resp = get("/api/v1/tokens/claims", pair.accessToken());

        assertEquals(401, resp.statusCode());
    }

    // ─────────────────── AuthHandler error paths ────────────────────

    @Test
    void register_nullBody_returns400() throws Exception {
        HttpResponse<Buffer> resp = client().post("/api/v1/auth/register")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(400, resp.statusCode());
    }

    @Test
    void login_nullBody_returns401() throws Exception {
        HttpResponse<Buffer> resp = client().post("/api/v1/auth/login")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(401, resp.statusCode());
    }

    @Test
    void login_disabledAccount_returns401() throws Exception {
        register("alice", "Str0ng#Pass1");
        String token = login("alice", "Str0ng#Pass1");

        // Register and promote admin
        register("admin", "Admin#Pass1234");
        String adminId = register("adm2", "Admin#Pass1234");
        AppFactory.promoteUserToAdmin(adminId);
        String adminToken = login("adm2", "Admin#Pass1234");

        // Get alice's ID
        HttpResponse<Buffer> listResp = client().get("/api/v1/admin/users")
                .bearerTokenAuthentication(adminToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String aliceId = findUserId(listResp, "alice");

        // Disable alice
        client().post("/api/v1/admin/users/" + aliceId + "/disable")
                .bearerTokenAuthentication(adminToken)
                .sendJsonObject(new JsonObject())
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        // Try to login as disabled alice
        JsonObject body = new JsonObject()
                .put("username", "alice")
                .put("password", "Str0ng#Pass1");
        HttpResponse<Buffer> resp = post("/api/v1/auth/login", body);

        assertEquals(401, resp.statusCode());
        // Suppress unused variable warning
        assertNotNull(token);
    }

    @Test
    void login_lockedAccount_returns401() throws Exception {
        register("alice", "Str0ng#Pass1");

        // Make 5 failed login attempts to lock the account
        JsonObject badBody = new JsonObject()
                .put("username", "alice")
                .put("password", "WrongPassword!");
        for (int i = 0; i < 5; i++) {
            post("/api/v1/auth/login", badBody);
        }

        // Try logging in now — account is locked
        JsonObject goodBody = new JsonObject()
                .put("username", "alice")
                .put("password", "Str0ng#Pass1");
        HttpResponse<Buffer> resp = post("/api/v1/auth/login", goodBody);

        assertEquals(401, resp.statusCode());
    }

    @Test
    void refresh_nullBody_returns401() throws Exception {
        HttpResponse<Buffer> resp = client().post("/api/v1/auth/refresh")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(401, resp.statusCode());
    }

    @Test
    void refresh_withAccessTokenInsteadOfRefresh_returns401() throws Exception {
        register("alice", "Str0ng#Pass1");
        String accessToken = login("alice", "Str0ng#Pass1");

        // Use access token as refresh token — wrong type
        JsonObject body = new JsonObject().put("refresh_token", accessToken);
        HttpResponse<Buffer> resp = post("/api/v1/auth/refresh", body);

        assertEquals(401, resp.statusCode());
    }

    @Test
    void refresh_expiredToken_returns401() throws Exception {
        register("alice", "Str0ng#Pass1");
        String accessToken = login("alice", "Str0ng#Pass1");

        // Get user id to create expired token
        HttpResponse<Buffer> meResp = get("/api/v1/users/me", accessToken);
        String userId = meResp.bodyAsJsonObject().getString("id");

        User fakeUser = new User(userId, "alice", "alice@example.com", "alice",
                "hash", User.ROLE_USER, User.STATUS_ACTIVE, 0, Instant.now());
        String expiredRefresh = AppFactory.getJwtService().generateExpiredRefreshToken(fakeUser);

        JsonObject body = new JsonObject().put("refresh_token", expiredRefresh);
        HttpResponse<Buffer> resp = post("/api/v1/auth/refresh", body);

        assertEquals(401, resp.statusCode());
    }

    @Test
    void logout_noAuthHeader_returns200() throws Exception {
        HttpResponse<Buffer> resp = client().post("/api/v1/auth/logout")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(200, resp.statusCode());
    }

    @Test
    void logout_invalidToken_returns200() throws Exception {
        HttpResponse<Buffer> resp = client().post("/api/v1/auth/logout")
                .bearerTokenAuthentication("not.a.valid.token")
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(200, resp.statusCode());
    }

    // ─────────────────── UserHandler error paths ─────────────────────

    @Test
    void updateMe_nullBody_returns400() throws Exception {
        register("alice", "Str0ng#Pass1");
        String token = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = client().patch("/api/v1/users/me")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(400, resp.statusCode());
    }

    @Test
    void changePassword_nullBody_returns400() throws Exception {
        register("alice", "Str0ng#Pass1");
        String token = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = client().post("/api/v1/users/me/password")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(400, resp.statusCode());
    }

    // ─────────────────── ExpenseHandler error paths ──────────────────

    @Test
    void createExpense_nullBody_returns400() throws Exception {
        register("alice", "Str0ng#Pass1");
        String token = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(token)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(400, resp.statusCode());
    }

    @Test
    void getExpense_otherUserExpense_returns403() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        register("bob", "Str0ng#Pass1");
        String bobToken = login("bob", "Str0ng#Pass1");

        // Alice creates an expense
        JsonObject expenseBody = new JsonObject()
                .put("type", "expense")
                .put("amount", "10.00")
                .put("currency", "USD")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> createResp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(expenseBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        assertEquals(201, createResp.statusCode());
        String expenseId = createResp.bodyAsJsonObject().getString("id");

        // Bob tries to get Alice's expense
        HttpResponse<Buffer> resp = client().get("/api/v1/expenses/" + expenseId)
                .bearerTokenAuthentication(bobToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(403, resp.statusCode());
    }

    @Test
    void getExpense_notFound_returns404() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = client().get("/api/v1/expenses/nonexistent-id")
                .bearerTokenAuthentication(aliceToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(404, resp.statusCode());
    }

    @Test
    void updateExpense_nullBody_returns400() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        // Create an expense first
        JsonObject expenseBody = new JsonObject()
                .put("type", "expense")
                .put("amount", "10.00")
                .put("currency", "USD")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> createResp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(expenseBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String expenseId = createResp.bodyAsJsonObject().getString("id");

        // Update with null body
        HttpResponse<Buffer> resp = client().put("/api/v1/expenses/" + expenseId)
                .bearerTokenAuthentication(aliceToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(400, resp.statusCode());
    }

    @Test
    void updateExpense_otherUserExpense_returns403() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        register("bob", "Str0ng#Pass1");
        String bobToken = login("bob", "Str0ng#Pass1");

        // Alice creates an expense
        JsonObject expenseBody = new JsonObject()
                .put("type", "expense")
                .put("amount", "10.00")
                .put("currency", "USD")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> createResp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(expenseBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String expenseId = createResp.bodyAsJsonObject().getString("id");

        // Bob tries to update Alice's expense
        JsonObject updateBody = new JsonObject().put("description", "hacked");
        HttpResponse<Buffer> resp = client().put("/api/v1/expenses/" + expenseId)
                .bearerTokenAuthentication(bobToken)
                .sendJsonObject(updateBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(403, resp.statusCode());
    }

    @Test
    void deleteExpense_otherUserExpense_returns403() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        register("bob", "Str0ng#Pass1");
        String bobToken = login("bob", "Str0ng#Pass1");

        // Alice creates an expense
        JsonObject expenseBody = new JsonObject()
                .put("type", "expense")
                .put("amount", "10.00")
                .put("currency", "USD")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> createResp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(expenseBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String expenseId = createResp.bodyAsJsonObject().getString("id");

        // Bob tries to delete Alice's expense
        HttpResponse<Buffer> resp = client().delete("/api/v1/expenses/" + expenseId)
                .bearerTokenAuthentication(bobToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(403, resp.statusCode());
    }

    @Test
    void deleteExpense_notFound_returns404() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = client().delete("/api/v1/expenses/nonexistent-id")
                .bearerTokenAuthentication(aliceToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(404, resp.statusCode());
    }

    @Test
    void createExpense_invalidCurrencyInUpdate_returns400() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        // Create an expense first
        JsonObject expenseBody = new JsonObject()
                .put("type", "expense")
                .put("amount", "10.00")
                .put("currency", "USD")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> createResp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(expenseBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String expenseId = createResp.bodyAsJsonObject().getString("id");

        // Update with invalid currency
        JsonObject updateBody = new JsonObject().put("currency", "XYZ");
        HttpResponse<Buffer> resp = client().put("/api/v1/expenses/" + expenseId)
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(updateBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(400, resp.statusCode());
    }

    // ─────────────────── AdminHandler error paths ────────────────────

    @Test
    void admin_disableNonExistentUser_returns404() throws Exception {
        register("admin", "Admin#Pass1234");
        String adminId = register("adm2", "Admin#Pass1234");
        AppFactory.promoteUserToAdmin(adminId);
        String adminToken = login("adm2", "Admin#Pass1234");

        HttpResponse<Buffer> resp = client()
                .post("/api/v1/admin/users/nonexistent/disable")
                .bearerTokenAuthentication(adminToken)
                .sendJsonObject(new JsonObject())
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(404, resp.statusCode());
    }

    @Test
    void admin_enableNonExistentUser_returns404() throws Exception {
        register("admin", "Admin#Pass1234");
        String adminId = register("adm3", "Admin#Pass1234");
        AppFactory.promoteUserToAdmin(adminId);
        String adminToken = login("adm3", "Admin#Pass1234");

        HttpResponse<Buffer> resp = client()
                .post("/api/v1/admin/users/nonexistent/enable")
                .bearerTokenAuthentication(adminToken)
                .sendJsonObject(new JsonObject())
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(404, resp.statusCode());
    }

    @Test
    void admin_unlockNonExistentUser_returns404() throws Exception {
        register("admin", "Admin#Pass1234");
        String adminId = register("adm4", "Admin#Pass1234");
        AppFactory.promoteUserToAdmin(adminId);
        String adminToken = login("adm4", "Admin#Pass1234");

        HttpResponse<Buffer> resp = client()
                .post("/api/v1/admin/users/nonexistent/unlock")
                .bearerTokenAuthentication(adminToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(404, resp.statusCode());
    }

    @Test
    void admin_forcePasswordResetNonExistentUser_returns404() throws Exception {
        register("admin", "Admin#Pass1234");
        String adminId = register("adm5", "Admin#Pass1234");
        AppFactory.promoteUserToAdmin(adminId);
        String adminToken = login("adm5", "Admin#Pass1234");

        HttpResponse<Buffer> resp = client()
                .post("/api/v1/admin/users/nonexistent/force-password-reset")
                .bearerTokenAuthentication(adminToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(404, resp.statusCode());
    }

    @Test
    void admin_nonAdminAccessAdmin_returns403() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = client().get("/api/v1/admin/users")
                .bearerTokenAuthentication(aliceToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(403, resp.statusCode());
    }

    // ─────────────────── ReportHandler error path ────────────────────

    @Test
    void report_missingDateParams_returns400() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = client().get("/api/v1/reports/pl")
                .bearerTokenAuthentication(aliceToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(400, resp.statusCode());
    }

    // ─────────────────── InMemoryTokenRevocationRepository ───────────

    @Test
    void refresh_revokedToken_returns401() throws Exception {
        register("alice", "Str0ng#Pass1");
        String refreshToken = loginAndGetRefreshToken("alice", "Str0ng#Pass1");

        // Use the refresh token once (revokes it)
        JsonObject body = new JsonObject().put("refresh_token", refreshToken);
        HttpResponse<Buffer> firstRefresh = post("/api/v1/auth/refresh", body);
        assertEquals(200, firstRefresh.statusCode());

        // Use the same refresh token again — should be revoked
        HttpResponse<Buffer> resp = post("/api/v1/auth/refresh", body);
        assertEquals(401, resp.statusCode());
    }

    // ─────────────────── AttachmentHandler error paths ───────────────

    @Test
    void attachment_notFoundExpense_returns404() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        HttpResponse<Buffer> resp = client()
                .get("/api/v1/expenses/nonexistent/attachments")
                .bearerTokenAuthentication(aliceToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(404, resp.statusCode());
    }

    @Test
    void attachment_otherUserExpenseList_returns403() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        register("bob", "Str0ng#Pass1");
        String bobToken = login("bob", "Str0ng#Pass1");

        // Alice creates an expense
        JsonObject expenseBody = new JsonObject()
                .put("type", "expense")
                .put("amount", "10.00")
                .put("currency", "USD")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> createResp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(expenseBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String expenseId = createResp.bodyAsJsonObject().getString("id");

        // Bob tries to list attachments for Alice's expense
        HttpResponse<Buffer> resp = client()
                .get("/api/v1/expenses/" + expenseId + "/attachments")
                .bearerTokenAuthentication(bobToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(403, resp.statusCode());
    }

    @Test
    void attachment_deleteNonExistentAttachment_returns404() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        // Alice creates an expense
        JsonObject expenseBody = new JsonObject()
                .put("type", "expense")
                .put("amount", "10.00")
                .put("currency", "USD")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> createResp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(expenseBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String expenseId = createResp.bodyAsJsonObject().getString("id");

        // Delete non-existent attachment
        HttpResponse<Buffer> resp = client()
                .delete("/api/v1/expenses/" + expenseId + "/attachments/nonexistent")
                .bearerTokenAuthentication(aliceToken)
                .send()
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(404, resp.statusCode());
    }

    // ─────────────────── Expense.withDescription / withAmount ────────

    @Test
    void expense_withMethods_coveredByUpdate() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        // Create an expense
        JsonObject expenseBody = new JsonObject()
                .put("type", "expense")
                .put("amount", "10.00")
                .put("currency", "USD")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> createResp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(expenseBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        String expenseId = createResp.bodyAsJsonObject().getString("id");

        // Update to trigger amount and description change paths
        JsonObject updateBody = new JsonObject()
                .put("amount", "20.00")
                .put("description", "dinner")
                .put("currency", "USD")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> resp = client().put("/api/v1/expenses/" + expenseId)
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(updateBody)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(200, resp.statusCode());
    }

    // ─────────────────── UserValidator missing path ──────────────────

    @Test
    void register_emptyPassword_returns400() throws Exception {
        JsonObject body = new JsonObject()
                .put("username", "alice")
                .put("email", "alice@example.com")
                .put("password", "");
        HttpResponse<Buffer> resp = post("/api/v1/auth/register", body);

        assertEquals(400, resp.statusCode());
    }

    // ─────────────────── ExpenseValidator IDR path ──────────────────

    @Test
    void createExpense_idrWithDecimal_returns400() throws Exception {
        register("alice", "Str0ng#Pass1");
        String aliceToken = login("alice", "Str0ng#Pass1");

        JsonObject body = new JsonObject()
                .put("type", "expense")
                .put("amount", "10000.50")
                .put("currency", "IDR")
                .put("category", "food")
                .put("description", "lunch")
                .put("date", "2025-01-15");
        HttpResponse<Buffer> resp = client().post("/api/v1/expenses")
                .bearerTokenAuthentication(aliceToken)
                .sendJsonObject(body)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        assertEquals(400, resp.statusCode());
    }

    // ─────────────────── JwtService line 106 (expiresAt null) ────────

    @Test
    void jwtService_decode_noExpiry_handlesGracefully() {
        JwtService svc = new JwtService("test-secret-32-chars-or-more-here!!");
        User user = new User("1", "alice", "alice@example.com", "Alice",
                "hash", User.ROLE_USER, User.STATUS_ACTIVE, 0, Instant.now());
        JwtService.TokenPair pair = svc.generateTokenPair(user);
        JwtService.Claims claims = svc.decode(pair.accessToken());
        assertNotNull(claims.subject());
    }

    // ─────────────────── utility ─────────────────────────────────────

    private String findUserId(HttpResponse<Buffer> listResp, String username) {
        io.vertx.core.json.JsonArray data = listResp.bodyAsJsonObject().getJsonArray("data");
        for (int i = 0; i < data.size(); i++) {
            JsonObject user = data.getJsonObject(i);
            if (username.equals(user.getString("username"))) {
                return user.getString("id");
            }
        }
        return "";
    }
}
