package com.ademobejavx.test;

import com.ademobejavx.domain.validation.DomainException;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.RoutingContext;

/**
 * Test-only HTTP handler for database reset and user promotion.
 *
 * <p>Only registered when {@code ENABLE_TEST_API=true}. These endpoints must never be exposed in
 * production.
 */
public class TestApiHandler implements Handler<RoutingContext> {

    private final TestApiService service;
    private final String action;

    public TestApiHandler(String action, TestApiService service) {
        this.action = action;
        this.service = service;
    }

    @Override
    public void handle(RoutingContext ctx) {
        switch (action) {
            case "reset-db" -> handleResetDb(ctx);
            case "promote-admin" -> handlePromoteAdmin(ctx);
            default -> ctx.fail(500);
        }
    }

    private void handleResetDb(RoutingContext ctx) {
        service.resetDb()
                .onSuccess(ignored -> ctx.response()
                        .setStatusCode(200)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                                .put("message", "Database reset successful")
                                .encode()))
                .onFailure(ctx::fail);
    }

    private void handlePromoteAdmin(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        if (body == null) {
            ctx.fail(new DomainException(400, "Request body is required"));
            return;
        }
        String username = body.getString("username", "");
        if (username.isBlank()) {
            ctx.fail(new DomainException(400, "username is required"));
            return;
        }

        service.promoteAdmin(username)
                .onSuccess(ignored -> ctx.response()
                        .setStatusCode(200)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                                .put("message", "User " + username + " promoted to ADMIN")
                                .encode()))
                .onFailure(err -> {
                    if (err instanceof TestApiService.UserNotFoundException) {
                        String msg = err.getMessage();
                        ctx.fail(new DomainException(404, msg != null ? msg : "User not found"));
                    } else {
                        ctx.fail(err);
                    }
                });
    }
}
