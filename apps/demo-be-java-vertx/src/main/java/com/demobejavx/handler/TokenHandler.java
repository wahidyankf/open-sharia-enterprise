package com.demobejavx.handler;

import com.auth0.jwt.exceptions.JWTVerificationException;
import com.demobejavx.auth.JwtService;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.RoutingContext;

public class TokenHandler implements Handler<RoutingContext> {

    private final JwtService jwtService;
    private final String action;

    public TokenHandler(String action, JwtService jwtService) {
        this.action = action;
        this.jwtService = jwtService;
    }

    @Override
    public void handle(RoutingContext ctx) {
        switch (action) {
            case "claims" -> handleClaims(ctx);
            case "jwks" -> handleJwks(ctx);
            default -> ctx.fail(500);
        }
    }

    private void handleClaims(RoutingContext ctx) {
        String authHeader = ctx.request().getHeader("Authorization");
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            ctx.fail(401);
            return;
        }
        String token = authHeader.substring(7);

        try {
            JwtService.Claims claims = jwtService.validate(token);
            JsonObject resp = new JsonObject()
                    .put("sub", claims.subject())
                    .put("iss", "demo-be-java-vertx")
                    .put("jti", claims.jti())
                    .put("role", claims.role());
            ctx.response()
                    .setStatusCode(200)
                    .putHeader("Content-Type", "application/json")
                    .end(resp.encode());
        } catch (JWTVerificationException e) {
            ctx.fail(401);
        }
    }

    private void handleJwks(RoutingContext ctx) {
        ctx.response()
                .setStatusCode(200)
                .putHeader("Content-Type", "application/json")
                .end(jwtService.getJwks());
    }
}
