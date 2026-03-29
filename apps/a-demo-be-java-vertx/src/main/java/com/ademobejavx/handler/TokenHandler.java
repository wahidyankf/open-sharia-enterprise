package com.ademobejavx.handler;

import com.auth0.jwt.exceptions.JWTVerificationException;
import com.ademobejavx.auth.JwtService;
import com.ademobejavx.contracts.TokenClaims;
import io.vertx.core.Handler;
import io.vertx.ext.web.RoutingContext;
import java.util.List;

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
            TokenClaims resp = new TokenClaims()
                    .sub(claims.subject())
                    .iss("a-demo-be-java-vertx")
                    .roles(List.of(claims.role()));
            AuthHandler.sendJson(ctx, 200, resp);
        } catch (JWTVerificationException e) {
            ctx.fail(401);
        }
    }

    private void handleJwks(RoutingContext ctx) {
        // jwtService.getJwks() returns a pre-built JSON string in JwksResponse format
        ctx.response()
                .setStatusCode(200)
                .putHeader("Content-Type", "application/json")
                .end(jwtService.getJwks());
    }
}
