package com.ademobejavx.handler;

import com.auth0.jwt.exceptions.JWTVerificationException;
import com.ademobejavx.auth.JwtService;
import com.ademobejavx.auth.PasswordService;
import com.ademobejavx.contracts.AuthTokens;
import com.ademobejavx.contracts.LoginRequest;
import com.ademobejavx.contracts.RefreshRequest;
import com.ademobejavx.contracts.RegisterRequest;
import com.ademobejavx.contracts.User;
import com.ademobejavx.domain.model.TokenRevocation;
import com.ademobejavx.domain.validation.DomainException;
import com.ademobejavx.domain.validation.UserValidator;
import com.ademobejavx.domain.validation.ValidationException;
import com.ademobejavx.repository.TokenRevocationRepository;
import com.ademobejavx.repository.UserRepository;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.RoutingContext;
import java.time.Instant;

public class AuthHandler implements Handler<RoutingContext> {

    private static final int MAX_FAILED_ATTEMPTS = 5;
    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

    private final UserRepository userRepo;
    private final TokenRevocationRepository revocationRepo;
    private final JwtService jwtService;
    private final PasswordService passwordService;
    private final String action;

    public AuthHandler(String action, UserRepository userRepo,
            TokenRevocationRepository revocationRepo,
            JwtService jwtService, PasswordService passwordService) {
        this.action = action;
        this.userRepo = userRepo;
        this.revocationRepo = revocationRepo;
        this.jwtService = jwtService;
        this.passwordService = passwordService;
    }

    @Override
    public void handle(RoutingContext ctx) {
        switch (action) {
            case "register" -> handleRegister(ctx);
            case "login" -> handleLogin(ctx);
            case "refresh" -> handleRefresh(ctx);
            case "logout" -> handleLogout(ctx);
            case "logout-all" -> handleLogoutAll(ctx);
            default -> ctx.fail(500);
        }
    }

    private void handleRegister(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        if (body == null) {
            ctx.fail(new ValidationException("body", "Request body is required"));
            return;
        }

        RegisterRequest req;
        try {
            req = MAPPER.readValue(body.encode(), RegisterRequest.class);
        } catch (Exception e) {
            ctx.fail(new ValidationException("body", "Invalid request body"));
            return;
        }

        String username = req.getUsername() != null ? req.getUsername() : "";
        String email = req.getEmail() != null ? req.getEmail() : "";
        String password = req.getPassword() != null ? req.getPassword() : "";

        try {
            UserValidator.validateRegistration(username, email, password);
        } catch (ValidationException e) {
            ctx.fail(e);
            return;
        }

        userRepo.existsByUsername(username)
                .compose(exists -> {
                    if (exists) {
                        return Future.failedFuture(new DomainException(409,
                                "Username already exists"));
                    }
                    String hash = passwordService.hash(password);
                    com.ademobejavx.domain.model.User newUser = new com.ademobejavx.domain.model.User(
                            null, username, email, username,
                            hash, com.ademobejavx.domain.model.User.ROLE_USER,
                            com.ademobejavx.domain.model.User.STATUS_ACTIVE, 0, Instant.now());
                    return userRepo.save(newUser);
                })
                .onSuccess(user -> {
                    User resp = buildContractUser(user);
                    sendJson(ctx, 201, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleLogin(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        if (body == null) {
            ctx.fail(401);
            return;
        }

        LoginRequest req;
        try {
            req = MAPPER.readValue(body.encode(), LoginRequest.class);
        } catch (Exception e) {
            ctx.fail(401);
            return;
        }

        String username = req.getUsername() != null ? req.getUsername() : "";
        String password = req.getPassword() != null ? req.getPassword() : "";

        userRepo.findByUsername(username)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(401,
                                "Invalid credentials"));
                    }
                    com.ademobejavx.domain.model.User user = userOpt.get();
                    if (com.ademobejavx.domain.model.User.STATUS_INACTIVE.equals(user.status())) {
                        return Future.failedFuture(new DomainException(401,
                                "Account deactivated"));
                    }
                    if (com.ademobejavx.domain.model.User.STATUS_DISABLED.equals(user.status())) {
                        return Future.failedFuture(new DomainException(401,
                                "Account disabled"));
                    }
                    if (com.ademobejavx.domain.model.User.STATUS_LOCKED.equals(user.status())) {
                        return Future.failedFuture(new DomainException(401,
                                "Account locked"));
                    }
                    if (!passwordService.verify(password, user.passwordHash())) {
                        int attempts = user.failedLoginAttempts() + 1;
                        com.ademobejavx.domain.model.User updated =
                                user.withFailedLoginAttempts(attempts);
                        if (attempts >= MAX_FAILED_ATTEMPTS) {
                            updated = updated.withStatus(
                                    com.ademobejavx.domain.model.User.STATUS_LOCKED);
                        }
                        return userRepo.update(updated)
                                .compose(u -> Future.failedFuture(
                                        new DomainException(401, "Invalid credentials")));
                    }
                    com.ademobejavx.domain.model.User resetUser =
                            user.withFailedLoginAttempts(0);
                    return userRepo.update(resetUser);
                })
                .compose(user -> {
                    JwtService.TokenPair tokens = jwtService.generateTokenPair(user);
                    return Future.succeededFuture(tokens);
                })
                .onSuccess(tokens -> {
                    AuthTokens resp = new AuthTokens()
                            .accessToken(tokens.accessToken())
                            .refreshToken(tokens.refreshToken())
                            .tokenType("Bearer");
                    sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleRefresh(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        if (body == null) {
            ctx.fail(401);
            return;
        }

        RefreshRequest req;
        try {
            req = MAPPER.readValue(body.encode(), RefreshRequest.class);
        } catch (Exception e) {
            ctx.fail(401);
            return;
        }

        String refreshToken = req.getRefreshToken() != null ? req.getRefreshToken() : "";

        JwtService.Claims claims;
        try {
            claims = jwtService.validate(refreshToken);
        } catch (JWTVerificationException e) {
            ctx.fail(new DomainException(401, "Token expired or invalid"));
            return;
        }

        if (!"refresh".equals(claims.type())) {
            ctx.fail(new DomainException(401, "Invalid token type"));
            return;
        }

        revocationRepo.isRevoked(claims.jti())
                .compose(revoked -> {
                    if (revoked) {
                        return Future.failedFuture(new DomainException(401,
                                "Token invalid"));
                    }
                    return userRepo.findById(claims.subject());
                })
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(401, "User not found"));
                    }
                    com.ademobejavx.domain.model.User user = userOpt.get();
                    if (com.ademobejavx.domain.model.User.STATUS_DISABLED.equals(user.status())) {
                        return Future.failedFuture(new DomainException(401,
                                "Account disabled"));
                    }
                    if (!com.ademobejavx.domain.model.User.STATUS_ACTIVE.equals(user.status())) {
                        return Future.failedFuture(new DomainException(401,
                                "Account deactivated"));
                    }
                    String uid = user.id();
                    if (uid == null) {
                        return Future.failedFuture(new DomainException(500, "User id is null"));
                    }
                    TokenRevocation revoke = new TokenRevocation(claims.jti(), uid,
                            Instant.now());
                    return revocationRepo.save(revoke).map(ignored -> user);
                })
                .compose(user -> {
                    JwtService.TokenPair tokens = jwtService.generateTokenPair(user);
                    String uid = user.id();
                    if (uid == null) {
                        return Future.failedFuture(new DomainException(500, "User id is null"));
                    }
                    TokenRevocation revocation = new TokenRevocation(
                            tokens.refreshJti(), uid, Instant.now());
                    return revocationRepo.save(revocation).map(ignored -> tokens);
                })
                .onSuccess(tokens -> {
                    AuthTokens resp = new AuthTokens()
                            .accessToken(tokens.accessToken())
                            .refreshToken(tokens.refreshToken())
                            .tokenType("Bearer");
                    sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleLogout(RoutingContext ctx) {
        String authHeader = ctx.request().getHeader("Authorization");
        if (authHeader == null || !authHeader.startsWith("Bearer ")) {
            ctx.response().setStatusCode(200).end();
            return;
        }
        String token = authHeader.substring(7);

        JwtService.Claims claims;
        try {
            claims = jwtService.decode(token);
        } catch (Exception e) {
            ctx.response().setStatusCode(200).end();
            return;
        }

        TokenRevocation revocation = new TokenRevocation(claims.jti(), claims.subject(),
                Instant.now());
        revocationRepo.save(revocation)
                .onSuccess(ignored -> ctx.response().setStatusCode(200).end())
                .onFailure(ctx::fail);
    }

    private void handleLogoutAll(RoutingContext ctx) {
        String userId = ctx.get("userId");
        String jti = ctx.get("jti");

        if (userId == null || jti == null) {
            ctx.fail(400);
            return;
        }
        // Revoke the current access token
        TokenRevocation accessRevocation = new TokenRevocation(jti, userId, Instant.now());
        // Revoke a sentinel entry for "all sessions" to block any existing refresh tokens
        TokenRevocation allRevoke = new TokenRevocation(
                "all-" + userId, userId, Instant.now());
        revocationRepo.save(accessRevocation)
                .compose(ignored -> revocationRepo.save(allRevoke))
                .onSuccess(ignored -> ctx.response().setStatusCode(200).end())
                .onFailure(ctx::fail);
    }

    static User buildContractUser(com.ademobejavx.domain.model.User user) {
        User.StatusEnum status;
        switch (user.status()) {
            case com.ademobejavx.domain.model.User.STATUS_INACTIVE ->
                status = User.StatusEnum.INACTIVE;
            case com.ademobejavx.domain.model.User.STATUS_DISABLED ->
                status = User.StatusEnum.DISABLED;
            case com.ademobejavx.domain.model.User.STATUS_LOCKED ->
                status = User.StatusEnum.LOCKED;
            default -> status = User.StatusEnum.ACTIVE;
        }
        return new User()
                .id(user.id() != null ? user.id() : "")
                .username(user.username())
                .email(user.email())
                .displayName(user.displayName() != null ? user.displayName() : "")
                .status(status)
                .roles(java.util.List.of(user.role()));
    }

    static void sendJson(RoutingContext ctx, int statusCode, Object obj) {
        try {
            String json = MAPPER.writeValueAsString(obj);
            ctx.response()
                    .setStatusCode(statusCode)
                    .putHeader("Content-Type", "application/json")
                    .end(json);
        } catch (Exception e) {
            ctx.fail(500);
        }
    }
}
