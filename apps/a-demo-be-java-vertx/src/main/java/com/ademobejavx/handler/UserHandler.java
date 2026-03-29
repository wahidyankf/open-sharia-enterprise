package com.ademobejavx.handler;

import com.ademobejavx.auth.PasswordService;
import com.ademobejavx.contracts.ChangePasswordRequest;
import com.ademobejavx.contracts.UpdateProfileRequest;
import com.ademobejavx.contracts.User;
import com.ademobejavx.domain.validation.DomainException;
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

public class UserHandler implements Handler<RoutingContext> {

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

    private final UserRepository userRepo;
    private final TokenRevocationRepository revocationRepo;
    private final PasswordService passwordService;
    private final String action;

    public UserHandler(String action, UserRepository userRepo,
            TokenRevocationRepository revocationRepo, PasswordService passwordService) {
        this.action = action;
        this.userRepo = userRepo;
        this.revocationRepo = revocationRepo;
        this.passwordService = passwordService;
    }

    @Override
    public void handle(RoutingContext ctx) {
        switch (action) {
            case "getMe" -> handleGetMe(ctx);
            case "updateMe" -> handleUpdateMe(ctx);
            case "changePassword" -> handleChangePassword(ctx);
            case "deactivate" -> handleDeactivate(ctx);
            default -> ctx.fail(500);
        }
    }

    private void handleGetMe(RoutingContext ctx) {
        String userId = ctx.get("userId");
        if (userId == null) {
            ctx.fail(400);
            return;
        }
        userRepo.findById(userId)
                .onSuccess(userOpt -> {
                    if (userOpt.isEmpty()) {
                        ctx.fail(404);
                        return;
                    }
                    User resp = AuthHandler.buildContractUser(userOpt.get());
                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleUpdateMe(RoutingContext ctx) {
        String userId = ctx.get("userId");
        JsonObject body = ctx.body().asJsonObject();
        if (body == null) {
            ctx.fail(400);
            return;
        }
        if (userId == null) {
            ctx.fail(400);
            return;
        }

        UpdateProfileRequest req;
        try {
            req = MAPPER.readValue(body.encode(), UpdateProfileRequest.class);
        } catch (Exception e) {
            ctx.fail(400);
            return;
        }

        String displayName = req.getDisplayName() != null ? req.getDisplayName() : "";

        userRepo.findById(userId)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "User not found"));
                    }
                    com.ademobejavx.domain.model.User updated =
                            userOpt.get().withDisplayName(displayName);
                    return userRepo.update(updated);
                })
                .onSuccess(user -> {
                    User resp = AuthHandler.buildContractUser(user);
                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleChangePassword(RoutingContext ctx) {
        String userId = ctx.get("userId");
        JsonObject body = ctx.body().asJsonObject();
        if (body == null) {
            ctx.fail(400);
            return;
        }
        if (userId == null) {
            ctx.fail(400);
            return;
        }

        ChangePasswordRequest req;
        try {
            req = MAPPER.readValue(body.encode(), ChangePasswordRequest.class);
        } catch (Exception e) {
            ctx.fail(400);
            return;
        }

        String oldPassword = req.getOldPassword() != null ? req.getOldPassword() : "";
        String newPassword = req.getNewPassword() != null ? req.getNewPassword() : "";

        if (newPassword.isEmpty()) {
            ctx.fail(new ValidationException("newPassword", "New password must not be empty"));
            return;
        }

        userRepo.findById(userId)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "User not found"));
                    }
                    com.ademobejavx.domain.model.User user = userOpt.get();
                    if (!passwordService.verify(oldPassword, user.passwordHash())) {
                        return Future.failedFuture(new DomainException(401,
                                "Invalid credentials"));
                    }
                    String newHash = passwordService.hash(newPassword);
                    return userRepo.update(user.withPasswordHash(newHash));
                })
                .onSuccess(user -> ctx.response().setStatusCode(200).end())
                .onFailure(ctx::fail);
    }

    private void handleDeactivate(RoutingContext ctx) {
        String userId = ctx.get("userId");
        if (userId == null) {
            ctx.fail(400);
            return;
        }

        userRepo.findById(userId)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "User not found"));
                    }
                    com.ademobejavx.domain.model.User updated = userOpt.get()
                            .withStatus(com.ademobejavx.domain.model.User.STATUS_INACTIVE);
                    return userRepo.update(updated);
                })
                .compose(user -> revocationRepo.deleteByUserId(userId))
                .onSuccess(ignored -> ctx.response().setStatusCode(200).end())
                .onFailure(ctx::fail);
    }
}
