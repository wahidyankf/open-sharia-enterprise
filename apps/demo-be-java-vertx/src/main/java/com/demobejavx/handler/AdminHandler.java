package com.demobejavx.handler;

import com.demobejavx.contracts.PasswordResetResponse;
import com.demobejavx.contracts.User;
import com.demobejavx.contracts.UserListResponse;
import com.demobejavx.domain.validation.DomainException;
import com.demobejavx.repository.UserRepository;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.ext.web.RoutingContext;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class AdminHandler implements Handler<RoutingContext> {

    private final UserRepository userRepo;
    private final String action;

    public AdminHandler(String action, UserRepository userRepo) {
        this.action = action;
        this.userRepo = userRepo;
    }

    @Override
    public void handle(RoutingContext ctx) {
        switch (action) {
            case "list" -> handleList(ctx);
            case "disable" -> handleDisable(ctx);
            case "enable" -> handleEnable(ctx);
            case "unlock" -> handleUnlock(ctx);
            case "forcePasswordReset" -> handleForcePasswordReset(ctx);
            default -> ctx.fail(500);
        }
    }

    private void handleList(RoutingContext ctx) {
        String emailFilter = ctx.queryParam("search").stream().findFirst().orElse(null);
        String pageParam = ctx.queryParam("page").stream().findFirst().orElse("1");
        String sizeParam = ctx.queryParam("size").stream().findFirst().orElse("20");

        int page = Math.max(1, parseInt(pageParam, 1));
        int size = Math.max(1, parseInt(sizeParam, 20));

        userRepo.findByEmail(emailFilter)
                .onSuccess(users -> {
                    int total = users.size();
                    int totalPages = size > 0 ? (int) Math.ceil((double) total / size) : 0;
                    int start = (page - 1) * size;
                    List<com.demobejavx.domain.model.User> pageUsers = users.stream()
                            .skip(start)
                            .limit(size)
                            .toList();

                    List<User> content = new ArrayList<>();
                    for (com.demobejavx.domain.model.User u : pageUsers) {
                        content.add(AuthHandler.buildContractUser(u));
                    }

                    UserListResponse resp = new UserListResponse()
                            .content(content)
                            .totalElements(total)
                            .totalPages(totalPages)
                            .page(page)
                            .size(size);

                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleDisable(RoutingContext ctx) {
        String userId = ctx.pathParam("id");
        if (userId == null) {
            ctx.fail(400);
            return;
        }
        userRepo.findById(userId)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "User not found"));
                    }
                    return userRepo.update(userOpt.get().withStatus(
                            com.demobejavx.domain.model.User.STATUS_DISABLED));
                })
                .onSuccess(user -> {
                    User resp = AuthHandler.buildContractUser(user);
                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleEnable(RoutingContext ctx) {
        String userId = ctx.pathParam("id");
        if (userId == null) {
            ctx.fail(400);
            return;
        }
        userRepo.findById(userId)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "User not found"));
                    }
                    return userRepo.update(userOpt.get().withStatus(
                            com.demobejavx.domain.model.User.STATUS_ACTIVE));
                })
                .onSuccess(user -> {
                    User resp = AuthHandler.buildContractUser(user);
                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private void handleUnlock(RoutingContext ctx) {
        String userId = ctx.pathParam("id");
        if (userId == null) {
            ctx.fail(400);
            return;
        }
        userRepo.findById(userId)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "User not found"));
                    }
                    com.demobejavx.domain.model.User updated = userOpt.get()
                            .withStatus(com.demobejavx.domain.model.User.STATUS_ACTIVE)
                            .withFailedLoginAttempts(0);
                    return userRepo.update(updated);
                })
                .onSuccess(user -> ctx.response().setStatusCode(200).end())
                .onFailure(ctx::fail);
    }

    private void handleForcePasswordReset(RoutingContext ctx) {
        String userId = ctx.pathParam("id");
        if (userId == null) {
            ctx.fail(400);
            return;
        }
        userRepo.findById(userId)
                .compose(userOpt -> {
                    if (userOpt.isEmpty()) {
                        return Future.failedFuture(new DomainException(404, "User not found"));
                    }
                    return Future.succeededFuture(userOpt.get());
                })
                .onSuccess(user -> {
                    String resetToken = UUID.randomUUID().toString();
                    PasswordResetResponse resp = new PasswordResetResponse().token(resetToken);
                    AuthHandler.sendJson(ctx, 200, resp);
                })
                .onFailure(ctx::fail);
    }

    private int parseInt(String value, int defaultValue) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }
}
