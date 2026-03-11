package com.organiclever.demojavx.router;

import com.organiclever.demojavx.auth.AdminAuthHandler;
import com.organiclever.demojavx.auth.JwtAuthHandler;
import com.organiclever.demojavx.auth.JwtService;
import com.organiclever.demojavx.auth.PasswordService;
import com.organiclever.demojavx.domain.validation.DomainException;
import com.organiclever.demojavx.domain.validation.ValidationException;
import com.organiclever.demojavx.handler.AdminHandler;
import com.organiclever.demojavx.handler.AttachmentHandler;
import com.organiclever.demojavx.handler.AuthHandler;
import com.organiclever.demojavx.handler.ExpenseHandler;
import com.organiclever.demojavx.handler.HealthHandler;
import com.organiclever.demojavx.handler.ReportHandler;
import com.organiclever.demojavx.handler.TokenHandler;
import com.organiclever.demojavx.handler.UserHandler;
import com.organiclever.demojavx.repository.AttachmentRepository;
import com.organiclever.demojavx.repository.ExpenseRepository;
import com.organiclever.demojavx.repository.TokenRevocationRepository;
import com.organiclever.demojavx.repository.UserRepository;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.handler.BodyHandler;

public final class AppRouter {

    private AppRouter() {
    }

    public static Router create(Vertx vertx, JwtService jwtService, UserRepository userRepo,
            ExpenseRepository expenseRepo, AttachmentRepository attachmentRepo,
            TokenRevocationRepository revocationRepo, PasswordService passwordService) {

        Router router = Router.router(vertx);

        // REQUIRED: parse request body for POST/PUT/PATCH routes
        // Set body limit to 20MB for file uploads; size enforcement in handler
        router.route().handler(BodyHandler.create().setBodyLimit(20 * 1024 * 1024));

        // Global failure handler
        router.route().failureHandler(ctx -> {
            Throwable failure = ctx.failure();
            int statusCode = ctx.statusCode();

            if (failure instanceof ValidationException ve) {
                ctx.response()
                        .setStatusCode(400)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                                .put("message", ve.getMessage())
                                .put("field", ve.getField())
                                .encode());
            } else if (failure instanceof DomainException de) {
                ctx.response()
                        .setStatusCode(de.getStatusCode())
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                                .put("message", de.getMessage())
                                .encode());
            } else if (failure instanceof AttachmentHandler.FileSizeLimitException fse) {
                ctx.response()
                        .setStatusCode(413)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                                .put("message", fse.getMessage())
                                .encode());
            } else if (failure instanceof AttachmentHandler.UnsupportedMediaTypeException ume) {
                ctx.response()
                        .setStatusCode(415)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                                .put("message", ume.getMessage())
                                .put("field", "file")
                                .encode());
            } else if (statusCode > 0) {
                ctx.response().setStatusCode(statusCode).end();
            } else {
                if (failure != null) {
                    failure.printStackTrace();
                }
                String errMsg = failure != null ? failure.toString() : "Internal error";
                ctx.response()
                        .setStatusCode(500)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject().put("message", errMsg).encode());
            }
        });

        JwtAuthHandler jwtAuth = new JwtAuthHandler(jwtService, revocationRepo, userRepo);
        AdminAuthHandler adminAuth = new AdminAuthHandler();

        // Public routes
        router.get("/health").handler(new HealthHandler());
        router.get("/.well-known/jwks.json")
                .handler(new TokenHandler("jwks", jwtService));

        // Auth — public
        router.post("/api/v1/auth/register")
                .handler(new AuthHandler("register", userRepo, revocationRepo,
                        jwtService, passwordService));
        router.post("/api/v1/auth/login")
                .handler(new AuthHandler("login", userRepo, revocationRepo,
                        jwtService, passwordService));
        router.post("/api/v1/auth/refresh")
                .handler(new AuthHandler("refresh", userRepo, revocationRepo,
                        jwtService, passwordService));
        router.post("/api/v1/auth/logout")
                .handler(new AuthHandler("logout", userRepo, revocationRepo,
                        jwtService, passwordService));

        // Auth — JWT protected
        router.post("/api/v1/auth/logout-all")
                .handler(jwtAuth)
                .handler(new AuthHandler("logout-all", userRepo, revocationRepo,
                        jwtService, passwordService));

        // User routes — JWT protected
        router.get("/api/v1/users/me")
                .handler(jwtAuth)
                .handler(new UserHandler("getMe", userRepo, revocationRepo, passwordService));
        router.patch("/api/v1/users/me")
                .handler(jwtAuth)
                .handler(new UserHandler("updateMe", userRepo, revocationRepo, passwordService));
        router.post("/api/v1/users/me/password")
                .handler(jwtAuth)
                .handler(new UserHandler("changePassword", userRepo, revocationRepo,
                        passwordService));
        router.post("/api/v1/users/me/deactivate")
                .handler(jwtAuth)
                .handler(new UserHandler("deactivate", userRepo, revocationRepo, passwordService));

        // Token routes — JWT protected
        router.get("/api/v1/tokens/claims")
                .handler(jwtAuth)
                .handler(new TokenHandler("claims", jwtService));

        // Admin routes — JWT + Admin protected
        router.get("/api/v1/admin/users")
                .handler(jwtAuth)
                .handler(adminAuth)
                .handler(new AdminHandler("list", userRepo));
        router.post("/api/v1/admin/users/:id/disable")
                .handler(jwtAuth)
                .handler(adminAuth)
                .handler(new AdminHandler("disable", userRepo));
        router.post("/api/v1/admin/users/:id/enable")
                .handler(jwtAuth)
                .handler(adminAuth)
                .handler(new AdminHandler("enable", userRepo));
        router.post("/api/v1/admin/users/:id/unlock")
                .handler(jwtAuth)
                .handler(adminAuth)
                .handler(new AdminHandler("unlock", userRepo));
        router.post("/api/v1/admin/users/:id/force-password-reset")
                .handler(jwtAuth)
                .handler(adminAuth)
                .handler(new AdminHandler("forcePasswordReset", userRepo));

        // Report routes — JWT protected (register BEFORE /expenses routes)
        router.get("/api/v1/reports/pl")
                .handler(jwtAuth)
                .handler(new ReportHandler(expenseRepo));

        // Expense routes — JWT protected
        // CRITICAL: /summary BEFORE /:id to prevent "summary" matched as an ID
        router.get("/api/v1/expenses/summary")
                .handler(jwtAuth)
                .handler(new ExpenseHandler("summary", expenseRepo));
        router.post("/api/v1/expenses")
                .handler(jwtAuth)
                .handler(new ExpenseHandler("create", expenseRepo));
        router.get("/api/v1/expenses")
                .handler(jwtAuth)
                .handler(new ExpenseHandler("list", expenseRepo));
        router.get("/api/v1/expenses/:id")
                .handler(jwtAuth)
                .handler(new ExpenseHandler("get", expenseRepo));
        router.put("/api/v1/expenses/:id")
                .handler(jwtAuth)
                .handler(new ExpenseHandler("update", expenseRepo));
        router.delete("/api/v1/expenses/:id")
                .handler(jwtAuth)
                .handler(new ExpenseHandler("delete", expenseRepo));

        // Attachment routes — JWT protected
        router.post("/api/v1/expenses/:id/attachments")
                .handler(jwtAuth)
                .handler(new AttachmentHandler("upload", expenseRepo, attachmentRepo));
        router.get("/api/v1/expenses/:id/attachments")
                .handler(jwtAuth)
                .handler(new AttachmentHandler("list", expenseRepo, attachmentRepo));
        router.delete("/api/v1/expenses/:id/attachments/:aid")
                .handler(jwtAuth)
                .handler(new AttachmentHandler("delete", expenseRepo, attachmentRepo));

        return router;
    }
}
