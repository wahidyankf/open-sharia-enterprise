package com.organiclever.demojavx;

import com.organiclever.demojavx.auth.JwtService;
import com.organiclever.demojavx.auth.PasswordService;
import com.organiclever.demojavx.repository.memory.InMemoryAttachmentRepository;
import com.organiclever.demojavx.repository.memory.InMemoryExpenseRepository;
import com.organiclever.demojavx.repository.memory.InMemoryTokenRevocationRepository;
import com.organiclever.demojavx.repository.memory.InMemoryUserRepository;
import com.organiclever.demojavx.router.AppRouter;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.ext.web.Router;

public class MainVerticle extends AbstractVerticle {

    private static final int DEFAULT_PORT = 8201;
    private static final String DEFAULT_JWT_SECRET = "dev-jwt-secret-at-least-32-chars-long";

    @Override
    public void start(Promise<Void> startPromise) {
        String jwtSecret = System.getenv().getOrDefault("APP_JWT_SECRET", DEFAULT_JWT_SECRET);
        int port = parsePort(System.getenv("APP_PORT"), DEFAULT_PORT);

        JwtService jwtService = new JwtService(jwtSecret);
        PasswordService passwordService = new PasswordService();

        InMemoryUserRepository userRepo = new InMemoryUserRepository();
        InMemoryExpenseRepository expenseRepo = new InMemoryExpenseRepository();
        InMemoryAttachmentRepository attachmentRepo = new InMemoryAttachmentRepository();
        InMemoryTokenRevocationRepository revocationRepo = new InMemoryTokenRevocationRepository();

        Router router = AppRouter.create(vertx, jwtService, userRepo, expenseRepo,
                attachmentRepo, revocationRepo, passwordService);

        vertx.createHttpServer()
                .requestHandler(router)
                .listen(port)
                .<Void>mapEmpty()
                .onComplete(startPromise);
    }

    private int parsePort(String portEnv, int defaultPort) {
        if (portEnv == null || portEnv.isBlank()) {
            return defaultPort;
        }
        try {
            return Integer.parseInt(portEnv);
        } catch (NumberFormatException e) {
            return defaultPort;
        }
    }
}
