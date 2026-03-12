package com.organiclever.demojavx.support;

import com.organiclever.demojavx.auth.JwtService;
import com.organiclever.demojavx.auth.PasswordService;
import com.organiclever.demojavx.domain.model.User;
import com.organiclever.demojavx.repository.memory.InMemoryAttachmentRepository;
import com.organiclever.demojavx.repository.memory.InMemoryExpenseRepository;
import com.organiclever.demojavx.repository.memory.InMemoryTokenRevocationRepository;
import com.organiclever.demojavx.repository.memory.InMemoryUserRepository;
import com.organiclever.demojavx.router.AppRouter;
import io.vertx.core.Vertx;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.WebClientOptions;
import java.io.IOException;
import java.net.ServerSocket;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public final class AppFactory {

    private static Vertx vertx;
    private static WebClient client;
    private static int port;
    private static InMemoryUserRepository userRepo;
    private static InMemoryExpenseRepository expenseRepo;
    private static InMemoryAttachmentRepository attachmentRepo;
    private static InMemoryTokenRevocationRepository revocationRepo;
    private static JwtService jwtService;
    private static PasswordService passwordService;

    private AppFactory() {
    }

    public static synchronized void deploy() throws Exception {
        if (vertx != null) {
            return;
        }
        vertx = Vertx.vertx();
        port = findFreePort();

        jwtService = new JwtService("test-secret-32-chars-or-more-here!!");
        passwordService = new PasswordService();
        userRepo = new InMemoryUserRepository();
        expenseRepo = new InMemoryExpenseRepository();
        attachmentRepo = new InMemoryAttachmentRepository();
        revocationRepo = new InMemoryTokenRevocationRepository();

        Router router = AppRouter.create(vertx, jwtService, userRepo, expenseRepo,
                attachmentRepo, revocationRepo, passwordService);

        vertx.createHttpServer()
                .requestHandler(router)
                .listen(port)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);

        client = WebClient.create(vertx,
                new WebClientOptions()
                        .setDefaultHost("localhost")
                        .setDefaultPort(port));
    }

    public static WebClient getClient() {
        return client;
    }

    public static JwtService getJwtService() {
        return jwtService;
    }

    public static PasswordService getPasswordService() {
        return passwordService;
    }

    public static void reset() {
        userRepo.reset();
        expenseRepo.reset();
        attachmentRepo.reset();
        revocationRepo.reset();
    }

    public static synchronized void close() {
        if (vertx != null) {
            vertx.close();
            vertx = null;
        }
    }

    public static void promoteUserToAdmin(String userId) throws Exception {
        Optional<User> userOpt = userRepo.findById(userId)
                .toCompletionStage()
                .toCompletableFuture()
                .get(5, TimeUnit.SECONDS);
        if (userOpt.isPresent()) {
            User admin = new User(userOpt.get().id(), userOpt.get().username(),
                    userOpt.get().email(), userOpt.get().displayName(),
                    userOpt.get().passwordHash(), User.ROLE_ADMIN, userOpt.get().status(),
                    0, userOpt.get().createdAt());
            userRepo.update(admin)
                    .toCompletionStage()
                    .toCompletableFuture()
                    .get(5, TimeUnit.SECONDS);
        }
    }

    private static int findFreePort() throws IOException {
        try (ServerSocket socket = new ServerSocket(0)) {
            return socket.getLocalPort();
        }
    }
}
