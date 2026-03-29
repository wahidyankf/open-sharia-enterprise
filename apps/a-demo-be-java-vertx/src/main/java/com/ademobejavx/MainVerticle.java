package com.ademobejavx;

import com.ademobejavx.auth.JwtService;
import com.ademobejavx.auth.PasswordService;
import com.ademobejavx.db.SchemaInitializer;
import com.ademobejavx.repository.AttachmentRepository;
import com.ademobejavx.repository.ExpenseRepository;
import com.ademobejavx.repository.TokenRevocationRepository;
import com.ademobejavx.repository.UserRepository;
import com.ademobejavx.repository.memory.InMemoryAttachmentRepository;
import com.ademobejavx.repository.memory.InMemoryExpenseRepository;
import com.ademobejavx.repository.memory.InMemoryTokenRevocationRepository;
import com.ademobejavx.repository.memory.InMemoryUserRepository;
import com.ademobejavx.repository.pg.PgAttachmentRepository;
import com.ademobejavx.repository.pg.PgExpenseRepository;
import com.ademobejavx.repository.pg.PgTokenRevocationRepository;
import com.ademobejavx.repository.pg.PgUserRepository;
import com.ademobejavx.router.AppRouter;
import com.ademobejavx.test.InMemoryTestApiService;
import com.ademobejavx.test.PgTestApiService;
import com.ademobejavx.test.TestApiService;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.ext.web.Router;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgBuilder;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.PoolOptions;
import java.net.URI;
import org.jspecify.annotations.Nullable;

public class MainVerticle extends AbstractVerticle {

    private static final int DEFAULT_PORT = 8201;
    private static final String DEFAULT_JWT_SECRET = "dev-jwt-secret-at-least-32-chars-long";

    @Override
    public void start(Promise<Void> startPromise) {
        String jwtSecret = System.getenv().getOrDefault("APP_JWT_SECRET", DEFAULT_JWT_SECRET);
        int port = parsePort(System.getenv("APP_PORT"), DEFAULT_PORT);
        String databaseUrl = System.getenv("DATABASE_URL");
        boolean enableTestApi = "true".equalsIgnoreCase(System.getenv("ENABLE_TEST_API"));

        JwtService jwtService = new JwtService(jwtSecret);
        PasswordService passwordService = new PasswordService();

        buildRepositories(databaseUrl)
                .onSuccess(repos -> {
                    System.out.println("Repositories initialized, creating HTTP server on port " + port);
                    TestApiService testApiService = enableTestApi
                            ? buildTestApiService(repos)
                            : null;
                    Router router = AppRouter.create(vertx, jwtService, repos.userRepo(),
                            repos.expenseRepo(), repos.attachmentRepo(), repos.revocationRepo(),
                            passwordService, testApiService);

                    vertx.createHttpServer()
                            .requestHandler(router)
                            .listen(port)
                            .onSuccess(server -> System.out.println(
                                    "Server listening on port " + server.actualPort()))
                            .<Void>mapEmpty()
                            .onComplete(startPromise);
                })
                .onFailure(err -> {
                    System.err.println("Failed to initialize repositories: " + err.getMessage());
                    startPromise.fail(err);
                });
    }

    private Future<Repositories> buildRepositories(@Nullable String databaseUrl) {
        if (databaseUrl == null || databaseUrl.isBlank()) {
            InMemoryUserRepository userRepo = new InMemoryUserRepository();
            InMemoryExpenseRepository expenseRepo = new InMemoryExpenseRepository();
            InMemoryAttachmentRepository attachmentRepo = new InMemoryAttachmentRepository();
            InMemoryTokenRevocationRepository revocationRepo =
                    new InMemoryTokenRevocationRepository();
            return Future.succeededFuture(
                    new Repositories(userRepo, expenseRepo, attachmentRepo, revocationRepo, null));
        }

        Pool pool = createPgPool(databaseUrl);
        return SchemaInitializer.initialize(vertx, databaseUrl)
                .map(ignored -> new Repositories(
                        new PgUserRepository(pool),
                        new PgExpenseRepository(pool),
                        new PgAttachmentRepository(pool),
                        new PgTokenRevocationRepository(pool),
                        pool));
    }

    private Pool createPgPool(String databaseUrl) {
        URI uri = URI.create(databaseUrl);
        String host = uri.getHost();
        int pgPort = uri.getPort() > 0 ? uri.getPort() : 5432;
        String path = uri.getPath();
        String database = path != null && path.startsWith("/") ? path.substring(1) : path;
        String userInfo = uri.getUserInfo();
        String user = "";
        String password = "";
        if (userInfo != null && !userInfo.isBlank()) {
            int colon = userInfo.indexOf(':');
            if (colon >= 0) {
                user = userInfo.substring(0, colon);
                password = userInfo.substring(colon + 1);
            } else {
                user = userInfo;
            }
        }

        PgConnectOptions connectOptions = new PgConnectOptions()
                .setHost(host)
                .setPort(pgPort)
                .setDatabase(database)
                .setUser(user)
                .setPassword(password);

        PoolOptions poolOptions = new PoolOptions().setMaxSize(5);

        return PgBuilder.pool()
                .with(poolOptions)
                .connectingTo(connectOptions)
                .using(vertx)
                .build();
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

    @Nullable
    private TestApiService buildTestApiService(Repositories repos) {
        if (repos.pool() != null) {
            return new PgTestApiService(repos.pool(), repos.userRepo());
        }
        if (repos.userRepo() instanceof InMemoryUserRepository inMemUser
                && repos.expenseRepo() instanceof InMemoryExpenseRepository inMemExpense
                && repos.attachmentRepo() instanceof InMemoryAttachmentRepository inMemAttachment
                && repos.revocationRepo()
                        instanceof InMemoryTokenRevocationRepository inMemRevocation) {
            return new InMemoryTestApiService(
                    inMemUser, inMemExpense, inMemAttachment, inMemRevocation);
        }
        return null;
    }

    private record Repositories(
            UserRepository userRepo,
            ExpenseRepository expenseRepo,
            AttachmentRepository attachmentRepo,
            TokenRevocationRepository revocationRepo,
            @Nullable Pool pool) {}
}
