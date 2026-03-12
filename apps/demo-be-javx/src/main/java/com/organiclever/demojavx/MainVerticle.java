package com.organiclever.demojavx;

import com.organiclever.demojavx.auth.JwtService;
import com.organiclever.demojavx.auth.PasswordService;
import com.organiclever.demojavx.db.SchemaInitializer;
import com.organiclever.demojavx.repository.AttachmentRepository;
import com.organiclever.demojavx.repository.ExpenseRepository;
import com.organiclever.demojavx.repository.TokenRevocationRepository;
import com.organiclever.demojavx.repository.UserRepository;
import com.organiclever.demojavx.repository.memory.InMemoryAttachmentRepository;
import com.organiclever.demojavx.repository.memory.InMemoryExpenseRepository;
import com.organiclever.demojavx.repository.memory.InMemoryTokenRevocationRepository;
import com.organiclever.demojavx.repository.memory.InMemoryUserRepository;
import com.organiclever.demojavx.repository.pg.PgAttachmentRepository;
import com.organiclever.demojavx.repository.pg.PgExpenseRepository;
import com.organiclever.demojavx.repository.pg.PgTokenRevocationRepository;
import com.organiclever.demojavx.repository.pg.PgUserRepository;
import com.organiclever.demojavx.router.AppRouter;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.ext.web.Router;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgBuilder;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.PoolOptions;
import java.net.URI;

public class MainVerticle extends AbstractVerticle {

    private static final int DEFAULT_PORT = 8201;
    private static final String DEFAULT_JWT_SECRET = "dev-jwt-secret-at-least-32-chars-long";

    @Override
    public void start(Promise<Void> startPromise) {
        String jwtSecret = System.getenv().getOrDefault("APP_JWT_SECRET", DEFAULT_JWT_SECRET);
        int port = parsePort(System.getenv("APP_PORT"), DEFAULT_PORT);
        String databaseUrl = System.getenv("DATABASE_URL");

        JwtService jwtService = new JwtService(jwtSecret);
        PasswordService passwordService = new PasswordService();

        buildRepositories(databaseUrl)
                .onSuccess(repos -> {
                    Router router = AppRouter.create(vertx, jwtService, repos.userRepo(),
                            repos.expenseRepo(), repos.attachmentRepo(), repos.revocationRepo(),
                            passwordService);

                    vertx.createHttpServer()
                            .requestHandler(router)
                            .listen(port)
                            .<Void>mapEmpty()
                            .onComplete(startPromise);
                })
                .onFailure(startPromise::fail);
    }

    private Future<Repositories> buildRepositories(String databaseUrl) {
        if (databaseUrl == null || databaseUrl.isBlank()) {
            Repositories repos = new Repositories(
                    new InMemoryUserRepository(),
                    new InMemoryExpenseRepository(),
                    new InMemoryAttachmentRepository(),
                    new InMemoryTokenRevocationRepository());
            return Future.succeededFuture(repos);
        }

        Pool pool = createPgPool(databaseUrl);
        return SchemaInitializer.initialize(pool)
                .map(ignored -> new Repositories(
                        new PgUserRepository(pool),
                        new PgExpenseRepository(pool),
                        new PgAttachmentRepository(pool),
                        new PgTokenRevocationRepository(pool)));
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

    private record Repositories(
            UserRepository userRepo,
            ExpenseRepository expenseRepo,
            AttachmentRepository attachmentRepo,
            TokenRevocationRepository revocationRepo) {}
}
