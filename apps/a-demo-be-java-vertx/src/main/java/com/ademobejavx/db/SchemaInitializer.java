package com.ademobejavx.db;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import java.sql.Connection;
import java.sql.DriverManager;
import java.net.URI;
import liquibase.Liquibase;
import liquibase.database.Database;
import liquibase.database.DatabaseFactory;
import liquibase.database.jvm.JdbcConnection;
import liquibase.resource.ClassLoaderResourceAccessor;

/**
 * Runs Liquibase database migrations on application startup using the programmatic API.
 *
 * <p>Liquibase requires a standard JDBC connection. Since this is a Vert.x application that uses
 * the reactive {@code Pool} for normal queries, {@code SchemaInitializer} opens a short-lived JDBC
 * connection solely for the migration step, then closes it. The blocking Liquibase call is
 * executed on a worker thread via {@link Vertx#executeBlocking} so the Vert.x event loop is never
 * blocked.
 */
public final class SchemaInitializer {

    private SchemaInitializer() {}

    /**
     * Runs all pending Liquibase migrations and returns a {@link Future} that completes when the
     * migrations have finished.
     *
     * @param vertx      the Vert.x instance used to dispatch the blocking migration work
     * @param databaseUrl the PostgreSQL URL in {@code postgresql://user:pass@host:port/db} format
     * @return a {@code Future<Void>} that succeeds when migrations complete or fails on error
     */
    public static Future<Void> initialize(Vertx vertx, String databaseUrl) {
        return vertx.executeBlocking(() -> {
            runMigrations(databaseUrl);
            return null;
        });
    }

    private static void runMigrations(String databaseUrl) throws Exception {
        String jdbcUrl = toJdbcUrl(databaseUrl);
        URI uri = URI.create(databaseUrl);
        String[] credentials = parseCredentials(uri);
        String user = credentials[0];
        String password = credentials[1];

        try (Connection connection = DriverManager.getConnection(jdbcUrl, user, password)) {
            Database database = DatabaseFactory.getInstance()
                    .findCorrectDatabaseImplementation(new JdbcConnection(connection));
            try (Liquibase liquibase = new Liquibase(
                    "db/changelog/db.changelog-master.yaml",
                    new ClassLoaderResourceAccessor(),
                    database)) {
                liquibase.update("");
            }
        }
    }

    /**
     * Converts a {@code postgresql://user:pass@host:port/db} URL to a JDBC URL in the form
     * {@code jdbc:postgresql://host:port/db}.
     */
    static String toJdbcUrl(String databaseUrl) {
        URI uri = URI.create(databaseUrl);
        String host = uri.getHost();
        int port = uri.getPort() > 0 ? uri.getPort() : 5432;
        String path = uri.getPath();
        String database = path != null && path.startsWith("/") ? path.substring(1) : path;
        return "jdbc:postgresql://" + host + ":" + port + "/" + database;
    }

    private static String[] parseCredentials(URI uri) {
        String userInfo = uri.getUserInfo();
        if (userInfo == null || userInfo.isBlank()) {
            return new String[]{"", ""};
        }
        int colon = userInfo.indexOf(':');
        if (colon >= 0) {
            return new String[]{userInfo.substring(0, colon), userInfo.substring(colon + 1)};
        }
        return new String[]{userInfo, ""};
    }
}
