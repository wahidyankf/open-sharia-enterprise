package com.demobejavx.db;

import io.vertx.core.Future;
import io.vertx.sqlclient.Pool;

/**
 * Creates all required database tables on first startup. Uses CREATE TABLE IF NOT EXISTS so it is
 * idempotent and safe to run on every application start.
 */
public final class SchemaInitializer {

    private SchemaInitializer() {}

    public static Future<Void> initialize(Pool pool) {
        return pool.query(
                        "CREATE TABLE IF NOT EXISTS users ("
                                + "  id                    UUID         NOT NULL DEFAULT gen_random_uuid(),"
                                + "  username              VARCHAR(50)  NOT NULL,"
                                + "  email                 VARCHAR(255),"
                                + "  display_name          VARCHAR(255),"
                                + "  password_hash         VARCHAR(255) NOT NULL,"
                                + "  role                  VARCHAR(20)  NOT NULL DEFAULT 'USER',"
                                + "  status                VARCHAR(20)  NOT NULL DEFAULT 'ACTIVE',"
                                + "  failed_login_attempts INT          NOT NULL DEFAULT 0,"
                                + "  password_reset_token  VARCHAR(255),"
                                + "  created_at            TIMESTAMPTZ  NOT NULL DEFAULT NOW(),"
                                + "  updated_at            TIMESTAMPTZ  NOT NULL DEFAULT NOW(),"
                                + "  CONSTRAINT pk_users PRIMARY KEY (id),"
                                + "  CONSTRAINT uq_users_username UNIQUE (username)"
                                + ")")
                .execute()
                .compose(ignored -> pool.query(
                                "CREATE TABLE IF NOT EXISTS expenses ("
                                        + "  id          UUID           NOT NULL DEFAULT gen_random_uuid(),"
                                        + "  user_id     UUID           NOT NULL,"
                                        + "  type        VARCHAR(10)    NOT NULL,"
                                        + "  amount      DECIMAL        NOT NULL,"
                                        + "  currency    VARCHAR(3)     NOT NULL,"
                                        + "  category    VARCHAR(100)   NOT NULL,"
                                        + "  description VARCHAR(500)   NOT NULL,"
                                        + "  date        DATE           NOT NULL,"
                                        + "  quantity    DECIMAL,"
                                        + "  unit        VARCHAR(20),"
                                        + "  created_at  TIMESTAMPTZ    NOT NULL DEFAULT NOW(),"
                                        + "  updated_at  TIMESTAMPTZ    NOT NULL DEFAULT NOW(),"
                                        + "  CONSTRAINT pk_expenses PRIMARY KEY (id),"
                                        + "  CONSTRAINT fk_expenses_user FOREIGN KEY (user_id)"
                                        + "    REFERENCES users(id)"
                                        + ")")
                        .execute())
                .compose(ignored -> pool.query(
                                "CREATE TABLE IF NOT EXISTS attachments ("
                                        + "  id           UUID           NOT NULL DEFAULT gen_random_uuid(),"
                                        + "  expense_id   UUID           NOT NULL,"
                                        + "  user_id      UUID           NOT NULL,"
                                        + "  filename     VARCHAR(255)   NOT NULL,"
                                        + "  content_type VARCHAR(100)   NOT NULL,"
                                        + "  size         BIGINT         NOT NULL,"
                                        + "  data         BYTEA          NOT NULL,"
                                        + "  created_at   TIMESTAMPTZ    NOT NULL DEFAULT NOW(),"
                                        + "  CONSTRAINT pk_attachments PRIMARY KEY (id),"
                                        + "  CONSTRAINT fk_attachments_expense FOREIGN KEY (expense_id)"
                                        + "    REFERENCES expenses(id)"
                                        + ")")
                        .execute())
                .compose(ignored -> pool.query(
                                "CREATE TABLE IF NOT EXISTS revoked_tokens ("
                                        + "  id         UUID         NOT NULL DEFAULT gen_random_uuid(),"
                                        + "  jti        VARCHAR(512) NOT NULL,"
                                        + "  user_id    UUID         NOT NULL,"
                                        + "  revoked_at TIMESTAMPTZ  NOT NULL DEFAULT NOW(),"
                                        + "  CONSTRAINT pk_revoked_tokens PRIMARY KEY (id),"
                                        + "  CONSTRAINT uq_revoked_tokens_jti UNIQUE (jti)"
                                        + ")")
                        .execute())
                .mapEmpty();
    }
}
