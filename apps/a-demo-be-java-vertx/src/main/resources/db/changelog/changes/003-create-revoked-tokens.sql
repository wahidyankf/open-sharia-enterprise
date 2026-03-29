-- liquibase formatted sql

-- changeset demo-be:003-create-revoked-tokens dbms:postgresql
CREATE TABLE revoked_tokens (
    id         UUID         NOT NULL DEFAULT gen_random_uuid(),
    jti        VARCHAR(512) NOT NULL,
    user_id    UUID         NOT NULL,
    revoked_at TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
    CONSTRAINT pk_revoked_tokens PRIMARY KEY (id),
    CONSTRAINT uq_revoked_tokens_jti UNIQUE (jti)
);
-- rollback DROP TABLE revoked_tokens;
