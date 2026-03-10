-- liquibase formatted sql

-- changeset demo-be:004-create-revoked-tokens dbms:postgresql
CREATE TABLE revoked_tokens (
    id          UUID         NOT NULL DEFAULT gen_random_uuid(),
    token       VARCHAR(512) NOT NULL,
    revoked_at  TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
    CONSTRAINT pk_revoked_tokens PRIMARY KEY (id),
    CONSTRAINT uq_revoked_token UNIQUE (token)
);
-- rollback DROP TABLE revoked_tokens;

-- changeset demo-be:004-create-revoked-tokens-h2 dbms:h2
CREATE TABLE revoked_tokens (
    id          UUID         NOT NULL DEFAULT RANDOM_UUID(),
    token       VARCHAR(512) NOT NULL,
    revoked_at  TIMESTAMPTZ  NOT NULL DEFAULT NOW(),
    CONSTRAINT pk_revoked_tokens PRIMARY KEY (id),
    CONSTRAINT uq_revoked_token UNIQUE (token)
);
-- rollback DROP TABLE revoked_tokens;
