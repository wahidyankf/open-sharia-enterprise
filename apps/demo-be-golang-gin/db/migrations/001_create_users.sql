-- +goose Up
CREATE TABLE users (
    id              TEXT        NOT NULL PRIMARY KEY,
    username        TEXT        NOT NULL,
    email           TEXT        NOT NULL,
    password_hash   TEXT        NOT NULL,
    display_name    TEXT        NOT NULL DEFAULT '',
    status          TEXT        NOT NULL DEFAULT 'ACTIVE',
    role            TEXT        NOT NULL DEFAULT 'USER',
    failed_attempts INTEGER     NOT NULL DEFAULT 0,
    locked_at       TIMESTAMPTZ,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    created_by      TEXT,
    updated_by      TEXT,
    deleted_at      TIMESTAMPTZ,
    deleted_by      TEXT,
    CONSTRAINT users_username_key UNIQUE (username)
);

-- +goose Down
DROP TABLE IF EXISTS users;
