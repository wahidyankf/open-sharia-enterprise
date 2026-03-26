-- +goose Up
CREATE TABLE revoked_tokens (
    jti        TEXT        NOT NULL PRIMARY KEY,
    expires_at TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- +goose Down
DROP TABLE IF EXISTS revoked_tokens;
