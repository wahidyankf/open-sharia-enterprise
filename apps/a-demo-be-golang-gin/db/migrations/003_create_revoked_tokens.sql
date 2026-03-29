-- +goose Up
CREATE TABLE revoked_tokens (
    id         UUID        NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    jti        VARCHAR(255) NOT NULL UNIQUE,
    user_id    UUID        NOT NULL,
    revoked_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_revoked_tokens_user_id ON revoked_tokens (user_id);

-- +goose Down
DROP TABLE IF EXISTS revoked_tokens;
