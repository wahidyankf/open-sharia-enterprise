-- +goose Up
CREATE TABLE refresh_tokens (
    id         TEXT        NOT NULL PRIMARY KEY,
    user_id    TEXT        NOT NULL,
    token_str  TEXT        NOT NULL,
    revoked    BOOLEAN     NOT NULL DEFAULT FALSE,
    expires_at TIMESTAMPTZ NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT refresh_tokens_token_str_key UNIQUE (token_str)
);

CREATE INDEX idx_refresh_tokens_user_id ON refresh_tokens (user_id);

-- +goose Down
DROP TABLE IF EXISTS refresh_tokens;
