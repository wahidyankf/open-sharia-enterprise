-- +goose Up
CREATE TABLE expenses (
    id          TEXT             NOT NULL PRIMARY KEY,
    user_id     TEXT             NOT NULL,
    amount      DOUBLE PRECISION NOT NULL,
    currency    TEXT             NOT NULL,
    category    TEXT             NOT NULL,
    description TEXT             NOT NULL DEFAULT '',
    date        TEXT             NOT NULL,
    type        TEXT             NOT NULL,
    quantity    DOUBLE PRECISION,
    unit        TEXT             NOT NULL DEFAULT '',
    created_at  TIMESTAMPTZ      NOT NULL DEFAULT NOW(),
    updated_at  TIMESTAMPTZ      NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_expenses_user_id ON expenses (user_id);

-- +goose Down
DROP TABLE IF EXISTS expenses;
