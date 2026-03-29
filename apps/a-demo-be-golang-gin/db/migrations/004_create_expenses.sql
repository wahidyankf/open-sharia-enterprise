-- +goose Up
CREATE TABLE expenses (
    id          UUID          NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id     UUID          NOT NULL REFERENCES users(id),
    amount      DECIMAL(19,4) NOT NULL,
    currency    VARCHAR(10)   NOT NULL,
    category    VARCHAR(100)  NOT NULL,
    description VARCHAR(500)  NOT NULL DEFAULT '',
    date        DATE          NOT NULL,
    type        VARCHAR(20)   NOT NULL,
    quantity    DECIMAL(19,4),
    unit        VARCHAR(50),
    created_at  TIMESTAMPTZ   NOT NULL DEFAULT NOW(),
    created_by  VARCHAR(255)  NOT NULL DEFAULT 'system',
    updated_at  TIMESTAMPTZ   NOT NULL DEFAULT NOW(),
    updated_by  VARCHAR(255)  NOT NULL DEFAULT 'system',
    deleted_at  TIMESTAMPTZ,
    deleted_by  VARCHAR(255)
);

CREATE INDEX idx_expenses_user_id ON expenses (user_id);

-- +goose Down
DROP TABLE IF EXISTS expenses;
