-- +goose Up
CREATE TABLE attachments (
    id           UUID         NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    expense_id   UUID         NOT NULL REFERENCES expenses(id) ON DELETE CASCADE,
    filename     VARCHAR(255) NOT NULL,
    content_type VARCHAR(100) NOT NULL,
    size         BIGINT       NOT NULL,
    data         BYTEA        NOT NULL,
    created_at   TIMESTAMPTZ  NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_attachments_expense_id ON attachments (expense_id);

-- +goose Down
DROP TABLE IF EXISTS attachments;
