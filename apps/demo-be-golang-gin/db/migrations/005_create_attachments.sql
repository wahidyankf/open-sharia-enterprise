-- +goose Up
CREATE TABLE attachments (
    id           TEXT    NOT NULL PRIMARY KEY,
    expense_id   TEXT    NOT NULL,
    filename     TEXT    NOT NULL,
    content_type TEXT    NOT NULL,
    size         BIGINT  NOT NULL,
    url          TEXT    NOT NULL DEFAULT '',
    created_at   TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_attachments_expense_id ON attachments (expense_id);

-- +goose Down
DROP TABLE IF EXISTS attachments;
