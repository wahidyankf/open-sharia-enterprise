CREATE TABLE attachments (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    expense_id UUID NOT NULL REFERENCES expenses (id),
    filename VARCHAR NOT NULL,
    content_type VARCHAR NOT NULL,
    file_size BIGINT NOT NULL,
    data BYTEA NOT NULL,
    url VARCHAR NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
