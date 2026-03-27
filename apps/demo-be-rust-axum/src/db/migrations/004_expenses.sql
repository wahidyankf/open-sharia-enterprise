CREATE TABLE IF NOT EXISTS expenses (
    id UUID PRIMARY KEY,
    user_id UUID NOT NULL REFERENCES users(id),
    amount DECIMAL(19,4) NOT NULL,
    currency VARCHAR(10) NOT NULL,
    category VARCHAR(100) NOT NULL,
    description VARCHAR(500) NOT NULL DEFAULT '',
    date VARCHAR(10) NOT NULL,
    type VARCHAR(20) NOT NULL,
    quantity VARCHAR(50),
    unit VARCHAR(50),
    created_at TIMESTAMPTZ NOT NULL,
    created_by VARCHAR(255) NOT NULL DEFAULT 'system',
    updated_at TIMESTAMPTZ NOT NULL,
    updated_by VARCHAR(255) NOT NULL DEFAULT 'system',
    deleted_at TIMESTAMPTZ,
    deleted_by VARCHAR(255)
);
