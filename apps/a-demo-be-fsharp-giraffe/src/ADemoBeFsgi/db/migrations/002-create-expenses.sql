CREATE TABLE expenses (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users (id),
    amount DECIMAL(18, 6) NOT NULL,
    currency VARCHAR NOT NULL,
    category VARCHAR NOT NULL,
    description VARCHAR NOT NULL,
    date DATE NOT NULL,
    entry_type VARCHAR NOT NULL,
    quantity DECIMAL(18, 6),
    unit VARCHAR,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
