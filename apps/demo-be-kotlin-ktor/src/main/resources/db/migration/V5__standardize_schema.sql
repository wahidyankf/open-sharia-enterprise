-- V5: Standardize schema to canonical form
-- 1. Drop tokens table and create separate refresh_tokens and revoked_tokens
-- 2. Rename failed_login_count to failed_login_attempts in users
-- 3. Add password_reset_token to users
-- 4. Fix expenses: precision, column widths, add audit columns
-- 5. Fix attachments: rename size_bytes->size, replace stored_path with data BYTEA, remove user_id

-- ============================================================
-- 1. Tokens: drop old unified table, create canonical tables
-- ============================================================
DROP TABLE IF EXISTS tokens;

CREATE TABLE refresh_tokens (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users(id),
    token_hash VARCHAR(512) NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL,
    revoked BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT uq_refresh_tokens_token_hash UNIQUE (token_hash)
);
CREATE INDEX idx_refresh_tokens_user_id ON refresh_tokens(user_id);

CREATE TABLE revoked_tokens (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    jti VARCHAR(255) NOT NULL,
    user_id UUID NOT NULL,
    revoked_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    CONSTRAINT uq_revoked_tokens_jti UNIQUE (jti)
);
CREATE INDEX idx_revoked_tokens_user_id ON revoked_tokens(user_id);

-- ============================================================
-- 2. Users: rename failed_login_count, add password_reset_token
-- ============================================================
ALTER TABLE users RENAME COLUMN failed_login_count TO failed_login_attempts;
ALTER TABLE users ADD COLUMN IF NOT EXISTS password_reset_token VARCHAR(255);

-- ============================================================
-- 3. Expenses: fix precision, column widths, add audit columns
-- ============================================================
ALTER TABLE expenses
    ALTER COLUMN amount TYPE DECIMAL(19, 4),
    ALTER COLUMN quantity TYPE DECIMAL(19, 4),
    ALTER COLUMN currency TYPE VARCHAR(10),
    ALTER COLUMN type TYPE VARCHAR(20),
    ALTER COLUMN unit TYPE VARCHAR(50);

ALTER TABLE expenses
    ADD COLUMN IF NOT EXISTS created_by VARCHAR(255) NOT NULL DEFAULT 'system',
    ADD COLUMN IF NOT EXISTS updated_by VARCHAR(255) NOT NULL DEFAULT 'system',
    ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ,
    ADD COLUMN IF NOT EXISTS deleted_by VARCHAR(255);

-- ============================================================
-- 4. Attachments: rename size_bytes->size, replace stored_path
--    with data BYTEA, remove user_id FK and column
-- ============================================================
ALTER TABLE attachments DROP CONSTRAINT IF EXISTS fk_attachments_user;
ALTER TABLE attachments DROP COLUMN IF EXISTS user_id;
ALTER TABLE attachments DROP COLUMN IF EXISTS stored_path;
ALTER TABLE attachments RENAME COLUMN size_bytes TO size;
ALTER TABLE attachments ADD COLUMN IF NOT EXISTS data BYTEA NOT NULL DEFAULT '\x';

-- Ensure expense FK has ON DELETE CASCADE (recreate constraint)
ALTER TABLE attachments DROP CONSTRAINT IF EXISTS fk_attachments_expense;
ALTER TABLE attachments ADD CONSTRAINT fk_attachments_expense
    FOREIGN KEY (expense_id) REFERENCES expenses(id) ON DELETE CASCADE;
