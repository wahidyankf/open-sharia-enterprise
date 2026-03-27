-- Standardize schema to match canonical definitions

-- ─── users ───────────────────────────────────────────────────────────────────
ALTER TABLE users
    ALTER COLUMN display_name SET NOT NULL,
    ALTER COLUMN display_name SET DEFAULT '',
    ADD COLUMN IF NOT EXISTS created_by VARCHAR(255) NOT NULL DEFAULT 'system',
    ADD COLUMN IF NOT EXISTS updated_by VARCHAR(255) NOT NULL DEFAULT 'system',
    ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ,
    ADD COLUMN IF NOT EXISTS deleted_by VARCHAR(255),
    ADD COLUMN IF NOT EXISTS password_reset_token VARCHAR(255);

-- ─── revoked_tokens ──────────────────────────────────────────────────────────
DROP INDEX IF EXISTS ix_revoked_tokens_token_jti;
ALTER TABLE revoked_tokens
    RENAME COLUMN token_jti TO jti;
ALTER TABLE revoked_tokens
    DROP COLUMN IF EXISTS expires_at;
CREATE UNIQUE INDEX IF NOT EXISTS ix_revoked_tokens_jti ON revoked_tokens (jti);

-- ─── expenses ────────────────────────────────────────────────────────────────
ALTER TABLE expenses
    RENAME COLUMN entry_type TO type;
ALTER TABLE expenses
    ALTER COLUMN amount TYPE DECIMAL(19, 4),
    ALTER COLUMN quantity TYPE DECIMAL(19, 4),
    ALTER COLUMN currency TYPE VARCHAR(10),
    ALTER COLUMN type TYPE VARCHAR(20),
    ALTER COLUMN unit TYPE VARCHAR(50),
    ADD COLUMN IF NOT EXISTS created_by VARCHAR(255) NOT NULL DEFAULT 'system',
    ADD COLUMN IF NOT EXISTS updated_by VARCHAR(255) NOT NULL DEFAULT 'system',
    ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ,
    ADD COLUMN IF NOT EXISTS deleted_by VARCHAR(255);

-- ─── attachments ─────────────────────────────────────────────────────────────
ALTER TABLE attachments
    RENAME COLUMN file_size TO size;
ALTER TABLE attachments
    DROP COLUMN IF EXISTS url;

-- Re-create FK with ON DELETE CASCADE
ALTER TABLE attachments
    DROP CONSTRAINT IF EXISTS attachments_expense_id_fkey;
ALTER TABLE attachments
    ADD CONSTRAINT attachments_expense_id_fkey
        FOREIGN KEY (expense_id) REFERENCES expenses (id) ON DELETE CASCADE;
