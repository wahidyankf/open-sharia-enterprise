-- liquibase formatted sql

-- changeset demo-be:007-standardize-schema dbms:postgresql
-- Rename token column to jti and widen length in revoked_tokens
ALTER TABLE revoked_tokens RENAME COLUMN token TO jti;
ALTER TABLE revoked_tokens ALTER COLUMN jti TYPE VARCHAR(255);
-- Drop old unique constraint on token column and recreate on jti
ALTER TABLE revoked_tokens DROP CONSTRAINT uq_revoked_token;
ALTER TABLE revoked_tokens ADD CONSTRAINT uq_revoked_tokens_jti UNIQUE (jti);
-- Add user_id column (no FK per canonical schema)
ALTER TABLE revoked_tokens ADD COLUMN user_id UUID NOT NULL DEFAULT '00000000-0000-0000-0000-000000000000';
ALTER TABLE revoked_tokens ALTER COLUMN user_id DROP DEFAULT;
CREATE INDEX idx_revoked_tokens_user_id ON revoked_tokens (user_id);

-- Add audit columns to expenses
ALTER TABLE expenses ADD COLUMN created_by VARCHAR(255) NOT NULL DEFAULT 'system';
ALTER TABLE expenses ADD COLUMN updated_by VARCHAR(255) NOT NULL DEFAULT 'system';
ALTER TABLE expenses ADD COLUMN deleted_at TIMESTAMPTZ;
ALTER TABLE expenses ADD COLUMN deleted_by VARCHAR(255);
-- Widen currency, type, unit columns
ALTER TABLE expenses ALTER COLUMN currency TYPE VARCHAR(10);
ALTER TABLE expenses ALTER COLUMN type TYPE VARCHAR(20);
ALTER TABLE expenses ALTER COLUMN unit TYPE VARCHAR(50);

-- Add ON DELETE CASCADE to attachments expense_id FK
ALTER TABLE attachments DROP CONSTRAINT fk_attachments_expense;
ALTER TABLE attachments ADD CONSTRAINT fk_attachments_expense FOREIGN KEY (expense_id) REFERENCES expenses(id) ON DELETE CASCADE;
-- rollback ALTER TABLE attachments DROP CONSTRAINT fk_attachments_expense;
-- rollback ALTER TABLE attachments ADD CONSTRAINT fk_attachments_expense FOREIGN KEY (expense_id) REFERENCES expenses(id);
-- rollback ALTER TABLE expenses ALTER COLUMN unit TYPE VARCHAR(20);
-- rollback ALTER TABLE expenses ALTER COLUMN type TYPE VARCHAR(10);
-- rollback ALTER TABLE expenses ALTER COLUMN currency TYPE VARCHAR(3);
-- rollback ALTER TABLE expenses DROP COLUMN deleted_by;
-- rollback ALTER TABLE expenses DROP COLUMN deleted_at;
-- rollback ALTER TABLE expenses DROP COLUMN updated_by;
-- rollback ALTER TABLE expenses DROP COLUMN created_by;
-- rollback DROP INDEX idx_revoked_tokens_user_id;
-- rollback ALTER TABLE revoked_tokens DROP COLUMN user_id;
-- rollback ALTER TABLE revoked_tokens DROP CONSTRAINT uq_revoked_tokens_jti;
-- rollback ALTER TABLE revoked_tokens ADD CONSTRAINT uq_revoked_token UNIQUE (jti);
-- rollback ALTER TABLE revoked_tokens ALTER COLUMN jti TYPE VARCHAR(512);
-- rollback ALTER TABLE revoked_tokens RENAME COLUMN jti TO token;
