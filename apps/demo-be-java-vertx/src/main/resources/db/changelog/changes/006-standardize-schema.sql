-- liquibase formatted sql

-- changeset demo-be:006-standardize-schema dbms:postgresql
ALTER TABLE expenses
    ALTER COLUMN amount TYPE DECIMAL(19,4),
    ALTER COLUMN quantity TYPE DECIMAL(19,4),
    ALTER COLUMN currency TYPE VARCHAR(10),
    ALTER COLUMN type TYPE VARCHAR(20),
    ALTER COLUMN unit TYPE VARCHAR(50),
    ADD COLUMN created_by VARCHAR(255) NOT NULL DEFAULT 'system',
    ADD COLUMN updated_by VARCHAR(255) NOT NULL DEFAULT 'system',
    ADD COLUMN deleted_at TIMESTAMPTZ,
    ADD COLUMN deleted_by VARCHAR(255);

ALTER TABLE revoked_tokens
    ALTER COLUMN jti TYPE VARCHAR(255);

ALTER TABLE attachments
    DROP COLUMN user_id;

ALTER TABLE attachments
    DROP CONSTRAINT fk_attachments_expense;

ALTER TABLE attachments
    ADD CONSTRAINT fk_attachments_expense FOREIGN KEY (expense_id) REFERENCES expenses(id) ON DELETE CASCADE;
-- rollback ALTER TABLE attachments DROP CONSTRAINT fk_attachments_expense; ALTER TABLE attachments ADD CONSTRAINT fk_attachments_expense FOREIGN KEY (expense_id) REFERENCES expenses(id); ALTER TABLE attachments ADD COLUMN user_id UUID NOT NULL DEFAULT gen_random_uuid(); ALTER TABLE expenses DROP COLUMN deleted_by, DROP COLUMN deleted_at, DROP COLUMN updated_by, DROP COLUMN created_by, ALTER COLUMN unit TYPE VARCHAR(20), ALTER COLUMN type TYPE VARCHAR(10), ALTER COLUMN currency TYPE VARCHAR(3), ALTER COLUMN quantity TYPE DECIMAL, ALTER COLUMN amount TYPE DECIMAL; ALTER TABLE revoked_tokens ALTER COLUMN jti TYPE VARCHAR(512);
