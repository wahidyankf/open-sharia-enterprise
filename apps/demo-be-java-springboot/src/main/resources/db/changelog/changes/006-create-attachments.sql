-- liquibase formatted sql

-- changeset demo-be:006-create-attachments dbms:postgresql
CREATE TABLE attachments (
    id           UUID           NOT NULL DEFAULT gen_random_uuid(),
    expense_id   UUID           NOT NULL,
    filename     VARCHAR(255)   NOT NULL,
    content_type VARCHAR(100)   NOT NULL,
    size         BIGINT         NOT NULL,
    data         BYTEA          NOT NULL,
    created_at   TIMESTAMPTZ    NOT NULL DEFAULT NOW(),
    CONSTRAINT pk_attachments PRIMARY KEY (id),
    CONSTRAINT fk_attachments_expense FOREIGN KEY (expense_id) REFERENCES expenses(id)
);
-- rollback DROP TABLE attachments;
