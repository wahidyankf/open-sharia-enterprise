-- liquibase formatted sql

-- changeset demo-be:002-alter-users-add-columns dbms:postgresql
ALTER TABLE users ADD COLUMN email VARCHAR(255);
ALTER TABLE users ADD COLUMN display_name VARCHAR(255);
ALTER TABLE users ADD COLUMN role VARCHAR(20) NOT NULL DEFAULT 'USER';
ALTER TABLE users ADD COLUMN status VARCHAR(20) NOT NULL DEFAULT 'ACTIVE';
ALTER TABLE users ADD COLUMN failed_login_attempts INT NOT NULL DEFAULT 0;
ALTER TABLE users ADD COLUMN password_reset_token VARCHAR(255);
ALTER TABLE users ADD CONSTRAINT uq_users_email UNIQUE (email);
-- rollback ALTER TABLE users DROP CONSTRAINT uq_users_email; DROP COLUMN password_reset_token; DROP COLUMN failed_login_attempts; DROP COLUMN status; DROP COLUMN role; DROP COLUMN display_name; DROP COLUMN email;
