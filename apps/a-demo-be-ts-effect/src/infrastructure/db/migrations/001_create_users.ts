import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

export default Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`
    CREATE TABLE IF NOT EXISTS users (
      id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
      username VARCHAR(50) NOT NULL,
      email VARCHAR(255) NOT NULL,
      password_hash VARCHAR(255) NOT NULL,
      display_name VARCHAR(255) NOT NULL DEFAULT '',
      role VARCHAR(20) NOT NULL DEFAULT 'USER',
      status VARCHAR(20) NOT NULL DEFAULT 'ACTIVE',
      failed_login_attempts INTEGER NOT NULL DEFAULT 0,
      password_reset_token VARCHAR(255),
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      created_by VARCHAR(255) NOT NULL DEFAULT 'system',
      updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      updated_by VARCHAR(255) NOT NULL DEFAULT 'system',
      deleted_at TIMESTAMPTZ,
      deleted_by VARCHAR(255),
      CONSTRAINT uq_users_username UNIQUE (username),
      CONSTRAINT uq_users_email UNIQUE (email)
    )
  `;
});
