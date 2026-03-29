import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

export default Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`
    CREATE TABLE IF NOT EXISTS revoked_tokens (
      id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
      jti VARCHAR(255) NOT NULL,
      user_id UUID NOT NULL,
      revoked_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      CONSTRAINT uq_revoked_tokens_jti UNIQUE (jti)
    )
  `;
  yield* sql`CREATE INDEX IF NOT EXISTS idx_revoked_tokens_user_id ON revoked_tokens(user_id)`;
});
