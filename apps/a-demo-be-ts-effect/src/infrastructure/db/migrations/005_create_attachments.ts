import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

export default Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`
    CREATE TABLE IF NOT EXISTS attachments (
      id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
      expense_id UUID NOT NULL REFERENCES expenses(id) ON DELETE CASCADE,
      filename VARCHAR(255) NOT NULL,
      content_type VARCHAR(100) NOT NULL,
      size BIGINT NOT NULL,
      data BYTEA NOT NULL,
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
    )
  `;
  yield* sql`CREATE INDEX IF NOT EXISTS idx_attachments_expense_id ON attachments(expense_id)`;
});
