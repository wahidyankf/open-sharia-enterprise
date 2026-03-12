import { ManagedRuntime, Effect, Layer, Redacted } from "effect";
import { SqliteClient } from "@effect/sql-sqlite-node";
import { PgClient } from "@effect/sql-pg";
import { SqlClient } from "@effect/sql";
import type { SqlError } from "@effect/sql/SqlError";
import { makeServerLayer } from "./app.js";
import { loadConfig } from "./config.js";
import { UserRepositoryLive } from "./infrastructure/db/user-repo.js";
import { ExpenseRepositoryLive } from "./infrastructure/db/expense-repo.js";
import { AttachmentRepositoryLive } from "./infrastructure/db/attachment-repo.js";
import { RevokedTokenRepositoryLive } from "./infrastructure/db/token-repo.js";
import { PasswordServiceLive } from "./infrastructure/password.js";
import { JwtServiceLive } from "./auth/jwt.js";
import { CREATE_TABLES_SQL, CREATE_TABLES_SQL_PG } from "./infrastructure/db/schema.js";

function isPostgres(url: string): boolean {
  return url.startsWith("postgresql://") || url.startsWith("postgres://");
}

type DbLayer = Layer.Layer<SqlClient.SqlClient, SqlError, never>;

function makeDbLayer(databaseUrl: string): { layer: DbLayer; schemaSql: string } {
  if (isPostgres(databaseUrl)) {
    return {
      layer: PgClient.layer({ url: Redacted.make(databaseUrl) }) as unknown as DbLayer,
      schemaSql: CREATE_TABLES_SQL_PG,
    };
  }
  return {
    layer: SqliteClient.layer({
      filename: databaseUrl === "sqlite::memory:" ? ":memory:" : databaseUrl,
    }) as unknown as DbLayer,
    schemaSql: CREATE_TABLES_SQL,
  };
}

const main = Effect.gen(function* () {
  const config = yield* loadConfig();
  console.log(`Starting demo-be-tsex on port ${config.port}`);

  const db = makeDbLayer(config.databaseUrl);

  // Initialize schema
  yield* Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.unsafe(db.schemaSql);
  }).pipe(Effect.provide(db.layer));

  const appLayer = makeServerLayer(config.port).pipe(
    Layer.provide(UserRepositoryLive),
    Layer.provide(ExpenseRepositoryLive),
    Layer.provide(AttachmentRepositoryLive),
    Layer.provide(RevokedTokenRepositoryLive),
    Layer.provide(PasswordServiceLive),
    Layer.provide(JwtServiceLive(config.jwtSecret)),
    Layer.provide(db.layer),
  );

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const runtime = ManagedRuntime.make(appLayer as unknown as Layer.Layer<never, never, never>);
  yield* Effect.promise(() => runtime.runPromise(Effect.never));
});

Effect.runPromise(main).catch((error: unknown) => {
  console.error("Fatal error:", error);
  process.exit(1);
});
