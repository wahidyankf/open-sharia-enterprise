import { ManagedRuntime, Effect, Layer } from "effect";
import { SqliteClient } from "@effect/sql-sqlite-node";
import { SqlClient } from "@effect/sql";
import { makeServerLayer } from "./app.js";
import { loadConfig } from "./config.js";
import { UserRepositoryLive } from "./infrastructure/db/user-repo.js";
import { ExpenseRepositoryLive } from "./infrastructure/db/expense-repo.js";
import { AttachmentRepositoryLive } from "./infrastructure/db/attachment-repo.js";
import { RevokedTokenRepositoryLive } from "./infrastructure/db/token-repo.js";
import { PasswordServiceLive } from "./infrastructure/password.js";
import { JwtServiceLive } from "./auth/jwt.js";
import { CREATE_TABLES_SQL } from "./infrastructure/db/schema.js";

const main = Effect.gen(function* () {
  const config = yield* loadConfig();
  console.log(`Starting demo-be-tsex on port ${config.port}`);

  const dbFilename = config.databaseUrl === "sqlite::memory:" ? ":memory:" : config.databaseUrl;
  const SqliteLayer = SqliteClient.layer({ filename: dbFilename });

  // Initialize schema
  yield* Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.unsafe(CREATE_TABLES_SQL);
  }).pipe(Effect.provide(SqliteLayer));

  const appLayer = makeServerLayer(config.port).pipe(
    Layer.provide(UserRepositoryLive),
    Layer.provide(ExpenseRepositoryLive),
    Layer.provide(AttachmentRepositoryLive),
    Layer.provide(RevokedTokenRepositoryLive),
    Layer.provide(PasswordServiceLive),
    Layer.provide(JwtServiceLive(config.jwtSecret)),
    Layer.provide(SqliteLayer),
  );

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const runtime = ManagedRuntime.make(appLayer as unknown as Layer.Layer<never, never, never>);
  yield* Effect.promise(() => runtime.runPromise(Effect.never));
});

Effect.runPromise(main).catch((error: unknown) => {
  console.error("Fatal error:", error);
  process.exit(1);
});
