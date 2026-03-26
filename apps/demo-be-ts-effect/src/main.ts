import { ManagedRuntime, Effect, Layer, Redacted } from "effect";
import { SqliteClient } from "@effect/sql-sqlite-node";
import { SqliteMigrator } from "@effect/sql-sqlite-node";
import { PgClient } from "@effect/sql-pg";
import { PgMigrator } from "@effect/sql-pg";
import type { SqlClient } from "@effect/sql";
import type { SqlError } from "@effect/sql/SqlError";
import { NodeContext } from "@effect/platform-node";
import { makeServerLayer } from "./app.js";
import { loadConfig } from "./config.js";
import { UserRepositoryLive } from "./infrastructure/db/user-repo.js";
import { ExpenseRepositoryLive } from "./infrastructure/db/expense-repo.js";
import { AttachmentRepositoryLive } from "./infrastructure/db/attachment-repo.js";
import { RevokedTokenRepositoryLive } from "./infrastructure/db/token-repo.js";
import { PasswordServiceLive } from "./infrastructure/password.js";
import { JwtServiceLive } from "./auth/jwt.js";
import { migrations } from "./infrastructure/db/migrations/index.js";

function isPostgres(url: string): boolean {
  return url.startsWith("postgresql://") || url.startsWith("postgres://");
}

type DbLayer = Layer.Layer<SqlClient.SqlClient, SqlError, never>;

function makeDbLayer(databaseUrl: string): { dbLayer: DbLayer; migratorLayer: Layer.Layer<never, never, never> } {
  if (isPostgres(databaseUrl)) {
    const dbLayer = PgClient.layer({ url: Redacted.make(databaseUrl) }) as unknown as DbLayer;
    const migratorLayer = PgMigrator.layer({
      loader: PgMigrator.fromRecord(migrations),
      table: "effect_sql_migrations",
    }).pipe(Layer.provide(dbLayer), Layer.provide(NodeContext.layer)) as unknown as Layer.Layer<never, never, never>;
    return { dbLayer, migratorLayer };
  }
  const dbLayer = SqliteClient.layer({
    filename: databaseUrl === "sqlite::memory:" ? ":memory:" : databaseUrl,
  }) as unknown as DbLayer;
  const migratorLayer = SqliteMigrator.layer({
    loader: SqliteMigrator.fromRecord(migrations),
    table: "effect_sql_migrations",
  }).pipe(Layer.provide(dbLayer)) as unknown as Layer.Layer<never, never, never>;
  return { dbLayer, migratorLayer };
}

const main = Effect.gen(function* () {
  const config = yield* loadConfig();
  console.log(`Starting demo-be-ts-effect on port ${config.port}`);

  const { dbLayer, migratorLayer } = makeDbLayer(config.databaseUrl);

  // Run migrations before starting the server
  yield* Layer.build(migratorLayer).pipe(Effect.scoped);

  const appLayer = makeServerLayer(config.port).pipe(
    Layer.provide(UserRepositoryLive),
    Layer.provide(ExpenseRepositoryLive),
    Layer.provide(AttachmentRepositoryLive),
    Layer.provide(RevokedTokenRepositoryLive),
    Layer.provide(PasswordServiceLive),
    Layer.provide(JwtServiceLive(config.jwtSecret)),
    Layer.provide(dbLayer),
  );

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const runtime = ManagedRuntime.make(appLayer as unknown as Layer.Layer<never, never, never>);
  yield* Effect.promise(() => runtime.runPromise(Effect.never));
});

Effect.runPromise(main).catch((error: unknown) => {
  console.error("Fatal error:", error);
  process.exit(1);
});
