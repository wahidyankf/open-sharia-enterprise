import { BeforeAll, AfterAll, Before } from "@cucumber/cucumber";
import { Effect, Layer, ManagedRuntime, Redacted } from "effect";
import { PgClient } from "@effect/sql-pg";
import { SqliteClient } from "@effect/sql-sqlite-node";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { existsSync, unlinkSync } from "node:fs";
import { CREATE_TABLE_STATEMENTS, CREATE_TABLES_SQL_PG } from "../../src/infrastructure/db/schema.js";
import { UserRepositoryLive } from "../../src/infrastructure/db/user-repo.js";
import { ExpenseRepositoryLive } from "../../src/infrastructure/db/expense-repo.js";
import { AttachmentRepositoryLive } from "../../src/infrastructure/db/attachment-repo.js";
import { RevokedTokenRepositoryLive } from "../../src/infrastructure/db/token-repo.js";
import { PasswordServiceLive } from "../../src/infrastructure/password.js";
import { JwtServiceLive } from "../../src/auth/jwt.js";
import { SqlClient } from "@effect/sql";
import type { SqlError } from "@effect/sql/SqlError";

export const TEST_JWT_SECRET = process.env["APP_JWT_SECRET"] ?? "test-jwt-secret-at-least-32-chars-long!!";

const DATABASE_URL = process.env["DATABASE_URL"] ?? "";

function isPostgres(url: string): boolean {
  return url.startsWith("postgresql://") || url.startsWith("postgres://");
}

type DbLayer = Layer.Layer<SqlClient.SqlClient, SqlError, never>;

interface DbSetup {
  layer: DbLayer;
  schemaSql: string | null;
  tableStatements: string[] | null;
  dbPath: string | null;
}

function makeDbSetup(): DbSetup {
  if (isPostgres(DATABASE_URL)) {
    return {
      layer: PgClient.layer({ url: Redacted.make(DATABASE_URL) }) as unknown as DbLayer,
      schemaSql: CREATE_TABLES_SQL_PG,
      tableStatements: null,
      dbPath: null,
    };
  }
  const dbPath = join(tmpdir(), `demo-be-ts-effect-integration-test-${process.pid}.db`);
  return {
    layer: SqliteClient.layer({ filename: dbPath }) as unknown as DbLayer,
    schemaSql: null,
    tableStatements: CREATE_TABLE_STATEMENTS,
    dbPath,
  };
}

const DB = makeDbSetup();
export const SqlLayer = DB.layer;

/**
 * Service layer — all domain services backed by real PostgreSQL (or SQLite fallback).
 * No HTTP server is started. Integration tests call service functions directly.
 */
export const ServiceLayer = Layer.mergeAll(
  UserRepositoryLive,
  ExpenseRepositoryLive,
  AttachmentRepositoryLive,
  RevokedTokenRepositoryLive,
  PasswordServiceLive,
  JwtServiceLive(TEST_JWT_SECRET),
).pipe(Layer.provide(SqlLayer));

export type ServiceRuntime = ManagedRuntime.ManagedRuntime<
  | import("../../src/infrastructure/db/user-repo.js").UserRepository
  | import("../../src/infrastructure/db/expense-repo.js").ExpenseRepository
  | import("../../src/infrastructure/db/attachment-repo.js").AttachmentRepository
  | import("../../src/infrastructure/db/token-repo.js").RevokedTokenRepository
  | import("../../src/infrastructure/password.js").PasswordService
  | import("../../src/auth/jwt.js").JwtService,
  never
>;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export let serviceRuntime: any = null;

BeforeAll(async function () {
  // Initialize schema
  await Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      if (DB.schemaSql) {
        yield* sql.unsafe(DB.schemaSql);
      } else if (DB.tableStatements) {
        for (const statement of DB.tableStatements) {
          yield* sql.unsafe(statement);
        }
      }
    }).pipe(Effect.provide(SqlLayer)),
  );

  serviceRuntime = ManagedRuntime.make(ServiceLayer);
});

AfterAll(async function () {
  if (serviceRuntime) {
    serviceRuntime.dispose().catch(() => {
      /* ignore */
    });
  }
  // Clean up temp DB file (SQLite only)
  if (DB.dbPath && existsSync(DB.dbPath)) {
    try {
      unlinkSync(DB.dbPath);
    } catch {
      // Ignore cleanup errors
    }
  }
  // Schedule force-exit after cucumber has had time to write output
  setImmediate(() => {
    setTimeout(() => process.exit(0), 200);
  });
});

// Clear all tables before each scenario to ensure test isolation
Before(async function () {
  try {
    await Effect.runPromise(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.unsafe("DELETE FROM revoked_tokens");
        yield* sql.unsafe("DELETE FROM attachments");
        yield* sql.unsafe("DELETE FROM expenses");
        yield* sql.unsafe("DELETE FROM users");
      }).pipe(Effect.provide(SqlLayer)),
    );
  } catch (e) {
    console.error("Before hook DB clear error:", e);
    throw e;
  }
});

/**
 * Promote a user to ADMIN role directly in the DB.
 * Used by integration test step definitions for admin scenarios.
 */
export async function promoteToAdmin(username: string): Promise<void> {
  await Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql.unsafe(`UPDATE users SET role = 'ADMIN' WHERE username = '${username}'`);
    }).pipe(Effect.provide(SqlLayer)),
  );
}
