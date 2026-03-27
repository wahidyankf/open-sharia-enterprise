import { BeforeAll, AfterAll, Before } from "@cucumber/cucumber";
import { Effect, Layer, ManagedRuntime } from "effect";
import { SqliteClient } from "@effect/sql-sqlite-node";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { existsSync, unlinkSync } from "node:fs";
import { CREATE_TABLE_STATEMENTS } from "../../../src/infrastructure/db/schema.js";
import { UserRepositoryLive } from "../../../src/infrastructure/db/user-repo.js";
import { ExpenseRepositoryLive } from "../../../src/infrastructure/db/expense-repo.js";
import { AttachmentRepositoryLive } from "../../../src/infrastructure/db/attachment-repo.js";
import { RevokedTokenRepositoryLive } from "../../../src/infrastructure/db/token-repo.js";
import { PasswordServiceLive } from "../../../src/infrastructure/password.js";
import { JwtServiceLive } from "../../../src/auth/jwt.js";
import { SqlClient } from "@effect/sql";

export const TEST_JWT_SECRET = "test-jwt-secret-at-least-32-chars-long!!";

/**
 * TEST_PORT is kept for backward compatibility with step definitions that reference it.
 * No HTTP server is started — tests call service functions directly.
 */
export const TEST_PORT = 8300;

// Use a temp file so both schema-init and ServiceLayer share the same SQLite database
const TEST_DB_PATH = join(tmpdir(), `demo-be-ts-effect-unit-bdd-${process.pid}.db`);

const SqlLayer = SqliteClient.layer({ filename: TEST_DB_PATH });

// Initialize the SQLite schema using SQLite-compatible DDL statements.
// The migration files use PostgreSQL-specific syntax (UUID, gen_random_uuid(), TIMESTAMPTZ)
// and cannot be run against SQLite. We use CREATE_TABLE_STATEMENTS which provides
// equivalent SQLite-compatible DDL for unit test purposes.
const InitDbLayer = Layer.effectDiscard(
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const stmt of CREATE_TABLE_STATEMENTS) {
      yield* sql.unsafe(stmt);
    }
  }),
).pipe(Layer.provide(SqlLayer)) as unknown as Layer.Layer<never, never, never>;

/**
 * Service layer — all domain services backed by SQLite.
 * No HTTP server is started. Unit BDD tests call service functions directly.
 */
const ServiceLayer = Layer.mergeAll(
  UserRepositoryLive,
  ExpenseRepositoryLive,
  AttachmentRepositoryLive,
  RevokedTokenRepositoryLive,
  PasswordServiceLive,
  JwtServiceLive(TEST_JWT_SECRET),
).pipe(Layer.provide(SqlLayer));

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export let serviceRuntime: any = null;

BeforeAll(async function () {
  // Initialize SQLite schema using SQLite-compatible DDL
  await Effect.runPromise(Layer.build(InitDbLayer).pipe(Effect.scoped));

  serviceRuntime = ManagedRuntime.make(ServiceLayer);
});

AfterAll(async function () {
  if (serviceRuntime) {
    serviceRuntime.dispose().catch(() => {
      /* ignore */
    });
  }
  // Clean up temp DB file
  if (existsSync(TEST_DB_PATH)) {
    try {
      unlinkSync(TEST_DB_PATH);
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
        yield* sql.unsafe("DELETE FROM refresh_tokens");
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
 * Used by unit BDD step definitions for admin scenarios.
 */
export async function promoteToAdmin(username: string): Promise<void> {
  await Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql.unsafe(`UPDATE users SET role = 'ADMIN' WHERE username = '${username}'`);
    }).pipe(Effect.provide(SqlLayer)),
  );
}
