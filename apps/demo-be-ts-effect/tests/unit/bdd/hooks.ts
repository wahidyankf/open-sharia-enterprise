import { BeforeAll, AfterAll, Before } from "@cucumber/cucumber";
import { Effect, Layer, ManagedRuntime, Option } from "effect";
import { SqliteClient } from "@effect/sql-sqlite-node";
import { NodeHttpServer } from "@effect/platform-node";
import { HttpServer, HttpServerRequest, FileSystem } from "@effect/platform";
import { createServer, type Server } from "node:http";
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
import { AppRouter } from "../../../src/app.js";
import { SqlClient } from "@effect/sql";

export const TEST_PORT = 8300;
export const TEST_JWT_SECRET = "test-jwt-secret-at-least-32-chars-long!!";

// Use a temp file so both schema-init and AppLayer share the same SQLite database
const TEST_DB_PATH = join(tmpdir(), `demo-be-ts-effect-unit-bdd-${process.pid}.db`);

const SqliteLayer = SqliteClient.layer({ filename: TEST_DB_PATH });

// Keep reference to HTTP server for explicit close in AfterAll
let httpServer: Server | null = null;

// Increase max body size to 20MB to allow the server to receive oversized files
// (route handlers check and reject files > MAX_ATTACHMENT_SIZE = 10MB with 413)
const MaxBodySizeLayer = Layer.succeed(HttpServerRequest.MaxBodySize, Option.some(FileSystem.Size(20 * 1024 * 1024)));

const AppLayer = HttpServer.serve(AppRouter).pipe(
  Layer.provide(
    NodeHttpServer.layer(
      () => {
        httpServer = createServer();
        return httpServer;
      },
      { port: TEST_PORT },
    ),
  ),
  Layer.provide(MaxBodySizeLayer),
  Layer.provide(UserRepositoryLive),
  Layer.provide(ExpenseRepositoryLive),
  Layer.provide(AttachmentRepositoryLive),
  Layer.provide(RevokedTokenRepositoryLive),
  Layer.provide(PasswordServiceLive),
  Layer.provide(JwtServiceLive(TEST_JWT_SECRET)),
  Layer.provide(SqliteLayer),
);

// eslint-disable-next-line @typescript-eslint/no-explicit-any
let runtime: ManagedRuntime.ManagedRuntime<never, never>;

BeforeAll(async function () {
  // Initialize schema - execute each statement individually (SQLite doesn't support multi-statement prepare)
  await Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      for (const statement of CREATE_TABLE_STATEMENTS) {
        yield* sql.unsafe(statement);
      }
    }).pipe(Effect.provide(SqliteLayer)),
  );

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  runtime = ManagedRuntime.make(AppLayer) as unknown as ManagedRuntime.ManagedRuntime<never, never>;
  // Start the server by running Effect.never (non-blocking — the server runs in background)
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (runtime as unknown as { runPromise: (effect: any) => Promise<any> }).runPromise(Effect.never).catch(() => {
    // Ignore disposal error when runtime is disposed in AfterAll
  });
  // Give the server time to fully bind the port
  await new Promise((resolve) => setTimeout(resolve, 500));
});

AfterAll(async function () {
  // Close the HTTP server first to release the port and connections
  if (httpServer) {
    // Close all connections (Node.js 18.2+)
    if (typeof httpServer.closeAllConnections === "function") {
      httpServer.closeAllConnections();
    }
    httpServer.close();
  }
  if (runtime) {
    // Best-effort disposal
    runtime.dispose().catch(() => {
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
        yield* sql.unsafe("DELETE FROM attachments");
        yield* sql.unsafe("DELETE FROM expenses");
        yield* sql.unsafe("DELETE FROM users");
      }).pipe(Effect.provide(SqliteLayer)),
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
    }).pipe(Effect.provide(SqliteLayer)),
  );
}
