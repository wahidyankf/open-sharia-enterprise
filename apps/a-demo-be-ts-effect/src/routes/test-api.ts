import { HttpRouter, HttpServerResponse, HttpServerRequest } from "@effect/platform";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import { UserRepository } from "../infrastructure/db/user-repo.js";
import { NotFoundError } from "../domain/errors.js";

const resetDb = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`DELETE FROM attachments`;
  yield* sql`DELETE FROM expenses`;
  yield* sql`DELETE FROM revoked_tokens`;
  yield* sql`DELETE FROM users`;
  return yield* HttpServerResponse.json({ message: "Database reset successful" }, { status: 200 });
});

const promoteAdmin = HttpServerRequest.HttpServerRequest.pipe(
  Effect.flatMap((req) =>
    Effect.gen(function* () {
      const body = yield* req.json as Effect.Effect<Record<string, unknown>, unknown>;
      const username = (body["username"] as string | undefined) ?? "";

      const userRepo = yield* UserRepository;
      const user = yield* userRepo.findByUsername(username);
      if (!user) {
        return yield* Effect.fail(new NotFoundError({ resource: "User" }));
      }

      const sql = yield* SqlClient.SqlClient;
      const now = new Date().toISOString();
      yield* sql`UPDATE users SET role = 'ADMIN', updated_at = ${now} WHERE id = ${user.id}`;

      return yield* HttpServerResponse.json({ message: `User ${username} promoted to ADMIN` }, { status: 200 });
    }),
  ),
);

export const testApiRouter = HttpRouter.empty.pipe(
  HttpRouter.post("/api/v1/test/reset-db", resetDb),
  HttpRouter.post("/api/v1/test/promote-admin", promoteAdmin),
);
