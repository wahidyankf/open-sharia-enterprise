import { Context, Effect, Layer } from "effect";
import { SqlClient } from "@effect/sql";
import { SqlError } from "@effect/sql/SqlError";

export interface RevokedTokenRepositoryApi {
  readonly revoke: (jti: string, userId: string) => Effect.Effect<void, SqlError>;
  readonly isRevoked: (jti: string, userId?: string, issuedAt?: number) => Effect.Effect<boolean, SqlError>;
  readonly revokeAllForUser: (userId: string) => Effect.Effect<void, SqlError>;
}

export class RevokedTokenRepository extends Context.Tag("RevokedTokenRepository")<
  RevokedTokenRepository,
  RevokedTokenRepositoryApi
>() {}

// Special prefix for user-level logout-all entries
const USER_LOGOUT_ALL_PREFIX = "__logout_all__";

export const RevokedTokenRepositoryLive = Layer.effect(
  RevokedTokenRepository,
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;

    return {
      revoke: (jti: string, userId: string) =>
        Effect.gen(function* () {
          const now = new Date().toISOString();
          yield* sql`
            INSERT OR IGNORE INTO revoked_tokens (jti, user_id, revoked_at)
            VALUES (${jti}, ${userId}, ${now})
          `;
        }),

      isRevoked: (jti: string, userId?: string, issuedAt?: number) =>
        Effect.gen(function* () {
          // Check direct JTI revocation
          const directRows = yield* sql`SELECT 1 FROM revoked_tokens WHERE jti = ${jti}`;
          if (directRows.length > 0) return true;

          // Check user-level logout-all (if userId and issuedAt provided)
          if (userId !== undefined && issuedAt !== undefined) {
            const logoutAllJti = `${USER_LOGOUT_ALL_PREFIX}${userId}`;
            const logoutRows = yield* sql`
              SELECT revoked_at FROM revoked_tokens
              WHERE jti = ${logoutAllJti}
            `;
            if (logoutRows.length > 0) {
              // eslint-disable-next-line @typescript-eslint/no-explicit-any
              const revokedAt = new Date((logoutRows[0] as any).revoked_at as string).getTime() / 1000;
              // Token issued before the logout-all timestamp is revoked
              if (issuedAt < revokedAt) return true;
            }
          }

          return false;
        }),

      revokeAllForUser: (userId: string) =>
        Effect.gen(function* () {
          const now = new Date().toISOString();
          const logoutAllJti = `${USER_LOGOUT_ALL_PREFIX}${userId}`;
          // Upsert: update revoked_at if already exists, or insert new
          yield* sql`
            INSERT INTO revoked_tokens (jti, user_id, revoked_at)
            VALUES (${logoutAllJti}, ${userId}, ${now})
            ON CONFLICT(jti) DO UPDATE SET revoked_at = ${now}
          `;
        }),
    };
  }),
);
