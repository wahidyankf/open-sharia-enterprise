import { eq } from "drizzle-orm";
import type { Database } from "@/db/client";
import { refreshTokens, revokedTokens } from "@/db/schema";
import type { SessionRepository } from "./interfaces";

export function createSessionRepository(db: Database): SessionRepository {
  return {
    async createRefreshToken(data) {
      const [row] = await db
        .insert(refreshTokens)
        .values({
          userId: data.userId,
          tokenHash: data.tokenHash,
          expiresAt: data.expiresAt,
        })
        .returning();
      return row!;
    },

    async findRefreshTokenByHash(hash) {
      const [row] = await db.select().from(refreshTokens).where(eq(refreshTokens.tokenHash, hash));
      return row ?? null;
    },

    async revokeRefreshToken(id) {
      await db.update(refreshTokens).set({ revoked: true }).where(eq(refreshTokens.id, id));
    },

    async revokeAllUserTokens(userId) {
      await db.update(refreshTokens).set({ revoked: true }).where(eq(refreshTokens.userId, userId));
    },

    async revokeAccessToken(jti, userId) {
      await db.insert(revokedTokens).values({ jti, userId }).onConflictDoNothing();
    },

    async isAccessTokenRevoked(jti) {
      const [row] = await db.select().from(revokedTokens).where(eq(revokedTokens.jti, jti));
      return !!row;
    },

    async deleteAll() {
      await db.delete(revokedTokens);
      await db.delete(refreshTokens);
    },
  };
}
