import { describe, it, expect } from "vitest";
import { Effect, Layer, Stream } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import type { HttpServerRequest as HttpServerRequestType } from "@effect/platform/HttpServerRequest";
import { adminRouter } from "../../src/routes/admin.js";
import { UserRepository } from "../../src/infrastructure/db/user-repo.js";
import { RevokedTokenRepository } from "../../src/infrastructure/db/token-repo.js";
import { JwtService } from "../../src/auth/jwt.js";
import type { User } from "../../src/domain/user.js";
import type { UserRepositoryApi } from "../../src/infrastructure/db/user-repo.js";
import type { RevokedTokenRepositoryApi } from "../../src/infrastructure/db/token-repo.js";
import type { JwtServiceApi } from "../../src/auth/jwt.js";

const mockUser: User = {
  id: "user-1",
  username: "alice",
  email: "alice@example.com",
  passwordHash: "hashed",
  displayName: "alice",
  role: "USER",
  status: "ACTIVE",
  failedLoginAttempts: 0,
  createdAt: new Date("2024-01-01"),
  updatedAt: new Date("2024-01-01"),
};

const mockAdmin: User = {
  id: "admin-1",
  username: "superadmin",
  email: "superadmin@example.com",
  passwordHash: "hashed",
  displayName: "superadmin",
  role: "ADMIN",
  status: "ACTIVE",
  failedLoginAttempts: 0,
  createdAt: new Date("2024-01-01"),
  updatedAt: new Date("2024-01-01"),
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function makeRequest(options: {
  url: string;
  method?: string;
  headers?: Record<string, string>;
  body?: any;
}): HttpServerRequestType {
  const headers = options.headers ?? {};
  return {
    headers,
    url: options.url,
    method: options.method ?? "GET",
    json: Effect.succeed(options.body ?? {}),
    text: Effect.succeed(JSON.stringify(options.body ?? {})),
    arrayBuffer: Effect.succeed(new ArrayBuffer(0)),
    multipartStream: Stream.empty,
    multipart: Effect.succeed({}),
  } as unknown as HttpServerRequestType;
}

function makeUserRepoLayer(overrides: Partial<UserRepositoryApi> = {}): Layer.Layer<UserRepository> {
  const base: UserRepositoryApi = {
    create: () => Effect.succeed(mockUser),
    findByUsername: () => Effect.succeed(mockAdmin),
    findByEmail: () => Effect.succeed(null),
    findById: () => Effect.succeed(mockUser),
    updateStatus: () => Effect.succeed(undefined),
    updateDisplayName: () => Effect.succeed(undefined),
    updatePassword: () => Effect.succeed(undefined),
    incrementFailedAttempts: () => Effect.succeed(undefined),
    resetFailedAttempts: () => Effect.succeed(undefined),
    listUsers: () => Effect.succeed({ items: [mockUser], total: 1 }),
  };
  return Layer.succeed(UserRepository, { ...base, ...overrides });
}

function makeTokenRepoLayer(): Layer.Layer<RevokedTokenRepository> {
  const impl: RevokedTokenRepositoryApi = {
    revoke: () => Effect.succeed(undefined),
    isRevoked: () => Effect.succeed(false),
    revokeAllForUser: () => Effect.succeed(undefined),
  };
  return Layer.succeed(RevokedTokenRepository, impl);
}

function makeJwtLayer(): Layer.Layer<JwtService> {
  const impl: JwtServiceApi = {
    signAccess: () => Effect.succeed("admin-access-token"),
    signRefresh: () => Effect.succeed("refresh-token"),
    verify: (token: string) => {
      if (token === "valid-admin-token") {
        return Effect.succeed({
          sub: "admin-1",
          username: "superadmin",
          role: "ADMIN" as const,
          jti: "jti-admin",
          tokenType: "access" as const,
        });
      }
      if (token === "valid-user-token") {
        return Effect.succeed({
          sub: "user-1",
          username: "alice",
          role: "USER" as const,
          jti: "jti-user",
          tokenType: "access" as const,
        });
      }
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      return Effect.fail({ _tag: "UnauthorizedError", reason: "Invalid token" }) as any;
    },
    getJwks: () => Effect.succeed({}),
  };
  return Layer.succeed(JwtService, impl);
}

function makeTestLayer(userOverrides: Partial<UserRepositoryApi> = {}) {
  return Layer.mergeAll(makeUserRepoLayer(userOverrides), makeTokenRepoLayer(), makeJwtLayer());
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
async function runRouter(
  req: HttpServerRequestType,
  layers: Layer.Layer<any>,
): Promise<{ status: number; body: Record<string, unknown> }> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const app = adminRouter as unknown as Effect.Effect<HttpServerResponse.HttpServerResponse, any, any>;
  const result = await Effect.runPromise(
    Effect.either(
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      (app as any).pipe(
        Effect.provideService(HttpServerRequest.HttpServerRequest, req),
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        Effect.provide(layers as any),
      ),
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    ) as any,
  );
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  if ((result as any)._tag === "Left") {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const err = (result as any).left;
    if (err?._tag === "RouteNotFound") return { status: 404, body: {} };
    if (err?._tag === "UnauthorizedError") return { status: 401, body: { message: err.reason } };
    if (err?._tag === "ForbiddenError") return { status: 403, body: { message: err.reason } };
    if (err?._tag === "NotFoundError") return { status: 404, body: { message: `${err.resource} not found` } };
    return { status: 500, body: {} };
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const response = (result as any).right;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const bodyObj = (response as unknown as { body: { body?: Uint8Array } }).body;
  let body: Record<string, unknown> = {};
  if (bodyObj?.body instanceof Uint8Array) {
    body = JSON.parse(Buffer.from(bodyObj.body).toString("utf-8")) as Record<string, unknown>;
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return { status: (response as any).status as number, body };
}

describe("GET /api/v1/admin/users", () => {
  it("returns user list for admin", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users",
      method: "GET",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["data"]).toBeDefined();
    expect(body["total"]).toBeDefined();
  });

  it("returns 403 for non-admin user", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users",
      method: "GET",
      headers: { authorization: "Bearer valid-user-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(403);
  });

  it("returns 401 without token", async () => {
    const req = makeRequest({ url: "/api/v1/admin/users", method: "GET" });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns filtered users by email", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users?email=alice@example.com",
      method: "GET",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["data"]).toBeDefined();
  });
});

describe("POST /api/v1/admin/users/:userId/disable", () => {
  it("disables a user account", async () => {
    const disabledUser = { ...mockUser, status: "DISABLED" as const };
    let count = 0;
    const req = makeRequest({
      url: "/api/v1/admin/users/user-1/disable",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
      body: { reason: "Policy violation" },
    });
    const { status, body } = await runRouter(
      req,
      makeTestLayer({
        findById: () => {
          count++;
          return count > 1 ? Effect.succeed(disabledUser) : Effect.succeed(mockUser);
        },
      }),
    );
    expect(status).toBe(200);
    expect(body["status"]).toBe("DISABLED");
  });

  it("returns 404 when user not found", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users/nonexistent/disable",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
      body: { reason: "Policy violation" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 403 for non-admin", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users/user-1/disable",
      method: "POST",
      headers: { authorization: "Bearer valid-user-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(403);
  });
});

describe("POST /api/v1/admin/users/:userId/enable", () => {
  it("enables a disabled user account", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users/user-1/enable",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
  });

  it("returns 404 when user not found", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users/nonexistent/enable",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 404 when user disappears after enable", async () => {
    let callCount = 0;
    const req = makeRequest({
      url: "/api/v1/admin/users/user-1/enable",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status } = await runRouter(
      req,
      makeTestLayer({
        findById: () => {
          callCount++;
          return callCount === 1 ? Effect.succeed(mockUser) : Effect.succeed(null);
        },
      }),
    );
    expect(status).toBe(404);
  });
});

describe("POST /api/v1/admin/users/:userId/unlock", () => {
  it("unlocks a locked user account", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users/user-1/unlock",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
  });

  it("returns 404 when user not found", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users/nonexistent/unlock",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 404 when user disappears after unlock", async () => {
    let callCount = 0;
    const req = makeRequest({
      url: "/api/v1/admin/users/user-1/unlock",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status } = await runRouter(
      req,
      makeTestLayer({
        findById: () => {
          callCount++;
          return callCount === 1 ? Effect.succeed(mockUser) : Effect.succeed(null);
        },
      }),
    );
    expect(status).toBe(404);
  });
});

describe("POST /api/v1/admin/users/:userId/force-password-reset", () => {
  it("generates a reset token", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users/user-1/force-password-reset",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["reset_token"]).toBeDefined();
  });

  it("returns 404 when user not found", async () => {
    const req = makeRequest({
      url: "/api/v1/admin/users/nonexistent/force-password-reset",
      method: "POST",
      headers: { authorization: "Bearer valid-admin-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });
});
