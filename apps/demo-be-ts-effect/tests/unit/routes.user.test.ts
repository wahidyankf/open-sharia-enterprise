import { describe, it, expect } from "vitest";
import { Effect, Layer, Stream } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import type { HttpServerRequest as HttpServerRequestType } from "@effect/platform/HttpServerRequest";
import { userRouter } from "../../src/routes/user.js";
import { UserRepository } from "../../src/infrastructure/db/user-repo.js";
import { RevokedTokenRepository } from "../../src/infrastructure/db/token-repo.js";
import { PasswordService } from "../../src/infrastructure/password.js";
import { JwtService } from "../../src/auth/jwt.js";
import type { User } from "../../src/domain/user.js";
import type { UserRepositoryApi } from "../../src/infrastructure/db/user-repo.js";
import type { RevokedTokenRepositoryApi } from "../../src/infrastructure/db/token-repo.js";
import type { PasswordServiceApi } from "../../src/infrastructure/password.js";
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
    findByUsername: () => Effect.succeed(mockUser),
    findByEmail: () => Effect.succeed(null),
    findById: () => Effect.succeed(mockUser),
    updateStatus: () => Effect.succeed(undefined),
    updateDisplayName: () => Effect.succeed(undefined),
    updatePassword: () => Effect.succeed(undefined),
    incrementFailedAttempts: () => Effect.succeed(undefined),
    resetFailedAttempts: () => Effect.succeed(undefined),
    listUsers: () => Effect.succeed({ items: [], total: 0 }),
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

function makePasswordLayer(valid = true): Layer.Layer<PasswordService> {
  const impl: PasswordServiceApi = {
    hash: (password: string) => Effect.succeed(`hashed:${password}`),
    verify: () => Effect.succeed(valid),
  };
  return Layer.succeed(PasswordService, impl);
}

function makeJwtLayer(): Layer.Layer<JwtService> {
  const impl: JwtServiceApi = {
    signAccess: () => Effect.succeed("access-token"),
    signRefresh: () => Effect.succeed("refresh-token"),
    verify: (token: string) => {
      if (token === "valid-access-token") {
        return Effect.succeed({
          sub: "user-1",
          username: "alice",
          role: "USER" as const,
          jti: "jti-1",
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

function makeTestLayer(userOverrides: Partial<UserRepositoryApi> = {}, passwordValid = true) {
  return Layer.mergeAll(
    makeUserRepoLayer(userOverrides),
    makeTokenRepoLayer(),
    makePasswordLayer(passwordValid),
    makeJwtLayer(),
  );
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
async function runRouter(
  req: HttpServerRequestType,
  layers: Layer.Layer<any>,
): Promise<{ status: number; body: Record<string, unknown> }> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const app = userRouter as unknown as Effect.Effect<HttpServerResponse.HttpServerResponse, any, any>;
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
    if (err?._tag === "ValidationError") return { status: 400, body: { field: err.field, message: err.message } };
    if (err?._tag === "NotFoundError") return { status: 404, body: { message: `${err.resource} not found` } };
    if (err?._tag === "ConflictError") return { status: 409, body: { message: err.message } };
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

describe("GET /api/v1/users/me", () => {
  it("returns user profile for authenticated user", async () => {
    const req = makeRequest({
      url: "/api/v1/users/me",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["username"]).toBe("alice");
    expect(body["email"]).toBe("alice@example.com");
  });

  it("returns 401 without token", async () => {
    const req = makeRequest({ url: "/api/v1/users/me", method: "GET" });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns 401 for non-active user", async () => {
    const inactiveUser = { ...mockUser, status: "INACTIVE" as const };
    const req = makeRequest({
      url: "/api/v1/users/me",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(inactiveUser) }));
    expect(status).toBe(401);
  });

  it("returns 404 when user not found", async () => {
    const req = makeRequest({
      url: "/api/v1/users/me",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });
});

describe("PATCH /api/v1/users/me", () => {
  it("updates display name", async () => {
    const updatedUser = { ...mockUser, displayName: "Alice Smith" };
    const req = makeRequest({
      url: "/api/v1/users/me",
      method: "PATCH",
      headers: { authorization: "Bearer valid-access-token" },
      body: { display_name: "Alice Smith" },
    });
    const { status, body } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(updatedUser) }));
    expect(status).toBe(200);
    expect(body["display_name"]).toBe("Alice Smith");
  });

  it("returns 401 without token", async () => {
    const req = makeRequest({ url: "/api/v1/users/me", method: "PATCH" });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns 404 when user not found", async () => {
    const req = makeRequest({
      url: "/api/v1/users/me",
      method: "PATCH",
      headers: { authorization: "Bearer valid-access-token" },
      body: { display_name: "Alice" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 404 when user disappears after display name update", async () => {
    let callCount = 0;
    const req = makeRequest({
      url: "/api/v1/users/me",
      method: "PATCH",
      headers: { authorization: "Bearer valid-access-token" },
      body: { display_name: "Alice Smith" },
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

describe("POST /api/v1/users/me/password", () => {
  it("changes password successfully", async () => {
    const req = makeRequest({
      url: "/api/v1/users/me/password",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: { old_password: "OldPass", new_password: "NewPass" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
  });

  it("returns 401 with wrong old password", async () => {
    const req = makeRequest({
      url: "/api/v1/users/me/password",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: { old_password: "WrongOld", new_password: "NewPass" },
    });
    const { status } = await runRouter(req, makeTestLayer({}, false));
    expect(status).toBe(401);
  });

  it("returns 404 when user not found", async () => {
    const req = makeRequest({
      url: "/api/v1/users/me/password",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: { old_password: "OldPass", new_password: "NewPass" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });
});

describe("POST /api/v1/users/me/deactivate", () => {
  it("deactivates the user account", async () => {
    const req = makeRequest({
      url: "/api/v1/users/me/deactivate",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
  });

  it("returns 401 without token", async () => {
    const req = makeRequest({ url: "/api/v1/users/me/deactivate", method: "POST" });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns 404 when user not found", async () => {
    const req = makeRequest({
      url: "/api/v1/users/me/deactivate",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });
});
