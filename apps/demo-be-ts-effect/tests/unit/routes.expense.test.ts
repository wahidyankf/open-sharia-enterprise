import { describe, it, expect } from "vitest";
import { Effect, Layer, Stream } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import type { HttpServerRequest as HttpServerRequestType } from "@effect/platform/HttpServerRequest";
import { expenseRouter } from "../../src/routes/expense.js";
import { ExpenseRepository } from "../../src/infrastructure/db/expense-repo.js";
import { UserRepository } from "../../src/infrastructure/db/user-repo.js";
import { RevokedTokenRepository } from "../../src/infrastructure/db/token-repo.js";
import { JwtService } from "../../src/auth/jwt.js";
import type { Expense } from "../../src/domain/expense.js";
import type { ExpenseRepositoryApi } from "../../src/infrastructure/db/expense-repo.js";
import type { UserRepositoryApi } from "../../src/infrastructure/db/user-repo.js";
import type { RevokedTokenRepositoryApi } from "../../src/infrastructure/db/token-repo.js";
import type { JwtServiceApi } from "../../src/auth/jwt.js";
import type { User } from "../../src/domain/user.js";

const mockExpense: Expense = {
  id: "expense-1",
  userId: "user-1",
  type: "EXPENSE",
  amount: 25.0,
  currency: "USD",
  category: "food",
  description: "Lunch",
  quantity: null,
  unit: null,
  date: "2024-01-15",
  createdAt: new Date("2024-01-15"),
  updatedAt: new Date("2024-01-15"),
};

const mockUser: User = {
  id: "user-1",
  username: "alice",
  email: "alice@example.com",
  passwordHash: "hashed",
  displayName: "alice",
  role: "USER",
  status: "ACTIVE",
  failedLoginAttempts: 0,
  createdAt: new Date(),
  updatedAt: new Date(),
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

function makeExpenseRepoLayer(overrides: Partial<ExpenseRepositoryApi> = {}): Layer.Layer<ExpenseRepository> {
  const base: ExpenseRepositoryApi = {
    create: () => Effect.succeed(mockExpense),
    findById: () => Effect.succeed(mockExpense),
    findByUserId: () => Effect.succeed({ items: [mockExpense], total: 1 }),
    update: () => Effect.succeed(mockExpense),
    delete: () => Effect.succeed(undefined),
    summarize: () => Effect.succeed({ USD: { income: 100, expense: 40 } }),
    findByDateRange: () => Effect.succeed([mockExpense]),
    findByDateRangeGroupedByCategory: () => Effect.succeed([{ category: "food", total: 25 }]),
  };
  return Layer.succeed(ExpenseRepository, { ...base, ...overrides });
}

function makeUserRepoLayer(): Layer.Layer<UserRepository> {
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
  return Layer.succeed(UserRepository, base);
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

function makeTestLayer(expenseOverrides: Partial<ExpenseRepositoryApi> = {}) {
  return Layer.mergeAll(
    makeExpenseRepoLayer(expenseOverrides),
    makeUserRepoLayer(),
    makeTokenRepoLayer(),
    makeJwtLayer(),
  );
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
async function runRouter(
  req: HttpServerRequestType,
  layers: Layer.Layer<any>,
): Promise<{ status: number; body: Record<string, unknown> }> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const app = expenseRouter as unknown as Effect.Effect<HttpServerResponse.HttpServerResponse, any, any>;
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
    if (err?._tag === "ForbiddenError") return { status: 403, body: { message: err.reason } };
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

describe("POST /api/v1/expenses", () => {
  it("creates an expense and returns 201", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: {
        amount: "25.00",
        currency: "USD",
        category: "food",
        description: "Lunch",
        date: "2024-01-15",
        type: "expense",
      },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(201);
    expect(body["id"]).toBeDefined();
  });

  it("returns 401 without auth token", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      body: { amount: "25.00", currency: "USD", description: "Lunch", date: "2024-01-15", type: "expense" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns 400 for missing description", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: { amount: "25.00", currency: "USD", description: "", date: "2024-01-15", type: "expense" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(400);
  });

  it("returns 400 for missing date", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: { amount: "25.00", currency: "USD", description: "Lunch", date: "", type: "expense" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(400);
  });

  it("returns 400 for invalid amount (NaN)", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: { amount: "notanumber", currency: "USD", description: "Lunch", date: "2024-01-15", type: "expense" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(400);
  });

  it("returns 400 for unsupported currency", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: { amount: "10.00", currency: "EUR", description: "Lunch", date: "2024-01-15", type: "expense" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(400);
  });

  it("returns 400 for invalid unit", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: {
        amount: "10.00",
        currency: "USD",
        description: "Lunch",
        date: "2024-01-15",
        type: "expense",
        unit: "barrel",
      },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(400);
  });

  it("creates an income entry", async () => {
    const incomeExpense = { ...mockExpense, type: "INCOME" as const };
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: {
        amount: "1000.00",
        currency: "USD",
        category: "salary",
        description: "Salary",
        date: "2024-01-31",
        type: "income",
      },
    });
    const { status } = await runRouter(req, makeTestLayer({ create: () => Effect.succeed(incomeExpense) }));
    expect(status).toBe(201);
  });
});

describe("GET /api/v1/expenses", () => {
  it("lists expenses with pagination", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["data"]).toBeDefined();
    expect(body["total"]).toBeDefined();
    expect(body["page"]).toBeDefined();
  });

  it("returns 401 without token", async () => {
    const req = makeRequest({ url: "/api/v1/expenses", method: "GET" });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });
});

describe("GET /api/v1/expenses/summary", () => {
  it("returns expense summary", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/summary",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["USD"]).toBeDefined();
  });

  it("returns 401 without token", async () => {
    const req = makeRequest({ url: "/api/v1/expenses/summary", method: "GET" });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });
});

describe("GET /api/v1/expenses/:id", () => {
  it("returns expense by id", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["id"]).toBeDefined();
  });

  it("returns 404 for not found expense", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/nonexistent",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 403 for other user's expense", async () => {
    const otherExpense = { ...mockExpense, userId: "other-user" };
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(otherExpense) }));
    expect(status).toBe(403);
  });

  it("returns 401 without token", async () => {
    const req = makeRequest({ url: "/api/v1/expenses/expense-1", method: "GET" });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });
});

describe("PUT /api/v1/expenses/:id", () => {
  it("updates expense and returns 200", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "PUT",
      headers: { authorization: "Bearer valid-access-token" },
      body: { amount: "30.00", currency: "USD", description: "Updated lunch", date: "2024-01-15", type: "expense" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
  });

  it("returns 404 for not found expense on update", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/nonexistent",
      method: "PUT",
      headers: { authorization: "Bearer valid-access-token" },
      body: { amount: "30.00", currency: "USD", description: "Updated lunch", date: "2024-01-15", type: "expense" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 403 for other user's expense on update", async () => {
    const otherExpense = { ...mockExpense, userId: "other-user" };
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "PUT",
      headers: { authorization: "Bearer valid-access-token" },
      body: { amount: "30.00", currency: "USD" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(otherExpense) }));
    expect(status).toBe(403);
  });
});

describe("DELETE /api/v1/expenses/:id", () => {
  it("deletes expense and returns 204", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "DELETE",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(204);
  });

  it("returns 404 for not found expense on delete", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/nonexistent",
      method: "DELETE",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 403 for other user's expense on delete", async () => {
    const otherExpense = { ...mockExpense, userId: "other-user" };
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "DELETE",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(otherExpense) }));
    expect(status).toBe(403);
  });
});

describe("formatAmount branch coverage", () => {
  it("summary with unsupported currency formats as toString", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/summary",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    // Use unsupported currency EUR in summary to hit the fallback branch in formatAmount
    const { status } = await runRouter(
      req,
      makeTestLayer({
        summarize: () => Effect.succeed({ EUR: { income: 100.5, expense: 50.25 } }),
      }),
    );
    expect(status).toBe(200);
  });

  it("updates expense with only description (no amount/currency — fallback branch)", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "PUT",
      headers: { authorization: "Bearer valid-access-token" },
      body: { description: "New description" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
  });

  it("updates expense with amount as numeric (not string)", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "PUT",
      headers: { authorization: "Bearer valid-access-token" },
      body: { amount: 30, currency: "USD" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
  });

  it("updates expense with empty unit string (skips validateUnit branch)", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "PUT",
      headers: { authorization: "Bearer valid-access-token" },
      body: { description: "Updated", unit: "" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
  });

  it("creates expense with quantity and unit", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      body: {
        amount: "10.00",
        currency: "USD",
        description: "Fuel",
        date: "2024-01-15",
        type: "expense",
        quantity: "2",
        unit: "liter",
      },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(201);
  });

  it("returns expense with IDR currency (0 decimal places)", async () => {
    const idrExpense = { ...mockExpense, currency: "IDR", amount: 150000 };
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(idrExpense) }));
    expect(status).toBe(200);
    expect(body["amount"]).toBe("150000");
  });
});
