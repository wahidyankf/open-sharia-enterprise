import { describe, it, expect } from "vitest";
import { Effect, Layer, Stream } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import type { HttpServerRequest as HttpServerRequestType } from "@effect/platform/HttpServerRequest";
import { reportRouter } from "../../src/routes/report.js";
import { ExpenseRepository } from "../../src/infrastructure/db/expense-repo.js";
import { RevokedTokenRepository } from "../../src/infrastructure/db/token-repo.js";
import { JwtService } from "../../src/auth/jwt.js";
import type { Expense } from "../../src/domain/expense.js";
import type { ExpenseRepositoryApi } from "../../src/infrastructure/db/expense-repo.js";
import type { RevokedTokenRepositoryApi } from "../../src/infrastructure/db/token-repo.js";
import type { JwtServiceApi } from "../../src/auth/jwt.js";

const mockExpense: Expense = {
  id: "expense-1",
  userId: "user-1",
  type: "INCOME",
  amount: 5000.0,
  currency: "USD",
  category: "salary",
  description: "Monthly salary",
  quantity: null,
  unit: null,
  date: "2025-01-15",
  createdAt: new Date("2025-01-15"),
  updatedAt: new Date("2025-01-15"),
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
    findByUserId: () => Effect.succeed({ items: [], total: 0 }),
    update: () => Effect.succeed(mockExpense),
    delete: () => Effect.succeed(undefined),
    summarize: () => Effect.succeed({}),
    findByDateRange: () => Effect.succeed([mockExpense]),
    findByDateRangeGroupedByCategory: () => Effect.succeed([{ category: "salary", total: 5000 }]),
  };
  return Layer.succeed(ExpenseRepository, { ...base, ...overrides });
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
  return Layer.mergeAll(makeExpenseRepoLayer(expenseOverrides), makeTokenRepoLayer(), makeJwtLayer());
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
async function runRouter(
  req: HttpServerRequestType,
  layers: Layer.Layer<any>,
): Promise<{ status: number; body: Record<string, unknown> }> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const app = reportRouter as unknown as Effect.Effect<HttpServerResponse.HttpServerResponse, any, any>;
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

describe("GET /api/v1/reports/pl", () => {
  it("returns P&L report for valid parameters", async () => {
    const req = makeRequest({
      url: "/api/v1/reports/pl?from=2025-01-01&to=2025-01-31&currency=USD",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["totalIncome"]).toBeDefined();
    expect(body["totalExpense"]).toBeDefined();
    expect(body["net"]).toBeDefined();
  });

  it("returns 401 without auth token", async () => {
    const req = makeRequest({
      url: "/api/v1/reports/pl?from=2025-01-01&to=2025-01-31&currency=USD",
      method: "GET",
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns 400 when from is missing", async () => {
    const req = makeRequest({
      url: "/api/v1/reports/pl?to=2025-01-31&currency=USD",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(400);
  });

  it("returns 400 when to is missing", async () => {
    const req = makeRequest({
      url: "/api/v1/reports/pl?from=2025-01-01&currency=USD",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(400);
  });

  it("returns 400 when currency is missing", async () => {
    const req = makeRequest({
      url: "/api/v1/reports/pl?from=2025-01-01&to=2025-01-31",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(400);
  });

  it("returns zero totals when no expenses in range", async () => {
    const req = makeRequest({
      url: "/api/v1/reports/pl?from=2099-01-01&to=2099-01-31&currency=USD",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer({ findByDateRange: () => Effect.succeed([]) }));
    expect(status).toBe(200);
    expect(body["totalIncome"]).toBe("0.00");
    expect(body["totalExpense"]).toBe("0.00");
    expect(body["net"]).toBe("0.00");
  });

  it("correctly separates income and expense totals", async () => {
    const entries = [
      { ...mockExpense, type: "INCOME" as const, amount: 5000, category: "salary" },
      { ...mockExpense, type: "EXPENSE" as const, amount: 150, category: "food" },
    ];
    const req = makeRequest({
      url: "/api/v1/reports/pl?from=2025-01-01&to=2025-01-31&currency=USD",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer({ findByDateRange: () => Effect.succeed(entries) }));
    expect(status).toBe(200);
    expect(body["totalIncome"]).toBe("5000.00");
    expect(body["totalExpense"]).toBe("150.00");
    expect(body["net"]).toBe("4850.00");
  });

  it("returns P&L report for unsupported currency (fallback toFixed(2))", async () => {
    const entries = [
      { ...mockExpense, type: "INCOME" as const, amount: 100.5, currency: "EUR", category: "salary" },
      { ...mockExpense, type: "EXPENSE" as const, amount: 50.25, currency: "EUR", category: "food" },
    ];
    const req = makeRequest({
      url: "/api/v1/reports/pl?from=2025-01-01&to=2025-01-31&currency=EUR",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer({ findByDateRange: () => Effect.succeed(entries) }));
    expect(status).toBe(200);
    expect(body["totalIncome"]).toBe("100.50");
    expect(body["totalExpense"]).toBe("50.25");
  });

  it("includes income and expense breakdown by category", async () => {
    const entries = [
      { ...mockExpense, type: "INCOME" as const, amount: 3000, category: "salary" },
      { ...mockExpense, type: "INCOME" as const, amount: 500, category: "freelance" },
      { ...mockExpense, type: "EXPENSE" as const, amount: 200, category: "transport" },
    ];
    const req = makeRequest({
      url: "/api/v1/reports/pl?from=2025-02-01&to=2025-02-28&currency=USD",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer({ findByDateRange: () => Effect.succeed(entries) }));
    expect(status).toBe(200);
    const incomeBreakdown = body["income_breakdown"] as Record<string, string>;
    expect(incomeBreakdown["salary"]).toBe("3000.00");
    expect(incomeBreakdown["freelance"]).toBe("500.00");
    const expenseBreakdown = body["expense_breakdown"] as Record<string, string>;
    expect(expenseBreakdown["transport"]).toBe("200.00");
  });
});
