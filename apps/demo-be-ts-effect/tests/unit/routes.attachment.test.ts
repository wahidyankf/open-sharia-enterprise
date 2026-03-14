import { describe, it, expect } from "vitest";
import { Effect, Layer, Stream } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import type { HttpServerRequest as HttpServerRequestType } from "@effect/platform/HttpServerRequest";
import type { Part } from "@effect/platform/Multipart";
import { attachmentRouter } from "../../src/routes/attachment.js";
import { ExpenseRepository } from "../../src/infrastructure/db/expense-repo.js";
import { AttachmentRepository } from "../../src/infrastructure/db/attachment-repo.js";
import { RevokedTokenRepository } from "../../src/infrastructure/db/token-repo.js";
import { JwtService } from "../../src/auth/jwt.js";
import type { Expense } from "../../src/domain/expense.js";
import type { Attachment } from "../../src/domain/attachment.js";
import type { ExpenseRepositoryApi } from "../../src/infrastructure/db/expense-repo.js";
import type { AttachmentRepositoryApi } from "../../src/infrastructure/db/attachment-repo.js";
import type { RevokedTokenRepositoryApi } from "../../src/infrastructure/db/token-repo.js";
import type { JwtServiceApi } from "../../src/auth/jwt.js";

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

const mockAttachment: Attachment = {
  id: "attachment-1",
  expenseId: "expense-1",
  userId: "user-1",
  filename: "receipt.jpg",
  contentType: "image/jpeg",
  size: 1024,
  data: Buffer.from("fake image data"),
  createdAt: new Date("2024-01-15"),
};

const MultipartTypeId = Symbol.for("@effect/platform/Multipart");

function makeFilePart(filename: string, contentType: string, content: Buffer): Part {
  return {
    _tag: "File",
    key: "file",
    name: filename,
    contentType,
    content: Stream.fromIterable([content]) as Stream.Stream<Uint8Array, never>,
    contentEffect: Effect.succeed(content) as Effect.Effect<Uint8Array, never>,
    [MultipartTypeId]: MultipartTypeId,
    toJSON: () => ({}),
    toString: () => "FilePart",
    inspect: () => "FilePart",
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
  } as unknown as Part;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function makeRequest(options: {
  url: string;
  method?: string;
  headers?: Record<string, string>;
  multipartStream?: Stream.Stream<Part, any>;
}): HttpServerRequestType {
  const headers = options.headers ?? {};
  return {
    headers,
    url: options.url,
    method: options.method ?? "GET",
    json: Effect.succeed({}),
    text: Effect.succeed("{}"),
    arrayBuffer: Effect.succeed(new ArrayBuffer(0)),
    multipartStream: options.multipartStream ?? Stream.empty,
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
    summarize: () => Effect.succeed({}),
    findByDateRange: () => Effect.succeed([mockExpense]),
    findByDateRangeGroupedByCategory: () => Effect.succeed([]),
  };
  return Layer.succeed(ExpenseRepository, { ...base, ...overrides });
}

function makeAttachmentRepoLayer(overrides: Partial<AttachmentRepositoryApi> = {}): Layer.Layer<AttachmentRepository> {
  const base: AttachmentRepositoryApi = {
    create: () => Effect.succeed(mockAttachment),
    findByExpenseId: () => Effect.succeed([mockAttachment]),
    findById: () => Effect.succeed(mockAttachment),
    delete: () => Effect.succeed(undefined),
  };
  return Layer.succeed(AttachmentRepository, { ...base, ...overrides });
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

function makeTestLayer(
  expenseOverrides: Partial<ExpenseRepositoryApi> = {},
  attachmentOverrides: Partial<AttachmentRepositoryApi> = {},
) {
  return Layer.mergeAll(
    makeExpenseRepoLayer(expenseOverrides),
    makeAttachmentRepoLayer(attachmentOverrides),
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
  const app = attachmentRouter as unknown as Effect.Effect<HttpServerResponse.HttpServerResponse, any, any>;
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
    if (err?._tag === "NotFoundError") return { status: 404, body: { message: `${err.resource} not found` } };
    if (err?._tag === "ForbiddenError") return { status: 403, body: { message: err.reason } };
    if (err?._tag === "UnsupportedMediaTypeError") return { status: 415, body: {} };
    if (err?._tag === "FileTooLargeError") return { status: 413, body: {} };
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

describe("POST /api/v1/expenses/:expenseId/attachments", () => {
  it("uploads an attachment and returns 201", async () => {
    const filePart = makeFilePart("receipt.jpg", "image/jpeg", Buffer.from("fake image"));
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.make(filePart),
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(201);
    expect(body["filename"]).toBeDefined();
    expect(body["contentType"]).toBeDefined();
  });

  it("uploads a PNG attachment", async () => {
    const filePart = makeFilePart("photo.png", "image/png", Buffer.from("png data"));
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.make(filePart),
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(201);
  });

  it("uploads a PDF attachment", async () => {
    const filePart = makeFilePart("receipt.pdf", "application/pdf", Buffer.from("pdf data"));
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.make(filePart),
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(201);
  });

  it("returns 401 without auth token", async () => {
    const filePart = makeFilePart("receipt.jpg", "image/jpeg", Buffer.from("data"));
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      multipartStream: Stream.make(filePart),
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns 404 when expense not found", async () => {
    const filePart = makeFilePart("receipt.jpg", "image/jpeg", Buffer.from("data"));
    const req = makeRequest({
      url: "/api/v1/expenses/nonexistent/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.make(filePart),
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 403 when expense belongs to another user", async () => {
    const otherExpense = { ...mockExpense, userId: "other-user" };
    const filePart = makeFilePart("receipt.jpg", "image/jpeg", Buffer.from("data"));
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.make(filePart),
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(otherExpense) }));
    expect(status).toBe(403);
  });

  it("returns 415 when no file is provided", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.empty,
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(415);
  });

  it("returns 415 for unsupported content type", async () => {
    const filePart = makeFilePart("file.txt", "text/plain", Buffer.from("text data"));
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.make(filePart),
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(415);
  });

  it("returns 413 when file is too large", async () => {
    const largeBuffer = Buffer.alloc(11 * 1024 * 1024, 0); // 11MB
    const filePart = makeFilePart("large.jpg", "image/jpeg", largeBuffer);
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.make(filePart),
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(413);
  });

  it("uses default filename when part name is empty", async () => {
    const filePart = makeFilePart("", "image/jpeg", Buffer.from("data"));
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "POST",
      headers: { authorization: "Bearer valid-access-token" },
      multipartStream: Stream.make(filePart),
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(201);
    expect(body["filename"]).toBeDefined();
  });
});

describe("GET /api/v1/expenses/:expenseId/attachments", () => {
  it("lists attachments for an expense", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status, body } = await runRouter(req, makeTestLayer());
    expect(status).toBe(200);
    expect(body["attachments"]).toBeDefined();
  });

  it("returns 401 without auth token", async () => {
    const req = makeRequest({ url: "/api/v1/expenses/expense-1/attachments", method: "GET" });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns 404 when expense not found", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/nonexistent/attachments",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 403 when expense belongs to another user", async () => {
    const otherExpense = { ...mockExpense, userId: "other-user" };
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments",
      method: "GET",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(otherExpense) }));
    expect(status).toBe(403);
  });
});

describe("DELETE /api/v1/expenses/:expenseId/attachments/:attachmentId", () => {
  it("deletes an attachment and returns 204", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments/attachment-1",
      method: "DELETE",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(204);
  });

  it("returns 401 without auth token", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments/attachment-1",
      method: "DELETE",
    });
    const { status } = await runRouter(req, makeTestLayer());
    expect(status).toBe(401);
  });

  it("returns 404 when expense not found", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/nonexistent/attachments/attachment-1",
      method: "DELETE",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 403 when expense belongs to another user", async () => {
    const otherExpense = { ...mockExpense, userId: "other-user" };
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments/attachment-1",
      method: "DELETE",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({ findById: () => Effect.succeed(otherExpense) }));
    expect(status).toBe(403);
  });

  it("returns 404 when attachment not found", async () => {
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments/nonexistent",
      method: "DELETE",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({}, { findById: () => Effect.succeed(null) }));
    expect(status).toBe(404);
  });

  it("returns 403 when attachment belongs to another user", async () => {
    const otherAttachment = { ...mockAttachment, userId: "other-user" };
    const req = makeRequest({
      url: "/api/v1/expenses/expense-1/attachments/attachment-1",
      method: "DELETE",
      headers: { authorization: "Bearer valid-access-token" },
    });
    const { status } = await runRouter(req, makeTestLayer({}, { findById: () => Effect.succeed(otherAttachment) }));
    expect(status).toBe(403);
  });
});
