import { HttpRouter, HttpServer, HttpServerResponse, HttpBody } from "@effect/platform";
import { NodeHttpServer } from "@effect/platform-node";
import { Layer, Effect } from "effect";
import { createServer } from "node:http";
import { healthRouter } from "./routes/health.js";
import { authRouter } from "./routes/auth.js";
import { userRouter } from "./routes/user.js";
import { expenseRouter } from "./routes/expense.js";
import { attachmentRouter } from "./routes/attachment.js";
import { reportRouter } from "./routes/report.js";
import { adminRouter } from "./routes/admin.js";
import { jwksRouter } from "./routes/jwks.js";
import { testApiRouter } from "./routes/test-api.js";
import {
  ValidationError,
  NotFoundError,
  UnauthorizedError,
  ForbiddenError,
  ConflictError,
  FileTooLargeError,
  UnsupportedMediaTypeError,
} from "./domain/errors.js";

export const handleDomainError = (
  error: unknown,
): Effect.Effect<HttpServerResponse.HttpServerResponse, HttpBody.HttpBodyError> => {
  if (error instanceof ValidationError) {
    return HttpServerResponse.json(
      { error: "Validation error", field: error.field, message: error.message },
      { status: 400 },
    );
  }
  if (error instanceof UnauthorizedError) {
    return HttpServerResponse.json({ error: "Unauthorized", message: error.reason }, { status: 401 });
  }
  if (error instanceof ForbiddenError) {
    return HttpServerResponse.json({ error: "Forbidden", message: error.reason }, { status: 403 });
  }
  if (error instanceof NotFoundError) {
    return HttpServerResponse.json({ error: "Not found", message: `${error.resource} not found` }, { status: 404 });
  }
  if (error instanceof ConflictError) {
    return HttpServerResponse.json({ error: "Conflict", message: error.message }, { status: 409 });
  }
  if (error instanceof FileTooLargeError) {
    return HttpServerResponse.json(
      { error: "File too large", message: "File exceeds maximum allowed size" },
      { status: 413 },
    );
  }
  if (error instanceof UnsupportedMediaTypeError) {
    return HttpServerResponse.json(
      { error: "Unsupported media type", message: "File type not allowed" },
      { status: 415 },
    );
  }
  return HttpServerResponse.json({ error: "Internal server error" }, { status: 500 });
};

const baseRouter = HttpRouter.empty.pipe(
  HttpRouter.concat(healthRouter),
  HttpRouter.concat(authRouter),
  HttpRouter.concat(userRouter),
  HttpRouter.concat(expenseRouter),
  HttpRouter.concat(attachmentRouter),
  HttpRouter.concat(reportRouter),
  HttpRouter.concat(adminRouter),
  HttpRouter.concat(jwksRouter),
);

export const AppRouter = (
  process.env["ENABLE_TEST_API"] === "true" ? baseRouter.pipe(HttpRouter.concat(testApiRouter)) : baseRouter
).pipe(HttpRouter.catchAll(handleDomainError));

export const makeServerLayer = (port: number) =>
  HttpServer.serve(AppRouter).pipe(Layer.provide(NodeHttpServer.layer(() => createServer(), { port })));

// Kept for backward compat — callers that also provide service layers
export const makeAppLayer = makeServerLayer;
