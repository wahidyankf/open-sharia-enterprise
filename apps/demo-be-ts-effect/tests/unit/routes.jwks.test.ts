import { describe, it, expect } from "vitest";
import { Effect, Layer, Stream } from "effect";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import type { HttpServerRequest as HttpServerRequestType } from "@effect/platform/HttpServerRequest";
import { jwksRouter } from "../../src/routes/jwks.js";
import { JwtService } from "../../src/auth/jwt.js";
import type { JwtServiceApi } from "../../src/auth/jwt.js";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function makeRequest(options: { url: string; method?: string }): HttpServerRequestType {
  return {
    headers: {},
    url: options.url,
    method: options.method ?? "GET",
    json: Effect.succeed({}),
    text: Effect.succeed("{}"),
    arrayBuffer: Effect.succeed(new ArrayBuffer(0)),
    multipartStream: Stream.empty,
    multipart: Effect.succeed({}),
  } as unknown as HttpServerRequestType;
}

function makeJwtLayer(jwks: Record<string, unknown> = {}): Layer.Layer<JwtService> {
  const impl: JwtServiceApi = {
    signAccess: () => Effect.succeed("access-token"),
    signRefresh: () => Effect.succeed("refresh-token"),
    verify: () => Effect.fail({ _tag: "UnauthorizedError", reason: "Invalid token" }) as never,
    getJwks: () => Effect.succeed(jwks),
  };
  return Layer.succeed(JwtService, impl);
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
async function runRouter(
  req: HttpServerRequestType,
  layers: Layer.Layer<any>,
): Promise<{ status: number; body: Record<string, unknown> }> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const app = jwksRouter as unknown as Effect.Effect<HttpServerResponse.HttpServerResponse, any, any>;
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

describe("GET /.well-known/jwks.json", () => {
  it("returns JWKS document", async () => {
    const jwks = { keys: [{ kty: "RSA", use: "sig", kid: "key-1" }] };
    const req = makeRequest({ url: "/.well-known/jwks.json", method: "GET" });
    const { status, body } = await runRouter(req, makeJwtLayer(jwks));
    expect(status).toBe(200);
    expect(body["keys"]).toBeDefined();
  });

  it("returns empty JWKS document when no keys configured", async () => {
    const req = makeRequest({ url: "/.well-known/jwks.json", method: "GET" });
    const { status, body } = await runRouter(req, makeJwtLayer({}));
    expect(status).toBe(200);
    expect(body).toBeDefined();
  });

  it("returns 404 for unknown routes", async () => {
    const req = makeRequest({ url: "/unknown-route", method: "GET" });
    const { status } = await runRouter(req, makeJwtLayer());
    expect(status).toBe(404);
  });
});
