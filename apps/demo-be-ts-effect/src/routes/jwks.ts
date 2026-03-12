import { HttpRouter, HttpServerResponse } from "@effect/platform";
import { Effect } from "effect";
import { JwtService } from "../auth/jwt.js";

const getJwks = Effect.gen(function* () {
  const jwt = yield* JwtService;
  const jwks = yield* jwt.getJwks();
  return yield* HttpServerResponse.json(jwks);
});

export const jwksRouter = HttpRouter.empty.pipe(HttpRouter.get("/.well-known/jwks.json", getJwks));
