import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import { SignJWT } from "jose";
import type { CustomWorld } from "../world";

const JWT_SECRET = process.env.APP_JWT_SECRET ?? "test-jwt-secret-at-least-32-chars-long!!";

Given("alice's refresh token has expired", async function (this: CustomWorld) {
  const secret = new TextEncoder().encode(JWT_SECRET);
  const expired = await new SignJWT({ sub: this.userIds.get("alice"), tokenType: "refresh" })
    .setProtectedHeader({ alg: "HS256" })
    .setIssuedAt(Math.floor(Date.now() / 1000) - 7200)
    .setExpirationTime(Math.floor(Date.now() / 1000) - 3600)
    .setJti(crypto.randomUUID())
    .setIssuer("demo-fs-ts-nextjs")
    .sign(secret);
  this.tokens.set("alice_refresh", expired);
});

Given("alice has used her refresh token to get a new token pair", async function (this: CustomWorld) {
  const refreshToken = this.tokens.get("alice_refresh")!;
  this.context.originalRefresh = refreshToken;
  const resp = await this.dispatch("POST", "/api/v1/auth/refresh", { refreshToken }, null);
  const body = resp.body as { accessToken: string; refreshToken: string };
  this.tokens.set("alice_access", body.accessToken);
  this.tokens.set("alice_refresh", body.refreshToken);
});

Given("the user {string} has been deactivated", async function (this: CustomWorld, _username: string) {
  await this.dispatch("POST", "/api/v1/users/me/deactivate", null, this.getAuth("alice"));
});

Given("alice has already logged out once", async function (this: CustomWorld) {
  await this.dispatch("POST", "/api/v1/auth/logout", null, this.getAuth("alice"));
});

When(/^alice sends POST \/api\/v1\/auth\/refresh with her refresh token$/, async function (this: CustomWorld) {
  const refreshToken = this.tokens.get("alice_refresh")!;
  this.response = await this.dispatch("POST", "/api/v1/auth/refresh", { refreshToken }, null);
});

When(/^alice sends POST \/api\/v1\/auth\/refresh with her original refresh token$/, async function (this: CustomWorld) {
  this.response = await this.dispatch(
    "POST",
    "/api/v1/auth/refresh",
    { refreshToken: this.context.originalRefresh as string },
    null,
  );
});

When(/^alice sends POST \/api\/v1\/auth\/logout with her access token$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("POST", "/api/v1/auth/logout", null, this.getAuth("alice"));
});

When(/^alice sends POST \/api\/v1\/auth\/logout-all with her access token$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("POST", "/api/v1/auth/logout-all", null, this.getAuth("alice"));
});

Then("alice's access token should be invalidated", async function (this: CustomWorld) {
  const resp = await this.dispatch("GET", "/api/v1/users/me", null, this.getAuth("alice"));
  assert.strictEqual(resp.status, 401);
});
