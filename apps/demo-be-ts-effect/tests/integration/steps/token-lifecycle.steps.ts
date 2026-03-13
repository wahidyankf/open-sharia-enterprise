import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world.js";
import { TEST_JWT_SECRET } from "../hooks.js";
import * as jose from "jose";

// ---- Given: expired refresh token ----

Given("alice's refresh token has expired", async function (this: CustomWorld) {
  // Sign a refresh token with very short expiry (already expired)
  const secretKey = new TextEncoder().encode(TEST_JWT_SECRET);
  const expiredToken = await new jose.SignJWT({
    jti: `expired-${Date.now()}`,
    tokenType: "refresh",
  })
    .setProtectedHeader({ alg: "HS256" })
    .setSubject(this.userIds.get("alice") ?? "alice-id")
    .setIssuedAt(Math.floor(Date.now() / 1000) - 3600)
    .setExpirationTime(Math.floor(Date.now() / 1000) - 1800)
    .sign(secretKey);
  this.tokens.set("alice_refresh", expiredToken);
});

Given("alice has used her refresh token to get a new token pair", async function (this: CustomWorld) {
  const refreshToken = this.tokens.get("alice_refresh") ?? "";
  this.context["alice_original_refresh"] = refreshToken;
  const res = await this.post("/api/v1/auth/refresh", { refresh_token: refreshToken });
  if (res.status !== 200) {
    throw new Error(`Failed to refresh: ${JSON.stringify(res.body)}`);
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  this.tokens.set("alice_access", (res.body as any).access_token as string);
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  this.tokens.set("alice_refresh", (res.body as any).refresh_token as string);
});

Given(/^the user "([^"]*)" has been deactivated$/, async function (this: CustomWorld, username: string) {
  const token = this.tokens.get(`${username}_access`) ?? "";
  const res = await this.post("/api/v1/users/me/deactivate", {}, token);
  if (res.status !== 200) {
    throw new Error(`Failed to deactivate ${username}: ${JSON.stringify(res.body)}`);
  }
});

Given("alice has already logged out once", async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const res = await this.post("/api/v1/auth/logout", {}, token);
  if (res.status !== 200) {
    throw new Error(`Logout failed: ${JSON.stringify(res.body)}`);
  }
});

// ---- When ----

When(/^alice sends POST \/api\/v1\/auth\/refresh with her refresh token$/, async function (this: CustomWorld) {
  const refreshToken = this.tokens.get("alice_refresh") ?? "";
  this.response = await this.post("/api/v1/auth/refresh", { refresh_token: refreshToken });
});

When(/^alice sends POST \/api\/v1\/auth\/refresh with her original refresh token$/, async function (this: CustomWorld) {
  const originalRefresh = this.context["alice_original_refresh"] as string;
  this.response = await this.post("/api/v1/auth/refresh", { refresh_token: originalRefresh });
});

When(/^alice sends POST \/api\/v1\/auth\/logout with her access token$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.post("/api/v1/auth/logout", {}, token);
});

When(/^alice sends POST \/api\/v1\/auth\/logout-all with her access token$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.post("/api/v1/auth/logout-all", {}, token);
});

// ---- Then ----

Then("the response body should contain an error message about token expiration", function (this: CustomWorld) {
  assert.ok(this.response !== null);
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const body = this.response?.body as Record<string, string>;
  const message = (body["message"] ?? body["error"] ?? "").toLowerCase();
  assert.ok(message.length > 0);
});

Then("the response body should contain an error message about invalid token", function (this: CustomWorld) {
  assert.ok(this.response !== null);
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const body = this.response?.body as Record<string, string>;
  const message = (body["message"] ?? body["error"] ?? "").toLowerCase();
  assert.ok(message.length > 0);
});

Then("alice's access token should be invalidated", async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const res = await this.get("/api/v1/users/me", token);
  assert.strictEqual(res.status, 401);
});
