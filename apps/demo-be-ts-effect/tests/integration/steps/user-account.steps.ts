import { Given, When } from "@cucumber/cucumber";
import type { CustomWorld } from "../world.js";

// ---- Given ----

Given(
  /^alice has deactivated her own account via POST \/api\/v1\/users\/me\/deactivate$/,
  async function (this: CustomWorld) {
    const token = this.tokens.get("alice_access") ?? "";
    const res = await this.post("/api/v1/users/me/deactivate", {}, token);
    if (res.status !== 200) {
      throw new Error(`Failed to deactivate: ${JSON.stringify(res.body)}`);
    }
  },
);

// ---- When ----

When(/^alice sends GET \/api\/v1\/users\/me$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.get("/api/v1/users/me", token);
});

When(/^alice sends PATCH \/api\/v1\/users\/me with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  const body = JSON.parse(bodyStr) as Record<string, unknown>;
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.patch("/api/v1/users/me", body, token);
});

When(
  /^alice sends POST \/api\/v1\/users\/me\/password with body (.+)$/,
  async function (this: CustomWorld, bodyStr: string) {
    const body = JSON.parse(bodyStr) as Record<string, unknown>;
    const token = this.tokens.get("alice_access") ?? "";
    this.response = await this.post("/api/v1/users/me/password", body, token);
  },
);

When(/^alice sends POST \/api\/v1\/users\/me\/deactivate$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.post("/api/v1/users/me/deactivate", {}, token);
});

// Note: "the client sends GET {path} with alice's access token" is handled in auth.steps.ts
// Note: "the response body should contain a non-null {string} field" is defined in auth.steps.ts
// Note: "the response body should contain {string} equal to {string}" is defined in auth.steps.ts
