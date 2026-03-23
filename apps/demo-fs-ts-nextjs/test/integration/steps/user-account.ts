import { Given, When } from "@cucumber/cucumber";
import type { CustomWorld } from "../world";

Given(
  /^alice has deactivated her own account via POST \/api\/v1\/users\/me\/deactivate$/,
  async function (this: CustomWorld) {
    await this.dispatch("POST", "/api/v1/users/me/deactivate", null, this.getAuth("alice"));
  },
);

When(/^alice sends GET \/api\/v1\/users\/me$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", "/api/v1/users/me", null, this.getAuth("alice"));
});

When(/^alice sends PATCH \/api\/v1\/users\/me with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  this.response = await this.dispatch("PATCH", "/api/v1/users/me", JSON.parse(bodyStr), this.getAuth("alice"));
});

When(
  /^alice sends POST \/api\/v1\/users\/me\/password with body (.+)$/,
  async function (this: CustomWorld, bodyStr: string) {
    const body = JSON.parse(bodyStr);
    this.response = await this.dispatch(
      "POST",
      "/api/v1/users/me/password",
      { currentPassword: body.oldPassword, newPassword: body.newPassword },
      this.getAuth("alice"),
    );
  },
);

When(/^alice sends POST \/api\/v1\/users\/me\/deactivate$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("POST", "/api/v1/users/me/deactivate", null, this.getAuth("alice"));
});
