import { Given, When } from "@cucumber/cucumber";
import type { CustomWorld } from "../world";

Given(
  "a user {string} is registered with password {string}",
  async function (this: CustomWorld, username: string, password: string) {
    await this.registerUser(username, `${username}@example.com`, password);
  },
);

Given(
  "a user {string} is registered with email {string} and password {string}",
  async function (this: CustomWorld, username: string, email: string, password: string) {
    await this.registerUser(username, email, password);
    await this.loginUser(username, password);
  },
);

Given(
  "{string} has logged in and stored the access token and refresh token",
  async function (this: CustomWorld, username: string) {
    await this.loginUser(username, "Str0ng#Pass1");
  },
);

Given("{string} has logged in and stored the access token", async function (this: CustomWorld, username: string) {
  await this.loginUser(username, "Str0ng#Pass1");
});

Given("a user {string} is registered and deactivated", async function (this: CustomWorld, username: string) {
  // User already registered in Background — login and deactivate
  const resp = await this.dispatch("POST", "/api/v1/auth/login", { username, password: "Str0ng#Pass1" }, null);
  const token = (resp.body as { accessToken: string }).accessToken;
  await this.dispatch("POST", "/api/v1/users/me/deactivate", null, `Bearer ${token}`);
});

When(
  /^the client sends POST \/api\/v1\/auth\/register with body (.+)$/,
  async function (this: CustomWorld, bodyStr: string) {
    this.response = await this.dispatch("POST", "/api/v1/auth/register", JSON.parse(bodyStr), null);
  },
);

When(
  /^the client sends POST \/api\/v1\/auth\/login with body (.+)$/,
  async function (this: CustomWorld, bodyStr: string) {
    this.response = await this.dispatch("POST", "/api/v1/auth/login", JSON.parse(bodyStr), null);
  },
);
