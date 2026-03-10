import { createBdd } from "playwright-bdd";

const { Given, When } = createBdd();

// Stubs — implement alongside production features

// Steps from registration.feature
When(
  /^the client sends POST \/api\/v1\/auth\/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "Str0ng#Pass1" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

When(
  /^the client sends POST \/api\/v1\/auth\/register with body \{ "username": "alice", "email": "new@example\.com", "password": "Str0ng#Pass1" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

When(
  /^the client sends POST \/api\/v1\/auth\/register with body \{ "username": "alice", "email": "not-an-email", "password": "Str0ng#Pass1" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

When(
  /^the client sends POST \/api\/v1\/auth\/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

When(
  /^the client sends POST \/api\/v1\/auth\/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "str0ng#pass1" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

// Steps from user-account.feature
When(/^alice sends GET \/api\/v1\/users\/me$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends PATCH \/api\/v1\/users\/me with body \{ "display_name": "Alice Smith" \}$/, async () => {
  throw new Error("TODO: not implemented");
});

When(
  /^alice sends POST \/api\/v1\/users\/me\/password with body \{ "old_password": "Str0ng#Pass1", "new_password": "NewPass#456" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

When(
  /^alice sends POST \/api\/v1\/users\/me\/password with body \{ "old_password": "Wr0ngOld!", "new_password": "NewPass#456" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

When(/^alice sends POST \/api\/v1\/users\/me\/deactivate$/, async () => {
  throw new Error("TODO: not implemented");
});

Given(/^alice has deactivated her own account via POST \/api\/v1\/users\/me\/deactivate$/, async () => {
  throw new Error("TODO: not implemented");
});
