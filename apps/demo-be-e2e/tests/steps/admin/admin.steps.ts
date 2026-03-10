import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { setResponse, getResponse } from "../../utils/response-store";
import { getTokenForUser, getIdForUser, setIdForUser } from "../../utils/token-store";

const { Given, When, Then } = createBdd();

// ---------------------------------------------------------------------------
// Admin step definitions
// ---------------------------------------------------------------------------

Given(
  "users {string}, {string}, and {string} are registered",
  async ({ request }, user1: string, user2: string, user3: string) => {
    for (const username of [user1, user2, user3]) {
      const res = await request.post("/api/v1/auth/register", {
        data: { username, email: `${username}@example.com`, password: "Str0ng#Pass1" },
        headers: { "Content-Type": "application/json" },
      });
      const body = (await res.json()) as Record<string, unknown>;
      if (body["id"]) {
        setIdForUser(username, body["id"] as string);
      }
    }
  },
);

When(/^the admin sends GET \/api\/v1\/admin\/users$/, async ({ request }) => {
  const token = getTokenForUser("superadmin");
  setResponse(
    await request.get("/api/v1/admin/users", {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

When(/^the admin sends GET \/api\/v1\/admin\/users\?email=alice@example\.com$/, async ({ request }) => {
  const token = getTokenForUser("superadmin");
  setResponse(
    await request.get("/api/v1/admin/users?email=alice@example.com", {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

When(
  /^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/disable with body \{ "reason": "Policy violation" \}$/,
  async ({ request }) => {
    const token = getTokenForUser("superadmin");
    const aliceId = getIdForUser("alice");
    setResponse(
      await request.post(`/api/v1/admin/users/${aliceId}/disable`, {
        data: { reason: "Policy violation" },
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${token}`,
        },
      }),
    );
  },
);

Given("alice's account has been disabled by the admin", async ({ request }) => {
  const token = getTokenForUser("superadmin");
  const aliceId = getIdForUser("alice");
  await request.post(`/api/v1/admin/users/${aliceId}/disable`, {
    data: { reason: "Test disable" },
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  });
});

Given("alice's account has been disabled", async ({ request }) => {
  const token = getTokenForUser("superadmin");
  const aliceId = getIdForUser("alice");
  await request.post(`/api/v1/admin/users/${aliceId}/disable`, {
    data: { reason: "Test disable" },
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  });
});

When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/enable$/, async ({ request }) => {
  const token = getTokenForUser("superadmin");
  const aliceId = getIdForUser("alice");
  setResponse(
    await request.post(`/api/v1/admin/users/${aliceId}/enable`, {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/force-password-reset$/, async ({ request }) => {
  const token = getTokenForUser("superadmin");
  const aliceId = getIdForUser("alice");
  setResponse(
    await request.post(`/api/v1/admin/users/${aliceId}/force-password-reset`, {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/unlock$/, async ({ request }) => {
  const token = getTokenForUser("superadmin");
  const aliceId = getIdForUser("alice");
  setResponse(
    await request.post(`/api/v1/admin/users/${aliceId}/unlock`, {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

Then(
  "the response body should contain at least one user with {string} equal to {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string, value: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    // The paginated response has a "data" array
    const data = body["data"] as Array<Record<string, unknown>>;
    expect(Array.isArray(data)).toBe(true);
    const match = data.find((user) => user[field] === value);
    expect(match).toBeDefined();
  },
);
