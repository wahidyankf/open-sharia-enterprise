import { createBdd } from "playwright-bdd";
import { setResponse } from "../../utils/response-store";
import { getTokenForUser } from "../../utils/token-store";

const { Given, When } = createBdd();

// ---------------------------------------------------------------------------
// User account management steps
// ---------------------------------------------------------------------------

When(/^alice sends GET \/api\/v1\/users\/me$/, async ({ request }) => {
  const token = getTokenForUser("alice");
  setResponse(
    await request.get("/api/v1/users/me", {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

When(/^alice sends PATCH \/api\/v1\/users\/me with body \{ "displayName": "Alice Smith" \}$/, async ({ request }) => {
  const token = getTokenForUser("alice");
  setResponse(
    await request.patch("/api/v1/users/me", {
      data: { displayName: "Alice Smith" },
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
    }),
  );
});

When(
  /^alice sends POST \/api\/v1\/users\/me\/password with body \{ "oldPassword": "Str0ng#Pass1", "newPassword": "NewPass#456" \}$/,
  async ({ request }) => {
    const token = getTokenForUser("alice");
    setResponse(
      await request.post("/api/v1/users/me/password", {
        data: { oldPassword: "Str0ng#Pass1", newPassword: "NewPass#456" },
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${token}`,
        },
      }),
    );
  },
);

When(
  /^alice sends POST \/api\/v1\/users\/me\/password with body \{ "oldPassword": "Wr0ngOld!", "newPassword": "NewPass#456" \}$/,
  async ({ request }) => {
    const token = getTokenForUser("alice");
    setResponse(
      await request.post("/api/v1/users/me/password", {
        data: { oldPassword: "Wr0ngOld!", newPassword: "NewPass#456" },
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${token}`,
        },
      }),
    );
  },
);

When(/^alice sends POST \/api\/v1\/users\/me\/deactivate$/, async ({ request }) => {
  const token = getTokenForUser("alice");
  setResponse(
    await request.post("/api/v1/users/me/deactivate", {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

Given(/^alice has deactivated her own account via POST \/api\/v1\/users\/me\/deactivate$/, async ({ request }) => {
  const token = getTokenForUser("alice");
  await request.post("/api/v1/users/me/deactivate", {
    headers: { Authorization: `Bearer ${token}` },
  });
});
