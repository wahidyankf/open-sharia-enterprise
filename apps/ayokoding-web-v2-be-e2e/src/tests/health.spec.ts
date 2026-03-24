import { test, expect } from "@playwright/test";

test.describe("Health", () => {
  test("GET /api/trpc/meta.health returns { status: 'ok' }", async ({ request }) => {
    const response = await request.get("/api/trpc/meta.health?batch=1&input=%7B%7D");

    expect(response.ok()).toBeTruthy();

    const body = await response.json();

    // tRPC batch response is an array; index 0 holds the first procedure result
    const result = Array.isArray(body) ? body[0] : body;

    // superjson wraps data in { json: ... }
    expect(result).toMatchObject({
      result: {
        data: { json: { status: "ok" } },
      },
    });
  });
});
