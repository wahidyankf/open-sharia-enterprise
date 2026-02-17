import { test, expect } from "@playwright/test";

test.describe("Hello API", () => {
  test.describe("GET /api/v1/hello", () => {
    test("returns HTTP 200", async ({ request }) => {
      const response = await request.get("/api/v1/hello");
      expect(response.status()).toBe(200);
    });

    test("returns world message", async ({ request }) => {
      const response = await request.get("/api/v1/hello");
      const body = await response.json();
      expect(body.message).toBe("world!");
    });

    test("returns JSON content type", async ({ request }) => {
      const response = await request.get("/api/v1/hello");
      expect(response.headers()["content-type"]).toContain("application/json");
    });
  });
});
