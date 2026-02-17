import { test, expect } from "@playwright/test";

test.describe("Actuator API", () => {
  test.describe("GET /actuator/health", () => {
    test("returns HTTP 200", async ({ request }) => {
      const response = await request.get("/actuator/health");
      expect(response.status()).toBe(200);
    });

    test("reports UP status", async ({ request }) => {
      const response = await request.get("/actuator/health");
      const body = await response.json();
      expect(body.status).toBe("UP");
    });
  });
});
