import { test, expect } from "@playwright/test";

function buildTrpcUrl(procedure: string, input: unknown): string {
  const encoded = encodeURIComponent(JSON.stringify({ "0": { json: input } }));
  return `/api/trpc/${procedure}?batch=1&input=${encoded}`;
}

test.describe("i18n API", () => {
  test("English content served for locale en", async ({ request }) => {
    const url = buildTrpcUrl("content.getTree", { locale: "en" });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();

    const body = await response.json();
    const data = body[0].result.data.json;

    expect(Array.isArray(data)).toBeTruthy();
    expect(data.length).toBeGreaterThan(0);
  });

  test("Indonesian content served for locale id", async ({ request }) => {
    const url = buildTrpcUrl("content.getTree", { locale: "id" });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();

    const body = await response.json();
    const data = body[0].result.data.json;

    expect(Array.isArray(data)).toBeTruthy();
    expect(data.length).toBeGreaterThan(0);
  });

  test("Invalid locale returns error", async ({ request }) => {
    const url = buildTrpcUrl("content.getBySlug", { locale: "fr", slug: "test" });
    const response = await request.get(url);

    const body = await response.json();
    expect(body[0]).toHaveProperty("error");
  });
});
