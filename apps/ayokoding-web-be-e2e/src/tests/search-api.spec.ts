import { test, expect } from "@playwright/test";

function buildTrpcUrl(procedure: string, input: unknown): string {
  const encoded = encodeURIComponent(JSON.stringify({ "0": { json: input } }));
  return `/api/trpc/${procedure}?batch=1&input=${encoded}`;
}

test.describe("Search API", () => {
  test("search.query returns results for a valid query", async ({ request }) => {
    const url = buildTrpcUrl("search.query", { query: "python", locale: "en", limit: 5 });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();

    const body = await response.json();
    const data = body[0].result.data.json;

    expect(Array.isArray(data)).toBeTruthy();
  });

  test("search.query rejects empty query", async ({ request }) => {
    const url = buildTrpcUrl("search.query", { query: "", locale: "en" });
    const response = await request.get(url);

    const body = await response.json();
    expect(body[0]).toHaveProperty("error");
  });

  test("search.query scopes results to locale", async ({ request }) => {
    const url = buildTrpcUrl("search.query", { query: "ikhtisar", locale: "id", limit: 10 });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();

    const body = await response.json();
    const data = body[0].result.data.json;

    expect(Array.isArray(data)).toBeTruthy();
    for (const result of data) {
      expect(result.locale).toBe("id");
    }
  });
});
