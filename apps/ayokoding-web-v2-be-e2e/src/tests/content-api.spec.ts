import { test, expect } from "@playwright/test";

function buildTrpcUrl(procedure: string, input: unknown): string {
  const encoded = encodeURIComponent(JSON.stringify({ "0": { json: input } }));
  return `/api/trpc/${procedure}?batch=1&input=${encoded}`;
}

test.describe("Content API", () => {
  test("content.getBySlug returns content for a valid slug", async ({ request }) => {
    const url = buildTrpcUrl("content.getBySlug", { locale: "en", slug: "learn/overview" });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();

    const body = await response.json();
    const data = body[0].result.data.json;

    expect(data.title).toBe("Overview");
    expect(data.slug).toBe("learn/overview");
    expect(data.locale).toBe("en");
    expect(data.html).toContain("<");
    expect(data.headings).toBeDefined();
  });

  test("content.getBySlug returns error for unknown slug", async ({ request }) => {
    const url = buildTrpcUrl("content.getBySlug", { locale: "en", slug: "does-not-exist" });
    const response = await request.get(url);

    const body = await response.json();
    expect(body[0]).toHaveProperty("error");
  });

  test("content.getTree returns hierarchical tree", async ({ request }) => {
    const url = buildTrpcUrl("content.getTree", { locale: "en" });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();

    const body = await response.json();
    const data = body[0].result.data.json;

    expect(Array.isArray(data)).toBeTruthy();
    expect(data.length).toBeGreaterThan(0);
  });

  test("content.listChildren returns children for a section", async ({ request }) => {
    const url = buildTrpcUrl("content.listChildren", { locale: "en", parentSlug: "learn" });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();

    const body = await response.json();
    const data = body[0].result.data.json;

    expect(Array.isArray(data)).toBeTruthy();
    expect(data.length).toBeGreaterThan(0);
  });
});
