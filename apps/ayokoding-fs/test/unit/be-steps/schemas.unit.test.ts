import { describe, it, expect } from "vitest";
import { frontmatterSchema } from "@/lib/schemas/content";
import { searchQuerySchema } from "@/lib/schemas/search";
import { localeSchema } from "@/lib/schemas/navigation";

describe("frontmatterSchema", () => {
  it("parses valid frontmatter", () => {
    const result = frontmatterSchema.parse({
      title: "Test Page",
      weight: 100,
      draft: false,
    });
    expect(result.title).toBe("Test Page");
    expect(result.weight).toBe(100);
    expect(result.draft).toBe(false);
    expect(result.tags).toEqual([]);
  });

  it("applies defaults for optional fields", () => {
    const result = frontmatterSchema.parse({ title: "Minimal" });
    expect(result.draft).toBe(false);
    expect(result.weight).toBe(0);
    expect(result.tags).toEqual([]);
  });

  it("parses date as Date object", () => {
    const result = frontmatterSchema.parse({
      title: "Dated",
      date: "2025-01-15T00:00:00Z",
    });
    expect(result.date).toBeInstanceOf(Date);
  });

  it("rejects missing title", () => {
    const result = frontmatterSchema.safeParse({ weight: 10 });
    expect(result.success).toBe(false);
  });

  it("handles Hugo-specific fields", () => {
    const result = frontmatterSchema.parse({
      title: "Hugo Page",
      cascade: { type: "docs" },
      breadcrumbs: false,
      bookCollapseSection: true,
    });
    expect(result.cascade).toEqual({ type: "docs" });
    expect(result.breadcrumbs).toBe(false);
  });
});

describe("searchQuerySchema", () => {
  it("validates a proper search query", () => {
    const result = searchQuerySchema.parse({
      query: "golang",
      locale: "en",
    });
    expect(result.query).toBe("golang");
    expect(result.locale).toBe("en");
    expect(result.limit).toBe(20);
  });

  it("rejects empty query", () => {
    const result = searchQuerySchema.safeParse({
      query: "",
      locale: "en",
    });
    expect(result.success).toBe(false);
  });

  it("rejects invalid locale", () => {
    const result = searchQuerySchema.safeParse({
      query: "test",
      locale: "fr",
    });
    expect(result.success).toBe(false);
  });

  it("clamps limit", () => {
    const result = searchQuerySchema.safeParse({
      query: "test",
      locale: "en",
      limit: 0,
    });
    expect(result.success).toBe(false);
  });
});

describe("localeSchema", () => {
  it("accepts valid locales", () => {
    expect(localeSchema.parse("en")).toBe("en");
    expect(localeSchema.parse("id")).toBe("id");
  });

  it("rejects invalid locales", () => {
    const result = localeSchema.safeParse("fr");
    expect(result.success).toBe(false);
  });
});
