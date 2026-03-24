import { describe, it, expect } from "vitest";
import { deriveSlug, stripMarkdown } from "@/server/content/reader";

describe("deriveSlug", () => {
  it("derives slug for regular content page", () => {
    const result = deriveSlug("/content/en/learn/overview.md", "/content");
    expect(result).toEqual({ locale: "en", slug: "learn/overview", isSection: false });
  });

  it("derives slug for section _index.md", () => {
    const result = deriveSlug("/content/en/learn/_index.md", "/content");
    expect(result).toEqual({ locale: "en", slug: "learn", isSection: true });
  });

  it("derives slug for root _index.md", () => {
    const result = deriveSlug("/content/en/_index.md", "/content");
    expect(result).toEqual({ locale: "en", slug: "", isSection: true });
  });

  it("derives slug for nested content", () => {
    const result = deriveSlug(
      "/content/en/learn/software-engineering/programming-languages/golang/overview.md",
      "/content",
    );
    expect(result).toEqual({
      locale: "en",
      slug: "learn/software-engineering/programming-languages/golang/overview",
      isSection: false,
    });
  });

  it("derives slug for Indonesian content", () => {
    const result = deriveSlug("/content/id/belajar/ikhtisar.md", "/content");
    expect(result).toEqual({ locale: "id", slug: "belajar/ikhtisar", isSection: false });
  });
});

describe("stripMarkdown", () => {
  it("strips markdown formatting", () => {
    const result = stripMarkdown("# Hello\n\n**bold** and *italic* text");
    expect(result).toContain("bold");
    expect(result).toContain("italic");
    expect(result).not.toContain("**");
    expect(result).not.toContain("*italic*");
  });

  it("strips code blocks", () => {
    const result = stripMarkdown("text\n```go\nfmt.Println()\n```\nmore text");
    expect(result).toContain("text");
    expect(result).not.toContain("fmt.Println");
  });

  it("strips links", () => {
    const result = stripMarkdown("[click here](https://example.com)");
    expect(result).toContain("click here");
    expect(result).not.toContain("https://");
  });

  it("strips Hugo shortcodes", () => {
    const result = stripMarkdown('text {{< callout type="info" >}}callout{{< /callout >}} more');
    expect(result).toContain("text");
  });

  it("strips HTML tags", () => {
    const result = stripMarkdown("text <div>inner</div> more");
    expect(result).toContain("text");
    expect(result).toContain("inner");
    expect(result).not.toContain("<div>");
  });
});
