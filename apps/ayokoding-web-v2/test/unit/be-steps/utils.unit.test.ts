import { describe, it, expect } from "vitest";
import { cn } from "@/lib/utils";

describe("cn utility", () => {
  it("merges class names", () => {
    const result = cn("px-4", "py-2");
    expect(result).toContain("px-4");
    expect(result).toContain("py-2");
  });

  it("handles conditional classes", () => {
    const result = cn("base", false && "hidden", "visible");
    expect(result).toContain("base");
    expect(result).toContain("visible");
    expect(result).not.toContain("hidden");
  });

  it("deduplicates tailwind classes", () => {
    const result = cn("px-4", "px-8");
    expect(result).toBe("px-8");
  });
});
