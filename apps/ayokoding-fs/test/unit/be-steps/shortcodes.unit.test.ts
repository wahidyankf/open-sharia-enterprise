import { describe, it, expect } from "vitest";
import { transformShortcodes } from "@/server/content/shortcodes";

describe("transformShortcodes", () => {
  it("transforms callout shortcodes", () => {
    const input = '{{< callout type="warning" >}}Be careful!{{< /callout >}}';
    const result = transformShortcodes(input);
    expect(result).toContain('data-callout="warning"');
    expect(result).toContain("Be careful!");
  });

  it("transforms callout with different types", () => {
    const info = '{{< callout type="info" >}}Info here{{< /callout >}}';
    const tip = '{{< callout type="tip" >}}Tip here{{< /callout >}}';
    expect(transformShortcodes(info)).toContain('data-callout="info"');
    expect(transformShortcodes(tip)).toContain('data-callout="tip"');
  });

  it("transforms tabs shortcodes", () => {
    const input =
      '{{< tabs items="Go,Python" >}}{{< tab >}}Go code{{< /tab >}}{{< tab >}}Python code{{< /tab >}}{{< /tabs >}}';
    const result = transformShortcodes(input);
    expect(result).toContain('data-tabs="Go,Python"');
    expect(result).toContain("data-tab");
    expect(result).toContain("Go code");
    expect(result).toContain("Python code");
  });

  it("transforms youtube shortcodes", () => {
    const input = "{{< youtube dQw4w9WgXcQ >}}";
    const result = transformShortcodes(input);
    expect(result).toContain('data-youtube="dQw4w9WgXcQ"');
  });

  it("transforms steps shortcodes", () => {
    const input = "{{% steps %}}Step 1\nStep 2{{% /steps %}}";
    const result = transformShortcodes(input);
    expect(result).toContain("data-steps");
    expect(result).toContain("Step 1");
  });

  it("passes through plain markdown unchanged", () => {
    const input = "# Hello\n\nThis is **bold** text.";
    const result = transformShortcodes(input);
    expect(result).toBe(input);
  });

  it("handles multiple shortcodes in one document", () => {
    const input = '{{< callout type="info" >}}Info{{< /callout >}}\n\n{{< youtube abc123 >}}';
    const result = transformShortcodes(input);
    expect(result).toContain('data-callout="info"');
    expect(result).toContain('data-youtube="abc123"');
  });
});
