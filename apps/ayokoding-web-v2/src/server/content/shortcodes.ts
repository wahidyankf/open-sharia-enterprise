/**
 * Custom remark plugin to transform Hugo shortcodes into HTML data attributes.
 * These are later mapped to React components via html-react-parser.
 *
 * Supported shortcodes:
 * - {{< callout type="warning|info|tip" >}}...{{< /callout >}}
 * - {{< tabs items="..." >}}...{{< /tabs >}} with {{< tab >}}...{{< /tab >}}
 * - {{< youtube ID >}}
 * - {{% steps %}}...{{% /steps %}}
 */

export function transformShortcodes(content: string): string {
  let result = content;

  // Transform callout shortcodes
  result = result.replace(
    /{{<\s*callout\s+type="([^"]+)"\s*>}}([\s\S]*?){{<\s*\/callout\s*>}}/g,
    (_match, type: string, body: string) => `<div data-callout="${type}">\n\n${body.trim()}\n\n</div>`,
  );

  // Transform tabs shortcodes
  result = result.replace(
    /{{<\s*tabs\s+items="([^"]+)"\s*>}}([\s\S]*?){{<\s*\/tabs\s*>}}/g,
    (_match, items: string, body: string) => {
      const tabContent = body.replace(
        /{{<\s*tab\s*>}}([\s\S]*?){{<\s*\/tab\s*>}}/g,
        (_m: string, tabBody: string) => `<div data-tab>\n\n${tabBody.trim()}\n\n</div>`,
      );
      return `<div data-tabs="${items}">\n\n${tabContent.trim()}\n\n</div>`;
    },
  );

  // Transform youtube shortcodes
  result = result.replace(
    /{{<\s*youtube\s+([^\s>]+)\s*>}}/g,
    (_match, id: string) => `<div data-youtube="${id}"></div>`,
  );

  // Transform steps shortcodes (uses {{% %}} delimiters)
  result = result.replace(
    /{{% steps %}}([\s\S]*?){{% \/steps %}}/g,
    (_match, body: string) => `<div data-steps>\n\n${body.trim()}\n\n</div>`,
  );

  return result;
}
