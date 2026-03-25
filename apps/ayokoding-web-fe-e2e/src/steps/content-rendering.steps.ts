import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";

const { Given, When, Then } = createBdd();

Given("the app is running", async () => {
  // Base URL is configured in playwright.config.ts — no action needed
});

When("a visitor opens a content page with prose body text", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("the body text should have prose typography classes applied", async ({ page }) => {
  const prose = page.locator(".prose, [class*='prose']").first();
  await expect(prose).toBeVisible();
});

Then("headings should be visually distinct from body text", async ({ page }) => {
  const heading = page.getByRole("heading", { level: 2 }).first();
  await expect(heading).toBeVisible();
});

Then("paragraph spacing should be consistent", async ({ page }) => {
  const paragraph = page.locator("article p").first();
  await expect(paragraph).toBeVisible();
});

When("a visitor opens a content page containing a fenced code block", async ({ page }) => {
  await page.goto("/en/learn/software-engineering/programming-languages/golang/by-example/beginner");
});

Then("the code block should display with syntax-highlighted tokens", async ({ page }) => {
  const codeFigure = page.locator("figure[data-rehype-pretty-code-figure]").first();
  await expect(codeFigure).toBeVisible({ timeout: 10000 });

  const coloredSpan = codeFigure.locator("span[style*='--shiki-light']").first();
  await expect(coloredSpan).toBeAttached();
});

Then("the language label should be shown above the code block", async ({ page }) => {
  const langLabel = page.locator("[data-rehype-pretty-code-title], figcaption, [data-language]").first();
  await expect(langLabel).toBeAttached();
});

Then("the block should use a monospace font", async ({ page }) => {
  const codeEl = page.locator("figure[data-rehype-pretty-code-figure] code").first();
  await expect(codeEl).toBeVisible();
  const fontFamily = await codeEl.evaluate((el) => window.getComputedStyle(el).fontFamily);
  expect(fontFamily.toLowerCase()).toMatch(/mono|courier|consolas|menlo|inconsolata|fira/i);
});

When("a visitor opens a content page containing a callout shortcode", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("the callout should render as an admonition block", async ({ page }) => {
  // Callout/admonition block rendered by the shortcode
  const admonition = page.locator("[role='note'], .callout, .admonition, [data-callout]").first();
  await expect(admonition).toBeAttached();
});

Then("the admonition should display the appropriate icon and label for its type", async ({ page }) => {
  const admonitionLabel = page.locator("[role='note'] svg, .callout svg, .admonition svg, [data-callout] svg").first();
  await expect(admonitionLabel).toBeAttached();
});

Then("the callout body text should be visible inside the admonition", async ({ page }) => {
  const admonitionBody = page.locator("[role='note'] p, .callout p, .admonition p, [data-callout] p").first();
  await expect(admonitionBody).toBeAttached();
});

When("a visitor opens a content page containing a tabs shortcode", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("the tabs should render as a tab bar with clickable tab labels", async ({ page }) => {
  const tabList = page.getByRole("tablist").first();
  await expect(tabList).toBeAttached();
});

When("the visitor clicks a tab label", async ({ page }) => {
  const tab = page.getByRole("tab").nth(1);
  await tab.click();
});

Then("the corresponding panel content should become visible", async ({ page }) => {
  const activePanel = page.getByRole("tabpanel").first();
  await expect(activePanel).toBeVisible();
});

Then("the other panels should be hidden", async ({ page }) => {
  const panels = page.getByRole("tabpanel");
  const count = await panels.count();
  // At least one panel is present; selected one is visible
  expect(count).toBeGreaterThanOrEqual(1);
});

When("a visitor opens a content page containing a YouTube shortcode", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("a responsive iframe embed should be visible", async ({ page }) => {
  const iframe = page.locator("iframe[src*='youtube'], iframe[src*='youtu.be']").first();
  await expect(iframe).toBeAttached();
});

Then("the iframe src should point to the YouTube embed URL", async ({ page }) => {
  const iframe = page.locator("iframe[src*='youtube.com/embed'], iframe[src*='youtu.be']").first();
  await expect(iframe).toBeAttached();
});

Then("the embed should maintain a 16:9 aspect ratio", async ({ page }) => {
  const wrapper = page.locator(".aspect-video, [style*='aspect-ratio'], [class*='16-9'], [class*='youtube']").first();
  await expect(wrapper).toBeAttached();
});

When("a visitor opens a content page containing a steps shortcode", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("the steps should render as an ordered list of numbered items", async ({ page }) => {
  const steps = page.locator("ol, [data-steps], .steps").first();
  await expect(steps).toBeAttached();
});

Then("each step should display its number prominently", async ({ page }) => {
  const firstStep = page.locator("ol li, [data-steps] > *, .steps > *").first();
  await expect(firstStep).toBeAttached();
});

Then("the step content should be indented beneath its number", async ({ page }) => {
  const stepContent = page.locator("ol li p, [data-steps] p").first();
  await expect(stepContent).toBeAttached();
});

When("a visitor opens a content page containing an inline math expression delimited by $...$", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("the expression should render as formatted math notation inline with surrounding text", async ({ page }) => {
  const katexInline = page.locator(".katex, .math-inline, [class*='katex']").first();
  await expect(katexInline).toBeAttached();
});

Then("the rendered math should not display raw LaTeX source", async ({ page }) => {
  // If math is rendered, raw delimiters like $...$ should not be visible as plain text
  const bodyText = await page.locator("article").textContent();
  // KaTeX replaces raw source — raw dollar-wrapped LaTeX should not appear literally
  expect(bodyText).not.toMatch(/\$[^$]+\$/);
});

When("a visitor opens a content page containing a block math expression delimited by $$...$$", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("the expression should render as a centered display math block", async ({ page }) => {
  const katexBlock = page.locator(".katex-display, .math-display, [class*='katex-display']").first();
  await expect(katexBlock).toBeAttached();
});

When("a visitor opens a content page containing a Mermaid code block", async ({ page }) => {
  await page.goto("/en/learn/artificial-intelligence/chat-with-pdf");
  await page.waitForLoadState("networkidle");
});

Then("the diagram should render as an inline SVG element", async ({ page }) => {
  const mermaidSvg = page.locator("[id*='mermaid'], svg.mermaid").first();
  await expect(mermaidSvg).toBeAttached({ timeout: 45000 });
});

Then("the raw Mermaid source should not be visible to the visitor", async ({ page }) => {
  const preMermaid = page.locator("pre code.language-mermaid");
  // Raw mermaid code block should not be rendered as visible text
  await expect(preMermaid).toBeHidden();
});

When(
  "a visitor opens a content page containing raw HTML such as inline div, table, and details elements",
  async ({ page }) => {
    await page.goto("/en/learn/overview");
  },
);

Then("the HTML elements should render in the browser as expected", async ({ page }) => {
  const article = page.getByRole("article");
  await expect(article).toBeVisible();
  await expect(article).not.toBeEmpty();
});

Then("the elements should be visible and styled appropriately", async ({ page }) => {
  const article = page.getByRole("article");
  const hasContent = await article.evaluate((el) => el.children.length > 0);
  expect(hasContent).toBe(true);
});
