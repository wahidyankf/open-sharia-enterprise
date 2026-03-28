import { test, expect } from "@playwright/test";

const PAGES = [
  { name: "landing", url: "/" },
  { name: "about", url: "/about/" },
  { name: "updates", url: "/updates/" },
  { name: "update-detail", url: "/updates/2026-02-08-phase-0-end-of-phase-0/" },
];

for (const page of PAGES) {
  test(`${page.name} renders correctly`, async ({ page: p }) => {
    await p.goto(page.url);
    await expect(p).toHaveScreenshot(`${page.name}.png`, {
      fullPage: true,
      maxDiffPixelRatio: 0.1,
    });
  });

  test(`${page.name} has no console errors`, async ({ page: p }) => {
    const errors: string[] = [];
    p.on("console", (msg) => {
      if (msg.type() === "error") errors.push(msg.text());
    });
    await p.goto(page.url);
    expect(errors).toEqual([]);
  });
}
