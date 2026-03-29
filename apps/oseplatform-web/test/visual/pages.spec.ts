import { test, expect } from "@playwright/test";

const PAGES = [
  { name: "landing", url: "/" },
  { name: "about", url: "/about/" },
  { name: "updates", url: "/updates/" },
  { name: "update-detail", url: "/updates/2026-02-08-phase-0-end-of-phase-0/" },
];

for (const page of PAGES) {
  test(`${page.name} loads without error`, async ({ page: p }) => {
    const response = await p.goto(page.url);
    expect(response?.status()).toBe(200);
    await expect(p.locator("body")).toBeVisible();
  });
}
