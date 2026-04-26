import { defineConfig, devices } from "@playwright/test";
import { defineBddConfig } from "playwright-bdd";

const testDir = defineBddConfig({
  featuresRoot: "../../specs/apps/organiclever/fe/gherkin",
  features: "../../specs/apps/organiclever/fe/gherkin/**/*.feature",
  steps: ["./steps/**/*.steps.ts"],
});

// When the staging URL sits behind Vercel Deployment Protection, the workflow
// supplies a Protection Bypass for Automation token via this env var. Playwright
// then sends `x-vercel-protection-bypass` on every request so navigations land
// on the actual app instead of the Vercel SSO auth wall. Local dev (no Vercel)
// leaves the var unset and the headers map stays empty.
const vercelBypass = process.env.VERCEL_AUTOMATION_BYPASS_SECRET;
const extraHTTPHeaders: Record<string, string> = vercelBypass
  ? {
      "x-vercel-protection-bypass": vercelBypass,
      "x-vercel-set-bypass-cookie": "true",
    }
  : {};

// Optional grep filter via env var. Used by the staging E2E workflow to skip
// scenarios tagged `@local-fullstack` (which need a real backend). Local dev
// runs every scenario by default.
const grepInvert = process.env.PLAYWRIGHT_GREP_INVERT ? new RegExp(process.env.PLAYWRIGHT_GREP_INVERT) : undefined;

export default defineConfig({
  testDir,
  timeout: 60000,
  // Tests run sequentially to avoid auth state conflicts across scenarios.
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  workers: 1,
  reporter: process.env.CI ? [["list"], ["html"]] : "list",
  grepInvert,
  use: {
    baseURL: process.env.WEB_BASE_URL || "http://localhost:3200",
    trace: "on-first-retry",
    screenshot: "only-on-failure",
    extraHTTPHeaders,
  },
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],
});
