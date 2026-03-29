import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./test/visual",
  outputDir: "./playwright-report",
  use: {
    baseURL: "http://localhost:3100",
  },
  projects: [
    { name: "mobile", use: { ...devices["Pixel 7"] } },
    { name: "tablet", use: { ...devices["Galaxy Tab S4"] } },
    { name: "desktop", use: { viewport: { width: 1440, height: 900 } } },
  ],
});
