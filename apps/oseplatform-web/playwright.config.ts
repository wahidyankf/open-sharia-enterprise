import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./test/visual",
  outputDir: "./playwright-report",
  use: {
    baseURL: "http://localhost:3100",
  },
  projects: [
    { name: "mobile", use: { ...devices["iPhone 14"] } },
    { name: "tablet", use: { ...devices["iPad Mini"] } },
    { name: "desktop", use: { viewport: { width: 1440, height: 900 } } },
  ],
});
