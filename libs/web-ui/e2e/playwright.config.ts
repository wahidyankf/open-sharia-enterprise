import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: ".",
  testMatch: "**/*.visual.ts",
  snapshotDir: "./screenshots",
  snapshotPathTemplate: "{snapshotDir}/{testFilePath}/{arg}{ext}",
  use: {
    baseURL: "http://localhost:6006",
  },
  expect: {
    toHaveScreenshot: {
      maxDiffPixelRatio: 0.01,
    },
  },
  webServer: {
    command: "npx storybook dev -p 6006 --no-open --ci",
    port: 6006,
    reuseExistingServer: true,
    cwd: "..",
  },
});
