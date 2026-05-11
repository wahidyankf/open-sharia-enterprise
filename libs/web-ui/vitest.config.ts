import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import path from "path";

export default defineConfig({
  plugins: [react()],
  test: {
    globals: true,
    environment: "jsdom",
    setupFiles: [path.resolve(__dirname, "vitest.setup.ts")],
    include: ["src/**/*.test.{ts,tsx}", "src/**/*.steps.{ts,tsx}"],
    coverage: {
      provider: "v8",
      reporter: ["text", "lcov"],
      reportsDirectory: "./coverage",
      include: ["src/**/*.{ts,tsx}"],
      exclude: ["src/**/*.test.{ts,tsx}", "src/**/*.stories.{ts,tsx}", "src/types/**"],
    },
  },
  resolve: {
    alias: {
      "@open-sharia-enterprise/ts-ui-tokens": path.resolve(__dirname, "../ts-ui-tokens/src"),
    },
  },
});
