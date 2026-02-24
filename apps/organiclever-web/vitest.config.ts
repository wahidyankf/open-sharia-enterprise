import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import tsconfigPaths from "vite-tsconfig-paths";

const sharedPlugins = [react(), tsconfigPaths()];

export default defineConfig({
  plugins: sharedPlugins,
  test: {
    passWithNoTests: true,
    projects: [
      {
        plugins: sharedPlugins,
        test: {
          name: "unit",
          include: ["**/*.unit.{test,spec}.{ts,tsx}", "**/__tests__/**/*.{ts,tsx}"],
          exclude: ["**/*.integration.{test,spec}.{ts,tsx}", "node_modules"],
          environment: "jsdom",
        },
      },
      {
        plugins: sharedPlugins,
        test: {
          name: "integration",
          include: ["**/*.integration.{test,spec}.{ts,tsx}"],
          exclude: ["node_modules"],
          environment: "jsdom",
        },
      },
    ],
  },
});
