import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import tsconfigPaths from "vite-tsconfig-paths";

const sharedPlugins = [react(), tsconfigPaths()];

export default defineConfig({
  plugins: sharedPlugins,
  test: {
    passWithNoTests: true,
    env: {
      APP_JWT_SECRET: "test-jwt-secret-at-least-32-chars-long!!",
    },
    coverage: {
      provider: "v8",
      include: ["src/**/*.{ts,tsx}"],
      exclude: [
        "src/app/layout.tsx",
        "src/app/(dashboard)/layout.tsx",
        "src/app/(dashboard)/expenses/new/**",
        "src/app/api/**",
        "src/app/health/**",
        "src/app/.well-known/**",
        "src/repositories/**",
        "src/db/**",
        "src/lib/auth-middleware.ts",
        "src/lib/api/**",
        "src/lib/auth/**",
        "src/lib/queries/**",
        "src/test/**",
        "src/generated-contracts/**",
        "**/*.{test,spec}.{ts,tsx}",
      ],
      reporter: ["text", "json-summary", "lcov"],
    },
    projects: [
      {
        plugins: sharedPlugins,
        test: {
          name: "unit",
          include: ["test/unit/be-steps/**/*.steps.ts", "**/*.unit.{test,spec}.{ts,tsx}"],
          exclude: ["node_modules"],
          environment: "node",
        },
      },
      {
        plugins: sharedPlugins,
        test: {
          name: "unit-fe",
          include: ["test/unit/fe-steps/**/*.steps.{ts,tsx}"],
          exclude: ["node_modules"],
          environment: "jsdom",
          setupFiles: ["./src/test/setup.ts"],
        },
      },
    ],
  },
});
