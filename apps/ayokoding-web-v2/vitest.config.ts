import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import tsconfigPaths from "vite-tsconfig-paths";

const sharedPlugins = [react(), tsconfigPaths()];

export default defineConfig({
  plugins: sharedPlugins,
  test: {
    passWithNoTests: true,
    coverage: {
      provider: "v8",
      include: ["src/**/*.{ts,tsx}"],
      exclude: [
        "src/components/ui/**",
        "src/components/layout/**",
        "src/components/content/**",
        "src/components/search/**",
        "src/app/**",
        "src/lib/hooks/**",
        "src/lib/trpc/client.ts",
        "src/lib/trpc/provider.tsx",
        "src/lib/trpc/server.ts",
        "src/middleware.ts",
        "src/server/content/index.ts",
        "src/server/content/parser.ts",
        "src/server/content/reader.ts",
        "src/server/content/search-index.ts",
        "src/server/content/types.ts",
        "src/server/trpc/procedures/**",
        "src/test/**",
        "**/*.{test,spec}.{ts,tsx}",
      ],
      thresholds: {
        lines: 80,
        functions: 80,
        branches: 80,
        statements: 80,
      },
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
