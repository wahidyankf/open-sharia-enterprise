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
        "src/app/**",
        "src/lib/trpc/client.ts",
        "src/lib/trpc/provider.tsx",
        "src/lib/trpc/server.ts",
        "src/contexts/content/application/types.ts",
        "src/contexts/content/infrastructure/reader.ts",
        "src/contexts/content/infrastructure/repository.ts",
        "src/contexts/content/infrastructure/repository-fs.ts",
        "src/contexts/rss-feed/application/feed-builder.ts",
        "src/contexts/seo/application/sitemap-builder.ts",
        "src/contexts/seo/presentation/**",
        "src/contexts/health/presentation/**",
        "src/contexts/landing/presentation/**",
        "src/contexts/app-shell/presentation/**",
        "src/contexts/content/presentation/**",
        "src/contexts/search/application/service.ts",
        "src/contexts/search/presentation/**",
        "src/scripts/**",
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
          globals: true,
          setupFiles: ["./src/test/setup.ts"],
        },
      },
      {
        plugins: sharedPlugins,
        test: {
          name: "integration",
          include: ["test/integration/be-steps/**/*.steps.ts", "**/*.integration.{test,spec}.{ts,tsx}"],
          exclude: ["node_modules"],
          environment: "node",
        },
      },
    ],
  },
});
