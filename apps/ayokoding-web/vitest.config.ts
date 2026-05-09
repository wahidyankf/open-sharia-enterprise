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
        // App shell presentation (chrome) — passive UI primitives + layout shell
        "src/contexts/app-shell/presentation/**",
        // Per-BC presentation (UI surfaces — exercised via E2E + fe-step Gherkin scenarios)
        "src/contexts/content/presentation/**",
        "src/contexts/search/presentation/**",
        "src/contexts/i18n/presentation/**",
        "src/contexts/navigation/presentation/**",
        // Re-export shim — pure type re-export, no executable code
        "src/contexts/navigation/application/schemas.ts",
        // Next.js app router pages — covered via E2E
        "src/app/**",
        // Cross-cutting tRPC client wiring
        "src/lib/trpc/client.ts",
        "src/lib/trpc/provider.tsx",
        "src/lib/trpc/server.ts",
        // i18n middleware re-export shim and implementation (covered via fe-e2e)
        "src/middleware.ts",
        "src/contexts/i18n/application/middleware.ts",
        // Content infrastructure adapters + scripts (covered via integration suite)
        "src/contexts/content/infrastructure/parser.ts",
        "src/contexts/content/infrastructure/reader.ts",
        "src/contexts/content/infrastructure/repository.ts",
        "src/contexts/content/infrastructure/repository-fs.ts",
        "src/contexts/content/infrastructure/types.ts",
        "src/contexts/content/infrastructure/index-generator.ts",
        "src/contexts/search/infrastructure/**",
        "src/scripts/**",
        // tRPC routers — composition-only; behaviour is exercised via BE-E2E
        "src/contexts/**/application/router.ts",
        "src/contexts/app-shell/application/root-router.ts",
        // Test infra
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
