import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";
import tsconfigPaths from "vite-tsconfig-paths";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const sharedPlugins: any[] = [react(), tsconfigPaths()];

export default defineConfig({
  plugins: sharedPlugins,
  test: {
    passWithNoTests: true,
    coverage: {
      provider: "v8",
      include: ["app/**/*.{ts,tsx}"],
      exclude: [
        "app/root.tsx",
        "app/test/**",
        "app/lib/api/**",
        "app/lib/auth/**",
        "app/lib/queries/**",
        "**/*.{test,spec}.{ts,tsx}",
      ],
      thresholds: {
        lines: 25,
        functions: 10,
        branches: 20,
        statements: 25,
      },
      reporter: ["text", "json-summary", "lcov"],
    },
    projects: [
      {
        plugins: sharedPlugins,
        test: {
          name: "unit",
          include: ["test/unit/**/*.steps.{ts,tsx}"],
          exclude: ["node_modules"],
          environment: "jsdom",
          setupFiles: ["./app/test/setup.ts"],
        },
      },
    ],
  },
});
