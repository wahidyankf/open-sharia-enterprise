import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { TanStackRouterVite } from "@tanstack/router-vite-plugin";
import tsconfigPaths from "vite-tsconfig-paths";

export default defineConfig({
  plugins: [
    TanStackRouterVite({
      routesDirectory: "./src/routes",
      generatedRouteTree: "./src/routeTree.gen.ts",
    }),
    react(),
    tsconfigPaths(),
  ],
  server: {
    proxy: {
      "/api": {
        target: process.env.BACKEND_URL || "http://localhost:8201",
        changeOrigin: true,
      },
      "/health": {
        target: process.env.BACKEND_URL || "http://localhost:8201",
        changeOrigin: true,
      },
      "/.well-known": {
        target: process.env.BACKEND_URL || "http://localhost:8201",
        changeOrigin: true,
      },
    },
  },
});
