import { reactRouter } from "@react-router/dev/vite";
import { defineConfig } from "vite";
import tsconfigPaths from "vite-tsconfig-paths";

export default defineConfig({
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  plugins: [reactRouter(), tsconfigPaths()] as any,
  server: {
    port: 3301,
    proxy: {
      "/api": process.env.BACKEND_URL || "http://localhost:8201",
      "/health": process.env.BACKEND_URL || "http://localhost:8201",
      "/.well-known": process.env.BACKEND_URL || "http://localhost:8201",
    },
  },
});
