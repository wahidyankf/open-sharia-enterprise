import { defineConfig } from "vite";
import { tanstackStart } from "@tanstack/react-start/plugin/vite";
import { resolve } from "path";

export default defineConfig({
  resolve: {
    alias: {
      "~": resolve(__dirname, "app"),
    },
  },
  server: {
    port: 3301,
    proxy: {
      "/api": process.env.BACKEND_URL || "http://localhost:8201",
      "/health": process.env.BACKEND_URL || "http://localhost:8201",
      "/.well-known": process.env.BACKEND_URL || "http://localhost:8201",
    },
  },
  plugins: [tanstackStart({ srcDirectory: "app" })],
});
