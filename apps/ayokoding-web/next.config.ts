import type { NextConfig } from "next";
import path from "node:path";

const nextConfig: NextConfig = {
  output: "standalone",
  outputFileTracingRoot: path.join(__dirname, "../../"),
  outputFileTracingIncludes: {
    "/**": ["./content/**/*"],
  },
  serverExternalPackages: ["flexsearch"],
};

export default nextConfig;
