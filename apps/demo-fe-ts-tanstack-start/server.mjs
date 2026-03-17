import { createServer } from "node:http";
import { request as httpRequest } from "node:http";
import { request as httpsRequest } from "node:https";
import { readFileSync, existsSync } from "node:fs";
import { join, extname } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = fileURLToPath(new URL(".", import.meta.url));
const distDir = join(__dirname, "dist");
const BACKEND_URL = process.env.BACKEND_URL || "http://localhost:8201";
const PORT = parseInt(process.env.PORT || "3301", 10);

const mimeTypes = {
  ".html": "text/html",
  ".js": "application/javascript",
  ".css": "text/css",
  ".json": "application/json",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".svg": "image/svg+xml",
  ".ico": "image/x-icon",
  ".woff": "font/woff",
  ".woff2": "font/woff2",
};

function proxyRequest(req, res, targetPath) {
  const url = new URL(targetPath, BACKEND_URL);
  const requester = url.protocol === "https:" ? httpsRequest : httpRequest;

  const options = {
    hostname: url.hostname,
    port: url.port || (url.protocol === "https:" ? 443 : 80),
    path: url.pathname + url.search,
    method: req.method,
    headers: { ...req.headers, host: url.host },
  };

  const proxyReq = requester(options, (proxyRes) => {
    res.writeHead(proxyRes.statusCode || 500, proxyRes.headers);
    proxyRes.pipe(res);
  });

  proxyReq.on("error", () => {
    res.writeHead(502);
    res.end("Bad Gateway");
  });

  req.pipe(proxyReq);
}

const server = createServer((req, res) => {
  const pathname = new URL(req.url || "/", `http://localhost:${PORT}`).pathname;

  // Proxy API, health, and well-known requests
  if (pathname.startsWith("/api/") || pathname === "/health" || pathname.startsWith("/.well-known/")) {
    proxyRequest(req, res, req.url);
    return;
  }

  // Serve static files
  const filePath = join(distDir, pathname);
  if (existsSync(filePath) && !pathname.endsWith("/")) {
    const ext = extname(filePath);
    const contentType = mimeTypes[ext] || "application/octet-stream";
    res.writeHead(200, { "Content-Type": contentType });
    res.end(readFileSync(filePath));
    return;
  }

  // SPA fallback - serve index.html
  const indexPath = join(distDir, "index.html");
  res.writeHead(200, { "Content-Type": "text/html" });
  res.end(readFileSync(indexPath));
});

server.listen(PORT, () => {
  console.log(`Server running on http://localhost:${PORT}`);
});
