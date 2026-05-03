/**
 * Shared E2E helper for OrganicLever web. Centralises the base URL and
 * exposes a `appPath` builder for the URL-routed shell so each step file
 * does not hard-code the base URL.
 *
 * Reads `WEB_BASE_URL` (same env var Playwright's `baseURL` reads) so step
 * files can run against staging via `vars.WEB_BASE_URL`. Falls back to
 * `http://localhost:3200` for local dev. Trailing slash trimmed so concat
 * never produces `//app/...`.
 */

const RAW_BASE_URL = process.env.WEB_BASE_URL || "http://localhost:3200";
export const APP_BASE_URL = RAW_BASE_URL.replace(/\/+$/, "");

/**
 * Build a fully-qualified URL for a /app/<tab> path or any sub-path under /app.
 * Pass the segment after `/app/` (e.g. `home`, `history`, `workout/finish`).
 *
 * Examples:
 *   appPath("home")          → "http://localhost:3200/app/home"
 *   appPath("workout/finish")→ "http://localhost:3200/app/workout/finish"
 *   appPath("")              → "http://localhost:3200/app/home" (default tab)
 */
export function appPath(segment: string = "home"): string {
  const trimmed = segment.replace(/^\/+/, "").replace(/\/+$/, "");
  if (trimmed === "") return `${APP_BASE_URL}/app/home`;
  return `${APP_BASE_URL}/app/${trimmed}`;
}

/** Bare /app URL — used to assert the 308 redirect to /app/home. */
export const APP_BARE_URL = `${APP_BASE_URL}/app`;
