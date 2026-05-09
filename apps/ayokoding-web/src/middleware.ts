export { middleware } from "./contexts/i18n/application/middleware";

// Next.js 16 statically analyzes `config` and rejects re-exports.
// Per tech-docs.md §Risk and rollback fallback, define config inline here.
// Implementation of the middleware function itself stays in i18n BC.
export const config = {
  matcher: ["/((?!_next/static|_next/image|favicon.ico|favicon.png).*)"],
};
