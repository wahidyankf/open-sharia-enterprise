/**
 * Migration record for use in tests and environments where filesystem
 * access is unavailable. Each key must match the pattern `\d+_<name>`.
 *
 * For production, migrations are loaded via fromFileSystem pointing to
 * this directory so that the migrator discovers them automatically.
 */
import m001 from "./001_create_users.js";
import m002 from "./002_create_refresh_tokens.js";
import m003 from "./003_create_revoked_tokens.js";
import m004 from "./004_create_expenses.js";
import m005 from "./005_create_attachments.js";

export const migrations = {
  "0001_create_users": m001,
  "0002_create_refresh_tokens": m002,
  "0003_create_revoked_tokens": m003,
  "0004_create_expenses": m004,
  "0005_create_attachments": m005,
};
