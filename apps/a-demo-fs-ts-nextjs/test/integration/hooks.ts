import { BeforeAll, Before, AfterAll } from "@cucumber/cucumber";
import { db, ensureMigrations } from "../../src/db/client";
import { sql } from "drizzle-orm";

BeforeAll(async function () {
  await ensureMigrations();
});

Before(async function () {
  // Clear all tables before each scenario for isolation
  // Order matters: respect FK constraints
  await db.execute(sql`DELETE FROM "attachments"`);
  await db.execute(sql`DELETE FROM "revoked_tokens"`);
  await db.execute(sql`DELETE FROM "refresh_tokens"`);
  await db.execute(sql`DELETE FROM "expenses"`);
  await db.execute(sql`DELETE FROM "users"`);
});

AfterAll(async function () {
  // Give connections time to close
  await new Promise((resolve) => setTimeout(resolve, 200));
  process.exit(0);
});
