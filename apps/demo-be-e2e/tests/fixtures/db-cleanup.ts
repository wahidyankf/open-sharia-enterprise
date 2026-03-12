import { Client } from "pg";

const DATABASE_URL = process.env.DATABASE_URL || "postgresql://organiclever:organiclever@localhost:5432/demo_be";

export async function cleanupDatabase(): Promise<void> {
  const client = new Client({ connectionString: DATABASE_URL });
  await client.connect();
  try {
    // Delete in FK dependency order; skip tables that don't exist (42P01).
    const tables = ["attachments", "expenses", "revoked_tokens", "refresh_tokens", "users"];
    for (const table of tables) {
      await client.query(`DELETE FROM ${table}`).catch((err: { code?: string }) => {
        if (err.code !== "42P01") throw err;
      });
    }
  } finally {
    await client.end();
  }
}

export async function setAdminRole(username: string): Promise<void> {
  const client = new Client({ connectionString: DATABASE_URL });
  await client.connect();
  try {
    await client.query("UPDATE users SET role = 'ADMIN' WHERE username = $1", [username]);
  } finally {
    await client.end();
  }
}
