import { Client } from "pg";

const DATABASE_URL = process.env.DATABASE_URL || "postgresql://organiclever:organiclever@localhost:5432/demo_be";

export async function cleanupDatabase(): Promise<void> {
  const client = new Client({ connectionString: DATABASE_URL });
  await client.connect();
  try {
    // Delete in FK dependency order: child tables first
    await client.query("DELETE FROM attachments");
    await client.query("DELETE FROM expenses");
    await client.query("DELETE FROM revoked_tokens");
    await client.query("DELETE FROM refresh_tokens");
    await client.query("DELETE FROM users");
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
