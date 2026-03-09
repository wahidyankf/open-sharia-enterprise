import { Client } from "pg";

const DATABASE_URL = process.env.DATABASE_URL || "postgresql://organiclever:organiclever@localhost:5432/organiclever";

export async function cleanupDatabase(): Promise<void> {
  const client = new Client({ connectionString: DATABASE_URL });
  await client.connect();
  try {
    await client.query("DELETE FROM users");
  } finally {
    await client.end();
  }
}
