const BASE_URL = process.env["BASE_URL"] ?? "http://localhost:8201";
const DATABASE_URL = process.env["DATABASE_URL"];

export async function cleanupDatabase(): Promise<void> {
  if (DATABASE_URL) {
    // Direct DB cleanup for backends without test API (DATABASE_URL set in workflow)
    const { Client } = await import("pg");
    const client = new Client({ connectionString: DATABASE_URL });
    await client.connect();
    try {
      const tables = ["attachments", "expenses", "revoked_tokens", "refresh_tokens", "users"];
      for (const table of tables) {
        await client.query(`DELETE FROM ${table}`).catch((err: { code?: string }) => {
          if (err.code !== "42P01") throw err;
        });
      }
    } finally {
      await client.end();
    }
  } else {
    // Test API cleanup (requires backend running with ENABLE_TEST_API=true)
    const response = await fetch(`${BASE_URL}/api/v1/test/reset-db`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
    });
    if (!response.ok) {
      throw new Error(
        `cleanupDatabase failed: ${response.status} — ensure backend is running with ENABLE_TEST_API=true`,
      );
    }
  }
}

export async function setAdminRole(username: string): Promise<void> {
  if (DATABASE_URL) {
    // Direct DB update for backends without test API
    const { Client } = await import("pg");
    const client = new Client({ connectionString: DATABASE_URL });
    await client.connect();
    try {
      await client.query("UPDATE users SET role = 'ADMIN' WHERE username = $1", [username]);
    } finally {
      await client.end();
    }
  } else {
    // Test API admin promotion (requires ENABLE_TEST_API=true)
    const response = await fetch(`${BASE_URL}/api/v1/test/promote-admin`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ username }),
    });
    if (!response.ok) {
      throw new Error(`setAdminRole failed: ${response.status} — ensure backend is running with ENABLE_TEST_API=true`);
    }
  }
}
