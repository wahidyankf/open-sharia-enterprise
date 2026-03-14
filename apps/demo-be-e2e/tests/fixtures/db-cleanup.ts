const BASE_URL = process.env["BASE_URL"] ?? "http://localhost:8201";

export async function cleanupDatabase(): Promise<void> {
  const response = await fetch(`${BASE_URL}/api/v1/test/reset-db`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
  });
  if (!response.ok) {
    throw new Error(`cleanupDatabase failed: ${response.status} — ensure backend is running with ENABLE_TEST_API=true`);
  }
}

export async function setAdminRole(username: string): Promise<void> {
  const response = await fetch(`${BASE_URL}/api/v1/test/promote-admin`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ username }),
  });
  if (!response.ok) {
    throw new Error(`setAdminRole failed: ${response.status} — ensure backend is running with ENABLE_TEST_API=true`);
  }
}
