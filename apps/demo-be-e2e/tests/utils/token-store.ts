let storedToken: string | null = null;

export function setToken(token: string): void {
  storedToken = token;
}

export function getToken(): string {
  if (!storedToken) {
    throw new Error("No token stored. A login step must run first.");
  }
  return storedToken;
}

export function clearToken(): void {
  storedToken = null;
}
