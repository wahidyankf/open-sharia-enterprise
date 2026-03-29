// Named token map: username -> access token
const tokenMap = new Map<string, string>();
// Named refresh token map: username -> refresh token
const refreshTokenMap = new Map<string, string>();
// Named ID map: username -> userId
const idMap = new Map<string, string>();
// Last created expense ID
let lastExpenseId: string | null = null;
// Last created attachment ID
let lastAttachmentId: string | null = null;
// Legacy single token (for backward compat)
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

export function setTokenForUser(username: string, token: string): void {
  tokenMap.set(username, token);
  storedToken = token;
}

export function getTokenForUser(username: string): string {
  const token = tokenMap.get(username);
  if (!token) {
    throw new Error(`No token stored for user "${username}". A login step must run first.`);
  }
  return token;
}

export function setRefreshTokenForUser(username: string, token: string): void {
  refreshTokenMap.set(username, token);
}

export function getRefreshTokenForUser(username: string): string {
  const token = refreshTokenMap.get(username);
  if (!token) {
    throw new Error(`No refresh token stored for user "${username}". A login step must run first.`);
  }
  return token;
}

export function setIdForUser(username: string, id: string): void {
  idMap.set(username, id);
}

export function getIdForUser(username: string): string {
  const id = idMap.get(username);
  if (!id) {
    throw new Error(`No ID stored for user "${username}". A registration step must run first.`);
  }
  return id;
}

export function setLastExpenseId(id: string): void {
  lastExpenseId = id;
}

export function getLastExpenseId(): string {
  if (!lastExpenseId) {
    throw new Error("No expense ID stored. An expense creation step must run first.");
  }
  return lastExpenseId;
}

export function setLastAttachmentId(id: string): void {
  lastAttachmentId = id;
}

export function getLastAttachmentId(): string {
  if (!lastAttachmentId) {
    throw new Error("No attachment ID stored. An attachment upload step must run first.");
  }
  return lastAttachmentId;
}

export function clearAll(): void {
  tokenMap.clear();
  refreshTokenMap.clear();
  idMap.clear();
  lastExpenseId = null;
  lastAttachmentId = null;
  storedToken = null;
}
