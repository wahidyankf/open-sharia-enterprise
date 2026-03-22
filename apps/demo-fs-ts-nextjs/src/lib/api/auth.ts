import { apiFetch, setTokens } from "./client";
import type { AuthTokens, LoginRequest, RegisterRequest, HealthResponse } from "./types";

export function getHealth(): Promise<HealthResponse> {
  return apiFetch<HealthResponse>("/health");
}

export function register(data: RegisterRequest): Promise<void> {
  return apiFetch("/api/v1/auth/register", {
    method: "POST",
    body: JSON.stringify(data),
  });
}

export async function login(data: LoginRequest): Promise<AuthTokens> {
  const tokens = await apiFetch<AuthTokens>("/api/v1/auth/login", {
    method: "POST",
    body: JSON.stringify(data),
  });
  setTokens(tokens.accessToken, tokens.refreshToken);
  return tokens;
}

export function refreshToken(token: string): Promise<AuthTokens> {
  return apiFetch<AuthTokens>("/api/v1/auth/refresh", {
    method: "POST",
    body: JSON.stringify({ refreshToken: token }),
  });
}

export function logout(token: string): Promise<void> {
  return apiFetch("/api/v1/auth/logout", {
    method: "POST",
    body: JSON.stringify({ refreshToken: token }),
  });
}

export function logoutAll(): Promise<void> {
  return apiFetch("/api/v1/auth/logout-all", {
    method: "POST",
  });
}
