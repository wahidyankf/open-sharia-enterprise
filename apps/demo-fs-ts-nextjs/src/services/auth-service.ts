import type { UserRepository, SessionRepository } from "@/repositories/interfaces";
import { hashPassword, verifyPassword } from "@/lib/password";
import { signAccessToken, signRefreshToken, verifyToken } from "@/lib/jwt";
import { validatePassword, validateEmail, validateUsername } from "@/lib/validation";
import { ok, err, type ServiceResult, MAX_FAILED_ATTEMPTS } from "@/lib/types";

interface AuthDeps {
  users: UserRepository;
  sessions: SessionRepository;
}

export interface AuthTokens {
  accessToken: string;
  refreshToken: string;
  tokenType: "Bearer";
}

export interface RegisterResult {
  id: string;
  username: string;
  email: string;
  displayName: string;
  role: string;
  status: string;
}

export async function register(
  deps: AuthDeps,
  data: { username: string; email: string; password: string },
): Promise<ServiceResult<RegisterResult>> {
  if (!data.username) return err("Username is required", 400);
  if (!data.email) return err("Email is required", 400);
  if (!data.password) return err("Password is required", 400);

  const usernameErr = validateUsername(data.username);
  if (usernameErr) return err(usernameErr, 400);

  const emailErr = validateEmail(data.email);
  if (emailErr) return err(emailErr, 400);

  const passwordErr = validatePassword(data.password);
  if (passwordErr) return err(passwordErr, 400);

  const existingByUsername = await deps.users.findByUsername(data.username);
  if (existingByUsername) return err("Username already exists", 409);

  const existingByEmail = await deps.users.findByEmail(data.email);
  if (existingByEmail) return err("Email already exists", 409);

  const passwordHash = await hashPassword(data.password);
  const user = await deps.users.create({
    username: data.username,
    email: data.email,
    passwordHash,
    displayName: data.username,
  });

  return ok({
    id: user.id,
    username: user.username,
    email: user.email,
    displayName: user.displayName,
    role: user.role,
    status: user.status,
  });
}

export async function login(
  deps: AuthDeps,
  data: { username: string; password: string },
): Promise<ServiceResult<AuthTokens>> {
  if (!data.username) return err("Username is required", 400);
  if (!data.password) return err("Password is required", 400);

  const user = await deps.users.findByUsername(data.username);
  if (!user) return err("Invalid credentials", 401);

  if (user.status === "DISABLED") return err("Account is disabled", 401);
  if (user.status === "INACTIVE") return err("Account is deactivated", 401);
  if (user.status === "LOCKED") {
    return err("Account is locked due to too many failed attempts", 401);
  }

  const valid = await verifyPassword(data.password, user.passwordHash);
  if (!valid) {
    await deps.users.incrementFailedAttempts(user.id);
    if (user.failedLoginAttempts + 1 >= MAX_FAILED_ATTEMPTS) {
      await deps.users.updateStatus(user.id, "LOCKED");
    }
    return err("Invalid credentials", 401);
  }

  await deps.users.resetFailedAttempts(user.id);

  const accessToken = await signAccessToken(user.id, user.username, user.role);
  const refreshToken = await signRefreshToken(user.id);

  return ok({ accessToken, refreshToken, tokenType: "Bearer" as const });
}

export async function logout(deps: AuthDeps, authHeader: string | null): Promise<ServiceResult<{ message: string }>> {
  if (!authHeader?.startsWith("Bearer ")) {
    return err("Missing Authorization header", 401);
  }

  const token = authHeader.slice(7);
  const claims = await verifyToken(token);
  if (!claims) return err("Invalid or expired token", 401);
  if (claims.tokenType !== "access") return err("Not an access token", 401);

  await deps.sessions.revokeAccessToken(claims.jti, claims.sub);
  return ok({ message: "Logged out successfully" });
}

export async function logoutAll(
  deps: AuthDeps,
  userId: string,
  jti: string,
): Promise<ServiceResult<{ message: string }>> {
  await deps.sessions.revokeAccessToken(jti, userId);
  await deps.sessions.revokeAllUserTokens(userId);
  return ok({ message: "All sessions logged out" });
}

export async function refresh(deps: AuthDeps, refreshTokenStr: string): Promise<ServiceResult<AuthTokens>> {
  if (!refreshTokenStr) return err("Missing refresh token", 401);

  const claims = await verifyToken(refreshTokenStr);
  if (!claims) return err("Invalid or expired token", 401);
  if (claims.tokenType !== "refresh") return err("Not a refresh token", 401);

  const user = await deps.users.findById(claims.sub);
  if (!user) return err("User not found", 401);
  if (user.status !== "ACTIVE") return err("Account is deactivated", 401);

  const isRevoked = await deps.sessions.isAccessTokenRevoked(claims.jti);
  if (isRevoked) return err("Token has been revoked", 401);

  await deps.sessions.revokeAccessToken(claims.jti, claims.sub);

  const newAccessToken = await signAccessToken(user.id, user.username, user.role);
  const newRefreshToken = await signRefreshToken(user.id);

  return ok({ accessToken: newAccessToken, refreshToken: newRefreshToken, tokenType: "Bearer" as const });
}
