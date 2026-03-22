import type { UserRepository, SessionRepository } from "@/repositories/interfaces";
import { hashPassword, verifyPassword } from "@/lib/password";
import { validatePassword } from "@/lib/validation";
import { ok, err, type ServiceResult, type User, type PagedResult } from "@/lib/types";

interface UserDeps {
  users: UserRepository;
  sessions: SessionRepository;
}

export interface UserProfile {
  id: string;
  username: string;
  email: string;
  displayName: string | null;
  role: string;
  status: string;
}

function toProfile(user: User): UserProfile {
  return {
    id: user.id,
    username: user.username,
    email: user.email,
    displayName: user.displayName,
    role: user.role,
    status: user.status,
  };
}

export async function getProfile(deps: UserDeps, userId: string): Promise<ServiceResult<UserProfile>> {
  const user = await deps.users.findById(userId);
  if (!user) return err("User not found", 404);
  return ok(toProfile(user));
}

export async function updateDisplayName(
  deps: UserDeps,
  userId: string,
  displayName: string,
): Promise<ServiceResult<UserProfile>> {
  if (!displayName) return err("Display name is required", 400);
  await deps.users.updateDisplayName(userId, displayName);
  const user = await deps.users.findById(userId);
  if (!user) return err("User not found", 404);
  return ok(toProfile(user));
}

export async function changePassword(
  deps: UserDeps,
  userId: string,
  data: { currentPassword: string; newPassword: string },
): Promise<ServiceResult<{ message: string }>> {
  if (!data.currentPassword) return err("Current password is required", 400);
  if (!data.newPassword) return err("New password is required", 400);

  const passwordErr = validatePassword(data.newPassword);
  if (passwordErr) return err(passwordErr, 400);

  const user = await deps.users.findById(userId);
  if (!user) return err("User not found", 404);

  const valid = await verifyPassword(data.currentPassword, user.passwordHash);
  if (!valid) return err("Current password is incorrect", 401);

  const newHash = await hashPassword(data.newPassword);
  await deps.users.updatePassword(userId, newHash);
  return ok({ message: "Password changed successfully" });
}

export async function deactivateAccount(deps: UserDeps, userId: string): Promise<ServiceResult<{ message: string }>> {
  await deps.users.updateStatus(userId, "INACTIVE");
  await deps.sessions.revokeAllUserTokens(userId);
  return ok({ message: "Account deactivated" });
}

export async function listUsers(
  deps: UserDeps,
  page: number,
  size: number,
  email?: string,
): Promise<ServiceResult<PagedResult<UserProfile>>> {
  const safePage = Math.max(page, 1);
  const result = await deps.users.listUsers(safePage, size, email);
  return ok({
    items: result.items.map(toProfile),
    total: result.total,
    page: safePage,
    size,
  });
}

export async function disableUser(deps: UserDeps, targetId: string): Promise<ServiceResult<{ message: string }>> {
  const user = await deps.users.findById(targetId);
  if (!user) return err("User not found", 404);
  await deps.users.updateStatus(targetId, "DISABLED");
  return ok({ message: "User disabled" });
}

export async function enableUser(deps: UserDeps, targetId: string): Promise<ServiceResult<{ message: string }>> {
  const user = await deps.users.findById(targetId);
  if (!user) return err("User not found", 404);
  await deps.users.updateStatus(targetId, "ACTIVE");
  return ok({ message: "User enabled" });
}

export async function unlockUser(deps: UserDeps, targetId: string): Promise<ServiceResult<{ message: string }>> {
  const user = await deps.users.findById(targetId);
  if (!user) return err("User not found", 404);
  await deps.users.updateStatus(targetId, "ACTIVE");
  await deps.users.resetFailedAttempts(targetId);
  return ok({ message: "User unlocked" });
}

export async function forcePasswordReset(
  deps: UserDeps,
  targetId: string,
): Promise<ServiceResult<{ resetToken: string }>> {
  const user = await deps.users.findById(targetId);
  if (!user) return err("User not found", 404);
  const resetToken = crypto.randomUUID();
  await deps.users.updatePasswordResetToken(targetId, resetToken);
  return ok({ resetToken });
}
