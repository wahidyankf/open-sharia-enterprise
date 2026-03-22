export type Role = "USER" | "ADMIN";
export type UserStatus = "ACTIVE" | "INACTIVE" | "DISABLED" | "LOCKED";
export type EntryType = "INCOME" | "EXPENSE";

export const SUPPORTED_CURRENCIES = ["USD", "IDR"] as const;
export type SupportedCurrency = (typeof SUPPORTED_CURRENCIES)[number];

export const CURRENCY_DECIMALS: Record<SupportedCurrency, number> = {
  USD: 2,
  IDR: 0,
};

export const SUPPORTED_UNITS = [
  "liter",
  "ml",
  "kg",
  "g",
  "km",
  "meter",
  "gallon",
  "lb",
  "oz",
  "mile",
  "piece",
  "hour",
] as const;
export type SupportedUnit = (typeof SUPPORTED_UNITS)[number];

export const MAX_FAILED_ATTEMPTS = 5;
export const TOKEN_EXPIRY_MINUTES = 15;
export const REFRESH_TOKEN_EXPIRY_DAYS = 7;
export const MAX_ATTACHMENT_SIZE = 10 * 1024 * 1024; // 10MB

export interface User {
  readonly id: string;
  readonly username: string;
  readonly email: string;
  readonly passwordHash: string;
  readonly displayName: string | null;
  readonly role: Role;
  readonly status: UserStatus;
  readonly failedLoginAttempts: number;
  readonly passwordResetToken: string | null;
  readonly createdAt: Date;
  readonly updatedAt: Date;
}

export interface Expense {
  readonly id: string;
  readonly userId: string;
  readonly amount: string;
  readonly currency: string;
  readonly category: string;
  readonly description: string;
  readonly date: string;
  readonly type: string;
  readonly quantity: string | null;
  readonly unit: string | null;
  readonly createdAt: Date;
  readonly updatedAt: Date;
}

export interface Attachment {
  readonly id: string;
  readonly expenseId: string;
  readonly filename: string;
  readonly contentType: string;
  readonly size: number;
  readonly data: Buffer;
  readonly createdAt: Date;
}

export interface RefreshToken {
  readonly id: string;
  readonly userId: string;
  readonly tokenHash: string;
  readonly expiresAt: Date;
  readonly revoked: boolean;
  readonly createdAt: Date;
}

export interface PagedResult<T> {
  readonly items: readonly T[];
  readonly total: number;
  readonly page: number;
  readonly size: number;
}

export type ServiceResult<T> = { ok: true; data: T } | { ok: false; error: string; status: number };

export function ok<T>(data: T): ServiceResult<T> {
  return { ok: true, data };
}

export function err<T>(error: string, status: number): ServiceResult<T> {
  return { ok: false, error, status };
}
