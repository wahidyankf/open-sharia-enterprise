import type { User, Expense, Attachment, RefreshToken, PagedResult, UserStatus } from "@/lib/types";

export interface UserRepository {
  create(data: { username: string; email: string; passwordHash: string; displayName: string }): Promise<User>;
  findByUsername(username: string): Promise<User | null>;
  findByEmail(email: string): Promise<User | null>;
  findById(id: string): Promise<User | null>;
  updateStatus(id: string, status: UserStatus): Promise<void>;
  updateDisplayName(id: string, displayName: string): Promise<void>;
  updatePassword(id: string, passwordHash: string): Promise<void>;
  updatePasswordResetToken(id: string, token: string | null): Promise<void>;
  incrementFailedAttempts(id: string): Promise<void>;
  resetFailedAttempts(id: string): Promise<void>;
  listUsers(page: number, size: number, email?: string): Promise<PagedResult<User>>;
  deleteAll(): Promise<void>;
}

export interface SessionRepository {
  createRefreshToken(data: { userId: string; tokenHash: string; expiresAt: Date }): Promise<RefreshToken>;
  findRefreshTokenByHash(hash: string): Promise<RefreshToken | null>;
  revokeRefreshToken(id: string): Promise<void>;
  revokeAllUserTokens(userId: string): Promise<void>;
  revokeAccessToken(jti: string, userId: string): Promise<void>;
  isAccessTokenRevoked(jti: string): Promise<boolean>;
  deleteAll(): Promise<void>;
}

export interface ExpenseRepository {
  create(data: {
    userId: string;
    amount: string;
    currency: string;
    category: string;
    description: string;
    date: string;
    type: string;
    quantity?: string;
    unit?: string;
  }): Promise<Expense>;
  findById(id: string): Promise<Expense | null>;
  findByIdAndUserId(id: string, userId: string): Promise<Expense | null>;
  update(
    id: string,
    data: {
      amount: string;
      currency: string;
      category: string;
      description: string;
      date: string;
      type: string;
      quantity?: string | null;
      unit?: string | null;
    },
  ): Promise<Expense>;
  delete(id: string): Promise<void>;
  listByUserId(userId: string, page: number, size: number): Promise<PagedResult<Expense>>;
  summaryByUserId(userId: string): Promise<{ currency: string; totalIncome: string; totalExpense: string }[]>;
  findByUserIdFiltered(
    userId: string,
    from?: string,
    to?: string,
    currency?: string,
  ): Promise<import("@/lib/types").Expense[]>;
  deleteAll(): Promise<void>;
}

export interface AttachmentRepository {
  create(data: {
    expenseId: string;
    filename: string;
    contentType: string;
    size: number;
    data: Buffer;
  }): Promise<Attachment>;
  findById(id: string): Promise<Attachment | null>;
  findByIdAndExpenseId(id: string, expenseId: string): Promise<Attachment | null>;
  listByExpenseId(expenseId: string): Promise<Attachment[]>;
  delete(id: string): Promise<void>;
  deleteAll(): Promise<void>;
}

export interface Repositories {
  users: UserRepository;
  sessions: SessionRepository;
  expenses: ExpenseRepository;
  attachments: AttachmentRepository;
}
