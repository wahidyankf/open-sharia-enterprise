import { setWorldConstructor, World } from "@cucumber/cucumber";
import { db } from "../../src/db/client";
import { createUserRepository } from "../../src/repositories/user-repository";
import { createSessionRepository } from "../../src/repositories/session-repository";
import { createExpenseRepository } from "../../src/repositories/expense-repository";
import { createAttachmentRepository } from "../../src/repositories/attachment-repository";
import type { Repositories } from "../../src/repositories/interfaces";
import { ServiceClient, type ServiceResponse } from "./service-client";

const repos: Repositories = {
  users: createUserRepository(db),
  sessions: createSessionRepository(db),
  expenses: createExpenseRepository(db),
  attachments: createAttachmentRepository(db),
};

const client = new ServiceClient(repos);

export class CustomWorld extends World {
  public response: ServiceResponse | null = null;
  public tokens: Map<string, string> = new Map();
  public userIds: Map<string, string> = new Map();
  public context: Record<string, unknown> = {};

  async dispatch(
    method: string,
    path: string,
    body: Record<string, unknown> | null,
    authHeader: string | null,
    file?: { filename: string; contentType: string; size: number; data: Buffer },
  ): Promise<ServiceResponse> {
    return client.dispatch(method, path, body, authHeader, file);
  }

  getAuth(username: string): string {
    const token = this.tokens.get(`${username}_access`);
    if (!token) throw new Error(`No access token for ${username}`);
    return `Bearer ${token}`;
  }

  async registerUser(username: string, email: string, password: string): Promise<void> {
    const resp = await this.dispatch("POST", "/api/v1/auth/register", { username, email, password }, null);
    if (resp.status !== 201) throw new Error(`Register failed: ${JSON.stringify(resp.body)}`);
    this.userIds.set(username, (resp.body as { id: string }).id);
  }

  async loginUser(username: string, password: string): Promise<void> {
    const resp = await this.dispatch("POST", "/api/v1/auth/login", { username, password }, null);
    if (resp.status !== 200) throw new Error(`Login failed: ${JSON.stringify(resp.body)}`);
    const body = resp.body as { accessToken: string; refreshToken: string };
    this.tokens.set(`${username}_access`, body.accessToken);
    this.tokens.set(`${username}_refresh`, body.refreshToken);
  }
}

setWorldConstructor(CustomWorld);
