import type { Repositories } from "@/repositories/interfaces";
import { createInMemoryRepositories } from "./in-memory-repos";
import { ServiceClient, type ServiceResponse } from "./service-client";

export interface TestContext {
  repos: Repositories;
  client: ServiceClient;
  tokens: Map<string, string>;
  userIds: Map<string, string>;
  context: Record<string, unknown>;
  response: ServiceResponse | null;
}

export function createTestContext(): TestContext {
  const repos = createInMemoryRepositories();
  return {
    repos,
    client: new ServiceClient(repos),
    tokens: new Map(),
    userIds: new Map(),
    context: {},
    response: null,
  };
}

export function resetTestContext(ctx: TestContext): void {
  const repos = createInMemoryRepositories();
  ctx.repos = repos;
  ctx.client = new ServiceClient(repos);
  ctx.tokens = new Map();
  ctx.userIds = new Map();
  ctx.context = {};
  ctx.response = null;
}

export async function registerUser(ctx: TestContext, username: string, email: string, password: string): Promise<void> {
  const resp = await ctx.client.dispatch("POST", "/api/v1/auth/register", { username, email, password }, null);
  if (resp.status !== 201) throw new Error(`Failed to register ${username}: ${JSON.stringify(resp.body)}`);
  const body = resp.body as { id: string };
  ctx.userIds.set(username, body.id);
}

export async function loginUser(ctx: TestContext, username: string, password: string): Promise<void> {
  const resp = await ctx.client.dispatch("POST", "/api/v1/auth/login", { username, password }, null);
  if (resp.status !== 200) throw new Error(`Failed to login ${username}: ${JSON.stringify(resp.body)}`);
  const body = resp.body as { accessToken: string; refreshToken: string };
  ctx.tokens.set(`${username}_access`, body.accessToken);
  ctx.tokens.set(`${username}_refresh`, body.refreshToken);
}

export function getAuth(ctx: TestContext, username: string): string {
  const token = ctx.tokens.get(`${username}_access`);
  if (!token) throw new Error(`No access token for ${username}`);
  return `Bearer ${token}`;
}
