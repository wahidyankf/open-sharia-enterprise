import { setWorldConstructor, World } from "@cucumber/cucumber";
import { dispatchRequest, uploadAttachment } from "./service-layer.js";

export interface HttpResponse {
  readonly status: number;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly body: any;
  readonly headers: Record<string, string>;
}

/**
 * CustomWorld provides an HTTP-like interface over the Effect service layer.
 *
 * All methods previously used fetch() to call a real HTTP server. They now
 * call Effect service functions directly through the ManagedRuntime created
 * in hooks.ts. The response shape (status, body, headers) is preserved so
 * that all existing step definitions continue to work without changes.
 *
 * No HTTP server is started. No fetch() calls are made.
 */
export class CustomWorld extends World {
  public response: HttpResponse | null = null;
  public tokens: Map<string, string> = new Map();
  public userIds: Map<string, string> = new Map();
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  public context: Record<string, any> = {};

  async get(path: string, token?: string): Promise<HttpResponse> {
    const authHeader = token ? `Bearer ${token}` : undefined;
    return dispatchRequest("GET", path, {}, authHeader);
  }

  async post(
    path: string,
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    body: any,
    token?: string,
  ): Promise<HttpResponse> {
    const authHeader = token ? `Bearer ${token}` : undefined;
    return dispatchRequest("POST", path, body as Record<string, unknown>, authHeader);
  }

  async patch(
    path: string,
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    body: any,
    token?: string,
  ): Promise<HttpResponse> {
    const authHeader = token ? `Bearer ${token}` : undefined;
    return dispatchRequest("PATCH", path, body as Record<string, unknown>, authHeader);
  }

  async put(
    path: string,
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    body: any,
    token?: string,
  ): Promise<HttpResponse> {
    const authHeader = token ? `Bearer ${token}` : undefined;
    return dispatchRequest("PUT", path, body as Record<string, unknown>, authHeader);
  }

  async delete(path: string, token?: string): Promise<HttpResponse> {
    const authHeader = token ? `Bearer ${token}` : undefined;
    return dispatchRequest("DELETE", path, {}, authHeader);
  }

  async uploadFile(
    path: string,
    filename: string,
    contentType: string,
    content: Buffer,
    token?: string,
  ): Promise<HttpResponse> {
    // Extract the expenseId from the path: /api/v1/expenses/:expenseId/attachments
    const match = path.match(/\/api\/v1\/expenses\/([^/]+)\/attachments/);
    if (!match) {
      return { status: 400, body: { error: "Invalid upload path" }, headers: {} };
    }
    const expenseId = match[1]!;
    const authHeader = token ? `Bearer ${token}` : undefined;
    return uploadAttachment(authHeader, expenseId, filename, contentType, content);
  }
}

setWorldConstructor(CustomWorld);
