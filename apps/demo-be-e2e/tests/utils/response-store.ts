import type { APIResponse } from "@playwright/test";
import { validateResponseAgainstContract } from "./contract-validator";

let response: APIResponse | null = null;

export function setResponse(r: APIResponse): void {
  response = r;
}

export function getResponse(): APIResponse {
  if (!response) {
    throw new Error("No response stored. A When step must run before Then steps.");
  }
  return response;
}

export function clearResponse(): void {
  response = null;
}

/**
 * Validate the stored response against the OpenAPI contract.
 * Call this after checking the status code in Then steps.
 * Returns null if valid or no schema exists, or an error string.
 */
export async function validateStoredResponse(
  method: string,
  path: string,
): Promise<string | null> {
  const res = getResponse();
  const status = res.status();
  if (status < 200 || status >= 300) return null;
  try {
    const body = await res.json();
    return validateResponseAgainstContract(path, method, status, body);
  } catch {
    return null; // No JSON body (e.g., 204)
  }
}
