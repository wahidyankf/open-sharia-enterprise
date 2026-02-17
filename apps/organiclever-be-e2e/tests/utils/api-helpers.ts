import { APIRequestContext } from "@playwright/test";

export async function getJson(request: APIRequestContext, path: string) {
  const response = await request.get(path);
  return { response, body: await response.json() };
}
