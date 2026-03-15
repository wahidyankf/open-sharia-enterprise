import { apiFetch, getAccessToken, ApiError } from "./client";
import type { Attachment } from "./types";

export async function listAttachments(expenseId: string): Promise<Attachment[]> {
  const result = await apiFetch<{ attachments: Attachment[] } | Attachment[]>(
    `/api/v1/expenses/${expenseId}/attachments`,
  );
  // Backend returns { attachments: [...] } wrapper
  if (result && !Array.isArray(result) && "attachments" in result) {
    return result.attachments;
  }
  return Array.isArray(result) ? result : [];
}

export async function uploadAttachment(expenseId: string, file: File): Promise<Attachment> {
  const formData = new FormData();
  formData.append("file", file);

  const token = getAccessToken();
  const headers: Record<string, string> = {};
  if (token) headers["Authorization"] = `Bearer ${token}`;

  const res = await fetch(`/api/v1/expenses/${expenseId}/attachments`, {
    method: "POST",
    headers,
    body: formData,
  });

  if (!res.ok) {
    const body = await res.json().catch(() => null);
    throw new ApiError(res.status, body);
  }

  return res.json() as Promise<Attachment>;
}

export function deleteAttachment(expenseId: string, attachmentId: string): Promise<void> {
  return apiFetch(`/api/v1/expenses/${expenseId}/attachments/${attachmentId}`, { method: "DELETE" });
}
