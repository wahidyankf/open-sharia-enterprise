import type { AttachmentRepository, ExpenseRepository } from "@/repositories/interfaces";
import { ok, err, type ServiceResult, type Attachment, MAX_ATTACHMENT_SIZE } from "@/lib/types";

interface AttachmentDeps {
  attachments: AttachmentRepository;
  expenses: ExpenseRepository;
}

const ALLOWED_CONTENT_TYPES = ["image/jpeg", "image/png", "application/pdf"];

async function checkExpenseAccess(
  deps: AttachmentDeps,
  expenseId: string,
  userId: string,
): Promise<ServiceResult<true>> {
  const expense = await deps.expenses.findById(expenseId);
  if (!expense) return err("Expense not found", 404);
  if (expense.userId !== userId) return err("Forbidden", 403);
  return ok(true);
}

export async function uploadAttachment(
  deps: AttachmentDeps,
  expenseId: string,
  userId: string,
  file: { filename: string; contentType: string; size: number; data: Buffer },
): Promise<ServiceResult<Omit<Attachment, "data">>> {
  const access = await checkExpenseAccess(deps, expenseId, userId);
  if (!access.ok) return access;

  if (!ALLOWED_CONTENT_TYPES.includes(file.contentType)) {
    return err("Unsupported file type", 415);
  }

  if (file.size > MAX_ATTACHMENT_SIZE) {
    return err(`File size exceeds maximum of ${MAX_ATTACHMENT_SIZE / (1024 * 1024)}MB`, 413);
  }

  const attachment = await deps.attachments.create({
    expenseId,
    filename: file.filename,
    contentType: file.contentType,
    size: file.size,
    data: file.data,
  });

  return ok({
    id: attachment.id,
    expenseId: attachment.expenseId,
    filename: attachment.filename,
    contentType: attachment.contentType,
    size: attachment.size,
    createdAt: attachment.createdAt,
  });
}

export async function getAttachment(
  deps: AttachmentDeps,
  expenseId: string,
  attachmentId: string,
  userId: string,
): Promise<ServiceResult<Omit<Attachment, "data">>> {
  const access = await checkExpenseAccess(deps, expenseId, userId);
  if (!access.ok) return access;

  const attachment = await deps.attachments.findByIdAndExpenseId(attachmentId, expenseId);
  if (!attachment) return err("Attachment not found", 404);

  return ok({
    id: attachment.id,
    expenseId: attachment.expenseId,
    filename: attachment.filename,
    contentType: attachment.contentType,
    size: attachment.size,
    createdAt: attachment.createdAt,
  });
}

export async function listAttachments(
  deps: AttachmentDeps,
  expenseId: string,
  userId: string,
): Promise<ServiceResult<Omit<Attachment, "data">[]>> {
  const access = await checkExpenseAccess(deps, expenseId, userId);
  if (!access.ok) return access;

  const attachments = await deps.attachments.listByExpenseId(expenseId);
  return ok(
    attachments.map((a) => ({
      id: a.id,
      expenseId: a.expenseId,
      filename: a.filename,
      contentType: a.contentType,
      size: a.size,
      createdAt: a.createdAt,
    })),
  );
}

export async function deleteAttachment(
  deps: AttachmentDeps,
  expenseId: string,
  attachmentId: string,
  userId: string,
): Promise<ServiceResult<{ message: string }>> {
  const access = await checkExpenseAccess(deps, expenseId, userId);
  if (!access.ok) return access;

  const attachment = await deps.attachments.findByIdAndExpenseId(attachmentId, expenseId);
  if (!attachment) return err("Attachment not found", 404);

  await deps.attachments.delete(attachmentId);
  return ok({ message: "Attachment deleted" });
}
