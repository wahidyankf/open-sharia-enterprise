"use client";

import { useState, use } from "react";
import { useRouter } from "next/navigation";
import { AppShell } from "@/components/layout/app-shell";
import { useExpense, useUpdateExpense, useDeleteExpense } from "@/lib/queries/use-expenses";
import { useAttachments, useUploadAttachment, useDeleteAttachment } from "@/lib/queries/use-attachments";
import { useCurrentUser } from "@/lib/queries/use-user";
import type { UpdateExpenseRequest } from "@/lib/api/types";
import { ApiError } from "@/lib/api/client";

const SUPPORTED_CURRENCIES = ["USD", "IDR"];
const SUPPORTED_UNITS = [
  "kg",
  "g",
  "mg",
  "lb",
  "oz",
  "l",
  "ml",
  "m",
  "cm",
  "km",
  "ft",
  "in",
  "unit",
  "pcs",
  "dozen",
  "box",
  "pack",
];
const EXPENSE_TYPES = ["income", "expense"];

const inputClassName = "w-full px-3 py-2 border border-gray-400 rounded text-[0.9rem] box-border";

const labelClassName = "block mb-[0.3rem] font-semibold text-[0.85rem]";

const cardClassName = "bg-white p-6 rounded-lg border border-gray-300 shadow-md mb-6";

interface PageProps {
  params: Promise<{ id: string }>;
}

export default function ExpenseDetailPage({ params }: PageProps) {
  const { id } = use(params);
  const router = useRouter();

  const { data: expense, isLoading, isError } = useExpense(id);
  const { data: attachments, isLoading: attachmentsLoading } = useAttachments(id);
  const { data: currentUser } = useCurrentUser();
  const updateMutation = useUpdateExpense();
  const deleteMutation = useDeleteExpense();
  const uploadMutation = useUploadAttachment();
  const deleteAttachmentMutation = useDeleteAttachment();

  const [isEditing, setIsEditing] = useState(false);
  const [editForm, setEditForm] = useState<UpdateExpenseRequest>({});
  const [updateError, setUpdateError] = useState<string | null>(null);
  const [deleteConfirm, setDeleteConfirm] = useState(false);
  const [deleteAttachmentId, setDeleteAttachmentId] = useState<string | null>(null);
  const [uploadError, setUploadError] = useState<string | null>(null);
  const [deleteAttachmentError, setDeleteAttachmentError] = useState<string | null>(null);

  const isOwner = currentUser?.id === expense?.userId;

  const handleStartEdit = () => {
    if (!expense) return;
    setEditForm({
      amount: expense.amount,
      currency: expense.currency,
      category: expense.category,
      description: expense.description,
      date: expense.date,
      type: expense.type,
      quantity: expense.quantity,
      unit: expense.unit,
    });
    setIsEditing(true);
  };

  const handleUpdate = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setUpdateError(null);

    const amountNum = parseFloat(editForm.amount ?? "0");
    if (editForm.amount !== undefined && (isNaN(amountNum) || amountNum < 0)) {
      setUpdateError("Amount must be a non-negative number.");
      return;
    }

    updateMutation.mutate(
      { id, data: editForm },
      {
        onSuccess: () => setIsEditing(false),
        onError: () => setUpdateError("Failed to update expense."),
      },
    );
  };

  const handleDelete = () => {
    deleteMutation.mutate(id, {
      onSuccess: () => router.push("/expenses"),
    });
  };

  const handleUpload = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;
    setUploadError(null);

    const MAX_SIZE = 10 * 1024 * 1024;
    const ALLOWED_TYPES = ["image/jpeg", "image/png", "image/gif", "image/webp", "application/pdf", "text/plain"];

    if (!ALLOWED_TYPES.includes(file.type)) {
      setUploadError("Unsupported file type. Please upload an image, PDF, or text file.");
      e.target.value = "";
      return;
    }
    if (file.size > MAX_SIZE) {
      setUploadError("File is too large. Maximum size is 10MB.");
      e.target.value = "";
      return;
    }

    uploadMutation.mutate(
      { expenseId: id, file },
      {
        onError: (err) => {
          if (err instanceof ApiError && err.status === 415) {
            setUploadError("Unsupported file type.");
          } else if (err instanceof ApiError && err.status === 413) {
            setUploadError("File is too large.");
          } else {
            setUploadError("Upload failed. Please try again.");
          }
          e.target.value = "";
        },
      },
    );
  };

  const handleDeleteAttachment = (attachmentId: string) => {
    setDeleteAttachmentError(null);
    deleteAttachmentMutation.mutate(
      { expenseId: id, attachmentId },
      {
        onSuccess: () => setDeleteAttachmentId(null),
        onError: (err) => {
          setDeleteAttachmentId(null);
          if (err instanceof ApiError && err.status === 404) {
            setDeleteAttachmentError("Attachment not found. It may have been deleted already.");
          } else {
            setDeleteAttachmentError("Failed to delete attachment. Please try again.");
          }
        },
      },
    );
  };

  const formatFileSize = (bytes: number): string => {
    if (bytes < 1024) return `${bytes} B`;
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
    return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
  };

  if (isLoading) {
    return (
      <AppShell>
        <p>Loading expense...</p>
      </AppShell>
    );
  }

  if (isError || !expense) {
    return (
      <AppShell>
        <p role="alert" className="text-red-700">
          Expense not found or failed to load.
        </p>
        <a href="/expenses" className="text-blue-600">
          Back to Expenses
        </a>
      </AppShell>
    );
  }

  return (
    <AppShell>
      <div className="mb-6 flex items-center justify-between">
        <div>
          <a href="/expenses" className="mb-1 block text-[0.9rem] text-blue-600">
            &#8592; Back to Expenses
          </a>
          <h1 className="m-0">{expense.description}</h1>
        </div>
        <div className="flex gap-2">
          {!isEditing && (
            <button
              onClick={handleStartEdit}
              className="cursor-pointer rounded border-none bg-blue-600 px-5 py-[0.6rem] font-semibold text-white"
            >
              Edit
            </button>
          )}
          <button
            onClick={() => setDeleteConfirm(true)}
            className="cursor-pointer rounded border-none bg-red-700 px-5 py-[0.6rem] font-semibold text-white"
          >
            Delete
          </button>
        </div>
      </div>

      {deleteConfirm && (
        <div
          role="alertdialog"
          aria-modal="true"
          aria-labelledby="delete-dialog-title"
          onKeyDown={(e) => {
            if (e.key === "Escape") {
              setDeleteConfirm(false);
              return;
            }
            if (e.key === "Tab") {
              const focusable = Array.from(
                (e.currentTarget as HTMLElement).querySelectorAll<HTMLElement>(
                  'button:not([disabled]), [href], input, select, textarea, [tabindex]:not([tabindex="-1"])',
                ),
              );
              const first = focusable[0];
              const last = focusable[focusable.length - 1];
              if (e.shiftKey) {
                if (document.activeElement === first) {
                  e.preventDefault();
                  last?.focus();
                }
              } else {
                if (document.activeElement === last) {
                  e.preventDefault();
                  first?.focus();
                }
              }
            }
          }}
          className="fixed inset-0 z-[300] flex items-center justify-center bg-black/40"
        >
          <div className="w-[22rem] rounded-lg bg-white p-6">
            <h2 id="delete-dialog-title" className="mt-0">
              Delete Expense
            </h2>
            <p>Are you sure you want to delete this expense?</p>
            <div className="flex gap-3">
              <button
                // eslint-disable-next-line jsx-a11y/no-autofocus
                autoFocus
                onClick={handleDelete}
                disabled={deleteMutation.isPending}
                className="cursor-pointer rounded border-none bg-red-700 px-4 py-2 font-semibold text-white"
              >
                {deleteMutation.isPending ? "Deleting..." : "Delete"}
              </button>
              <button
                onClick={() => setDeleteConfirm(false)}
                className="cursor-pointer rounded border border-gray-400 bg-white px-4 py-2 text-gray-800"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      )}

      {!isEditing ? (
        <div className={cardClassName}>
          <h2 className="mt-0">Details</h2>
          <dl>
            {[
              ["Amount", `${expense.currency} ${expense.amount}`],
              ["Type", expense.type],
              ["Category", expense.category],
              ["Date", expense.date],
              ["Quantity", expense.quantity ? String(expense.quantity) : "—"],
              ["Unit", expense.unit ?? "—"],
            ].map(([label, value]) => (
              <div key={label} className="mb-2 flex gap-4">
                <dt className="min-w-[8rem] font-semibold">{label}</dt>
                <dd className="m-0 text-gray-600">{value}</dd>
              </div>
            ))}
          </dl>
        </div>
      ) : (
        <div className={cardClassName}>
          <h2 className="mt-0">Edit Expense</h2>

          {updateError && (
            <div id="update-error" role="alert" className="mb-4 rounded bg-red-50 px-4 py-[0.6rem] text-red-700">
              {updateError}
            </div>
          )}

          <form onSubmit={handleUpdate} noValidate aria-describedby={updateError ? "update-error" : undefined}>
            <div className="mb-4 grid grid-cols-[repeat(auto-fill,minmax(200px,1fr))] gap-4">
              <div>
                <label htmlFor="edit-amount" className={labelClassName}>
                  Amount
                </label>
                <input
                  id="edit-amount"
                  type="number"
                  min="0"
                  step="0.01"
                  value={editForm.amount ?? ""}
                  onChange={(e) => setEditForm({ ...editForm, amount: e.target.value })}
                  className={inputClassName}
                />
              </div>

              <div>
                <label htmlFor="edit-currency" className={labelClassName}>
                  Currency
                </label>
                <input
                  id="edit-currency"
                  type="text"
                  list="edit-currency-list"
                  value={editForm.currency ?? "USD"}
                  onChange={(e) => setEditForm({ ...editForm, currency: e.target.value })}
                  className={inputClassName}
                />
                <datalist id="edit-currency-list">
                  {SUPPORTED_CURRENCIES.map((c) => (
                    <option key={c} value={c} />
                  ))}
                </datalist>
              </div>

              <div>
                <label htmlFor="edit-type" className={labelClassName}>
                  Type
                </label>
                <input
                  id="edit-type"
                  type="text"
                  list="edit-type-list"
                  value={editForm.type ?? "expense"}
                  onChange={(e) => setEditForm({ ...editForm, type: e.target.value as UpdateExpenseRequest["type"] })}
                  className={inputClassName}
                />
                <datalist id="edit-type-list">
                  {EXPENSE_TYPES.map((t) => (
                    <option key={t} value={t} />
                  ))}
                </datalist>
              </div>

              <div>
                <label htmlFor="edit-category" className={labelClassName}>
                  Category
                </label>
                <input
                  id="edit-category"
                  type="text"
                  value={editForm.category ?? ""}
                  onChange={(e) => setEditForm({ ...editForm, category: e.target.value })}
                  className={inputClassName}
                />
              </div>

              <div>
                <label htmlFor="edit-date" className={labelClassName}>
                  Date
                </label>
                <input
                  id="edit-date"
                  type="date"
                  value={editForm.date ?? ""}
                  onChange={(e) => setEditForm({ ...editForm, date: e.target.value })}
                  className={inputClassName}
                />
              </div>

              <div>
                <label htmlFor="edit-quantity" className={labelClassName}>
                  Quantity (optional)
                </label>
                <input
                  id="edit-quantity"
                  type="number"
                  min="0"
                  step="any"
                  value={editForm.quantity ?? ""}
                  onChange={(e) =>
                    setEditForm({
                      ...editForm,
                      quantity: e.target.value ? parseFloat(e.target.value) : undefined,
                    })
                  }
                  className={inputClassName}
                />
              </div>

              <div>
                <label htmlFor="edit-unit" className={labelClassName}>
                  Unit (optional)
                </label>
                <input
                  id="edit-unit"
                  type="text"
                  list="edit-unit-list"
                  value={editForm.unit ?? ""}
                  onChange={(e) => setEditForm({ ...editForm, unit: e.target.value || undefined })}
                  className={inputClassName}
                />
                <datalist id="edit-unit-list">
                  {SUPPORTED_UNITS.map((u) => (
                    <option key={u} value={u} />
                  ))}
                </datalist>
              </div>
            </div>

            <div className="mb-4">
              <label htmlFor="edit-description" className={labelClassName}>
                Description
              </label>
              <input
                id="edit-description"
                type="text"
                value={editForm.description ?? ""}
                onChange={(e) => setEditForm({ ...editForm, description: e.target.value })}
                className={inputClassName}
              />
            </div>

            <div className="flex gap-3">
              <button
                type="submit"
                disabled={updateMutation.isPending}
                className={`rounded border-none bg-blue-600 px-5 py-[0.6rem] font-semibold text-white ${
                  updateMutation.isPending ? "cursor-not-allowed" : "cursor-pointer"
                }`}
              >
                {updateMutation.isPending ? "Saving..." : "Save Changes"}
              </button>
              <button
                type="button"
                onClick={() => setIsEditing(false)}
                className="cursor-pointer rounded border border-gray-400 bg-white px-5 py-[0.6rem] text-gray-800"
              >
                Cancel
              </button>
            </div>
          </form>
        </div>
      )}

      <div className={cardClassName}>
        <h2 className="mt-0">Attachments</h2>

        {deleteAttachmentError && (
          <div role="alert" className="mb-4 rounded bg-red-50 px-4 py-[0.6rem] text-red-700">
            {deleteAttachmentError}
          </div>
        )}

        {uploadError && (
          <div id="upload-error" role="alert" className="mb-4 rounded bg-red-50 px-4 py-[0.6rem] text-red-700">
            {uploadError}
          </div>
        )}

        {isOwner && (
          <div className="mb-4">
            <label htmlFor="file-upload" className="mb-[0.4rem] block font-semibold">
              Upload Attachment
            </label>
            <input
              id="file-upload"
              type="file"
              onChange={handleUpload}
              disabled={uploadMutation.isPending}
              aria-describedby={uploadError ? "upload-error" : undefined}
              className="text-[0.9rem]"
              accept="image/*,.pdf,.txt"
            />
            {uploadMutation.isPending && <span className="ml-3 text-gray-500">Uploading...</span>}
          </div>
        )}

        {deleteAttachmentId && (
          <div
            role="alertdialog"
            aria-modal="true"
            aria-labelledby="del-attach-title"
            className="mb-4 rounded border border-red-200 bg-red-50 p-4"
          >
            <p id="del-attach-title" className="mt-0 font-semibold">
              Delete this attachment?
            </p>
            <div className="flex gap-3">
              <button
                onClick={() => handleDeleteAttachment(deleteAttachmentId)}
                disabled={deleteAttachmentMutation.isPending}
                className="cursor-pointer rounded border-none bg-red-700 px-[0.9rem] py-[0.4rem] font-semibold text-white"
              >
                {deleteAttachmentMutation.isPending ? "Deleting..." : "Delete"}
              </button>
              <button
                onClick={() => setDeleteAttachmentId(null)}
                className="cursor-pointer rounded border border-gray-400 bg-white px-[0.9rem] py-[0.4rem] text-gray-800"
              >
                Cancel
              </button>
            </div>
          </div>
        )}

        {attachmentsLoading && <p>Loading attachments...</p>}
        {attachments && attachments.length === 0 && <p className="text-gray-500">No attachments.</p>}
        {attachments && attachments.length > 0 && (
          <ul className="m-0 list-none p-0">
            {attachments.map((attachment) => (
              <li
                key={attachment.id}
                className="flex items-center justify-between border-b border-gray-200 py-[0.6rem]"
              >
                <div>
                  {attachment.contentType.startsWith("image/") && (
                    <img
                      src={`/api/v1/expenses/${id}/attachments/${attachment.id}/content`}
                      alt={`Attachment: ${attachment.filename}`}
                      width={120}
                      height={80}
                      className="mb-2 block rounded border border-gray-200 bg-gray-50 object-contain"
                    />
                  )}
                  <span className="font-medium">{attachment.filename}</span>
                  <span className="ml-3 text-[0.85rem] text-gray-500">
                    {attachment.contentType} &middot; {formatFileSize(attachment.size)}
                  </span>
                </div>
                {isOwner && (
                  <button
                    onClick={() => setDeleteAttachmentId(attachment.id)}
                    className="cursor-pointer rounded border-none bg-red-700 px-[0.6rem] py-[0.3rem] text-[0.8rem] text-white"
                    aria-label={`Delete attachment ${attachment.filename}`}
                  >
                    Delete
                  </button>
                )}
              </li>
            ))}
          </ul>
        )}
      </div>
    </AppShell>
  );
}
