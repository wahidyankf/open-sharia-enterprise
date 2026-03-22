import { eq, and } from "drizzle-orm";
import type { Database } from "@/db/client";
import { attachments } from "@/db/schema";
import type { AttachmentRepository } from "./interfaces";
import type { Attachment } from "@/lib/types";

function rowToAttachment(row: typeof attachments.$inferSelect): Attachment {
  return {
    id: row.id,
    expenseId: row.expenseId,
    filename: row.filename,
    contentType: row.contentType,
    size: row.size,
    data: row.data,
    createdAt: row.createdAt,
  };
}

export function createAttachmentRepository(db: Database): AttachmentRepository {
  return {
    async create(data) {
      const [row] = await db
        .insert(attachments)
        .values({
          expenseId: data.expenseId,
          filename: data.filename,
          contentType: data.contentType,
          size: data.size,
          data: data.data,
        })
        .returning();
      return rowToAttachment(row!);
    },

    async findById(id) {
      const [row] = await db.select().from(attachments).where(eq(attachments.id, id));
      return row ? rowToAttachment(row) : null;
    },

    async findByIdAndExpenseId(id, expenseId) {
      const [row] = await db
        .select()
        .from(attachments)
        .where(and(eq(attachments.id, id), eq(attachments.expenseId, expenseId)));
      return row ? rowToAttachment(row) : null;
    },

    async listByExpenseId(expenseId) {
      const rows = await db
        .select()
        .from(attachments)
        .where(eq(attachments.expenseId, expenseId))
        .orderBy(attachments.createdAt);
      return rows.map(rowToAttachment);
    },

    async delete(id) {
      await db.delete(attachments).where(eq(attachments.id, id));
    },

    async deleteAll() {
      await db.delete(attachments);
    },
  };
}
