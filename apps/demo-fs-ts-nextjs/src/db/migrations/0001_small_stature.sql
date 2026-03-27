ALTER TABLE "revoked_tokens" DROP CONSTRAINT "revoked_tokens_user_id_users_id_fk";
--> statement-breakpoint
ALTER TABLE "expenses" ALTER COLUMN "currency" SET DATA TYPE varchar(10);--> statement-breakpoint
ALTER TABLE "expenses" ALTER COLUMN "description" SET DEFAULT '';--> statement-breakpoint
ALTER TABLE "expenses" ALTER COLUMN "type" SET DATA TYPE varchar(20);--> statement-breakpoint
ALTER TABLE "expenses" ALTER COLUMN "unit" SET DATA TYPE varchar(50);--> statement-breakpoint
ALTER TABLE "users" ALTER COLUMN "display_name" SET DEFAULT '';--> statement-breakpoint
ALTER TABLE "users" ALTER COLUMN "display_name" SET NOT NULL;--> statement-breakpoint
ALTER TABLE "users" ALTER COLUMN "created_by" SET NOT NULL;--> statement-breakpoint
ALTER TABLE "users" ALTER COLUMN "updated_by" SET NOT NULL;--> statement-breakpoint
ALTER TABLE "expenses" ADD COLUMN "created_by" varchar(255) DEFAULT 'system' NOT NULL;--> statement-breakpoint
ALTER TABLE "expenses" ADD COLUMN "updated_by" varchar(255) DEFAULT 'system' NOT NULL;--> statement-breakpoint
ALTER TABLE "expenses" ADD COLUMN "deleted_at" timestamp with time zone;--> statement-breakpoint
ALTER TABLE "expenses" ADD COLUMN "deleted_by" varchar(255);