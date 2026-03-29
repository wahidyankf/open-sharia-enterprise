CREATE TABLE IF NOT EXISTS attachments (
  id TEXT PRIMARY KEY,
  expense_id TEXT NOT NULL,
  user_id TEXT NOT NULL,
  filename TEXT NOT NULL,
  content_type TEXT NOT NULL,
  size INTEGER NOT NULL,
  data BYTEA NOT NULL,
  created_at TEXT NOT NULL
);
