CREATE TABLE IF NOT EXISTS revoked_tokens (
  id TEXT PRIMARY KEY,
  jti TEXT NOT NULL UNIQUE,
  user_id TEXT NOT NULL,
  revoked_at TEXT NOT NULL
);
