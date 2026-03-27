(ns demo-be-cjpd.db.protocols
  "Repository protocols for all database entities.
   Implementations: jdbc_user_repo, jdbc_expense_repo,
   jdbc_attachment_repo, jdbc_token_repo (production);
   in_memory_repos (unit tests).")

(defprotocol UserRepo
  "Operations on the users table."
  (count-users [this]
    "Return total count of users.")
  (find-user-by-id [this id]
    "Find a user by UUID string. Returns user map or nil.")
  (find-user-by-username [this username]
    "Find a user by username. Returns user map or nil.")
  (find-user-by-email [this email]
    "Find a user by email. Returns user map or nil.")
  (create-user! [this user]
    "Insert a new user. Returns created user map.")
  (update-display-name! [this user-id display-name]
    "Update display name. Returns updated user map.")
  (update-password! [this user-id password-hash]
    "Update password hash. Returns updated user map.")
  (update-status! [this user-id status]
    "Update user status. Returns updated user map.")
  (increment-failed-attempts! [this user-id]
    "Increment failed login attempts, potentially locking account. Returns updated user map.")
  (reset-failed-attempts! [this user-id]
    "Reset failed login attempts to zero. Returns updated user map.")
  (list-users [this opts]
    "Return paginated list of users. opts: {:search :page :size}.
     Returns {:data [...] :total N :page N :size N}."))

(defprotocol ExpenseRepo
  "Operations on the expenses table."
  (create-expense! [this expense]
    "Insert a new expense. Returns created expense map.")
  (find-expense-by-id [this id]
    "Find an expense by ID. Returns expense map or nil.")
  (find-expense-by-id-and-user [this id user-id]
    "Find an expense by ID and user ID. Returns expense map or nil.")
  (list-expenses-by-user [this user-id opts]
    "Return paginated expenses for a user. opts: {:page :size}.
     Returns {:data [...] :total N :page N :size N}.")
  (update-expense! [this id fields]
    "Update an expense. Returns updated expense map.")
  (delete-expense! [this id]
    "Delete an expense by ID.")
  (summary-by-user [this user-id]
    "Return expense totals grouped by currency for a user.")
  (pl-report [this user-id from-date to-date currency]
    "Return P&L report for a user filtered by date range and currency."))

(defprotocol AttachmentRepo
  "Operations on the attachments table."
  (create-attachment! [this attachment]
    "Insert a new attachment. Returns created attachment map.")
  (find-attachment-by-id [this id]
    "Find an attachment by ID (excluding binary data). Returns attachment map or nil.")
  (list-attachments-by-expense [this expense-id]
    "Return all attachments for an expense.")
  (delete-attachment! [this id]
    "Delete an attachment by ID.")
  (get-attachment-data [this id]
    "Return the raw binary data for an attachment."))

(defprotocol TokenRepo
  "Operations on the revoked_tokens table."
  (revoke-token! [this jti user-id]
    "Revoke a token by its JTI for a user.")
  (token-revoked? [this jti]
    "Return true if the given JTI has been revoked.")
  (revoke-all-for-user! [this user-id]
    "Revoke all tokens for a given user.")
  (all-revoked-for-user? [this user-id iat]
    "Return true if a logout-all was issued for user after iat timestamp."))
