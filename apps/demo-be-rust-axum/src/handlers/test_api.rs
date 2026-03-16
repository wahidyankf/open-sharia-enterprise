use axum::{extract::State, http::StatusCode, response::IntoResponse, Json};
use serde::Deserialize;
use serde_json::json;
use std::sync::Arc;

use crate::domain::errors::AppError;
use crate::state::AppState;

/// Deletes all data in dependency order:
/// attachments → expenses → refresh_tokens → revoked_tokens → users.
pub async fn reset_db(State(state): State<Arc<AppState>>) -> Result<impl IntoResponse, AppError> {
    sqlx::query("DELETE FROM attachments")
        .execute(&state.pool)
        .await?;
    sqlx::query("DELETE FROM expenses")
        .execute(&state.pool)
        .await?;
    sqlx::query("DELETE FROM token_revocations")
        .execute(&state.pool)
        .await?;
    sqlx::query("DELETE FROM users")
        .execute(&state.pool)
        .await?;

    Ok((
        StatusCode::OK,
        Json(json!({"message": "Database reset successful"})),
    ))
}

#[derive(Deserialize)]
pub struct PromoteAdminRequest {
    pub username: String,
}

/// Sets the role of the given user to "ADMIN".
/// Returns 404 if the username does not exist.
pub async fn promote_admin(
    State(state): State<Arc<AppState>>,
    Json(body): Json<PromoteAdminRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = sqlx::query("SELECT id FROM users WHERE username = $1")
        .bind(&body.username)
        .fetch_optional(&state.pool)
        .await?;

    let _row = row.ok_or_else(|| AppError::NotFound {
        entity: "user".to_string(),
    })?;

    sqlx::query("UPDATE users SET role = 'ADMIN' WHERE username = $1")
        .bind(&body.username)
        .execute(&state.pool)
        .await?;

    Ok((
        StatusCode::OK,
        Json(json!({"message": format!("User {} promoted to ADMIN", body.username)})),
    ))
}
