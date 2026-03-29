use chrono::Utc;
use sqlx::any::AnyRow;
use sqlx::AnyPool;
use uuid::Uuid;

use crate::domain::errors::AppError;

pub async fn revoke_token(pool: &AnyPool, jti: &str, user_id: Uuid) -> Result<(), AppError> {
    let id_str = Uuid::new_v4().to_string();
    let user_id_str = user_id.to_string();
    let now_str = Utc::now().to_rfc3339();
    sqlx::query(
        "INSERT INTO revoked_tokens (id, jti, user_id, revoked_at) VALUES ($1, $2, $3, $4) ON CONFLICT DO NOTHING",
    )
    .bind(&id_str)
    .bind(jti)
    .bind(&user_id_str)
    .bind(&now_str)
    .execute(pool)
    .await?;
    Ok(())
}

pub async fn revoke_all_for_user(pool: &AnyPool, user_id: Uuid) -> Result<(), AppError> {
    let id_str = Uuid::new_v4().to_string();
    let user_id_str = user_id.to_string();
    let sentinel_jti = format!(
        "user-revoke-all-{}-{}",
        user_id,
        Utc::now().timestamp_nanos_opt().unwrap_or(0)
    );
    let now_str = Utc::now().to_rfc3339();
    sqlx::query(
        "INSERT INTO revoked_tokens (id, jti, user_id, revoked_at) VALUES ($1, $2, $3, $4) ON CONFLICT DO NOTHING",
    )
    .bind(&id_str)
    .bind(&sentinel_jti)
    .bind(&user_id_str)
    .bind(&now_str)
    .execute(pool)
    .await?;
    Ok(())
}

pub async fn is_revoked(pool: &AnyPool, jti: &str) -> Result<bool, AppError> {
    use sqlx::Row;
    let row: AnyRow = sqlx::query("SELECT COUNT(*) as cnt FROM revoked_tokens WHERE jti = $1")
        .bind(jti)
        .fetch_one(pool)
        .await?;
    let count: i64 = row.get("cnt");
    Ok(count > 0)
}

/// Check if there is any revoke-all entry for the user issued after the token's iat.
pub async fn is_user_all_revoked_after(
    pool: &AnyPool,
    user_id: Uuid,
    issued_at: i64,
) -> Result<bool, AppError> {
    use sqlx::Row;
    let user_id_str = user_id.to_string();
    let iat_dt = chrono::DateTime::from_timestamp(issued_at, 0)
        .unwrap_or_else(chrono::Utc::now)
        .to_rfc3339();
    let prefix = format!("user-revoke-all-{user_id}-%");
    let row: AnyRow = sqlx::query(
        r#"SELECT COUNT(*) as cnt FROM revoked_tokens
           WHERE user_id = $1 AND jti LIKE $2 AND revoked_at > $3"#,
    )
    .bind(&user_id_str)
    .bind(&prefix)
    .bind(&iat_dt)
    .fetch_one(pool)
    .await?;
    let count: i64 = row.get("cnt");
    Ok(count > 0)
}
