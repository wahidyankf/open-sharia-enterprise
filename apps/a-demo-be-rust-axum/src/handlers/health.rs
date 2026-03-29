use axum::Json;
use a_demo_contracts::models::HealthResponse;

pub async fn get_health() -> Json<HealthResponse> {
    Json(HealthResponse {
        status: "UP".to_string(),
    })
}
