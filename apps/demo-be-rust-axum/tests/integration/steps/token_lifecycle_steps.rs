use cucumber::{given, then, when};

use crate::world::AppWorld;

#[when("alice sends POST /api/v1/auth/refresh with her refresh token")]
async fn refresh_with_alice_token(world: &mut AppWorld) {
    let token = world.refresh_token.clone().unwrap_or_default();
    world.svc_refresh(&token).await;
    if world.last_status == 200 {
        world.auth_token = world
            .last_body
            .get("access_token")
            .and_then(|v| v.as_str())
            .map(String::from);
        world.refresh_token = world
            .last_body
            .get("refresh_token")
            .and_then(|v| v.as_str())
            .map(String::from);
    }
}

#[given("alice's refresh token has expired")]
async fn alice_refresh_expired(world: &mut AppWorld) {
    // Use an obviously expired/invalid token
    world.refresh_token = Some(
        "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ0ZXN0IiwiZXhwIjoxfQ.invalid"
            .to_string(),
    );
}

#[given("alice has used her refresh token to get a new token pair")]
async fn alice_used_refresh_token(world: &mut AppWorld) {
    let original = world.refresh_token.clone().unwrap_or_default();
    world.original_refresh_token = Some(original.clone());

    world.svc_refresh(&original).await;
    if world.last_status == 200 {
        world.auth_token = world
            .last_body
            .get("access_token")
            .and_then(|v| v.as_str())
            .map(String::from);
        world.refresh_token = world
            .last_body
            .get("refresh_token")
            .and_then(|v| v.as_str())
            .map(String::from);
    }
}

#[when("alice sends POST /api/v1/auth/refresh with her original refresh token")]
async fn refresh_with_original_token(world: &mut AppWorld) {
    let token = world.original_refresh_token.clone().unwrap_or_default();
    world.svc_refresh(&token).await;
}

#[given(expr = "the user {string} has been deactivated")]
async fn user_deactivated_step(world: &mut AppWorld, _username: String) {
    let token = world.auth_token.clone().unwrap_or_default();
    world.svc_deactivate(&format!("Bearer {token}")).await;
}

#[when("alice sends POST /api/v1/auth/logout with her access token")]
async fn logout_alice(world: &mut AppWorld) {
    let token = world.auth_token.clone().unwrap_or_default();
    world.svc_logout(&format!("Bearer {token}")).await;
}

#[when("alice sends POST /api/v1/auth/logout-all with her access token")]
async fn logout_all_alice(world: &mut AppWorld) {
    let token = world.auth_token.clone().unwrap_or_default();
    world.svc_logout_all(&format!("Bearer {token}")).await;
}

#[then("alice's access token should be invalidated")]
async fn check_token_invalidated(world: &mut AppWorld) {
    let token = world.auth_token.clone().unwrap_or_default();
    world
        .svc_get_profile(&format!("Bearer {token}"))
        .await;
    assert_eq!(
        world.last_status, 401,
        "Expected 401 after logout, got {}",
        world.last_status
    );
}

#[given("alice has already logged out once")]
async fn alice_already_logged_out(world: &mut AppWorld) {
    let token = world.auth_token.clone().unwrap_or_default();
    world.svc_logout(&format!("Bearer {token}")).await;
}

#[then("the response body should contain an error message about token expiration")]
async fn error_token_expired(world: &mut AppWorld) {
    let msg = world
        .last_body
        .get("message")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    assert!(
        !msg.is_empty(),
        "Expected token expiration error, got: {}",
        world.last_body
    );
}

#[then("the response body should contain an error message about invalid token")]
async fn error_invalid_token(world: &mut AppWorld) {
    let msg = world
        .last_body
        .get("message")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    assert!(
        !msg.is_empty(),
        "Expected invalid token error, got: {}",
        world.last_body
    );
}
