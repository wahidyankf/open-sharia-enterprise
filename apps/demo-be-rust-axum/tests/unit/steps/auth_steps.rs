use cucumber::{given, when};

use crate::world::AppWorld;

#[when(
    regex = r#"the client sends POST /api/v1/auth/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "Str0ng#Pass1" \}"#
)]
async fn register_alice_strong(world: &mut AppWorld) {
    world
        .svc_register("alice", "alice@example.com", "Str0ng#Pass1")
        .await;
    if world.last_status == 201 {
        world.alice_id = world
            .last_body
            .get("id")
            .and_then(|v| v.as_str())
            .and_then(|s| uuid::Uuid::parse_str(s).ok());
    }
}

#[when(
    regex = r#"the client sends POST /api/v1/auth/register with body \{ "username": "alice", "email": "new@example\.com", "password": "Str0ng#Pass1" \}"#
)]
async fn register_alice_new_email(world: &mut AppWorld) {
    world
        .svc_register("alice", "new@example.com", "Str0ng#Pass1")
        .await;
}

#[when(
    regex = r#"the client sends POST /api/v1/auth/register with body \{ "username": "alice", "email": "not-an-email", "password": "Str0ng#Pass1" \}"#
)]
async fn register_alice_bad_email(world: &mut AppWorld) {
    world
        .svc_register("alice", "not-an-email", "Str0ng#Pass1")
        .await;
}

#[when(
    regex = r#"the client sends POST /api/v1/auth/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "" \}"#
)]
async fn register_alice_empty_password(world: &mut AppWorld) {
    world.svc_register("alice", "alice@example.com", "").await;
}

#[when(
    regex = r#"the client sends POST /api/v1/auth/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "str0ng#pass1" \}"#
)]
async fn register_alice_no_uppercase(world: &mut AppWorld) {
    world
        .svc_register("alice", "alice@example.com", "str0ng#pass1")
        .await;
}

#[given(expr = "a user {string} is registered and deactivated")]
async fn register_and_deactivate(world: &mut AppWorld, username: String) {
    let email = format!("{username}@example.com");
    world.svc_register(&username, &email, "Str0ng#Pass1").await;

    world.svc_login(&username, "Str0ng#Pass1").await;
    let token = world
        .last_body
        .get("accessToken")
        .and_then(|v| v.as_str())
        .map(String::from)
        .unwrap_or_default();

    if !token.is_empty() {
        world.svc_deactivate(&format!("Bearer {token}")).await;
    }
}

#[given(regex = r#"a user "([^"]+)" is registered with email "([^"]+)" and password "([^"]+)""#)]
async fn register_with_email_password(
    world: &mut AppWorld,
    username: String,
    email: String,
    password: String,
) {
    world.svc_register(&username, &email, &password).await;
    if world.last_status == 201 && username == "alice" {
        world.alice_id = world
            .last_body
            .get("id")
            .and_then(|v| v.as_str())
            .and_then(|s| uuid::Uuid::parse_str(s).ok());
    }
}

// Login steps
#[when(
    regex = r#"the client sends POST /api/v1/auth/login with body \{ "username": "alice", "password": "Str0ng#Pass1" \}"#
)]
async fn login_alice_correct(world: &mut AppWorld) {
    world.svc_login("alice", "Str0ng#Pass1").await;
    if world.last_status == 200 {
        world.auth_token = world
            .last_body
            .get("accessToken")
            .and_then(|v| v.as_str())
            .map(String::from);
        world.refresh_token = world
            .last_body
            .get("refreshToken")
            .and_then(|v| v.as_str())
            .map(String::from);
    }
}

#[when(
    regex = r#"the client sends POST /api/v1/auth/login with body \{ "username": "alice", "password": "Wr0ngPass!" \}"#
)]
async fn login_alice_wrong_password(world: &mut AppWorld) {
    world.svc_login("alice", "Wr0ngPass!").await;
}

#[when(
    regex = r#"the client sends POST /api/v1/auth/login with body \{ "username": "ghost", "password": "Str0ng#Pass1" \}"#
)]
async fn login_ghost(world: &mut AppWorld) {
    world.svc_login("ghost", "Str0ng#Pass1").await;
}
