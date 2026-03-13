use cucumber::{given, when};

use crate::world::AppWorld;

#[when(
    regex = r#"the client sends POST /api/v1/auth/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "Short1!Ab" \}"#
)]
async fn register_short_password(world: &mut AppWorld) {
    world
        .svc_register("alice", "alice@example.com", "Short1!Ab")
        .await;
}

#[when(
    regex = r#"the client sends POST /api/v1/auth/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "AllUpperCase1234" \}"#
)]
async fn register_no_special_char(world: &mut AppWorld) {
    world
        .svc_register("alice", "alice@example.com", "AllUpperCase1234")
        .await;
}

#[given(expr = "{string} has had the maximum number of failed login attempts")]
async fn max_failed_attempts(world: &mut AppWorld, username: String) {
    for _ in 0..5 {
        world.svc_login(&username, "WrongPass!123").await;
    }
}

#[given(expr = "a user {string} is registered and locked after too many failed logins")]
async fn register_and_lock(world: &mut AppWorld, username: String) {
    let email = format!("{username}@example.com");
    world.svc_register(&username, &email, "Str0ng#Pass1").await;
    if world.last_status == 201 && username == "alice" {
        world.alice_id = world
            .last_body
            .get("id")
            .and_then(|v| v.as_str())
            .and_then(|s| uuid::Uuid::parse_str(s).ok());
    }

    for _ in 0..5 {
        world.svc_login(&username, "WrongPass!123").await;
    }
}

#[given(expr = "an admin user {string} is registered and logged in")]
async fn register_admin_and_login(world: &mut AppWorld, username: String) {
    let email = format!("{username}@example.com");
    world.svc_register(&username, &email, "Str0ng#Pass1").await;

    if world.last_status == 201 {
        let admin_id = world
            .last_body
            .get("id")
            .and_then(|v| v.as_str())
            .and_then(|s| uuid::Uuid::parse_str(s).ok());

        if let Some(admin_uuid) = admin_id {
            world.promote_to_admin(admin_uuid).await.unwrap();
        }

        world.svc_login(&username, "Str0ng#Pass1").await;
        if world.last_status == 200 {
            world.admin_token = world
                .last_body
                .get("access_token")
                .and_then(|v| v.as_str())
                .map(String::from);
        }
    }
}

#[given("an admin has unlocked alice's account")]
async fn admin_unlocked_alice(world: &mut AppWorld) {
    // Register a sysadmin if we don't have an admin token yet
    if world.admin_token.is_none() {
        world
            .svc_register("sysadmin", "sysadmin@example.com", "Str0ng#Pass1")
            .await;
        if world.last_status == 201 {
            let admin_id = world
                .last_body
                .get("id")
                .and_then(|v| v.as_str())
                .and_then(|s| uuid::Uuid::parse_str(s).ok());
            if let Some(admin_uuid) = admin_id {
                world.promote_to_admin(admin_uuid).await.unwrap();
            }
            world.svc_login("sysadmin", "Str0ng#Pass1").await;
            if world.last_status == 200 {
                world.admin_token = world
                    .last_body
                    .get("access_token")
                    .and_then(|v| v.as_str())
                    .map(String::from);
            }
        }
    }

    let admin_bearer = world.admin_bearer();
    let alice_id = match world.alice_id {
        Some(id) => id,
        None => return,
    };
    if !admin_bearer.is_empty() {
        world.svc_admin_unlock_user(&admin_bearer, alice_id).await;
    }
}

#[when("the admin sends POST /api/v1/admin/users/{alice_id}/unlock")]
async fn admin_unlock_alice(world: &mut AppWorld) {
    let admin_bearer = world.admin_bearer();
    let alice_id = match world.alice_id {
        Some(id) => id,
        None => return,
    };
    world.svc_admin_unlock_user(&admin_bearer, alice_id).await;
}
