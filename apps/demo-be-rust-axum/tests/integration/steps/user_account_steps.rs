use cucumber::{given, when};

use crate::world::AppWorld;

#[when("alice sends GET /api/v1/users/me")]
async fn get_alice_profile(world: &mut AppWorld) {
    let bearer = world.bearer();
    world.svc_get_profile(&bearer).await;
}

#[when(
    regex = r#"alice sends PATCH /api/v1/users/me with body \{ "display_name": "Alice Smith" \}"#
)]
async fn patch_display_name(world: &mut AppWorld) {
    let bearer = world.bearer();
    world.svc_update_profile(&bearer, "Alice Smith").await;
}

#[when(
    regex = r#"alice sends POST /api/v1/users/me/password with body \{ "old_password": "Str0ng#Pass1", "new_password": "NewPass#456" \}"#
)]
async fn change_password_correct(world: &mut AppWorld) {
    let bearer = world.bearer();
    world
        .svc_change_password(&bearer, "Str0ng#Pass1", "NewPass#456")
        .await;
}

#[when(
    regex = r#"alice sends POST /api/v1/users/me/password with body \{ "old_password": "Wr0ngOld!", "new_password": "NewPass#456" \}"#
)]
async fn change_password_wrong_old(world: &mut AppWorld) {
    let bearer = world.bearer();
    world
        .svc_change_password(&bearer, "Wr0ngOld!", "NewPass#456")
        .await;
}

#[when("alice sends POST /api/v1/users/me/deactivate")]
async fn deactivate_alice(world: &mut AppWorld) {
    let bearer = world.bearer();
    world.svc_deactivate(&bearer).await;
}

#[given("alice has deactivated her own account via POST /api/v1/users/me/deactivate")]
async fn alice_deactivated_herself(world: &mut AppWorld) {
    let bearer = world.bearer();
    world.svc_deactivate(&bearer).await;
}

#[when("the client sends GET /api/v1/users/me with alice's access token")]
async fn get_me_with_alice_token(world: &mut AppWorld) {
    let token = world.auth_token.clone().unwrap_or_default();
    world
        .svc_get_profile(&format!("Bearer {token}"))
        .await;
}
