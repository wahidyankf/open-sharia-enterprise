use cucumber::{given, then, when};

use crate::world::AppWorld;

#[when("alice decodes her access token payload")]
async fn decode_alice_token(world: &mut AppWorld) {
    let bearer = world.bearer();
    world.svc_get_claims(&bearer).await;
}

#[then(expr = "the token should contain a non-null {string} claim")]
async fn token_claim_non_null(world: &mut AppWorld, claim: String) {
    let val = world.last_body.get(&claim);
    assert!(
        val.is_some() && !val.unwrap().is_null(),
        "Claim '{claim}' should be non-null in body: {}",
        world.last_body
    );
}

#[when("the client sends GET /.well-known/jwks.json")]
async fn get_jwks(world: &mut AppWorld) {
    world.svc_jwks().await;
}

#[then(expr = "the response body should contain at least one key in the {string} array")]
async fn jwks_has_keys(world: &mut AppWorld, field: String) {
    let keys = world
        .last_body
        .get(&field)
        .and_then(|v| v.as_array())
        .map(|a| a.len())
        .unwrap_or(0);
    assert!(
        keys >= 1,
        "Expected at least one key in '{field}', got {keys}"
    );
}

#[then("alice's access token should be recorded as revoked")]
async fn token_is_revoked(world: &mut AppWorld) {
    let token = world.auth_token.clone().unwrap_or_default();
    world.svc_get_profile(&format!("Bearer {token}")).await;
    assert_eq!(
        world.last_status, 401,
        "Expected 401 for revoked token, got {}",
        world.last_status
    );
}

#[given("alice has logged out and her access token is blacklisted")]
async fn alice_logged_out_blacklisted(world: &mut AppWorld) {
    let token = world.auth_token.clone().unwrap_or_default();
    world.svc_logout(&format!("Bearer {token}")).await;
}

#[given(
    regex = r#"the admin has disabled alice's account via POST /api/v1/admin/users/\{alice_id\}/disable"#
)]
async fn admin_disabled_alice(world: &mut AppWorld) {
    let admin_bearer = world.admin_bearer();
    let alice_id = match world.alice_id {
        Some(id) => id,
        None => return,
    };
    if !admin_bearer.is_empty() {
        world.svc_admin_disable_user(&admin_bearer, alice_id).await;
    }
}
