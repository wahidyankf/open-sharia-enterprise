use cucumber::{given, then, when};

use crate::world::AppWorld;

async fn upload_file(
    world: &mut AppWorld,
    expense_id: uuid::Uuid,
    filename: &str,
    content_type: &str,
    bearer: String,
    data: Vec<u8>,
) {
    world
        .svc_upload_attachment(&bearer, expense_id, filename, content_type, data)
        .await;
}

#[when(
    regex = r#"alice uploads file "([^"]+)" with content type "([^"]+)" to POST /api/v1/expenses/\{expenseId\}/attachments"#
)]
async fn alice_uploads_file(world: &mut AppWorld, filename: String, content_type: String) {
    let bearer = world.bearer();
    let expense_id = match world.last_expense_id {
        Some(id) => id,
        None => return,
    };
    let data = b"fake file content for testing".to_vec();
    upload_file(world, expense_id, &filename, &content_type, bearer, data).await;
}

#[given(regex = r#"alice has uploaded file "([^"]+)" with content type "([^"]+)" to the entry"#)]
async fn alice_uploaded_file(world: &mut AppWorld, filename: String, content_type: String) {
    let bearer = world.bearer();
    let expense_id = match world.last_expense_id {
        Some(id) => id,
        None => return,
    };
    let data = b"fake file content".to_vec();
    upload_file(world, expense_id, &filename, &content_type, bearer, data).await;
}

#[when("alice sends GET /api/v1/expenses/{expenseId}/attachments")]
async fn alice_list_attachments(world: &mut AppWorld) {
    let bearer = world.bearer();
    let expense_id = match world.last_expense_id {
        Some(id) => id,
        None => return,
    };
    world.svc_list_attachments(&bearer, expense_id).await;
}

#[when("alice sends DELETE /api/v1/expenses/{expenseId}/attachments/{attachmentId}")]
async fn alice_delete_attachment(world: &mut AppWorld) {
    let bearer = world.bearer();
    let expense_id = match world.last_expense_id {
        Some(id) => id,
        None => return,
    };
    let att_id = match world.last_attachment_id {
        Some(id) => id,
        None => return,
    };
    world
        .svc_delete_attachment(&bearer, expense_id, att_id)
        .await;
}

#[when("alice uploads an oversized file to POST /api/v1/expenses/{expenseId}/attachments")]
async fn alice_uploads_oversized(world: &mut AppWorld) {
    let bearer = world.bearer();
    let expense_id = match world.last_expense_id {
        Some(id) => id,
        None => return,
    };
    let data = vec![0u8; 11 * 1024 * 1024];
    upload_file(world, expense_id, "big.jpg", "image/jpeg", bearer, data).await;
}

#[then(expr = "the response body should contain {int} items in the {string} array")]
async fn response_array_count(world: &mut AppWorld, count: usize, field: String) {
    let actual = world
        .last_body
        .get(&field)
        .and_then(|v| v.as_array())
        .map(|a| a.len())
        .unwrap_or(0);
    assert_eq!(
        actual, count,
        "Expected {count} items in '{field}', got {actual}, body: {}",
        world.last_body
    );
}

#[then(
    regex = r#"the response body should contain an attachment with "([^"]+)" equal to "([^"]+)""#
)]
async fn attachment_with_field(world: &mut AppWorld, field: String, value: String) {
    let attachments = world
        .last_body
        .get("attachments")
        .and_then(|v| v.as_array());
    let found = attachments
        .map(|arr| {
            arr.iter().any(|att| {
                att.get(&field)
                    .and_then(|v| v.as_str())
                    .map(|s| s == value.as_str())
                    .unwrap_or(false)
            })
        })
        .unwrap_or(false);
    assert!(
        found,
        "Expected attachment with '{field}' = '{value}' in: {}",
        world.last_body
    );
}

#[then("the response body should contain an error message about file size")]
async fn error_file_size(world: &mut AppWorld) {
    let msg = world
        .last_body
        .get("message")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    assert!(
        !msg.is_empty(),
        "Expected file size error, got: {}",
        world.last_body
    );
}

// Bob steps for cross-user ownership tests

#[given(
    regex = r#"bob has created an entry with body \{ "amount": "25\.00", "currency": "USD", "category": "transport", "description": "Taxi", "date": "2025-01-15", "type": "expense" \}"#
)]
async fn bob_created_entry(world: &mut AppWorld) {
    world.svc_login("bob", "Str0ng#Pass2").await;
    if world.last_status == 200 {
        world.bob_auth_token = world
            .last_body
            .get("access_token")
            .and_then(|v| v.as_str())
            .map(String::from);
        let bob_bearer = world.bob_bearer();
        world
            .svc_create_expense(
                &bob_bearer,
                "25.00",
                "USD",
                "transport",
                "Taxi",
                "2025-01-15",
                "expense",
                None,
                None,
            )
            .await;
        if world.last_status == 201 {
            world.bob_expense_id = world
                .last_body
                .get("id")
                .and_then(|v| v.as_str())
                .and_then(|s| uuid::Uuid::parse_str(s).ok());
        }
    }
}

#[when(
    regex = r#"alice uploads file "([^"]+)" with content type "([^"]+)" to POST /api/v1/expenses/\{bobExpenseId\}/attachments"#
)]
async fn alice_uploads_to_bob_expense(
    world: &mut AppWorld,
    filename: String,
    content_type: String,
) {
    let bearer = world.bearer();
    let expense_id = match world.bob_expense_id {
        Some(id) => id,
        None => return,
    };
    let data = b"file content".to_vec();
    upload_file(world, expense_id, &filename, &content_type, bearer, data).await;
}

#[when("alice sends GET /api/v1/expenses/{bobExpenseId}/attachments")]
async fn alice_list_bob_attachments(world: &mut AppWorld) {
    let bearer = world.bearer();
    let expense_id = match world.bob_expense_id {
        Some(id) => id,
        None => return,
    };
    world.svc_list_attachments(&bearer, expense_id).await;
}

#[when("alice sends DELETE /api/v1/expenses/{bobExpenseId}/attachments/{attachmentId}")]
async fn alice_delete_bob_attachment(world: &mut AppWorld) {
    let bearer = world.bearer();
    let expense_id = match world.bob_expense_id {
        Some(id) => id,
        None => return,
    };
    let att_id = match world.last_attachment_id {
        Some(id) => id,
        None => return,
    };
    world
        .svc_delete_attachment(&bearer, expense_id, att_id)
        .await;
}

#[when("alice sends DELETE /api/v1/expenses/{expenseId}/attachments/{randomAttachmentId}")]
async fn alice_delete_nonexistent_attachment(world: &mut AppWorld) {
    let bearer = world.bearer();
    let expense_id = match world.last_expense_id {
        Some(id) => id,
        None => return,
    };
    let random_id = uuid::Uuid::new_v4();
    world
        .svc_delete_attachment(&bearer, expense_id, random_id)
        .await;
}
