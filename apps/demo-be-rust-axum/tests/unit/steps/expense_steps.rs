use cucumber::{given, when};

use crate::world::AppWorld;

pub async fn create_expense_helper(world: &mut AppWorld, body_json: &str) {
    // Parse the JSON body to extract fields
    let val: serde_json::Value = serde_json::from_str(body_json).unwrap();
    let bearer = world.bearer();

    let amount = val["amount"].as_str().unwrap_or("");
    let currency = val["currency"].as_str().unwrap_or("");
    let category = val["category"].as_str().unwrap_or("");
    let description = val["description"].as_str().unwrap_or("");
    let date = val["date"].as_str().unwrap_or("");
    let entry_type = val["type"].as_str().unwrap_or("");
    let quantity = val["quantity"].as_f64();
    let unit = val["unit"].as_str();

    world
        .svc_create_expense(
            &bearer,
            amount,
            currency,
            category,
            description,
            date,
            entry_type,
            quantity,
            unit,
        )
        .await;
}

#[when(
    regex = r#"alice sends POST /api/v1/expenses with body \{ "amount": "10\.50", "currency": "USD", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense" \}"#
)]
async fn alice_create_expense_lunch(world: &mut AppWorld) {
    create_expense_helper(
        world,
        r#"{"amount": "10.50", "currency": "USD", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense"}"#,
    )
    .await;
}

#[when(
    regex = r#"alice sends POST /api/v1/expenses with body \{ "amount": "3000\.00", "currency": "USD", "category": "salary", "description": "Monthly salary", "date": "2025-01-31", "type": "income" \}"#
)]
async fn alice_create_income_salary(world: &mut AppWorld) {
    create_expense_helper(
        world,
        r#"{"amount": "3000.00", "currency": "USD", "category": "salary", "description": "Monthly salary", "date": "2025-01-31", "type": "income"}"#,
    )
    .await;
}

#[given(
    regex = r#"alice has created an entry with body \{ "amount": "10\.50", "currency": "USD", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense" \}"#
)]
async fn alice_created_entry_lunch(world: &mut AppWorld) {
    create_expense_helper(
        world,
        r#"{"amount": "10.50", "currency": "USD", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense"}"#,
    )
    .await;
}

#[given(regex = r#"alice has created 3 entries"#)]
async fn alice_created_3_entries(world: &mut AppWorld) {
    let entries = [
        r#"{"amount": "10.00", "currency": "USD", "category": "food", "description": "Entry 1", "date": "2025-01-01", "type": "expense"}"#,
        r#"{"amount": "20.00", "currency": "USD", "category": "food", "description": "Entry 2", "date": "2025-01-02", "type": "expense"}"#,
        r#"{"amount": "30.00", "currency": "USD", "category": "food", "description": "Entry 3", "date": "2025-01-03", "type": "expense"}"#,
    ];
    for body in entries {
        create_expense_helper(world, body).await;
    }
}

#[when("alice sends GET /api/v1/expenses")]
async fn alice_list_expenses(world: &mut AppWorld) {
    let bearer = world.bearer();
    world.svc_list_expenses(&bearer).await;
}

#[given(
    regex = r#"alice has created an entry with body \{ "amount": "10\.00", "currency": "USD", "category": "food", "description": "Breakfast", "date": "2025-01-10", "type": "expense" \}"#
)]
async fn alice_created_entry_breakfast(world: &mut AppWorld) {
    create_expense_helper(
        world,
        r#"{"amount": "10.00", "currency": "USD", "category": "food", "description": "Breakfast", "date": "2025-01-10", "type": "expense"}"#,
    )
    .await;
}

#[when(
    regex = r#"alice sends PUT /api/v1/expenses/\{expenseId\} with body \{ "amount": "12\.00", "currency": "USD", "category": "food", "description": "Updated breakfast", "date": "2025-01-10", "type": "expense" \}"#
)]
async fn alice_update_expense(world: &mut AppWorld) {
    let bearer = world.bearer();
    let expense_id = match world.last_expense_id {
        Some(id) => id,
        None => return,
    };
    world
        .svc_update_expense(
            &bearer,
            expense_id,
            "12.00",
            "USD",
            "food",
            "Updated breakfast",
            "2025-01-10",
            "expense",
            None,
            None,
        )
        .await;
}

#[given(
    regex = r#"alice has created an entry with body \{ "amount": "10\.00", "currency": "USD", "category": "food", "description": "Snack", "date": "2025-01-05", "type": "expense" \}"#
)]
async fn alice_created_entry_snack(world: &mut AppWorld) {
    create_expense_helper(
        world,
        r#"{"amount": "10.00", "currency": "USD", "category": "food", "description": "Snack", "date": "2025-01-05", "type": "expense"}"#,
    )
    .await;
}

#[when("alice sends DELETE /api/v1/expenses/{expenseId}")]
async fn alice_delete_expense(world: &mut AppWorld) {
    let bearer = world.bearer();
    let expense_id = match world.last_expense_id {
        Some(id) => id,
        None => return,
    };
    world.svc_delete_expense(&bearer, expense_id).await;
}

#[when(
    regex = r#"the client sends POST /api/v1/expenses with body \{ "amount": "10\.00", "currency": "USD", "category": "food", "description": "Coffee", "date": "2025-01-01", "type": "expense" \}"#
)]
async fn unauthenticated_create_expense(world: &mut AppWorld) {
    // No bearer token — svc_create_expense with empty bearer returns 401
    world
        .svc_create_expense(
            "",
            "10.00",
            "USD",
            "food",
            "Coffee",
            "2025-01-01",
            "expense",
            None,
            None,
        )
        .await;
}
