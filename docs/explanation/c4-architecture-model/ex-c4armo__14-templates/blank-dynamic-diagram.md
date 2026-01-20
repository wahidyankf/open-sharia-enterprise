---
title: "[Workflow Name] - Dynamic Diagram"
description: Shows the runtime interaction sequence for [Workflow Name]
category: explanation
subcategory: architecture
tags:
  - c4-model
  - dynamic-diagram
  - sequence-diagram
  - your-system-name
created: YYYY-MM-DD
updated: YYYY-MM-DD
---

# [Workflow Name] - Dynamic Diagram

## Purpose

This diagram shows the runtime collaboration between containers or components for **[Workflow Name]**. It captures:

- The sequence of interactions over time
- The order in which things happen
- What data is passed between elements
- Error handling paths (optional)

**Audience**: Developers, QA engineers, support teams troubleshooting workflows.

## Diagram

```mermaid
sequenceDiagram
    participant User as User<br/>Person
    participant WebApp as Web Application<br/>Container
    participant APIService as API Service<br/>Container
    participant Database as Database<br/>Container
    participant ExternalSystem as External System<br/>External

    %% Main workflow
    User->>+WebApp: 1. [Action]<br/>e.g. Clicks "Submit Order" button
    WebApp->>WebApp: 2. [Validation]<br/>e.g. Validates form inputs
    WebApp->>+APIService: 3. [API Call]<br/>POST /api/orders<br/>JSON: {item, quantity, price}

    APIService->>APIService: 4. [Business Logic]<br/>e.g. Calculate total, apply discounts

    APIService->>+Database: 5. [Database Write]<br/>INSERT INTO orders<br/>Values: {user_id, total, status}
    Database-->>-APIService: 6. [Response]<br/>Order ID: 12345

    APIService->>+ExternalSystem: 7. [External API Call]<br/>POST /charge<br/>JSON: {amount, card_token}
    ExternalSystem-->>-APIService: 8. [Response]<br/>Payment Success: transaction_id

    APIService->>+Database: 9. [Database Update]<br/>UPDATE orders SET status='paid'<br/>WHERE id=12345
    Database-->>-APIService: 10. [Confirmation]<br/>1 row updated

    APIService-->>-WebApp: 11. [API Response]<br/>200 OK<br/>JSON: {order_id, status, receipt_url}
    WebApp-->>-User: 12. [UI Update]<br/>Display success message and receipt

    %% Error path (optional)
    Note over User,ExternalSystem: Error Path: Payment Failure
    ExternalSystem-->>APIService: Payment Failed: insufficient_funds
    APIService->>Database: UPDATE orders SET status='failed'
    APIService-->>WebApp: 402 Payment Required<br/>JSON: {error: "Payment failed"}
    WebApp-->>User: Display error message
```

## Workflow Steps

### Happy Path (Success Scenario)

1. **User Action**: User [describes action, e.g., submits order form]
2. **Client-Side Validation**: Web app validates input (e.g., checks required fields)
3. **API Request**: Web app sends POST request to API service with order data
4. **Business Logic**: API service calculates total, applies business rules
5. **Database Write**: API service inserts order record into database
6. **Database Response**: Database returns generated order ID
7. **External API Call**: API service calls payment gateway to charge customer
8. **External Response**: Payment gateway returns success with transaction ID
9. **Database Update**: API service updates order status to "paid"
10. **Database Confirmation**: Database confirms update
11. **API Response**: API service returns success response with order details
12. **UI Update**: Web app displays success message and receipt to user

### Error Path (Failure Scenario)

**Trigger**: Payment gateway returns payment failure (e.g., insufficient funds)

1. Payment gateway returns error response to API service
2. API service updates order status to "failed" in database
3. API service returns 402 Payment Required to web app
4. Web app displays error message to user

## Alternative Flows

### [Alternative Flow 1: e.g., Cached Response]

If order data is cached:

- Step 5-6 skipped (no database read)
- API service returns cached order details immediately

### [Alternative Flow 2: e.g., Retry Logic]

If external API call fails (timeout, 500 error):

- API service retries up to 3 times with exponential backoff
- If all retries fail, order marked as "pending_manual_review"
- User notified of delay

## Data Formats

### Request: POST /api/orders

```json
{
  "item_id": "prod-123",
  "quantity": 2,
  "price": 29.99,
  "card_token": "tok_visa_1234"
}
```

### Response: 200 OK

```json
{
  "order_id": "12345",
  "status": "paid",
  "receipt_url": "https://example.com/receipts/12345",
  "transaction_id": "txn_abc123"
}
```

### Error Response: 402 Payment Required

```json
{
  "error": "Payment failed",
  "reason": "insufficient_funds",
  "order_id": "12345"
}
```

## Performance Considerations

- **Expected Latency**: [e.g., Low latency for happy path]
- **Bottlenecks**: [e.g., External payment API is slowest component]
- **Timeout Policies**: [e.g., Timeout configured for external API calls]

## Notes

- Add any important notes about edge cases, race conditions, or retry logic
- Mention if workflow involves transactions or eventual consistency

## Related Diagrams

- **[Container Diagram](./blank-container-diagram.md)**: Shows the containers involved in this workflow
- **[Component Diagram](./blank-component-diagram.md)**: Shows internal components of API Service

---

**Template Instructions**:

1. Replace `[Workflow Name]` with the actual workflow (e.g., "Place Order Workflow", "User Registration Flow")
2. Replace participants (User, WebApp, APIService, Database, ExternalSystem) with actual elements from your system
3. Update sequence steps to match your workflow (add/remove steps as needed)
4. Add detailed step descriptions in "Workflow Steps" section
5. Document error paths and alternative flows
6. Include sample request/response data formats
7. Update frontmatter (title, description, tags, created/updated dates)
8. Delete this template instructions section when done

**See**: [Supplementary Diagrams: Dynamic Diagram](../ex-c4armo__07-supplementary-diagrams.md#dynamic-diagram) for detailed guidance.
