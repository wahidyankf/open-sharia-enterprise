---
title: "Intermediate"
date: 2025-12-29T11:09:00+07:00
draft: false
weight: 10000002
description: "Examples 31-60: Production patterns including advanced transformations, loops, database operations, and error workflows (40-75% coverage)"
tags: ["n8n", "tutorial", "by-example", "intermediate", "production", "database", "loops"]
---

## Example 31: Loop Over Items

Loop Over Items node processes each item sequentially through a sub-workflow. This enables complex multi-step processing per item.

```mermaid
%% Loop processes items one by one
graph TD
    A[Input: 3 items] --> B[Loop Over Items]
    B --> C[Process Item 1]
    C --> D[Process Item 2]
    D --> E[Process Item 3]
    E --> F[Output: All results]

    style A fill:#0173B2,color:#fff
    style B fill:#CC78BC,color:#000
    style C fill:#DE8F05,color:#000
    style D fill:#DE8F05,color:#000
    style E fill:#DE8F05,color:#000
    style F fill:#029E73,color:#fff
```

```json
{
  "name": "Loop Processing",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "functionCode": "return [\n  { json: { id: 1, value: 10 } },\n  { json: { id: 2, value: 20 } },\n  { json: { id: 3, value: 30 } }\n];"
      },
      "name": "Create Items",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {},
      "name": "Loop Over Items",
      "type": "n8n-nodes-base.splitInBatches", // => Loop Over Items node
      "typeVersion": 3,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "values": {
          "number": [
            {
              "name": "doubled",
              "value": "={{ $json.value * 2 }}" // => Process current item
            }
          ]
        }
      },
      "name": "Process",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [850, 300],
      "id": "4"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Create Items", "type": "main", "index": 0 }]]
    },
    "Create Items": {
      "main": [[{ "node": "Loop Over Items", "type": "main", "index": 0 }]]
    },
    "Loop Over Items": {
      "main": [[{ "node": "Process", "type": "main", "index": 0 }]]
    },
    "Process": {
      "main": [[{ "node": "Loop Over Items", "type": "main", "index": 0 }]] // => Return to loop
    }
  }
}
// => Create Items: 3 items (id: 1,2,3, value: 10,20,30)
// => Loop iteration 1: Process item 1 (value: 10 → doubled: 20)
// => Loop iteration 2: Process item 2 (value: 20 → doubled: 40)
// => Loop iteration 3: Process item 3 (value: 30 → doubled: 60)
// => Final output: All processed items
// =>   [ { id: 1, value: 10, doubled: 20 }, { id: 2, value: 20, doubled: 40 }, { id: 3, value: 30, doubled: 60 } ]
```

**Key Takeaway**: Use Loop Over Items (Split In Batches with batch size 1) when each item requires complex sequential processing that can't be done in parallel.

---

## Example 32: Split In Batches for Pagination

Split In Batches processes items in groups. Essential for paginating API requests and avoiding rate limits.

```json
{
  "name": "Batch Processing",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "functionCode": "// => Generate 10 items to process in batches\nconst items = [];\nfor (let i = 1; i <= 10; i++) {\n  items.push({ json: { id: i, name: `Item ${i}` } });\n}\nreturn items;"
      },
      "name": "Generate Items",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "batchSize": 3, // => Process 3 items per batch
        "options": {}
      },
      "name": "Split In Batches",
      "type": "n8n-nodes-base.splitInBatches",
      "typeVersion": 3,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "batchInfo",
              "value": "={{ \"Processing batch with \" + $input.all().length + \" items\" }}"
              // => $input.all() returns all items in current batch
            }
          ]
        }
      },
      "name": "Process Batch",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [850, 300],
      "id": "4"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Generate Items", "type": "main", "index": 0 }]]
    },
    "Generate Items": {
      "main": [[{ "node": "Split In Batches", "type": "main", "index": 0 }]]
    },
    "Split In Batches": {
      "main": [[{ "node": "Process Batch", "type": "main", "index": 0 }]]
    },
    "Process Batch": {
      "main": [[{ "node": "Split In Batches", "type": "main", "index": 0 }]]
    }
  }
}
// => Generate Items: 10 items total
// => Batch 1: Items 1-3 (3 items)
// =>   Process Batch: "Processing batch with 3 items"
// => Batch 2: Items 4-6 (3 items)
// =>   Process Batch: "Processing batch with 3 items"
// => Batch 3: Items 7-9 (3 items)
// => Batch 4: Item 10 (1 item, final batch)
// =>   Process Batch: "Processing batch with 1 items"
// => Total: 4 batch iterations
```

**Key Takeaway**: Use Split In Batches with batch size > 1 to paginate large datasets or respect API rate limits (e.g., 100 items per request).

---

## Example 33: Advanced Expressions - $now and $today

n8n provides utility variables for common operations. This example demonstrates time-related helpers.

```json
{
  "name": "Time Utilities",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "nowISO",
              "value": "={{ $now.toISO() }}" // => Current timestamp in ISO format
            },
            {
              "name": "todayISO",
              "value": "={{ $today.toISO() }}" // => Today at midnight
            },
            {
              "name": "formattedNow",
              "value": "={{ $now.toFormat('yyyy-MM-dd HH:mm:ss') }}" // => Custom format
            },
            {
              "name": "weekStart",
              "value": "={{ $now.startOf('week').toISO() }}" // => Start of current week
            },
            {
              "name": "monthEnd",
              "value": "={{ $now.endOf('month').toISO() }}" // => End of current month
            },
            {
              "name": "daysUntilEndOfMonth",
              "value": "={{ Math.ceil($now.endOf('month').diff($now, 'days').days) }}"
              // => Days remaining in month
            }
          ]
        }
      },
      "name": "Time Expressions",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Time Expressions", "type": "main", "index": 0 }]]
    }
  }
}
// => $now: Luxon DateTime object for current time
// => $today: Luxon DateTime object for today at 00:00:00
// => Output (example for 2025-12-29 11:09:00):
// =>   nowISO: "2025-12-29T11:09:00.000+07:00"
// =>   todayISO: "2025-12-29T00:00:00.000+07:00"
// =>   formattedNow: "2025-12-29 11:09:00"
// =>   weekStart: "2025-12-28T00:00:00.000+07:00" (Sunday)
// =>   monthEnd: "2025-12-31T23:59:59.999+07:00"
// =>   daysUntilEndOfMonth: 3
```

**Key Takeaway**: Use `$now` and `$today` Luxon DateTime objects for time operations. Methods include toISO(), toFormat(), startOf(), endOf(), diff(), and arithmetic.

---

## Example 34: Complex Conditionals with Code Node

When IF node conditions become complex, use Code nodes for JavaScript-based logic.

```mermaid
%% Complex conditional logic
graph TD
    A[Input Data] --> B[Code: Complex Logic]
    B --> C{Valid?}
    C -->|true| D[Process]
    C -->|false| E[Reject]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#CC78BC,color:#000
    style D fill:#029E73,color:#fff
    style E fill:#CA9161,color:#000
```

```json
{
  "name": "Complex Conditionals",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "number": [
            { "name": "age", "value": 25 },
            { "name": "score", "value": 85 }
          ],
          "string": [{ "name": "country", "value": "US" }],
          "boolean": [{ "name": "verified", "value": true }]
        }
      },
      "name": "User Data",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "jsCode": "// => Access input data\nconst { age, score, country, verified } = $input.item.json;\n\n// => Complex validation logic\nconst isEligible = \n  age >= 18 && age <= 65 &&          // => Age range check\n  score >= 80 &&                     // => Minimum score\n  ['US', 'CA', 'UK'].includes(country) &&  // => Allowed countries\n  verified === true;                 // => Must be verified\n\n// => Add validation result\nreturn {\n  ...($input.item.json),\n  eligible: isEligible,\n  reason: isEligible \n    ? 'All criteria met' \n    : 'Failed one or more criteria'\n};"
      },
      "name": "Validate Eligibility",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "conditions": {
          "boolean": [
            {
              "value1": "={{ $json.eligible }}",
              "value2": true
            }
          ]
        }
      },
      "name": "Check Result",
      "type": "n8n-nodes-base.if",
      "typeVersion": 1,
      "position": [850, 300],
      "id": "4"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "User Data", "type": "main", "index": 0 }]]
    },
    "User Data": {
      "main": [[{ "node": "Validate Eligibility", "type": "main", "index": 0 }]]
    },
    "Validate Eligibility": {
      "main": [[{ "node": "Check Result", "type": "main", "index": 0 }]]
    }
  }
}
// => User Data: age=25, score=85, country="US", verified=true
// => Validate Eligibility:
// =>   age check: 25 >= 18 && 25 <= 65 → true
// =>   score check: 85 >= 80 → true
// =>   country check: "US" in ["US","CA","UK"] → true
// =>   verified check: true === true → true
// =>   All conditions met → eligible: true
// => Output: { age: 25, score: 85, country: "US", verified: true, eligible: true, reason: "All criteria met" }
```

**Key Takeaway**: Use Code nodes for complex conditional logic that would require multiple IF nodes. Write clear validation logic with descriptive variable names.

---

## Example 35: HTTP Request with Basic Auth

Basic authentication sends credentials in the Authorization header. This example shows both manual and credential-based approaches.

```json
{
  "name": "Basic Auth",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "url": "https://httpbin.org/basic-auth/user/pass",
        "authentication": "genericCredentialType", // => Use generic credential
        "genericAuthType": "httpBasicAuth", // => Basic Auth type
        "options": {}
      },
      "credentials": {
        "httpBasicAuth": {
          "id": "1",
          "name": "Basic Auth Credential"
        }
      },
      "name": "Authenticated Request",
      "type": "n8n-nodes-base.httpRequest",
      "typeVersion": 3,
      "position": [450, 300],
      "id": "2"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Authenticated Request", "type": "main", "index": 0 }]]
    }
  }
}
// => Credential configuration (in n8n UI):
// =>   Type: Basic Auth
// =>   User: user
// =>   Password: pass
// => Request header: Authorization: Basic dXNlcjpwYXNz (base64 encoded "user:pass")
// => Response: { "authenticated": true, "user": "user" }
```

**Key Takeaway**: Store Basic Auth credentials in n8n credentials manager. The platform handles base64 encoding and header construction automatically.

---

## Example 36: Webhook Authentication

Webhooks should verify requests are from authorized sources. This example validates webhook signatures.

```json
{
  "name": "Secure Webhook",
  "nodes": [
    {
      "parameters": {
        "httpMethod": "POST",
        "path": "secure-webhook",
        "responseMode": "responseNode",
        "options": {}
      },
      "name": "Webhook",
      "type": "n8n-nodes-base.webhook",
      "typeVersion": 1,
      "position": [250, 300],
      "webhookId": "secure123",
      "id": "1"
    },
    {
      "parameters": {
        "jsCode": "// => Verify webhook signature\nconst crypto = require('crypto');\n\n// => Get signature from headers\nconst receivedSignature = $input.item.json.headers['x-signature'];\n\n// => Get webhook body\nconst body = JSON.stringify($input.item.json.body);\n\n// => Calculate expected signature\nconst secret = process.env.WEBHOOK_SECRET || 'default-secret';\nconst expectedSignature = crypto\n  .createHmac('sha256', secret)\n  .update(body)\n  .digest('hex');\n\n// => Compare signatures\nconst isValid = receivedSignature === expectedSignature;\n\nreturn {\n  ...($input.item.json),\n  signatureValid: isValid,\n  message: isValid ? 'Authenticated' : 'Invalid signature'\n};"
      },
      "name": "Verify Signature",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "conditions": {
          "boolean": [
            {
              "value1": "={{ $json.signatureValid }}",
              "value2": true
            }
          ]
        }
      },
      "name": "Check Auth",
      "type": "n8n-nodes-base.if",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "respondWith": "json",
        "responseBody": "={{ { \"status\": \"success\", \"data\": $json.body } }}"
      },
      "name": "Success Response",
      "type": "n8n-nodes-base.respondToWebhook",
      "typeVersion": 1,
      "position": [850, 250],
      "id": "4"
    },
    {
      "parameters": {
        "respondWith": "json",
        "responseCode": 401, // => Unauthorized status code
        "responseBody": "={{ { \"error\": \"Invalid signature\" } }}"
      },
      "name": "Error Response",
      "type": "n8n-nodes-base.respondToWebhook",
      "typeVersion": 1,
      "position": [850, 350],
      "id": "5"
    }
  ],
  "connections": {
    "Webhook": {
      "main": [[{ "node": "Verify Signature", "type": "main", "index": 0 }]]
    },
    "Verify Signature": {
      "main": [[{ "node": "Check Auth", "type": "main", "index": 0 }]]
    },
    "Check Auth": {
      "main": [
        [{ "node": "Success Response", "type": "main", "index": 0 }],
        [{ "node": "Error Response", "type": "main", "index": 0 }]
      ]
    }
  }
}
// => Request with signature:
// =>   POST /webhook/secure-webhook
// =>   Header: X-Signature: abc123...
// =>   Body: { "data": "test" }
// => Verify Signature: Calculate HMAC-SHA256
// =>   Expected: HMAC-SHA256(body, secret)
// =>   Received: "abc123..."
// =>   Valid if match
// => Success response: 200 with data
// => Error response: 401 Unauthorized
```

**Key Takeaway**: Always validate webhook authenticity using signatures. Store webhook secrets in environment variables, never hardcode.

---

## Example 37: Error Workflow Example

Error workflows handle failures from other workflows. This example creates a reusable error handler.

```mermaid
%% Error workflow triggered on failure
graph LR
    A[Main Workflow Error] --> B[Error Workflow Trigger]
    B --> C[Log Error]
    C --> D[Send Notification]

    style A fill:#CA9161,color:#000
    style B fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style D fill:#029E73,color:#fff
```

```json
{
  "name": "Error Handler Workflow",
  "nodes": [
    {
      "parameters": {},
      "name": "Error Trigger",
      "type": "n8n-nodes-base.errorTrigger", // => Special trigger for error workflows
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "errorMessage",
              "value": "={{ $json.error.message }}" // => Error message from failed workflow
            },
            {
              "name": "workflowName",
              "value": "={{ $json.workflow.name }}" // => Name of workflow that failed
            },
            {
              "name": "executionId",
              "value": "={{ $json.execution.id }}" // => Failed execution ID
            },
            {
              "name": "nodeName",
              "value": "={{ $json.node.name }}" // => Node where error occurred
            },
            {
              "name": "timestamp",
              "value": "={{ $now.toISO() }}"
            }
          ]
        }
      },
      "name": "Extract Error Info",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "functionCode": "// => Log error to console (in production, use logging service)\nconsole.error('Workflow Error:', {\n  workflow: $input.item.json.workflowName,\n  execution: $input.item.json.executionId,\n  node: $input.item.json.nodeName,\n  error: $input.item.json.errorMessage,\n  timestamp: $input.item.json.timestamp\n});\n\nreturn $input.item.json;"
      },
      "name": "Log Error",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Error Trigger": {
      "main": [[{ "node": "Extract Error Info", "type": "main", "index": 0 }]]
    },
    "Extract Error Info": {
      "main": [[{ "node": "Log Error", "type": "main", "index": 0 }]]
    }
  },
  "settings": {}
}
// => Error Trigger receives error context from failed workflow:
// =>   $json.error: { message: "...", stack: "..." }
// =>   $json.workflow: { id: "1", name: "Main Workflow" }
// =>   $json.execution: { id: "12345", mode: "trigger" }
// =>   $json.node: { name: "Failing Node", type: "n8n-nodes-base.httpRequest" }
// => Use this workflow ID in main workflow settings.errorWorkflow
```

**Key Takeaway**: Create a global error workflow to handle failures across all workflows. Access error details via `$json.error`, workflow info via `$json.workflow`.

---

## Example 38: Retry Logic with Continue On Fail

Production workflows should retry transient failures. This example implements retry logic.

```json
{
  "name": "Retry Pattern",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "number": [{ "name": "attemptCount", "value": 0 }]
        }
      },
      "name": "Initialize",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "url": "https://api.unreliable-service.com/data",
        "options": {
          "timeout": 5000 // => 5 second timeout
        }
      },
      "continueOnFail": true, // => Don't stop workflow on error
      "retryOnFail": true, // => Enable automatic retries
      "maxTries": 3, // => Maximum 3 attempts
      "waitBetweenTries": 5000, // => Wait 5 seconds between retries
      "name": "Unreliable API",
      "type": "n8n-nodes-base.httpRequest",
      "typeVersion": 3,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "conditions": {
          "boolean": [
            {
              "value1": "={{ $json.error !== undefined }}",
              "value2": true
            }
          ]
        }
      },
      "name": "Check Success",
      "type": "n8n-nodes-base.if",
      "typeVersion": 1,
      "position": [850, 300],
      "id": "4"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "status",
              "value": "Failed after retries"
            },
            {
              "name": "error",
              "value": "={{ $json.error.message }}"
            }
          ]
        }
      },
      "name": "Handle Failure",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [1050, 250],
      "id": "5"
    },
    {
      "parameters": {
        "values": {
          "string": [{ "name": "status", "value": "Success" }]
        }
      },
      "name": "Handle Success",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [1050, 350],
      "id": "6"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Initialize", "type": "main", "index": 0 }]]
    },
    "Initialize": {
      "main": [[{ "node": "Unreliable API", "type": "main", "index": 0 }]]
    },
    "Unreliable API": {
      "main": [[{ "node": "Check Success", "type": "main", "index": 0 }]]
    },
    "Check Success": {
      "main": [
        [{ "node": "Handle Failure", "type": "main", "index": 0 }],
        [{ "node": "Handle Success", "type": "main", "index": 0 }]
      ]
    }
  }
}
// => Unreliable API configuration:
// =>   continueOnFail: true (continue if fails)
// =>   retryOnFail: true (auto retry)
// =>   maxTries: 3 (attempts: try 1, retry 1, retry 2)
// =>   waitBetweenTries: 5000ms (5 seconds between attempts)
// => Execution flow:
// =>   Attempt 1: Fails → wait 5s
// =>   Attempt 2: Fails → wait 5s
// =>   Attempt 3: Fails → continue with error
// => Check Success: error !== undefined → true (failure path)
```

**Key Takeaway**: Enable `retryOnFail` on nodes calling unreliable APIs. Configure `maxTries` and `waitBetweenTries` for exponential backoff patterns.

---

## Example 39: JSON Parsing and Stringifying

APIs often return nested JSON strings. This example shows parsing and stringifying operations.

```json
{
  "name": "JSON Operations",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "jsonString",
              "value": "{\"user\":{\"name\":\"Alice\",\"age\":30,\"tags\":[\"developer\",\"admin\"]}}"
            }
          ]
        }
      },
      "name": "JSON String",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "parsed",
              "value": "={{ JSON.parse($json.jsonString) }}" // => Parse JSON string to object
            },
            {
              "name": "userName",
              "value": "={{ JSON.parse($json.jsonString).user.name }}" // => Access nested property
            },
            {
              "name": "firstTag",
              "value": "={{ JSON.parse($json.jsonString).user.tags[0] }}" // => Access array element
            },
            {
              "name": "stringified",
              "value": "={{ JSON.stringify({ processed: true, timestamp: $now.toISO() }) }}"
              // => Convert object to JSON string
            },
            {
              "name": "prettyJson",
              "value": "={{ JSON.stringify(JSON.parse($json.jsonString), null, 2) }}"
              // => Pretty print with 2-space indentation
            }
          ]
        }
      },
      "name": "Parse and Stringify",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "JSON String", "type": "main", "index": 0 }]]
    },
    "JSON String": {
      "main": [[{ "node": "Parse and Stringify", "type": "main", "index": 0 }]]
    }
  }
}
// => JSON String output: String containing JSON
// => Parse and Stringify:
// =>   parsed: { user: { name: "Alice", age: 30, tags: ["developer", "admin"] } } (object)
// =>   userName: "Alice" (extracted from nested object)
// =>   firstTag: "developer" (array element)
// =>   stringified: "{\"processed\":true,\"timestamp\":\"2025-12-29T04:09:00.000Z\"}"
// =>   prettyJson: Multi-line formatted JSON with indentation
```

**Key Takeaway**: Use JSON.parse() to convert JSON strings to objects for property access. Use JSON.stringify() to convert objects to JSON strings for API requests.

---

## Example 40: File Operations - Read Binary

File processing requires handling binary data. This example reads and processes files.

```json
{
  "name": "Read File",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "filePath": "/tmp/sample.txt" // => File path to read
      },
      "name": "Read Binary File",
      "type": "n8n-nodes-base.readBinaryFile", // => Read Binary File node
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "options": {}
      },
      "name": "Move Binary Data",
      "type": "n8n-nodes-base.moveBinaryData", // => Convert binary to text
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "content",
              "value": "={{ $json.data }}" // => Extracted text content
            },
            {
              "name": "length",
              "value": "={{ $json.data.length }}" // => Content length
            }
          ]
        }
      },
      "name": "Process Content",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [850, 300],
      "id": "4"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Read Binary File", "type": "main", "index": 0 }]]
    },
    "Read Binary File": {
      "main": [[{ "node": "Move Binary Data", "type": "main", "index": 0 }]]
    },
    "Move Binary Data": {
      "main": [[{ "node": "Process Content", "type": "main", "index": 0 }]]
    }
  }
}
// => Read Binary File: Reads /tmp/sample.txt
// =>   Output: Binary data in $binary.data
// => Move Binary Data: Converts binary to text
// =>   Mode: "binaryToJson" (default)
// =>   Output: Text content in $json.data
// => Process Content: Access file content as string
// =>   content: "File contents here..."
// =>   length: 123 (characters)
```

**Key Takeaway**: Use Read Binary File to load files into workflows. Use Move Binary Data to convert between binary and JSON formats for text processing.

---

## Example 41: File Operations - Write Binary

Writing files enables exporting workflow results. This example creates and saves files.

```json
{
  "name": "Write File",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "reportContent",
              "value": "=Report Generated: {{ $now.toFormat('yyyy-MM-dd HH:mm:ss') }}\n\nTotal Items: 150\nProcessed: 145\nFailed: 5"
            }
          ]
        }
      },
      "name": "Generate Report",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "mode": "jsonToBinary", // => Convert JSON to binary
        "options": {
          "fileName": "={{ \"report_\" + $now.toFormat('yyyyMMdd_HHmmss') + \".txt\" }}",
          "mimeType": "text/plain"
        }
      },
      "name": "Convert to Binary",
      "type": "n8n-nodes-base.moveBinaryData",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "fileName": "={{ $binary.data.fileName }}", // => Use dynamic filename
        "dataPropertyName": "data" // => Binary property name
      },
      "name": "Write Binary File",
      "type": "n8n-nodes-base.writeBinaryFile", // => Write Binary File node
      "typeVersion": 1,
      "position": [850, 300],
      "id": "4"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Generate Report", "type": "main", "index": 0 }]]
    },
    "Generate Report": {
      "main": [[{ "node": "Convert to Binary", "type": "main", "index": 0 }]]
    },
    "Convert to Binary": {
      "main": [[{ "node": "Write Binary File", "type": "main", "index": 0 }]]
    }
  }
}
// => Generate Report: Create text content
// =>   reportContent: "Report Generated: 2025-12-29 11:09:00\n..."
// => Convert to Binary: JSON → binary
// =>   fileName: "report_20251229_110900.txt"
// =>   $binary.data contains file content
// => Write Binary File: Save to disk
// =>   File path: /tmp/report_20251229_110900.txt (or configured directory)
// =>   File created successfully
```

**Key Takeaway**: Use Move Binary Data (jsonToBinary mode) to convert text to binary, then Write Binary File to save. Dynamic filenames prevent overwriting.

---

## Example 42: Send Email

Email nodes send notifications and reports. This example sends formatted emails.

```json
{
  "name": "Send Email",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            { "name": "recipientEmail", "value": "user@example.com" },
            { "name": "orderID", "value": "ORD-12345" },
            { "name": "customerName", "value": "Alice Johnson" }
          ],
          "number": [{ "name": "totalAmount", "value": 149.99 }]
        }
      },
      "name": "Order Data",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "fromEmail": "noreply@example.com", // => Sender email
        "toEmail": "={{ $json.recipientEmail }}", // => Recipient from data
        "subject": "={{ \"Order Confirmation - \" + $json.orderID }}", // => Dynamic subject
        "text": "={{ \"Dear \" + $json.customerName + \",\\n\\nYour order \" + $json.orderID + \" has been confirmed.\\n\\nTotal: $\" + $json.totalAmount + \"\\n\\nThank you!\" }}",
        "options": {
          "allowUnauthorizedCerts": false
        }
      },
      "credentials": {
        "smtp": {
          "id": "1",
          "name": "SMTP Account"
        }
      },
      "name": "Send Email",
      "type": "n8n-nodes-base.emailSend", // => Email Send node
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Order Data", "type": "main", "index": 0 }]]
    },
    "Order Data": {
      "main": [[{ "node": "Send Email", "type": "main", "index": 0 }]]
    }
  }
}
// => SMTP credential configuration (in n8n UI):
// =>   Host: smtp.gmail.com
// =>   Port: 587
// =>   User: your-email@gmail.com
// =>   Password: app-specific-password
// => Email sent:
// =>   From: noreply@example.com
// =>   To: user@example.com
// =>   Subject: "Order Confirmation - ORD-12345"
// =>   Body: "Dear Alice Johnson,\n\nYour order ORD-12345 has been confirmed..."
```

**Key Takeaway**: Configure SMTP credentials once, reuse across workflows. Use expressions for dynamic email content (subject, body, recipients).

---

## Example 43: PostgreSQL Database Query

Database integration enables persistent data storage. This example queries PostgreSQL.

```json
{
  "name": "PostgreSQL Query",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "operation": "executeQuery", // => Execute SQL query
        "query": "SELECT id, name, email, created_at FROM users WHERE active = true ORDER BY created_at DESC LIMIT 10",
        "options": {}
      },
      "credentials": {
        "postgres": {
          "id": "1",
          "name": "PostgreSQL Database"
        }
      },
      "name": "Get Active Users",
      "type": "n8n-nodes-base.postgres", // => PostgreSQL node
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "userInfo",
              "value": "={{ $json.name + \" (\" + $json.email + \")\" }}"
            }
          ]
        }
      },
      "name": "Format Results",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Get Active Users", "type": "main", "index": 0 }]]
    },
    "Get Active Users": {
      "main": [[{ "node": "Format Results", "type": "main", "index": 0 }]]
    }
  }
}
// => PostgreSQL credential (in n8n UI):
// =>   Host: localhost
// =>   Database: myapp
// =>   User: n8n_user
// =>   Password: secure_password
// =>   Port: 5432
// => Query execution: SELECT id, name, email, created_at FROM users...
// => Output: Multiple items (one per row)
// =>   Item 0: { id: 105, name: "Alice", email: "alice@example.com", created_at: "2025-12-29T04:00:00Z" }
// =>   Item 1: { id: 104, name: "Bob", email: "bob@example.com", created_at: "2025-12-28T15:30:00Z" }
// =>   ... (up to 10 items)
```

**Key Takeaway**: PostgreSQL node returns query results as multiple items (one per row). Use executeQuery operation for SELECT, INSERT, UPDATE, DELETE statements.

---

## Example 44: PostgreSQL Insert with Parameters

Parameterized queries prevent SQL injection. This example safely inserts data.

```mermaid
%% Database insert flow
graph LR
    A[Input Data] --> B[Prepare Query]
    B --> C[PostgreSQL Insert]
    C --> D[Return ID]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#CC78BC,color:#000
    style D fill:#029E73,color:#fff
```

```json
{
  "name": "PostgreSQL Insert",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            { "name": "name", "value": "Charlie Brown" },
            { "name": "email", "value": "charlie@example.com" }
          ],
          "number": [{ "name": "age", "value": 28 }]
        }
      },
      "name": "User Data",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "operation": "executeQuery",
        "query": "=INSERT INTO users (name, email, age, created_at)\nVALUES (\n  '{{ $json.name }}',\n  '{{ $json.email }}',\n  {{ $json.age }},\n  NOW()\n)\nRETURNING id, name, created_at", // => Parameterized INSERT with RETURNING
        "options": {}
      },
      "credentials": {
        "postgres": {
          "id": "1",
          "name": "PostgreSQL Database"
        }
      },
      "name": "Insert User",
      "type": "n8n-nodes-base.postgres",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "User Data", "type": "main", "index": 0 }]]
    },
    "User Data": {
      "main": [[{ "node": "Insert User", "type": "main", "index": 0 }]]
    }
  }
}
// => User Data: { name: "Charlie Brown", email: "charlie@example.com", age: 28 }
// => Insert User: Execute parameterized INSERT
// =>   SQL: INSERT INTO users (name, email, age, created_at) VALUES ('Charlie Brown', 'charlie@example.com', 28, NOW()) RETURNING id, name, created_at
// =>   String values: Single-quoted ('Charlie Brown')
// =>   Numbers: Unquoted (28)
// =>   RETURNING clause: Get inserted row data
// => Output: { id: 106, name: "Charlie Brown", created_at: "2025-12-29T04:09:00Z" }
```

**Key Takeaway**: Use expressions to safely interpolate values into SQL queries. Always quote strings, leave numbers unquoted. Use RETURNING to get inserted data.

---

## Example 45: MongoDB Operations

MongoDB provides NoSQL document storage. This example inserts and queries documents.

```json
{
  "name": "MongoDB Operations",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "operation": "insert", // => Insert operation
        "collection": "orders", // => Collection name
        "fields": "={{ JSON.stringify({\n  orderId: \"ORD-\" + Date.now(),\n  customer: \"Alice\",\n  items: [\n    { product: \"Laptop\", price: 999 },\n    { product: \"Mouse\", price: 29 }\n  ],\n  total: 1028,\n  createdAt: new Date()\n}) }}",
        "options": {}
      },
      "credentials": {
        "mongoDb": {
          "id": "1",
          "name": "MongoDB Connection"
        }
      },
      "name": "Insert Order",
      "type": "n8n-nodes-base.mongoDb", // => MongoDB node
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "operation": "find", // => Query operation
        "collection": "orders",
        "query": "={{ JSON.stringify({ customer: \"Alice\" }) }}", // => Query filter
        "options": {
          "sort": "={{ JSON.stringify({ createdAt: -1 }) }}", // => Sort descending
          "limit": 5
        }
      },
      "credentials": {
        "mongoDb": {
          "id": "1",
          "name": "MongoDB Connection"
        }
      },
      "name": "Find Orders",
      "type": "n8n-nodes-base.mongoDb",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Insert Order", "type": "main", "index": 0 }]]
    },
    "Insert Order": {
      "main": [[{ "node": "Find Orders", "type": "main", "index": 0 }]]
    }
  }
}
// => MongoDB credential (in n8n UI):
// =>   Connection String: mongodb://localhost:27017/myapp
// => Insert Order:
// =>   Document: { orderId: "ORD-1735449540000", customer: "Alice", items: [...], total: 1028, createdAt: ISODate(...) }
// =>   Output: { _id: "...", orderId: "ORD-1735449540000", ... }
// => Find Orders:
// =>   Query: { customer: "Alice" }
// =>   Sort: { createdAt: -1 } (newest first)
// =>   Output: Up to 5 matching documents as items
```

**Key Takeaway**: MongoDB node accepts JSON strings for queries and documents. Use JSON.stringify() to convert objects to required format.

---

## Example 46: API Pagination

APIs limit results per request. This example implements pagination to fetch all data.

```json
{
  "name": "API Pagination",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "number": [
            { "name": "page", "value": 1 }, // => Start at page 1
            { "name": "perPage", "value": 50 }
          ]
        }
      },
      "name": "Initialize",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "batchSize": 1, // => Process one page at a time
        "options": {}
      },
      "name": "Loop Pages",
      "type": "n8n-nodes-base.splitInBatches",
      "typeVersion": 3,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "url": "=https://api.github.com/repositories?since={{ ($json.page - 1) * $json.perPage }}",
        "options": {
          "queryParameters": {
            "parameters": [
              {
                "name": "per_page",
                "value": "={{ $json.perPage }}"
              }
            ]
          }
        }
      },
      "name": "Fetch Page",
      "type": "n8n-nodes-base.httpRequest",
      "typeVersion": 3,
      "position": [850, 300],
      "id": "4"
    },
    {
      "parameters": {
        "conditions": {
          "number": [
            {
              "value1": "={{ $input.all().length }}", // => Items received
              "operation": "smaller",
              "value2": "={{ $node[\"Initialize\"].json.perPage }}" // => Less than per_page means last page
            }
          ]
        }
      },
      "name": "Check Has More",
      "type": "n8n-nodes-base.if",
      "typeVersion": 1,
      "position": [1050, 300],
      "id": "5"
    },
    {
      "parameters": {
        "values": {
          "number": [
            {
              "name": "page",
              "value": "={{ $node[\"Initialize\"].json.page + 1 }}" // => Increment page
            }
          ]
        }
      },
      "name": "Next Page",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [1250, 350],
      "id": "6"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Initialize", "type": "main", "index": 0 }]]
    },
    "Initialize": {
      "main": [[{ "node": "Loop Pages", "type": "main", "index": 0 }]]
    },
    "Loop Pages": {
      "main": [[{ "node": "Fetch Page", "type": "main", "index": 0 }]]
    },
    "Fetch Page": {
      "main": [[{ "node": "Check Has More", "type": "main", "index": 0 }]]
    },
    "Check Has More": {
      "main": [
        [], // => No more pages: exit loop
        [{ "node": "Next Page", "type": "main", "index": 0 }] // => Has more: continue
      ]
    },
    "Next Page": {
      "main": [[{ "node": "Loop Pages", "type": "main", "index": 0 }]] // => Return to loop
    }
  }
}
// => Pagination flow:
// =>   Page 1: Fetch 50 items → has more (50 items returned)
// =>   Page 2: Fetch 50 items → has more
// =>   Page 3: Fetch 30 items → no more (< 50 items, last page)
// => Final output: All items from all pages combined
```

**Key Takeaway**: Use Loop Over Items with page counter to implement pagination. Check response size to detect last page (fewer items than per_page).

---

## Example 47: Rate Limiting with Wait

APIs enforce rate limits. This example adds delays between requests to stay within limits.

```json
{
  "name": "Rate Limited Requests",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "functionCode": "// => Create multiple API endpoints to call\nconst endpoints = [\n  'https://api.github.com/users/n8n-io',\n  'https://api.github.com/users/nodejs',\n  'https://api.github.com/users/microsoft'\n];\n\nreturn endpoints.map(url => ({ json: { url } }));"
      },
      "name": "Endpoints",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "batchSize": 1 // => Process one at a time
      },
      "name": "Loop Endpoints",
      "type": "n8n-nodes-base.splitInBatches",
      "typeVersion": 3,
      "position": [650, 300],
      "id": "3"
    },
    {
      "parameters": {
        "url": "={{ $json.url }}",
        "options": {}
      },
      "name": "API Request",
      "type": "n8n-nodes-base.httpRequest",
      "typeVersion": 3,
      "position": [850, 300],
      "id": "4"
    },
    {
      "parameters": {
        "amount": 2, // => Wait 2 seconds
        "unit": "seconds"
      },
      "name": "Rate Limit Delay",
      "type": "n8n-nodes-base.wait",
      "typeVersion": 1,
      "position": [1050, 300],
      "webhookId": "ratelimit123",
      "id": "5"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Endpoints", "type": "main", "index": 0 }]]
    },
    "Endpoints": {
      "main": [[{ "node": "Loop Endpoints", "type": "main", "index": 0 }]]
    },
    "Loop Endpoints": {
      "main": [[{ "node": "API Request", "type": "main", "index": 0 }]]
    },
    "API Request": {
      "main": [[{ "node": "Rate Limit Delay", "type": "main", "index": 0 }]]
    },
    "Rate Limit Delay": {
      "main": [[{ "node": "Loop Endpoints", "type": "main", "index": 0 }]]
    }
  }
}
// => Execution timeline:
// =>   T+0s: Request 1 (https://api.github.com/users/n8n-io)
// =>   T+2s: Request 2 (https://api.github.com/users/nodejs)
// =>   T+4s: Request 3 (https://api.github.com/users/microsoft)
// => Rate: 1 request per 2 seconds = 30 requests/minute
// => API limit: 60 requests/minute → safe margin
```

**Key Takeaway**: Add Wait nodes between API requests to respect rate limits. Calculate delay: (60 / requests_per_minute) seconds to stay within limits.

---

## Example 48: Data Transformation with $items

The `$items()` function accesses all items from a node. This enables aggregation and cross-item operations.

```json
{
  "name": "Cross-Item Operations",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "functionCode": "return [\n  { json: { product: 'A', revenue: 1000 } },\n  { json: { product: 'B', revenue: 1500 } },\n  { json: { product: 'C', revenue: 2000 } }\n];"
      },
      "name": "Sales Data",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "jsCode": "// => Access all items from previous node\nconst salesData = $input.all();\n\n// => Calculate total revenue\nconst totalRevenue = salesData.reduce((sum, item) => \n  sum + item.json.revenue, 0\n);  // => Sum: 1000 + 1500 + 2000 = 4500\n\n// => Calculate average\nconst averageRevenue = totalRevenue / salesData.length;\n  // => Average: 4500 / 3 = 1500\n\n// => Find max revenue\nconst maxRevenue = Math.max(...salesData.map(item => item.json.revenue));\n  // => Max: 2000\n\n// => Return single summary item\nreturn {\n  totalRevenue,\n  averageRevenue,\n  maxRevenue,\n  productCount: salesData.length\n};"
      },
      "name": "Calculate Metrics",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Sales Data", "type": "main", "index": 0 }]]
    },
    "Sales Data": {
      "main": [[{ "node": "Calculate Metrics", "type": "main", "index": 0 }]]
    }
  }
}
// => Sales Data: 3 items (revenue: 1000, 1500, 2000)
// => Calculate Metrics: Access all items via $input.all()
// =>   totalRevenue: 1000 + 1500 + 2000 = 4500
// =>   averageRevenue: 4500 / 3 = 1500
// =>   maxRevenue: max(1000, 1500, 2000) = 2000
// =>   productCount: 3
// => Output: Single item with aggregated metrics
```

**Key Takeaway**: Use `$input.all()` in Code nodes to access all items for aggregation. Common operations: sum, average, min, max, count.

---

## Example 49: Dynamic Webhook Paths

Webhook paths can be dynamic based on data. This example creates multi-tenant webhook endpoints.

```json
{
  "name": "Dynamic Webhook",
  "nodes": [
    {
      "parameters": {
        "httpMethod": "POST",
        "path": "={{ $parameter.tenantId }}/orders", // => Dynamic path from parameter
        "responseMode": "responseNode",
        "options": {}
      },
      "name": "Tenant Webhook",
      "type": "n8n-nodes-base.webhook",
      "typeVersion": 1,
      "position": [250, 300],
      "webhookId": "tenant123",
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "tenantId",
              "value": "={{ $parameter.tenantId }}" // => Extract tenant from path
            },
            {
              "name": "orderData",
              "value": "={{ JSON.stringify($json.body) }}"
            }
          ]
        }
      },
      "name": "Process Order",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "respondWith": "json",
        "responseBody": "={{ { \"status\": \"received\", \"tenant\": $parameter.tenantId } }}"
      },
      "name": "Respond",
      "type": "n8n-nodes-base.respondToWebhook",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Tenant Webhook": {
      "main": [[{ "node": "Process Order", "type": "main", "index": 0 }]]
    },
    "Process Order": {
      "main": [[{ "node": "Respond", "type": "main", "index": 0 }]]
    }
  }
}
// => Webhook configuration:
// =>   Path parameter: tenantId
// =>   Path template: "={{ $parameter.tenantId }}/orders"
// => Example URLs:
// =>   POST /webhook/tenant-a/orders → tenantId = "tenant-a"
// =>   POST /webhook/tenant-b/orders → tenantId = "tenant-b"
// => $parameter.tenantId extracts value from URL path
// => Enables single workflow for multiple tenants
```

**Key Takeaway**: Use `$parameter` to access URL path parameters in webhook triggers. Enables dynamic, multi-tenant webhook endpoints.

---

## Example 50: Merge Node - Append

Merge node "Append" mode combines all items from inputs. This example merges data from multiple sources.

```mermaid
%% Merge appends items from all inputs
graph TD
    A[Manual Trigger] --> B[Source 1: 2 items]
    A --> C[Source 2: 3 items]
    B --> D[Merge: Append Mode]
    C --> D
    D --> E[Output: 5 items total]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#DE8F05,color:#000
    style D fill:#CC78BC,color:#000
    style E fill:#029E73,color:#fff
```

```json
{
  "name": "Merge Append",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "functionCode": "return [\n  { json: { source: 'A', id: 1 } },\n  { json: { source: 'A', id: 2 } }\n];"
      },
      "name": "Source A",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 250],
      "id": "2"
    },
    {
      "parameters": {
        "functionCode": "return [\n  { json: { source: 'B', id: 3 } },\n  { json: { source: 'B', id: 4 } },\n  { json: { source: 'B', id: 5 } }\n];"
      },
      "name": "Source B",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 350],
      "id": "3"
    },
    {
      "parameters": {
        "mode": "append", // => Append mode: combine all items
        "options": {}
      },
      "name": "Merge",
      "type": "n8n-nodes-base.merge",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "4"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [
        [{ "node": "Source A", "type": "main", "index": 0 }],
        [{ "node": "Source B", "type": "main", "index": 0 }]
      ]
    },
    "Source A": {
      "main": [[{ "node": "Merge", "type": "main", "index": 0 }]]
    },
    "Source B": {
      "main": [[{ "node": "Merge", "type": "main", "index": 1 }]]
    }
  }
}
// => Source A: 2 items (id: 1, 2)
// => Source B: 3 items (id: 3, 4, 5)
// => Merge (append mode): Concatenate all items
// => Output: 5 items total
// =>   Item 0: { source: 'A', id: 1 }
// =>   Item 1: { source: 'A', id: 2 }
// =>   Item 2: { source: 'B', id: 3 }
// =>   Item 3: { source: 'B', id: 4 }
// =>   Item 4: { source: 'B', id: 5 }
```

**Key Takeaway**: Use Merge node "Append" mode to combine items from multiple branches into a single list. Order: input 1 items, then input 2 items.

---

## Example 51: Webhook with Query Parameters

Webhooks can accept query parameters for filtering or configuration. This example processes query params.

```json
{
  "name": "Webhook Query Params",
  "nodes": [
    {
      "parameters": {
        "httpMethod": "GET", // => GET method for query parameters
        "path": "search",
        "responseMode": "responseNode",
        "options": {}
      },
      "name": "Webhook",
      "type": "n8n-nodes-base.webhook",
      "typeVersion": 1,
      "position": [250, 300],
      "webhookId": "search123",
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "query",
              "value": "={{ $json.query.q || '' }}" // => Access query parameter 'q'
            },
            {
              "name": "category",
              "value": "={{ $json.query.category || 'all' }}" // => With default
            },
            {
              "name": "page",
              "value": "={{ $json.query.page || '1' }}"
            }
          ]
        }
      },
      "name": "Extract Params",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "respondWith": "json",
        "responseBody": "={{ { \"search\": $json.query, \"category\": $json.category, \"page\": $json.page } }}"
      },
      "name": "Respond",
      "type": "n8n-nodes-base.respondToWebhook",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Webhook": {
      "main": [[{ "node": "Extract Params", "type": "main", "index": 0 }]]
    },
    "Extract Params": {
      "main": [[{ "node": "Respond", "type": "main", "index": 0 }]]
    }
  }
}
// => Request: GET /webhook/search?q=n8n&category=tools&page=2
// => $json.query contains:
// =>   { q: "n8n", category: "tools", page: "2" }
// => Extract Params:
// =>   query: "n8n" (from $json.query.q)
// =>   category: "tools" (from $json.query.category)
// =>   page: "2" (from $json.query.page)
// => Response: { "search": "n8n", "category": "tools", "page": "2" }
```

**Key Takeaway**: Access webhook query parameters via `$json.query.paramName`. Use `|| 'default'` for optional parameters with defaults.

---

## Example 52: HTTP Request - Handle Redirects

HTTP redirects (3xx status codes) need special handling. This example follows redirects.

```json
{
  "name": "Follow Redirects",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "url": "https://httpbin.org/redirect/3", // => Endpoint that redirects 3 times
        "options": {
          "redirect": {
            "redirect": {
              "followRedirects": true, // => Follow redirects automatically
              "maxRedirects": 5 // => Maximum redirect hops
            }
          },
          "response": {
            "response": {
              "fullResponse": true // => Include headers and status
            }
          }
        }
      },
      "name": "Redirecting Request",
      "type": "n8n-nodes-base.httpRequest",
      "typeVersion": 3,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "finalUrl",
              "value": "={{ $json.headers.request.url }}" // => Final URL after redirects
            },
            {
              "name": "statusCode",
              "value": "={{ $json.statusCode }}" // => Final status code
            }
          ]
        }
      },
      "name": "Extract Info",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Redirecting Request", "type": "main", "index": 0 }]]
    },
    "Redirecting Request": {
      "main": [[{ "node": "Extract Info", "type": "main", "index": 0 }]]
    }
  }
}
// => Request flow:
// =>   1. GET https://httpbin.org/redirect/3 → 302 redirect
// =>   2. GET https://httpbin.org/redirect/2 → 302 redirect
// =>   3. GET https://httpbin.org/redirect/1 → 302 redirect
// =>   4. GET https://httpbin.org/get → 200 OK (final)
// => followRedirects: true → automatically follows all redirects
// => maxRedirects: 5 → stops after 5 hops (prevents infinite loops)
// => Final response: 200 OK from https://httpbin.org/get
```

**Key Takeaway**: Enable `followRedirects` option for HTTP Request nodes handling APIs with redirects. Set `maxRedirects` to prevent infinite redirect loops.

---

## Example 53: Custom Headers from Variables

Headers often need dynamic values from previous nodes or environment. This example builds dynamic headers.

```json
{
  "name": "Dynamic Headers",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            { "name": "apiKey", "value": "sk_live_abc123xyz789" },
            { "name": "sessionId", "value": "sess_def456uvw012" },
            { "name": "userAgent", "value": "MyApp/1.0" }
          ]
        }
      },
      "name": "Auth Data",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "url": "https://api.example.com/data",
        "options": {
          "headers": {
            "parameters": [
              {
                "name": "Authorization",
                "value": "={{ \"Bearer \" + $json.apiKey }}" // => Build auth header
              },
              {
                "name": "X-Session-ID",
                "value": "={{ $json.sessionId }}" // => Custom session header
              },
              {
                "name": "User-Agent",
                "value": "={{ $json.userAgent }}"
              },
              {
                "name": "X-Request-ID",
                "value": "={{ $execution.id }}" // => Execution ID for tracing
              }
            ]
          }
        }
      },
      "name": "API Request",
      "type": "n8n-nodes-base.httpRequest",
      "typeVersion": 3,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Auth Data", "type": "main", "index": 0 }]]
    },
    "Auth Data": {
      "main": [[{ "node": "API Request", "type": "main", "index": 0 }]]
    }
  }
}
// => Auth Data: { apiKey: "sk_live_abc123xyz789", sessionId: "sess_def456uvw012", userAgent: "MyApp/1.0" }
// => API Request headers:
// =>   Authorization: Bearer sk_live_abc123xyz789
// =>   X-Session-ID: sess_def456uvw012
// =>   User-Agent: MyApp/1.0
// =>   X-Request-ID: 12345 (execution ID)
// => Use expressions to build headers from data and context
```

**Key Takeaway**: Build dynamic headers using expressions. Combine data from previous nodes (`$json`), environment (`$env`), and execution context (`$execution`).

---

## Example 54: HTML Extraction with Cheerio

Web scraping requires parsing HTML. This example uses Code node with Cheerio library.

```json
{
  "name": "HTML Parsing",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "url": "https://example.com",
        "options": {}
      },
      "name": "Fetch HTML",
      "type": "n8n-nodes-base.httpRequest",
      "typeVersion": 3,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "jsCode": "// => n8n includes Cheerio library for HTML parsing\nconst cheerio = require('cheerio');\n\n// => Load HTML\nconst html = $input.item.json;\nconst $ = cheerio.load(html);\n\n// => Extract data using CSS selectors\nconst title = $('title').text();  // => Page title\nconst headings = [];\n$('h1, h2').each((i, elem) => {\n  headings.push($(elem).text());\n});  // => All h1 and h2 headings\n\nconst links = [];\n$('a').each((i, elem) => {\n  links.push({\n    text: $(elem).text(),\n    href: $(elem).attr('href')\n  });\n});  // => All links with text and href\n\nreturn {\n  title,\n  headingCount: headings.length,\n  headings,\n  linkCount: links.length,\n  links: links.slice(0, 5)  // => First 5 links only\n};"
      },
      "name": "Parse HTML",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Fetch HTML", "type": "main", "index": 0 }]]
    },
    "Fetch HTML": {
      "main": [[{ "node": "Parse HTML", "type": "main", "index": 0 }]]
    }
  }
}
// => Fetch HTML: Returns HTML string
// => Parse HTML: Load with Cheerio
// =>   $('title').text() → "Example Domain"
// =>   $('h1, h2').each() → Extract all headings
// =>   $('a').each() → Extract all links
// => Output: { title: "Example Domain", headingCount: 1, headings: ["Example Domain"], linkCount: 1, links: [...] }
```

**Key Takeaway**: Use Cheerio in Code nodes for HTML parsing. CSS selectors extract elements, `.text()` gets content, `.attr()` gets attributes.

---

## Example 55: XML Parsing

XML data requires parsing to JSON. This example converts XML responses.

```json
{
  "name": "XML to JSON",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "xmlData",
              "value": "<?xml version=\"1.0\"?>\n<users>\n  <user id=\"1\">\n    <name>Alice</name>\n    <email>alice@example.com</email>\n  </user>\n  <user id=\"2\">\n    <name>Bob</name>\n    <email>bob@example.com</email>\n  </user>\n</users>"
            }
          ]
        }
      },
      "name": "XML Data",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "jsCode": "// => n8n includes xml2js library for XML parsing\nconst { parseStringPromise } = require('xml2js');\n\n// => Parse XML to JSON\nconst xmlString = $input.item.json.xmlData;\nconst result = await parseStringPromise(xmlString, {\n  explicitArray: false  // => Don't wrap single elements in arrays\n});\n\n// => Extract users\nconst users = Array.isArray(result.users.user) \n  ? result.users.user \n  : [result.users.user];  // => Normalize to array\n\nreturn users.map(user => ({\n  id: user.$.id,  // => Attributes in $ property\n  name: user.name,\n  email: user.email\n}));"
      },
      "name": "Parse XML",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "XML Data", "type": "main", "index": 0 }]]
    },
    "XML Data": {
      "main": [[{ "node": "Parse XML", "type": "main", "index": 0 }]]
    }
  }
}
// => XML Data: XML string
// => Parse XML: Convert to JSON using xml2js
// =>   parseStringPromise() returns Promise
// =>   explicitArray: false → simpler structure
// =>   Attributes in user.$ (user.$.id)
// =>   Element content in user.name, user.email
// => Output: 2 items
// =>   Item 0: { id: "1", name: "Alice", email: "alice@example.com" }
// =>   Item 1: { id: "2", name: "Bob", email: "bob@example.com" }
```

**Key Takeaway**: Use xml2js library in Code nodes for XML parsing. Set `explicitArray: false` for simpler JSON structure. Attributes are in `$` property.

---

## Example 56: CSV Parsing

CSV data is common in file imports and exports. This example parses CSV strings.

```json
{
  "name": "CSV Parsing",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "csvData",
              "value": "id,name,email,age\n1,Alice,alice@example.com,30\n2,Bob,bob@example.com,25\n3,Charlie,charlie@example.com,35"
            }
          ]
        }
      },
      "name": "CSV Data",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "jsCode": "// => Parse CSV manually (simple approach)\nconst csvString = $input.item.json.csvData;\nconst lines = csvString.split('\\n');\n\n// => Extract headers\nconst headers = lines[0].split(',');  // => ['id', 'name', 'email', 'age']\n\n// => Parse rows\nconst rows = lines.slice(1).map(line => {\n  const values = line.split(',');\n  const row = {};\n  headers.forEach((header, index) => {\n    row[header] = values[index];\n  });\n  return { json: row };\n});\n\nreturn rows;"
      },
      "name": "Parse CSV",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "CSV Data", "type": "main", "index": 0 }]]
    },
    "CSV Data": {
      "main": [[{ "node": "Parse CSV", "type": "main", "index": 0 }]]
    }
  }
}
// => CSV Data: CSV string with headers
// => Parse CSV:
// =>   Split by newline: 4 lines (1 header + 3 data rows)
// =>   Extract headers: ['id', 'name', 'email', 'age']
// =>   Parse each row: split by comma, map to object
// => Output: 3 items
// =>   Item 0: { id: "1", name: "Alice", email: "alice@example.com", age: "30" }
// =>   Item 1: { id: "2", name: "Bob", email: "bob@example.com", age: "25" }
// =>   Item 2: { id: "3", name: "Charlie", email: "charlie@example.com", age: "35" }
```

**Key Takeaway**: Parse CSV in Code nodes by splitting on newlines and commas. For complex CSV (quoted fields, escaped commas), use csv-parser library.

---

## Example 57: Remove Duplicates

Data often contains duplicates. This example removes duplicate items based on a field.

```json
{
  "name": "Remove Duplicates",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "functionCode": "return [\n  { json: { email: 'alice@example.com', name: 'Alice' } },\n  { json: { email: 'bob@example.com', name: 'Bob' } },\n  { json: { email: 'alice@example.com', name: 'Alice Duplicate' } },\n  { json: { email: 'charlie@example.com', name: 'Charlie' } },\n  { json: { email: 'bob@example.com', name: 'Bob Duplicate' } }\n];"
      },
      "name": "Data with Duplicates",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "jsCode": "// => Remove duplicates based on email field\nconst items = $input.all();\nconst seen = new Set();\nconst unique = [];\n\nfor (const item of items) {\n  const email = item.json.email;\n  if (!seen.has(email)) {\n    seen.add(email);  // => Mark email as seen\n    unique.push(item);  // => Keep first occurrence\n  }\n  // => Skip subsequent duplicates\n}\n\nreturn unique;"
      },
      "name": "Deduplicate",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Data with Duplicates", "type": "main", "index": 0 }]]
    },
    "Data with Duplicates": {
      "main": [[{ "node": "Deduplicate", "type": "main", "index": 0 }]]
    }
  }
}
// => Data with Duplicates: 5 items (2 duplicate emails)
// =>   alice@example.com: 2 occurrences
// =>   bob@example.com: 2 occurrences
// => Deduplicate:
// =>   Track seen emails in Set
// =>   Keep first occurrence, skip duplicates
// => Output: 3 unique items
// =>   Item 0: { email: 'alice@example.com', name: 'Alice' }
// =>   Item 1: { email: 'bob@example.com', name: 'Bob' }
// =>   Item 2: { email: 'charlie@example.com', name: 'Charlie' }
```

**Key Takeaway**: Use Set to track seen values and filter duplicates. Keeps first occurrence by default. Modify logic to keep last or best match.

---

## Example 58: Sort Items

Sorting items enables ordered processing. This example sorts by multiple fields.

```json
{
  "name": "Sort Items",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "functionCode": "return [\n  { json: { name: 'Alice', department: 'Sales', salary: 75000 } },\n  { json: { name: 'Bob', department: 'Engineering', salary: 95000 } },\n  { json: { name: 'Charlie', department: 'Sales', salary: 85000 } },\n  { json: { name: 'Diana', department: 'Engineering', salary: 90000 } }\n];"
      },
      "name": "Employee Data",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "jsCode": "// => Sort by department (ascending), then salary (descending)\nconst items = $input.all();\n\nitems.sort((a, b) => {\n  // => Primary sort: department (alphabetical)\n  if (a.json.department < b.json.department) return -1;\n  if (a.json.department > b.json.department) return 1;\n  \n  // => Secondary sort: salary (highest first)\n  return b.json.salary - a.json.salary;\n});\n\nreturn items;"
      },
      "name": "Sort",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Employee Data", "type": "main", "index": 0 }]]
    },
    "Employee Data": {
      "main": [[{ "node": "Sort", "type": "main", "index": 0 }]]
    }
  }
}
// => Employee Data: 4 items (unsorted)
// => Sort: Multi-field sorting
// =>   Primary: department (alphabetical)
// =>   Secondary: salary (descending)
// => Output: 4 items (sorted)
// =>   Item 0: { name: 'Bob', department: 'Engineering', salary: 95000 }
// =>   Item 1: { name: 'Diana', department: 'Engineering', salary: 90000 }
// =>   Item 2: { name: 'Charlie', department: 'Sales', salary: 85000 }
// =>   Item 3: { name: 'Alice', department: 'Sales', salary: 75000 }
```

**Key Takeaway**: Sort items in Code nodes using array `.sort()` method. For multi-field sorting, check primary field first, then secondary.

---

## Example 59: Limit and Offset (Pagination)

Process subsets of items using limit and offset patterns. This example implements pagination logic.

```json
{
  "name": "Limit and Offset",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "functionCode": "// => Generate 20 sample items\nconst items = [];\nfor (let i = 1; i <= 20; i++) {\n  items.push({ json: { id: i, name: `Item ${i}` } });\n}\nreturn items;"
      },
      "name": "Generate Items",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "values": {
          "number": [
            { "name": "page", "value": 2 }, // => Page number (1-indexed)
            { "name": "pageSize", "value": 5 } // => Items per page
          ]
        }
      },
      "name": "Pagination Config",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [650, 250],
      "id": "3"
    },
    {
      "parameters": {
        "jsCode": "// => Calculate offset and limit\nconst page = $node[\"Pagination Config\"].json.page;\nconst pageSize = $node[\"Pagination Config\"].json.pageSize;\n\nconst offset = (page - 1) * pageSize;  // => Page 2, size 5 → offset 5\nconst limit = pageSize;  // => 5 items\n\n// => Get all items and slice\nconst allItems = $input.all();\nconst pageItems = allItems.slice(offset, offset + limit);\n  // => slice(5, 10) → items 6-10 (0-indexed)\n\nreturn pageItems;"
      },
      "name": "Apply Pagination",
      "type": "n8n-nodes-base.code",
      "typeVersion": 2,
      "position": [850, 300],
      "id": "4"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Generate Items", "type": "main", "index": 0 }]]
    },
    "Generate Items": {
      "main": [
        [
          { "node": "Pagination Config", "type": "main", "index": 0 },
          { "node": "Apply Pagination", "type": "main", "index": 0 }
        ]
      ]
    },
    "Pagination Config": {
      "main": [[{ "node": "Apply Pagination", "type": "main", "index": 0 }]]
    }
  }
}
// => Generate Items: 20 items (id: 1-20)
// => Pagination Config: page=2, pageSize=5
// => Apply Pagination:
// =>   offset = (2 - 1) * 5 = 5
// =>   limit = 5
// =>   slice(5, 10) → items at indices 5-9
// => Output: 5 items (id: 6-10)
// =>   Item 0: { id: 6, name: "Item 6" }
// =>   Item 1: { id: 7, name: "Item 7" }
// =>   ...
// =>   Item 4: { id: 10, name: "Item 10" }
```

**Key Takeaway**: Implement pagination using `offset = (page - 1) * pageSize` and `array.slice(offset, offset + limit)`. Useful for batch processing.

---

## Example 60: Workflow Variables with Set Node

Set nodes can store workflow-level variables for reuse. This example demonstrates variable patterns.

```json
{
  "name": "Workflow Variables",
  "nodes": [
    {
      "parameters": {},
      "name": "Manual Trigger",
      "type": "n8n-nodes-base.manualTrigger",
      "typeVersion": 1,
      "position": [250, 300],
      "id": "1"
    },
    {
      "parameters": {
        "values": {
          "string": [
            { "name": "apiBaseUrl", "value": "https://api.example.com/v1" },
            { "name": "apiKey", "value": "sk_live_abc123" }
          ],
          "number": [
            { "name": "maxRetries", "value": 3 },
            { "name": "timeout", "value": 30000 }
          ],
          "boolean": [{ "name": "enableLogging", "value": true }]
        }
      },
      "name": "Config Variables",
      "type": "n8n-nodes-base.set",
      "typeVersion": 1,
      "position": [450, 300],
      "id": "2"
    },
    {
      "parameters": {
        "url": "={{ $node[\"Config Variables\"].json.apiBaseUrl }}/users", // => Reference variable
        "options": {
          "headers": {
            "parameters": [
              {
                "name": "Authorization",
                "value": "={{ \"Bearer \" + $node[\"Config Variables\"].json.apiKey }}"
              }
            ]
          },
          "timeout": "={{ $node[\"Config Variables\"].json.timeout }}" // => Timeout from variable
        }
      },
      "name": "API Call",
      "type": "n8n-nodes-base.httpRequest",
      "typeVersion": 3,
      "position": [650, 300],
      "id": "3"
    }
  ],
  "connections": {
    "Manual Trigger": {
      "main": [[{ "node": "Config Variables", "type": "main", "index": 0 }]]
    },
    "Config Variables": {
      "main": [[{ "node": "API Call", "type": "main", "index": 0 }]]
    }
  }
}
// => Config Variables: Store all configuration
// =>   apiBaseUrl: "https://api.example.com/v1"
// =>   apiKey: "sk_live_abc123"
// =>   maxRetries: 3
// =>   timeout: 30000
// => API Call: Reference variables via $node["Config Variables"].json.fieldName
// =>   URL: https://api.example.com/v1/users
// =>   Authorization: Bearer sk_live_abc123
// =>   Timeout: 30000ms
// => Centralize configuration for easy updates
```

**Key Takeaway**: Use Set nodes to store workflow variables at the start. Reference via `$node["NodeName"].json.fieldName` throughout workflow for centralized configuration.

---

**End of Intermediate Examples (31-60)**

These 30 intermediate examples cover production patterns: loops, batching, pagination, database operations, file processing, error handling, API integration, and data transformation. You now understand how to build robust, production-ready workflows and are prepared for advanced optimization techniques.
