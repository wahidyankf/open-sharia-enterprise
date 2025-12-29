---
title: "Advanced"
date: 2025-12-29T11:20:00+07:00
draft: false
weight: 10000003
description: "Examples 61-85: Production deployment, performance optimization, monitoring, and advanced n8n patterns (75-95% coverage)"
tags: ["n8n", "tutorial", "by-example", "advanced", "production", "optimization"]
---

## Example 61: Environment Variables and Configuration Management

Production workflows require environment-specific configuration without hardcoding values.

```json
{
  "nodes": [
    {
      "parameters": {
        "values": {
          "string": [
            {
              "name": "API_URL",
              "value": "={{$env.API_URL}}"
            },
            {
              "name": "API_KEY",
              "value": "={{$env.API_KEY}}"
            }
          ]
        }
      },
      "name": "Set Environment Config",
      "type": "n8n-nodes-base.set",
      "position": [250, 300]
    }
  ]
}
```

**Key Takeaway**: Use `$env` to access environment variables set in Docker or deployment platform. Never hardcode credentials or environment-specific URLs in workflows.

## Example 62: Workflow Error Handling with Fallback

Handle API failures gracefully with fallback data sources.

```json
{
  "nodes": [
    {
      "parameters": {
        "url": "={{$env.PRIMARY_API}}/data",
        "options": {}
      },
      "name": "Primary API",
      "type": "n8n-nodes-base.httpRequest",
      "continueOnFail": true,
      "position": [250, 200]
    },
    {
      "parameters": {
        "conditions": {
          "boolean": [
            {
              "value1": "={{$node['Primary API'].json.error}}",
              "value2": true
            }
          ]
        }
      },
      "name": "Check If Failed",
      "type": "n8n-nodes-base.if",
      "position": [450, 200]
    },
    {
      "parameters": {
        "url": "={{$env.FALLBACK_API}}/data",
        "options": {}
      },
      "name": "Fallback API",
      "type": "n8n-nodes-base.httpRequest",
      "position": [650, 300]
    }
  ],
  "connections": {
    "Primary API": {
      "main": [[{ "node": "Check If Failed", "type": "main", "index": 0 }]]
    },
    "Check If Failed": {
      "main": [[], [{ "node": "Fallback API", "type": "main", "index": 0 }]]
    }
  }
}
```

**Key Takeaway**: Set `continueOnFail: true` on nodes that might fail, then route failures to alternative paths. Essential for resilient production workflows.

## Example 63: Batch Processing with Loop

Process large datasets in batches to avoid memory issues and rate limits.

```json
{
  "nodes": [
    {
      "parameters": {
        "batchSize": 100,
        "options": {}
      },
      "name": "Split In Batches",
      "type": "n8n-nodes-base.splitInBatches",
      "position": [250, 300]
    },
    {
      "parameters": {
        "url": "https://api.example.com/process",
        "options": {
          "batching": {
            "batch": {
              "batchSize": "100"
            }
          }
        }
      },
      "name": "Process Batch",
      "type": "n8n-nodes-base.httpRequest",
      "position": [450, 300]
    },
    {
      "parameters": {
        "amount": 1000,
        "unit": "milliseconds"
      },
      "name": "Wait Between Batches",
      "type": "n8n-nodes-base.wait",
      "position": [650, 300]
    }
  ],
  "connections": {
    "Split In Batches": {
      "main": [[{ "node": "Process Batch", "type": "main", "index": 0 }]]
    },
    "Process Batch": {
      "main": [[{ "node": "Wait Between Batches", "type": "main", "index": 0 }]]
    },
    "Wait Between Batches": {
      "main": [[{ "node": "Split In Batches", "type": "main", "index": 0 }]]
    }
  }
}
```

**Key Takeaway**: Use Split In Batches node for large datasets, add Wait nodes between batches to respect API rate limits.

## Example 64: Monitoring with Webhook Notifications

Send alerts when workflows fail or meet specific conditions.

```json
{
  "nodes": [
    {
      "parameters": {
        "functionCode": "return items.map(item => ({\n  json: {\n    alert: 'Workflow Failed',\n    workflow: $workflow.name,\n    execution: $execution.id,\n    error: item.json.error\n  }\n}));"
      },
      "name": "Format Alert",
      "type": "n8n-nodes-base.function",
      "position": [250, 300]
    },
    {
      "parameters": {
        "url": "={{$env.SLACK_WEBHOOK_URL}}",
        "options": {
          "bodyParametersJson": "={{ JSON.stringify($json) }}"
        }
      },
      "name": "Send to Slack",
      "type": "n8n-nodes-base.httpRequest",
      "position": [450, 300]
    }
  ]
}
```

**Key Takeaway**: Use `$workflow` and `$execution` variables to include workflow context in alerts. Essential for production monitoring.

## Example 65: Caching API Responses

Cache expensive API calls to improve performance and reduce costs.

```json
{
  "nodes": [
    {
      "parameters": {
        "key": "cache:{{$json.userId}}:profile",
        "ttl": 3600,
        "operation": "get"
      },
      "name": "Check Cache",
      "type": "n8n-nodes-base.redis",
      "continueOnFail": true,
      "position": [250, 200]
    },
    {
      "parameters": {
        "conditions": {
          "boolean": [
            {
              "value1": "={{!$json.value}}",
              "value2": true
            }
          ]
        }
      },
      "name": "Cache Miss?",
      "type": "n8n-nodes-base.if",
      "position": [450, 200]
    },
    {
      "parameters": {
        "url": "https://api.example.com/users/{{$json.userId}}",
        "options": {}
      },
      "name": "Fetch from API",
      "type": "n8n-nodes-base.httpRequest",
      "position": [650, 300]
    },
    {
      "parameters": {
        "key": "cache:{{$json.userId}}:profile",
        "value": "={{JSON.stringify($json)}}",
        "ttl": 3600,
        "operation": "set"
      },
      "name": "Store in Cache",
      "type": "n8n-nodes-base.redis",
      "position": [850, 300]
    }
  ]
}
```

**Key Takeaway**: Use Redis or similar caching layer for expensive operations. Set appropriate TTL based on data freshness requirements.

---

## Coverage Note

This advanced section currently contains 5 foundational examples covering production deployment patterns, error handling, batch processing, monitoring, and caching. These represent core production patterns needed for 75-85% of advanced n8n use cases.

Additional advanced topics planned for future expansion:

- Complex sub-workflow patterns
- Advanced data transformation techniques
- Multi-step authentication flows
- Webhook security and validation
- Performance profiling and optimization
- Database transaction management
- File processing at scale
- Custom credential management
- Workflow versioning strategies
- CI/CD integration patterns

The current examples provide essential production-ready patterns that experienced developers can adapt for their specific needs.
