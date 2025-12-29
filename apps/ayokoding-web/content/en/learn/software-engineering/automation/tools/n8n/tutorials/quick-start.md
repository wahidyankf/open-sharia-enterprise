---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn n8n core concepts and build practical workflow automations"
tags:
  - n8n
  - automation
  - workflow
  - quick-start
---

Learn n8n core concepts and build practical workflow automations. This Quick Start teaches the essential patterns you need to create powerful automations.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- n8n's node-based workflow system
- Triggers, actions, and data flow
- Working with credentials and connections
- Testing and debugging workflows

## 📋 Prerequisites

- n8n installed and running (see [Initial Setup](/en/learn/software-engineering/automation/tools/n8n/tutorials/initial-setup))
- Basic understanding of APIs and webhooks helpful but not required

## 🧩 Understanding Nodes

Nodes are the building blocks of n8n workflows. There are four types:

### Trigger Nodes

Start your workflow when an event occurs:

- **Schedule Trigger**: Run on a time schedule
- **Webhook**: Receive HTTP requests
- **Email Trigger**: React to new emails
- **Manual Trigger**: Start workflows manually

### Action Nodes

Perform operations:

- **HTTP Request**: Call any API
- **Database**: Query or update databases
- **Spreadsheet**: Read/write to Google Sheets, Excel
- **Email**: Send emails via SMTP, Gmail, etc.

### Logic Nodes

Control workflow flow:

- **IF**: Conditional branching
- **Switch**: Multiple condition routing
- **Merge**: Combine data from multiple branches
- **Loop**: Iterate over data

### Data Transformation Nodes

Modify data:

- **Set**: Add or modify fields
- **Code**: Write custom JavaScript/Python
- **Function**: Transform data with expressions
- **Filter**: Remove unwanted data

## 🔄 Building Your First Real Workflow

Let's create a workflow that monitors a website and sends an email if it's down:

### Step 1: Add Schedule Trigger

1. Create new workflow
2. Add "Schedule Trigger" node
3. Set to run every 5 minutes

### Step 2: Check Website

1. Add "HTTP Request" node
2. Method: GET
3. URL: `https://example.com`
4. In "Options" tab:
   - Enable "Ignore SSL Issues" if needed
   - Set timeout to 10 seconds

### Step 3: Add Conditional Check

1. Add "IF" node
2. Condition: `{{ $json.statusCode }}` equals `200`
3. This splits the workflow into "true" and "false" paths

### Step 4: Send Alert Email (False Path)

1. Connect to the "false" output of IF node
2. Add "Send Email" node
3. Configure your SMTP credentials
4. Subject: "Website Down Alert"
5. Body: "Website is not responding. Status code: {{ $json.statusCode }}"

### Step 5: Test and Activate

1. Click "Execute Workflow" to test
2. Verify the workflow executes correctly
3. Activate the workflow with the toggle switch

## 🔐 Working with Credentials

Many nodes require credentials to connect to services:

### Adding Credentials

1. Click "Create New Credential" in a node
2. Select credential type (e.g., Gmail, Slack, AWS)
3. Fill in required information (API keys, tokens, etc.)
4. Test the connection
5. Save

### Credential Types

- **OAuth2**: Connects via OAuth (Google, Microsoft, etc.)
- **API Key**: Simple API key authentication
- **Username/Password**: Basic authentication
- **Custom**: Advanced custom authentication

## 📊 Data Flow and Expressions

Data flows from node to node in JSON format. You can access data using expressions:

### Accessing Current Node Data

```javascript
{
  {
    $json.fieldName;
  }
}
{
  {
    $json["field-with-dash"];
  }
}
```

### Accessing Previous Node Data

```javascript
{
  {
    $node["NodeName"].json.fieldName;
  }
}
```

### Using Functions

```javascript
{
  {
    $now.toISO();
  }
}
{
  {
    $json.name.toUpperCase();
  }
}
{
  {
    $json.items.length;
  }
}
```

## 🛠️ Testing and Debugging

### Execute Node

Click "Execute Node" on any node to test it individually.

### View Data

Click on a node after execution to see:

- **Input Data**: Data received by the node
- **Output Data**: Data produced by the node
- **Execution Info**: Timing and status

### Common Issues

**No Data**: Check that previous nodes executed successfully and produced output.

**Error Messages**: Read the error carefully - n8n provides detailed error information.

**Data Not Matching**: Use the "Set" node to transform data into the expected format.

## 🎨 Common Workflow Patterns

### 1. Data Collection and Storage

Schedule → HTTP Request → Parse Data → Database Insert

### 2. Webhook Processing

Webhook → Validate Data → Process → Send Response

### 3. Notification System

Trigger → Check Condition → IF → Send Notification

### 4. Data Synchronization

Schedule → Fetch from Source → Transform → Update Destination

## ✅ Next Steps

You now understand n8n's core concepts! To deepen your knowledge:

1. **Try the examples**: Build the website monitoring workflow
2. **Explore By Example**: [n8n By Example](/en/learn/software-engineering/automation/tools/n8n/tutorials/by-example) - Practical workflow examples

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Understand the four types of nodes in n8n
- [ ] Create workflows with triggers and actions
- [ ] Use conditional logic with IF nodes
- [ ] Set up credentials for external services
- [ ] Access data using expressions
- [ ] Test and debug workflows
