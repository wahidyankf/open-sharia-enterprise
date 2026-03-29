---
title: "Quick Start"
date: 2026-02-02T00:00:00+07:00
draft: false
weight: 100002
description: "Build a REST API with Claude Code in 30 minutes - learn essential commands, file operations, and AI-assisted development workflows (5-30% coverage)"
tags: ["claude-code", "quick-start", "tutorial", "rest-api", "hands-on", "project-based"]
---

**Build a working REST API in 30 minutes with Claude Code.** This hands-on tutorial teaches essential Claude Code commands while creating a practical project - a task management REST API with full CRUD operations.

## What You'll Build

A **Task Management REST API** with:

- Create, read, update, delete tasks
- In-memory storage (simple for learning)
- JSON responses
- Error handling
- Basic validation

**Technologies**: Node.js + Express (or your preferred stack)

**What you'll learn**:

- Starting conversations with Claude Code
- Asking for code generation
- Reading and understanding AI responses
- Editing files based on suggestions
- Running and testing code
- Iterating based on errors

## Prerequisites

- Claude Code installed and configured ([Initial Setup](/en/learn/software-engineering/automation-tools/claude-code/initial-setup))
- Basic programming experience (any language)
- Terminal familiarity

**No Node.js knowledge required** - Claude Code will guide you through it.

## Project Setup

### Step 1: Create Project Directory

```bash
mkdir task-api
cd task-api

# Initialize Claude Code
claude init
```

**Output**:

```
✓ Created .claude/ directory
✓ Indexed 0 files
✓ Ready to use Claude Code
```

### Step 2: Start Conversation

```bash
# Start interactive session
claude
```

**You'll see**:

```
Claude Code v1.0.0
Ready to assist with your code

You:
```

## Building the API

### Step 3: Request Project Scaffold

**Type this** at the `You:` prompt:

```
Create a Node.js REST API for task management with Express. Include:
- package.json with dependencies
- server.js with basic Express setup
- routes for CRUD operations on tasks
- In-memory storage (array)
- Each task has: id, title, description, completed
```

**Claude Code responds**:

```
I'll create a task management REST API. Let me set up the project structure.

[Creates files]

Created:
- package.json
- server.js
- routes/tasks.js

Would you like me to explain the code or should we test it?
```

**Files created**:

`package.json`:

```json
{
  "name": "task-api",
  "version": "1.0.0",
  "description": "Task Management REST API",
  "main": "server.js",
  "scripts": {
    "start": "node server.js",
    "dev": "nodemon server.js"
  },
  "dependencies": {
    "express": "^4.18.2"
  },
  "devDependencies": {
    "nodemon": "^3.0.1"
  }
}
```

`server.js`:

```javascript
const express = require("express");
const taskRoutes = require("./routes/tasks");

const app = express();
const PORT = 3000;

// Middleware
app.use(express.json());

// Routes
app.use("/api/tasks", taskRoutes);

// Start server
app.listen(PORT, () => {
  console.log(`Server running on http://localhost:${PORT}`);
});
```

`routes/tasks.js`:

```javascript
const express = require("express");
const router = express.Router();

// In-memory storage
let tasks = [];
let nextId = 1;

// GET /api/tasks - List all tasks
router.get("/", (req, res) => {
  res.json(tasks);
});

// GET /api/tasks/:id - Get single task
router.get("/:id", (req, res) => {
  const task = tasks.find((t) => t.id === parseInt(req.params.id));
  if (!task) {
    return res.status(404).json({ error: "Task not found" });
  }
  res.json(task);
});

// POST /api/tasks - Create task
router.post("/", (req, res) => {
  const { title, description } = req.body;

  if (!title) {
    return res.status(400).json({ error: "Title is required" });
  }

  const task = {
    id: nextId++,
    title,
    description: description || "",
    completed: false,
  };

  tasks.push(task);
  res.status(201).json(task);
});

// PUT /api/tasks/:id - Update task
router.put("/:id", (req, res) => {
  const task = tasks.find((t) => t.id === parseInt(req.params.id));

  if (!task) {
    return res.status(404).json({ error: "Task not found" });
  }

  const { title, description, completed } = req.body;

  if (title !== undefined) task.title = title;
  if (description !== undefined) task.description = description;
  if (completed !== undefined) task.completed = completed;

  res.json(task);
});

// DELETE /api/tasks/:id - Delete task
router.delete("/:id", (req, res) => {
  const index = tasks.findIndex((t) => t.id === parseInt(req.params.id));

  if (index === -1) {
    return res.status(404).json({ error: "Task not found" });
  }

  tasks.splice(index, 1);
  res.status(204).send();
});

module.exports = router;
```

### Step 4: Install Dependencies

**Ask Claude Code**:

```
You: How do I install dependencies?
```

**Claude Code responds**:

```
Run this command:

npm install

This installs Express and nodemon from package.json.
```

**Run it**:

```bash
npm install
```

### Step 5: Test the API

**Ask Claude Code**:

```
You: How do I test this API?
```

**Claude Code responds**:

```
Let me create a test script using curl commands.

[Creates test.sh]
```

`test.sh`:

```bash
#!/bin/bash

echo "Creating tasks..."
curl -X POST http://localhost:3000/api/tasks \
  -H "Content-Type: application/json" \
  -d '{"title":"Learn Claude Code","description":"Complete quick start tutorial"}'

curl -X POST http://localhost:3000/api/tasks \
  -H "Content-Type: application/json" \
  -d '{"title":"Build REST API","description":"Using Express and Node.js"}'

echo -e "\n\nListing all tasks..."
curl http://localhost:3000/api/tasks

echo -e "\n\nGetting task 1..."
curl http://localhost:3000/api/tasks/1

echo -e "\n\nUpdating task 1..."
curl -X PUT http://localhost:3000/api/tasks/1 \
  -H "Content-Type: application/json" \
  -d '{"completed":true}'

echo -e "\n\nDeleting task 2..."
curl -X DELETE http://localhost:3000/api/tasks/2

echo -e "\n\nFinal task list..."
curl http://localhost:3000/api/tasks
```

**Run server**:

```bash
npm start
```

**In another terminal**:

```bash
chmod +x test.sh
./test.sh
```

**Expected output**:

```json
Creating tasks...
{"id":1,"title":"Learn Claude Code","description":"Complete quick start tutorial","completed":false}
{"id":2,"title":"Build REST API","description":"Using Express and Node.js","completed":false}

Listing all tasks...
[{"id":1,"title":"Learn Claude Code","description":"Complete quick start tutorial","completed":false},{"id":2,"title":"Build REST API","description":"Using Express and Node.js","completed":false}]

Getting task 1...
{"id":1,"title":"Learn Claude Code","description":"Complete quick start tutorial","completed":false}

Updating task 1...
{"id":1,"title":"Learn Claude Code","description":"Complete quick start tutorial","completed":true}

Deleting task 2...

Final task list...
[{"id":1,"title":"Learn Claude Code","description":"Complete quick start tutorial","completed":true}]
```

## Adding Features

### Step 6: Add Input Validation

**Ask Claude Code**:

```
You: Add validation to ensure title is not empty and description doesn't exceed 500 characters
```

**Claude Code**:

```
I'll update the POST and PUT routes with validation.

[Shows diff for routes/tasks.js]

Should I apply these changes?
```

**You**:

```
Yes
```

**Claude Code applies changes** and shows:

```
✓ Updated routes/tasks.js

The POST route now validates:
- Title is required and not empty
- Description max 500 characters

Want to test the validation?
```

**Updated code** (POST route section):

```javascript
router.post("/", (req, res) => {
  const { title, description } = req.body;

  // Validation
  if (!title || title.trim() === "") {
    return res.status(400).json({ error: "Title is required and cannot be empty" });
  }

  if (description && description.length > 500) {
    return res.status(400).json({ error: "Description cannot exceed 500 characters" });
  }

  const task = {
    id: nextId++,
    title: title.trim(),
    description: description?.trim() || "",
    completed: false,
  };

  tasks.push(task);
  res.status(201).json(task);
});
```

### Step 7: Add Error Handling Middleware

**Ask Claude Code**:

```
You: Add global error handling middleware to catch and format errors consistently
```

**Claude Code responds** with updated `server.js`:

```javascript
const express = require("express");
const taskRoutes = require("./routes/tasks");

const app = express();
const PORT = 3000;

// Middleware
app.use(express.json());

// Routes
app.use("/api/tasks", taskRoutes);

// 404 handler
app.use((req, res) => {
  res.status(404).json({ error: "Route not found" });
});

// Error handler
app.use((err, req, res, next) => {
  console.error(err.stack);
  res.status(500).json({
    error: "Internal server error",
    message: process.env.NODE_ENV === "development" ? err.message : undefined,
  });
});

// Start server
app.listen(PORT, () => {
  console.log(`Server running on http://localhost:${PORT}`);
});
```

### Step 8: Add Filtering

**Ask Claude Code**:

```
You: Add query parameter to filter tasks by completed status. Example: GET /api/tasks?completed=true
```

**Claude Code** updates GET route:

```javascript
router.get("/", (req, res) => {
  let filteredTasks = tasks;

  // Filter by completed status if query parameter provided
  if (req.query.completed !== undefined) {
    const isCompleted = req.query.completed === "true";
    filteredTasks = tasks.filter((t) => t.completed === isCompleted);
  }

  res.json(filteredTasks);
});
```

**Test filtering**:

```bash
# Get only completed tasks
curl "http://localhost:3000/api/tasks?completed=true"

# Get only incomplete tasks
curl "http://localhost:3000/api/tasks?completed=false"
```

## Iterating on Errors

### Step 9: Handle JSON Parse Errors

**Test invalid JSON**:

```bash
curl -X POST http://localhost:3000/api/tasks \
  -H "Content-Type: application/json" \
  -d '{invalid json}'
```

**You get 500 error**. Ask Claude Code:

```
You: The server crashes when I send invalid JSON. How do I handle this gracefully?
```

**Claude Code responds**:

```
Add error handling for malformed JSON in the middleware stack.

[Shows code]
```

**Updated server.js** (add before routes):

```javascript
// JSON parsing error handler
app.use((err, req, res, next) => {
  if (err instanceof SyntaxError && err.status === 400 && "body" in err) {
    return res.status(400).json({ error: "Invalid JSON" });
  }
  next(err);
});
```

### Step 10: Add Tests

**Ask Claude Code**:

```
You: Generate unit tests for the task routes using Jest
```

**Claude Code creates**:

`package.json` (updated):

```json
{
  "devDependencies": {
    "nodemon": "^3.0.1",
    "jest": "^29.7.0",
    "supertest": "^6.3.3"
  },
  "scripts": {
    "test": "jest"
  }
}
```

`routes/tasks.test.js`:

```javascript
const request = require("supertest");
const express = require("express");
const taskRoutes = require("./tasks");

const app = express();
app.use(express.json());
app.use("/api/tasks", taskRoutes);

describe("Task API", () => {
  beforeEach(() => {
    // Reset tasks before each test
    // Note: This requires exposing tasks array or using dependency injection
  });

  test("GET /api/tasks returns empty array initially", async () => {
    const response = await request(app).get("/api/tasks");
    expect(response.status).toBe(200);
    expect(response.body).toEqual([]);
  });

  test("POST /api/tasks creates a task", async () => {
    const response = await request(app)
      .post("/api/tasks")
      .send({ title: "Test Task", description: "Test Description" });

    expect(response.status).toBe(201);
    expect(response.body).toMatchObject({
      id: expect.any(Number),
      title: "Test Task",
      description: "Test Description",
      completed: false,
    });
  });

  test("POST /api/tasks returns 400 without title", async () => {
    const response = await request(app).post("/api/tasks").send({ description: "No title" });

    expect(response.status).toBe(400);
    expect(response.body.error).toBeDefined();
  });

  // More tests...
});
```

**Run tests**:

```bash
npm install --save-dev jest supertest
npm test
```

## What You've Learned

**Essential Commands**:

- `claude` - Start interactive session
- Ask questions in natural language
- Claude Code creates/edits files automatically
- Iterate based on errors and feedback

**Workflow Pattern**:

1. Describe what you want
2. Claude Code generates code
3. Review and test
4. Ask for modifications
5. Repeat until satisfied

**Key Capabilities**:

- Code generation from descriptions
- File creation and editing
- Error diagnosis and fixing
- Test generation
- Explanations and documentation

## Project Structure

Your final project:

```
task-api/
├── package.json
├── server.js
├── routes/
│   ├── tasks.js
│   └── tasks.test.js
├── test.sh
├── .claude/
│   └── context.json
└── node_modules/
```

## Next Steps

### Enhance the API

Try asking Claude Code to:

1. **Add persistence**: "Add SQLite database instead of in-memory storage"
2. **Add authentication**: "Add JWT authentication to protect routes"
3. **Add pagination**: "Add pagination to GET /api/tasks with page and limit query params"
4. **Add search**: "Add search by title using query parameter"
5. **Add sorting**: "Add sorting by created date or title"

### Explore Features

- **Read existing code**: "Explain how the PUT route works"
- **Refactor**: "Refactor routes to use async/await for future database integration"
- **Add documentation**: "Generate OpenAPI/Swagger documentation"
- **Optimize**: "What are potential performance issues in this code?"

### Learn Advanced Workflows

Continue to:

- **By Example** → [By Example (75+ Examples)](/en/learn/software-engineering/automation-tools/claude-code/by-example) - Comprehensive coverage of all Claude Code features
- **Overview** → [Claude Code Overview](/en/learn/software-engineering/automation-tools/claude-code/overview) - Understand full capabilities

## Key Takeaways

**Conversational Development**:

- Describe goals in natural language
- Claude Code translates to working code
- Iterate through conversation

**AI as Pair Programmer**:

- Generates boilerplate quickly
- Explains unfamiliar patterns
- Suggests improvements
- Catches potential issues

**Rapid Prototyping**:

- Built complete REST API in 30 minutes
- No manual boilerplate writing
- Focus on business logic, not setup

**Learning by Doing**:

- Learn frameworks through practice
- Ask "how" and "why" questions
- Experiment with changes
- Build real projects

## Common Questions

**Q: Can I use languages other than JavaScript?**

A: Yes! Claude Code supports all major languages. Just specify in your request: "Create a Python Flask API..." or "Build this in Java Spring Boot..."

**Q: How do I undo changes?**

A: Use git. Claude Code respects version control:

```bash
git init
git add .
git commit -m "Initial commit"

# After Claude Code makes changes
git diff  # Review changes
git checkout .  # Undo if needed
```

**Q: Can Claude Code run commands?**

A: Yes! Ask it to run tests, start servers, or execute scripts. It can also interpret errors and suggest fixes.

**Q: Does this work offline?**

A: No, Claude Code requires internet connection for API calls. Cache helps with repeated queries.

**Q: How much does this cost?**

A: Based on Anthropic API usage (tokens consumed). This tutorial uses ~50k tokens (~$0.10 with Sonnet). Check [console.anthropic.com](https://console.anthropic.com) for current pricing.

## Coverage Achieved

**Quick Start Coverage: 5-30%**

You've learned:

- ✅ Starting conversations
- ✅ Code generation from descriptions
- ✅ File operations (create, edit, read)
- ✅ Testing and iteration
- ✅ Error handling and debugging
- ✅ Asking follow-up questions

**Not covered** (see By Example for these):

- Advanced prompting techniques
- Multi-file refactoring
- Codebase analysis
- Complex workflows
- Performance optimization
- Security patterns

**Ready for production?** Not yet. This API needs:

- Persistent storage (database)
- Authentication/authorization
- Input sanitization
- Rate limiting
- Logging and monitoring
- Production error handling

Learn these in [By Example](/en/learn/software-engineering/automation-tools/claude-code/by-example) tutorial!
