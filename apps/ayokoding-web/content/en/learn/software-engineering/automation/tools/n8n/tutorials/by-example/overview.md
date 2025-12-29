---
title: "Overview"
date: 2025-12-29T11:09:00+07:00
draft: false
weight: 10000000
description: "Learn n8n through 85+ annotated workflow examples covering 95% of the platform - ideal for experienced developers building production automations"
tags: ["n8n", "tutorial", "by-example", "examples", "code-first", "workflow", "automation"]
---

## What Is By-Example

This tutorial teaches n8n through **85 heavily annotated workflow examples** that you can copy, run, and modify immediately. Unlike traditional tutorials that explain concepts before showing code, we show working workflows first and explain through inline annotations.

**Target audience**: Experienced developers who prefer learning through working code rather than narrative explanations. You should be comfortable with APIs, JSON, and basic programming concepts.

## What 95% Coverage Means

This tutorial covers **95% of n8n features needed for production workflow automation**:

**Included** (the 95%):

- Core workflow concepts and node types
- Trigger mechanisms (webhook, schedule, manual)
- Data transformation and expressions
- Error handling and retry strategies
- HTTP requests and API integration
- Database operations (PostgreSQL, MongoDB)
- File processing and manipulation
- Conditional logic and routing
- Loops and batch processing
- Sub-workflows and modularity
- Authentication patterns
- Production deployment
- Performance optimization
- Monitoring and debugging

**Excluded** (the remaining 5%):

- Rare edge case nodes
- Platform internals and source code
- Custom node development
- Specialized integrations outside common use
- Deprecated features

## How to Use This Tutorial

### Setup Requirements

All examples use Docker-based execution for reproducibility:

```bash
# Start n8n in Docker
docker run -it --rm \
  --name n8n \
  -p 5678:5678 \
  -v ~/.n8n:/home/node/.n8n \
  n8nio/n8n

# Access n8n at http://localhost:5678
```

### Example Format

Every example follows a **four-part format**:

1. **Brief explanation** (2-3 sentences) - What, why, when
2. **Mermaid diagram** (30-50% of examples) - Visual workflow representation
3. **Annotated workflow JSON** - Copy-paste-runnable with `-- =>` annotations
4. **Key takeaway** (1-2 sentences) - Core insight and production guidance

### Self-Containment

Examples are **self-contained within their level**:

- **Beginner**: Each example is completely standalone
- **Intermediate**: Assumes beginner concepts, but runnable independently
- **Advanced**: Assumes beginner + intermediate knowledge, still fully runnable

**Golden rule**: You can copy any example, paste it into n8n, and run it immediately without referring to other examples.

### Reading Annotations

Workflow JSON annotations use `-- =>` notation to show data flow and state changes:

```json
{
  "parameters": {
    "url": "https://api.example.com/users" // -- => HTTP GET request to API
  },
  "position": [250, 300] // -- => Visual position in n8n editor
}
```

**Key annotation patterns**:

- `// -- => Output: {...}` - Shows node execution result
- `// -- => Data transformed: {...}` - Shows data transformation
- `// -- => Conditional: true/false` - Shows condition evaluation
- `// -- => Error: ...` - Shows error handling

## Tutorial Structure

### Beginner (Examples 1-30, 0-40% coverage)

**Focus**: n8n fundamentals and core workflow concepts

**Topics**: Installation, first workflow, manual triggers, basic nodes (HTTP Request, Set, IF), credentials, webhook triggers, schedule triggers, data inspection, basic expressions, simple error handling, testing workflows

**Diagram frequency**: ~30%

### Intermediate (Examples 31-60, 40-75% coverage)

**Focus**: Production patterns and advanced nodes

**Topics**: Advanced data transformation, complex expressions, loops and iterations, switch/router nodes, merge patterns, function nodes, database operations, file processing, email automation, webhook authentication, error workflows, retry strategies, environment variables

**Diagram frequency**: ~40%

### Advanced (Examples 61-85, 75-95% coverage)

**Focus**: Expert mastery and optimization

**Topics**: Sub-workflows, workflow composition, advanced error handling, performance optimization, batch processing, API rate limiting, custom functions, queue patterns, monitoring strategies, production deployment, debugging techniques, security patterns, scaling patterns, best practices

**Diagram frequency**: ~50%

## Example Count Distribution

| Level        | Examples | Coverage | Focus                           |
| ------------ | -------- | -------- | ------------------------------- |
| Beginner     | 1-30     | 0-40%    | Fundamentals and syntax         |
| Intermediate | 31-60    | 40-75%   | Production patterns             |
| Advanced     | 61-85    | 75-95%   | Expert mastery and optimization |
| **Total**    | **85**   | **95%**  | **Comprehensive coverage**      |

## Why Docker-Based Examples

All examples use Docker deployment because:

- **Reproducible**: Same environment for all users
- **Isolated**: No conflicts with system packages
- **Portable**: Works on Linux, macOS, Windows
- **Production-like**: Docker is common n8n deployment method
- **Clean**: Easy to reset and start fresh

## Learning Path

**Recommended progression**:

1. **Start with beginner** - Even if experienced, understand n8n concepts
2. **Work sequentially** - Examples build conceptual knowledge
3. **Run every example** - Learning happens through execution
4. **Modify examples** - Change parameters and observe results
5. **Reference advanced** - Jump to specific patterns as needed

**Not recommended**:

- Skipping to advanced without basics
- Reading without running
- Memorizing workflows
- Using as reference without understanding

## Next Steps

Ready to build production workflows? Start with **Beginner** examples to understand n8n fundamentals, then progress to **Intermediate** for production patterns, and finally **Advanced** for optimization and scaling.

## Additional Resources

- [n8n Official Documentation](https://docs.n8n.io)
- [n8n Community Forum](https://community.n8n.io)
- [n8n Workflow Templates](https://n8n.io/workflows)
- [n8n GitHub Repository](https://github.com/n8n-io/n8n)
