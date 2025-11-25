# AI Agents

This directory contains specialized AI agents for repository maintenance and documentation tasks. Each agent has specific responsibilities and tools to ensure consistency, quality, and adherence to project conventions.

## Available Agents

### `doc-writer.md`

Expert documentation writer specializing in Obsidian-optimized markdown and Diátaxis framework.

- **Primary Use:** Creating, editing, or organizing project documentation
- **Specialization:** Markdown optimization, Diátaxis framework, convention compliance
- **Tools:** Read, Write, Edit, Glob, Grep
- **When to Use:**
  - Creating new documentation files
  - Editing existing documentation for clarity or structure
  - Organizing documentation according to Diátaxis framework
  - Ensuring documentation follows file naming and linking conventions

### `repo-rule-checker.md`

Validates consistency between agents, CLAUDE.md, conventions, and documentation.

- **Primary Use:** Checking for inconsistencies, contradictions, or verifying compliance
- **Specialization:** Cross-file validation, duplication detection, convention enforcement
- **Tools:** Read, Glob, Grep
- **When to Use:**
  - After making changes to conventions or CLAUDE.md
  - Periodic audits of repository consistency
  - Validating that all files follow documented standards
  - Detecting contradictions or outdated references
  - Identifying duplicate content that could be consolidated

### `repo-rule-updater.md`

Propagates rule and convention changes across CLAUDE.md, convention docs, agents, and indices.

- **Primary Use:** Adding/modifying rules, conventions, or standards affecting multiple files
- **Specialization:** Systematic propagation, cascade updates, consistency maintenance
- **Tools:** Read, Edit, Glob, Grep
- **When to Use:**
  - Adding new conventions or standards
  - Modifying existing rules that affect multiple files
  - Ensuring changes cascade to all relevant locations
  - Updating cross-references after structural changes
  - Maintaining consistency across agent definitions

## Agent Workflow

The three agents work together in a complementary cycle:

```
1. Make Changes
   └─> Use repo-rule-updater to propagate across files
        └─> Ensures consistency in CLAUDE.md, conventions, agents, indices

2. Validate Changes
   └─> Use repo-rule-checker to verify consistency
        └─> Detects inconsistencies, contradictions, duplications

3. Fix Issues (if any)
   └─> Use repo-rule-updater to fix detected issues
        └─> Return to step 2 for re-validation

4. Write/Update Documentation
   └─> Use doc-writer for documentation tasks
        └─> Ensures proper formatting and convention compliance
```

## Best Practices

- **After adding new conventions:** Use `repo-rule-updater` → `repo-rule-checker`
- **Before major releases:** Run `repo-rule-checker` for full audit
- **When creating documentation:** Use `doc-writer` for proper structure
- **When modifying CLAUDE.md:** Use `repo-rule-updater` to cascade changes

## Resources

- [AI Agents Convention](../docs/explanation/development/ex-de__ai-agents.md) - Complete agent specification and standards
- [CLAUDE.md](../CLAUDE.md) - Project guidance for all agents
- [Documentation Conventions](../docs/explanation/conventions/README.md) - File naming, linking, and Diátaxis framework
- [Plans Organization](../plans/README.md) - Planning document structure and conventions (use ASCII art for diagrams)

## Adding New Agents

When creating new agents:

1. Follow the [AI Agents Convention](../docs/explanation/development/ex-de__ai-agents.md)
2. Add the agent to this README index
3. Use `repo-rule-updater` to propagate references to CLAUDE.md and other files
4. Use `repo-rule-checker` to validate the new agent follows all conventions
5. Update CLAUDE.md if the agent should be mentioned in project guidance

---

**Note:** This README follows the naming exception for README.md files documented in the [File Naming Convention](../docs/explanation/conventions/ex-co__file-naming-convention.md).
