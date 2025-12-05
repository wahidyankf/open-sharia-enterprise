# Plans

<!--
  MAINTENANCE NOTE: Master reference for plans organization
  This is duplicated (intentionally) in multiple files for different audiences:
  1. plans/README.md (this file - comprehensive reference)
  2. CLAUDE.md (summary for AI agents)
  3. .claude/agents/plan-maker.md (context for plan creation)
  When updating, synchronize all three locations.
-->

This folder contains temporary, ephemeral project planning documents, distinct from permanent documentation in `docs/`.

## Structure

```
plans/
├── ideas.md         # Quick 1-3 liner ideas not yet formalized into plans
├── in-progress/      # Active plans currently being worked on
├── backlog/         # Planned projects for future implementation
└── done/            # Completed and archived plans
```

## Ideas File

The `ideas.md` file at the root of the plans/ folder is for capturing quick ideas and todos that haven't been formalized into full plan documents yet.

**Purpose:**

- Quick capture of project ideas (1-3 lines each)
- Todos for potential future plans
- Ideas that need more thought before becoming formal plans
- Lightweight brainstorming without full plan structure

**Format:**

- Simple markdown file with bullet points or numbered lists
- Each idea should be 1-3 lines maximum
- No formal structure required

**Example:**

```markdown
# Ideas

Quick ideas and todos that haven't been formalized into plans yet.

- Add OAuth2 authentication system with Google and GitHub providers
- Implement real-time notification system using WebSockets
- Create admin dashboard for user management and analytics
```

**How to promote an idea to a plan:**

When an idea is ready for formal planning:

1. Create a new plan folder in `backlog/` with `YYYY-MM-DD__[project-identifier]/` format
2. Create the standard plan files (README.md, requirements.md, tech-docs.md, delivery.md)
3. Remove or check off the idea from `ideas.md`
4. The idea now has a structured plan with requirements, technical docs, and delivery timeline

## Plan Folder Naming

Each plan folder follows the naming pattern:

```
YYYY-MM-DD__[project-identifier]/
```

Examples:

- `2025-11-24__init-monorepo/`
- `2025-12-01__auth-system/`

## Plan Contents

Each plan folder contains the following standard files (WITHOUT naming prefixes):

- `README.md` - Plan overview and navigation
- `requirements.md` - Detailed requirements and objectives
- `tech-docs.md` - Technical documentation and architecture
- `delivery.md` - Timeline and milestones

## Key Differences from Documentation

Plans differ from `docs/` in several important ways:

1. **Location**: Root-level `plans/` folder (not inside `docs/`)
2. **Purpose**: Temporary project planning (not permanent documentation)
3. **File Naming**: No prefixes inside plan folders (folder structure provides context)
4. **Lifecycle**: Plans move between in-progress, backlog, and done folders
5. **Diagram Format**: Mermaid (primary format for all markdown files), ASCII art optional for simple directory trees

## Working with Plans

- **Creating Plans**: Place new plans in `backlog/` folder
- **Starting Work**: Move plan folder from `backlog/` to `in-progress/`
- **Completing Work**: Move plan folder from `in-progress/` to `done/`
- **Plan Index**: Each subfolder has a README.md listing all plans in that category

## Diagrams in Plans

Files in `plans/` folder should use **Mermaid diagrams** as the primary format (same as all markdown files in the repository). ASCII art is optional and only needed for simple directory trees or rare edge cases. See [Diagram and Schema Convention](../docs/explanation/conventions/ex-co__diagrams.md) for complete details.

## Related Documentation

For guidance on organizing your work:

- [How to Organize Your Work](../docs/how-to/hoto__organize-work.md) - Decision guide for choosing between journals/, plans/, and docs/

For permanent documentation, see:

- [Documentation Home](../docs/README.md)
- [CLAUDE.md Plans Organization](../CLAUDE.md#plans-organization)
