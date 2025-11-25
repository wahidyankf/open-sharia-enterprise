# Plans

This folder contains temporary, ephemeral project planning documents, distinct from permanent documentation in `docs/`.

## Structure

```
plans/
├── in-progress/      # Active plans currently being worked on
├── backlog/         # Planned projects for future implementation
└── done/            # Completed and archived plans
```

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
5. **Diagram Format**: ASCII art only (since plans/ is outside the Obsidian vault)

## Working with Plans

- **Creating Plans**: Place new plans in `backlog/` folder
- **Starting Work**: Move plan folder from `backlog/` to `in-progress/`
- **Completing Work**: Move plan folder from `in-progress/` to `done/`
- **Plan Index**: Each subfolder has a README.md listing all plans in that category

## Diagrams in Plans

Since `plans/` is outside the `docs/` folder (Obsidian vault), **all files in plans/ must use ASCII art** for diagrams and schemas. This ensures universal compatibility across text editors, terminals, and version control tools.

For details, see [Diagram and Schema Convention](../docs/explanation/conventions/ex-co__diagrams.md).

## Related Documentation

For guidance on organizing your work:

- [How to Organize Your Work](../docs/how-to/ht__organize-work.md) - Decision guide for choosing between journals/, plans/, and docs/

For permanent documentation, see:

- [Documentation Home](../docs/README.md)
- [CLAUDE.md Plans Organization](../CLAUDE.md#plans-organization)
