# Plan: Enhanced Env Backup/Restore — Confirmation Prompts and Config File Support

**Status**: In Progress
**Created**: 2026-03-31

## Overview

Extend `rhino-cli env backup` and `env restore` with three changes:

1. **Overwrite confirmation prompt** — When destination files already exist, prompt the user for
   confirmation before overwriting. Add `--force` flag to skip the prompt (for CI/scripts).
2. **Uncommitted config file backup** — Extend backup/restore to also handle known uncommitted local
   configuration files (`.claude/settings.local.json`, AI tool configs, Docker overrides, etc.) via a
   new `--include-config` flag.
3. **Rename default backup directory** — Change from `~/ose-env-bkup` to `~/ose-open-env-backup`
   for clarity and consistency with the project name.

**Git Workflow**: Commit to `main` (Trunk Based Development)

## Quick Links

- [Requirements](./requirements.md) — Functional requirements, edge cases, and acceptance criteria
- [Technical Documentation](./tech-docs.md) — Architecture, config patterns, and testability design
- [Delivery Plan](./delivery.md) — Phased checklist and validation steps

## Why This Matters

1. **Data safety**: The current implementation silently overwrites existing files. A confirmation
   prompt prevents accidental loss of newer backup or manually-edited env files.
2. **Config file loss**: Developer-local config files (AI tool settings, Docker overrides, version
   manager locals) are gitignored and equally vulnerable to loss as `.env` files — but the current
   backup only covers `.env*` files.
3. **AI tool proliferation**: Modern developer environments have settings from multiple AI tools
   (Claude Code, Cursor, Windsurf, Cline, Gemini CLI, etc.) that contain MCP server configs, API
   keys, and personal preferences. Losing these disrupts workflow significantly.
4. **CI compatibility**: The `--force` flag ensures automated scripts and CI pipelines can use
   backup/restore non-interactively without hanging on a prompt.
