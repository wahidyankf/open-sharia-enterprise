# CLAUDE.md

@AGENTS.md

## Platform Binding Examples

This file is the Claude Code platform-binding shim. The single-line `@AGENTS.md` directive above imports the canonical, vendor-neutral instruction surface. The rest of this file documents Claude-Code-specific binding details and is intentionally vendor-specific. Per the
[Governance Vendor-Independence Convention](./governance/conventions/structure/governance-vendor-independence.md),
the vendor-audit scanner skips every line under this heading until the next same-level heading or end of file.

### Markdown Quality (Claude Code hook)

In addition to the standard Prettier + markdownlint pipeline, a Claude Code hook auto-formats and lints after Edit/Write operations (requires `jq`).

### Working with `.claude/` and `.opencode/` directories

Edit `.claude/` and `.opencode/` files with normal `Write` / `Edit` tools. Both paths pre-authorized in `.claude/settings.json` (`Write(.claude/**)`, `Edit(.claude/**)`, `Write(.opencode/**)`, `Edit(.opencode/**)`), no approval prompt fires. `Bash` heredoc and `sed` remain fine for bulk mechanical substitutions, but no rule against direct edits.

**Applies to all paths**:

- `.claude/agents/*.md` — agent definition files (Claude Code format)
- `.claude/skills/*/SKILL.md` — agent skill files (source of truth for both Claude Code AND OpenCode; OpenCode reads natively per [opencode.ai/docs/skills](https://opencode.ai/docs/skills/), no mirror)
- `.claude/skills/*/reference/*.md` — skill reference modules
- `.opencode/agents/*.md` — OpenCode agent mirrors (auto-synced from `.claude/agents/`)

**See**: [primary binding agent catalog](./.claude/agents/README.md)

### Dual-mode configuration (Claude Code + OpenCode)

Repo maintains **dual compatibility** with Claude Code and OpenCode:

- **`.claude/`**: Source of truth (PRIMARY) — All updates happen here first
- **`.opencode/`**: Auto-generated (SECONDARY) — Synced from `.claude/`

**Making changes:**

1. Edit agents/skills in `.claude/` first
2. Run sync: `npm run sync:claude-to-opencode`
3. Both systems stay synced automatically

**Format differences:**

- **Tools**: Claude Code uses arrays `[Read, Write]`, OpenCode uses boolean flags `{ read: true, write: true }`
- **Models**: Claude Code uses `sonnet`/`opus`/`haiku` (or omits for budget-adaptive opus-inherit — intentional, not legacy); OpenCode uses `opencode-go/minimax-m2.7` (opus/sonnet/omitted) and `opencode-go/glm-5` (haiku). See [model-selection.md](./governance/development/agents/model-selection.md) for full capability-tier mapping.
- **Skills**: NOT mirrored — OpenCode reads `.claude/skills/{name}/SKILL.md` natively per [opencode.ai/docs/skills](https://opencode.ai/docs/skills/). The validate:sync `No Synced Skill Mirror` check fails if a stale `.opencode/skill/` or `.opencode/skills/<claude-name>` mirror reappears.
- **Permissions**: Claude Code uses `settings.json` permissions, OpenCode uses `opencode.json` permission block (both configured with equivalent access)
- **Colors**: Claude Code agents use named colors (`blue`, `green`, `yellow`, `purple`, etc.) written by hand in `.claude/agents/*.md`. `rhino-cli agents sync` translates these to OpenCode theme tokens (`primary`, `success`, `warning`, `secondary`, etc.) when generating `.opencode/agents/*.md` — current OpenCode rejects named colors. See [Dual-Mode Color Translation](./governance/development/agents/ai-agents.md#dual-mode-color-translation-claude-code-to-opencode) for the full mapping.
- **MCP/Plugins**: Claude Code uses plugins (Context7, Playwright, Nx, LSPs), OpenCode uses MCP servers (Playwright, Nx, Perplexity)

**Security policy**: Only use skills from trusted sources. All skills in this repo maintained by project team.

**See**: [primary binding agent catalog](./.claude/agents/README.md)

### organiclever-web skill

The organiclever-web content development skill is at [.claude/skills/apps-organiclever-web-developing-content/SKILL.md](./.claude/skills/apps-organiclever-web-developing-content/SKILL.md).

<!-- nx configuration start-->
<!-- Leave the start & end comments to automatically receive updates. -->

### Nx-related notes (Claude-Code binding)

The Nx tooling guidelines, generator usage, and `nx_docs` policy are documented in [`AGENTS.md`](./AGENTS.md) and apply identically here. The `<!-- nx configuration -->` markers above are preserved so the Nx auto-injection tool can refresh content if needed.

<!-- nx configuration end-->

<!-- rtk-instructions v2 -->

### RTK and caveman (Claude-Code binding)

The RTK CLI proxy and caveman token-compression skill are documented in [`AGENTS.md`](./AGENTS.md#platform-binding-examples) under the Platform Binding Examples section, and apply identically here. The `<!-- rtk-instructions -->` markers above are preserved so the RTK auto-injection tool can refresh content if needed.

<!-- /rtk-instructions -->
