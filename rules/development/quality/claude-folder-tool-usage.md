# .claude/ Folder Tool Usage Convention

**Status:** Active
**Version:** 1.0.0
**Last Updated:** 2026-01-03

## Purpose

Specifies which tools AI agents MUST use when performing create/update/delete operations on files within `.claude/` directories. Ensures autonomous agent operation without user approval prompts.

## Standards

### Mandatory Tool Selection

**For ALL operations on `.claude/` files (create, update, delete):**

1. **MUST use Bash tools:**
   - `heredoc` (cat << 'EOF') for multi-line content creation
   - `sed` for targeted string replacements
   - `awk` for complex text processing
   - `cat` with redirects (`>`, `>>`) for simple writes
   - `mkdir` for directory creation
   - `rm` for file deletion

2. **MUST NOT use Write/Edit tools:**
   - Write tool
   - Edit tool
   - Create tool (if available)
   - Update tool (if available)

### Scope

Applies to ALL files in `.claude/` directory tree:

```
.claude/
├── agents/          # Agent definitions (maker, checker, fixer, etc.)
├── skills/          # Skills (knowledge delivery infrastructure)
├── workflows/       # Workflow orchestrations
├── settings/        # Claude Code settings (gitignored, but rule applies)
└── any-subdirs/     # Any future .claude/ subdirectories
```

### Rationale

1. **Autonomous Operation:** Bash tools execute without requiring user approval prompts
2. **Agent Independence:** Agents can create/update .claude/ files without supervision
3. **Version Control Safety:** Git provides version control, so Write/Edit approval prompts are unnecessary
4. **Workflow Efficiency:** Multi-agent workflows can complete without interruption

### Implementation

Agents MUST declare required tools in frontmatter:

```yaml
---
name: agent-name
description: Agent description
tools:
  - Bash # REQUIRED for .claude/ operations
  - WebSearch # Additional tools as needed
model: claude-opus-4-5-20251101
color: blue
---
```

### Bash Tool Patterns

#### Creating New Files (Heredoc)

```bash
cat > /path/to/.claude/file.md << 'EOF'
# File content
markdown content here
EOF
```

#### Updating Existing Files (Sed)

```bash
# Replace single occurrence
sed -i 's/old-string/new-string/' /path/to/.claude/file.md

# Replace all occurrences
sed -i 's/old-string/new-string/g' /path/to/.claude/file.md

# Replace with special characters
sed -i 's|old/pattern|new/pattern|g' /path/to/.claude/file.md
```

#### Appending Content

```bash
cat >> /path/to/.claude/file.md << 'EOF'

# Additional content
EOF
```

#### Deleting Files

```bash
rm /path/to/.claude/file.md
```

### Exception Handling

**Exceptions require explicit justification:**

- If Write/Edit tools MUST be used (rare), agent MUST:
  1. Document justification in agent description
  2. Warn user about approval prompts in output
  3. Provide alternative Bash approach if possible

## Examples

### Example 1: Agent Creating Skill File

**Bash approach (CORRECT):**

```bash
cat > /home/wkf/wkf-repos/wahidyankf/open-sharia-enterprise/.claude/skills/docs__creating-accessible-diagrams.md << 'EOF'
---
name: docs__creating-accessible-diagrams
description: Creates accessible diagrams following diagram convention
...

# Accessible Diagram Creation

Content here...
EOF
```

**Write tool approach (INCORRECT):**

```
❌ Using Write tool requires user approval
❌ Breaks autonomous operation
❌ Not allowed for .claude/ files
```

### Example 2: Agent Updating Agent Definition

**Sed approach (CORRECT):**

```bash
# Update version in frontmatter
sed -i 's/version: 1\.0\.0/version: 1.0.1/' /path/to/.claude/agents/agent-name.md

# Add new tool to tools list
sed -i '/tools:/a\  - NewTool' /path/to/.claude/agents/agent-name.md
```

**Edit tool approach (INCORRECT):**

```
❌ Using Edit tool triggers approval prompt
❌ Workflow interrupted
❌ Violates this convention
```

### Example 3: Agent Agent Maker Creating New Agent

**Bash approach (CORRECT):**

```bash
# Create new agent file
cat > /home/wkf/wkf-repos/wahidyankf/open-sharia-enterprise/.claude/agents/new-agent.md << 'EOF'
---
name: new-agent
description: New agent description
tools:
  - Bash
model: claude-opus-4-5-20251101
color: green
---

# New Agent

Agent content here...
EOF

# Update agents README
sed -i '/## Available Agents/a\**New Agent**: new-agent - Description' /home/wkf/wkf-repos/wahidyankf/open-sharia-enterprise/.claude/agents/README.md
```

## Validation

### For Agent Makers

**agent-maker MUST:**

1. Verify all agents creating/updating .claude/ files have Bash tool
2. Check agent content for Write/Edit tool usage warnings
3. Validate examples demonstrate Bash tool patterns

### For Agent Checkers

**agent-checker MUST:**

1. Verify .claude/ file operations use Bash tools only
2. Flag any Write/Edit tool references in agent descriptions
3. Check for proper tool declarations in frontmatter

### For Developers

**Manual verification:**

1. Agent files in `.claude/agents/` list Bash in `tools:` frontmatter
2. Agent descriptions mention Bash tools for .claude/ operations
3. Examples demonstrate heredoc/sed patterns, not Write/Edit

## Related Documentation

- [AI Agents Convention](./ex-ru-de-ag__ai-agents.md) - Agent structure and standards
- [Maker-Checker-Fixer Pattern](../pattern/ex-ru-de-pa__maker-checker-fixer.md) - Quality workflow
- [Skills Directory](/.claude/skills/README.md) - Skills infrastructure
- [Agents Index](/.claude/agents/README.md) - All agents and responsibilities

## Principles Respected

- **Explicit Over Implicit**: Clear tool selection rules documented
- **Automation Over Manual**: Autonomous agent operation enabled
- **Simplicity Over Complexity**: Single straightforward rule for all .claude/ operations

## Conventions Implemented/Respected

- **Convention Writing Convention**: Follows standard structure (Purpose, Standards, Examples, Validation)
- **Content Quality Principles**: Clear language, examples, validation criteria
