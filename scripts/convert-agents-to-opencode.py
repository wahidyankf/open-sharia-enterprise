#!/usr/bin/env python3
"""
Convert Claude Code agents to OpenCode format.

This script:
1. Parses Claude Code frontmatter (.claude/agents/*.md)
2. Maps fields to OpenCode schema
3. Embeds missing fields (skills, color, timestamps) in body
4. Augments body with workflow logic (checkers/fixers)
5. Writes OpenCode agents to .opencode/agent/

Usage:
    python scripts/convert-agents-to-opencode.py
    python scripts/convert-agents-to-opencode.py --agent docs-maker
    python scripts/convert-agents-to-opencode.py --dry-run
"""

import os
import re
import yaml
import argparse
from pathlib import Path
from typing import Dict, List, Optional, Set
from dataclasses import dataclass

@dataclass
class AgentMetadata:
    """Metadata extracted from Claude Code agent."""
    name: str
    description: str
    tools: List[str]
    model: str
    color: str
    skills: List[str]
    created: Optional[str]
    updated: Optional[str]
    body: str  # Original markdown body after frontmatter


# Tool mapping: Claude Code → OpenCode (lowercase)
TOOL_NAME_MAP = {
    'Read': 'read',
    'Write': 'write',
    'Edit': 'edit',
    'Glob': 'glob',
    'Grep': 'grep',
    'Bash': 'bash',
    'WebFetch': 'webfetch',
    'WebSearch': 'websearch',
    'TodoWrite': 'todowrite'
}

# Model mapping: Claude Code → OpenCode
MODEL_MAP = {
    'inherit': None,  # Use config default
    'sonnet': 'zai/glm-4.7',
    'haiku': 'zai/glm-4.5-air',
    'opus': 'zai/glm-4.7'  # Map opus to sonnet equivalent
}

# Color → Role mapping
COLOR_ROLE_MAP = {
    'blue': 'Writer',
    'green': 'Checker',
    'yellow': 'Updater',
    'purple': 'Implementor',
    'orange': 'Specialist'
}

# Agents that should be subagent mode (spawned by workflows)
SUBAGENT_NAMES = {
    'plan-execution-checker',
    'docs-checker',
    'docs-tutorial-checker',
    'readme-checker',
    'repo-governance-checker',
    'repo-workflow-checker',
    'apps-ayokoding-web-general-checker',
    'apps-ayokoding-web-by-example-checker',
    'apps-ayokoding-web-facts-checker',
    'apps-ayokoding-web-link-checker',
    'apps-ayokoding-web-structure-checker',
    'apps-ose-platform-web-content-checker',
    'plan-checker'
}


def parse_claude_agent(filepath: Path) -> AgentMetadata:
    """Parse Claude Code agent file and extract metadata."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    # Extract frontmatter
    match = re.match(r'^---\s*\n(.*?)\n---\s*\n(.*)$', content, re.DOTALL)
    if not match:
        raise ValueError(f"No frontmatter found in {filepath}")

    frontmatter_text = match.group(1)
    body = match.group(2)

    # Parse YAML frontmatter
    frontmatter = yaml.safe_load(frontmatter_text)

    # Extract fields (with defaults)
    # Handle tools field (can be comma-separated string or list)
    tools_raw = frontmatter.get('tools', [])
    if isinstance(tools_raw, str):
        tools = [t.strip() for t in tools_raw.split(',')]
    else:
        tools = tools_raw if isinstance(tools_raw, list) else []

    # Handle skills field (can be formatted as list or string)
    skills_raw = frontmatter.get('skills', [])
    if isinstance(skills_raw, str):
        skills = [s.strip() for s in skills_raw.split(',')]
    elif isinstance(skills_raw, list):
        skills = skills_raw
    else:
        skills = []

    metadata = AgentMetadata(
        name=frontmatter.get('name', filepath.stem),
        description=frontmatter.get('description', ''),
        tools=tools,
        model=frontmatter.get('model', 'inherit'),
        color=frontmatter.get('color', 'blue'),
        skills=skills,
        created=frontmatter.get('created'),
        updated=frontmatter.get('updated'),
        body=body.strip()
    )

    return metadata


def calculate_permissions(tools: List[str]) -> Dict[str, str]:
    """
    Calculate OpenCode permission denylist from Claude Code tools whitelist.

    OpenCode uses denylist, so we deny tools NOT in whitelist.
    """
    # Normalize tools to lowercase
    allowed_tools = {TOOL_NAME_MAP.get(t, t.lower()) for t in tools}

    # All possible tools
    all_tools = {'read', 'write', 'edit', 'glob', 'grep', 'bash', 'webfetch', 'websearch', 'todowrite'}

    # Deny tools not in whitelist
    denied_tools = all_tools - allowed_tools

    # Convert to permission dict
    permissions = {tool: 'deny' for tool in denied_tools}

    # If empty, return empty dict (all allowed)
    return permissions if permissions else {}


def determine_mode(agent_name: str) -> str:
    """Determine agent mode (all vs subagent)."""
    return 'subagent' if agent_name in SUBAGENT_NAMES else 'all'


def is_checker_agent(agent_name: str) -> bool:
    """Check if agent is a checker."""
    return 'checker' in agent_name


def is_fixer_agent(agent_name: str) -> bool:
    """Check if agent is a fixer."""
    return 'fixer' in agent_name


def generate_workflow_section(agent_name: str, color: str) -> str:
    """Generate workflow integration section for checkers/fixers."""
    if is_checker_agent(agent_name):
        return """
## Workflow Integration (Maker-Checker-Fixer)

**Stage**: Checker (validates content)
**Before**: Maker creates content
**After**: User reviews → Fixer applies validated fixes

### Progressive Report Writing (MANDATORY)

1. **Initialize**: `generated-reports/{agent}__{uuid}__{YYYY-MM-DD--HH-MM}__audit.md`
2. **Write findings IMMEDIATELY** (not buffered)
3. **Update continuously** throughout execution
4. **Finalize** with statistics

### UUID Chain Generation

```bash
# Root UUID (6-char hex)
uuid=$(uuidgen | tr '[:upper:]' '[:lower:]' | head -c 6)

# Child UUID (if spawned by another agent)
# Format: {parent}.{new-uuid}
```

**Purpose**: Prevents parallel execution collisions

### Criticality Levels

- 🔴 **CRITICAL**: Breaks functionality, must fix before publication
- 🟠 **HIGH**: Significant quality degradation
- 🟡 **MEDIUM**: Minor issues, can defer
- 🟢 **LOW**: Suggestions, nice-to-have

**Execution Order**: CRITICAL → HIGH → MEDIUM → LOW
"""

    elif is_fixer_agent(agent_name):
        return """
## Confidence Assessment (Re-validation Required)

**Before Applying Any Fix**:

1. **Read audit report finding**
2. **Verify issue still exists** (file may have changed since audit)
3. **Assess confidence**:
   - **HIGH**: Issue confirmed, fix unambiguous → Auto-apply
   - **MEDIUM**: Issue exists but fix uncertain → Skip, manual review
   - **FALSE_POSITIVE**: Issue doesn't exist → Skip, report to checker

### Priority Matrix (Criticality × Confidence)

| Criticality | Confidence | Priority | Action |
|-------------|-----------|----------|--------|
| CRITICAL | HIGH | **P0** | Auto-fix immediately |
| HIGH | HIGH | **P1** | Auto-fix |
| CRITICAL | MEDIUM | **P1** | Urgent manual review |
| MEDIUM | HIGH | **P2** | Approved auto-fix |
| HIGH | MEDIUM | **P2** | Manual review |
| LOW | HIGH | **P3** | Suggestions |
| MEDIUM | MEDIUM | **P3** | Suggestions |
| LOW | MEDIUM | **P4** | Optional |

**Execution Order**: P0 → P1 → P2 → P3 → P4
"""

    return ""


def generate_skills_section(skills: List[str]) -> str:
    """Generate skills reference section."""
    if not skills:
        return ""

    lines = ["\n## Knowledge Dependencies (Skills)\n"]
    lines.append("This agent leverages Skills from `.claude/skills/`:\n")

    for i, skill in enumerate(skills, 1):
        lines.append(f"{i}. **`{skill}`** - Progressive knowledge delivery")

    lines.append("\n**Execution**: Reference these Skills for detailed guidance.\n")

    return "\n".join(lines)


def generate_tool_usage_section(tools: List[str]) -> str:
    """Generate tool usage documentation."""
    if not tools:
        return ""

    # Normalize to lowercase
    normalized = [TOOL_NAME_MAP.get(t, t.lower()) for t in tools]

    lines = ["\n## Tool Usage\n"]
    lines.append(f"**Required Tools**: {', '.join(normalized)}\n")

    # Tool descriptions
    tool_desc = {
        'read': 'Load files for analysis',
        'write': 'Generate reports (checkers) or create content (makers)',
        'edit': 'Modify existing files',
        'glob': 'Discover files matching patterns',
        'grep': 'Search content across files',
        'bash': 'Execute git, timestamps, file operations',
        'webfetch': 'Fetch web content for verification',
        'websearch': 'Search web for factual validation',
        'todowrite': 'Track task progress'
    }

    for tool in normalized:
        if tool in tool_desc:
            lines.append(f"- **{tool}**: {tool_desc[tool]}")

    return "\n".join(lines) + "\n"


def generate_metadata_section(color: str, created: Optional[str], updated: Optional[str]) -> str:
    """Generate agent metadata section."""
    lines = ["\n## Agent Metadata\n"]

    # Role from color
    role = COLOR_ROLE_MAP.get(color, 'Unknown')
    lines.append(f"- **Role**: {role} ({color})")

    # Timestamps
    if created:
        lines.append(f"- **Created**: {created}")
    if updated:
        lines.append(f"- **Last Updated**: {updated}")

    return "\n".join(lines) + "\n"


def generate_opencode_agent(metadata: AgentMetadata) -> str:
    """Generate OpenCode agent content from Claude Code metadata."""

    # Build frontmatter
    frontmatter = {
        'description': metadata.description
    }

    # Add mode
    mode = determine_mode(metadata.name)
    frontmatter['mode'] = mode

    # Add model (if not inherit)
    mapped_model = MODEL_MAP.get(metadata.model)
    if mapped_model:
        frontmatter['model'] = mapped_model

    # Add temperature for checkers (deterministic)
    if is_checker_agent(metadata.name):
        frontmatter['temperature'] = 0.1

    # Add maxSteps for complex agents
    if is_checker_agent(metadata.name) or metadata.name in ['plan-executor', 'repo-governance-checker']:
        frontmatter['maxSteps'] = 50

    # Add tools (boolean object)
    tools_obj = {}
    for tool in metadata.tools:
        normalized = TOOL_NAME_MAP.get(tool, tool.lower())
        tools_obj[normalized] = True
    frontmatter['tools'] = tools_obj

    # Add permissions (denylist)
    permissions = calculate_permissions(metadata.tools)

    # Add skill permissions (OpenCode uses permission.skill for access control)
    if metadata.skills:
        skill_permissions = {skill: 'allow' for skill in metadata.skills}
        if permissions:
            permissions['skill'] = skill_permissions
        else:
            permissions = {'skill': skill_permissions}

    if permissions:
        frontmatter['permission'] = permissions

    # Build augmented body
    body_parts = []

    # Add metadata section
    body_parts.append(generate_metadata_section(metadata.color, metadata.created, metadata.updated))

    # Add workflow section (checkers/fixers)
    workflow_section = generate_workflow_section(metadata.name, metadata.color)
    if workflow_section:
        body_parts.append(workflow_section)

    # Add skills section
    skills_section = generate_skills_section(metadata.skills)
    if skills_section:
        body_parts.append(skills_section)

    # Add tool usage section
    tool_section = generate_tool_usage_section(metadata.tools)
    if tool_section:
        body_parts.append(tool_section)

    # Add original body
    body_parts.append(metadata.body)

    # Combine
    augmented_body = "\n".join(body_parts)

    # Generate YAML frontmatter
    frontmatter_yaml = yaml.dump(frontmatter, default_flow_style=False, sort_keys=False, allow_unicode=True)

    # Build final content
    return f"---\n{frontmatter_yaml}---\n\n{augmented_body}"


def convert_agent(source_path: Path, dest_path: Path, dry_run: bool = False):
    """Convert single agent from Claude Code to OpenCode format."""
    print(f"Converting {source_path.name}...")

    # Parse Claude Code agent
    metadata = parse_claude_agent(source_path)

    # Generate OpenCode agent
    opencode_content = generate_opencode_agent(metadata)

    if dry_run:
        print(f"  [DRY RUN] Would write to {dest_path}")
        print(f"  Frontmatter: description={len(metadata.description)} chars, "
              f"tools={len(metadata.tools)}, skills={len(metadata.skills)}")
    else:
        # Ensure destination directory exists
        dest_path.parent.mkdir(parents=True, exist_ok=True)

        # Write OpenCode agent
        with open(dest_path, 'w', encoding='utf-8') as f:
            f.write(opencode_content)

        print(f"  ✓ Written to {dest_path}")


def convert_all_agents(source_dir: Path, dest_dir: Path, dry_run: bool = False, agent_name: Optional[str] = None):
    """Convert all agents or specific agent."""

    if agent_name:
        # Convert single agent
        source_path = source_dir / f"{agent_name}.md"
        dest_path = dest_dir / f"{agent_name}.md"

        if not source_path.exists():
            print(f"Error: Agent '{agent_name}' not found in {source_dir}")
            return

        convert_agent(source_path, dest_path, dry_run)
    else:
        # Convert all agents
        agent_files = sorted(source_dir.glob('*.md'))
        agent_files = [f for f in agent_files if f.name != 'README.md']  # Skip README

        print(f"Found {len(agent_files)} agents to convert\n")

        for source_path in agent_files:
            dest_path = dest_dir / source_path.name
            try:
                convert_agent(source_path, dest_path, dry_run)
            except Exception as e:
                print(f"  ✗ Error: {e}")

        print(f"\n{'[DRY RUN] ' if dry_run else ''}Conversion complete: {len(agent_files)} agents")


def main():
    parser = argparse.ArgumentParser(
        description='Convert Claude Code agents to OpenCode format'
    )
    parser.add_argument(
        '--agent',
        help='Convert specific agent by name (e.g., docs-maker)'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without writing files'
    )
    parser.add_argument(
        '--source',
        default='.claude/agents',
        help='Source directory (default: .claude/agents)'
    )
    parser.add_argument(
        '--dest',
        default='.opencode/agent',
        help='Destination directory (default: .opencode/agent)'
    )

    args = parser.parse_args()

    # Resolve paths
    source_dir = Path(args.source)
    dest_dir = Path(args.dest)

    if not source_dir.exists():
        print(f"Error: Source directory '{source_dir}' does not exist")
        return

    # Run conversion
    convert_all_agents(source_dir, dest_dir, args.dry_run, args.agent)


if __name__ == '__main__':
    main()
