#!/usr/bin/env python3
"""
Validate OpenCode agents for correctness and compliance.

This script validates:
1. Required frontmatter fields present
2. Tool names lowercase (read, write vs Read, Write)
3. Permission references valid tools
4. Skills exist in .claude/skills/
5. No forbidden fields (color, skills in frontmatter)
6. Workflow instructions present (for checkers/fixers)

Usage:
    python scripts/validate-opencode-agents.py
    python scripts/validate-opencode-agents.py --agent docs-maker
    python scripts/validate-opencode-agents.py --strict
"""

import os
import re
import yaml
import argparse
from pathlib import Path
from typing import Dict, List, Set, Tuple
from dataclasses import dataclass, field

@dataclass
class ValidationResult:
    """Result of validation for a single agent."""
    agent_name: str
    passed: bool = True
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)


# Valid tool names (lowercase only)
VALID_TOOLS = {'read', 'write', 'edit', 'glob', 'grep', 'bash', 'webfetch', 'websearch', 'todowrite'}

# Forbidden frontmatter fields (should be in body)
FORBIDDEN_FIELDS = {'color', 'skills', 'name', 'created', 'updated'}

# Required frontmatter fields
REQUIRED_FIELDS = {'description'}

# Valid mode values
VALID_MODES = {'all', 'subagent', 'primary'}


def parse_opencode_agent(filepath: Path) -> Tuple[Dict, str]:
    """Parse OpenCode agent and return frontmatter and body."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    # Extract frontmatter
    match = re.match(r'^---\s*\n(.*?)\n---\s*\n(.*)$', content, re.DOTALL)
    if not match:
        raise ValueError("No frontmatter found")

    frontmatter_text = match.group(1)
    body = match.group(2).strip()

    # Parse YAML
    frontmatter = yaml.safe_load(frontmatter_text)

    return frontmatter, body


def validate_agent(filepath: Path, strict: bool = False) -> ValidationResult:
    """Validate single OpenCode agent."""
    agent_name = filepath.stem
    result = ValidationResult(agent_name=agent_name)

    try:
        frontmatter, body = parse_opencode_agent(filepath)
    except Exception as e:
        result.passed = False
        result.errors.append(f"Parse error: {e}")
        return result

    # 1. Required fields
    for field in REQUIRED_FIELDS:
        if field not in frontmatter:
            result.passed = False
            result.errors.append(f"Missing required field: {field}")

    # 2. Description not empty
    description = frontmatter.get('description', '')
    if not description or len(description.strip()) == 0:
        result.passed = False
        result.errors.append("Description field is empty")

    # 3. Forbidden fields (should be in body)
    for field in FORBIDDEN_FIELDS:
        if field in frontmatter:
            result.passed = False
            result.errors.append(f"Forbidden field in frontmatter: {field} (should be in body)")

    # 4. Mode validation
    mode = frontmatter.get('mode')
    if mode and mode not in VALID_MODES:
        result.passed = False
        result.errors.append(f"Invalid mode: {mode} (must be one of {VALID_MODES})")

    # 5. Tools validation
    tools = frontmatter.get('tools', {})
    if tools:
        if not isinstance(tools, dict):
            result.passed = False
            result.errors.append(f"Tools must be object (dict), got {type(tools).__name__}")
        else:
            for tool_name in tools.keys():
                # Check lowercase
                if tool_name != tool_name.lower():
                    result.passed = False
                    result.errors.append(f"Tool name must be lowercase: {tool_name}")

                # Check valid tool
                if tool_name.lower() not in VALID_TOOLS:
                    result.warnings.append(f"Unknown tool: {tool_name} (not in {VALID_TOOLS})")

    # 6. Permission validation
    permissions = frontmatter.get('permission', {})
    if permissions:
        if not isinstance(permissions, dict):
            result.passed = False
            result.errors.append(f"Permission must be object (dict), got {type(permissions).__name__}")
        else:
            for tool_name, action in permissions.items():
                # Check lowercase
                if tool_name != tool_name.lower():
                    result.passed = False
                    result.errors.append(f"Permission tool must be lowercase: {tool_name}")

                # Handle skill permissions (nested dict) vs regular permissions (string)
                if tool_name == 'skill':
                    # Skill permissions: permission.skill: {skill-name: allow}
                    if not isinstance(action, dict):
                        result.passed = False
                        result.errors.append(f"Skill permission must be a dict, got {type(action).__name__}")
                    else:
                        # Validate each skill permission
                        for skill_name, skill_action in action.items():
                            if skill_action not in {'allow', 'deny', 'ask'}:
                                result.passed = False
                                result.errors.append(f"Invalid skill permission action for {skill_name}: {skill_action} (must be allow/deny/ask)")
                else:
                    # Regular tool permissions
                    # Check valid tool
                    if tool_name.lower() not in VALID_TOOLS:
                        result.warnings.append(f"Unknown permission tool: {tool_name}")

                    # Check valid action
                    if action not in {'allow', 'deny', 'ask'}:
                        result.passed = False
                        result.errors.append(f"Invalid permission action for {tool_name}: {action} (must be allow/deny/ask)")

    # 7. Checker agents must have workflow instructions
    if 'checker' in agent_name.lower():
        if 'Workflow Integration' not in body:
            result.warnings.append("Checker agent missing 'Workflow Integration' section")
        if 'Progressive Report Writing' not in body:
            result.warnings.append("Checker agent missing 'Progressive Report Writing' instructions")
        if 'Criticality Levels' not in body:
            result.warnings.append("Checker agent missing 'Criticality Levels' documentation")

    # 8. Fixer agents must have confidence assessment
    if 'fixer' in agent_name.lower():
        if 'Confidence Assessment' not in body:
            result.warnings.append("Fixer agent missing 'Confidence Assessment' section")
        if 'Priority Matrix' not in body:
            result.warnings.append("Fixer agent missing 'Priority Matrix' documentation")

    # 9. Skills referenced should exist
    # Extract skill names from body (pattern: `skill-name`)
    skill_pattern = r'`([a-z0-9-]+(?:__[a-z0-9-]+)*)`'
    potential_skills = re.findall(skill_pattern, body)

    # Check if skills exist (if .claude/skills/ directory exists)
    skills_dir = Path('.claude/skills')
    if skills_dir.exists():
        for skill_ref in potential_skills:
            # Check if this looks like a skill (has hyphen, not a filename)
            if '-' in skill_ref and not skill_ref.endswith('.md'):
                skill_path = skills_dir / skill_ref / 'SKILL.md'
                if not skill_path.exists():
                    # Could be false positive, so warning not error
                    if strict:
                        result.warnings.append(f"Referenced skill may not exist: {skill_ref}")

    # 10. Metadata section present (converted from color/timestamps)
    if 'Agent Metadata' not in body:
        result.warnings.append("Missing 'Agent Metadata' section (should contain role, timestamps)")

    return result


def validate_all_agents(agent_dir: Path, strict: bool = False, agent_name: str = None) -> List[ValidationResult]:
    """Validate all agents or specific agent."""
    results = []

    if agent_name:
        # Validate single agent
        filepath = agent_dir / f"{agent_name}.md"
        if not filepath.exists():
            print(f"Error: Agent '{agent_name}' not found in {agent_dir}")
            return results

        result = validate_agent(filepath, strict)
        results.append(result)
    else:
        # Validate all agents
        agent_files = sorted(agent_dir.glob('*.md'))
        agent_files = [f for f in agent_files if f.name != 'README.md']

        print(f"Validating {len(agent_files)} agents...\n")

        for filepath in agent_files:
            result = validate_agent(filepath, strict)
            results.append(result)

    return results


def print_results(results: List[ValidationResult]):
    """Print validation results."""
    passed_count = sum(1 for r in results if r.passed)
    failed_count = len(results) - passed_count
    total_errors = sum(len(r.errors) for r in results)
    total_warnings = sum(len(r.warnings) for r in results)

    # Print individual results
    for result in results:
        status = "✓ PASS" if result.passed else "✗ FAIL"
        print(f"{status:8} {result.agent_name}")

        if result.errors:
            for error in result.errors:
                print(f"         ERROR: {error}")

        if result.warnings:
            for warning in result.warnings:
                print(f"         WARN:  {warning}")

        if result.errors or result.warnings:
            print()

    # Print summary
    print("=" * 70)
    print(f"VALIDATION SUMMARY")
    print("=" * 70)
    print(f"Total agents:   {len(results)}")
    print(f"Passed:         {passed_count} ✓")
    print(f"Failed:         {failed_count} {'✗' if failed_count > 0 else ''}")
    print(f"Total errors:   {total_errors}")
    print(f"Total warnings: {total_warnings}")
    print("=" * 70)

    if failed_count == 0:
        print("✓ All agents passed validation!")
    else:
        print(f"✗ {failed_count} agent(s) failed validation")


def main():
    parser = argparse.ArgumentParser(
        description='Validate OpenCode agents for correctness'
    )
    parser.add_argument(
        '--agent',
        help='Validate specific agent by name (e.g., docs-maker)'
    )
    parser.add_argument(
        '--strict',
        action='store_true',
        help='Enable strict validation (more warnings)'
    )
    parser.add_argument(
        '--dir',
        default='.opencode/agent',
        help='Agent directory to validate (default: .opencode/agent)'
    )

    args = parser.parse_args()

    # Resolve path
    agent_dir = Path(args.dir)

    if not agent_dir.exists():
        print(f"Error: Agent directory '{agent_dir}' does not exist")
        print("Have you run convert-agents-to-opencode.py first?")
        return

    # Run validation
    results = validate_all_agents(agent_dir, args.strict, args.agent)

    # Print results
    print_results(results)

    # Exit code
    failed_count = sum(1 for r in results if not r.passed)
    exit(1 if failed_count > 0 else 0)


if __name__ == '__main__':
    main()
