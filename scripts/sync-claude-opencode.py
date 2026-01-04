#!/usr/bin/env python3
"""
Sync Claude Code and OpenCode agent formats.

This script:
1. Detects which file changed (modification time comparison)
2. Claude Code newer → apply conversion → update OpenCode
3. OpenCode newer → prompt user (manual backport required)
4. Generates sync report

Usage:
    python scripts/sync-claude-opencode.py
    python scripts/sync-claude-opencode.py --agent docs-maker
    python scripts/sync-claude-opencode.py --force-direction claude-to-opencode
    python scripts/sync-claude-opencode.py --dry-run
"""

import os
import sys
import argparse
from pathlib import Path
from datetime import datetime
from typing import List, Tuple, Optional
from dataclasses import dataclass
import subprocess

@dataclass
class SyncStatus:
    """Sync status for a single agent."""
    agent_name: str
    claude_exists: bool
    opencode_exists: bool
    claude_mtime: Optional[float]
    opencode_mtime: Optional[float]
    direction: str  # 'claude_newer', 'opencode_newer', 'same', 'missing'
    action: str  # 'sync', 'skip', 'error', 'prompt'


def get_file_mtime(filepath: Path) -> Optional[float]:
    """Get file modification time, return None if doesn't exist."""
    if filepath.exists():
        return filepath.stat().st_mtime
    return None


def compare_agents(claude_dir: Path, opencode_dir: Path, agent_name: str) -> SyncStatus:
    """Compare single agent between Claude Code and OpenCode."""
    claude_path = claude_dir / f"{agent_name}.md"
    opencode_path = opencode_dir / f"{agent_name}.md"

    claude_exists = claude_path.exists()
    opencode_exists = opencode_path.exists()

    claude_mtime = get_file_mtime(claude_path)
    opencode_mtime = get_file_mtime(opencode_path)

    # Determine direction
    if not claude_exists and not opencode_exists:
        direction = 'missing'
        action = 'error'
    elif not claude_exists:
        direction = 'opencode_only'
        action = 'prompt'  # User needs to backport manually
    elif not opencode_exists:
        direction = 'claude_only'
        action = 'sync'  # Create OpenCode version
    elif claude_mtime > opencode_mtime:
        direction = 'claude_newer'
        action = 'sync'  # Claude Code → OpenCode
    elif opencode_mtime > claude_mtime:
        direction = 'opencode_newer'
        action = 'prompt'  # User needs to backport manually
    else:
        direction = 'same'
        action = 'skip'  # No sync needed

    return SyncStatus(
        agent_name=agent_name,
        claude_exists=claude_exists,
        opencode_exists=opencode_exists,
        claude_mtime=claude_mtime,
        opencode_mtime=opencode_mtime,
        direction=direction,
        action=action
    )


def sync_agent(status: SyncStatus, claude_dir: Path, opencode_dir: Path, dry_run: bool = False):
    """Sync single agent (Claude Code → OpenCode)."""
    if status.action != 'sync':
        return

    claude_path = claude_dir / f"{status.agent_name}.md"
    opencode_path = opencode_dir / f"{status.agent_name}.md"

    if dry_run:
        print(f"  [DRY RUN] Would sync {status.agent_name}: Claude Code → OpenCode")
    else:
        # Run conversion script for this agent
        try:
            result = subprocess.run(
                [sys.executable, 'scripts/convert-agents-to-opencode.py', '--agent', status.agent_name],
                capture_output=True,
                text=True,
                check=True
            )
            print(f"  ✓ Synced {status.agent_name}: Claude Code → OpenCode")
        except subprocess.CalledProcessError as e:
            print(f"  ✗ Error syncing {status.agent_name}: {e.stderr}")


def prompt_manual_backport(status: SyncStatus):
    """Prompt user for manual backport (OpenCode → Claude Code)."""
    print(f"\n⚠️  Manual backport required: {status.agent_name}")
    print(f"    OpenCode agent is newer than Claude Code agent")
    print(f"    Options:")
    print(f"      1. Edit .claude/agents/{status.agent_name}.md manually")
    print(f"      2. Discard OpenCode changes (run sync with --force-direction claude-to-opencode)")
    print(f"      3. Accept OpenCode as source of truth (update mtime: touch .claude/agents/{status.agent_name}.md)")


def sync_all_agents(
    claude_dir: Path,
    opencode_dir: Path,
    dry_run: bool = False,
    agent_name: Optional[str] = None,
    force_direction: Optional[str] = None
):
    """Sync all agents or specific agent."""

    # Get agent list
    if agent_name:
        agent_names = [agent_name]
    else:
        # Get all agents from both directories
        claude_agents = {f.stem for f in claude_dir.glob('*.md') if f.name != 'README.md'}
        opencode_agents = {f.stem for f in opencode_dir.glob('*.md') if f.name != 'README.md'}
        agent_names = sorted(claude_agents | opencode_agents)

    print(f"Checking {len(agent_names)} agents for sync...\n")

    # Compare and collect statuses
    statuses = []
    for name in agent_names:
        status = compare_agents(claude_dir, opencode_dir, name)

        # Force direction if specified
        if force_direction == 'claude-to-opencode':
            if status.claude_exists:
                status.action = 'sync'
            else:
                status.action = 'error'
        elif force_direction == 'opencode-to-claude':
            status.action = 'prompt'  # Always prompt for backport

        statuses.append(status)

    # Categorize statuses
    to_sync = [s for s in statuses if s.action == 'sync']
    to_prompt = [s for s in statuses if s.action == 'prompt']
    to_skip = [s for s in statuses if s.action == 'skip']
    errors = [s for s in statuses if s.action == 'error']

    # Report
    print("=" * 70)
    print("SYNC ANALYSIS")
    print("=" * 70)
    print(f"Total agents:         {len(statuses)}")
    print(f"Claude → OpenCode:    {len(to_sync)}")
    print(f"OpenCode → Claude:    {len(to_prompt)} (manual backport needed)")
    print(f"Up to date:           {len(to_skip)}")
    print(f"Errors:               {len(errors)}")
    print("=" * 70)
    print()

    # Sync agents (Claude → OpenCode)
    if to_sync:
        print(f"Syncing {len(to_sync)} agents (Claude Code → OpenCode)...\n")
        for status in to_sync:
            sync_agent(status, claude_dir, opencode_dir, dry_run)
        print()

    # Prompt for manual backports
    if to_prompt:
        print(f"⚠️  {len(to_prompt)} agents require manual backport:\n")
        for status in to_prompt:
            prompt_manual_backport(status)
        print()

    # Report errors
    if errors:
        print(f"✗ {len(errors)} errors:\n")
        for status in errors:
            print(f"  {status.agent_name}: {status.direction}")
        print()

    # Summary
    print("=" * 70)
    print("SYNC COMPLETE")
    print("=" * 70)
    if dry_run:
        print("[DRY RUN] No files were actually modified")
    else:
        print(f"Synced: {len(to_sync)} agents")
        if to_prompt:
            print(f"Manual action needed: {len(to_prompt)} agents")


def main():
    parser = argparse.ArgumentParser(
        description='Sync Claude Code and OpenCode agent formats'
    )
    parser.add_argument(
        '--agent',
        help='Sync specific agent by name (e.g., docs-maker)'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without making changes'
    )
    parser.add_argument(
        '--force-direction',
        choices=['claude-to-opencode', 'opencode-to-claude'],
        help='Force sync direction (ignores modification times)'
    )
    parser.add_argument(
        '--claude-dir',
        default='.claude/agents',
        help='Claude Code agents directory (default: .claude/agents)'
    )
    parser.add_argument(
        '--opencode-dir',
        default='.opencode/agent',
        help='OpenCode agents directory (default: .opencode/agent)'
    )

    args = parser.parse_args()

    # Resolve paths
    claude_dir = Path(args.claude_dir)
    opencode_dir = Path(args.opencode_dir)

    if not claude_dir.exists():
        print(f"Error: Claude Code directory '{claude_dir}' does not exist")
        return

    if not opencode_dir.exists() and not args.dry_run:
        print(f"Warning: OpenCode directory '{opencode_dir}' does not exist")
        print("Run convert-agents-to-opencode.py first to create OpenCode agents")
        return

    # Run sync
    sync_all_agents(
        claude_dir,
        opencode_dir,
        args.dry_run,
        args.agent,
        args.force_direction
    )


if __name__ == '__main__':
    main()
