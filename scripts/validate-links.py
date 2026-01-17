#!/usr/bin/env python3
"""
Link validation script for markdown files.

Scans markdown files for broken links and generates a categorized report.
"""

import re
import sys
from pathlib import Path
from typing import Dict, List, Tuple
from collections import defaultdict
import argparse


class LinkValidator:
    """Validates markdown links in the repository."""

    def __init__(self, repo_root: Path, staged_only: bool = False):
        self.repo_root = repo_root
        self.staged_only = staged_only
        self.broken_links: Dict[str, List[Tuple[int, str, str]]] = defaultdict(list)

    def get_markdown_files(self) -> List[Path]:
        """Get all markdown files to validate."""
        if self.staged_only:
            # Get staged markdown files from git
            import subprocess
            result = subprocess.run(
                ['git', 'diff', '--cached', '--name-only', '--diff-filter=ACM'],
                capture_output=True,
                text=True,
                cwd=self.repo_root
            )
            files = [
                self.repo_root / f
                for f in result.stdout.strip().split('\n')
                if f.endswith('.md') and f
            ]
            return files
        else:
            # Get all markdown files in core directories
            patterns = [
                'governance/**/*.md',
                'docs/**/*.md',
                '.claude/**/*.md',
                '*.md'
            ]
            files = []
            for pattern in patterns:
                files.extend(self.repo_root.glob(pattern))
            return files

    def should_skip_link(self, link: str) -> bool:
        """Determine if a link should be skipped (placeholder/example/Hugo path)."""
        # Skip Hugo absolute paths (these are valid in Hugo sites)
        if link.startswith('/'):
            return True

        # Skip Hugo shortcodes
        if '{{<' in link or '{{%' in link:
            return True

        # Skip obvious placeholder patterns
        placeholders = [
            'path.md', 'target', 'link',
            './path/to/', '../path/to/',
            'path/to/convention.md', 'path/to/practice.md',
            'path/to/rule.md', './relative/path/to/',
        ]
        if any(placeholder in link for placeholder in placeholders):
            return True

        # Skip links with template placeholders in square brackets
        if re.search(r'\[[\w-]+\]', link):
            return True

        # Skip links that are just "path"
        if link in ['path', 'target', 'link']:
            return True

        # Skip example image paths
        if '/images/' in link and not link.startswith('../'):
            return True

        # Skip example tutorial file names (clearly examples)
        example_patterns = [
            'tu__rag-', 'ex-co__', 'ex__', 'tu-bufi__',
            './tu__', './re__', './hoto__', './ex__',
            './overview', './guide.md', './examples.md', './reference.md',
            './diagram.png', './image.png', './screenshots/',
            './auth-guide.md', 'by-concept/beginner', './by-example/beginner',
            'swe/prog-lang/', '../parent', './ai/', '../swe/', '../../advanced/',
            'url', './LICENSE', '../../features.md',
            './tutorials/tu__', # Example tutorial paths
            '../../.opencode/', # OpenCode references (not part of this repo)
        ]
        if any(pattern in link for pattern in example_patterns):
            return True

        # Skip links in .claude/skills/ directory (often contain examples)
        # We'll check this in the validate_file method instead

        return False

    def extract_links(self, file_path: Path) -> List[Tuple[int, str]]:
        """Extract markdown links from a file with line numbers."""
        links = []
        try:
            content = file_path.read_text(encoding='utf-8')
            for line_num, line in enumerate(content.split('\n'), 1):
                # Match markdown links: [text](url)
                matches = re.finditer(r'\[([^\]]+)\]\(([^)]+)\)', line)
                for match in matches:
                    url = match.group(2)
                    # Strip angle brackets if present (markdown autolink syntax)
                    url = url.strip('<>')
                    # Skip external URLs, anchors, and mailto
                    if url.startswith(('http://', 'https://', '#', 'mailto:')):
                        continue
                    # Skip placeholder/example/Hugo paths
                    if self.should_skip_link(url):
                        continue
                    links.append((line_num, url))
        except Exception as e:
            print(f"Error reading {file_path}: {e}", file=sys.stderr)
        return links

    def resolve_link(self, source_file: Path, link: str) -> Path:
        """Resolve a relative link to an absolute path."""
        # Remove anchor if present
        link_without_anchor = link.split('#')[0]
        if not link_without_anchor:
            # Pure anchor link (internal to same file)
            return source_file

        # Resolve relative to source file's directory
        source_dir = source_file.parent
        target_path = (source_dir / link_without_anchor).resolve()
        return target_path

    def categorize_broken_link(self, link: str) -> str:
        """Categorize a broken link by pattern."""
        if 'ex-ru-' in link or 'ex__ru__' in link:
            return 'Old ex-ru-* prefixes'
        elif 'workflows/' in link and 'governance/workflows/' not in link:
            return 'workflows/ paths'
        elif 'vision/' in link and 'governance/vision/' not in link:
            return 'vision/ paths'
        elif 'conventions/README.md' in link:
            return 'conventions README'
        elif link in ['CODE_OF_CONDUCT.md', 'CHANGELOG.md']:
            return 'Missing files'
        else:
            return 'General/other paths'

    def validate_file(self, file_path: Path) -> None:
        """Validate all links in a file."""
        # Skip validation for skill files as they contain many examples
        if '.claude/skills/' in str(file_path):
            return

        links = self.extract_links(file_path)

        for line_num, link in links:
            target = self.resolve_link(file_path, link)

            if not target.exists():
                category = self.categorize_broken_link(link)
                relative_path = file_path.relative_to(self.repo_root)
                self.broken_links[category].append((line_num, str(relative_path), link))

    def validate_all(self) -> int:
        """Validate all markdown files and return count of broken links."""
        files = self.get_markdown_files()

        for file_path in files:
            self.validate_file(file_path)

        return sum(len(links) for links in self.broken_links.values())

    def generate_report(self) -> str:
        """Generate a categorized report of broken links."""
        if not self.broken_links:
            return "✓ All links valid! No broken links found."

        report_lines = ["# Broken Links Report\n"]

        total = sum(len(links) for links in self.broken_links.values())
        report_lines.append(f"**Total broken links**: {total}\n")

        # Category order for report
        category_order = [
            'Old ex-ru-* prefixes',
            'Missing files',
            'General/other paths',
            'workflows/ paths',
            'vision/ paths',
            'conventions README'
        ]

        for category in category_order:
            if category not in self.broken_links:
                continue

            links = self.broken_links[category]
            report_lines.append(f"\n## {category} ({len(links)} links)\n")

            # Group by file
            by_file = defaultdict(list)
            for line_num, file_path, link in links:
                by_file[file_path].append((line_num, link))

            for file_path in sorted(by_file.keys()):
                report_lines.append(f"\n### {file_path}\n")
                for line_num, link in sorted(by_file[file_path]):
                    report_lines.append(f"- Line {line_num}: `{link}`\n")

        return ''.join(report_lines)


def main():
    parser = argparse.ArgumentParser(description='Validate markdown links')
    parser.add_argument('--staged-only', action='store_true',
                        help='Only validate staged files')
    parser.add_argument('--report', action='store_true',
                        help='Generate full report instead of just error count')
    args = parser.parse_args()

    repo_root = Path(__file__).parent.parent
    validator = LinkValidator(repo_root, staged_only=args.staged_only)

    broken_count = validator.validate_all()

    if args.report or broken_count > 0:
        report = validator.generate_report()
        print(report)

    if broken_count > 0:
        print(f"\n❌ Found {broken_count} broken links", file=sys.stderr)
        sys.exit(1)
    else:
        if not args.report:
            print("✓ All links valid!")
        sys.exit(0)


if __name__ == '__main__':
    main()
