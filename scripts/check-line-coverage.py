#!/usr/bin/env python3
"""check-line-coverage.py - Compute line coverage from Go cover.out using Codecov's algorithm.

Usage: python3 check-line-coverage.py <cover.out> <threshold_percent>

Run from the Go module directory (the one containing go.mod and cover.out).

Implements Codecov's line coverage algorithm:
- Expands each block [startLine, endLine] to individual source lines
- Skips blank lines, comment-only lines, and brace-only lines
- A line is COVERED if all blocks covering it have count > 0
- A line is PARTIAL if some blocks have count > 0 and some have count = 0
- A line is MISSED if all blocks covering it have count = 0
- Coverage % = covered / (covered + partial + missed)
- Partial lines count as NOT covered (matching Codecov's badge calculation)
"""

import re
import sys
from collections import defaultdict


def get_module_name():
    """Read go.mod in current directory to get module path."""
    try:
        with open("go.mod") as f:
            for line in f:
                m = re.match(r"^module\s+(\S+)", line)
                if m:
                    return m.group(1)
    except FileNotFoundError:
        pass
    return None


def get_source_lines(rel_path):
    """Return dict of line_no -> content for a source file relative to cwd."""
    try:
        with open(rel_path) as f:
            return {i + 1: line.rstrip() for i, line in enumerate(f)}
    except (FileNotFoundError, OSError):
        return {}


def is_code_line(content):
    """Return True if the line contains actual executable code."""
    s = content.strip()
    if not s:
        return False  # blank line
    if s.startswith("//"):
        return False  # comment-only line
    if s in ("{", "}", "(", ")"):
        return False  # brace-only line
    return True


def parse_cover_out(filename):
    """Parse Go cover.out, return list of (filepath, start_line, end_line, count)."""
    blocks = []
    with open(filename) as f:
        for line in f:
            line = line.strip()
            if line.startswith("mode:") or not line:
                continue
            m = re.match(r"^(.+):(\d+)\.\d+,(\d+)\.\d+ \d+ (\d+)$", line)
            if m:
                blocks.append(
                    (m.group(1), int(m.group(2)), int(m.group(3)), int(m.group(4)))
                )
    return blocks


def compute_line_coverage(blocks, module_name):
    """Compute Codecov-equivalent line coverage.

    Returns (pct, covered, partial, missed, total).
    """
    # Group blocks by file
    file_blocks = defaultdict(list)
    for filepath, sl, el, count in blocks:
        file_blocks[filepath].append((sl, el, count))

    covered = partial = missed = 0

    for filepath, fblocks in file_blocks.items():
        # Resolve module path to relative file path
        if module_name and filepath.startswith(module_name + "/"):
            rel_path = filepath[len(module_name) + 1 :]
        else:
            rel_path = filepath

        source = get_source_lines(rel_path)

        # For each line in each block's range, collect all block counts
        line_counts = defaultdict(list)
        for sl, el, count in fblocks:
            for line_no in range(sl, el + 1):
                line_counts[line_no].append(count)

        for line_no, counts in line_counts.items():
            # Skip non-code lines using actual source when available
            if source:
                if line_no not in source or not is_code_line(source[line_no]):
                    continue

            has_covered = any(c > 0 for c in counts)
            has_missed = any(c == 0 for c in counts)

            if has_covered and not has_missed:
                covered += 1
            elif has_covered and has_missed:
                partial += 1
            else:
                missed += 1

    total = covered + partial + missed
    pct = 100.0 * covered / total if total > 0 else 100.0
    return pct, covered, partial, missed, total


def main():
    if len(sys.argv) != 3:
        print(
            f"Usage: {sys.argv[0]} <cover.out> <threshold_percent>",
            file=sys.stderr,
        )
        sys.exit(1)

    cover_file = sys.argv[1]
    threshold = float(sys.argv[2])

    try:
        blocks = parse_cover_out(cover_file)
    except FileNotFoundError:
        print(f"Error: file not found: {cover_file}", file=sys.stderr)
        sys.exit(1)

    if not blocks:
        print("No coverage blocks found — nothing to measure.")
        print("PASS: 100.0% (no tracked lines)")
        return

    module_name = get_module_name()
    pct, covered, partial, missed, total = compute_line_coverage(blocks, module_name)

    print(
        f"Line coverage: {pct:.1f}% "
        f"({covered} covered, {partial} partial, {missed} missed, {total} total)"
    )

    if pct < threshold:
        print(f"FAIL: {pct:.1f}% < {threshold:.0f}% threshold", file=sys.stderr)
        sys.exit(1)
    else:
        print(f"PASS: {pct:.1f}% >= {threshold:.0f}% threshold")


if __name__ == "__main__":
    main()
