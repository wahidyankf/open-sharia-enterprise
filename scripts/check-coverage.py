#!/usr/bin/env python3
"""check-coverage.py - Compute line coverage using Codecov's algorithm.

Usage: python3 check-coverage.py <coverage-file> <threshold_percent>

Auto-detects format from the coverage file:
  - Go cover.out format (*.out): parses block ranges, filters non-code lines
  - LCOV format (*.info): parses DA/BRDA entries, includes BRDA-only lines

Both formats implement Codecov's line coverage algorithm:
  - A line is COVERED if hit count > 0 AND all branches taken (or no branches)
  - A line is PARTIAL if hit count > 0 but some branches not taken
  - A line is MISSED if hit count = 0
  - Coverage % = covered / (covered + partial + missed)
  - Partial lines count as NOT covered (matching Codecov's badge calculation)

Go-specific filtering (matching Codecov's file fixes for Go):
  - Blank lines are excluded
  - Comment-only lines (//) are excluded
  - Brace-only lines ({ or }) are excluded — note: ( and ) are NOT excluded
"""

import re
import sys
from collections import defaultdict


# ---------------------------------------------------------------------------
# Format detection
# ---------------------------------------------------------------------------


def detect_format(filename):
    """Return 'go' or 'lcov' based on file name and content."""
    if filename.endswith(".info") or "lcov" in filename.lower():
        return "lcov"
    # Try to detect from content
    try:
        with open(filename) as f:
            first = f.readline().strip()
        if first.startswith("mode:"):
            return "go"
        if first.startswith("SF:") or first.startswith("TN:"):
            return "lcov"
    except (FileNotFoundError, OSError):
        pass
    return "go"  # default


# ---------------------------------------------------------------------------
# Go cover.out support
# ---------------------------------------------------------------------------


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


def is_go_code_line(content):
    """Return True if the line contains actual executable Go code.

    Matches Codecov's file fixes for Go:
    - Blank lines → excluded
    - Comment-only lines (//) → excluded
    - Brace-only lines ({ or }) → excluded
    Note: ( and ) are NOT excluded (Codecov only filters { and })
    """
    s = content.strip()
    if not s:
        return False  # blank line
    if s.startswith("//"):
        return False  # comment-only line
    if s in ("{", "}"):
        return False  # brace-only line
    return True


def parse_cover_out(filename):
    """Parse Go cover.out, return list of (filepath, start_line, end_line, count)."""
    blocks = []
    try:
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
    except FileNotFoundError:
        print(f"Error: file not found: {filename}", file=sys.stderr)
        sys.exit(1)
    return blocks


def compute_go_coverage(blocks):
    """Compute Codecov-equivalent line coverage from Go cover.out blocks.

    Returns (pct, covered, partial, missed, total).
    """
    module_name = get_module_name()

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
                if line_no not in source or not is_go_code_line(source[line_no]):
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


# ---------------------------------------------------------------------------
# LCOV support
# ---------------------------------------------------------------------------


def parse_lcov(filename):
    """Parse LCOV file, return list of per-file (da_lines, brda_data) tuples."""
    files = []
    current_da = {}
    current_brda = defaultdict(list)

    try:
        with open(filename) as f:
            content = f.read()
    except FileNotFoundError:
        print(f"Error: file not found: {filename}", file=sys.stderr)
        sys.exit(1)

    for line in content.split("\n"):
        line = line.strip()
        if line.startswith("DA:"):
            parts = line[3:].split(",")
            if len(parts) >= 2:
                try:
                    ln, cnt = int(parts[0]), int(parts[1])
                    # If a line appears multiple times, take the max count
                    current_da[ln] = max(current_da.get(ln, 0), cnt)
                except ValueError:
                    pass
        elif line.startswith("BRDA:"):
            parts = line[5:].split(",")
            if len(parts) >= 4:
                try:
                    ln = int(parts[0])
                    cnt_str = parts[3]
                    cnt = 0 if cnt_str in ("-", "") else int(cnt_str)
                    current_brda[ln].append(cnt)
                except ValueError:
                    pass
        elif line.startswith("end_of_record"):
            files.append((dict(current_da), dict(current_brda)))
            current_da = {}
            current_brda = defaultdict(list)

    return files


def compute_lcov_coverage(files):
    """Compute Codecov-equivalent line coverage from LCOV data.

    Includes BRDA-only lines (lines in branch data but not DA entries) —
    Codecov counts these as covered/partial/missed based on branch hit counts.

    Returns (pct, covered, partial, missed, total).
    """
    covered = partial = missed = 0

    for da_lines, brda_data in files:
        # Classify DA lines
        for line_no, count in da_lines.items():
            branches = brda_data.get(line_no, [])
            if count > 0:
                if branches and not all(b > 0 for b in branches):
                    partial += 1
                else:
                    covered += 1
            else:
                missed += 1

        # Classify BRDA-only lines (not in DA but in BRDA)
        for line_no, branch_counts in brda_data.items():
            if line_no not in da_lines:
                if all(b > 0 for b in branch_counts):
                    covered += 1
                elif any(b > 0 for b in branch_counts):
                    partial += 1
                else:
                    missed += 1

    total = covered + partial + missed
    pct = 100.0 * covered / total if total > 0 else 100.0
    return pct, covered, partial, missed, total


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main():
    if len(sys.argv) != 3:
        print(
            f"Usage: {sys.argv[0]} <coverage-file> <threshold_percent>",
            file=sys.stderr,
        )
        sys.exit(1)

    coverage_file = sys.argv[1]
    threshold = float(sys.argv[2])

    fmt = detect_format(coverage_file)

    if fmt == "lcov":
        files = parse_lcov(coverage_file)
        if not files:
            print("No coverage data found — nothing to measure.")
            print("PASS: 100.00% (no tracked lines)")
            return
        pct, covered, partial, missed, total = compute_lcov_coverage(files)
    else:
        blocks = parse_cover_out(coverage_file)
        if not blocks:
            print("No coverage blocks found — nothing to measure.")
            print("PASS: 100.0% (no tracked lines)")
            return
        pct, covered, partial, missed, total = compute_go_coverage(blocks)

    print(
        f"Line coverage: {pct:.2f}% "
        f"({covered} covered, {partial} partial, {missed} missed, {total} total)"
    )

    if pct < threshold:
        print(f"FAIL: {pct:.2f}% < {threshold:.0f}% threshold", file=sys.stderr)
        sys.exit(1)
    else:
        print(f"PASS: {pct:.2f}% >= {threshold:.0f}% threshold")


if __name__ == "__main__":
    main()
