---
title: "Overview"
weight: 10000000
date: 2026-04-01T00:00:00+07:00
draft: false
description: "Overview of the sed by-example series: structure, approach, and what to expect from each level"
tags: ["sed", "stream-editor", "text-processing", "tutorial", "by-example", "code-first"]
---

This series teaches sed through heavily annotated, self-contained shell examples. Each example
focuses on a single concept and includes inline annotations explaining what each command does,
why it matters, and what output or transformation results from it. All examples use `echo`
piped to `sed` or heredoc input so you can run them directly in any Unix-like shell without
creating files first.

## Series Structure

The examples are organized into three levels based on complexity:

- [Beginner](/en/learn/software-engineering/automation-tools/sed/by-example/beginner) —
  Basic substitution, print and delete commands, line addressing, range addressing, in-place
  editing, and single-character commands
- [Intermediate](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate) —
  Capture groups, extended regex, hold space, multiline processing, branching and labels,
  and sed script files
- [Advanced](/en/learn/software-engineering/automation-tools/sed/by-example/advanced) —
  GNU vs BSD portability, complex multiline transforms, config file manipulation, log processing,
  pipeline integration, and real-world automation scripts

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the sed command does and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of data flow or command execution (when appropriate)
3. **Heavily Annotated Code** — shell commands with `# =>` comments describing each flag, address,
   command, and its effect on the pattern space
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## How to Use This Series

Each page presents annotated shell one-liners and short scripts. Read the annotations alongside
the command to understand both the mechanics and the intent. The examples build on each other
within each level, so reading sequentially gives the fullest understanding. Every example is
self-contained — you can copy any single example and run it immediately without setup.

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: Minimal Substitution](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-1-minimal-substitution)
- [Example 2: Global Flag `g`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-2-global-flag-g)
- [Example 3: Case-Insensitive Flag `I`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-3-case-insensitive-flag-i)
- [Example 4: Printing with `p` and `-n`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-4-printing-with-p-and--n)
- [Example 5: Deleting Lines with `d`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-5-deleting-lines-with-d)
- [Example 6: Addressing by Line Number](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-6-addressing-by-line-number)
- [Example 7: The `$` Last-Line Address](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-7-the--last-line-address)
- [Example 8: Range Addressing with Line Numbers](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-8-range-addressing-with-line-numbers)
- [Example 9: Regex Addressing](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-9-regex-addressing)
- [Example 10: Regex Range Addressing](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-10-regex-range-addressing)
- [Example 11: First Occurrence Only (No `g` Flag)](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-11-first-occurrence-only-no-g-flag)
- [Example 12: Nth Occurrence Flag](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-12-nth-occurrence-flag)
- [Example 13: In-Place Editing with `-i`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-13-in-place-editing-with--i)
- [Example 14: In-Place with Backup (`-i.bak`)](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-14-in-place-with-backup--ibak)
- [Example 15: Multiple Expressions with `-e`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-15-multiple-expressions-with--e)
- [Example 16: Commands from a Script File with `-f`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-16-commands-from-a-script-file-with--f)
- [Example 17: Append with `a`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-17-append-with-a)
- [Example 18: Insert with `i`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-18-insert-with-i)
- [Example 19: Change with `c`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-19-change-with-c)
- [Example 20: Transliterate with `y`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-20-transliterate-with-y)
- [Example 21: Quit with `q`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-21-quit-with-q)
- [Example 22: Read from File with `r`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-22-read-from-file-with-r)
- [Example 23: Write to File with `w`](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-23-write-to-file-with-w)
- [Example 24: Suppress Output and Print Selectively](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-24-suppress-output-and-print-selectively)
- [Example 25: Combining Address and Substitution](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-25-combining-address-and-substitution)
- [Example 26: Deleting Blank Lines](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-26-deleting-blank-lines)
- [Example 27: Stripping Leading Whitespace](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-27-stripping-leading-whitespace)
- [Example 28: Stripping Trailing Whitespace](/en/learn/software-engineering/automation-tools/sed/by-example/beginner#example-28-stripping-trailing-whitespace)

### Intermediate (Examples 29–56)

- [Example 29: Capture Groups and `\1` Backreferences](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-29-capture-groups-and-1-backreferences)
- [Example 30: Reformatting Dates with Backreferences](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-30-reformatting-dates-with-backreferences)
- [Example 31: Extended Regex with `-E`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-31-extended-regex-with--e)
- [Example 32: Alternation with `|`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-32-alternation-with-)
- [Example 33: POSIX Character Classes](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-33-posix-character-classes)
- [Example 34: Negated Address with `!`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-34-negated-address-with-)
- [Example 35: Negating a Line Range](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-35-negating-a-line-range)
- [Example 36: Save to Hold Space with `h` and Retrieve with `g`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-36-save-to-hold-space-with-h-and-retrieve-with-g)
- [Example 37: Accumulate with `H` and `G`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-37-accumulate-with-h-and-g)
- [Example 38: Exchange Pattern and Hold Space with `x`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-38-exchange-pattern-and-hold-space-with-x)
- [Example 39: Append Next Line with `N`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-39-append-next-line-with-n)
- [Example 40: Print First Line of Multiline Pattern Space with `P`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-40-print-first-line-of-multiline-pattern-space-with-p)
- [Example 41: Delete First Line of Multiline Pattern Space with `D`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-41-delete-first-line-of-multiline-pattern-space-with-d)
- [Example 42: Unconditional Branch with `b`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-42-unconditional-branch-with-b)
- [Example 43: Labels and Conditional Branch with `t`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-43-labels-and-conditional-branch-with-t)
- [Example 44: Negated Conditional Branch with `T`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-44-negated-conditional-branch-with-t)
- [Example 45: Step Addressing with `first~step`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-45-step-addressing-with-firststep)
- [Example 46: Address with `0,/regex/`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-46-address-with-0regex)
- [Example 47: Removing Duplicate Consecutive Lines](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-47-removing-duplicate-consecutive-lines)
- [Example 48: Printing Lines Between Two Patterns](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-48-printing-lines-between-two-patterns)
- [Example 49: Counting Lines (Using `=`)](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-49-counting-lines-using-)
- [Example 50: Reversing File Lines](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-50-reversing-file-lines)
- [Example 51: Converting Windows Line Endings to Unix](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-51-converting-windows-line-endings-to-unix)
- [Example 52: Inserting a Line After a Match](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-52-inserting-a-line-after-a-match)
- [Example 53: Deleting Lines Matching a Range of Patterns](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-53-deleting-lines-matching-a-range-of-patterns)
- [Example 54: Multiple Commands in a Block `{}`](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-54-multiple-commands-in-a-block-)
- [Example 55: Trimming Whitespace from Both Ends](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-55-trimming-whitespace-from-both-ends)
- [Example 56: Wrapping Lines in Quotes](/en/learn/software-engineering/automation-tools/sed/by-example/intermediate#example-56-wrapping-lines-in-quotes)

### Advanced (Examples 57–85)

- [Example 57: GNU sed vs BSD sed In-Place Editing](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-57-gnu-sed-vs-bsd-sed-in-place-editing)
- [Example 58: Extended Regex: GNU `-E` vs BSD `-E`](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-58-extended-regex-gnu--e-vs-bsd--e)
- [Example 59: Multiline Pattern Matching Across Line Boundaries](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-59-multiline-pattern-matching-across-line-boundaries)
- [Example 60: Sliding Window with `N` for Context](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-60-sliding-window-with-n-for-context)
- [Example 61: Removing a Block When Start and End Are on Adjacent Lines](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-61-removing-a-block-when-start-and-end-are-on-adjacent-lines)
- [Example 62: Extracting a Config Value](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-62-extracting-a-config-value)
- [Example 63: Updating a Config Value In-Place](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-63-updating-a-config-value-in-place)
- [Example 64: Commenting Out a Config Line](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-64-commenting-out-a-config-line)
- [Example 65: Uncommenting a Config Line](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-65-uncommenting-a-config-line)
- [Example 66: Extracting Fields from Log Lines](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-66-extracting-fields-from-log-lines)
- [Example 67: Filtering Log Lines by HTTP Status Code](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-67-filtering-log-lines-by-http-status-code)
- [Example 68: Normalizing Timestamp Formats in Logs](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-68-normalizing-timestamp-formats-in-logs)
- [Example 69: Extracting a CSV Field by Column Position](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-69-extracting-a-csv-field-by-column-position)
- [Example 70: Adding a CSV Header](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-70-adding-a-csv-header)
- [Example 71: Stripping HTML Tags](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-71-stripping-html-tags)
- [Example 72: Extracting Simple JSON Field Values](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-72-extracting-simple-json-field-values)
- [Example 73: Sed in a Pipeline Chain](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-73-sed-in-a-pipeline-chain)
- [Example 74: Sed with `find -exec`](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-74-sed-with-find--exec)
- [Example 75: Sed with Heredoc Input](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-75-sed-with-heredoc-input)
- [Example 76: Environment Variable Interpolation in Sed](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-76-environment-variable-interpolation-in-sed)
- [Example 77: Alternative Delimiters](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-77-alternative-delimiters)
- [Example 78: Early Exit for Large Files](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-78-early-exit-for-large-files)
- [Example 79: Processing Only a Line Range for Performance](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-79-processing-only-a-line-range-for-performance)
- [Example 80: Block Commenting Multiple Lines](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-80-block-commenting-multiple-lines)
- [Example 81: Uncommenting a Block](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-81-uncommenting-a-block)
- [Example 82: Sed One-Liner Collection for Common Tasks](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-82-sed-one-liner-collection-for-common-tasks)
- [Example 83: Real-World: Patching a Version String in Multiple Files](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-83-real-world-patching-a-version-string-in-multiple-files)
- [Example 84: Real-World: Anonymizing Log Data](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-84-real-world-anonymizing-log-data)
- [Example 85: Real-World: Generating a Configuration File from a Template](/en/learn/software-engineering/automation-tools/sed/by-example/advanced#example-85-real-world-generating-a-configuration-file-from-a-template)
