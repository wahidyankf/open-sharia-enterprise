---
title: "Overview"
weight: 10000000
date: 2026-04-01T00:00:00+07:00
draft: false
description: "Overview of the awk by-example series: structure, approach, and what to expect from each level"
tags: ["awk", "text-processing", "tutorial", "by-example", "code-first"]
---

This series teaches awk through 85 heavily annotated, self-contained code examples. Each example
focuses on a single concept and includes inline annotations explaining what each line does, why
it matters, and what value or state results from it. All examples use `echo` or heredoc input
piped into awk — they run directly in any POSIX shell without additional setup.

## Series Structure

The examples are organized into three levels based on complexity:

- [Beginner](/en/learn/software-engineering/automation-tools/awk/by-example/beginner) —
  Field printing, separators, built-in variables, pattern matching, BEGIN/END blocks,
  arithmetic, string operations, and basic output formatting (Examples 1–28)
- [Intermediate](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate) —
  Associative arrays, user-defined functions, string functions, getline, multiple file
  processing, record/field separators, and environment variables (Examples 29–56)
- [Advanced](/en/learn/software-engineering/automation-tools/awk/by-example/advanced) —
  Multidimensional arrays, coprocesses, CSV parsing, report generation, state machines,
  real-world data analysis pipelines, and gawk-specific extensions (Examples 57–85)

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the example demonstrates and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of data flow or concept relationships (when appropriate)
3. **Heavily Annotated Code** — self-contained awk program with `# =>` comments showing values,
   states, and output at each step
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world application (50-100 words)

## How to Use This Series

Each example is a complete, runnable shell snippet. The `# =>` annotations show expected output
and intermediate values inline — read them alongside the code rather than running each example
independently. Examples within each level build on each other, so reading sequentially within
a level provides the fullest understanding. Readers already familiar with awk basics can jump
directly to Intermediate or Advanced.

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: Print Every Line](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-1-print-every-line)
- [Example 2: Print a Specific Field](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-2-print-a-specific-field)
- [Example 3: Print Multiple Fields](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-3-print-multiple-fields)
- [Example 4: Print the Last Field with $NF](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-4-print-the-last-field-with-nf)
- [Example 5: Print the Entire Record with $0](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-5-print-the-entire-record-with-0)
- [Example 6: Custom Input Field Separator with -F](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-6-custom-input-field-separator-with--f)
- [Example 7: Tab-Separated Input](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-7-tab-separated-input)
- [Example 8: Setting OFS — Output Field Separator](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-8-setting-ofs--output-field-separator)
- [Example 9: Multi-Character Field Separator](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-9-multi-character-field-separator)
- [Example 10: NR — Record Number](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-10-nr--record-number)
- [Example 11: NF — Number of Fields](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-11-nf--number-of-fields)
- [Example 12: Combining NR and NF](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-12-combining-nr-and-nf)
- [Example 13: Regex Pattern Matching](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-13-regex-pattern-matching)
- [Example 14: Numeric Comparison Pattern](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-14-numeric-comparison-pattern)
- [Example 15: String Comparison Pattern](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-15-string-comparison-pattern)
- [Example 16: Compound Patterns with && and ||](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-16-compound-patterns-with--and-)
- [Example 17: Negated Pattern with Exclamation Mark](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-17-negated-pattern-with-exclamation-mark)
- [Example 18: Range Pattern](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-18-range-pattern)
- [Example 19: BEGIN Block](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-19-begin-block)
- [Example 20: END Block](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-20-end-block)
- [Example 21: BEGIN and END Together](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-21-begin-and-end-together)
- [Example 22: Arithmetic Operations](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-22-arithmetic-operations)
- [Example 23: Increment, Decrement, and Assignment Operators](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-23-increment-decrement-and-assignment-operators)
- [Example 24: String Concatenation](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-24-string-concatenation)
- [Example 25: printf for Formatted Output](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-25-printf-for-formatted-output)
- [Example 26: Redirect Output to a File](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-26-redirect-output-to-a-file)
- [Example 27: Pipe Output to a Command](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-27-pipe-output-to-a-command)
- [Example 28: Multiple Statements and Comments](/en/learn/software-engineering/automation-tools/awk/by-example/beginner#example-28-multiple-statements-and-comments)

### Intermediate (Examples 29–56)

- [Example 29: Basic Associative Array](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-29-basic-associative-array)
- [Example 30: Array Iteration with for..in](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-30-array-iteration-with-forin)
- [Example 31: Testing if a Key Exists](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-31-testing-if-a-key-exists)
- [Example 32: Deleting Array Elements](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-32-deleting-array-elements)
- [Example 33: Array as a Set for Deduplication](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-33-array-as-a-set-for-deduplication)
- [Example 34: Defining and Calling a Function](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-34-defining-and-calling-a-function)
- [Example 35: Local Variables in Functions](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-35-local-variables-in-functions)
- [Example 36: length() — String and Array Length](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-36-length--string-and-array-length)
- [Example 37: substr() — Substring Extraction](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-37-substr--substring-extraction)
- [Example 38: index() — Finding a Substring](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-38-index--finding-a-substring)
- [Example 39: split() — Split String into Array](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-39-split--split-string-into-array)
- [Example 40: sub() and gsub() — Substitution](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-40-sub-and-gsub--substitution)
- [Example 41: match() — Regex Match with Position](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-41-match--regex-match-with-position)
- [Example 42: tolower() and toupper()](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-42-tolower-and-toupper)
- [Example 43: sprintf() — Format String Without Printing](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-43-sprintf--format-string-without-printing)
- [Example 44: getline from Standard Input](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-44-getline-from-standard-input)
- [Example 45: getline from a File](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-45-getline-from-a-file)
- [Example 46: getline from a Pipe](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-46-getline-from-a-pipe)
- [Example 47: FILENAME Variable](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-47-filename-variable)
- [Example 48: Using FNR and NR Together for Two-File Join](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-48-using-fnr-and-nr-together-for-two-file-join)
- [Example 49: ARGC and ARGV](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-49-argc-and-argv)
- [Example 50: RS — Custom Record Separator](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-50-rs--custom-record-separator)
- [Example 51: ORS — Output Record Separator](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-51-ors--output-record-separator)
- [Example 52: Multiline Records with RS=""](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-52-multiline-records-with-rs)
- [Example 53: ~ and !~ Operators](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-53--and--operators)
- [Example 54: OFMT and CONVFMT](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-54-ofmt-and-convfmt)
- [Example 55: ENVIRON Array](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-55-environ-array)
- [Example 56: Ternary Operator](/en/learn/software-engineering/automation-tools/awk/by-example/intermediate#example-56-ternary-operator)

### Advanced (Examples 57–85)

- [Example 57: Multidimensional Arrays with SUBSEP](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-57-multidimensional-arrays-with-subsep)
- [Example 58: Checking Multi-Key Existence](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-58-checking-multi-key-existence)
- [Example 59: Recursive Functions](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-59-recursive-functions)
- [Example 60: Functions Modifying Arrays by Reference](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-60-functions-modifying-arrays-by-reference)
- [Example 61: systime() and strftime() — Date and Time](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-61-systime-and-strftime--date-and-time)
- [Example 62: system() — Running Shell Commands](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-62-system--running-shell-commands)
- [Example 63: Coprocesses with |& (gawk)](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-63-coprocesses-with--gawk)
- [Example 64: FPAT for CSV Parsing (gawk)](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-64-fpat-for-csv-parsing-gawk)
- [Example 65: Log File Analysis — Access Log Parser](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-65-log-file-analysis--access-log-parser)
- [Example 66: Report Generation with Headers and Footers](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-66-report-generation-with-headers-and-footers)
- [Example 67: Word Frequency Counter](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-67-word-frequency-counter)
- [Example 68: State Machine Pattern](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-68-state-machine-pattern)
- [Example 69: Histogram Generation](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-69-histogram-generation)
- [Example 70: Transposing Rows and Columns](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-70-transposing-rows-and-columns)
- [Example 71: Cross-Referencing Two Files](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-71-cross-referencing-two-files)
- [Example 72: Pivot Table Generation](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-72-pivot-table-generation)
- [Example 73: Deduplication by Field Value](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-73-deduplication-by-field-value)
- [Example 74: Generating JSON-Like Output](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-74-generating-json-like-output)
- [Example 75: awk Script as a File with Shebang](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-75-awk-script-as-a-file-with-shebang)
- [Example 76: Command-Line Variable Passing with -v](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-76-command-line-variable-passing-with--v)
- [Example 77: @include — Including Other awk Files (gawk)](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-77-include--including-other-awk-files-gawk)
- [Example 78: gawk Profiling with --profile](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-78-gawk-profiling-with---profile)
- [Example 79: Network Programming with /inet/ (gawk)](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-79-network-programming-with-inet-gawk)
- [Example 80: Real-World Pipeline — Nginx Log to Alert](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-80-real-world-pipeline--nginx-log-to-alert)
- [Example 81: Frequency Table with Percentages](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-81-frequency-table-with-percentages)
- [Example 82: Running Average and Standard Deviation](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-82-running-average-and-standard-deviation)
- [Example 83: AWK Automation Pipeline — CSV to SQL INSERT](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-83-awk-automation-pipeline--csv-to-sql-insert)
- [Example 84: In-Place File Editing Pattern](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-84-in-place-file-editing-pattern)
- [Example 85: Complete Data Pipeline — Sales Analysis](/en/learn/software-engineering/automation-tools/awk/by-example/advanced#example-85-complete-data-pipeline--sales-analysis)
