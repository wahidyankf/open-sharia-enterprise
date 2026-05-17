---
title: "Overview"
weight: 10000000
date: 2026-04-01T00:00:00+07:00
draft: false
description: "Overview of the jq by-example series: structure, approach, and what to expect from each level"
tags: ["jq", "json", "data-processing", "tutorial", "by-example", "code-first"]
---

This series teaches jq through heavily annotated, self-contained shell examples. Each
example focuses on a single concept and includes inline annotations explaining what
each command does, why that approach is idiomatic, and what output or state results.

## Series Structure

The examples are organized into three levels based on complexity:

- [Beginner](/en/learn/software-engineering/automation-tools/jq/by-example/beginner) —
  Identity filter, field access, array indexing, pipes, basic types, object and array
  construction, iteration, optional operators, and command-line flags
- [Intermediate](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate) —
  `map`, `select`, `reduce`, `group_by`, `sort_by`, `unique_by`, conditionals, string
  functions, path expressions, and type conversions
- [Advanced](/en/learn/software-engineering/automation-tools/jq/by-example/advanced) —
  User-defined functions, recursion, path manipulation, format strings, streaming parser,
  SQL-style joins, environment variables, and real-world API data processing

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the filter does and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of data flow or filter composition (when appropriate)
3. **Heavily Annotated Code** — shell commands with `# =>` comments describing each filter stage and its output
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## How to Use This Series

Each page presents annotated shell invocations using `echo '...' | jq '...'` so every
example runs on any machine with jq installed. Read the annotations alongside the code
to understand both the mechanics and the intent. The examples build on each other within
each level, so reading sequentially gives the fullest understanding of how jq filters
compose into real-world pipelines.

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: Identity Filter](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-1-identity-filter)
- [Example 2: Field Access with `.foo`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-2-field-access-with-foo)
- [Example 3: Nested Field Access with `.foo.bar`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-3-nested-field-access-with-foobar)
- [Example 4: Array Index with `.[N]`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-4-array-index-with-n)
- [Example 5: Array Slice with `.[M:N]`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-5-array-slice-with-mn)
- [Example 6: Pipe Operator `|`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-6-pipe-operator-)
- [Example 7: Comma Operator for Multiple Outputs](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-7-comma-operator-for-multiple-outputs)
- [Example 8: Object Construction with `{}`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-8-object-construction-with-)
- [Example 9: Array Construction with `[]`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-9-array-construction-with-)
- [Example 10: String Interpolation with `\(expr)`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-10-string-interpolation-with-expr)
- [Example 11: The `type` Function](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-11-the-type-function)
- [Example 12: Null Handling](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-12-null-handling)
- [Example 13: `length` Function](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-13-length-function)
- [Example 14: `keys` and `values`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-14-keys-and-values)
- [Example 15: `keys_unsorted`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-15-keys_unsorted)
- [Example 16: `has` Function](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-16-has-function)
- [Example 17: `in` Operator](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-17-in-operator)
- [Example 18: `empty` Filter](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-18-empty-filter)
- [Example 19: `.[]` Array Iteration](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-19--array-iteration)
- [Example 20: `.[]` Object Iteration](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-20--object-iteration)
- [Example 21: Optional Field Access with `.foo?`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-21-optional-field-access-with-foo)
- [Example 22: `.[]?` Optional Iteration](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-22--optional-iteration)
- [Example 23: Raw Output with `-r`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-23-raw-output-with--r)
- [Example 24: Compact Output with `-c`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-24-compact-output-with--c)
- [Example 25: Null Input with `-n`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-25-null-input-with--n)
- [Example 26: Slurp Mode with `-s`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-26-slurp-mode-with--s)
- [Example 27: Raw Input with `-R`](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-27-raw-input-with--r)
- [Example 28: Multiple Input Files](/en/learn/software-engineering/automation-tools/jq/by-example/beginner#example-28-multiple-input-files)

### Intermediate (Examples 29–56)

- [Example 29: `map` Function](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-29-map-function)
- [Example 30: `select` Function](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-30-select-function)
- [Example 31: `map_values` Function](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-31-map_values-function)
- [Example 32: `select` with Object Filtering](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-32-select-with-object-filtering)
- [Example 33: `select` with Multiple Conditions](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-33-select-with-multiple-conditions)
- [Example 34: `sort_by` Function](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-34-sort_by-function)
- [Example 35: `group_by` Function](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-35-group_by-function)
- [Example 36: `unique_by` Function](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-36-unique_by-function)
- [Example 37: `reverse` and `flatten`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-37-reverse-and-flatten)
- [Example 38: `add` Function](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-38-add-function)
- [Example 39: `any` and `all`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-39-any-and-all)
- [Example 40: `reduce` Expression](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-40-reduce-expression)
- [Example 41: `reduce` for Building Objects](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-41-reduce-for-building-objects)
- [Example 42: `limit` and `first`/`last`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-42-limit-and-firstlast)
- [Example 43: `range` Function](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-43-range-function)
- [Example 44: `if-then-else` Expression](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-44-if-then-else-expression)
- [Example 45: Alternative Operator `//`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-45-alternative-operator-)
- [Example 46: `try-catch` for Error Handling](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-46-try-catch-for-error-handling)
- [Example 47: `split` and `join`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-47-split-and-join)
- [Example 48: `test` and `match` for Regex](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-48-test-and-match-for-regex)
- [Example 49: `capture` for Named Groups](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-49-capture-for-named-groups)
- [Example 50: String Trimming and Case Functions](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-50-string-trimming-and-case-functions)
- [Example 51: `startswith` and `endswith`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-51-startswith-and-endswith)
- [Example 52: `tostring` and `tonumber`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-52-tostring-and-tonumber)
- [Example 53: `to_entries` and `from_entries`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-53-to_entries-and-from_entries)
- [Example 54: `with_entries` Shorthand](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-54-with_entries-shorthand)
- [Example 55: `contains` and `inside`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-55-contains-and-inside)
- [Example 56: `min_by` and `max_by`](/en/learn/software-engineering/automation-tools/jq/by-example/intermediate#example-56-min_by-and-max_by)

### Advanced (Examples 57–85)

- [Example 57: Defining Functions with `def`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-57-defining-functions-with-def)
- [Example 58: Functions with Filter Arguments](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-58-functions-with-filter-arguments)
- [Example 59: Recursive Functions](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-59-recursive-functions)
- [Example 60: `recurse` Built-in](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-60-recurse-built-in)
- [Example 61: `path` Expression](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-61-path-expression)
- [Example 62: `getpath`, `setpath`, and `delpaths`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-62-getpath-setpath-and-delpaths)
- [Example 63: `leaf_paths`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-63-leaf_paths)
- [Example 64: Update Operator `|=`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-64-update-operator-)
- [Example 65: Alternative Update `//=`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-65-alternative-update-)
- [Example 66: `del` Function](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-66-del-function)
- [Example 67: `indices`, `index`, `rindex`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-67-indices-index-rindex)
- [Example 68: `@base64` and `@base64d`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-68-base64-and-base64d)
- [Example 69: `@csv` and `@tsv`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-69-csv-and-tsv)
- [Example 70: `@html` and `@uri`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-70-html-and-uri)
- [Example 71: `@json` and `@text`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-71-json-and-text)
- [Example 72: `$ENV` and `env`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-72-env-and-env)
- [Example 73: Variable Binding with `as`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-73-variable-binding-with-as)
- [Example 74: Pattern Matching with `as`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-74-pattern-matching-with-as)
- [Example 75: SQL-Style Cross Join](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-75-sql-style-cross-join)
- [Example 76: `INDEX` Function](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-76-index-function)
- [Example 77: `inputs` for Multiple Documents](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-77-inputs-for-multiple-documents)
- [Example 78: `debug` for Inspection](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-78-debug-for-inspection)
- [Example 79: `builtins` Function](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-79-builtins-function)
- [Example 80: Streaming Parser with `tostream` and `fromstream`](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-80-streaming-parser-with-tostream-and-fromstream)
- [Example 81: GitHub API Response Processing](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-81-github-api-response-processing)
- [Example 82: npm Registry Response Processing](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-82-npm-registry-response-processing)
- [Example 83: Log File JSON Processing](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-83-log-file-json-processing)
- [Example 84: Configuration File Transformation](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-84-configuration-file-transformation)
- [Example 85: Complex Multi-Source Data Pipeline](/en/learn/software-engineering/automation-tools/jq/by-example/advanced#example-85-complex-multi-source-data-pipeline)
