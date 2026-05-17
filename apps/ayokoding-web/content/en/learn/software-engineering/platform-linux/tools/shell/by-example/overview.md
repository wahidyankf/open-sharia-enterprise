---
title: "Overview"
weight: 10000000
date: 2025-12-30T07:56:02+07:00
draft: false
description: "Learn Linux shell through 80 annotated code examples covering 95% of essential command-line skills - ideal for experienced developers"
tags: ["linux", "shell", "bash", "tutorial", "by-example", "examples", "code-first"]
---

The Linux Command-Line By Example tutorial provides hands-on learning through practical, annotated code examples. This tutorial targets experienced developers who want to quickly understand command-line patterns and shell scripting techniques through direct code exploration.

## Tutorial Approach

This tutorial follows the **by-example** methodology:

- **Code-first learning** - Examples presented with minimal prose
- **Annotated commands** - Inline comments explain syntax and behavior
- **Progressive complexity** - Examples build from basic to advanced patterns
- **Real-world scenarios** - Commands and scripts based on practical use cases
- **Immediate applicability** - Copy, modify, and use examples in actual work

## What You'll Learn

By working through these examples, you'll understand:

- **Shell Basics** - Command syntax, options, arguments, and shell behavior
- **File System Navigation** - Moving through directories and locating files
- **File Operations** - Creating, copying, moving, deleting, and managing files
- **Text Processing** - Searching, filtering, transforming, and analyzing text
- **Pipes and Redirection** - Combining commands and controlling I/O streams
- **Process Management** - Monitoring, controlling, and scheduling processes
- **Shell Scripting** - Writing reusable automation scripts
- **Environment Configuration** - Customizing shell behavior and profiles
- **Command Composition** - Building complex workflows from simple commands

## Prerequisites

This tutorial assumes:

- **Linux access** - WSL, virtual machine, or native Linux installation
- **Terminal familiarity** - Comfort opening and using a terminal emulator
- **Programming experience** - Understanding of variables, loops, and conditionals
- **Text editor skills** - Ability to create and edit text files

No prior shell scripting experience required - the examples teach through demonstration.

## Coverage Overview

This tutorial provides **95% coverage** of essential Linux shell skills through **80 annotated examples**, organized into three levels:

### Beginner Level (Examples 1-30, 0-40% Coverage)

- **Basic Commands**: echo, ls, cd, pwd, mkdir, rm, cp, mv, touch, cat
- **File Viewing**: less, head, tail, wc
- **Text Search**: grep, find
- **Pipes & Redirection**: |, >, >>, <, 2>&1, tee
- **Variables**: assignment, expansion, command substitution, environment
- **Conditionals**: if/else, test, [[]], case
- **Loops**: for, while, arrays
- **Functions**: definition, arguments, return values

### Intermediate Level (Examples 31-55, 40-75% Coverage)

- **Text Processing**: sed, awk for log analysis and data transformation
- **Scripting Patterns**: argument parsing, error handling, exit codes
- **Process Management**: ps, kill, jobs, signals
- **Permissions**: chmod, chown, file security
- **Archiving**: tar, gzip, zip for backups
- **Network**: curl, wget, ssh, scp, rsync
- **Scheduling**: cron, at for automation
- **Best Practices**: production-ready script patterns

### Advanced Level (Examples 56-80, 75-95% Coverage)

- **Advanced Scripting**: Complex automation, error handling patterns
- **Signal Handling**: Trap commands, process lifecycle management
- **Performance**: Optimization techniques, parallel processing
- **Debugging**: Advanced troubleshooting, script profiling
- **System Administration**: User management, service configuration
- **Security**: Secure scripting patterns, permission handling
- **Production Patterns**: Enterprise-grade scripts, monitoring integration

## How to Use This Tutorial

1. **Read the code** - Study each example to understand command structure
2. **Run the commands** - Execute examples in your terminal to see results
3. **Modify and experiment** - Change parameters to explore command behavior
4. **Apply to projects** - Adapt examples to solve real problems in your work
5. **Reference later** - Return to specific examples when facing similar tasks

## Example Format

Examples follow a consistent annotation pattern:

```bash
# Descriptive comment explaining the command's purpose
command --option argument  # Inline note about specific syntax

# Multi-line examples include step-by-step comments
variable="value"           # Variable assignment
echo "$variable"           # Variable expansion in double quotes
```

## What is "By Example"?

By-example tutorials are **code-first learning materials** designed for experienced developers switching to or deepening their Linux shell knowledge. Unlike narrative tutorials, by-example focuses on:

- **Working, runnable code** - Every example is copy-paste-executable
- **Heavy annotations** - Inline comments with `# =>` notation show outputs and states
- **Self-contained examples** - Each example includes all necessary context
- **Production relevance** - Real-world patterns used in actual work
- **Progressive complexity** - Examples build from basics to production patterns

## Tutorial Structure

- **Examples 1-30 (Beginner)**: Core commands, file operations, basic scripting (0-40% coverage)
- **Examples 31-55 (Intermediate)**: Text processing, automation, production patterns (40-75% coverage)
- **Examples 56-80 (Advanced)**: Performance, debugging, system administration, security patterns (75-95% coverage)

## Next Steps

Start with [Beginner](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner) examples for fundamentals, then progress to [Intermediate](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate) for production patterns, and finally [Advanced](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced) for expert-level techniques.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: Echo and Basic Output](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-1-echo-and-basic-output)
- [Example 2: Variables and Assignment](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-2-variables-and-assignment)
- [Example 3: Command Structure and Options](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-3-command-structure-and-options)
- [Example 4: Navigating Directories (pwd, cd, ls)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-4-navigating-directories-pwd-cd-ls)
- [Example 5: Creating and Removing Directories (mkdir, rmdir, rm)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-5-creating-and-removing-directories-mkdir-rmdir-rm)
- [Example 6: File Viewing (cat, less, head, tail)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-6-file-viewing-cat-less-head-tail)
- [Example 7: File Operations (cp, mv, rm)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-7-file-operations-cp-mv-rm)
- [Example 8: File Permissions (ls -l, chmod)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-8-file-permissions-ls--l-chmod)
- [Example 9: Output Redirection (>, >>, <, 2>)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-9-output-redirection----2)
- [Example 10: Pipes (|)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-10-pipes-)
- [Example 11: Searching Files (find)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-11-searching-files-find)
- [Example 12: Searching Text (grep)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-12-searching-text-grep)
- [Example 13: Text Processing (cut, sort, uniq)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-13-text-processing-cut-sort-uniq)
- [Example 14: Command Substitution and Subshells](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-14-command-substitution-and-subshells)
- [Example 15: Environment Variables (export, env, PATH)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-15-environment-variables-export-env-path)
- [Example 16: Conditional Execution (&&, ||, ;)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-16-conditional-execution---)
- [Example 17: Test Conditions ([[]], test, [)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-17-test-conditions--test-)
- [Example 18: If Statements](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-18-if-statements)
- [Example 19: For Loops](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-19-for-loops)
- [Example 20: While and Until Loops](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-20-while-and-until-loops)
- [Example 21: Case Statements](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-21-case-statements)
- [Example 22: Functions](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-22-functions)
- [Example 23: Command Line Arguments (`$1`, `$2`, `$#`, `$@`)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-23-command-line-arguments-1-2--)
- [Example 24: Exit Codes and `$?`](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-24-exit-codes-and-)
- [Example 25: Quoting and Escaping](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-25-quoting-and-escaping)
- [Example 26: Here Documents and Here Strings](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-26-here-documents-and-here-strings)
- [Example 27: Brace Expansion](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-27-brace-expansion)
- [Example 28: Globbing and Wildcards](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-28-globbing-and-wildcards)
- [Example 29: Process Substitution](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-29-process-substitution)
- [Example 30: Job Control (bg, fg, jobs, &)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/beginner#example-30-job-control-bg-fg-jobs-)

### Intermediate (Examples 31–55)

- [Example 31: Text Processing (sed)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-31-text-processing-sed)
- [Example 32: Text Processing (awk)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-32-text-processing-awk)
- [Example 33: Command Line Arguments and Parsing](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-33-command-line-arguments-and-parsing)
- [Example 34: Error Handling and Exit Codes](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-34-error-handling-and-exit-codes)
- [Example 35: Process Management (ps, kill, jobs)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-35-process-management-ps-kill-jobs)
- [Example 36: File Permissions and Ownership (chmod, chown)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-36-file-permissions-and-ownership-chmod-chown)
- [Example 37: Archiving and Compression (tar, gzip, zip)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-37-archiving-and-compression-tar-gzip-zip)
- [Example 38: Network Operations (curl, wget, ssh)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-38-network-operations-curl-wget-ssh)
- [Example 39: Scheduling Tasks (cron, at)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-39-scheduling-tasks-cron-at)
- [Example 40: Script Best Practices](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-40-script-best-practices)
- [Example 41: Find Command Mastery](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-41-find-command-mastery)
- [Example 42: Regular Expressions in Shell](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-42-regular-expressions-in-shell)
- [Example 43: Array Manipulation](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-43-array-manipulation)
- [Example 44: Associative Arrays](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-44-associative-arrays)
- [Example 45: String Manipulation](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-45-string-manipulation)
- [Example 46: Arithmetic Operations](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-46-arithmetic-operations)
- [Example 47: Date and Time Handling](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-47-date-and-time-handling)
- [Example 48: Here Documents](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-48-here-documents)
- [Example 49: Process Substitution](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-49-process-substitution)
- [Example 50: Subshells and Command Grouping](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-50-subshells-and-command-grouping)
- [Example 51: Temporary Files and Directories](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-51-temporary-files-and-directories)
- [Example 52: Lock Files and Mutex Patterns](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-52-lock-files-and-mutex-patterns)
- [Example 53: Signal Handling Deep Dive](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-53-signal-handling-deep-dive)
- [Example 54: Debugging Shell Scripts](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-54-debugging-shell-scripts)
- [Example 55: Environment Variables](/en/learn/software-engineering/platform-linux/tools/shell/by-example/intermediate#example-55-environment-variables)

### Advanced (Examples 56–80)

- [Example 56: Process Management and Job Control](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-56-process-management-and-job-control)
- [Example 57: Signal Handling and Traps](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-57-signal-handling-and-traps)
- [Example 58: Advanced Parameter Expansion](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-58-advanced-parameter-expansion)
- [Example 59: Process Substitution and Named Pipes](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-59-process-substitution-and-named-pipes)
- [Example 60: Advanced Looping and Iteration](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-60-advanced-looping-and-iteration)
- [Example 61: Debugging and Error Handling](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-61-debugging-and-error-handling)
- [Example 62: Performance Optimization and Benchmarking](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-62-performance-optimization-and-benchmarking)
- [Example 63: Secure Scripting Practices](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-63-secure-scripting-practices)
- [Example 64: Advanced Text Processing (awk)](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-64-advanced-text-processing-awk)
- [Example 65: Production Deployment Script Pattern](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-65-production-deployment-script-pattern)
- [Example 66: Docker Integration](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-66-docker-integration)
- [Example 67: Kubernetes CLI Integration](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-67-kubernetes-cli-integration)
- [Example 68: AWS CLI Automation](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-68-aws-cli-automation)
- [Example 69: Git Automation](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-69-git-automation)
- [Example 70: Database Operations](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-70-database-operations)
- [Example 71: Log Analysis and Monitoring](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-71-log-analysis-and-monitoring)
- [Example 72: Performance Monitoring](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-72-performance-monitoring)
- [Example 73: API Integration Patterns](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-73-api-integration-patterns)
- [Example 74: Security Automation](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-74-security-automation)
- [Example 75: CI/CD Pipeline Scripts](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-75-cicd-pipeline-scripts)
- [Example 76: Configuration Management](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-76-configuration-management)
- [Example 77: Parallel Processing](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-77-parallel-processing)
- [Example 78: Data Transformation](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-78-data-transformation)
- [Example 79: Disaster Recovery Scripts](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-79-disaster-recovery-scripts)
- [Example 80: Production Deployment Patterns](/en/learn/software-engineering/platform-linux/tools/shell/by-example/advanced#example-80-production-deployment-patterns)
