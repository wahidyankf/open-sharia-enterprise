---
title: "Overview"
weight: 10000000
date: 2026-03-20T00:00:00+07:00
draft: false
description: "Overview of the GitHub Actions by-example series: structure, approach, and what to expect from each level"
tags: ["github-actions", "ci-cd", "tutorial", "by-example", "code-first"]
---

This series teaches GitHub Actions through heavily annotated, self-contained code examples.
Each example focuses on a single concept and includes inline annotations explaining what
each line does, why it matters, and what value or state results from it.

## Series Structure

The examples are organized into three levels based on complexity:

- [Beginner](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner) —
  Core workflow syntax, basic triggers, single jobs, and fundamental step types
- [Intermediate](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate) —
  Multi-job workflows, secrets, environment variables, matrices, and caching
- [Advanced](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced) —
  Reusable workflows, custom actions, complex event handling, and deployment patterns

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the workflow does and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of workflow execution, job dependencies, or trigger flow (when appropriate)
3. **Heavily Annotated YAML** — workflow files with `# =>` comments describing each directive and its effect
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## How to Use This Series

Each page presents annotated YAML workflow files. Read the annotations alongside the code
to understand both the mechanics and the intent. The examples build on each other within
each level, so reading sequentially gives the fullest understanding.

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: Minimal Workflow File](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-1-minimal-workflow-file)
- [Example 2: Workflow Name and Job Name](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-2-workflow-name-and-job-name)
- [Example 3: The `on` Key with a Single Event](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-3-the-on-key-with-a-single-event)
- [Example 4: The `push` Trigger](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-4-the-push-trigger)
- [Example 5: The `pull_request` Trigger](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-5-the-pull_request-trigger)
- [Example 6: Multiple Triggers on One Workflow](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-6-multiple-triggers-on-one-workflow)
- [Example 7: Branch Filters on `push`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-7-branch-filters-on-push)
- [Example 8: Path Filters on `push`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-8-path-filters-on-push)
- [Example 9: The `schedule` Trigger (Cron Syntax)](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-9-the-schedule-trigger-cron-syntax)
- [Example 10: The `workflow_dispatch` Trigger (Manual Run)](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-10-the-workflow_dispatch-trigger-manual-run)
- [Example 11: Ubuntu, Windows, and macOS Runners](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-11-ubuntu-windows-and-macos-runners)
- [Example 12: Pinning a Specific Runner Version](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-12-pinning-a-specific-runner-version)
- [Example 13: The `run` Step with Multi-Line Commands](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-13-the-run-step-with-multi-line-commands)
- [Example 14: The `uses` Step with `actions/checkout`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-14-the-uses-step-with-actionscheckout)
- [Example 15: The `uses` Step with `actions/setup-node`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-15-the-uses-step-with-actionssetup-node)
- [Example 16: The `uses` Step with `actions/setup-python`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-16-the-uses-step-with-actionssetup-python)
- [Example 17: Passing Inputs to Actions with `with`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-17-passing-inputs-to-actions-with-with)
- [Example 18: Setting `env` at the Workflow Level](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-18-setting-env-at-the-workflow-level)
- [Example 19: Setting `env` at the Job Level](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-19-setting-env-at-the-job-level)
- [Example 20: Setting `env` at the Step Level](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-20-setting-env-at-the-step-level)
- [Example 21: Changing the Working Directory for `run` Steps](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-21-changing-the-working-directory-for-run-steps)
- [Example 22: Skipping Steps with `if` Conditionals](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-22-skipping-steps-with-if-conditionals)
- [Example 23: `if` on Jobs](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-23-if-on-jobs)
- [Example 24: Sequential Jobs with `needs`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-24-sequential-jobs-with-needs)
- [Example 25: Accessing Outputs from Required Jobs](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-25-accessing-outputs-from-required-jobs)
- [Example 26: Setting `timeout-minutes`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-26-setting-timeout-minutes)
- [Example 27: Using `continue-on-error`](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-27-using-continue-on-error)
- [Example 28: Combining Triggers, Env, Needs, and Conditionals](/en/learn/software-engineering/automation-tools/github-actions/by-example/beginner#example-28-combining-triggers-env-needs-and-conditionals)

### Intermediate (Examples 29–57)

- [Example 29: Secrets Context](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-29-secrets-context)
- [Example 30: Environment Variables at Workflow, Job, and Step Level](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-30-environment-variables-at-workflow-job-and-step-level)
- [Example 31: strategy.matrix for Cross-Platform Builds](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-31-strategymatrix-for-cross-platform-builds)
- [Example 32: Matrix Include and Exclude](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-32-matrix-include-and-exclude)
- [Example 33: actions/cache for Build Dependencies](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-33-actionscache-for-build-dependencies)
- [Example 34: actions/cache for Go Modules](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-34-actionscache-for-go-modules)
- [Example 35: actions/upload-artifact and download-artifact](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-35-actionsupload-artifact-and-download-artifact)
- [Example 36: Job Outputs Passed to Downstream Jobs](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-36-job-outputs-passed-to-downstream-jobs)
- [Example 37: needs with Conditional Execution](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-37-needs-with-conditional-execution)
- [Example 38: Concurrency Groups](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-38-concurrency-groups)
- [Example 39: Per-Job Concurrency Groups](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-39-per-job-concurrency-groups)
- [Example 40: permissions Key and GITHUB_TOKEN](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-40-permissions-key-and-github_token)
- [Example 41: success(), failure(), always(), cancelled()](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-41-success-failure-always-cancelled)
- [Example 42: github Context](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-42-github-context)
- [Example 43: env, steps, job, and runner Contexts](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-43-env-steps-job-and-runner-contexts)
- [Example 44: hashFiles() Built-in Function](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-44-hashfiles-built-in-function)
- [Example 45: fromJSON() and toJSON() Functions](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-45-fromjson-and-tojson-functions)
- [Example 46: Services (Docker Containers as Test Dependencies)](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-46-services-docker-containers-as-test-dependencies)
- [Example 47: Container Jobs](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-47-container-jobs)
- [Example 48: Local Composite Action](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-48-local-composite-action)
- [Example 49: workflow_call Trigger (Reusable Workflows Basics)](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-49-workflow_call-trigger-reusable-workflows-basics)
- [Example 50: github.event Context Details](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-50-githubevent-context-details)
- [Example 51: repository_dispatch Trigger](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-51-repository_dispatch-trigger)
- [Example 52: Environment Protection Rules](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-52-environment-protection-rules)
- [Example 53: Expressions — Operators and Built-in Functions](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-53-expressions--operators-and-built-in-functions)
- [Example 54: Multi-Stage Pipeline with All Contexts](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-54-multi-stage-pipeline-with-all-contexts)
- [Example 55: Passing Secrets Across Jobs with Inherited Secrets](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-55-passing-secrets-across-jobs-with-inherited-secrets)
- [Example 56: Dynamic Matrix from Step Output](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-56-dynamic-matrix-from-step-output)
- [Example 57: Combining Services, Containers, and Artifacts](/en/learn/software-engineering/automation-tools/github-actions/by-example/intermediate#example-57-combining-services-containers-and-artifacts)

### Advanced (Examples 58–85)

- [Example 58: Reusable Workflow with Inputs, Outputs, and Secrets](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-58-reusable-workflow-with-inputs-outputs-and-secrets)
- [Example 59: Composite Action in action.yml](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-59-composite-action-in-actionyml)
- [Example 60: JavaScript Action with action.yml and index.js](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-60-javascript-action-with-actionyml-and-indexjs)
- [Example 61: Docker Container Action](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-61-docker-container-action)
- [Example 62: Matrix with include/exclude and Dynamic fromJSON](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-62-matrix-with-includeexclude-and-dynamic-fromjson)
- [Example 63: Workflow Chaining with workflow_run](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-63-workflow-chaining-with-workflow_run)
- [Example 64: Deployment Environments with Required Approvals](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-64-deployment-environments-with-required-approvals)
- [Example 65: OIDC Federated Identity for AWS (No Long-Lived Credentials)](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-65-oidc-federated-identity-for-aws-no-long-lived-credentials)
- [Example 66: OIDC for GCP and Azure](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-66-oidc-for-gcp-and-azure)
- [Example 67: Self-Hosted Runners and Runner Groups](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-67-self-hosted-runners-and-runner-groups)
- [Example 68: GitHub Apps for Workflow Authentication](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-68-github-apps-for-workflow-authentication)
- [Example 69: GitHub API in Workflows with gh CLI and Octokit](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-69-github-api-in-workflows-with-gh-cli-and-octokit)
- [Example 70: Release Automation with Semantic Release](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-70-release-automation-with-semantic-release)
- [Example 71: Monorepo CI with Path Filters and Conditional Jobs](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-71-monorepo-ci-with-path-filters-and-conditional-jobs)
- [Example 72: Advanced Dependency Caching Strategies](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-72-advanced-dependency-caching-strategies)
- [Example 73: Build Artifact Retention and Cross-Job Sharing](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-73-build-artifact-retention-and-cross-job-sharing)
- [Example 74: Security Hardening — Pin Actions to SHA and Least-Privilege Permissions](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-74-security-hardening--pin-actions-to-sha-and-least-privilege-permissions)
- [Example 75: Workflow Dispatch with Complex Inputs](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-75-workflow-dispatch-with-complex-inputs)
- [Example 76: Caching Build Outputs for Incremental Compilation](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-76-caching-build-outputs-for-incremental-compilation)
- [Example 77: Artifact Attestation and SLSA Provenance](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-77-artifact-attestation-and-slsa-provenance)
- [Example 78: Large Runner Features and GPU Workflows](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-78-large-runner-features-and-gpu-workflows)
- [Example 79: Concurrency Control and Workflow Cancellation](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-79-concurrency-control-and-workflow-cancellation)
- [Example 80: Status Checks and Branch Protection Integration](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-80-status-checks-and-branch-protection-integration)
- [Example 81: Debugging Workflows with tmate and Step Summaries](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-81-debugging-workflows-with-tmate-and-step-summaries)
- [Example 82: Calling GitHub REST API with Pagination](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-82-calling-github-rest-api-with-pagination)
- [Example 83: Workflow Job Dependencies and Fan-Out/Fan-In Patterns](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-83-workflow-job-dependencies-and-fan-outfan-in-patterns)
- [Example 84: OpenID Connect Token Claims and Advanced Trust Policies](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-84-openid-connect-token-claims-and-advanced-trust-policies)
- [Example 85: Complete Production Workflow — Integration of All Advanced Patterns](/en/learn/software-engineering/automation-tools/github-actions/by-example/advanced#example-85-complete-production-workflow--integration-of-all-advanced-patterns)
