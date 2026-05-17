---
title: "Overview"
weight: 10000000
date: 2026-04-01T00:00:00+07:00
draft: false
description: "Overview of the GitHub CLI by-example series: structure, approach, and what to expect from each level"
tags: ["gh", "github-cli", "tutorial", "by-example", "code-first"]
---

This series teaches GitHub CLI through heavily annotated, self-contained shell examples.
Each example focuses on a single command or pattern and includes inline annotations showing
the expected output, explaining what each flag does, and describing why the approach matters
in real workflows.

## Series Structure

The examples are organized into three levels based on complexity:

- [Beginner](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner) —
  Authentication, repository operations, issue management, pull request basics, and configuration
- [Intermediate](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate) —
  Advanced PR workflows, releases, gists, GitHub Actions management, secrets, variables, labels,
  SSH keys, and Codespaces
- [Advanced](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced) —
  REST and GraphQL API calls, pagination, jq filtering, extensions, scripting patterns, search,
  cache management, attestations, GPG keys, Projects, and CI/CD integration

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the command does and why it exists (2-3 sentences)
2. **Mermaid Diagram** — visual representation of the operation or data flow (when appropriate)
3. **Heavily Annotated Code** — shell commands with `# =>` comments showing output and effects
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## How to Use This Series

Each example contains a complete, runnable shell snippet. Read the `# =>` annotations alongside
the commands to understand both the mechanics and the intent. The examples within each level
build thematically, so reading sequentially provides the fullest understanding. Experienced
developers can also jump directly to the level and topic relevant to their current task.

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: Interactive Login](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-1-interactive-login)
- [Example 2: Non-Interactive Login with Token](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-2-non-interactive-login-with-token)
- [Example 3: Check Authentication Status](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-3-check-authentication-status)
- [Example 4: Log Out](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-4-log-out)
- [Example 5: Clone a Repository](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-5-clone-a-repository)
- [Example 6: Create a New Repository](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-6-create-a-new-repository)
- [Example 7: View a Repository](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-7-view-a-repository)
- [Example 8: List Your Repositories](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-8-list-your-repositories)
- [Example 9: Fork a Repository](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-9-fork-a-repository)
- [Example 10: Create an Issue](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-10-create-an-issue)
- [Example 11: List Issues](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-11-list-issues)
- [Example 12: View an Issue](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-12-view-an-issue)
- [Example 13: Close and Reopen an Issue](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-13-close-and-reopen-an-issue)
- [Example 14: Edit an Issue](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-14-edit-an-issue)
- [Example 15: Create a Pull Request](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-15-create-a-pull-request)
- [Example 16: List Pull Requests](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-16-list-pull-requests)
- [Example 17: View a Pull Request](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-17-view-a-pull-request)
- [Example 18: Check Out a Pull Request Branch](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-18-check-out-a-pull-request-branch)
- [Example 19: Merge a Pull Request](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-19-merge-a-pull-request)
- [Example 20: Set a Configuration Value](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-20-set-a-configuration-value)
- [Example 21: Get and List Configuration Values](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-21-get-and-list-configuration-values)
- [Example 22: Create an Alias](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-22-create-an-alias)
- [Example 23: Add a Comment to an Issue](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-23-add-a-comment-to-an-issue)
- [Example 24: List Repository Collaborators and Topics](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-24-list-repository-collaborators-and-topics)
- [Example 25: Pin and Unpin Issues](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-25-pin-and-unpin-issues)
- [Example 26: Transfer an Issue](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-26-transfer-an-issue)
- [Example 27: Lock and Unlock an Issue](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-27-lock-and-unlock-an-issue)
- [Example 28: Delete a Repository](/en/learn/software-engineering/automation-tools/gh-cli/by-example/beginner#example-28-delete-a-repository)

### Intermediate (Examples 29–56)

- [Example 29: Review a Pull Request](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-29-review-a-pull-request)
- [Example 30: Mark a PR as Ready for Review](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-30-mark-a-pr-as-ready-for-review)
- [Example 31: Check PR CI Status](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-31-check-pr-ci-status)
- [Example 32: View PR Diff](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-32-view-pr-diff)
- [Example 33: Add a Comment to a Pull Request](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-33-add-a-comment-to-a-pull-request)
- [Example 34: Create a Release](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-34-create-a-release)
- [Example 35: List and View Releases](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-35-list-and-view-releases)
- [Example 36: Delete a Release](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-36-delete-a-release)
- [Example 37: Download Release Assets](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-37-download-release-assets)
- [Example 38: Create a Gist](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-38-create-a-gist)
- [Example 39: List and View Gists](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-39-list-and-view-gists)
- [Example 40: Edit and Delete a Gist](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-40-edit-and-delete-a-gist)
- [Example 41: List Workflow Runs](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-41-list-workflow-runs)
- [Example 42: View a Workflow Run](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-42-view-a-workflow-run)
- [Example 43: Watch a Running Workflow](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-43-watch-a-running-workflow)
- [Example 44: Re-run a Failed Workflow](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-44-re-run-a-failed-workflow)
- [Example 45: List and Enable/Disable Workflows](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-45-list-and-enabledisable-workflows)
- [Example 46: Trigger a Workflow Manually](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-46-trigger-a-workflow-manually)
- [Example 47: Manage Repository Secrets](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-47-manage-repository-secrets)
- [Example 48: Manage Environment Secrets](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-48-manage-environment-secrets)
- [Example 49: Manage Variables](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-49-manage-variables)
- [Example 50: Create and List Labels](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-50-create-and-list-labels)
- [Example 51: Edit and Delete Labels](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-51-edit-and-delete-labels)
- [Example 52: Add and List SSH Keys](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-52-add-and-list-ssh-keys)
- [Example 53: Codespace: Create and List](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-53-codespace-create-and-list)
- [Example 54: SSH into a Codespace](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-54-ssh-into-a-codespace)
- [Example 55: Stop and Delete a Codespace](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-55-stop-and-delete-a-codespace)
- [Example 56: Port Forwarding in a Codespace](/en/learn/software-engineering/automation-tools/gh-cli/by-example/intermediate#example-56-port-forwarding-in-a-codespace)

### Advanced (Examples 57–85)

- [Example 57: Call the GitHub REST API](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-57-call-the-github-rest-api)
- [Example 58: Make POST and PATCH API Requests](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-58-make-post-and-patch-api-requests)
- [Example 59: Extract Fields with --jq](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-59-extract-fields-with---jq)
- [Example 60: Paginate API Results](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-60-paginate-api-results)
- [Example 61: Run a GraphQL Query](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-61-run-a-graphql-query)
- [Example 62: GraphQL with Variables and Pagination](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-62-graphql-with-variables-and-pagination)
- [Example 63: Install and Use an Extension](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-63-install-and-use-an-extension)
- [Example 64: Create a Shell Extension](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-64-create-a-shell-extension)
- [Example 65: Search Repositories](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-65-search-repositories)
- [Example 66: Search Issues and Pull Requests](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-66-search-issues-and-pull-requests)
- [Example 67: Search Code and Commits](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-67-search-code-and-commits)
- [Example 68: Output JSON for Scripting](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-68-output-json-for-scripting)
- [Example 69: Combine gh Commands in a Shell Script](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-69-combine-gh-commands-in-a-shell-script)
- [Example 70: Loop Over Repositories](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-70-loop-over-repositories)
- [Example 71: Use gh in GitHub Actions Workflows](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-71-use-gh-in-github-actions-workflows)
- [Example 72: List and Delete Workflow Caches](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-72-list-and-delete-workflow-caches)
- [Example 73: Verify Artifact Attestations](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-73-verify-artifact-attestations)
- [Example 74: Manage GPG Keys](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-74-manage-gpg-keys)
- [Example 75: List and View Projects](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-75-list-and-view-projects)
- [Example 76: Create a Project and Add Items](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-76-create-a-project-and-add-items)
- [Example 77: List Project Fields](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-77-list-project-fields)
- [Example 78: Shell Alias for Complete PR Workflow](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-78-shell-alias-for-complete-pr-workflow)
- [Example 79: Automated Release Script](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-79-automated-release-script)
- [Example 80: Stale Issue Management Script](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-80-stale-issue-management-script)
- [Example 81: Cross-Repository Deployment Coordination](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-81-cross-repository-deployment-coordination)
- [Example 82: Mermaid Diagram: gh API Data Flow](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-82-mermaid-diagram-gh-api-data-flow)
- [Example 83: Mermaid Diagram: PR Lifecycle with gh](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-83-mermaid-diagram-pr-lifecycle-with-gh)
- [Example 84: gh in Pre-push Hooks](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-84-gh-in-pre-push-hooks)
- [Example 85: Complete Onboarding Automation Script](/en/learn/software-engineering/automation-tools/gh-cli/by-example/advanced#example-85-complete-onboarding-automation-script)
