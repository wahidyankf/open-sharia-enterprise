---
title: "Overview"
weight: 10000000
date: 2026-03-20T00:00:00+07:00
draft: false
---

GitHub Actions is a CI/CD platform built directly into GitHub that automates software workflows.
It enables teams to build, test, and deploy code automatically in response to events such as
pushes, pull requests, schedules, or manual triggers.

## What GitHub Actions Provides

GitHub Actions uses YAML-based workflow files stored in `.github/workflows/` to define
automation pipelines. Each workflow consists of one or more jobs, which run in isolated
virtual environments and execute a series of steps.

## Key Concepts

- **Workflows**: YAML files defining automated processes triggered by events
- **Jobs**: Groups of steps that run on the same runner
- **Steps**: Individual tasks within a job (shell commands or actions)
- **Actions**: Reusable units of automation (from the marketplace or custom-built)
- **Runners**: Virtual machines that execute jobs (GitHub-hosted or self-hosted)
- **Secrets**: Encrypted variables for sensitive configuration (tokens, passwords)
- **Matrices**: Strategy to run the same job across multiple configurations

## Content in This Section

- [By Example](/en/learn/software-engineering/automation-tools/github-actions/by-example) —
  Learn GitHub Actions through heavily annotated code examples covering workflows, triggers,
  jobs, steps, secrets, matrices, reusable workflows, and custom actions.
