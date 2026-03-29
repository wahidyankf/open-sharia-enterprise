---
title: "Overview"
weight: 10000000
date: 2026-03-20T00:00:00+07:00
draft: false
---

Infrastructure as Code (IaC) is the practice of managing and provisioning infrastructure through machine-readable configuration files rather than manual processes or interactive configuration tools.

## What Is Infrastructure as Code

IaC treats infrastructure configuration with the same rigor applied to application source code. Configuration files define the desired state of servers, networks, databases, and other infrastructure components. Tools then apply these configurations consistently across environments.

## Core Benefits

IaC delivers several key advantages over manual infrastructure management:

- **Repeatability**: The same configuration produces identical infrastructure every time.
- **Version control**: Infrastructure changes are tracked, reviewed, and auditable.
- **Reduced human error**: Automation eliminates inconsistencies introduced by manual steps.
- **Faster provisioning**: Entire environments spin up in minutes rather than hours or days.
- **Environment parity**: Development, staging, and production environments stay aligned.

## Declarative vs. Imperative Approaches

IaC tools generally fall into two categories:

**Declarative** tools (Terraform, CloudFormation, Pulumi) let you describe the desired end state. The tool determines how to reach that state.

**Imperative** tools (Ansible scripts, shell scripts) specify the exact steps to execute. You control the sequence of operations.

Most modern IaC workflows use declarative tools for infrastructure provisioning and imperative tools for configuration management.

## Common IaC Tools

- **Terraform**: Cloud-agnostic declarative provisioning using HCL
- **Ansible**: Agentless configuration management using YAML playbooks
- **CloudFormation**: AWS-native declarative templates (JSON or YAML)
- **Pulumi**: General-purpose languages (TypeScript, Python, Go, C#, Java) for infrastructure

## By Example

The [By Example](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example) section provides heavily annotated code examples covering real-world IaC patterns across all major tools and skill levels.
