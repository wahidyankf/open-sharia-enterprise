---
title: "Overview"
weight: 10000000
date: 2026-03-20T00:00:00+07:00
draft: false
description: "Learn infrastructure as code through 85+ annotated examples covering Terraform, Ansible, CloudFormation, Pulumi, and IaC patterns - ideal for experienced developers"
tags: ["infrastructure-as-code", "iac", "tutorial", "by-example", "code-first"]
---

This series teaches infrastructure as code through heavily annotated, self-contained examples.
Each example focuses on a single IaC concept and includes inline annotations explaining what
each directive does, why it matters, and what infrastructure state results from it.

## Series Structure

The examples are organized into three levels based on complexity:

- [Beginner](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner) —
  Foundational IaC concepts, basic resource provisioning, and single-provider configurations
- [Intermediate](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate) —
  Modules, state management, multi-environment patterns, and configuration management
- [Advanced](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced) —
  Multi-cloud patterns, policy as code, drift detection, and enterprise-scale IaC architectures

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the IaC pattern addresses and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of infrastructure topology, provisioning flow, or state transitions (when appropriate)
3. **Heavily Annotated Code** — HCL, YAML, or other IaC configuration with `# =>` comments documenting each directive and its effect
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## How to Use This Series

Each page presents annotated IaC configuration files. Read the annotations alongside the code
to understand both the mechanics and the intent. The examples build on each other within
each level, so reading sequentially gives the fullest understanding.

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: HCL Block Syntax](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-1-hcl-block-syntax)
- [Example 2: Terraform Provider Block](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-2-terraform-provider-block)
- [Example 3: String, Number, and Boolean Arguments](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-3-string-number-and-boolean-arguments)
- [Example 4: HCL Comments](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-4-hcl-comments)
- [Example 5: Terraform Init and the Lock File](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-5-terraform-init-and-the-lock-file)
- [Example 6: Declaring an AWS EC2 Resource](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-6-declaring-an-aws-ec2-resource)
- [Example 7: Resource References and Interpolation](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-7-resource-references-and-interpolation)
- [Example 8: Resource with Multiple Arguments](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-8-resource-with-multiple-arguments)
- [Example 9: Input Variables with Type Constraints](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-9-input-variables-with-type-constraints)
- [Example 10: Referencing Variables with `var.`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-10-referencing-variables-with-var)
- [Example 11: Variable Validation Rules](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-11-variable-validation-rules)
- [Example 12: The `.tfvars` File](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-12-the-tfvars-file)
- [Example 13: Output Values](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-13-output-values)
- [Example 14: Sensitive Outputs](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-14-sensitive-outputs)
- [Example 15: Data Sources](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-15-data-sources)
- [Example 16: Multiple Data Source Filters](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-16-multiple-data-source-filters)
- [Example 17: Local Values](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-17-local-values)
- [Example 18: String Interpolation](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-18-string-interpolation)
- [Example 19: `terraform plan` — The Dry-Run](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-19-terraform-plan--the-dry-run)
- [Example 20: `terraform apply` — Applying Changes](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-20-terraform-apply--applying-changes)
- [Example 21: `terraform destroy` — Tearing Down Resources](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-21-terraform-destroy--tearing-down-resources)
- [Example 22: Understanding Terraform State](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-22-understanding-terraform-state)
- [Example 23: Remote State Backend](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-23-remote-state-backend)
- [Example 24: Implicit vs Explicit Dependencies](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-24-implicit-vs-explicit-dependencies)
- [Example 25: Creating Multiple Resources with `count`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-25-creating-multiple-resources-with-count)
- [Example 26: `for_each` with a Map](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-26-for_each-with-a-map)
- [Example 27: Ansible Playbook Structure](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-27-ansible-playbook-structure)
- [Example 28: Ansible Inventory File](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/beginner#example-28-ansible-inventory-file)

### Intermediate (Examples 29–57)

- [Example 29: Local Module with Input Variables](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-29-local-module-with-input-variables)
- [Example 30: Module Output Values and Cross-Module Reference](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-30-module-output-values-and-cross-module-reference)
- [Example 31: Registry Module (Terraform Registry)](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-31-registry-module-terraform-registry)
- [Example 32: Remote Backend with S3 and DynamoDB State Locking](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-32-remote-backend-with-s3-and-dynamodb-state-locking)
- [Example 33: Terraform Workspaces for Environment Isolation](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-33-terraform-workspaces-for-environment-isolation)
- [Example 34: `terraform import` — Adopting Existing Resources](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-34-terraform-import--adopting-existing-resources)
- [Example 35: Data Sources for Cross-Stack References](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-35-data-sources-for-cross-stack-references)
- [Example 36: `dynamic` Blocks for Repeated Nested Structures](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-36-dynamic-blocks-for-repeated-nested-structures)
- [Example 37: Conditional Resources with `count`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-37-conditional-resources-with-count)
- [Example 38: Conditional Resources with `for_each`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-38-conditional-resources-with-for_each)
- [Example 39: `lifecycle` Rules — `prevent_destroy` and `ignore_changes`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-39-lifecycle-rules--prevent_destroy-and-ignore_changes)
- [Example 40: Variable Validation Rules](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-40-variable-validation-rules)
- [Example 41: `local-exec` Provisioner](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-41-local-exec-provisioner)
- [Example 42: `remote-exec` Provisioner](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-42-remote-exec-provisioner)
- [Example 43: `null_resource` with Triggers](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-43-null_resource-with-triggers)
- [Example 44: `terraform_remote_state` Data Source](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-44-terraform_remote_state-data-source)
- [Example 45: Role Structure and `ansible-galaxy init`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-45-role-structure-and-ansible-galaxy-init)
- [Example 46: Handlers and Notifications](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-46-handlers-and-notifications)
- [Example 47: Jinja2 Templates with `ansible.builtin.template`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-47-jinja2-templates-with-ansiblebuiltintemplate)
- [Example 48: Ansible Vault for Secrets Management](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-48-ansible-vault-for-secrets-management)
- [Example 49: Conditionals with `when`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-49-conditionals-with-when)
- [Example 50: Loops with `loop` and `with_items`](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-50-loops-with-loop-and-with_items)
- [Example 51: Directory Layout for Multi-Environment Terraform](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-51-directory-layout-for-multi-environment-terraform)
- [Example 52: Ansible Inventory for Multi-Environment Targets](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-52-ansible-inventory-for-multi-environment-targets)
- [Example 53: Ansible Conditionals with Registered Variables](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-53-ansible-conditionals-with-registered-variables)
- [Example 54: `for` Expressions and Collection Transformations](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-54-for-expressions-and-collection-transformations)
- [Example 55: `locals` for Expression Reuse and Complexity Management](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-55-locals-for-expression-reuse-and-complexity-management)
- [Example 56: `depends_on` for Explicit Dependency Management](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-56-depends_on-for-explicit-dependency-management)
- [Example 57: `moved` Block for State Refactoring Without Destroy](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/intermediate#example-57-moved-block-for-state-refactoring-without-destroy)

### Advanced (Examples 58–85)

- [Example 58: Multi-Cloud Provider Configuration](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-58-multi-cloud-provider-configuration)
- [Example 59: Terraform Cloud Workspace Configuration](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-59-terraform-cloud-workspace-configuration)
- [Example 60: Terraform Cloud Variable Sets](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-60-terraform-cloud-variable-sets)
- [Example 61: Sentinel Policy Enforcement](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-61-sentinel-policy-enforcement)
- [Example 62: OPA (Open Policy Agent) for Terraform](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-62-opa-open-policy-agent-for-terraform)
- [Example 63: Terraform Drift Detection](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-63-terraform-drift-detection)
- [Example 64: Terraform Import for Existing Resources](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-64-terraform-import-for-existing-resources)
- [Example 65: Blue-Green Infrastructure Deployment](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-65-blue-green-infrastructure-deployment)
- [Example 66: Immutable Infrastructure with AMI Baking](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-66-immutable-infrastructure-with-ami-baking)
- [Example 67: GitOps Workflow with Atlantis](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-67-gitops-workflow-with-atlantis)
- [Example 68: Infrastructure Pipeline with GitHub Actions](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-68-infrastructure-pipeline-with-github-actions)
- [Example 69: Terratest for Infrastructure Testing](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-69-terratest-for-infrastructure-testing)
- [Example 70: Module Contract Testing with Terraform Unit Tests](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-70-module-contract-testing-with-terraform-unit-tests)
- [Example 71: HashiCorp Vault Integration](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-71-hashicorp-vault-integration)
- [Example 72: Ansible Vault for Ansible Secrets](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-72-ansible-vault-for-ansible-secrets)
- [Example 73: Pulumi TypeScript Comparison](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-73-pulumi-typescript-comparison)
- [Example 74: Terraform CDK (CDKTF) Concept](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-74-terraform-cdk-cdktf-concept)
- [Example 75: Infrastructure Cost Estimation with Infracost](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-75-infrastructure-cost-estimation-with-infracost)
- [Example 76: Compliance as Code with SOPS](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-76-compliance-as-code-with-sops)
- [Example 77: Disaster Recovery Automation](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-77-disaster-recovery-automation)
- [Example 78: Auto-Scaling Infrastructure Patterns](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-78-auto-scaling-infrastructure-patterns)
- [Example 79: Infrastructure Dependency Graphs](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-79-infrastructure-dependency-graphs)
- [Example 80: State Migration Strategies](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-80-state-migration-strategies)
- [Example 81: Large-Scale Module Composition](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-81-large-scale-module-composition)
- [Example 82: Terraform Registry Module Versioning](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-82-terraform-registry-module-versioning)
- [Example 83: Ansible Dynamic Inventory](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-83-ansible-dynamic-inventory)
- [Example 84: Ansible Roles for Configuration Management](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-84-ansible-roles-for-configuration-management)
- [Example 85: IaC Architecture Decision Framework](/en/learn/software-engineering/infrastructure/infrastructure-as-code/by-example/advanced#example-85-iac-architecture-decision-framework)
