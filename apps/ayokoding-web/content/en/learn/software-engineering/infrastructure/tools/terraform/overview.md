---
title: Overview
weight: 100000
date: 2026-01-29T00:00:00+07:00
draft: false
description: Learn Terraform, the infrastructure as code tool for provisioning and managing cloud resources declaratively
---

**Terraform is an infrastructure as code (IaC) tool** that allows you to define, provision, and manage infrastructure across multiple cloud providers using declarative configuration files. It automates infrastructure lifecycle management with state tracking and dependency resolution.

## What Is Terraform

Terraform is an open-source tool developed by HashiCorp that uses the HashiCorp Configuration Language (HCL) to describe infrastructure resources. It translates declarative configurations into API calls to cloud providers, creating and managing resources safely and efficiently.

Key characteristics:

- **Declarative syntax** - Define desired state, Terraform determines execution plan
- **Multi-cloud** - Single tool for AWS, Azure, GCP, and 1000+ providers
- **State management** - Tracks current infrastructure state for drift detection
- **Dependency graph** - Automatically orders resource creation and updates
- **Immutable infrastructure** - Recreates resources rather than modifying in-place

## What You'll Learn

Through our Terraform tutorials, you'll master:

### Fundamentals

- HCL syntax: Resources, variables, outputs, data sources
- Providers: Configuring cloud provider authentication
- Resources: Creating infrastructure (VMs, networks, storage)
- State: Understanding terraform.tfstate and remote backends
- Commands: init, plan, apply, destroy, validate

### Production Patterns

- Modules: Reusable infrastructure components
- Workspaces: Managing multiple environments (dev, staging, prod)
- Remote state: S3, Azure Blob, Terraform Cloud backends
- Variables: Input validation, sensitive values, type constraints
- Outputs: Exposing values for other configurations

### Advanced Features

- Provisioners: Running scripts during resource creation
- Dynamic blocks: Programmatic configuration generation
- Count and for_each: Creating multiple similar resources
- Terraform Cloud: Collaboration, policy as code, cost estimation
- Import: Bringing existing infrastructure under Terraform management

### Administration

- State locking: Preventing concurrent modifications
- State migration: Moving state between backends
- Debugging: TF_LOG environment variable, verbose output
- Security: Secret management, least privilege IAM
- Testing: Terratest, Terraform validation, policy checks

## Learning Paths

### By-Example Tutorial (Code-First)

Learn Terraform through **80 annotated examples** covering 95% of the tool - ideal for DevOps engineers and cloud architects who prefer learning through working code rather than narrative explanations.

- **[Terraform By-Example](/en/learn/software-engineering/infrastructure/tools/terraform/by-example)** - Start here for rapid, hands-on learning

What you'll get:

- Self-contained, copy-paste-runnable configurations
- Heavy annotations showing plan/apply outputs and resource states
- Progressive complexity: Beginner (30 examples) → Intermediate (30 examples) → Advanced (20 examples)
- Production-ready patterns and best practices
- Mermaid diagrams for complex infrastructure

## Prerequisites and Getting Started

### Prerequisites

- Cloud provider account (AWS, Azure, or GCP free tier)
- Basic understanding of cloud infrastructure (VMs, networks, storage)
- Command-line familiarity
- HCL or JSON syntax knowledge (or willingness to learn)

No prior Terraform experience required - our tutorials start from installation and progress to advanced multi-cloud deployments.

### Quick Start

Get Terraform running locally:

```bash
# Install Terraform (Ubuntu/Debian)
wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/hashicorp.list
sudo apt update && sudo apt install terraform

# Install Terraform (macOS with Homebrew)
brew tap hashicorp/tap
brew install hashicorp/tap/terraform

# Verify installation
terraform version

# Create first configuration (main.tf)
cat > main.tf <<EOF
provider "aws" {
  region = "us-east-1"
}

resource "aws_s3_bucket" "example" {
  bucket = "my-terraform-example-bucket-12345"
}
EOF

# Initialize and apply
terraform init
terraform plan
terraform apply
```

Now you're ready to follow along with our by-example tutorials.

## Why Terraform

### When to Choose Terraform

Terraform excels in scenarios requiring:

- **Multi-cloud infrastructure** - Manage AWS, Azure, GCP from single tool
- **Infrastructure as code** - Version control infrastructure changes
- **Automation** - CI/CD integration for infrastructure deployments
- **Consistency** - Reproducible infrastructure across environments
- **Collaboration** - Team workflows with state locking and workspaces
- **Drift detection** - Identify manual changes outside Terraform

### Terraform vs Other Tools

- **vs AWS CloudFormation** - Terraform is multi-cloud; CloudFormation is AWS-only
- **vs Ansible** - Terraform provisions infrastructure; Ansible configures systems
- **vs Pulumi** - Terraform uses HCL; Pulumi uses general-purpose languages
- **vs AWS CDK** - Terraform is declarative; CDK generates CloudFormation imperatively

## Next Steps

Start your Terraform journey:

1. **[Initial Setup](/en/learn/software-engineering/infrastructure/tools/terraform/initial-setup)** - Install Terraform and configure providers
2. **[Quick Start](/en/learn/software-engineering/infrastructure/tools/terraform/quick-start)** - First resource and common patterns
3. **[Terraform By-Example Overview](/en/learn/software-engineering/infrastructure/tools/terraform/by-example/overview)** - Understand the by-example approach
4. **[Beginner Examples](/en/learn/software-engineering/infrastructure/tools/terraform/by-example/beginner)** - Master fundamentals (Examples 1-30)
5. **[Intermediate Examples](/en/learn/software-engineering/infrastructure/tools/terraform/by-example/intermediate)** - Production patterns (Examples 31-60)
6. **[Advanced Examples](/en/learn/software-engineering/infrastructure/tools/terraform/by-example/advanced)** - Expert mastery (Examples 61-80)

## Community and Resources

- [Official Terraform Documentation](https://www.terraform.io/docs)
- [Terraform Registry](https://registry.terraform.io/) - Providers and modules
- [Terraform GitHub](https://github.com/hashicorp/terraform)
- [HashiCorp Community](https://discuss.hashicorp.com/c/terraform-core/)
- [Stack Overflow Terraform Tag](https://stackoverflow.com/questions/tagged/terraform)
