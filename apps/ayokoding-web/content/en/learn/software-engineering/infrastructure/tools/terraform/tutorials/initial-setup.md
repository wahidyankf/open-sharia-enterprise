---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Terraform and create your first infrastructure"
tags:
  - terraform
  - iac
  - infrastructure
  - devops
  - installation
---

Get Terraform installed and create your first infrastructure. This guide walks you through installation and provisioning your first resources.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Terraform installed and verified
- ✅ Your first Terraform configuration
- ✅ Infrastructure provisioned and destroyed

## 📋 Prerequisites

- Windows, macOS, or Linux
- Cloud account (AWS, Azure, or GCP) for deployment examples
- Basic command line familiarity

## 💾 Step 1: Install Terraform

### macOS

```bash
brew tap hashicorp/tap
brew install hashicorp/tap/terraform
```

### Linux

```bash
wget -O- https://apt.releases.hashicorp.com/gpg | sudo gpg --dearmor -o /usr/share/keyrings/hashicorp-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/hashicorp-archive-keyring.gpg] https://apt.releases.hashicorp.com $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/hashicorp.list
sudo apt update && sudo apt install terraform
```

### Windows

```bash
choco install terraform
```

### Verify Installation

```bash
terraform --version
```

## 🚀 Step 2: Create Your First Configuration

Create `main.tf`:

```hcl
terraform {
  required_providers {
    local = {
      source  = "hashicorp/local"
      version = "~> 2.0"
    }
  }
}

resource "local_file" "hello" {
  content  = "Hello, Terraform!"
  filename = "${path.module}/hello.txt"
}

output "file_content" {
  value = local_file.hello.content
}
```

## 📊 Step 3: Initialize and Apply

```bash
terraform init
terraform plan
terraform apply
```

Type `yes` when prompted.

Check the created file:

```bash
cat hello.txt
```

## 🧹 Step 4: Destroy Infrastructure

```bash
terraform destroy
```

Type `yes` when prompted.

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] `terraform --version` shows Terraform installed
- [ ] `terraform init` completes successfully
- [ ] `terraform apply` creates resources
- [ ] `terraform destroy` removes resources

## 🎉 You're Done

You've successfully installed Terraform and provisioned your first infrastructure.

## 📚 What's Next?

**Quick learner**: [Terraform Quick Start](/en/learn/software-engineering/infrastructure/tools/terraform/tutorials/quick-start)

**Code-first learner**: [Terraform By Example](/en/learn/software-engineering/infrastructure/tools/terraform/tutorials/by-example)
