---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn core Terraform concepts and Infrastructure as Code patterns"
tags:
  - terraform
  - iac
  - infrastructure
  - devops
  - quick-start
---

Learn core Terraform concepts and Infrastructure as Code patterns. This Quick Start teaches essential Terraform skills.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- Resources and providers
- Variables and outputs
- State management
- Modules and best practices

## 📋 Prerequisites

- Terraform installed (see [Initial Setup](/en/learn/software-engineering/infrastructure/tools/terraform/tutorials/initial-setup))

## 📦 Resources and Providers

```hcl
terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region = "us-west-2"
}

resource "aws_instance" "web" {
  ami           = "ami-0c55b159cbfafe1f0"
  instance_type = "t2.micro"

  tags = {
    Name = "WebServer"
  }
}
```

## 🔧 Variables and Outputs

```hcl
variable "instance_type" {
  description = "EC2 instance type"
  type        = string
  default     = "t2.micro"
}

variable "environment" {
  description = "Environment name"
  type        = string
}

resource "aws_instance" "web" {
  ami           = "ami-0c55b159cbfafe1f0"
  instance_type = var.instance_type

  tags = {
    Name = "${var.environment}-web"
  }
}

output "instance_id" {
  value = aws_instance.web.id
}

output "public_ip" {
  value = aws_instance.web.public_ip
}
```

Apply with variables:

```bash
terraform apply -var="environment=dev"
```

## 💾 State Management

```hcl
terraform {
  backend "s3" {
    bucket = "my-terraform-state"
    key    = "prod/terraform.tfstate"
    region = "us-west-2"
  }
}
```

## 🧩 Modules

Create `modules/vpc/main.tf`:

```hcl
resource "aws_vpc" "main" {
  cidr_block = var.cidr_block

  tags = {
    Name = var.vpc_name
  }
}

variable "cidr_block" {
  type = string
}

variable "vpc_name" {
  type = string
}

output "vpc_id" {
  value = aws_vpc.main.id
}
```

Use the module:

```hcl
module "vpc" {
  source     = "./modules/vpc"
  cidr_block = "10.0.0.0/16"
  vpc_name   = "production-vpc"
}

output "vpc_id" {
  value = module.vpc.vpc_id
}
```

## ✅ Next Steps

You now understand Terraform essentials!

1. **Try the examples**: Create infrastructure configurations
2. **Explore By Example**: [Terraform By Example](/en/learn/software-engineering/infrastructure/tools/terraform/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Define resources and providers
- [ ] Use variables and outputs
- [ ] Manage state
- [ ] Create and use modules
