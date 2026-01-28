---
title: Overview
weight: 100000
date: 2025-12-29T00:00:00+07:00
draft: false
description: Master essential infrastructure tools for automation, containerization, orchestration, and infrastructure as code
---

Infrastructure tools automate the deployment, configuration, and management of systems and applications. This section covers the essential tools for modern infrastructure management.

## What You'll Learn

- **Infrastructure as Code** - Terraform for declarative infrastructure provisioning
- **Configuration Management** - Ansible for automating system configuration
- **Containerization** - Docker for application packaging and deployment
- **Orchestration** - Kubernetes for container management at scale

## Tool Categories

### Infrastructure as Code (IaC)

**[Terraform](/en/learn/software-engineering/infrastructure/tools/terraform)** - Declarative infrastructure provisioning across cloud providers

- Define infrastructure as code using HCL
- Multi-cloud support (AWS, Azure, GCP, and more)
- State management and dependency resolution
- Plan and apply changes safely
- Module system for reusable infrastructure components

### Configuration Management

**[Ansible](/en/learn/software-engineering/infrastructure/tools/ansible)** - Agentless automation for configuration and deployment

- YAML-based playbooks for task automation
- Idempotent operations ensure consistent state
- No agents required on managed nodes
- Extensive module library for common tasks
- Role-based organization for reusability

### Containerization

**[Docker](/en/learn/software-engineering/infrastructure/tools/docker)** - Platform for building, shipping, and running containers

- Package applications with dependencies
- Consistent environments from development to production
- Efficient resource utilization via containers
- Image layering and caching for fast builds
- Docker Compose for multi-container applications

### Container Orchestration

**[Kubernetes](/en/learn/software-engineering/infrastructure/tools/kubernetes)** - Production-grade container orchestration

- Automated deployment and scaling
- Self-healing and rollback capabilities
- Service discovery and load balancing
- Storage orchestration and configuration management
- Declarative configuration with YAML manifests

## Learning Approach

Each tool follows the **By Example** learning path:

- **Beginner** - Core concepts, basic usage, and common patterns
- **Intermediate** - Production workflows, best practices, and integration
- **Advanced** - Complex scenarios, optimization, and advanced patterns

Learn through practical, annotated examples that you can run immediately.

## Tool Integration

These tools work together in modern infrastructure:

1. **Terraform** provisions cloud infrastructure (VMs, networks, storage)
2. **Ansible** configures systems and deploys applications
3. **Docker** packages applications as portable containers
4. **Kubernetes** orchestrates containers at scale

**Typical workflow**: Terraform creates infrastructure → Ansible configures systems → Docker builds images → Kubernetes deploys and manages containers.

## When to Use Each Tool

**Use Terraform when**:

- Provisioning cloud resources (VMs, databases, networks)
- Managing infrastructure across multiple cloud providers
- Need declarative infrastructure definition
- Require state management and drift detection

**Use Ansible when**:

- Configuring existing systems
- Deploying applications to servers
- Running ad-hoc commands across infrastructure
- Need simple, agentless automation

**Use Docker when**:

- Packaging applications with dependencies
- Ensuring environment consistency
- Developing locally with production-like setup
- Creating portable, shareable application images

**Use Kubernetes when**:

- Running containerized applications at scale
- Need automated scaling and self-healing
- Managing complex multi-container applications
- Require service discovery and load balancing

## Getting Started

Start with the tool that matches your immediate need:

- **Infrastructure provisioning** → [Terraform](/en/learn/software-engineering/infrastructure/tools/terraform)
- **System configuration** → [Ansible](/en/learn/software-engineering/infrastructure/tools/ansible)
- **Application packaging** → [Docker](/en/learn/software-engineering/infrastructure/tools/docker)
- **Container orchestration** → [Kubernetes](/en/learn/software-engineering/infrastructure/tools/kubernetes)

Each tool includes comprehensive By Example tutorials with practical, runnable code.
