---
title: "Overview"
date: 2025-12-29T23:43:13+07:00
draft: false
weight: 10000000
description: "Learn Docker containerization through 84 annotated code examples covering 95% of Docker - ideal for experienced developers"
tags: ["docker", "tutorial", "by-example", "examples", "code-first", "containers", "devops"]
---

## What is Docker By Example?

This tutorial teaches Docker containerization through **84 heavily annotated, runnable examples** that achieve 95% coverage of Docker's features needed for production work. Each example is self-contained, copy-paste-runnable, and extensively commented to show exactly what happens at each step.

## Who is This For?

**Target audience**: Experienced developers and DevOps engineers who prefer learning through working code rather than theoretical explanations.

**Prerequisites**:

- Familiarity with command-line interfaces
- Basic understanding of software development and deployment
- Experience with at least one programming language

**What you'll learn**:

- Dockerfile syntax and image building (multi-stage builds, layer optimization)
- Container lifecycle management (creation, execution, networking, volumes)
- Docker Compose orchestration (multi-container applications, service dependencies)
- Production patterns (health checks, resource limits, security best practices)
- Container registries and distribution (Docker Hub, private registries)
- CI/CD integration (automated builds, testing, deployment)

## How to Use This Tutorial

### Code-First Learning Philosophy

**Show the code first, run it second, understand through interaction.**

Each example follows this pattern:

1. **Brief explanation** (2-3 sentences) - What and why
2. **Diagram** (when helpful) - Visualize the concept
3. **Annotated code** - Heavily commented with `# =>` notation showing outputs and states
4. **Key takeaway** - The core insight distilled

### Example Annotation Pattern

All examples use `# =>` notation to show outputs, states, and intermediate values:

```dockerfile
# Base image selection
FROM node:18-alpine     # => Pulls official Node.js 18 image based on Alpine Linux (small footprint)

# Working directory setup
WORKDIR /app            # => Sets /app as the working directory inside container

# Dependency installation
COPY package*.json ./   # => Copies package files to /app/ (leverages layer caching)
RUN npm ci              # => Installs exact dependencies from package-lock.json
                        # => Creates a new layer with node_modules/

# Application code
COPY . .                # => Copies all source files to /app/

# Container configuration
EXPOSE 3000             # => Documents that container listens on port 3000 (informational only)
CMD ["node", "server.js"] # => Default command when container starts
```

### Self-Contained Examples

**Every example is copy-paste-runnable** within its chapter scope:

- **Beginner examples**: Completely standalone (no external dependencies)
- **Intermediate examples**: Assume basic Docker knowledge but include all necessary files
- **Advanced examples**: Assume fundamentals but remain fully runnable

You can copy any example and run it immediately without referring to previous examples.

## 95% Coverage: What It Means

### Included in 95%

**Core Docker features for production work**:

- Image building (Dockerfile syntax, multi-stage builds, layer optimization)
- Container lifecycle (run, exec, logs, inspect, stop, remove)
- Networking (bridge, host, overlay networks, service discovery)
- Data persistence (volumes, bind mounts, tmpfs)
- Docker Compose (service definitions, dependencies, environment variables)
- Security (user permissions, secrets, scanning, best practices)
- Resource management (CPU/memory limits, health checks, restart policies)
- Registry operations (push, pull, authentication, private registries)
- Production patterns (logging, monitoring, orchestration basics)

### Excluded from 95% (the remaining 5%)

**Specialized or rare scenarios**:

- Docker engine internals and architecture
- Kubernetes-specific features (covered in separate Kubernetes tutorials)
- Deprecated Docker features (Swarm mode details)
- Platform-specific advanced features (Windows containers specifics)
- Docker plugin development
- Low-level container runtime details (containerd, runc internals)

## Tutorial Structure

### Beginner (Examples 1-27) - 0-40% Coverage

**Focus**: Docker fundamentals and core workflow

**Topics**:

- Installation and hello world
- Dockerfile basics (FROM, RUN, COPY, CMD)
- Image management (build, list, tag, remove)
- Container basics (run, stop, remove, logs)
- Volumes and data persistence
- Basic networking
- Docker Compose introduction

**Example count**: 27 examples

### Intermediate (Examples 28-54) - 40-75% Coverage

**Focus**: Production patterns and service orchestration

**Topics**:

- Multi-stage builds for optimization
- Docker Compose services and dependencies
- Health checks and restart policies
- Resource limits (CPU, memory)
- Logging strategies and monitoring
- Environment variable management
- Networking modes and custom networks

**Example count**: 27 examples

### Advanced (Examples 55-84) - 75-95% Coverage

**Focus**: Production deployment and optimization

**Topics**:

- Docker Swarm basics (orchestration introduction)
- Security best practices (scanning, secrets, user permissions)
- Registry operations (Docker Hub, private registries, authentication)
- CI/CD integration (automated builds, testing pipelines)
- Production deployment patterns
- Performance optimization

**Example count**: 30 examples

## Color-Blind Friendly Diagrams

All diagrams use a **WCAG AA compliant color palette**:

- **Blue** (#0173B2) - Primary elements, starting states
- **Orange** (#DE8F05) - Processing states, intermediate steps
- **Teal** (#029E73) - Success states, outputs
- **Purple** (#CC78BC) - Alternative paths, options
- **Brown** (#CA9161) - Neutral elements, helpers

**Never**: Red, green, or yellow (not color-blind accessible)

## Navigation

**Structure**:

- [Beginner](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner) - Examples 1-27 (fundamentals)
- [Intermediate](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate) - Examples 28-54 (production patterns)
- [Advanced](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced) - Examples 55-84 (expert mastery)

## Getting Started

**System Requirements**:

- Docker Engine 20.10+ (Linux, macOS, Windows)
- Docker Compose 2.0+ (included with Docker Desktop)
- Terminal/command-line access
- 4GB RAM minimum, 8GB recommended

**Installation**:

- Linux: [Docker Engine installation](https://docs.docker.com/engine/install/)
- macOS/Windows: [Docker Desktop](https://www.docker.com/products/docker-desktop/)

**Verify installation**:

```bash
docker --version    # => Docker version 24.0.0 or higher
docker compose version  # => Docker Compose version v2.20.0 or higher
```

## How This Differs from Other Tutorials

**Traditional tutorials**: Explain concepts, then show small code snippets.

**This tutorial**: Shows complete, runnable examples first, with heavy annotations explaining what happens at each step. Every example can be copied and executed immediately.

**Coverage approach**: Achieves 95% feature coverage through systematic progression across 84 examples, ensuring you learn everything needed for production Docker work.

## Ready to Start?

Begin with [Beginner](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner) for Docker fundamentals, or jump to [Intermediate](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate) if you already know basic Dockerfile syntax and container management.

**Learning tip**: Run every example. Docker is best learned by doing, not reading. Each example is designed to be executed, examined, and modified.

## Examples by Level

### Beginner (Examples 1–27)

- [Example 1: Hello World](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-1-hello-world)
- [Example 2: Running Interactive Containers](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-2-running-interactive-containers)
- [Example 3: Simple Dockerfile](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-3-simple-dockerfile)
- [Example 4: Installing Dependencies in Dockerfile](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-4-installing-dependencies-in-dockerfile)
- [Example 5: ARG for Build-Time Variables](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-5-arg-for-build-time-variables)
- [Example 6: ENV for Runtime Variables](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-6-env-for-runtime-variables)
- [Example 7: LABEL for Image Metadata](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-7-label-for-image-metadata)
- [Example 8: Image Listing and Management](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-8-image-listing-and-management)
- [Example 9: Container Lifecycle Management](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-9-container-lifecycle-management)
- [Example 10: Container Logs and Inspection](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-10-container-logs-and-inspection)
- [Example 11: Executing Commands in Running Containers](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-11-executing-commands-in-running-containers)
- [Example 12: Container Port Mapping](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-12-container-port-mapping)
- [Example 13: Named Volumes for Data Persistence](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-13-named-volumes-for-data-persistence)
- [Example 14: Bind Mounts for Development](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-14-bind-mounts-for-development)
- [Example 15: tmpfs Mounts for Temporary Data](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-15-tmpfs-mounts-for-temporary-data)
- [Example 16: Bridge Network Basics](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-16-bridge-network-basics)
- [Example 17: Container to Container Communication](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-17-container-to-container-communication)
- [Example 18: Environment Variables from File](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-18-environment-variables-from-file)
- [Example 19: Docker Compose Basics](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-19-docker-compose-basics)
- [Example 20: Docker Compose with Build Context](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-20-docker-compose-with-build-context)
- [Example 21: Docker Compose Environment Variables](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-21-docker-compose-environment-variables)
- [Example 22: Docker Compose Volumes](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-22-docker-compose-volumes)
- [Example 23: Docker Compose Depends On](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-23-docker-compose-depends-on)
- [Example 24: Docker Compose Networks](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-24-docker-compose-networks)
- [Example 25: Docker Compose Restart Policies](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-25-docker-compose-restart-policies)
- [Example 26: Docker Compose Profiles](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-26-docker-compose-profiles)
- [Example 27: Docker Compose Override Files](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner#example-27-docker-compose-override-files)

### Intermediate (Examples 28–54)

- [Example 28: Multi-Stage Build Basics](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-28-multi-stage-build-basics)
- [Example 29: Multi-Stage with Build Arguments](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-29-multi-stage-with-build-arguments)
- [Example 30: Multi-Stage with Multiple Runtimes](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-30-multi-stage-with-multiple-runtimes)
- [Example 31: Build-Time Secrets](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-31-build-time-secrets)
- [Example 32: Docker Compose Build Optimization](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-32-docker-compose-build-optimization)
- [Example 33: Health Checks in Docker Compose](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-33-health-checks-in-docker-compose)
- [Example 34: Service Dependencies with Restart](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-34-service-dependencies-with-restart)
- [Example 35: Resource Limits - CPU](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-35-resource-limits---cpu)
- [Example 36: Resource Limits - Memory](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-36-resource-limits---memory)
- [Example 37: Combined Resource Limits](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-37-combined-resource-limits)
- [Example 38: Logging Drivers](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-38-logging-drivers)
- [Example 39: Structured Logging](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-39-structured-logging)
- [Example 40: Log Aggregation with EFK Stack](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-40-log-aggregation-with-efk-stack)
- [Example 41: Custom Bridge Networks](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-41-custom-bridge-networks)
- [Example 42: Network Troubleshooting Tools](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-42-network-troubleshooting-tools)
- [Example 43: Init Containers Pattern](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-43-init-containers-pattern)
- [Example 44: Shared Volumes for Data Exchange](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-44-shared-volumes-for-data-exchange)
- [Example 45: External Networks Integration](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-45-external-networks-integration)
- [Example 46: Running as Non-Root User](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-46-running-as-non-root-user)
- [Example 47: Environment Variable Management](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-47-environment-variable-management)
- [Example 48: Docker Compose Profiles for Conditional Services](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-48-docker-compose-profiles-for-conditional-services)
- [Example 49: Container Resource Monitoring](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-49-container-resource-monitoring)
- [Example 50: Docker System Prune and Cleanup](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-50-docker-system-prune-and-cleanup)
- [Example 51: Docker Events Monitoring](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-51-docker-events-monitoring)
- [Example 52: Docker Compose Watch for Development](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-52-docker-compose-watch-for-development)
- [Example 53: Blue-Green Deployment with Docker Compose](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-53-blue-green-deployment-with-docker-compose)
- [Example 54: Canary Deployment Pattern](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate#example-54-canary-deployment-pattern)

### Advanced (Examples 55–84)

- [Example 55: Docker Swarm Initialization](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-55-docker-swarm-initialization)
- [Example 56: Docker Swarm Services](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-56-docker-swarm-services)
- [Example 57: Docker Secrets Management](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-57-docker-secrets-management)
- [Example 58: Read-Only Root Filesystem](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-58-read-only-root-filesystem)
- [Example 59: Dropping Linux Capabilities](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-59-dropping-linux-capabilities)
- [Example 60: Image Scanning for Vulnerabilities](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-60-image-scanning-for-vulnerabilities)
- [Example 61: Distroless Images for Minimal Attack Surface](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-61-distroless-images-for-minimal-attack-surface)
- [Example 62: User Namespaces for Privilege Isolation](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-62-user-namespaces-for-privilege-isolation)
- [Example 63: Security Scanning in CI/CD Pipeline](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-63-security-scanning-in-cicd-pipeline)
- [Example 64: Private Docker Registry](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-64-private-docker-registry)
- [Example 65: CI/CD with GitHub Actions](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-65-cicd-with-github-actions)
- [Example 66: Docker Stack Deployment](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-66-docker-stack-deployment)
- [Example 67: Docker Swarm Service Constraints](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-67-docker-swarm-service-constraints)
- [Example 68: Docker Swarm Rolling Updates and Rollback](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-68-docker-swarm-rolling-updates-and-rollback)
- [Example 69: Docker Swarm Service Scaling](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-69-docker-swarm-service-scaling)
- [Example 70: Docker Swarm Rolling Updates Strategy](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-70-docker-swarm-rolling-updates-strategy)
- [Example 71: Docker Swarm Secrets Management](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-71-docker-swarm-secrets-management)
- [Example 72: Docker Swarm Overlay Network Configuration](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-72-docker-swarm-overlay-network-configuration)
- [Example 73: Docker Swarm Health Checks and Auto-Restart](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-73-docker-swarm-health-checks-and-auto-restart)
- [Example 74: Distributed Tracing with Jaeger](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-74-distributed-tracing-with-jaeger)
- [Example 75: AppArmor Security Profiles](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-75-apparmor-security-profiles)
- [Example 76: Seccomp Security Profiles](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-76-seccomp-security-profiles)
- [Example 77: Container Resource Quotas and Limits](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-77-container-resource-quotas-and-limits)
- [Example 78: Docker Registry Garbage Collection](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-78-docker-registry-garbage-collection)
- [Example 79: Docker BuildKit Advanced Features](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-79-docker-buildkit-advanced-features)
- [Example 80: High Availability Docker Registry](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-80-high-availability-docker-registry)
- [Example 81: Docker Notary (Image Signing Infrastructure)](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-81-docker-notary-image-signing-infrastructure)
- [Example 82: Docker Image Vulnerability Remediation](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-82-docker-image-vulnerability-remediation)
- [Example 83: Docker Resource Monitoring with cAdvisor and Prometheus](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-83-docker-resource-monitoring-with-cadvisor-and-prometheus)
- [Example 84: Docker Compose Production Deployment Best Practices](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced#example-84-docker-compose-production-deployment-best-practices)
