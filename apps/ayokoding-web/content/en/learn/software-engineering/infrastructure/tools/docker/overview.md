---
title: Overview
weight: 100000
date: 2026-01-29T00:00:00+07:00
draft: false
description: Learn Docker, the platform for building, shipping, and running containerized applications
---

**Docker is a containerization platform** that packages applications and their dependencies into lightweight, portable containers. It enables consistent environments from development to production, simplifies deployment, and optimizes resource utilization.

## What Is Docker

Docker is an open-source platform that uses OS-level virtualization to deliver software in containers. Containers bundle application code with libraries and dependencies, ensuring the application runs reliably across different computing environments.

Key characteristics:

- **Containerization** - Lightweight isolation using Linux kernel features (cgroups, namespaces)
- **Portability** - Containers run consistently on any system with Docker
- **Efficiency** - Shares host OS kernel, minimal overhead compared to VMs
- **Image layering** - Reusable layers with copy-on-write filesystem
- **Docker Hub** - Public registry with millions of pre-built images

## What You'll Learn

Through our Docker tutorials, you'll master:

### Fundamentals

- Images: Building, tagging, pushing to registries
- Containers: Running, stopping, inspecting, logs, exec
- Dockerfile: Instructions for building images (FROM, RUN, COPY, CMD, ENTRYPOINT)
- Volumes: Persistent data storage across container restarts
- Networks: Container networking (bridge, host, overlay)

### Production Patterns

- Multi-stage builds: Optimized image sizes
- Docker Compose: Multi-container application definition
- Health checks: Container health monitoring
- Resource limits: CPU, memory constraints
- Security: User privileges, secrets management, image scanning

### Advanced Features

- Docker Swarm: Native container orchestration
- BuildKit: Enhanced build performance and caching
- Registry management: Private Docker registries
- CI/CD integration: Automated image builds and deployments
- Logging drivers: Centralized log management

### Administration

- Image optimization: Layer caching, minimal base images
- Networking deep dive: Custom networks, DNS, service discovery
- Storage drivers: Performance tuning for different workloads
- Monitoring: Container metrics, resource usage
- Troubleshooting: Debugging containers, inspecting layers

## Learning Paths

### By-Example Tutorial (Code-First)

Learn Docker through **85 annotated examples** covering 95% of the platform - ideal for developers and DevOps engineers who prefer learning through working code rather than narrative explanations.

- **[Docker By-Example](/en/learn/software-engineering/infrastructure/tools/docker/by-example)** - Start here for rapid, hands-on learning

What you'll get:

- Self-contained, copy-paste-runnable examples
- Heavy annotations showing build outputs and runtime behaviors
- Progressive complexity: Beginner (30 examples) → Intermediate (30 examples) → Advanced (25 examples)
- Production-ready patterns and best practices
- Mermaid diagrams for complex concepts

## Prerequisites and Getting Started

### Prerequisites

- Basic Linux/Unix command line knowledge
- Understanding of application deployment concepts
- Familiarity with package managers and dependencies
- Docker installed (Docker Desktop or Docker Engine)

No prior Docker experience required - our tutorials start from fundamentals and progress to advanced containerization.

### Quick Start

Get Docker running locally:

```bash
# Install Docker (Ubuntu/Debian)
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Install Docker Desktop (macOS/Windows)
# Download from https://www.docker.com/products/docker-desktop

# Verify installation
docker --version
docker run hello-world

# Run first container
docker run -it ubuntu:22.04 bash
```

Now you're ready to follow along with our by-example tutorials.

## Why Docker

### When to Choose Docker

Docker excels in scenarios requiring:

- **Consistent environments** - Identical development, staging, and production
- **Microservices** - Isolated, independently deployable services
- **CI/CD pipelines** - Fast, reliable builds and deployments
- **Resource efficiency** - Run more applications on same hardware
- **Dependency isolation** - No conflicts between application dependencies
- **Rapid deployment** - Start containers in seconds

### Docker vs Other Tools

- **vs Virtual Machines** - Docker is lighter and faster; VMs provide stronger isolation
- **vs Kubernetes** - Docker is container runtime; Kubernetes orchestrates containers at scale
- **vs Podman** - Podman is daemonless and rootless; Docker has broader ecosystem
- **vs LXC** - Docker provides higher-level abstractions; LXC is lower-level containerization

## Next Steps

Start your Docker journey:

1. **[Initial Setup](/en/learn/software-engineering/infrastructure/tools/docker/initial-setup)** - Install and configure Docker
2. **[Quick Start](/en/learn/software-engineering/infrastructure/tools/docker/quick-start)** - First container and common patterns
3. **[Docker By-Example Overview](/en/learn/software-engineering/infrastructure/tools/docker/by-example/overview)** - Understand the by-example approach
4. **[Beginner Examples](/en/learn/software-engineering/infrastructure/tools/docker/by-example/beginner)** - Master fundamentals (Examples 1-30)
5. **[Intermediate Examples](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate)** - Production patterns (Examples 31-60)
6. **[Advanced Examples](/en/learn/software-engineering/infrastructure/tools/docker/by-example/advanced)** - Expert mastery (Examples 61-85)

## Community and Resources

- [Official Docker Documentation](https://docs.docker.com/)
- [Docker Hub](https://hub.docker.com/) - Public container image registry
- [Docker GitHub](https://github.com/docker)
- [Docker Community](https://www.docker.com/community)
- [Stack Overflow Docker Tag](https://stackoverflow.com/questions/tagged/docker)
