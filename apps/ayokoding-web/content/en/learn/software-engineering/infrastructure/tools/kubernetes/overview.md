---
title: Overview
weight: 100000
date: 2026-01-29T00:00:00+07:00
draft: false
description: Learn Kubernetes, the production-grade container orchestration platform for automated deployment, scaling, and management
---

**Kubernetes is a container orchestration platform** that automates deployment, scaling, and operation of containerized applications. It provides the infrastructure to run distributed systems resiliently, handling scaling, failover, and deployment patterns.

## What Is Kubernetes

Kubernetes (K8s) is an open-source container orchestration system originally developed by Google and now maintained by the Cloud Native Computing Foundation (CNCF). It manages containerized workloads and services across clusters of machines.

Key characteristics:

- **Declarative configuration** - Define desired state via YAML manifests
- **Self-healing** - Automatic container restarts, replacement, and rescheduling
- **Horizontal scaling** - Scale applications up/down based on demand
- **Service discovery** - Built-in DNS and load balancing
- **Rolling updates** - Zero-downtime deployments with rollback capability

## What You'll Learn

Through our Kubernetes tutorials, you'll master:

### Fundamentals

- Pods: Smallest deployable units, multi-container pods
- Deployments: Declarative application management, replica sets
- Services: Stable networking endpoints (ClusterIP, NodePort, LoadBalancer)
- ConfigMaps and Secrets: Configuration and sensitive data management
- Namespaces: Resource isolation and multi-tenancy

### Production Patterns

- StatefulSets: Stateful applications with stable identities
- DaemonSets: One pod per node for system services
- Jobs and CronJobs: Batch processing and scheduled tasks
- Ingress: HTTP/HTTPS routing and load balancing
- Persistent Volumes: Durable storage for stateful workloads

### Advanced Features

- Custom Resource Definitions (CRDs): Extending Kubernetes API
- Operators: Automated application management patterns
- Network policies: Pod-to-pod communication control
- Resource quotas and limits: Multi-tenant resource management
- RBAC: Role-based access control for security

### Administration

- Cluster setup: kubeadm, managed Kubernetes (EKS, GKE, AKS)
- Kubectl: Command-line tool mastery
- Helm: Package manager for Kubernetes applications
- Monitoring: Prometheus, Grafana integration
- Troubleshooting: Debugging pods, inspecting events, logs

## Learning Paths

### By-Example Tutorial (Code-First)

Learn Kubernetes through **85 annotated examples** covering 95% of the platform - ideal for DevOps engineers and platform engineers who prefer learning through working code rather than narrative explanations.

- **[Kubernetes By-Example](/en/learn/software-engineering/infrastructure/tools/kubernetes/by-example)** - Start here for rapid, hands-on learning

What you'll get:

- Self-contained, copy-paste-runnable manifests
- Heavy annotations showing kubectl outputs and cluster behaviors
- Progressive complexity: Beginner (30 examples) → Intermediate (30 examples) → Advanced (25 examples)
- Production-ready patterns and best practices
- Mermaid diagrams for complex architectures

## Prerequisites and Getting Started

### Prerequisites

- Docker fundamentals (containers, images, registries)
- Basic networking concepts (DNS, load balancing)
- YAML syntax familiarity
- Command-line comfort
- Understanding of distributed systems concepts

No prior Kubernetes experience required - our tutorials build from Docker basics to advanced orchestration.

### Quick Start

Get Kubernetes running locally:

```bash
# Install kubectl
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
sudo install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl

# Install minikube (local Kubernetes cluster)
curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
sudo install minikube-linux-amd64 /usr/local/bin/minikube

# Start cluster
minikube start

# Verify cluster
kubectl cluster-info
kubectl get nodes

# Run first pod
kubectl run nginx --image=nginx:latest
kubectl get pods
```

Now you're ready to follow along with our by-example tutorials.

## Why Kubernetes

### When to Choose Kubernetes

Kubernetes excels in scenarios requiring:

- **Microservices architecture** - Manage hundreds of independent services
- **Cloud-native applications** - Portable across cloud providers
- **High availability** - Automated failover and self-healing
- **Auto-scaling** - Dynamic scaling based on metrics
- **Complex deployments** - Canary, blue/green, A/B testing
- **Multi-tenancy** - Resource isolation for different teams/projects

### Kubernetes vs Other Tools

- **vs Docker Swarm** - Kubernetes is more feature-rich; Swarm is simpler
- **vs Nomad** - Kubernetes is container-focused; Nomad supports VMs and binaries
- **vs AWS ECS** - Kubernetes is cloud-agnostic; ECS is AWS-specific
- **vs Mesos** - Kubernetes is container-native; Mesos is general-purpose orchestrator

## Next Steps

Start your Kubernetes journey:

1. **[Initial Setup](/en/learn/software-engineering/infrastructure/tools/kubernetes/initial-setup)** - Install kubectl, minikube, and cluster
2. **[Quick Start](/en/learn/software-engineering/infrastructure/tools/kubernetes/quick-start)** - First deployment and common patterns
3. **[Kubernetes By-Example Overview](/en/learn/software-engineering/infrastructure/tools/kubernetes/by-example/overview)** - Understand the by-example approach
4. **[Beginner Examples](/en/learn/software-engineering/infrastructure/tools/kubernetes/by-example/beginner)** - Master fundamentals (Examples 1-30)
5. **[Intermediate Examples](/en/learn/software-engineering/infrastructure/tools/kubernetes/by-example/intermediate)** - Production patterns (Examples 31-60)
6. **[Advanced Examples](/en/learn/software-engineering/infrastructure/tools/kubernetes/by-example/advanced)** - Expert mastery (Examples 61-85)

## Community and Resources

- [Official Kubernetes Documentation](https://kubernetes.io/docs/)
- [Kubernetes GitHub](https://github.com/kubernetes/kubernetes)
- [CNCF (Cloud Native Computing Foundation)](https://www.cncf.io/)
- [Kubernetes Slack](https://slack.k8s.io/)
- [Stack Overflow Kubernetes Tag](https://stackoverflow.com/questions/tagged/kubernetes)
