---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install Kubernetes and deploy your first application"
tags:
  - kubernetes
  - k8s
  - orchestration
  - devops
  - installation
---

Get Kubernetes installed and deploy your first application. This guide walks you through local Kubernetes setup and your first deployment.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ Kubernetes running locally
- ✅ kubectl CLI configured
- ✅ Your first pod deployed

## 📋 Prerequisites

- Docker installed
- Basic command line familiarity
- 4GB RAM minimum

## 💾 Step 1: Install Kubernetes

### Using Minikube (Recommended for Learning)

```bash
# macOS
brew install minikube

# Linux
curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
sudo install minikube-linux-amd64 /usr/local/bin/minikube

# Windows
choco install minikube
```

### Using Docker Desktop

Enable Kubernetes in Docker Desktop Settings.

### Install kubectl

```bash
# macOS
brew install kubectl

# Linux
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
sudo install kubectl /usr/local/bin/kubectl

# Windows
choco install kubernetes-cli
```

## 🚀 Step 2: Start Kubernetes

```bash
minikube start
kubectl cluster-info
kubectl get nodes
```

## 📦 Step 3: Deploy Your First Application

```bash
kubectl create deployment nginx --image=nginx
kubectl get deployments
kubectl get pods
```

## 🌐 Step 4: Expose the Application

```bash
kubectl expose deployment nginx --type=NodePort --port=80
kubectl get services
minikube service nginx --url
```

Visit the URL shown to see nginx running.

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] `kubectl version` shows client and server versions
- [ ] `kubectl get nodes` shows nodes ready
- [ ] Deployment created and pod running
- [ ] Service accessible via browser

## 🎉 You're Done

You've successfully set up Kubernetes and deployed your first application.

## 📚 What's Next?

**Quick learner**: [Kubernetes Quick Start](/en/learn/software-engineering/infrastructure/tools/kubernetes/tutorials/quick-start)

**Code-first learner**: [Kubernetes By Example](/en/learn/software-engineering/infrastructure/tools/kubernetes/tutorials/by-example)
