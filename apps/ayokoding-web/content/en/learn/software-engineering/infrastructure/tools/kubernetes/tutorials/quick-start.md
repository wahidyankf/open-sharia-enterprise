---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn core Kubernetes concepts and orchestration patterns"
tags:
  - kubernetes
  - k8s
  - orchestration
  - devops
  - quick-start
---

Learn core Kubernetes concepts and orchestration patterns. This Quick Start teaches essential Kubernetes skills.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- Pods, Deployments, and Services
- ConfigMaps and Secrets
- Persistent Volumes
- Scaling and updates

## 📋 Prerequisites

- Kubernetes installed (see [Initial Setup](/en/learn/software-engineering/infrastructure/tools/kubernetes/tutorials/initial-setup))

## 📦 Pods and Deployments

Create `deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
  template:
    metadata:
      labels:
        app: myapp
    spec:
      containers:
        - name: myapp
          image: nginx:latest
          ports:
            - containerPort: 80
```

Apply:

```bash
kubectl apply -f deployment.yaml
kubectl get pods
kubectl get deployments
```

## 🌐 Services

Create `service.yaml`:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: myapp-service
spec:
  selector:
    app: myapp
  ports:
    - protocol: TCP
      port: 80
      targetPort: 80
  type: LoadBalancer
```

Apply:

```bash
kubectl apply -f service.yaml
kubectl get services
```

## 🔐 ConfigMaps and Secrets

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: app-config
data:
  DATABASE_URL: "postgres://db:5432/myapp"
  LOG_LEVEL: "info"
---
apiVersion: v1
kind: Secret
metadata:
  name: app-secret
type: Opaque
stringData:
  password: "secret123"
```

## 📈 Scaling

```bash
kubectl scale deployment myapp --replicas=5
kubectl get pods
```

## ✅ Next Steps

You now understand Kubernetes essentials!

1. **Try the examples**: Create and manage resources
2. **Explore By Example**: [Kubernetes By Example](/en/learn/software-engineering/infrastructure/tools/kubernetes/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Create Deployments and Pods
- [ ] Expose applications with Services
- [ ] Use ConfigMaps and Secrets
- [ ] Scale applications
