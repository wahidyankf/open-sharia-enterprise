---
title: "Overview"
weight: 10000000
date: 2026-04-29T00:00:00+07:00
draft: false
description: "Learn K3s through 85 annotated examples covering installation, workloads, networking, storage, GitOps, and production operations - ideal for experienced engineers"
tags: ["k3s", "kubernetes", "infrastructure", "by-example", "containers", "devops"]
---

This series teaches K3s — the lightweight Kubernetes distribution by Rancher — through heavily annotated, self-contained examples. Each example focuses on a single K3s or Kubernetes concept and includes inline annotations explaining what each command or manifest directive does, why it matters, and what cluster state results from it.

K3s packages a full Kubernetes control plane into a single ~70 MB binary. It uses containerd instead of Docker, embeds Traefik as an ingress controller, ships local-path-provisioner for storage, and bundles CoreDNS, Flannel CNI, and Klipper load balancer — all with zero external dependencies for a single-node install. This series covers K3s v1.35.4+k3s1 running Kubernetes v1.35.4.

## Series Structure

The examples are organized into three levels based on complexity:

- [Beginner](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner) —
  Installation, core workload types, networking basics, storage, health checks, and K3s-specific
  features like the auto-manifest directory and config file (Examples 1-28)
- [Intermediate](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate) —
  High availability setup, CNI replacement, HelmChart CRD, cert-manager TLS, MetalLB, RBAC,
  NetworkPolicy, autoscaling, Longhorn storage, and workload placement (Examples 29-57)
- [Advanced](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced) —
  GitOps with Flux CD, multi-cluster with Rancher, policy enforcement with OPA Gatekeeper,
  observability stacks, cluster backup/restore, custom operators, and production hardening
  (Examples 58-85)

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the K3s concept addresses and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of cluster topology, traffic flow, or component relationships (when appropriate)
3. **Heavily Annotated Code** — shell commands or YAML manifests with `# =>` comments documenting each step and its cluster-state effect
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## Prerequisites

These examples assume you have:

- A Linux host (Ubuntu 22.04 or Debian 12 recommended) with at least 1 CPU and 512 MB RAM
- `sudo` access for installation commands
- Familiarity with basic shell commands and YAML syntax
- Understanding of core Kubernetes concepts (Pod, Deployment, Service) at a conceptual level

## How to Use This Series

Each page presents annotated shell sessions and YAML manifests. Read the `# =>` annotations alongside
the commands to understand both the mechanics and the intent. The examples within each level build
progressively, so reading sequentially within a level gives the fullest understanding.

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: Install K3s Single-Node Server](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-1-install-k3s-single-node-server)
- [Example 2: Configure KUBECONFIG for Local kubectl Access](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-2-configure-kubeconfig-for-local-kubectl-access)
- [Example 3: Verify Installation — Nodes and Component Health](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-3-verify-installation--nodes-and-component-health)
- [Example 4: Deploy a First Pod](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-4-deploy-a-first-pod)
- [Example 5: Create a Deployment with Replicas](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-5-create-a-deployment-with-replicas)
- [Example 6: Expose a Deployment with a Service](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-6-expose-a-deployment-with-a-service)
- [Example 7: View Pod Logs](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-7-view-pod-logs)
- [Example 8: Execute into a Running Pod](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-8-execute-into-a-running-pod)
- [Example 9: Create and Use a Namespace](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-9-create-and-use-a-namespace)
- [Example 10: ConfigMaps — Create from Literal and from File](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-10-configmaps--create-from-literal-and-from-file)
- [Example 11: Secrets — Create and Use in Pods](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-11-secrets--create-and-use-in-pods)
- [Example 12: Apply a YAML Manifest](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-12-apply-a-yaml-manifest)
- [Example 13: K3s Node Token — Find and Use It](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-13-k3s-node-token--find-and-use-it)
- [Example 14: Add and Verify a Worker Node Join](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-14-add-and-verify-a-worker-node-join)
- [Example 15: Port-Forward to a Pod or Service](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-15-port-forward-to-a-pod-or-service)
- [Example 16: Resource Limits — CPU and Memory](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-16-resource-limits--cpu-and-memory)
- [Example 17: Basic Ingress with Traefik](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-17-basic-ingress-with-traefik)
- [Example 18: Traefik IngressRoute CRD](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-18-traefik-ingressroute-crd)
- [Example 19: View the Traefik Dashboard](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-19-view-the-traefik-dashboard)
- [Example 20: local-path-provisioner — PVC and Pod Volume](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-20-local-path-provisioner--pvc-and-pod-volume)
- [Example 21: StatefulSet with PVC Template](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-21-statefulset-with-pvc-template)
- [Example 22: Health Checks — Liveness, Readiness, and Startup Probes](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-22-health-checks--liveness-readiness-and-startup-probes)
- [Example 23: Rolling Update and Rollback](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-23-rolling-update-and-rollback)
- [Example 24: DaemonSet for Per-Node Agents](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-24-daemonset-for-per-node-agents)
- [Example 25: CronJob for Scheduled Tasks](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-25-cronjob-for-scheduled-tasks)
- [Example 26: K3s Auto-Manifest Deployment](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-26-k3s-auto-manifest-deployment)
- [Example 27: K3s Config File](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-27-k3s-config-file)
- [Example 28: Uninstall K3s Cleanly](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/beginner#example-28-uninstall-k3s-cleanly)

### Intermediate (Examples 29–57)

- [Example 29: HA K3s Cluster — Embedded etcd (3 Nodes)](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-29-ha-k3s-cluster--embedded-etcd-3-nodes)
- [Example 30: HA K3s with External PostgreSQL Datastore](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-30-ha-k3s-with-external-postgresql-datastore)
- [Example 31: Disable Default K3s Components](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-31-disable-default-k3s-components)
- [Example 32: Replace Flannel with Calico CNI](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-32-replace-flannel-with-calico-cni)
- [Example 33: Replace Flannel with Cilium CNI](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-33-replace-flannel-with-cilium-cni)
- [Example 34: Custom Cluster CIDR and Service CIDR](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-34-custom-cluster-cidr-and-service-cidr)
- [Example 35: HelmChart CRD — Deploy Applications via K3s Helm Controller](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-35-helmchart-crd--deploy-applications-via-k3s-helm-controller)
- [Example 36: HelmChartConfig CRD — Customize Helm Release Values](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-36-helmchartconfig-crd--customize-helm-release-values)
- [Example 37: Install cert-manager via HelmChart CRD](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-37-install-cert-manager-via-helmchart-crd)
- [Example 38: ClusterIssuer and Certificate with cert-manager](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-38-clusterissuer-and-certificate-with-cert-manager)
- [Example 39: Traefik IngressRoute with TLS Termination](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-39-traefik-ingressroute-with-tls-termination)
- [Example 40: Traefik Middleware — Headers, Rate Limiting](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-40-traefik-middleware--headers-rate-limiting)
- [Example 41: MetalLB for LoadBalancer Services on Bare Metal](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-41-metallb-for-loadbalancer-services-on-bare-metal)
- [Example 42: K3s Registries — Private Registry Mirrors](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-42-k3s-registries--private-registry-mirrors)
- [Example 43: Airgap Installation — Offline K3s Setup](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-43-airgap-installation--offline-k3s-setup)
- [Example 44: RBAC — ClusterRole, ClusterRoleBinding, ServiceAccount](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-44-rbac--clusterrole-clusterrolebinding-serviceaccount)
- [Example 45: NetworkPolicy — Restrict Pod-to-Pod Traffic](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-45-networkpolicy--restrict-pod-to-pod-traffic)
- [Example 46: PodDisruptionBudget — Availability During Maintenance](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-46-poddisruptionbudget--availability-during-maintenance)
- [Example 47: HorizontalPodAutoscaler — Scale on CPU and Memory](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-47-horizontalpodautoscaler--scale-on-cpu-and-memory)
- [Example 48: metrics-server — kubectl top Nodes and Pods](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-48-metrics-server--kubectl-top-nodes-and-pods)
- [Example 49: Longhorn Distributed Block Storage — Install and StorageClass](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-49-longhorn-distributed-block-storage--install-and-storageclass)
- [Example 50: Longhorn Backup to S3-Compatible Storage](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-50-longhorn-backup-to-s3-compatible-storage)
- [Example 51: Longhorn Volume Snapshots and Restore](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-51-longhorn-volume-snapshots-and-restore)
- [Example 52: kube-vip for HA LoadBalancer on Bare Metal](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-52-kube-vip-for-ha-loadbalancer-on-bare-metal)
- [Example 53: kube-vip LoadBalancer for Services](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-53-kube-vip-loadbalancer-for-services)
- [Example 54: Secrets Encryption at Rest](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-54-secrets-encryption-at-rest)
- [Example 55: Pod Security Admission — Enforce Restricted Mode](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-55-pod-security-admission--enforce-restricted-mode)
- [Example 56: Node Taints and Tolerations for Workload Placement](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-56-node-taints-and-tolerations-for-workload-placement)
- [Example 57: Node Affinity and Pod Anti-Affinity Rules](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/intermediate#example-57-node-affinity-and-pod-anti-affinity-rules)

### Advanced (Examples 58–85)

- [Example 58: GitOps with Flux CD v2 — Bootstrap on K3s](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-58-gitops-with-flux-cd-v2--bootstrap-on-k3s)
- [Example 59: Flux Kustomization — Sync a Git Repository](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-59-flux-kustomization--sync-a-git-repository)
- [Example 60: Flux HelmRelease — Manage Helm Releases via Git](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-60-flux-helmrelease--manage-helm-releases-via-git)
- [Example 61: Flux Image Automation — Auto-Update Deployments](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-61-flux-image-automation--auto-update-deployments)
- [Example 62: Multi-Cluster Management with Rancher v2.10](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-62-multi-cluster-management-with-rancher-v210)
- [Example 63: Rancher Projects and Namespaces — Tenant Isolation](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-63-rancher-projects-and-namespaces--tenant-isolation)
- [Example 64: Rancher Apps and Marketplace — Deploy Catalog Applications](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-64-rancher-apps-and-marketplace--deploy-catalog-applications)
- [Example 65: OPA Gatekeeper for Policy Enforcement](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-65-opa-gatekeeper-for-policy-enforcement)
- [Example 66: Falco for Runtime Security Monitoring](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-66-falco-for-runtime-security-monitoring)
- [Example 67: Velero for Cluster Backup and Restore](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-67-velero-for-cluster-backup-and-restore)
- [Example 68: Velero Scheduled Backup to S3](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-68-velero-scheduled-backup-to-s3)
- [Example 69: Prometheus and Grafana Stack via Helm](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-69-prometheus-and-grafana-stack-via-helm)
- [Example 70: Custom Prometheus Alerting Rules and Alertmanager](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-70-custom-prometheus-alerting-rules-and-alertmanager)
- [Example 71: Loki and Promtail for Log Aggregation](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-71-loki-and-promtail-for-log-aggregation)
- [Example 72: Distributed Tracing with Tempo and Grafana](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-72-distributed-tracing-with-tempo-and-grafana)
- [Example 73: Multi-Tenancy with vcluster](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-73-multi-tenancy-with-vcluster)
- [Example 74: KEDA — Event-Driven Autoscaling](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-74-keda--event-driven-autoscaling)
- [Example 75: Vertical Pod Autoscaler — Auto-Right-Size Resource Requests](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-75-vertical-pod-autoscaler--auto-right-size-resource-requests)
- [Example 76: Spegel — Peer-to-Peer Container Image Distribution](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-76-spegel--peer-to-peer-container-image-distribution)
- [Example 77: K3s Upgrade via system-upgrade-controller](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-77-k3s-upgrade-via-system-upgrade-controller)
- [Example 78: K3s Backup and Restore — etcd Snapshot](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-78-k3s-backup-and-restore--etcd-snapshot)
- [Example 79: HA Node Replacement in K3s Cluster](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-79-ha-node-replacement-in-k3s-cluster)
- [Example 80: Custom Admission Webhooks](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-80-custom-admission-webhooks)
- [Example 81: Custom Resource Definitions — Write a Simple Operator](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-81-custom-resource-definitions--write-a-simple-operator)
- [Example 82: Kaniko — In-Cluster Container Image Builds](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-82-kaniko--in-cluster-container-image-builds)
- [Example 83: Tekton Pipelines — CI/CD Inside K3s](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-83-tekton-pipelines--cicd-inside-k3s)
- [Example 84: K3s Security Hardening — CIS Kubernetes Benchmark](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-84-k3s-security-hardening--cis-kubernetes-benchmark)
- [Example 85: Production Readiness Checklist](/en/learn/software-engineering/infrastructure/tools/k3s/by-example/advanced#example-85-production-readiness-checklist)
