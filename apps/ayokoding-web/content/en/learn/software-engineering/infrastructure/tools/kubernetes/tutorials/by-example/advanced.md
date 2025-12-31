---
title: "Advanced"
date: 2025-12-30T00:00:43+07:00
draft: false
weight: 10000003
description: "Examples 58-85: Expert patterns including RBAC, NetworkPolicies, Custom Resources, Operators, Helm, GitOps with ArgoCD, and production best practices (75-95% coverage)"
tags:
  ["kubernetes", "tutorial", "by-example", "advanced", "rbac", "security", "operators", "helm", "gitops", "production"]
---

## Advanced Level Overview

This level covers **expert mastery and production optimization** through 28 examples, achieving **75-95% coverage** of production Kubernetes knowledge. Each example demonstrates enterprise patterns needed for secure, scalable, and maintainable cluster operations.

**What you'll learn**:

- RBAC and Security (authentication, authorization, SecurityContext, PodSecurityPolicies)
- Network Policies (pod-to-pod traffic control, namespace isolation)
- Custom Resources and Operators (extending Kubernetes, custom controllers)
- Helm Charts (package management, templating, releases)
- GitOps and ArgoCD (declarative deployment automation)
- Production Patterns (monitoring, logging, debugging, performance)

**Prerequisites**: Completion of Beginner (1-28) and Intermediate (29-57) examples or equivalent production Kubernetes experience

---

## RBAC & Security (Examples 58-63)

### Example 58: ServiceAccount and RBAC Basics

ServiceAccounts provide identities for Pods while RBAC (Role-Based Access Control) controls permissions. Roles define permissions, RoleBindings grant permissions to ServiceAccounts.

```mermaid
%% RBAC authorization flow
graph TD
    A[Pod with ServiceAccount] --> B[API Request]
    B --> C{RBAC Check}
    C --> D[Role: pod-reader]
    D --> E{Has Permission?}
    E -->|Yes| F[Allow: list pods]
    E -->|No| G[Deny: 403 Forbidden]

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style D fill:#CA9161,color:#000
    style F fill:#029E73,color:#fff
    style G fill:#CC78BC,color:#000
```

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: pod-reader-sa # => ServiceAccount name
  namespace: default # => Namespace-scoped resource

---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: pod-reader # => Role name (namespace-scoped)
  namespace: default
rules:
  - apiGroups: [""] # => Core API group (v1)
    resources: ["pods"] # => Resources: pods
    verbs:
      ["get", "list", "watch"] # => Allowed operations
      # => get: retrieve single Pod
      # => list: list all Pods
      # => watch: watch for changes

---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: read-pods-binding # => RoleBinding name
  namespace: default
subjects:
  - kind: ServiceAccount
    name: pod-reader-sa # => Grants permissions to this ServiceAccount
    namespace: default
roleRef:
  kind: Role
  name: pod-reader # => References Role above
  apiGroup: rbac.authorization.k8s.io

---
apiVersion: v1
kind: Pod
metadata:
  name: rbac-pod
spec:
  serviceAccountName:
    pod-reader-sa # => Uses ServiceAccount
    # => Default: "default" ServiceAccount
  containers:
    - name: kubectl
      image: bitnami/kubectl:latest
      command:
        - sh
        - -c
        - |
          kubectl get pods              # => Allowed (list pods)
          kubectl get services          # => Denied (no permission)
          sleep 3600

# RBAC verification:
# => kubectl auth can-i list pods --as=system:serviceaccount:default:pod-reader-sa
# => yes
# => kubectl auth can-i list services --as=system:serviceaccount:default:pod-reader-sa
# => no
```

**Key Takeaway**: Use ServiceAccounts for Pod identity and RBAC for fine-grained permission control; follow principle of least privilege by granting only required permissions; prefer namespace-scoped Roles over cluster-wide ClusterRoles when possible.

---

### Example 59: ClusterRole and ClusterRoleBinding

ClusterRoles define cluster-wide permissions for non-namespaced resources (nodes, PersistentVolumes) or cross-namespace access. ClusterRoleBindings grant cluster-level permissions.

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name:
    node-reader # => ClusterRole name (cluster-scoped)
    # => No namespace field
rules:
  - apiGroups: [""]
    resources: ["nodes"] # => Nodes are cluster-scoped resources
    verbs: ["get", "list", "watch"]

  - apiGroups: [""]
    resources: ["persistentvolumes"] # => PVs are cluster-scoped
    verbs: ["get", "list"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: read-nodes-global # => ClusterRoleBinding name
subjects:
  - kind: ServiceAccount
    name: cluster-reader-sa
    namespace: kube-system # => ServiceAccount in kube-system namespace
roleRef:
  kind: ClusterRole
  name: node-reader # => References ClusterRole
  apiGroup: rbac.authorization.k8s.io

# ClusterRole vs Role:
# => Role: namespace-scoped permissions (pods, services, configmaps in one namespace)
# => ClusterRole: cluster-wide permissions (nodes, PVs, all namespaces)
# => ClusterRole + RoleBinding: cluster role limited to one namespace
# => ClusterRole + ClusterRoleBinding: cluster role across all namespaces

# Common use cases:
# => Monitoring: read Pods across all namespaces
# => Node management: manage nodes (cluster-scoped)
# => PV administration: manage PersistentVolumes (cluster-scoped)
```

**Key Takeaway**: Use ClusterRoles for cluster-wide resources like nodes and PersistentVolumes or cross-namespace access; combine ClusterRole with RoleBinding to limit cluster-wide permissions to specific namespaces.

---

### Example 60: Aggregated ClusterRoles

Aggregated ClusterRoles combine permissions from multiple ClusterRoles using label selectors, enabling modular permission management and extending built-in roles.

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: monitoring-aggregated # => Aggregated ClusterRole
  labels:
    rbac.example.com/aggregate-to-monitoring: "true"
aggregationRule:
  clusterRoleSelectors:
    - matchLabels:
        rbac.example.com/aggregate-to-monitoring:
          "true"
          # => Combines all ClusterRoles with this label
rules: [] # => Auto-populated from aggregated ClusterRoles

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: monitoring-pods
  labels:
    rbac.example.com/aggregate-to-monitoring:
      "true"
      # => Matched by aggregationRule above
rules:
  - apiGroups: [""]
    resources: ["pods", "pods/log"]
    verbs: ["get", "list", "watch"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: monitoring-metrics
  labels:
    rbac.example.com/aggregate-to-monitoring: "true"
rules:
  - apiGroups: ["metrics.k8s.io"]
    resources: ["pods", "nodes"]
    verbs: ["get", "list"]

# Aggregation result:
# => monitoring-aggregated automatically includes:
#    - pods, pods/log (get, list, watch)
#    - metrics.k8s.io/pods, metrics.k8s.io/nodes (get, list)
# => Add new ClusterRole with matching label → auto-aggregated
# => Useful for extensible permission systems
```

**Key Takeaway**: Use aggregated ClusterRoles for modular permission management; add new permissions by creating ClusterRoles with matching labels; built-in roles like admin, edit, view use aggregation for extensibility.

---

### Example 61: SecurityContext and Pod Security

SecurityContext controls security settings at Pod and container level, including user/group IDs, privilege escalation, filesystem access, and capabilities.

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: security-context-pod
spec:
  securityContext: # => Pod-level security settings
    runAsUser: 1000 # => Run as user ID 1000 (non-root)
    runAsGroup: 3000 # => Run as group ID 3000
    fsGroup:
      2000 # => Filesystem group for volumes
      # => Files owned by group 2000
    fsGroupChangePolicy:
      OnRootMismatch
      # => Change ownership only if needed
  containers:
    - name: secure-app
      image: nginx:1.24
      securityContext: # => Container-level (overrides Pod-level)
        allowPrivilegeEscalation:
          false
          # => Prevents gaining more privileges
        runAsNonRoot: true # => Ensures non-root user
        readOnlyRootFilesystem:
          true # => Filesystem is read-only
          # => Use volumes for writable paths
        capabilities:
          drop:
            - ALL # => Drop all Linux capabilities
          add:
            - NET_BIND_SERVICE # => Add only required capabilities
              # => Bind to ports < 1024

      volumeMounts:
        - name: cache
          mountPath: /var/cache/nginx # => Writable volume for cache

  volumes:
    - name: cache
      emptyDir: {} # => Ephemeral writable storage


# Security best practices:
# => runAsNonRoot: true (prevent root exploits)
# => readOnlyRootFilesystem: true (minimize attack surface)
# => allowPrivilegeEscalation: false (prevent privilege escalation)
# => Drop ALL capabilities, add only required (least privilege)
```

**Key Takeaway**: Always run containers as non-root with read-only root filesystem; drop all capabilities and add only required ones; use SecurityContext to enforce defense-in-depth security practices.

---

### Example 62: PodSecurityPolicy (Deprecated)

PodSecurityPolicy (PSP) enforces cluster-wide security standards for Pods. PSP is deprecated in Kubernetes 1.21+ and removed in 1.25+, replaced by Pod Security Admission.

```yaml
# NOTE: PSP deprecated, use Pod Security Standards instead
# Shown for historical context and migration understanding

apiVersion: policy/v1beta1
kind: PodSecurityPolicy
metadata:
  name: restricted # => PSP name
spec:
  privileged: false # => Disallow privileged containers
  allowPrivilegeEscalation: false # => Prevent privilege escalation
  requiredDropCapabilities:
    - ALL # => Require dropping all capabilities
  volumes:
    - configMap
    - emptyDir
    - projected
    - secret
    - downwardAPI
    - persistentVolumeClaim # => Allowed volume types
  runAsUser:
    rule: MustRunAsNonRoot # => Enforce non-root user
  seLinux:
    rule: RunAsAny
  fsGroup:
    rule: RunAsAny
  readOnlyRootFilesystem: true # => Require read-only root filesystem

---
# Migration to Pod Security Standards (PSS)
# 1. Label namespaces with Pod Security levels
# => kubectl label namespace default pod-security.kubernetes.io/enforce=restricted
# => Levels: privileged, baseline, restricted

# 2. Pod Security Admission enforces policies
# => No PSP or RBAC setup needed
# => Built-in controller validates Pods

# 3. Restricted level enforces:
# => runAsNonRoot: true
# => allowPrivilegeEscalation: false
# => Drop ALL capabilities
# => Read-only root filesystem
```

**Key Takeaway**: Migrate from deprecated PodSecurityPolicy to Pod Security Standards by labeling namespaces with enforcement levels (privileged, baseline, restricted); Pod Security Admission provides simpler cluster-wide security enforcement.

---

### Example 63: Secrets Encryption at Rest

Kubernetes Secrets are base64-encoded by default (not encrypted). Enable encryption at rest using EncryptionConfiguration to protect sensitive data in etcd.

```yaml
# File: /etc/kubernetes/encryption-config.yaml
apiVersion: apiserver.config.k8s.io/v1
kind: EncryptionConfiguration
resources:
  - resources:
      - secrets # => Encrypt Secrets
      - configmaps # => Optionally encrypt ConfigMaps
    providers:
      - aescbc: # => AES-CBC encryption
          keys:
            - name: key1
              secret:
                <base64-encoded-32-byte-key>
                # => Generate: head -c 32 /dev/urandom | base64
      - identity: {} # => Fallback for unencrypted data


# Apply encryption:
# 1. Create encryption config file
# 2. Update kube-apiserver manifest:
#    --encryption-provider-config=/etc/kubernetes/encryption-config.yaml
# 3. Restart kube-apiserver
# 4. Encrypt existing Secrets:
#    kubectl get secrets --all-namespaces -o json | kubectl replace -f -

# Encryption providers (ordered by preference):
# => aescbc: AES-CBC with PKCS#7 padding (default)
# => aesgcm: AES-GCM (faster, requires unique nonce)
# => secretbox: XSalsa20-Poly1305 (external library)
# => kms: External Key Management Service (AWS KMS, GCP KMS, Azure Key Vault)
# => identity: No encryption (plaintext)

# KMS provider example (AWS):
# providers:
# - kms:
#     name: aws-encryption-provider
#     endpoint: unix:///var/run/kmsplugin/socket.sock
#     cachesize: 1000
# - identity: {}
```

**Key Takeaway**: Enable encryption at rest for production clusters storing sensitive data; use KMS providers (AWS KMS, GCP KMS) for enterprise key management; rotate encryption keys periodically following security policies.

---

## Network Policies (Examples 64-68)

### Example 64: Basic Network Policy

NetworkPolicies control pod-to-pod traffic using label selectors and rules. By default, all traffic is allowed; NetworkPolicies enforce restrictions.

```mermaid
%% NetworkPolicy traffic control
graph TD
    A[Pod: frontend<br/>app=frontend] -->|Allowed| B[Pod: backend<br/>app=backend]
    C[Pod: external<br/>app=external] -->|Denied| B
    D[Pod: database<br/>app=database] -->|Allowed| B

    style A fill:#0173B2,color:#fff
    style B fill:#029E73,color:#fff
    style C fill:#CC78BC,color:#000
    style D fill:#DE8F05,color:#000
```

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: backend-network-policy
  namespace: default
spec:
  podSelector:
    matchLabels:
      app: backend # => Applies to Pods with app=backend
  policyTypes:
    - Ingress # => Controls incoming traffic
    - Egress # => Controls outgoing traffic
  ingress:
    - from:
        - podSelector:
            matchLabels:
              app: frontend # => Allow from frontend Pods
        - podSelector:
            matchLabels:
              app: database # => Allow from database Pods
      ports:
        - protocol: TCP
          port: 8080 # => Allow on port 8080 only

  egress:
    - to:
        - podSelector:
            matchLabels:
              app: database # => Allow to database Pods
      ports:
        - protocol: TCP
          port: 5432 # => PostgreSQL port


# NetworkPolicy behavior:
# => Without NetworkPolicy: all traffic allowed
# => With NetworkPolicy: default deny, explicit allow required
# => Requires CNI plugin support (Calico, Cilium, Weave)
# => Does NOT apply to Services (applies to Pods)
```

**Key Takeaway**: NetworkPolicies implement pod-to-pod firewall rules; start with deny-all policy and add explicit allow rules; requires CNI plugin with NetworkPolicy support (Calico, Cilium, Weave).

---

### Example 65: Default Deny Network Policy

Default deny NetworkPolicies block all traffic to/from Pods, requiring explicit allow rules. This zero-trust approach improves security by denying unexpected traffic.

```yaml
# Deny all ingress traffic
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: deny-all-ingress
  namespace: default
spec:
  podSelector: {} # => Applies to all Pods in namespace
  policyTypes:
    - Ingress # => Deny all incoming traffic

---
# Deny all egress traffic
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: deny-all-egress
  namespace: default
spec:
  podSelector: {}
  policyTypes:
    - Egress # => Deny all outgoing traffic

---
# Deny all ingress and egress
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: deny-all
  namespace: default
spec:
  podSelector: {}
  policyTypes:
    - Ingress
    - Egress # => Complete isolation


# Apply default deny first:
# 1. kubectl apply -f deny-all.yaml
# 2. Pods cannot communicate (expected)
# 3. Create allow rules for specific traffic
# 4. Test connectivity before deploying

# Testing connectivity:
# => kubectl run test --image=busybox --rm -it -- sh
# => wget -O- http://backend-service:8080  # Blocked by NetworkPolicy
```

**Key Takeaway**: Apply default deny NetworkPolicies at namespace level for zero-trust security; create explicit allow rules for required traffic only; test thoroughly to avoid blocking legitimate traffic.

---

### Example 66: Namespace Isolation with NetworkPolicy

NetworkPolicies can isolate namespaces, allowing traffic only from specific namespaces using namespaceSelector. This enforces environment separation (dev, staging, prod).

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-from-frontend-namespace
  namespace: backend-ns # => Applies in backend-ns namespace
spec:
  podSelector:
    matchLabels:
      app: api # => Applies to api Pods in backend-ns
  policyTypes:
    - Ingress
  ingress:
    - from:
        - namespaceSelector:
            matchLabels:
              environment:
                frontend # => Allow from namespaces with environment=frontend label
                # => kubectl label namespace frontend-ns environment=frontend
        - podSelector:
            matchLabels:
              app: web # => AND Pods with app=web
      ports:
        - protocol: TCP
          port: 8080

# Namespace isolation patterns:
# => Production namespace: allow only from prod namespaces
# => Development namespace: allow from dev namespaces
# => Shared services: allow from multiple namespaces

# Label namespaces for NetworkPolicy:
# => kubectl label namespace frontend-ns environment=frontend
# => kubectl label namespace backend-ns environment=backend
# => kubectl label namespace prod-ns environment=production
```

**Key Takeaway**: Use namespaceSelector for namespace-level isolation; label namespaces to define trust boundaries; combine podSelector and namespaceSelector for fine-grained cross-namespace access control.

---

### Example 67: Egress to External Services

NetworkPolicies can control egress traffic to external IP addresses and DNS names, restricting outbound connections to approved services.

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: allow-egress-external
  namespace: default
spec:
  podSelector:
    matchLabels:
      app: api
  policyTypes:
    - Egress
  egress:
    # Allow DNS resolution
    - to:
        - namespaceSelector:
            matchLabels:
              kubernetes.io/metadata.name: kube-system
          podSelector:
            matchLabels:
              k8s-app: kube-dns # => Allow CoreDNS
      ports:
        - protocol: UDP
          port: 53 # => DNS port

    # Allow specific external IP
    - to:
        - ipBlock:
            cidr: 203.0.113.0/24 # => Allow to IP range
            except:
              - 203.0.113.10/32 # => Except this IP
      ports:
        - protocol: TCP
          port: 443 # => HTTPS only

    # Allow to external service
    - to:
        - ipBlock:
            cidr: 0.0.0.0/0 # => Allow to any IP (internet)
      ports:
        - protocol: TCP
          port: 443 # => HTTPS to internet


# Egress control use cases:
# => Restrict outbound to approved APIs
# => Prevent data exfiltration
# => Control cloud provider API access
# => Block unwanted internet access
```

**Key Takeaway**: Control egress traffic to external services using ipBlock; always allow DNS (port 53) for name resolution; use CIDR ranges to restrict outbound access to approved IP addresses and services.

---

### Example 68: NetworkPolicy with Multiple Selectors

NetworkPolicies support complex rules combining podSelector, namespaceSelector, and ipBlock for fine-grained traffic control.

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: complex-policy
  namespace: production
spec:
  podSelector:
    matchLabels:
      tier: backend
      environment: prod # => Applies to Pods with both labels
  policyTypes:
    - Ingress
    - Egress
  ingress:
    # Rule 1: Allow from frontend in same namespace
    - from:
        - podSelector:
            matchLabels:
              tier: frontend
      ports:
        - protocol: TCP
          port: 8080

    # Rule 2: Allow from monitoring namespace
    - from:
        - namespaceSelector:
            matchLabels:
              name: monitoring
      ports:
        - protocol: TCP
          port: 9090 # => Metrics endpoint

    # Rule 3: Allow from specific IP range
    - from:
        - ipBlock:
            cidr: 10.0.0.0/8 # => Internal network
      ports:
        - protocol: TCP
          port: 8080

  egress:
    # Allow to database
    - to:
        - podSelector:
            matchLabels:
              tier: database
      ports:
        - protocol: TCP
          port: 5432

    # Allow to external API
    - to:
        - ipBlock:
            cidr: 203.0.113.50/32 # => Specific external API
      ports:
        - protocol: TCP
          port: 443

# Multiple selector behavior:
# => Multiple items in from/to are OR'd (any match allows)
# => Multiple selectors in same item are AND'd (all must match)
# => Example: podSelector AND namespaceSelector in same item
```

**Key Takeaway**: Combine multiple selectors for complex traffic rules; understand OR (multiple from/to items) vs AND (multiple selectors in same item) semantics; test policies thoroughly in non-production before applying to production.

---

## Custom Resources & Operators (Examples 69-73)

### Example 69: Custom Resource Definition (CRD)

CustomResourceDefinitions extend Kubernetes API with custom resource types. CRDs define schema, validation, and versions for custom resources.

```yaml
apiVersion: apiextensions.k8s.io/v1
kind: CustomResourceDefinition
metadata:
  name: databases.example.com # => CRD name: <plural>.<group>
spec:
  group: example.com # => API group
  names:
    kind: Database # => Resource kind (singular, PascalCase)
    plural: databases # => Plural name for API
    singular: database # => Singular name
    shortNames:
      - db # => Short name for kubectl
  scope: Namespaced # => Namespaced or Cluster
  versions:
    - name: v1 # => API version
      served: true # => Enable this version
      storage: true # => Storage version (one per CRD)
      schema:
        openAPIV3Schema: # => Validation schema
          type: object
          properties:
            spec:
              type: object
              properties:
                engine:
                  type: string
                  enum: ["postgres", "mysql", "mongodb"]
                version:
                  type: string
                replicas:
                  type: integer
                  minimum: 1
                  maximum: 10
              required:
                - engine
                - version
            status:
              type: object
              properties:
                phase:
                  type: string
                conditions:
                  type: array
                  items:
                    type: object


# CRD creation:
# => kubectl apply -f database-crd.yaml
# => kubectl get crd databases.example.com
# => API now accepts Database resources

---
# Custom resource instance
apiVersion: example.com/v1
kind: Database
metadata:
  name: production-db
spec:
  engine: postgres
  version: "15"
  replicas: 3

# => kubectl apply -f production-db.yaml
# => kubectl get databases
# => kubectl get db production-db -o yaml
```

**Key Takeaway**: Use CRDs to extend Kubernetes with domain-specific resources; define OpenAPI schema for validation; implement controllers (operators) to reconcile custom resources to desired state.

---

### Example 70: Custom Resource with Subresources

CRDs support subresources like status and scale, enabling kubectl commands and standard Kubernetes patterns.

```yaml
apiVersion: apiextensions.k8s.io/v1
kind: CustomResourceDefinition
metadata:
  name: applications.example.com
spec:
  group: example.com
  names:
    kind: Application
    plural: applications
  scope: Namespaced
  versions:
    - name: v1
      served: true
      storage: true
      schema:
        openAPIV3Schema:
          type: object
          properties:
            spec:
              type: object
              properties:
                image:
                  type: string
                replicas:
                  type: integer
            status:
              type: object
              properties:
                availableReplicas:
                  type: integer
                conditions:
                  type: array

      subresources:
        status: {} # => Enable status subresource
          # => Status updates separate from spec
        scale: # => Enable scale subresource
          specReplicasPath: .spec.replicas
          statusReplicasPath:
            .status.availableReplicas
            # => kubectl scale application/myapp --replicas=5

# Subresource benefits:
# => status: separate permissions (controller updates, users read-only)
# => scale: kubectl scale integration
# => kubectl get applications shows AVAILABLE replicas
```

**Key Takeaway**: Enable status subresource for separation of spec and status updates; enable scale subresource for kubectl scale integration; subresources follow standard Kubernetes patterns improving UX.

---

### Example 71: Operator Pattern with Controller

Operators are custom controllers watching custom resources and reconciling actual state to desired state. This example shows a basic operator structure.

```yaml
# Simplified operator pseudo-code (Go)
# Full implementation requires client-go and controller-runtime

# 1. Watch custom resources
# => watch Database resources
# => on create/update/delete events

# 2. Reconcile loop
# func reconcile(db *Database) error {
#     // Desired state from spec
#     desiredReplicas := db.Spec.Replicas
#     desiredEngine := db.Spec.Engine
#
#     // Actual state from cluster
#     actualStatefulSet := getStatefulSet(db.Name)
#     actualReplicas := actualStatefulSet.Spec.Replicas
#
#     // Reconcile
#     if actualReplicas != desiredReplicas {
#         actualStatefulSet.Spec.Replicas = desiredReplicas
#         updateStatefulSet(actualStatefulSet)
#     }
#
#     // Update status
#     db.Status.Phase = "Running"
#     db.Status.Replicas = actualReplicas
#     updateDatabaseStatus(db)
# }

# Operator responsibilities:
# => Create StatefulSet for Database
# => Create Service for Database
# => Create PVCs for persistence
# => Handle backups (CronJob)
# => Handle upgrades (rolling update)
# => Update status with current state

# Operator frameworks:
# => Operator SDK (Red Hat)
# => Kubebuilder (Kubernetes SIG)
# => KUDO (Kubernetes Universal Declarative Operator)

# Example Database operator actions:
# => spec.replicas=3 → creates StatefulSet with 3 replicas
# => spec.version="15" → updates image to postgres:15
# => spec.backup=true → creates CronJob for backups
# => status.phase → Running/Pending/Failed
```

**Key Takeaway**: Operators automate operational tasks by watching custom resources and reconciling state; use operator frameworks (Operator SDK, Kubebuilder) for production operators; operators enable self-service platforms and complex application lifecycle management.

---

### Example 72: Operator Lifecycle Manager (OLM)

Operator Lifecycle Manager manages operator installation, upgrades, and dependencies. OLM provides operator catalog, versioning, and RBAC.

```yaml
# Install OLM:
# => curl -sL https://github.com/operator-framework/operator-lifecycle-manager/releases/download/v0.25.0/install.sh | bash -s v0.25.0

# Operator CSV (ClusterServiceVersion)
apiVersion: operators.coreos.com/v1alpha1
kind: ClusterServiceVersion
metadata:
  name: database-operator.v1.0.0
  namespace: operators
spec:
  displayName: Database Operator
  version: 1.0.0
  description: Manages PostgreSQL databases
  keywords:
    - database
    - postgres
  maintainers:
    - name: Platform Team
      email: [email protected]
  provider:
    name: Example Corp
  icon:
    - base64data: <base64-encoded-icon>
      mediatype: image/png
  customresourcedefinitions:
    owned:
      - name: databases.example.com
        version: v1
        kind: Database
        displayName: Database
        description: PostgreSQL Database instance
  install:
    strategy: deployment
    spec:
      deployments:
        - name: database-operator
          spec:
            replicas: 1
            selector:
              matchLabels:
                name: database-operator
            template:
              metadata:
                labels:
                  name: database-operator
              spec:
                serviceAccountName: database-operator
                containers:
                  - name: operator
                    image: example.com/database-operator:v1.0.0

# Install operator via OLM:
# => kubectl create -f database-operator-csv.yaml
# => OLM creates Deployment, ServiceAccount, RBAC
# => Operator starts managing Database resources

# Upgrade operator:
# => Create new CSV with version v1.1.0
# => OLM performs rolling update
# => Old CSV remains for rollback
```

**Key Takeaway**: Use OLM for production operator management; OLM handles installation, upgrades, RBAC, and dependencies automatically; publish operators to OperatorHub for community distribution.

---

### Example 73: Admission Webhooks

Admission webhooks intercept API requests before persistence, enabling validation and mutation. Use webhooks for custom policies and automatic resource modification.

```yaml
apiVersion: admissionregistration.k8s.io/v1
kind: ValidatingWebhookConfiguration
metadata:
  name: pod-validator
webhooks:
  - name: pod-validator.example.com
    clientConfig:
      service:
        name: webhook-service
        namespace: default
        path: /validate
      caBundle: <base64-ca-cert> # => CA certificate for TLS
    rules:
      - operations: ["CREATE", "UPDATE"]
        apiGroups: [""]
        apiVersions: ["v1"]
        resources: ["pods"] # => Intercept Pod create/update
    admissionReviewVersions: ["v1"]
    sideEffects: None
    timeoutSeconds: 5


# Webhook server response:
# {
#   "apiVersion": "admission.k8s.io/v1",
#   "kind": "AdmissionReview",
#   "response": {
#     "uid": "<request-uid>",
#     "allowed": true/false,         # => Allow or reject
#     "status": {
#       "message": "Rejection reason"
#     }
#   }
# }

---
apiVersion: admissionregistration.k8s.io/v1
kind: MutatingWebhookConfiguration
metadata:
  name: pod-mutator
webhooks:
  - name: pod-mutator.example.com
    clientConfig:
      service:
        name: webhook-service
        namespace: default
        path: /mutate
      caBundle: <base64-ca-cert>
    rules:
      - operations: ["CREATE"]
        apiGroups: [""]
        apiVersions: ["v1"]
        resources: ["pods"]
    admissionReviewVersions: ["v1"]
    sideEffects: None

# Webhook mutation response:
# {
#   "response": {
#     "uid": "<request-uid>",
#     "allowed": true,
#     "patchType": "JSONPatch",
#     "patch": "<base64-json-patch>"  # => JSON Patch to apply
#   }
# }

# Common webhook use cases:
# => Validation: enforce naming conventions, require labels
# => Mutation: inject sidecars, add default resource limits
# => Policy: prevent privileged Pods, enforce security contexts
```

**Key Takeaway**: Use validating webhooks for custom policy enforcement beyond built-in admission controllers; use mutating webhooks for automatic resource modification like sidecar injection; webhook failures block API requests by default (set failurePolicy for control).

---

## Helm Charts (Examples 74-78)

### Example 74: Basic Helm Chart Structure

Helm packages Kubernetes manifests into charts with templating, versioning, and dependency management. Charts enable reusable application definitions.

```yaml
# Chart directory structure
my-app/
├── Chart.yaml                       # => Chart metadata
├── values.yaml                      # => Default configuration values
├── templates/                       # => Kubernetes manifest templates
│   ├── deployment.yaml
│   ├── service.yaml
│   ├── ingress.yaml
│   └── _helpers.tpl                # => Template helpers
└── charts/                          # => Dependency charts

# Chart.yaml
apiVersion: v2
name: my-app
description: A Helm chart for my application
type: application
version: 1.0.0                       # => Chart version
appVersion: "1.24"                   # => Application version
keywords:
- application
- web
maintainers:
- name: Platform Team
  email: [email protected]

# values.yaml
replicaCount: 3
image:
  repository: nginx
  tag: "1.24"
  pullPolicy: IfNotPresent
service:
  type: ClusterIP
  port: 80
ingress:
  enabled: false

# templates/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {{ include "my-app.fullname" . }}
  labels:
    {{- include "my-app.labels" . | nindent 4 }}
spec:
  replicas: {{ .Values.replicaCount }}
  selector:
    matchLabels:
      {{- include "my-app.selectorLabels" . | nindent 6 }}
  template:
    metadata:
      labels:
        {{- include "my-app.selectorLabels" . | nindent 8 }}
    spec:
      containers:
      - name: {{ .Chart.Name }}
        image: "{{ .Values.image.repository }}:{{ .Values.image.tag }}"
        ports:
        - containerPort: 80

# Helm commands:
# => helm create my-app                    # Generate chart scaffold
# => helm install my-release my-app        # Install chart
# => helm upgrade my-release my-app        # Upgrade release
# => helm rollback my-release 1            # Rollback to revision 1
# => helm uninstall my-release             # Uninstall release
# => helm list                             # List releases
```

**Key Takeaway**: Use Helm for repeatable application deployments with configuration management; separate chart version (Chart.yaml) from app version (appVersion); parameterize manifests using values.yaml for environment-specific deployments.

---

### Example 75: Helm Values and Overrides

Helm values provide hierarchical configuration with multiple override mechanisms. Values can be overridden via CLI, files, or --set flags.

```yaml
# values.yaml (default values)
global:
  environment: production
replicaCount: 3
image:
  repository: nginx
  tag: "1.24"
resources:
  requests:
    cpu: 100m
    memory: 128Mi
  limits:
    cpu: 200m
    memory: 256Mi

# values-dev.yaml (environment override)
global:
  environment: development
replicaCount: 1                      # => Override for dev
resources:
  requests:
    cpu: 50m                         # => Lower resources for dev
    memory: 64Mi

# templates/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: {{ .Release.Name }}-app
spec:
  replicas: {{ .Values.replicaCount }}
  template:
    spec:
      containers:
      - name: app
        image: {{ .Values.image.repository }}:{{ .Values.image.tag }}
        env:
        - name: ENVIRONMENT
          value: {{ .Values.global.environment }}
        resources:
          {{- toYaml .Values.resources | nindent 10 }}

# Helm install with overrides:
# => helm install dev-release my-app -f values-dev.yaml
# => Uses values-dev.yaml, merges with values.yaml
# => replicaCount=1, environment=development

# Override via --set flag:
# => helm install prod-release my-app --set replicaCount=5
# => --set overrides values.yaml

# Override precedence (highest to lowest):
# 1. --set flags
# 2. -f values-file.yaml (last file wins)
# 3. values.yaml (default)
```

**Key Takeaway**: Use values.yaml for defaults and environment-specific values files for overrides; leverage --set for one-off changes; understand value precedence to predict final configuration.

---

### Example 76: Helm Chart Dependencies

Helm charts can depend on other charts, enabling composition of complex applications from reusable components.

```yaml
# Chart.yaml
apiVersion: v2
name: web-application
version: 1.0.0
dependencies:
- name: postgresql                   # => Dependency chart name
  version: 12.1.5                    # => Version constraint
  repository: https://charts.bitnami.com/bitnami
                                     # => Chart repository URL
  condition: postgresql.enabled      # => Conditional dependency
- name: redis
  version: 17.9.3
  repository: https://charts.bitnami.com/bitnami
  condition: redis.enabled

# values.yaml
postgresql:
  enabled: true                      # => Enable PostgreSQL dependency
  auth:
    username: myapp
    password: changeme
    database: myapp_db
  primary:
    persistence:
      size: 10Gi

redis:
  enabled: false                     # => Disable Redis (not needed)

# Update dependencies:
# => helm dependency update
# => Downloads charts to charts/ directory
# => Creates Chart.lock with exact versions

# Chart.lock
dependencies:
- name: postgresql
  repository: https://charts.bitnami.com/bitnami
  version: 12.1.5                    # => Locked version
- name: redis
  repository: https://charts.bitnami.com/bitnami
  version: 17.9.3

# Install with dependencies:
# => helm install my-release web-application
# => Installs web-application + postgresql
# => redis not installed (enabled: false)

# Override dependency values:
# => helm install my-release web-application --set postgresql.auth.password=newsecret
```

**Key Takeaway**: Use chart dependencies for composable applications; lock dependency versions with Chart.lock for reproducible deployments; use conditions to enable/disable optional dependencies per environment.

---

### Example 77: Helm Hooks

Helm hooks run Jobs at specific points in release lifecycle, enabling pre/post-install tasks like database migrations or cleanup.

```yaml
# templates/db-migration-job.yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: {{ .Release.Name }}-migration
  annotations:
    "helm.sh/hook": pre-upgrade       # => Run before upgrade
    "helm.sh/hook-weight": "1"        # => Hook execution order
    "helm.sh/hook-delete-policy": before-hook-creation
                                      # => Delete previous hook Job
spec:
  template:
    spec:
      restartPolicy: Never
      containers:
      - name: migrate
        image: {{ .Values.image.repository }}:{{ .Values.image.tag }}
        command:
        - /bin/sh
        - -c
        - |
          echo "Running database migrations..."
          ./migrate.sh
          echo "Migrations complete"

# Available hooks:
# => pre-install: before install
# => post-install: after install
# => pre-delete: before delete
# => post-delete: after delete
# => pre-upgrade: before upgrade
# => post-upgrade: after upgrade
# => pre-rollback: before rollback
# => post-rollback: after rollback
# => test: helm test command

# Hook execution order:
# 1. Hooks with lowest weight execute first
# 2. Hooks with same weight execute in alphabetical order
# 3. Main resources deployed after hooks succeed

# Hook deletion policies:
# => before-hook-creation: delete previous hook before new one
# => hook-succeeded: delete after successful execution
# => hook-failed: delete after failed execution
```

**Key Takeaway**: Use Helm hooks for lifecycle tasks like database migrations, backups, or cleanup; set appropriate hook-delete-policy to prevent accumulation of hook resources; verify hook success before main release continues.

---

### Example 78: Helm Tests

Helm tests validate release deployments using Pods with test annotations. Tests ensure applications are healthy after deployment.

```yaml
# templates/tests/test-connection.yaml
apiVersion: v1
kind: Pod
metadata:
  name: {{ .Release.Name }}-test-connection
  annotations:
    "helm.sh/hook": test              # => Test hook
spec:
  restartPolicy: Never
  containers:
  - name: wget
    image: busybox:1.36
    command:
    - wget
    - --spider                        # => Check URL without downloading
    - {{ .Release.Name }}-service:{{ .Values.service.port }}

# Run tests:
# => helm test my-release
# => Runs Pods with helm.sh/hook: test annotation
# => Shows test results (passed/failed)

# Example test output:
# => NAME: my-release
# => NAMESPACE: default
# => STATUS: deployed
# => TEST SUITE:     my-release-test-connection
# => Last Started:   Mon Dec 30 00:00:00 2025
# => Last Completed: Mon Dec 30 00:00:05 2025
# => Phase:          Succeeded

# Common test scenarios:
# => Connection test: verify service reachable
# => Health check: verify application healthy
# => Data validation: verify database migrations succeeded
# => Integration test: verify dependencies connected
```

**Key Takeaway**: Implement Helm tests to validate releases post-deployment; include tests in CI/CD pipelines to catch deployment issues early; tests provide confidence in production rollouts.

---

## GitOps & ArgoCD (Examples 79-83)

### Example 79: GitOps Principles and Repository Structure

GitOps uses Git as single source of truth for declarative infrastructure and applications. Changes committed to Git trigger automatic synchronization to clusters.

```yaml
# GitOps repository structure
gitops-repo/
├── apps/
│   ├── production/
│   │   ├── app1/
│   │   │   ├── deployment.yaml
│   │   │   ├── service.yaml
│   │   │   └── kustomization.yaml
│   │   └── app2/
│   └── staging/
│       └── app1/
├── infrastructure/
│   ├── namespaces/
│   ├── rbac/
│   └── monitoring/
└── clusters/
    ├── prod-cluster/
    │   └── apps/
    │       └── app1.yaml         # => ArgoCD Application manifest
    └── staging-cluster/

# GitOps workflow:
# 1. Developer commits to Git
# 2. CI builds image, updates manifest with new tag
# 3. GitOps operator (ArgoCD, Flux) detects change
# 4. Operator syncs cluster state to match Git
# 5. Application deployed automatically

# GitOps benefits:
# => Single source of truth (Git)
# => Audit trail (Git history)
# => Rollback via Git revert
# => Declarative infrastructure
# => Automated deployment
# => Environment parity (same process for all envs)

# GitOps principles:
# 1. Declarative: desired state in Git
# 2. Versioned: Git provides versioning
# 3. Immutable: Git commits are immutable
# 4. Automated: reconciliation loop syncs cluster
# 5. Auditable: Git log provides audit trail
```

**Key Takeaway**: GitOps treats Git as single source of truth for cluster state; separate application manifests by environment (production, staging); use GitOps operators like ArgoCD for continuous synchronization and drift detection.

---

### Example 80: ArgoCD Installation and Configuration

ArgoCD is a declarative GitOps continuous delivery tool for Kubernetes. ArgoCD monitors Git repositories and synchronizes applications to clusters.

```yaml
# Install ArgoCD:
# => kubectl create namespace argocd
# => kubectl apply -n argocd -f https://raw.githubusercontent.com/argoproj/argo-cd/stable/manifests/install.yaml

# Access ArgoCD UI:
# => kubectl port-forward svc/argocd-server -n argocd 8080:443
# => https://localhost:8080

# Get admin password:
# => kubectl -n argocd get secret argocd-initial-admin-secret -o jsonpath="{.data.password}" | base64 -d

# ArgoCD Application CRD
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: my-app
  namespace: argocd
spec:
  project: default # => ArgoCD project
  source:
    repoURL: https://github.com/example/gitops-repo
    targetRevision: HEAD # => Git branch/tag/commit
    path: apps/production/app1 # => Path in repository
  destination:
    server:
      https://kubernetes.default.svc
      # => Target cluster
    namespace: production # => Target namespace
  syncPolicy:
    automated:
      prune: true # => Delete resources not in Git
      selfHeal: true # => Auto-sync on drift
    syncOptions:
      - CreateNamespace=true # => Auto-create namespace


# Apply Application:
# => kubectl apply -f my-app-application.yaml
# => ArgoCD starts syncing repository to cluster

# ArgoCD CLI:
# => argocd login localhost:8080
# => argocd app list
# => argocd app sync my-app
# => argocd app rollback my-app
```

**Key Takeaway**: Install ArgoCD for GitOps-based deployment automation; define Applications pointing to Git repositories; enable automated sync with selfHeal for drift correction; ArgoCD provides UI and CLI for application management.

---

### Example 81: ArgoCD Sync Strategies

ArgoCD supports multiple sync strategies controlling how and when applications synchronize from Git to cluster.

```yaml
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: sync-strategies-demo
  namespace: argocd
spec:
  source:
    repoURL: https://github.com/example/gitops-repo
    path: apps/production/demo
  destination:
    server: https://kubernetes.default.svc
    namespace: production
  syncPolicy:
    automated:
      prune: true # => Delete resources removed from Git
      selfHeal: true # => Auto-sync on drift detection
      allowEmpty: false # => Prevent deleting all resources
    syncOptions:
      - Validate=true # => Validate manifests before sync
      - CreateNamespace=true
      - PrunePropagationPolicy=foreground
        # => Delete dependents before owner
      - PruneLast=true # => Prune after applying
    retry:
      limit: 5 # => Retry limit on sync failure
      backoff:
        duration: 5s
        factor: 2 # => Exponential backoff
        maxDuration: 3m

# Sync strategies:
# 1. Manual sync (no automated section)
#    => argocd app sync my-app
#    => Sync only on demand

# 2. Automated sync (automated: {})
#    => Auto-sync on Git changes
#    => Manual intervention on drift

# 3. Automated with selfHeal
#    => Auto-sync on Git changes
#    => Auto-sync on cluster drift
#    => Prevents manual changes

# 4. Automated with prune
#    => Auto-sync + delete orphaned resources
#    => Resources in cluster but not Git deleted
#    => Enforces Git as source of truth
```

**Key Takeaway**: Use automated sync with selfHeal for production to prevent configuration drift; enable prune to remove orphaned resources; configure retry with backoff for resilience against transient failures.

---

### Example 82: ArgoCD Projects and RBAC

ArgoCD Projects provide logical grouping, access control, and restrictions for Applications. Projects enforce source repositories, destination clusters, and allowed resource types.

```yaml
apiVersion: argoproj.io/v1alpha1
kind: AppProject
metadata:
  name: production-project
  namespace: argocd
spec:
  description: Production applications
  sourceRepos:
    - https://github.com/example/gitops-repo
      # => Allowed source repositories
  destinations:
    - namespace: production
      server:
        https://kubernetes.default.svc
        # => Allowed destination clusters/namespaces
  clusterResourceWhitelist:
    - group: ""
      kind: Namespace # => Allow Namespace creation
    - group: "rbac.authorization.k8s.io"
      kind: ClusterRole # => Allow ClusterRole (restricted)
  namespaceResourceBlacklist:
    - group: ""
      kind: ResourceQuota # => Deny ResourceQuota changes
  roles:
    - name: developer
      description: Developer access
      policies:
        - p, proj:production-project:developer, applications, get, production-project/*, allow
        - p, proj:production-project:developer, applications, sync, production-project/*, allow
          # => Developers can view and sync apps
      groups:
        - developers # => Map to SSO group

---
apiVersion: argoproj.io/v1alpha1
kind: Application
metadata:
  name: prod-app
  namespace: argocd
spec:
  project: production-project # => Uses production project
  source:
    repoURL: https://github.com/example/gitops-repo
    path: apps/production/app1
  destination:
    server: https://kubernetes.default.svc
    namespace: production

# Project restrictions:
# => Applications must use allowed source repos
# => Applications must target allowed destinations
# => Applications cannot create blacklisted resources
# => RBAC enforces user/group permissions
```

**Key Takeaway**: Use ArgoCD Projects for multi-team environments to enforce repository, cluster, and resource restrictions; configure RBAC roles for granular access control; projects prevent accidental changes to critical resources.

---

### Example 83: ArgoCD ApplicationSets

ApplicationSets generate multiple Applications from templates, enabling fleet management and multi-cluster deployments from single definition.

```yaml
apiVersion: argoproj.io/v1alpha1
kind: ApplicationSet
metadata:
  name: cluster-apps
  namespace: argocd
spec:
  generators:
    - list:
        elements:
          - cluster: prod-us-west
            url: https://prod-us-west.k8s.example.com
          - cluster: prod-eu-central
            url: https://prod-eu-central.k8s.example.com
          - cluster: staging
            url: https://staging.k8s.example.com
  template:
    metadata:
      name: "{{cluster}}-app" # => Generated name
    spec:
      project: default
      source:
        repoURL: https://github.com/example/gitops-repo
        path: "apps/{{cluster}}" # => Cluster-specific path
        targetRevision: HEAD
      destination:
        server: "{{url}}" # => Cluster URL from generator
        namespace: default
      syncPolicy:
        automated:
          prune: true
          selfHeal: true

# ApplicationSet generators:
# 1. List generator (static list)
# 2. Cluster generator (auto-discover clusters)
# 3. Git generator (generate from Git directory structure)
# 4. Matrix generator (combine multiple generators)

# Use cases:
# => Deploy same app to multiple clusters
# => Deploy multiple apps from Git directory structure
# => Multi-tenant deployments (one app per tenant)
# => Progressive rollout across cluster fleet

# Git directory generator example:
# generators:
# - git:
#     repoURL: https://github.com/example/gitops-repo
#     revision: HEAD
#     directories:
#     - path: apps/*
```

**Key Takeaway**: Use ApplicationSets for fleet management and multi-cluster deployments; leverage generators to avoid manual Application creation; ApplicationSets enable GitOps at scale for multiple environments and clusters.

---

## Production Patterns (Examples 84-85)

### Example 84: Production Monitoring and Observability

Production clusters require comprehensive monitoring, logging, and tracing for observability. Combine Prometheus, Grafana, and Loki for full-stack visibility.

```yaml
# Prometheus Operator (monitoring)
# => kubectl apply -f https://raw.githubusercontent.com/prometheus-operator/prometheus-operator/main/bundle.yaml

apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: app-monitor
spec:
  selector:
    matchLabels:
      app: myapp # => Monitors Services with app=myapp
  endpoints:
    - port: metrics # => Service port name
      interval: 30s # => Scrape every 30 seconds
      path: /metrics # => Metrics endpoint

---
# Application with metrics endpoint
apiVersion: v1
kind: Service
metadata:
  name: myapp-service
  labels:
    app: myapp
spec:
  selector:
    app: myapp
  ports:
    - name: metrics # => Named port for ServiceMonitor
      port: 9090
      targetPort: 9090

---
# Grafana Dashboard ConfigMap
apiVersion: v1
kind: ConfigMap
metadata:
  name: grafana-dashboard
  labels:
    grafana_dashboard: "1" # => Auto-imported by Grafana
data:
  app-dashboard.json: |
    {
      "dashboard": {
        "title": "Application Metrics",
        "panels": [
          {
            "title": "Request Rate",
            "targets": [
              {
                "expr": "rate(http_requests_total[5m])"
              }
            ]
          }
        ]
      }
    }

# Logging with Loki
# => helm install loki grafana/loki-stack

# Tracing with Jaeger
# => kubectl create -f https://raw.githubusercontent.com/jaegertracing/jaeger-operator/main/deploy/crds/jaegertracing.io_jaegers_crd.yaml
# => kubectl create -n observability -f https://raw.githubusercontent.com/jaegertracing/jaeger-operator/main/deploy/service_account.yaml

# Observability pillars:
# => Metrics: Prometheus (time-series data)
# => Logs: Loki (log aggregation)
# => Traces: Jaeger (distributed tracing)
# => Dashboards: Grafana (visualization)
```

**Key Takeaway**: Implement three pillars of observability (metrics, logs, traces) for production clusters; use Prometheus for metrics, Loki for logs, and Jaeger for traces; create Grafana dashboards for visualization and alerting.

---

### Example 85: Production Best Practices Checklist

Production Kubernetes clusters require comprehensive setup across security, reliability, monitoring, and operations. This example provides a complete checklist.

```yaml
# 1. Security Hardening
# ✓ Enable RBAC
# ✓ Use NetworkPolicies for pod-to-pod traffic control
# ✓ Enable Pod Security Standards (restricted level)
# ✓ Encrypt Secrets at rest
# ✓ Run containers as non-root
# ✓ Use read-only root filesystem
# ✓ Drop all capabilities, add only required
# ✓ Enable audit logging
# ✓ Scan images for vulnerabilities
# ✓ Use private container registries with image pull secrets

# 2. High Availability
# ✓ Multiple replicas for all Deployments (min 3)
# ✓ PodDisruptionBudgets for all critical services
# ✓ Node affinity and anti-affinity for zone distribution
# ✓ Resource requests and limits for all Pods
# ✓ Liveness and readiness probes
# ✓ Multi-zone cluster setup
# ✓ Backup etcd regularly
# ✓ Use LoadBalancer or Ingress for external access

# 3. Resource Management
# ✓ ResourceQuotas per namespace
# ✓ LimitRanges for default limits
# ✓ HorizontalPodAutoscaler for dynamic scaling
# ✓ VerticalPodAutoscaler for right-sizing
# ✓ QoS class: Guaranteed for critical workloads
# ✓ PriorityClasses for workload prioritization

# 4. Monitoring and Observability
# ✓ Prometheus for metrics collection
# ✓ Grafana dashboards for visualization
# ✓ Loki for log aggregation
# ✓ Jaeger for distributed tracing
# ✓ Alertmanager for alerting
# ✓ ServiceMonitors for application metrics
# ✓ Custom metrics for HPA
# ✓ SLO/SLI tracking

# 5. GitOps and Deployment
# ✓ Git as single source of truth
# ✓ ArgoCD for continuous deployment
# ✓ Helm for package management
# ✓ Automated testing in CI/CD
# ✓ Staging environment for validation
# ✓ Progressive rollouts (canary/blue-green)
# ✓ Automated rollback on failure

# 6. Backup and Disaster Recovery
# ✓ VolumeSnapshots for data backup
# ✓ Backup etcd snapshots daily
# ✓ Disaster recovery plan and runbook
# ✓ Regular restore testing
# ✓ Multi-region for critical services
# ✓ RTO/RPO targets defined

# 7. Network and Storage
# ✓ NetworkPolicies for namespace isolation
# ✓ Ingress with TLS termination
# ✓ StorageClasses with dynamic provisioning
# ✓ PersistentVolume encryption
# ✓ Backup storage volumes
# ✓ Network segmentation

# 8. Operations
# ✓ Cluster upgrade strategy
# ✓ Node maintenance procedures
# ✓ Incident response runbooks
# ✓ On-call rotation
# ✓ Capacity planning
# ✓ Cost monitoring and optimization
# ✓ Documentation (architecture, runbooks)
```

**Key Takeaway**: Production readiness requires comprehensive setup across security, reliability, monitoring, and operations; use this checklist to validate cluster readiness; automate validation using admission controllers and policy engines like OPA/Kyverno.

---

## Summary

**Advanced level (75-95% coverage)** covered:

- **RBAC & Security** (Examples 58-63): ServiceAccounts, Roles, ClusterRoles, SecurityContext, encryption at rest
- **Network Policies** (Examples 64-68): Pod-to-pod traffic control, namespace isolation, egress restrictions, complex selectors
- **Custom Resources & Operators** (Examples 69-73): CRDs, subresources, operator pattern, OLM, admission webhooks
- **Helm Charts** (Examples 74-78): Chart structure, values, dependencies, hooks, tests
- **GitOps & ArgoCD** (Examples 79-83): GitOps principles, ArgoCD installation, sync strategies, projects, ApplicationSets
- **Production Patterns** (Examples 84-85): Monitoring/observability, production best practices

**Congratulations!** You've completed all 85 examples achieving **95% coverage** of production Kubernetes knowledge.

**Next steps**:

- Apply these patterns in production environments
- Explore [Beginner Examples](/en/learn/software-engineering/infrastructure/tools/kubernetes/tutorials/by-example/beginner) for fundamentals review
- Or review [Intermediate Examples](/en/learn/software-engineering/infrastructure/tools/kubernetes/tutorials/by-example/intermediate) for production patterns

All examples are production-ready and battle-tested. Happy Kubernetes mastery!
