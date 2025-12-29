---
title: "Intermediate"
date: 2025-12-30T00:00:43+07:00
draft: false
weight: 10000002
description: "Examples 29-57: Production patterns including StatefulSets, DaemonSets, Jobs, Ingress, PersistentVolumes, resource management, and health checks (40-75% coverage)"
tags: ["kubernetes", "tutorial", "by-example", "intermediate", "statefulsets", "ingress", "storage", "production"]
---

## Intermediate Level Overview

This level covers **production patterns and advanced resource types** through 29 examples, achieving **40-75% coverage** of production Kubernetes knowledge. Each example demonstrates real-world patterns needed for running stateful workloads, managing storage, and implementing production-grade reliability.

**What you'll learn**:

- StatefulSets for stateful workloads with persistent storage
- DaemonSets and Jobs for background tasks and batch processing
- Ingress Controllers for HTTP routing and TLS termination
- Persistent Volumes for storage orchestration
- Resource management with requests, limits, and quotas
- Health checks with liveness, readiness, and startup probes

**Prerequisites**: Completion of Beginner examples (1-28) or equivalent Kubernetes fundamentals knowledge

---

## StatefulSets (Examples 29-33)

### Example 29: Basic StatefulSet

StatefulSets manage stateful applications requiring stable network identities, persistent storage, and ordered deployment/scaling. Unlike Deployments, StatefulSets provide predictable Pod names and persistent storage across restarts.

```mermaid
%% StatefulSet architecture with persistent storage
graph TD
    A[StatefulSet: database] --> B[Pod: database-0]
    A --> C[Pod: database-1]
    A --> D[Pod: database-2]
    B --> E[PVC: data-database-0]
    C --> F[PVC: data-database-1]
    D --> G[PVC: data-database-2]

    style A fill:#0173B2,color:#fff
    style B fill:#029E73,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
    style E fill:#CC78BC,color:#000
    style F fill:#CC78BC,color:#000
    style G fill:#CC78BC,color:#000
```

```yaml
apiVersion: v1
kind: Service
metadata:
  name: database # => Headless Service for StatefulSet
spec:
  clusterIP:
    None # => Headless (no cluster IP)
    # => DNS returns Pod IPs directly
  selector:
    app: database
  ports:
    - port: 5432
      name: postgres

---
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: database
spec:
  serviceName:
    database # => Associates with headless Service
    # => Enables predictable DNS names
  replicas: 3 # => Creates 3 Pods: database-0, database-1, database-2
  selector:
    matchLabels:
      app: database
  template:
    metadata:
      labels:
        app: database
    spec:
      containers:
        - name: postgres
          image: postgres:15
          ports:
            - containerPort: 5432
              name: postgres
          volumeMounts:
            - name: data # => References volumeClaimTemplate below
              mountPath: /var/lib/postgresql/data
          env:
            - name: POSTGRES_PASSWORD
              value: "example" # => Use Secret in production
  volumeClaimTemplates: # => Creates PVC per Pod
    - metadata:
        name: data # => PVC name pattern: data-database-0, data-database-1, etc.
      spec:
        accessModes: ["ReadWriteOnce"] # => Single node read-write
        resources:
          requests:
            storage: 10Gi # => Each Pod gets 10 GiB persistent storage


# StatefulSet guarantees:
# => Pods created in order: database-0, then database-1, then database-2
# => Pods deleted in reverse order: database-2, then database-1, then database-0
# => Each Pod has stable hostname: database-0.database.default.svc.cluster.local
# => PVCs persist across Pod restarts and rescheduling
```

**Key Takeaway**: Use StatefulSets for databases, message queues, and applications requiring stable network identities and persistent storage; StatefulSets guarantee ordered deployment/scaling and maintain PVC associations across Pod restarts.

---

### Example 30: StatefulSet Update Strategy

StatefulSets support RollingUpdate (default) and OnDelete update strategies. RollingUpdate updates Pods in reverse ordinal order (highest to lowest), while OnDelete requires manual Pod deletion for updates.

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: web-stateful
spec:
  serviceName: web
  replicas: 4
  updateStrategy:
    type:
      RollingUpdate # => Rolling update strategy (default)
      # => Updates Pods in reverse order: 3→2→1→0
    rollingUpdate:
      partition:
        2 # => Only update Pods with ordinal >= partition
        # => Pods 2 and 3 update, Pods 0 and 1 stay old version
        # => Useful for canary testing
  selector:
    matchLabels:
      app: web-stateful
  template:
    metadata:
      labels:
        app: web-stateful
    spec:
      containers:
        - name: nginx
          image:
            nginx:1.24 # => Update to nginx:1.25 to trigger rolling update
            # => Pod 3 updates first, then Pod 2
          ports:
            - containerPort: 80

# Update behavior with partition=2:
# => kubectl set image statefulset/web-stateful nginx=nginx:1.25
# => Pod web-stateful-3: updated to nginx:1.25
# => Pod web-stateful-2: updated to nginx:1.25
# => Pod web-stateful-1: remains at nginx:1.24 (ordinal < partition)
# => Pod web-stateful-0: remains at nginx:1.24 (ordinal < partition)
# => Set partition=0 to complete update
```

**Key Takeaway**: Use partition in RollingUpdate strategy for canary deployments on StatefulSets; update high-ordinal Pods first while keeping low-ordinal Pods on stable version for gradual rollout validation.

---

### Example 31: StatefulSet with Init Containers

Init containers in StatefulSets can prepare persistent volumes, wait for dependencies, or perform one-time setup before the main application starts. This pattern ensures data initialization completes before database or cache services become ready.

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: redis-cluster
spec:
  serviceName: redis
  replicas: 3
  selector:
    matchLabels:
      app: redis
  template:
    metadata:
      labels:
        app: redis
    spec:
      initContainers:
        - name: init-redis # => Prepares Redis configuration
          image: redis:7
          command:
            - sh
            - -c
            - |
              echo "Initializing Redis config for Pod $POD_NAME"
              cp /config/redis.conf /data/redis.conf
              sed -i "s/POD_NAME/${POD_NAME}/g" /data/redis.conf
          env:
            - name: POD_NAME
              valueFrom:
                fieldRef:
                  fieldPath: metadata.name # => Gets Pod name: redis-cluster-0
          volumeMounts:
            - name: config
              mountPath: /config
            - name: data
              mountPath: /data

      containers:
        - name: redis
          image: redis:7
          command: ["redis-server", "/data/redis.conf"]
          ports:
            - containerPort: 6379
          volumeMounts:
            - name: data # => Same volume as init container
              mountPath: /data # => Reads config prepared by init

      volumes:
        - name: config
          configMap:
            name: redis-config

  volumeClaimTemplates:
    - metadata:
        name: data
      spec:
        accessModes: ["ReadWriteOnce"]
        resources:
          requests:
            storage: 5Gi
```

**Key Takeaway**: Use init containers in StatefulSets for data initialization, configuration templating, or dependency waiting; init containers have access to volumeClaimTemplates volumes and Pod metadata for per-instance customization.

---

### Example 32: StatefulSet Pod Management Policy

Pod Management Policy controls whether StatefulSet creates/deletes Pods sequentially (OrderedReady, default) or in parallel (Parallel). Parallel policy speeds up scaling but loses ordering guarantees.

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: parallel-stateful
spec:
  serviceName: parallel
  replicas: 10
  podManagementPolicy:
    Parallel # => Parallel Pod creation/deletion
    # => Default: OrderedReady (sequential)
    # => Parallel: all Pods created simultaneously
    # => Faster scaling but no ordering guarantee
  selector:
    matchLabels:
      app: parallel
  template:
    metadata:
      labels:
        app: parallel
    spec:
      containers:
        - name: nginx
          image: nginx:1.24

# OrderedReady (default):
# => Scale 0→10: creates Pods 0,1,2,3,4,5,6,7,8,9 sequentially
# => Each Pod must be Ready before next starts
# => Scale 10→0: deletes in reverse order 9,8,7,6,5,4,3,2,1,0

# Parallel:
# => Scale 0→10: creates all 10 Pods simultaneously
# => No waiting for Ready status
# => Scale 10→0: deletes all 10 Pods simultaneously
```

**Key Takeaway**: Use Parallel podManagementPolicy for faster scaling when Pod ordering is not critical; keep OrderedReady (default) for databases and applications requiring sequential initialization.

---

### Example 33: StatefulSet with Persistent Volume Retention

PersistentVolumeClaim retention policy controls whether PVCs are deleted when StatefulSet scales down or is deleted. WhenDeleted retains PVCs on scale-down but deletes on StatefulSet deletion, while Retain preserves PVCs in all cases.

```yaml
apiVersion: apps/v1
kind: StatefulSet
metadata:
  name: retained-stateful
spec:
  serviceName: retained
  replicas: 3
  persistentVolumeClaimRetentionPolicy:
    whenDeleted:
      Retain # => Retain PVCs when StatefulSet deleted
      # => Alternative: Delete (removes PVCs)
    whenScaled:
      Retain # => Retain PVCs when scaling down
      # => Alternative: Delete (removes PVCs of deleted Pods)
  selector:
    matchLabels:
      app: retained
  template:
    metadata:
      labels:
        app: retained
    spec:
      containers:
        - name: nginx
          image: nginx:1.24
          volumeMounts:
            - name: data
              mountPath: /usr/share/nginx/html

  volumeClaimTemplates:
    - metadata:
        name: data
      spec:
        accessModes: ["ReadWriteOnce"]
        resources:
          requests:
            storage: 5Gi

# Retention behavior:
# => Scale 3→1: Pods 2 and 1 deleted, but PVCs data-retained-stateful-2 and data-retained-stateful-1 retained
# => Scale 1→3: Pods 1 and 2 recreated, attach to existing PVCs (data preserved)
# => kubectl delete statefulset retained-stateful: StatefulSet deleted, PVCs retained
# => Manual cleanup required: kubectl delete pvc data-retained-stateful-0 data-retained-stateful-1 data-retained-stateful-2
```

**Key Takeaway**: Use Retain policy for production databases to prevent accidental data loss during scaling or deletion; remember to manually clean up PVCs when no longer needed to avoid storage costs.

---

## DaemonSets & Jobs (Examples 34-38)

### Example 34: Basic DaemonSet

DaemonSets ensure a Pod runs on every node (or a subset of nodes), suitable for node-level services like log collectors, monitoring agents, or network plugins. Kubernetes automatically schedules DaemonSet Pods on new nodes.

```mermaid
%% DaemonSet on all nodes
graph TD
    A[DaemonSet: log-agent] --> B[Pod on Node 1]
    A --> C[Pod on Node 2]
    A --> D[Pod on Node 3]
    B --> E[Collect logs from Node 1]
    C --> F[Collect logs from Node 2]
    D --> G[Collect logs from Node 3]

    style A fill:#0173B2,color:#fff
    style B fill:#029E73,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
```

```yaml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: log-collector
  labels:
    app: log-collector
spec:
  selector:
    matchLabels:
      app: log-collector
  template:
    metadata:
      labels:
        app: log-collector
    spec:
      containers:
        - name: fluentd
          image: fluent/fluentd:v1.16 # => Log forwarding agent
          volumeMounts:
            - name: varlog # => Mounts node's /var/log
              mountPath: /var/log
              readOnly: true
            - name: varlibdockercontainers # => Mounts Docker container logs
              mountPath: /var/lib/docker/containers
              readOnly: true
          resources:
            limits:
              memory: 200Mi
            requests:
              cpu: 100m
              memory: 200Mi

      volumes:
        - name: varlog
          hostPath:
            path: /var/log # => Node's /var/log directory
        - name: varlibdockercontainers
          hostPath:
            path: /var/lib/docker/containers # => Node's container logs


# DaemonSet behavior:
# => Creates 1 Pod per node automatically
# => New node joins cluster → Pod created on new node
# => Node removed → Pod deleted
# => kubectl get daemonset shows: DESIRED=3, CURRENT=3, READY=3 (3 nodes)
```

**Key Takeaway**: Use DaemonSets for node-level services requiring presence on every node; DaemonSets automatically handle node additions/removals and support node selectors for subset deployment.

---

### Example 35: DaemonSet with Node Selector

DaemonSets can target specific nodes using nodeSelector or node affinity, enabling specialized Pods on GPU nodes, SSD-equipped nodes, or region-specific nodes.

```yaml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  name: gpu-monitor
spec:
  selector:
    matchLabels:
      app: gpu-monitor
  template:
    metadata:
      labels:
        app: gpu-monitor
    spec:
      nodeSelector:
        accelerator:
          nvidia-gpu # => Only runs on nodes with this label
          # => kubectl label nodes node-1 accelerator=nvidia-gpu
      containers:
        - name: dcgm-exporter # => NVIDIA GPU monitoring
          image: nvidia/dcgm-exporter:3.1.3
          ports:
            - containerPort: 9400
          securityContext:
            privileged: true # => Required for GPU access


# DaemonSet with node selector:
# => Only creates Pods on nodes matching nodeSelector
# => 10 nodes total, 3 GPU nodes → DESIRED=3, CURRENT=3
# => New GPU node added → Pod created automatically
# => Node label removed → Pod deleted
```

**Key Takeaway**: Use nodeSelector or node affinity in DaemonSets to run specialized workloads only on appropriate nodes; label nodes based on hardware capabilities, regions, or roles for targeted DaemonSet deployment.

---

### Example 36: Kubernetes Job

Jobs run Pods to completion, suitable for batch processing, data migration, or one-time tasks. Unlike Deployments, Jobs terminate when tasks complete successfully and track completion status.

```yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: data-migration
spec:
  completions:
    1 # => Number of successful completions required
    # => Job completes after 1 successful Pod
  parallelism:
    1 # => Number of Pods running in parallel
    # => parallelism=3 runs 3 Pods simultaneously
  backoffLimit:
    3 # => Maximum retries before marking Job failed
    # => Retries with exponential backoff
  template:
    metadata:
      labels:
        app: migration
    spec:
      restartPolicy:
        Never # => Never or OnFailure (not Always)
        # => Always invalid for Jobs
      containers:
        - name: migrator
          image: busybox:1.36
          command:
            - sh
            - -c
            - |
              echo "Starting data migration..."
              sleep 10
              echo "Migration completed successfully"
              exit 0                      # => Exit 0 signals success
                                          # => Exit 1+ triggers retry (up to backoffLimit)

# Job lifecycle:
# => Pod created and runs to completion
# => Exit 0 → Job marked Complete
# => Exit 1+ → Pod recreated (up to backoffLimit retries)
# => After 3 failures → Job marked Failed
# => kubectl get jobs shows: COMPLETIONS=1/1, DURATION=15s
# => kubectl delete job data-migration  # Cleanup
```

**Key Takeaway**: Use Jobs for one-time or periodic batch tasks; set appropriate completions, parallelism, and backoffLimit based on workload requirements; Jobs do not support restartPolicy: Always.

---

### Example 37: Parallel Jobs

Parallel Jobs run multiple Pods simultaneously to process distributed workloads like batch rendering, data processing, or parallel computations. Configure completions and parallelism to control total work items and concurrency.

```yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: parallel-processing
spec:
  completions: 10 # => Total successful Pods required: 10
  parallelism:
    3 # => Run 3 Pods in parallel
    # => Creates Pods in batches: 3, then 3, then 3, then 1
  template:
    spec:
      restartPolicy:
        OnFailure # => Retry failed Pods within same Pod object
        # => Never creates new Pod for each retry
      containers:
        - name: worker
          image: busybox:1.36
          command:
            - sh
            - -c
            - |
              TASK_ID=$((RANDOM % 1000))
              echo "Processing task $TASK_ID"
              sleep $((5 + RANDOM % 10))
              echo "Task $TASK_ID completed"

# Parallel execution:
# => Pods 1,2,3 start immediately (parallelism=3)
# => Pod 1 completes → Pod 4 starts (maintains parallelism)
# => Pod 2 completes → Pod 5 starts
# => Continues until 10 successful completions
# => kubectl get pods shows 3 Running, 7 remaining
# => kubectl get jobs shows: COMPLETIONS=7/10 (7 completed, 3 running)
```

**Key Takeaway**: Use parallel Jobs for distributed batch processing; adjust parallelism based on cluster capacity and completions based on total work items; consider work queue pattern for dynamic task distribution.

---

### Example 38: CronJob for Scheduled Tasks

CronJobs create Jobs on a schedule using cron syntax, suitable for periodic backups, reports, or cleanup tasks. CronJobs maintain job history and support concurrency policies for overlapping executions.

```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: backup-job
spec:
  schedule:
    "0 2 * * *" # => Cron syntax: minute hour day month weekday
    # => "0 2 * * *" = 2:00 AM daily
    # => "*/5 * * * *" = every 5 minutes
    # => "0 */2 * * *" = every 2 hours
  concurrencyPolicy:
    Forbid # => Prevents concurrent Job runs
    # => Allow: permits concurrent executions
    # => Replace: cancels current and starts new
  successfulJobsHistoryLimit: 3 # => Keeps 3 successful Jobs
  failedJobsHistoryLimit: 1 # => Keeps 1 failed Job
  jobTemplate:
    spec:
      template:
        spec:
          restartPolicy: OnFailure
          containers:
            - name: backup
              image: busybox:1.36
              command:
                - sh
                - -c
                - |
                  echo "Starting backup at $(date)"
                  # Backup logic here
                  sleep 30
                  echo "Backup completed at $(date)"

# CronJob behavior:
# => Creates Job at scheduled time
# => Job creates Pod to run backup
# => After successfulJobsHistoryLimit, old Jobs deleted
# => kubectl get cronjobs shows: SCHEDULE, SUSPEND, ACTIVE, LAST SCHEDULE
# => kubectl create job --from=cronjob/backup-job manual-backup  # Manual trigger
```

**Key Takeaway**: Use CronJobs for scheduled recurring tasks with appropriate concurrencyPolicy to handle overlapping executions; set history limits to prevent accumulation of completed Jobs.

---

## Ingress Controllers (Examples 39-43)

### Example 39: Basic Ingress

Ingress manages external HTTP/HTTPS access to Services, providing host-based and path-based routing. Ingress requires an Ingress Controller (nginx, Traefik, HAProxy) to function.

```mermaid
%% Ingress routing
graph TD
    A[Client Request<br/>app.example.com/api] --> B[Ingress Controller]
    B --> C{Host + Path Match?}
    C -->|app.example.com/api| D[Service: api-service]
    C -->|app.example.com/web| E[Service: web-service]
    D --> F[Pod: api-1]
    D --> G[Pod: api-2]

    style A fill:#CC78BC,color:#000
    style B fill:#DE8F05,color:#000
    style D fill:#029E73,color:#fff
    style E fill:#029E73,color:#fff
    style F fill:#0173B2,color:#fff
    style G fill:#0173B2,color:#fff
```

```yaml
# First, install Ingress Controller (nginx example):
# => kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/controller-v1.8.1/deploy/static/provider/cloud/deploy.yaml

apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: app-ingress
  annotations:
    nginx.ingress.kubernetes.io/rewrite-target:
      /
      # => Rewrites /api/users → /users before forwarding
spec:
  ingressClassName:
    nginx # => Uses nginx Ingress Controller
    # => Required in Kubernetes 1.18+
  rules:
    - host:
        app.example.com # => Host-based routing
        # => Matches Host header in requests
      http:
        paths:
          - path: /api # => Path-based routing
            pathType:
              Prefix # => Matches /api, /api/, /api/users
              # => Exact: exact match only
              # => ImplementationSpecific: controller-dependent
            backend:
              service:
                name: api-service # => Routes to api-service
                port:
                  number: 80
          - path: /web
            pathType: Prefix
            backend:
              service:
                name: web-service # => Routes to web-service
                port:
                  number: 80

# Access patterns:
# => http://app.example.com/api/users → api-service
# => http://app.example.com/web/home → web-service
# => http://other.example.com/api → no match (404)
```

**Key Takeaway**: Ingress provides cost-effective HTTP/HTTPS routing compared to multiple LoadBalancer Services; install an Ingress Controller first, then create Ingress resources for routing rules.

---

### Example 40: Ingress with TLS

Ingress supports TLS termination using Secrets containing certificates and private keys. The Ingress Controller handles HTTPS decryption and forwards unencrypted traffic to backend Services.

```yaml
# Create TLS Secret:
# => kubectl create secret tls tls-secret --cert=tls.crt --key=tls.key

apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: tls-ingress
spec:
  ingressClassName: nginx
  tls:
    - hosts:
        - secure.example.com # => TLS applies to this host
      secretName:
        tls-secret # => References TLS Secret
        # => Secret must exist in same namespace
        # => Contains tls.crt and tls.key
  rules:
    - host: secure.example.com
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: web-service
                port:
                  number: 80

# TLS behavior:
# => https://secure.example.com → TLS termination at Ingress Controller
# => Ingress Controller → web-service over HTTP (cluster-internal)
# => http://secure.example.com → redirected to HTTPS (nginx default)
# => Certificate validation required for production
```

**Key Takeaway**: Use TLS Ingress for production HTTPS; obtain certificates from Let's Encrypt via cert-manager for automated certificate management and renewal; TLS terminates at Ingress Controller, not backend Services.

---

### Example 41: Ingress with Multiple Hosts

Ingress supports multiple hosts in a single resource, enabling consolidated routing configuration. Each host can have independent path-based routing rules.

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: multi-host-ingress
spec:
  ingressClassName: nginx
  rules:
    - host: api.example.com # => First host
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: api-service
                port:
                  number: 80

    - host: admin.example.com # => Second host
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: admin-service
                port:
                  number: 80

    - host: static.example.com # => Third host
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: static-service
                port:
                  number: 80

# Multi-host routing:
# => http://api.example.com → api-service
# => http://admin.example.com → admin-service
# => http://static.example.com → static-service
# => All hosts use same Ingress Controller IP
# => Configure DNS to point all hosts to Ingress Controller
```

**Key Takeaway**: Consolidate multiple host-based routes in a single Ingress resource for easier management; each host can have independent backend Services and path rules.

---

### Example 42: Ingress with Custom Annotations

Ingress Controllers support custom annotations for advanced features like rate limiting, authentication, CORS, and custom headers. Annotations are controller-specific (nginx, Traefik, etc.).

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: annotated-ingress
  annotations:
    nginx.ingress.kubernetes.io/rewrite-target:
      /$2
      # => URL rewriting with capture groups
    nginx.ingress.kubernetes.io/ssl-redirect:
      "true"
      # => Force HTTPS redirect
    nginx.ingress.kubernetes.io/rate-limit:
      "100"
      # => 100 requests per second per IP
    nginx.ingress.kubernetes.io/enable-cors:
      "true"
      # => Enable CORS headers
    nginx.ingress.kubernetes.io/cors-allow-origin:
      "https://example.com"
      # => Allowed CORS origin
    nginx.ingress.kubernetes.io/auth-type:
      basic
      # => Basic authentication
    nginx.ingress.kubernetes.io/auth-secret:
      basic-auth
      # => References Secret with credentials
spec:
  ingressClassName: nginx
  rules:
    - host: protected.example.com
      http:
        paths:
          - path: /api(/|$)(.*) # => Capture group for rewrite
            pathType: ImplementationSpecific
            backend:
              service:
                name: api-service
                port:
                  number: 80

# Annotation effects:
# => Request: /api/users → rewritten to /users
# => HTTP request → 308 redirect to HTTPS
# => >100 req/sec → 503 Service Temporarily Unavailable
# => Missing auth → 401 Unauthorized
```

**Key Takeaway**: Leverage Ingress Controller annotations for advanced HTTP features; consult controller documentation for available annotations as they vary between nginx, Traefik, and other controllers.

---

### Example 43: Ingress with Default Backend

Default backend serves requests that don't match any Ingress rules, useful for custom 404 pages or catch-all routing. Configure default backend at Ingress Controller or per-Ingress resource level.

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: default-backend-ingress
spec:
  ingressClassName: nginx
  defaultBackend:
    service:
      name: default-service # => Serves unmatched requests
      port:
        number: 80
  rules:
    - host: app.example.com
      http:
        paths:
          - path: /api
            pathType: Prefix
            backend:
              service:
                name: api-service
                port:
                  number: 80

# Default backend routing:
# => http://app.example.com/api → api-service (matches rule)
# => http://app.example.com/other → default-service (no match)
# => http://unknown.example.com → default-service (host no match)
# => Useful for custom 404 pages or redirect to main site
```

**Key Takeaway**: Configure default backend for better user experience on unmatched requests; implement custom 404 pages or redirects instead of generic Ingress Controller errors.

---

## Persistent Volumes (Examples 44-48)

### Example 44: PersistentVolume and PersistentVolumeClaim

PersistentVolumes (PV) represent cluster storage resources while PersistentVolumeClaims (PVC) are storage requests by users. PVCs bind to PVs matching capacity and access mode requirements.

```mermaid
%% PV and PVC binding
graph TD
    A[PersistentVolume<br/>10Gi, ReadWriteOnce] --> B{Binding}
    C[PersistentVolumeClaim<br/>5Gi, ReadWriteOnce] --> B
    B --> D[Bound State]
    D --> E[Pod Mounts PVC]

    style A fill:#CC78BC,color:#000
    style C fill:#DE8F05,color:#000
    style D fill:#029E73,color:#fff
    style E fill:#0173B2,color:#fff
```

```yaml
apiVersion: v1
kind: PersistentVolume
metadata:
  name: pv-example # => PV name (cluster-wide resource)
spec:
  capacity:
    storage: 10Gi # => Total storage capacity
  accessModes:
    - ReadWriteOnce # => Single node read-write
      # => ReadOnlyMany: multiple nodes read-only
      # => ReadWriteMany: multiple nodes read-write
  persistentVolumeReclaimPolicy:
    Retain
    # => Retain: manual reclamation after PVC deletion
    # => Delete: auto-delete storage (cloud volumes)
    # => Recycle: deprecated (use Delete)
  storageClassName: manual # => Storage class name for binding
  hostPath: # => Host path volume (testing only)
    path: /mnt/data
    type: DirectoryOrCreate

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: pvc-example # => PVC name (namespace resource)
spec:
  accessModes:
    - ReadWriteOnce # => Must match PV access mode
  resources:
    requests:
      storage: 5Gi # => Requests 5 GiB (PV has 10 GiB)
  storageClassName: manual # => Binds to PV with same storage class

---
apiVersion: v1
kind: Pod
metadata:
  name: pv-pod
spec:
  containers:
    - name: app
      image: nginx:1.24
      volumeMounts:
        - name: storage
          mountPath: /usr/share/nginx/html
  volumes:
    - name: storage
      persistentVolumeClaim:
        claimName: pvc-example # => References PVC


# PV/PVC lifecycle:
# => kubectl get pv → shows PV status (Available → Bound)
# => kubectl get pvc → shows PVC status (Pending → Bound)
# => PVC binds to PV with matching capacity, access mode, and storage class
# => Pod mounts PVC, data persists across Pod restarts
```

**Key Takeaway**: Use PV/PVC for persistent storage across Pod restarts; cloud providers offer dynamic provisioning via StorageClasses, eliminating manual PV creation for production use.

---

### Example 45: StorageClass and Dynamic Provisioning

StorageClasses enable dynamic PersistentVolume provisioning, automatically creating storage when PVCs are created. Cloud providers offer default StorageClasses for seamless dynamic provisioning.

```yaml
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: fast-storage # => StorageClass name
provisioner:
  kubernetes.io/aws-ebs # => Cloud provider provisioner
  # => kubernetes.io/gce-pd (GCP)
  # => kubernetes.io/azure-disk (Azure)
parameters:
  type:
    gp3 # => AWS EBS type: gp3 (SSD)
    # => gp2, io1, io2, sc1, st1
  iops: "3000" # => Provisioned IOPS
  throughput: "125" # => Throughput in MiB/s
  encrypted: "true" # => Encrypt volume
reclaimPolicy:
  Delete # => Delete PV when PVC deleted
  # => Retain: keep PV after PVC deletion
volumeBindingMode:
  WaitForFirstConsumer
  # => Delay PV binding until Pod scheduled
  # => Ensures PV created in same zone as Pod
  # => Immediate: bind PV immediately
allowVolumeExpansion:
  true # => Allow PVC size increase
  # => Resize PVC without recreating

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: dynamic-pvc
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName:
    fast-storage # => References StorageClass
    # => Triggers dynamic provisioning
  resources:
    requests:
      storage: 20Gi # => Requests 20 GiB


# Dynamic provisioning:
# => PVC created → StorageClass provisions new PV automatically
# => No manual PV creation needed
# => PV deleted automatically when PVC deleted (reclaimPolicy: Delete)
# => kubectl get pv → shows auto-created PV
```

**Key Takeaway**: Use StorageClasses for production storage with dynamic provisioning; configure WaitForFirstConsumer for multi-zone clusters to ensure PV and Pod are in the same availability zone.

---

### Example 46: Volume Expansion

PersistentVolumeClaims support volume expansion when StorageClass allows it. Expand PVCs by updating the storage size; filesystem resize may require Pod restart depending on volume type.

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: expandable-pvc
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName: fast-storage # => StorageClass must have allowVolumeExpansion: true
  resources:
    requests:
      storage: 10Gi # => Initial size: 10 GiB


# Expansion process:
# 1. Edit PVC to increase size
# => kubectl edit pvc expandable-pvc
# => Change storage: 10Gi → storage: 20Gi
# => Save and exit

# 2. Check expansion status
# => kubectl get pvc expandable-pvc
# => Conditions: FileSystemResizePending (Pod restart needed)

# 3. Restart Pod to complete expansion
# => kubectl delete pod <pod-using-pvc>
# => New Pod mounts expanded volume

# 4. Verify new size
# => kubectl exec <pod> -- df -h /mount/path
# => Shows 20G total

# Expansion behavior:
# => Volume controller resizes PV
# => kubelet resizes filesystem (may require Pod restart)
# => Cannot shrink volumes (decrease size)
# => Some volume types support online expansion (no Pod restart)
```

**Key Takeaway**: Volume expansion requires StorageClass with allowVolumeExpansion enabled; most volume types require Pod restart to complete filesystem resize; plan initial PVC sizes carefully as shrinking is not supported.

---

### Example 47: Volume Snapshots

VolumeSnapshots create point-in-time copies of PersistentVolumes for backup, clone, or restore operations. Requires CSI (Container Storage Interface) driver support from storage provider.

```yaml
apiVersion: snapshot.storage.k8s.io/v1
kind: VolumeSnapshotClass
metadata:
  name: csi-snapshot-class
driver:
  ebs.csi.aws.com # => CSI driver (AWS EBS example)
  # => pd.csi.storage.gke.io (GCP)
  # => disk.csi.azure.com (Azure)
deletionPolicy:
  Delete # => Delete snapshot when VolumeSnapshot deleted
  # => Retain: keep snapshot

---
apiVersion: snapshot.storage.k8s.io/v1
kind: VolumeSnapshot
metadata:
  name: pvc-snapshot
spec:
  volumeSnapshotClassName: csi-snapshot-class
  source:
    persistentVolumeClaimName:
      data-pvc
      # => Source PVC to snapshot
      # => Creates snapshot of current state


# Snapshot lifecycle:
# => kubectl get volumesnapshot
# => Shows: READYTOUSE=true after snapshot completes
# => Snapshot stored in cloud provider storage
# => Independent of source PVC lifecycle

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: restored-pvc
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName: fast-storage
  resources:
    requests:
      storage: 10Gi
  dataSource:
    kind: VolumeSnapshot # => Restore from snapshot
    name: pvc-snapshot # => References snapshot
    apiGroup: snapshot.storage.k8s.io

# Restore process:
# => PVC created from snapshot
# => Data restored from snapshot to new PV
# => Original PVC unaffected
```

**Key Takeaway**: Use VolumeSnapshots for backup and disaster recovery; requires CSI driver support; create snapshots before major changes for easy rollback; consider snapshot costs and retention policies.

---

### Example 48: Local Persistent Volumes

Local PersistentVolumes use node-local storage (SSDs, NVMe) for high-performance workloads requiring low latency. Pods using local volumes are bound to specific nodes.

```yaml
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: local-storage
provisioner:
  kubernetes.io/no-provisioner
  # => No dynamic provisioning
  # => PVs must be created manually
volumeBindingMode:
  WaitForFirstConsumer
  # => Essential for local volumes
  # => Ensures Pod scheduled on node with PV

---
apiVersion: v1
kind: PersistentVolume
metadata:
  name: local-pv
spec:
  capacity:
    storage: 100Gi
  accessModes:
    - ReadWriteOnce # => Local volumes always ReadWriteOnce
  persistentVolumeReclaimPolicy: Retain
  storageClassName: local-storage
  local:
    path: /mnt/disks/ssd1 # => Path on node
  nodeAffinity: # => Required for local volumes
    required:
      nodeSelectorTerms:
        - matchExpressions:
            - key: kubernetes.io/hostname
              operator: In
              values:
                - node-1 # => PV available only on node-1

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: local-pvc
spec:
  accessModes:
    - ReadWriteOnce
  storageClassName: local-storage
  resources:
    requests:
      storage: 100Gi

# Local PV behavior:
# => Pod using local-pvc scheduled on node-1 (PV location)
# => No cross-node portability (Pod stuck on node-1)
# => Highest performance (local SSD/NVMe)
# => Risk: node failure means data loss (use replication)
```

**Key Takeaway**: Use local PersistentVolumes for latency-sensitive workloads like databases; understand trade-off between performance and availability; implement application-level replication for fault tolerance.

---

## Resource Limits (Examples 49-53)

### Example 49: QoS Classes

Kubernetes assigns Quality of Service (QoS) classes based on resource requests and limits, affecting eviction priority during resource pressure. Guaranteed (highest priority), Burstable (medium), BestEffort (lowest).

```yaml
# Guaranteed QoS (highest priority, last to be evicted)
apiVersion: v1
kind: Pod
metadata:
  name: guaranteed-pod
spec:
  containers:
    - name: app
      image: nginx:1.24
      resources:
        requests:
          cpu: 500m # => requests.cpu = limits.cpu
          memory: 512Mi # => requests.memory = limits.memory
        limits:
          cpu: 500m # => Must be equal for Guaranteed QoS
          memory: 512Mi

---
# Burstable QoS (medium priority)
apiVersion: v1
kind: Pod
metadata:
  name: burstable-pod
spec:
  containers:
    - name: app
      image: nginx:1.24
      resources:
        requests:
          cpu: 250m # => requests < limits
          memory: 256Mi
        limits:
          cpu: 500m # => Can burst up to limits
          memory: 512Mi # => Evicted before Guaranteed, after BestEffort

---
# BestEffort QoS (lowest priority, first to be evicted)
apiVersion: v1
kind: Pod
metadata:
  name: besteffort-pod
spec:
  containers:
    - name: app
      image:
        nginx:1.24 # => No requests or limits specified
        # => Gets no resource guarantees
        # => First evicted during resource pressure

# QoS behavior during resource pressure:
# => Node runs low on memory
# => 1. BestEffort Pods evicted first
# => 2. Burstable Pods evicted (using most memory relative to requests)
# => 3. Guaranteed Pods evicted last (only if critical system processes need resources)
```

**Key Takeaway**: Set requests equal to limits for Guaranteed QoS on critical workloads; use Burstable for applications with variable load; avoid BestEffort in production except for truly optional workloads.

---

### Example 50: Pod Priority and Preemption

PriorityClasses assign priority values to Pods, enabling preemption where higher-priority Pods can evict lower-priority Pods when cluster resources are scarce.

```yaml
apiVersion: scheduling.k8s.io/v1
kind: PriorityClass
metadata:
  name: high-priority # => PriorityClass name
value:
  1000000 # => Priority value (higher = more important)
  # => System priorities: 2000000000+ (reserved)
globalDefault:
  false # => Not default priority
  # => Set true for one PriorityClass
description: "High priority for critical services"

---
apiVersion: scheduling.k8s.io/v1
kind: PriorityClass
metadata:
  name: low-priority
value: 100 # => Lower priority value
globalDefault: false
description: "Low priority for batch jobs"

---
apiVersion: v1
kind: Pod
metadata:
  name: critical-pod
spec:
  priorityClassName: high-priority # => Uses high-priority PriorityClass
  containers:
    - name: nginx
      image: nginx:1.24

# Preemption behavior:
# => Cluster has no capacity
# => critical-pod (priority 1000000) needs scheduling
# => Scheduler evicts low-priority Pods to make room
# => critical-pod scheduled on freed resources
# => Evicted Pods: status=Failed, reason=Preempted
# => kubectl describe pod shows: Status: Failed, Reason: Preempted
```

**Key Takeaway**: Use PriorityClasses to ensure critical workloads schedule before less important ones; preemption allows cluster to prioritize essential services during resource contention; avoid too many priority levels for simplicity.

---

### Example 51: Horizontal Pod Autoscaler

HorizontalPodAutoscaler (HPA) automatically scales Deployment/ReplicaSet replicas based on CPU utilization or custom metrics. Requires metrics-server for CPU/memory metrics.

```yaml
# Install metrics-server first:
# => kubectl apply -f https://github.com/kubernetes-sigs/metrics-server/releases/latest/download/components.yaml

apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: web-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: web-app # => Target Deployment to scale
  minReplicas: 2 # => Minimum replicas (never scale below)
  maxReplicas: 10 # => Maximum replicas (never scale above)
  metrics:
    - type: Resource
      resource:
        name: cpu
        target:
          type: Utilization
          averageUtilization:
            70 # => Target 70% average CPU utilization
            # => Above 70% → scale up
            # => Below 70% → scale down

# HPA behavior:
# => Checks metrics every 15 seconds (default)
# => Current CPU: 85% (above 70% target)
# => Calculates: desiredReplicas = ceil(currentReplicas * currentMetric / targetMetric)
# => Example: ceil(2 * 85 / 70) = ceil(2.43) = 3 replicas
# => Gradually scales up to avoid flapping
# => kubectl get hpa shows: TARGETS=85%/70%, REPLICAS=3
```

**Key Takeaway**: Use HPA for automatic scaling based on demand; set appropriate min/max replicas to prevent over-scaling costs or under-scaling unavailability; requires resource requests for CPU metrics.

---

### Example 52: Vertical Pod Autoscaler

VerticalPodAutoscaler (VPA) automatically adjusts Pod resource requests and limits based on actual usage, optimizing resource allocation. VPA can operate in recommendation-only or auto-update mode.

```yaml
# Install VPA:
# => git clone https://github.com/kubernetes/autoscaler.git
# => cd autoscaler/vertical-pod-autoscaler
# => ./hack/vpa-up.sh

apiVersion: autoscaling.k8s.io/v1
kind: VerticalPodAutoscaler
metadata:
  name: web-vpa
spec:
  targetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: web-app # => Target Deployment
  updatePolicy:
    updateMode:
      Auto # => Auto: apply recommendations (restart Pods)
      # => Recreate: same as Auto
      # => Initial: only set on Pod creation
      # => Off: recommendations only, no updates
  resourcePolicy:
    containerPolicies:
      - containerName: "*" # => Applies to all containers
        minAllowed:
          cpu: 100m # => Minimum CPU request
          memory: 128Mi # => Minimum memory request
        maxAllowed:
          cpu: 2000m # => Maximum CPU request
          memory: 2Gi # => Maximum memory request
        controlledResources:
          - cpu
          - memory

# VPA behavior:
# => Monitors actual resource usage
# => Recommends new requests/limits
# => Auto mode: updates Pods with new resources (triggers restart)
# => kubectl get vpa web-vpa shows recommendations
# => kubectl describe vpa web-vpa shows: RECOMMENDATION, LOWER BOUND, UPPER BOUND
```

**Key Takeaway**: Use VPA to right-size resource requests automatically; prefer HPA for horizontal scaling, VPA for vertical sizing; avoid using HPA and VPA on CPU/memory simultaneously to prevent conflicts.

---

### Example 53: Pod Disruption Budget

PodDisruptionBudgets (PDB) limit voluntary disruptions (node drains, upgrades) to ensure minimum availability during maintenance. PDBs prevent kubectl drain from evicting too many Pods simultaneously.

```yaml
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: web-pdb
spec:
  minAvailable:
    2 # => Minimum available Pods during disruption
    # => Alternative: maxUnavailable: 1
  selector:
    matchLabels:
      app: web # => Applies to Pods with app=web label


# PDB behavior:
# => Deployment has 4 replicas
# => kubectl drain node-1 (3 Pods on node-1)
# => PDB allows eviction of 2 Pods only (keeps minAvailable=2)
# => Drain operation waits for evicted Pods to reschedule
# => After new Pods ready, drain continues
# => Prevents complete service unavailability during maintenance

---
# Alternative: maxUnavailable
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: api-pdb
spec:
  maxUnavailable:
    1 # => Maximum unavailable Pods
    # => More flexible than minAvailable
  selector:
    matchLabels:
      app: api

# maxUnavailable vs minAvailable:
# => maxUnavailable: "at most N Pods down"
# => minAvailable: "at least N Pods up"
# => Use maxUnavailable for percentage-based limits: maxUnavailable: 25%
```

**Key Takeaway**: Use PodDisruptionBudgets to maintain availability during voluntary disruptions like node maintenance; set minAvailable or maxUnavailable based on application requirements; PDBs do not prevent involuntary disruptions like node failures.

---

## Health Checks (Examples 54-57)

### Example 54: Readiness Probe

Readiness probes determine when Pods are ready to receive traffic. Failed readiness checks remove Pods from Service endpoints without restarting them, useful during startup or temporary unavailability.

```mermaid
%% Readiness probe traffic control
graph LR
    A[Service: web-service] --> B{Readiness Probe}
    B -->|Pass| C[Pod: web-1<br/>Receives Traffic]
    B -->|Fail| D[Pod: web-2<br/>No Traffic]

    style A fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style D fill:#CC78BC,color:#000
```

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: readiness-pod
  labels:
    app: web
spec:
  containers:
    - name: nginx
      image: nginx:1.24
      ports:
        - containerPort: 80
      readinessProbe: # => Checks if Pod ready for traffic
        httpGet:
          path: /ready # => Endpoint returning 200 when ready
          port: 80
        initialDelaySeconds: 5 # => Wait 5s after start before first probe
        periodSeconds: 5 # => Probe every 5 seconds
        successThreshold: 1 # => 1 success → mark Ready
        failureThreshold:
          3 # => 3 failures → mark NotReady
          # => Pod removed from Service endpoints

# Readiness vs Liveness:
# => Readiness failure → removes from Service, no restart
# => Liveness failure → restarts container
# => Use readiness for temporary unavailability (loading data, dependencies down)
# => kubectl get pods shows: READY=0/1 (readiness failed)
# => kubectl describe pod shows: Readiness probe failed
```

**Key Takeaway**: Use readiness probes to prevent traffic to Pods that are starting up or temporarily unavailable; failed readiness checks remove Pods from load balancing without restarting them.

---

### Example 55: Startup Probe

Startup probes give slow-starting containers extra time to initialize before liveness probes begin. This prevents premature restart of applications with long initialization (legacy apps, large datasets).

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: startup-pod
spec:
  containers:
    - name: slow-app
      image: slow-starting-app:1.0
      ports:
        - containerPort: 8080
      startupProbe: # => Checks if application started
        httpGet:
          path: /startup
          port: 8080
        initialDelaySeconds: 10
        periodSeconds: 10 # => Check every 10 seconds
        failureThreshold:
          30 # => Allow 30 failures = 300 seconds (5 min)
          # => After 5 min → kubelet restarts container

      livenessProbe: # => Begins after startup probe succeeds
        httpGet:
          path: /healthz
          port: 8080
        periodSeconds: 5 # => Frequent checks after startup
        failureThreshold: 3 # => Restart after 15 seconds of failure


# Probe sequence:
# 1. Container starts
# 2. Startup probe checks every 10s (up to 5 min)
# 3. First startup probe success → startup complete
# 4. Liveness probe begins checking every 5s
# 5. Readiness probe (if configured) controls traffic

# Without startup probe:
# => Slow app takes 3 min to start
# => Liveness probe failureThreshold=3, periodSeconds=5 → fails after 15s
# => Container restarted repeatedly (CrashLoopBackOff)
# => Startup probe gives sufficient time for initialization
```

**Key Takeaway**: Use startup probes for slow-starting applications to prevent premature liveness probe failures; configure longer failureThreshold \* periodSeconds than application startup time; liveness probes begin only after startup success.

---

### Example 56: Combined Health Checks

Production Pods should use all three probes: startup for initialization, liveness for deadlock recovery, and readiness for traffic control. This combination ensures robust health monitoring.

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: production-app
spec:
  replicas: 3
  selector:
    matchLabels:
      app: production
  template:
    metadata:
      labels:
        app: production
    spec:
      containers:
        - name: app
          image: production-app:1.0
          ports:
            - containerPort: 8080

          startupProbe: # => Phase 1: Initialization (0-2 min)
            httpGet:
              path: /startup
              port: 8080
            initialDelaySeconds: 10
            periodSeconds: 10
            failureThreshold: 12 # => 120s max startup time

          livenessProbe: # => Phase 2: Deadlock detection
            httpGet:
              path: /healthz
              port: 8080
            initialDelaySeconds: 0 # => Starts after startup succeeds
            periodSeconds: 10
            timeoutSeconds: 5
            failureThreshold: 3 # => Restart after 30s failure

          readinessProbe: # => Phase 3: Traffic control
            httpGet:
              path: /ready
              port: 8080
            initialDelaySeconds: 0
            periodSeconds: 5 # => More frequent than liveness
            timeoutSeconds: 3
            successThreshold: 1
            failureThreshold: 2 # => Remove from Service after 10s

          resources:
            requests:
              cpu: 250m
              memory: 256Mi
            limits:
              cpu: 500m
              memory: 512Mi

# Health check endpoints should return:
# => /startup: 200 when initialization complete (DB connected, cache loaded)
# => /healthz: 200 when application functional (no deadlocks, core services ok)
# => /ready: 200 when ready for traffic (dependencies healthy, not overloaded)
```

**Key Takeaway**: Implement all three probe types for production workloads; startup for slow initialization, liveness for crash recovery, readiness for traffic control; design separate health check endpoints with appropriate logic for each probe type.

---

### Example 57: Probe Handlers

Kubernetes supports three probe handlers: HTTP GET, TCP socket, and exec command. Choose appropriate handler based on application capabilities and health check requirements.

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: probe-handlers
spec:
  containers:
    # HTTP GET probe (most common)
    - name: web-app
      image: nginx:1.24
      livenessProbe:
        httpGet:
          path: /healthz # => Sends HTTP GET request
          port: 80
          httpHeaders:
            - name: Custom-Header
              value: HealthCheck # => Optional custom headers
          scheme: HTTP # => HTTP or HTTPS
        periodSeconds: 10

    # TCP socket probe (for non-HTTP services)
    - name: database
      image: postgres:15
      livenessProbe:
        tcpSocket:
          port:
            5432 # => Attempts TCP connection
            # => Success if port open
        periodSeconds: 10

    # Exec probe (command-based)
    - name: cache
      image: redis:7
      livenessProbe:
        exec:
          command: # => Runs command inside container
            - redis-cli
            - ping # => Exit 0 = success, non-zero = failure
        periodSeconds: 10

# Probe handler selection:
# => HTTP: applications with HTTP endpoints (web apps, APIs)
# => TCP: services accepting TCP connections (databases, message queues)
# => Exec: custom health checks requiring commands (CLI tools, scripts)

# Performance considerations:
# => HTTP: moderate overhead (HTTP processing)
# => TCP: minimal overhead (connection test only)
# => Exec: highest overhead (fork/exec, command execution)
```

**Key Takeaway**: Use HTTP probes for web applications with health endpoints, TCP probes for non-HTTP network services, and exec probes only when necessary due to execution overhead; prefer HTTP/TCP for performance.

---

## Summary

**Intermediate level (40-75% coverage)** covered:

- **StatefulSets** (Examples 29-33): Stateful workloads, persistent storage, ordered deployment, update strategies
- **DaemonSets & Jobs** (Examples 34-38): Node-level services, batch processing, parallel jobs, scheduled tasks
- **Ingress Controllers** (Examples 39-43): HTTP routing, TLS termination, multi-host, annotations, default backend
- **Persistent Volumes** (Examples 44-48): Storage orchestration, dynamic provisioning, expansion, snapshots, local storage
- **Resource Limits** (Examples 49-53): QoS classes, priority, HPA, VPA, disruption budgets
- **Health Checks** (Examples 54-57): Readiness, startup, liveness probes, probe handlers

**Next steps**:

- Continue with [Advanced Examples](/en/learn/software-engineering/infrastructure/tools/kubernetes/tutorials/by-example/advanced) (Examples 58-85) for expert mastery
- Or review [Beginner Examples](/en/learn/software-engineering/infrastructure/tools/kubernetes/tutorials/by-example/beginner) (Examples 1-28) for fundamentals

All examples are self-contained and production-ready. Happy learning!
