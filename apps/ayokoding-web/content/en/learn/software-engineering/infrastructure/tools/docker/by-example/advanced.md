---
title: "Advanced"
date: 2025-12-29T23:43:13+07:00
draft: false
weight: 10000003
description: "Examples 55-79: Docker Swarm orchestration, security hardening, CI/CD integration, production patterns (75-95% coverage)"
tags: ["docker", "tutorial", "by-example", "advanced", "swarm", "security", "production"]
---

### Examples 55-79: Production Mastery

This chapter covers advanced Docker patterns through 25 examples, achieving 75-95% coverage. You'll learn Docker Swarm basics, security best practices, registry operations, CI/CD integration, and production deployment patterns.

---

### Example 55: Docker Swarm Initialization

Docker Swarm enables orchestrating containers across multiple hosts with built-in load balancing, service discovery, and rolling updates.

```mermaid
%% Swarm cluster architecture
graph TD
    A["Manager Node<br/>192.168.1.10"] --> B["Worker Node 1<br/>192.168.1.11"]
    A --> C["Worker Node 2<br/>192.168.1.12"]
    A --> D["Worker Node 3<br/>192.168.1.13"]
    A -.->|"Raft consensus"| E["Manager Node 2<br/>192.168.1.14"]
    A -.->|"Raft consensus"| F["Manager Node 3<br/>192.168.1.15"]

    style A fill:#0173B2,color:#fff
    style E fill:#0173B2,color:#fff
    style F fill:#0173B2,color:#fff
    style B fill:#029E73,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
```

```bash
# Initialize swarm on manager node (first node)
docker swarm init --advertise-addr 192.168.1.10
# => Swarm initialized: current node (abc123) is now a manager.
# => To add a worker to this swarm, run the following command:
# =>     docker swarm join --token SWMTKN-1-xxx 192.168.1.10:2377
# => To add a manager to this swarm, run 'docker swarm join-token manager'

# Get join token for worker nodes
docker swarm join-token worker
# => docker swarm join --token SWMTKN-1-worker-xxx 192.168.1.10:2377

# Get join token for manager nodes
docker swarm join-token manager
# => docker swarm join --token SWMTKN-1-manager-xxx 192.168.1.10:2377

# On worker nodes: Join swarm as worker
docker swarm join --token SWMTKN-1-worker-xxx 192.168.1.10:2377
# => This node joined a swarm as a worker.

# On additional manager nodes: Join swarm as manager
docker swarm join --token SWMTKN-1-manager-xxx 192.168.1.10:2377
# => This node joined a swarm as a manager.

# List swarm nodes (from manager)
docker node ls
# => ID              HOSTNAME   STATUS  AVAILABILITY  MANAGER STATUS
# => abc123def456 *  manager1   Ready   Active        Leader
# => def456ghi789    manager2   Ready   Active        Reachable
# => ghi789jkl012    manager3   Ready   Active        Reachable
# => jkl012mno345    worker1    Ready   Active
# => mno345pqr678    worker2    Ready   Active
# => pqr678stu901    worker3    Ready   Active
# => * indicates current node
# => Leader = primary manager (Raft consensus)

# Inspect swarm cluster
docker info | grep -A 10 Swarm
# => Swarm: active
# =>  NodeID: abc123def456
# =>  Is Manager: true
# =>  ClusterID: xyz789uvw012
# =>  Managers: 3
# =>  Nodes: 6
# =>  Default Address Pool: 10.0.0.0/8
# =>  SubnetSize: 24
# =>  Orchestration:
# =>   Task History Retention Limit: 5

# Promote worker to manager
docker node promote worker1
# => Node worker1 promoted to a manager in the swarm.

# Demote manager to worker
docker node demote manager3
# => Manager manager3 demoted in the swarm.

# Drain node (stop scheduling new tasks)
docker node update --availability drain worker1
# => worker1 availability set to drain
# => Existing tasks are moved to other nodes

# Activate drained node
docker node update --availability active worker1
# => worker1 availability set to active

# Add label to node (for task placement constraints)
docker node update --label-add environment=production worker1
docker node update --label-add datacenter=us-east worker1
# => Labels added to worker1

# Inspect node details
docker node inspect worker1 --pretty
# => ID:                  jkl012mno345
# => Hostname:            worker1
# => Status:
# =>  State:              Ready
# =>  Availability:       Active
# => Platform:
# =>  OS:                 linux
# =>  Architecture:       x86_64
# => Labels:
# =>  environment=production
# =>  datacenter=us-east

# Leave swarm (from worker node)
docker swarm leave
# => Node left the swarm.

# Leave swarm (from manager node - requires force)
docker swarm leave --force
# => Manager left the swarm. Cluster may be unstable.

# Remove node from swarm (from active manager)
docker node rm worker1
# => worker1 removed from swarm
```

**Key Takeaway**: Docker Swarm provides built-in orchestration without external dependencies. Use odd number of managers (3, 5, 7) for Raft consensus quorum. Label nodes for targeted task placement. Always maintain manager quorum for high availability.

**Why It Matters**: Docker Swarm enables production container orchestration without the operational complexity of Kubernetes, making it ideal for teams needing orchestration but lacking dedicated DevOps resources. The built-in Raft consensus algorithm provides automatic leader election and high availability - when a manager fails, another is elected in seconds without manual intervention. Companies running 50-500 containers often choose Swarm over Kubernetes because it delivers 80% of orchestration benefits with 20% of the operational overhead, allowing small teams to achieve enterprise-grade availability.

---

### Example 56: Docker Swarm Services

Swarm services define desired state for containerized applications. Swarm maintains replica count and handles failures automatically.

**Service Distribution and Ingress Routing:**

```mermaid
%% Swarm service distribution
graph TD
    A["Client Request<br/>worker1:8080"] --> B["Ingress Routing Mesh"]
    B --> C1["web.1<br/>worker1"]
    B --> C2["web.2<br/>worker2"]
    B --> C3["web.3<br/>worker3"]
    C1 --> D["Backend API<br/>5 replicas"]
    C2 --> D
    C3 --> D
    D --> E["Database<br/>1 replica on worker1"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C1 fill:#029E73,color:#fff
    style C2 fill:#029E73,color:#fff
    style C3 fill:#029E73,color:#fff
    style E fill:#CC78BC,color:#000
```

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  web:
    image: nginx:alpine
    deploy:
      replicas: 3
      # => Maintains 3 running replicas across swarm
      update_config:
        parallelism: 1
        # => Update 1 replica at a time
        delay: 10s
        # => Wait 10 seconds between updates
        failure_action: rollback
        # => Rollback on update failure
      restart_policy:
        condition: on-failure
        delay: 5s
        max_attempts: 3
      placement:
        constraints:
          - node.role == worker
          # => Only schedule on worker nodes
        preferences:
          - spread: node.labels.datacenter
          # => Spread across different datacenters
    ports:
      - "8080:80"
      # => Published port accessible on all nodes (ingress routing mesh)
    networks:
      - frontend

  api:
    image: my-api:latest
    deploy:
      replicas: 5
      resources:
        limits:
          cpus: "0.5"
          memory: 512M
        reservations:
          cpus: "0.25"
          memory: 256M
      update_config:
        parallelism: 2
        delay: 5s
        order: start-first
        # => Start new task before stopping old (zero-downtime)
      placement:
        constraints:
          - node.labels.environment == production
    networks:
      - frontend
      - backend

  database:
    image: postgres:15-alpine
    deploy:
      replicas: 1
      # => Stateful service: single replica
      placement:
        constraints:
          - node.hostname == worker1
          # => Pin to specific node (stateful data)
    environment:
      POSTGRES_PASSWORD: secret
    volumes:
      - db-data:/var/lib/postgresql/data
    networks:
      - backend

networks:
  frontend:
    driver: overlay
    # => Overlay network spans all swarm nodes
  backend:
    driver: overlay
    internal: true
    # => Internal-only (no external access)

volumes:
  db-data:
    driver: local
```

```bash
# Deploy stack to swarm
docker stack deploy -c docker-compose.yml myapp
# => Creating network myapp_frontend
# => Creating network myapp_backend
# => Creating service myapp_web
# => Creating service myapp_api
# => Creating service myapp_database

# List services
docker service ls
# => ID            NAME            MODE        REPLICAS  IMAGE
# => abc123def456  myapp_web       replicated  3/3       nginx:alpine
# => def456ghi789  myapp_api       replicated  5/5       my-api:latest
# => ghi789jkl012  myapp_database  replicated  1/1       postgres:15-alpine

# Inspect service
docker service ps myapp_web
# => ID        NAME          NODE      DESIRED STATE  CURRENT STATE
# => abc123    myapp_web.1   worker1   Running        Running 2 minutes ago
# => def456    myapp_web.2   worker2   Running        Running 2 minutes ago
# => ghi789    myapp_web.3   worker3   Running        Running 2 minutes ago

# Scale service manually
docker service scale myapp_api=10
# => myapp_api scaled to 10
# => Swarm creates 5 additional replicas across nodes

docker service ps myapp_api --filter desired-state=running
# => Shows 10 running replicas distributed across worker nodes

# Update service image (rolling update)
docker service update --image my-api:v2 myapp_api
# => myapp_api
# => overall progress: 5 out of 10 tasks
# => 1/10: running   [====================>]
# => 2/10: running   [====================>]
# => Updates 2 replicas at a time (parallelism: 2)
# => Zero-downtime deployment (order: start-first)

# Rollback service update
docker service rollback myapp_api
# => myapp_api
# => rollback: manually requested rollback
# => overall progress: rolling back update

# View service logs (aggregated from all replicas)
docker service logs -f myapp_web
# => myapp_web.1.abc123 | 192.168.1.50 - - [29/Dec/2025:11:20:00] "GET / HTTP/1.1" 200
# => myapp_web.2.def456 | 192.168.1.51 - - [29/Dec/2025:11:20:01] "GET / HTTP/1.1" 200
# => myapp_web.3.ghi789 | 192.168.1.52 - - [29/Dec/2025:11:20:02] "GET / HTTP/1.1" 200

# Test ingress routing mesh (access service from any node)
curl http://worker1:8080
# => <!DOCTYPE html>... (nginx welcome page from any replica)

curl http://worker2:8080
# => <!DOCTYPE html>... (load balanced across all replicas)

# Remove stack
docker stack rm myapp
# => Removing service myapp_database
# => Removing service myapp_api
# => Removing service myapp_web
# => Removing network myapp_backend
# => Removing network myapp_frontend
```

**Key Takeaway**: Swarm services maintain desired replica count automatically. Use overlay networks for cross-node communication. Ingress routing mesh makes services accessible on all nodes regardless of where replicas run. Configure rolling updates with `parallelism` and `order` for zero-downtime deployments.

**Why It Matters**: Swarm services implement declarative infrastructure - you specify desired state and Swarm maintains it automatically, restarting failed containers in seconds without manual intervention. The ingress routing mesh enables zero-configuration load balancing across all nodes, eliminating need for external load balancers in small deployments. The built-in rolling update mechanism with automatic rollback prevents bad deployments from causing downtime - if new version fails health checks, Swarm automatically reverts to previous version, protecting production availability.

---

### Example 57: Docker Secrets Management

Docker secrets provide secure credential distribution to swarm services. Secrets are encrypted at rest and in transit, mounted as files in containers.

**Secrets Distribution Flow:**

```mermaid
%% Docker secrets flow
graph TD
    A["Create Secret<br/>docker secret create"] --> B["Raft Store<br/>Encrypted at rest"]
    B --> C["Swarm Manager<br/>Leader"]
    C --> D1["Worker Node 1<br/>TLS encrypted"]
    C --> D2["Worker Node 2<br/>TLS encrypted"]
    D1 --> E1["Container<br/>/run/secrets/db_password"]
    D2 --> E2["Container<br/>/run/secrets/api_key"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#0173B2,color:#fff
    style E1 fill:#029E73,color:#fff
    style E2 fill:#029E73,color:#fff
```

```bash
# Create secret from file
echo "db_password_value" | docker secret create db_password -
# => Secret created: db_password

# Create secret from existing file
echo "api_key_12345" > api_key.txt
docker secret create api_key api_key.txt
rm api_key.txt  # Remove plaintext file
# => Secret created: api_key

# List secrets
docker secret ls
# => ID             NAME         CREATED         UPDATED
# => abc123def456   db_password  2 minutes ago   2 minutes ago
# => def456ghi789   api_key      1 minute ago    1 minute ago

# Inspect secret (data NOT shown)
docker secret inspect db_password
# => [
# =>   {
# =>     "ID": "abc123def456",
# =>     "Version": { "Index": 10 },
# =>     "CreatedAt": "2025-12-29T11:25:00Z",
# =>     "UpdatedAt": "2025-12-29T11:25:00Z",
# =>     "Spec": {
# =>       "Name": "db_password",
# =>       "Labels": {}
# =>     }
# =>   }
# => ]
# => Secret value NOT visible (encrypted)

# Create service using secrets
docker service create \
  --name postgres \
  --secret db_password \
  --env POSTGRES_PASSWORD_FILE=/run/secrets/db_password \
  postgres:15-alpine
# => Mounts secret at /run/secrets/db_password
# => Read-only, in-memory filesystem (tmpfs)

# Verify secret mount inside container
docker exec $(docker ps -q -f name=postgres) ls -l /run/secrets/
# => total 0
# => -r--r--r-- 1 root root 18 Dec 29 11:26 db_password

docker exec $(docker ps -q -f name=postgres) cat /run/secrets/db_password
# => db_password_value

# Use secrets in docker-compose (swarm mode)
cat > docker-compose.yml << 'EOF'
version: '3.8'

services:
  database:
    image: postgres:15-alpine
    secrets:
      - db_password
      # => Mounts secret at /run/secrets/db_password
    environment:
      POSTGRES_PASSWORD_FILE: /run/secrets/db_password
      # => PostgreSQL reads password from file
    deploy:
      replicas: 1

  api:
    image: my-api
    secrets:
      - source: api_key
        target: /run/secrets/api_key
        # => Custom mount path
        uid: '1000'
        gid: '1000'
        mode: 0400
        # => File permissions: read-only for uid 1000
    environment:
      API_KEY_FILE: /run/secrets/api_key
    deploy:
      replicas: 3

secrets:
  db_password:
    external: true
    # => Uses existing secret (created with docker secret create)
  api_key:
    external: true
EOF

# Deploy stack with secrets
docker stack deploy -c docker-compose.yml myapp
# => Services automatically get secrets mounted

# Update secret (requires service recreation)
docker service update \
  --secret-rm db_password \
  --secret-add source=db_password_new,target=db_password \
  myapp_database
# => Removes old secret, adds new secret
# => Service recreated with new secret

# Rotate secret safely
echo "new_password" | docker secret create db_password_v2 -
docker service update \
  --secret-rm db_password \
  --secret-add source=db_password_v2,target=db_password \
  myapp_database
# => Zero-downtime secret rotation
# => Old tasks use old secret until replaced

# Remove secret (must not be in use)
docker secret rm api_key
# => Error: secret 'api_key' is in use by service 'myapp_api'

# Remove service first, then secret
docker service rm myapp_api
docker secret rm api_key
# => api_key removed
```

**Key Takeaway**: Always use Docker secrets for sensitive data in swarm mode. Secrets are encrypted and only accessible to services that explicitly request them. Never use environment variables for passwords in production. Rotate secrets safely using service update with secret-rm and secret-add.

**Why It Matters**: Secrets management is critical for regulatory compliance with standards like PCI DSS, HIPAA, and SOC 2, which prohibit storing credentials in environment variables or configuration files. Docker secrets encrypt sensitive data at rest in the Raft consensus store and in transit over TLS, meeting security audit requirements. The secret rotation mechanism enables zero-downtime credential updates, allowing you to respond to security incidents without service interruption - when credentials are compromised, you can rotate them across hundreds of containers in minutes without downtime.

---

### Example 58: Read-Only Root Filesystem

Running containers with read-only root filesystem significantly improves security by preventing unauthorized file modifications.

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  api:
    image: my-api:latest
    read_only: true
    # => Root filesystem is read-only
    # => Prevents malicious code from modifying system files
    tmpfs:
      - /tmp:size=100M,mode=1777
      # => Writable temporary directory in memory
      - /var/run:size=10M,mode=755
      # => Writable runtime directory
    volumes:
      - logs:/var/log/app:rw
      # => Writable volume for application logs
    environment:
      NODE_ENV: production
    deploy:
      replicas: 3

  nginx:
    image: nginx:alpine
    read_only: true
    tmpfs:
      - /var/cache/nginx:size=50M
      # => Nginx cache directory (writable)
      - /var/run:size=5M
      # => PID file location
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      # => Configuration (read-only)
      - static-content:/usr/share/nginx/html:ro
      # => Static files (read-only)
    ports:
      - "8080:80"

volumes:
  logs:
  static-content:
```

```bash
# Run container with read-only root filesystem
docker run -d --name readonly-test \
  --read-only \
  --tmpfs /tmp:size=100M \
  nginx:alpine
# => Root filesystem is read-only
# => /tmp is writable (in memory)

# Try to modify root filesystem (fails)
docker exec readonly-test sh -c 'echo "test" > /test.txt'
# => sh: can't create /test.txt: Read-only file system

# Writable tmpfs works
docker exec readonly-test sh -c 'echo "test" > /tmp/test.txt'
# => Success (tmpfs is writable)

docker exec readonly-test cat /tmp/test.txt
# => test

# Verify read-only setting
docker inspect readonly-test --format='{{.HostConfig.ReadonlyRootfs}}'
# => true

# Test application functionality
curl http://localhost:8080
# => <!DOCTYPE html>... (works normally)
# => Read-only filesystem doesn't affect normal operation

# Deploy to swarm with read-only root
docker stack deploy -c docker-compose.yml myapp

# Verify security improvement (simulate intrusion attempt)
# Attacker gains access to container shell
docker exec -it $(docker ps -q -f name=myapp_api) sh

# Inside container: Try to install malware (fails)
/ # apk add --no-cache curl
# => ERROR: Unable to lock database: Read-only file system

# Try to modify system files (fails)
/ # echo "malicious" >> /etc/passwd
# => sh: can't create /etc/passwd: Read-only file system

# Try to write to writable tmpfs (succeeds but data lost on restart)
/ # echo "temp data" > /tmp/file.txt
/ # exit

# Restart container (tmpfs data lost)
docker restart $(docker ps -q -f name=myapp_api.1)

docker exec $(docker ps -q -f name=myapp_api.1) cat /tmp/file.txt
# => cat: can't open '/tmp/file.txt': No such file or directory
# => Temporary data not persisted
```

**Key Takeaway**: Always use read-only root filesystem in production for defense-in-depth security. Provide writable tmpfs for temporary data and volumes for persistent data. This prevents attackers from modifying system files, installing malware, or persisting backdoors.

**Why It Matters**: Read-only root filesystem is defense-in-depth security that mitigates entire classes of attacks - attackers cannot install malware, modify system binaries, or persist backdoors even if they exploit application vulnerabilities. This pattern prevented damage in real-world container breaches where attackers gained shell access but could not escalate privileges or persist access. Security compliance frameworks like CIS Docker Benchmark and NIST Application Container Security Guide require read-only root filesystem for production deployments, making this pattern mandatory for regulated industries.

---

### Example 59: Dropping Linux Capabilities

Linux capabilities provide fine-grained privilege control. Drop unnecessary capabilities to minimize attack surface.

```bash
# Default capabilities (root user in container)
docker run --rm alpine sh -c 'apk add --no-cache libcap && capsh --print'
# => Current: = cap_chown,cap_dac_override,cap_fowner,cap_fsetid,cap_kill,cap_setgid,cap_setuid,cap_setpcap,cap_net_bind_service,cap_net_raw,cap_sys_chroot,cap_mknod,cap_audit_write,cap_setfcap+eip
# => Many dangerous capabilities enabled by default

# Drop all capabilities, add only necessary ones
docker run --rm \
  --cap-drop=ALL \
  --cap-add=NET_BIND_SERVICE \
  alpine sh -c 'apk add --no-cache libcap && capsh --print'
# => Current: = cap_net_bind_service+eip
# => Only NET_BIND_SERVICE capability (bind to ports < 1024)

# Example: Web server needs only NET_BIND_SERVICE
docker run -d --name web \
  --cap-drop=ALL \
  --cap-add=NET_BIND_SERVICE \
  -p 80:80 \
  nginx:alpine
# => Runs with minimal capabilities

# Try to use dropped capability (fails)
docker exec web sh -c 'apk add --no-cache libcap && capsh --print | grep cap_sys_admin'
# => (no output - CAP_SYS_ADMIN not available)

# Example: Application needs no special capabilities
docker run -d --name api \
  --cap-drop=ALL \
  --user 1000:1000 \
  -p 3000:3000 \
  my-api
# => No capabilities at all (most secure)

# Compose example with dropped capabilities
cat > docker-compose.yml << 'EOF'
version: '3.8'

services:
  web:
    image: nginx:alpine
    cap_drop:
      - ALL
    cap_add:
      - NET_BIND_SERVICE
      # => Only capability needed for binding to port 80
    ports:
      - "80:80"

  api:
    image: my-api
    cap_drop:
      - ALL
      # => No capabilities needed (runs on port > 1024)
    user: "1000:1000"
    ports:
      - "3000:3000"

  database:
    image: postgres:15-alpine
    cap_drop:
      - ALL
    cap_add:
      - CHOWN
      - DAC_OVERRIDE
      - FOWNER
      - SETGID
      - SETUID
      # => Minimal set for PostgreSQL operation
    environment:
      POSTGRES_PASSWORD: secret
EOF

# Common capability uses:
# CAP_NET_BIND_SERVICE: Bind to ports < 1024
# CAP_CHOWN: Change file ownership
# CAP_DAC_OVERRIDE: Bypass file permission checks
# CAP_FOWNER: Bypass permission checks on file operations
# CAP_SETGID: Set GID
# CAP_SETUID: Set UID
# CAP_SYS_ADMIN: Mount filesystems, admin operations (DANGEROUS - never use)
# CAP_NET_RAW: Use raw sockets (DANGEROUS - enables network attacks)

# Dangerous capabilities to NEVER add:
# CAP_SYS_ADMIN: Full system administration (container escape risk)
# CAP_SYS_MODULE: Load kernel modules (container escape risk)
# CAP_SYS_RAWIO: Direct hardware access
# CAP_SYS_PTRACE: Trace processes (can dump secrets from memory)

# Check container capabilities at runtime
docker inspect web --format='{{.HostConfig.CapDrop}}'
# => [ALL]

docker inspect web --format='{{.HostConfig.CapAdd}}'
# => [NET_BIND_SERVICE]
```

**Key Takeaway**: Always drop ALL capabilities and add only the minimum required. Most applications need NO capabilities when running on ports > 1024 with non-root user. Never add CAP_SYS_ADMIN or CAP_SYS_MODULE as they enable container escape.

**Why It Matters**: Linux capabilities provide fine-grained privilege separation that prevents container escape attacks - dropping all capabilities and adding only necessary ones reduces attack surface by 90% compared to default settings. Real-world container escapes often exploit CAP_SYS_ADMIN or CAP_SYS_MODULE to mount host filesystems or load malicious kernel modules. The principle of least privilege through capability dropping is required by security compliance frameworks and dramatically reduces blast radius if containers are compromised - attackers cannot pivot to host systems or other containers.

---

### Example 60: Image Scanning for Vulnerabilities

Container image scanning detects known vulnerabilities in base images and dependencies before deployment.

**Security Scanning Pipeline:**

```mermaid
%% Image scanning flow
graph TD
    A["Build Image"] --> B["Trivy Scan"]
    B --> C{Vulnerabilities?}
    C -->|CRITICAL/HIGH| D["Fail Build<br/>Exit code 1"]
    C -->|MEDIUM/LOW| E["Generate Report"]
    E --> F["Upload to GitHub Security"]
    F --> G["Deploy to Production"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style D fill:#CC78BC,color:#000
    style G fill:#029E73,color:#fff
```

```bash
# Install Trivy scanner
curl -sfL https://raw.githubusercontent.com/aquasecurity/trivy/main/contrib/install.sh | sh -s -- -b /usr/local/bin
# => Trivy scanner installed

# Scan image for vulnerabilities
trivy image nginx:alpine
# => 2025-12-29T11:30:00.000Z  INFO    Vulnerability scanning is enabled
# => 2025-12-29T11:30:05.000Z  INFO    Detected OS: alpine
# => 2025-12-29T11:30:05.000Z  INFO    Detected vulnerabilities: 15
# =>
# => nginx:alpine (alpine 3.19.0)
# => Total: 15 (UNKNOWN: 0, LOW: 5, MEDIUM: 8, HIGH: 2, CRITICAL: 0)
# =>
# => +------------------+------------------+----------+-------------------+
# => | LIBRARY          | VULNERABILITY ID | SEVERITY | INSTALLED VERSION |
# => +------------------+------------------+----------+-------------------+
# => | openssl          | CVE-2023-12345   | HIGH     | 3.1.0-r1          |
# => | libcurl          | CVE-2023-67890   | HIGH     | 8.4.0-r0          |
# => | zlib             | CVE-2023-11111   | MEDIUM   | 1.3-r0            |
# => +------------------+------------------+----------+-------------------+

# Scan only for HIGH and CRITICAL vulnerabilities
trivy image --severity HIGH,CRITICAL nginx:alpine
# => Shows only serious vulnerabilities

# Scan and fail build if vulnerabilities found
trivy image --exit-code 1 --severity CRITICAL nginx:alpine
# => Exit code 0: No critical vulnerabilities
# => Exit code 1: Critical vulnerabilities found (fails CI/CD)

# Scan local Dockerfile before building
trivy config Dockerfile
# => Scans Dockerfile for misconfigurations
# => Detects: Running as root, missing health checks, etc.

# Scan built image with detailed output
trivy image --format json my-app:latest > scan-results.json
# => JSON output for automated processing

# Extract vulnerability summary
cat scan-results.json | jq '.Results[0].Vulnerabilities | group_by(.Severity) | map({Severity: .[0].Severity, Count: length})'
# => [
# =>   { "Severity": "CRITICAL", "Count": 2 },
# =>   { "Severity": "HIGH", "Count": 5 },
# =>   { "Severity": "MEDIUM", "Count": 12 }
# => ]

# CI/CD integration (GitHub Actions example)
cat > .github/workflows/scan.yml << 'EOF'
name: Container Scan

on: [push]

jobs:
  scan:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build image
        run: docker build -t my-app:${{ github.sha }} .

      - name: Run Trivy vulnerability scanner
        uses: aquasecurity/trivy-action@master
        with:
          image-ref: my-app:${{ github.sha }}
          format: 'sarif'
          output: 'trivy-results.sarif'
          severity: 'CRITICAL,HIGH'
          exit-code: '1'
          # => Fails build if CRITICAL or HIGH vulnerabilities found

      - name: Upload Trivy scan results to GitHub Security tab
        uses: github/codeql-action/upload-sarif@v2
        with:
          sarif_file: 'trivy-results.sarif'
EOF

# Scan multi-stage build (scan final stage only)
trivy image --image-src my-app:latest
# => Scans only production stage, not build artifacts

# Ignore specific vulnerabilities (use with caution)
cat > .trivyignore << 'EOF'
# False positive - not applicable to our use case
CVE-2023-12345

# Accepted risk - no patch available, mitigated at network level
CVE-2023-67890
EOF

trivy image --ignorefile .trivyignore nginx:alpine
# => Skips ignored CVEs

# Scan running container
trivy container $(docker ps -q -f name=web)
# => Scans running container filesystem

# Regular scanning schedule (cron job)
cat > scan-images.sh << 'EOF'
#!/bin/bash
# Scan all running containers daily

for container in $(docker ps --format '{{.Names}}'); do
  echo "Scanning $container..."
  trivy container $container --severity HIGH,CRITICAL
done
EOF

chmod +x scan-images.sh
# => Add to cron: 0 2 * * * /path/to/scan-images.sh
```

**Key Takeaway**: Always scan images before deployment. Integrate scanning into CI/CD pipelines to fail builds on critical vulnerabilities. Use minimal base images (alpine, distroless) to reduce attack surface. Regularly scan running containers as new CVEs are discovered daily.

**Why It Matters**: Container images inherit vulnerabilities from base images and dependencies - a single unpatched library can expose entire deployments to exploitation. Automated image scanning in CI/CD pipelines prevents vulnerable images from reaching production, catching 95% of known CVEs before deployment. Organizations face regulatory penalties and breach costs averaging 4.35 million USD when vulnerable software reaches production. Continuous scanning of running containers is equally critical - new vulnerabilities are published daily, and images safe yesterday may be exploitable today.

---

### Example 61: Distroless Images for Minimal Attack Surface

Distroless images contain only application and runtime dependencies - no shell, package manager, or utilities. This drastically reduces attack surface.

```dockerfile
# File: Dockerfile.distroless (Go application)

# Build stage
FROM golang:1.21-alpine AS builder

WORKDIR /app

COPY go.mod go.sum ./
RUN go mod download

COPY . .

RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o server .
# => Static binary (no dynamic linking)

# Production stage with distroless
FROM gcr.io/distroless/static-debian12
# => No shell, no package manager, no utilities
# => Only contains: /etc/passwd, /etc/group, tzdata, ca-certificates

COPY --from=builder /app/server /server

EXPOSE 8080

USER nonroot:nonroot
# => Runs as non-root user (UID 65532)

CMD ["/server"]
# => Array syntax (no shell needed)
```

```dockerfile
# File: Dockerfile.distroless-node (Node.js application)

# Build stage
FROM node:18-alpine AS builder

WORKDIR /app

COPY package*.json ./
RUN npm ci --only=production

COPY . .
RUN npm run build

# Production stage with distroless
FROM gcr.io/distroless/nodejs18-debian12
# => Contains Node.js runtime only

COPY --from=builder /app/package.json /app/package-lock.json ./
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/dist ./dist

EXPOSE 3000

CMD ["dist/main.js"]
# => Runs node dist/main.js automatically
```

```bash
# Build distroless image
docker build -f Dockerfile.distroless -t my-app:distroless .

# Compare image sizes
docker images | grep my-app
# => my-app  alpine      15MB   (alpine base)
# => my-app  distroless  10MB   (distroless base - 33% smaller)
# => my-app  debian      80MB   (debian base - 8x larger!)

# Try to access shell (fails - no shell in distroless)
docker run --rm my-app:distroless sh
# => docker: Error response: OCI runtime create failed: exec: "sh": executable file not found

# Debugging distroless containers (use debug variant)
FROM gcr.io/distroless/static-debian12:debug
# => Includes busybox shell for debugging

docker run --rm -it my-app:distroless-debug sh
# => / # (busybox shell available)

# Inspect distroless container filesystem
docker run --rm my-app:distroless ls /
# => docker: Error response: executable file not found
# => ls command doesn't exist!

# Use multi-stage to inspect
docker build -t my-app:inspect --target builder -f Dockerfile.distroless .
docker run --rm my-app:inspect ls -la /
# => Inspects builder stage (has shell and tools)

# Scan distroless image (minimal vulnerabilities)
trivy image my-app:distroless
# => Total: 0 (UNKNOWN: 0, LOW: 0, MEDIUM: 0, HIGH: 0, CRITICAL: 0)
# => No vulnerabilities! (minimal attack surface)

# Compare with alpine
trivy image my-app:alpine
# => Total: 8 (LOW: 3, MEDIUM: 4, HIGH: 1)
# => Alpine has more packages = more vulnerabilities

# Available distroless base images:
# gcr.io/distroless/static-debian12       - Static binaries only (Go, Rust)
# gcr.io/distroless/base-debian12         - glibc, libssl, ca-certificates
# gcr.io/distroless/cc-debian12           - libc, libssl (C/C++ apps)
# gcr.io/distroless/java17-debian12       - Java 17 runtime
# gcr.io/distroless/nodejs18-debian12     - Node.js 18 runtime
# gcr.io/distroless/python3-debian12      - Python 3 runtime

# Health check in distroless (use HEALTHCHECK in Dockerfile)
FROM gcr.io/distroless/static-debian12

COPY --from=builder /app/server /server
COPY --from=builder /app/healthcheck /healthcheck

HEALTHCHECK --interval=30s --timeout=3s \
  CMD ["/healthcheck"]
# => Custom healthcheck binary (no curl/wget available)

# Logging from distroless (stdout only)
# Ensure application logs to stdout (no need for log files)
```

**Key Takeaway**: Use distroless images for maximum security in production. No shell means attackers can't run commands even if they compromise the application. Debug with `:debug` variants during development. Distroless images have near-zero vulnerabilities due to minimal contents.

**Why It Matters**: Distroless images reduce attack surface by 95% compared to traditional base images - no shell means attackers cannot run commands even after exploiting application vulnerabilities. This pattern prevented lateral movement in real breaches where attackers gained container access but could not execute reconnaissance commands or download additional malware. Distroless images typically have zero CVEs compared to 50-200 vulnerabilities in full OS images, dramatically reducing compliance burden and security patch overhead. Organizations using distroless report 80% reduction in vulnerability management effort.

---

### Example 62: User Namespaces for Privilege Isolation

User namespaces remap container root user to unprivileged user on host. Even if attacker escalates to root inside container, they have no host privileges.

```bash
# Enable user namespace remapping in Docker daemon
cat > /etc/docker/daemon.json << 'EOF'
{
  "userns-remap": "default"
}
EOF
# => Creates dockremap user/group automatically
# => Container UID 0 (root) maps to unprivileged UID on host

# Restart Docker daemon
sudo systemctl restart docker

# Check user namespace mapping
cat /etc/subuid | grep dockremap
# => dockremap:100000:65536
# => Container UIDs 0-65535 map to host UIDs 100000-165535

cat /etc/subgid | grep dockremap
# => dockremap:100000:65536
# => Container GIDs mapped similarly

# Run container with user namespace
docker run -d --name test-userns alpine sleep 3600

# Check process on host (remapped UID)
ps aux | grep sleep | grep -v grep
# => 100000   12345  0.0  0.0  1234  567  ?  Ss  11:35  0:00  sleep 3600
# => Process runs as UID 100000 on host (not 0!)

# Inside container: Check UID (appears as root)
docker exec test-userns id
# => uid=0(root) gid=0(root) groups=0(root),1(bin),2(daemon),...
# => Container sees UID 0 (root)

# On host: Verify actual UID
ps -o user,uid,pid,cmd -p $(pgrep -f "sleep 3600")
# => USER    UID   PID  CMD
# => 100000  100000  12345  sleep 3600
# => Host sees unprivileged UID

# Try to access host resources from container root
docker exec test-userns sh -c 'ls -la /hostdata'
# => ls: can't open '/hostdata': Permission denied
# => Even as "root" in container, no access to host directories

# Test privilege escalation attempt
docker exec test-userns sh -c 'echo "malicious" > /host-etc-passwd'
# => sh: can't create /host-etc-passwd: Permission denied
# => User namespace prevents host file access

# Disable user namespace for specific container (requires privilege)
docker run --userns=host -d alpine sleep 3600
# => Runs with host user namespace (dangerous - only for trusted containers)

# Compose with user namespace
cat > docker-compose.yml << 'EOF'
version: '3.8'

services:
  app:
    image: my-app
    # User namespace enabled automatically if daemon configured
    # OR explicitly disable for specific service:
    # userns_mode: "host"  # Dangerous - bypasses isolation
EOF

# Check files created by container on host
docker run --rm -v /tmp/test:/data alpine sh -c 'touch /data/file.txt'

ls -ln /tmp/test/
# => total 0
# => -rw-r--r-- 1 100000 100000 0 Dec 29 11:40 file.txt
# => File owned by remapped UID (100000), not root (0)

# Security benefit: Root exploit in container
# Attacker gains root in container
docker exec test-userns sh -c 'whoami'
# => root

# On host: Still unprivileged user
ps aux | grep test-userns | head -1
# => 100000  12345  ...
# => Host sees unprivileged UID (defense in depth)
```

**Key Takeaway**: User namespaces provide critical defense-in-depth security. Even if attacker achieves root in container, they remain unprivileged on the host. Always enable user namespaces in production unless you have specific compatibility requirements. This mitigates privilege escalation attacks.

**Why It Matters**: User namespaces are defense-in-depth security that mitigates container escape attacks - even if attackers achieve root inside container through privilege escalation exploits, they remain unprivileged on the host. This protection is critical because container isolation is not perfect - kernel vulnerabilities occasionally enable container escape, but user namespaces limit damage by ensuring escaped processes cannot modify host systems or access other containers. Security standards like CIS Docker Benchmark recommend user namespaces for all production deployments handling sensitive data.

---

### Example 63: Security Scanning in CI/CD Pipeline

Integrate security scanning into CI/CD pipelines to prevent vulnerable images from reaching production.

```yaml
# File: .github/workflows/security.yml (GitHub Actions)

name: Security Scan

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  security-scan:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v2

      - name: Build Docker image
        run: |
          docker build -t ${{ github.repository }}:${{ github.sha }} .

      - name: Run Trivy vulnerability scanner
        uses: aquasecurity/trivy-action@master
        with:
          image-ref: ${{ github.repository }}:${{ github.sha }}
          format: "sarif"
          output: "trivy-results.sarif"
          severity: "CRITICAL,HIGH"
          exit-code: "1"
          # => Fails build if CRITICAL or HIGH vulnerabilities found

      - name: Upload Trivy results to GitHub Security
        uses: github/codeql-action/upload-sarif@v2
        if: always()
        with:
          sarif_file: "trivy-results.sarif"

      - name: Run Hadolint (Dockerfile linter)
        uses: hadolint/hadolint-action@v3.1.0
        with:
          dockerfile: Dockerfile
          failure-threshold: error
          # => Fails on Dockerfile errors (not warnings)

      - name: Scan for secrets in code
        uses: trufflesecurity/trufflehog@main
        with:
          path: ./
          base: ${{ github.event.repository.default_branch }}
          head: HEAD

      - name: Docker Scout CVEs
        uses: docker/scout-action@v1
        with:
          command: cves
          image: ${{ github.repository }}:${{ github.sha }}
          only-severities: critical,high
          exit-code: true
          # => Alternative scanner using Docker Scout

      - name: Generate SBOM (Software Bill of Materials)
        run: |
          docker sbom ${{ github.repository }}:${{ github.sha }} > sbom.json

      - name: Upload SBOM artifact
        uses: actions/upload-artifact@v3
        with:
          name: sbom
          path: sbom.json
```

```yaml
# File: .gitlab-ci.yml (GitLab CI/CD)

stages:
  - build
  - security
  - deploy

variables:
  IMAGE_NAME: $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA

build:
  stage: build
  script:
    - docker build -t $IMAGE_NAME .
    - docker push $IMAGE_NAME

trivy-scan:
  stage: security
  image: aquasec/trivy:latest
  script:
    - trivy image --exit-code 1 --severity CRITICAL,HIGH $IMAGE_NAME
  allow_failure: false
  # => Blocks pipeline if vulnerabilities found

hadolint:
  stage: security
  image: hadolint/hadolint:latest
  script:
    - hadolint Dockerfile
  allow_failure: false

grype-scan:
  stage: security
  image: anchore/grype:latest
  script:
    - grype $IMAGE_NAME --fail-on high
  # => Alternative vulnerability scanner

deploy:
  stage: deploy
  script:
    - kubectl set image deployment/myapp app=$IMAGE_NAME
  only:
    - main
  when: on_success
  # => Only deploys if security scans pass
```

```bash
# Local pre-commit hook for security
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash

echo "Running security checks..."

# Scan Dockerfile
echo "Checking Dockerfile..."
docker run --rm -i hadolint/hadolint < Dockerfile
if [ $? -ne 0 ]; then
  echo "Dockerfile has issues. Fix them before committing."
  exit 1
fi

# Check for secrets
echo "Scanning for secrets..."
docker run --rm -v $(pwd):/src trufflesecurity/trufflehog:latest filesystem /src
if [ $? -ne 0 ]; then
  echo "Potential secrets found. Remove them before committing."
  exit 1
fi

echo "Security checks passed."
EOF

chmod +x .git/hooks/pre-commit
# => Runs security checks before each commit

# Manual security scan before pushing
docker build -t myapp:latest .

# Multiple scanners for comprehensive coverage
trivy image myapp:latest
grype myapp:latest
docker scout cves myapp:latest

# Generate compliance report
trivy image --format json --output report.json myapp:latest
cat report.json | jq '.Results[0].Vulnerabilities | length'
# => Total vulnerability count

# Track vulnerabilities over time
git add scan-results/$(date +%Y-%m-%d).json
# => Version-controlled vulnerability tracking
```

**Key Takeaway**: Integrate multiple security scanners into CI/CD pipelines. Fail builds on HIGH/CRITICAL vulnerabilities to prevent deployment of vulnerable images. Use Trivy, Grype, and Docker Scout for comprehensive coverage. Generate SBOMs for supply chain security compliance.

**Why It Matters**: Shift-left security through automated CI/CD scanning prevents 95% of vulnerabilities from reaching production at 10x lower cost than remediating deployed systems. Each scanner detects different vulnerability classes - Trivy excels at OS vulnerabilities, Grype at language dependencies, Docker Scout at supply chain risks. Failing builds on HIGH/CRITICAL vulnerabilities creates automated security gates that prevent teams from deploying vulnerable code, eliminating the delay and risk of manual security reviews. SBOM generation enables rapid response to zero-day vulnerabilities - when Log4Shell was announced, organizations with SBOMs identified affected systems in hours instead of weeks.

---

### Example 64: Private Docker Registry

Host private Docker registries for storing proprietary images and controlling access.

**Private Registry Architecture:**

```mermaid
%% Registry infrastructure
graph TD
    A["Developer Push<br/>docker push"] --> B["Registry<br/>Port 5000"]
    B --> C["Authentication<br/>htpasswd"]
    C --> D["TLS Encryption<br/>domain.crt"]
    D --> E["Storage Backend"]
    E --> E1["Local Disk<br/>/data"]
    E --> E2["S3 Bucket<br/>AWS"]
    B --> F["Redis Cache<br/>Blob descriptors"]
    B --> G["Registry UI<br/>Port 8080"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#CC78BC,color:#000
    style D fill:#CC78BC,color:#000
    style E2 fill:#029E73,color:#fff
```

```yaml
# File: docker-compose.yml (Private registry with authentication)

version: "3.8"

services:
  registry:
    image: registry:2
    ports:
      - "5000:5000"
    environment:
      REGISTRY_AUTH: htpasswd
      REGISTRY_AUTH_HTPASSWD_PATH: /auth/htpasswd
      REGISTRY_AUTH_HTPASSWD_REALM: Registry Realm
      REGISTRY_STORAGE_FILESYSTEM_ROOTDIRECTORY: /data
      # TLS configuration
      REGISTRY_HTTP_TLS_CERTIFICATE: /certs/domain.crt
      REGISTRY_HTTP_TLS_KEY: /certs/domain.key
    volumes:
      - registry-data:/data
      - ./auth:/auth:ro
      - ./certs:/certs:ro
    restart: unless-stopped

  registry-ui:
    image: joxit/docker-registry-ui:latest
    ports:
      - "8080:80"
    environment:
      REGISTRY_TITLE: My Private Registry
      REGISTRY_URL: https://registry.example.com:5000
      DELETE_IMAGES: "true"
      SINGLE_REGISTRY: "true"
    depends_on:
      - registry

volumes:
  registry-data:
```

```bash
# Create authentication file
mkdir -p auth certs

# Generate htpasswd file
docker run --rm \
  --entrypoint htpasswd \
  httpd:2 -Bbn admin secretpassword > auth/htpasswd
# => Creates htpasswd file with user: admin, password: secretpassword

# Generate self-signed TLS certificate (production: use Let's Encrypt)
openssl req -newkey rsa:4096 -nodes -sha256 \
  -keyout certs/domain.key -x509 -days 365 \
  -out certs/domain.crt \
  -subj "/CN=registry.example.com"
# => Creates TLS certificate for HTTPS

# Start private registry
docker compose up -d

# Trust self-signed certificate (Linux)
sudo cp certs/domain.crt /usr/local/share/ca-certificates/registry.crt
sudo update-ca-certificates
# => Adds certificate to system trust store

# Login to private registry
docker login registry.example.com:5000
# => Username: admin
# => Password: secretpassword
# => Login Succeeded

# Tag image for private registry
docker tag my-app:latest registry.example.com:5000/my-app:latest
docker tag my-app:latest registry.example.com:5000/my-app:1.0.0

# Push to private registry
docker push registry.example.com:5000/my-app:latest
docker push registry.example.com:5000/my-app:1.0.0
# => Uploads image to private registry

# List images in registry (API)
curl -u admin:secretpassword https://registry.example.com:5000/v2/_catalog
# => {"repositories":["my-app"]}

# List tags for image
curl -u admin:secretpassword https://registry.example.com:5000/v2/my-app/tags/list
# => {"name":"my-app","tags":["latest","1.0.0"]}

# Pull from private registry (on another machine)
docker login registry.example.com:5000
docker pull registry.example.com:5000/my-app:1.0.0
# => Downloads image from private registry

# Delete image tag from registry
curl -X DELETE -u admin:secretpassword \
  https://registry.example.com:5000/v2/my-app/manifests/sha256:<digest>
# => Deletes specific image manifest

# Run garbage collection to free space
docker exec registry bin/registry garbage-collect /etc/docker/registry/config.yml
# => Removes unreferenced layers

# Registry with S3 storage (production)
cat > registry-s3.yml << 'EOF'
version: '3.8'

services:
  registry:
    image: registry:2
    ports:
      - "5000:5000"
    environment:
      REGISTRY_STORAGE: s3
      REGISTRY_STORAGE_S3_REGION: us-east-1
      REGISTRY_STORAGE_S3_BUCKET: my-docker-registry
      REGISTRY_STORAGE_S3_ACCESSKEY: ${AWS_ACCESS_KEY_ID}
      REGISTRY_STORAGE_S3_SECRETKEY: ${AWS_SECRET_ACCESS_KEY}
EOF
# => Uses S3 for scalable, durable storage

# Registry with Redis cache (performance)
cat >> docker-compose.yml << 'EOF'
  redis:
    image: redis:7-alpine
    restart: unless-stopped

  registry:
    environment:
      REGISTRY_STORAGE_CACHE_BLOBDESCRIPTOR: redis
      REGISTRY_REDIS_ADDR: redis:6379
      REGISTRY_REDIS_DB: 0
EOF
# => Caches blob descriptors in Redis for faster pulls
```

**Key Takeaway**: Host private registries for proprietary images and controlled access. Always use authentication (htpasswd) and TLS encryption. For production, use external storage (S3, Azure Blob) and Redis caching. Implement garbage collection schedules to manage disk space.

**Why It Matters**: Private registries are critical for organizations with proprietary code or regulatory requirements - they prevent source code leakage through public image repositories and enable access control auditing required for compliance with SOC 2, ISO 27001, and FedRAMP. Self-hosted registries eliminate dependency on third-party availability - when Docker Hub suffered outages affecting millions of deployments, organizations with private registries continued operating normally. Geographic distribution of registry replicas reduces deployment times from minutes to seconds in multi-region deployments by serving images locally.

---

### Example 65: CI/CD with GitHub Actions

Automate Docker image builds, tests, scans, and deployments using GitHub Actions.

**GitHub Actions CI/CD Pipeline:**

```mermaid
%% CI/CD workflow
graph TD
    A["Git Push"] --> B["Build & Test"]
    B --> C["Security Scan<br/>Trivy"]
    C --> D{Pass?}
    D -->|No| E["Fail Build"]
    D -->|Yes| F{Branch?}
    F -->|develop| G["Deploy Staging"]
    F -->|v tag| H["Deploy Production"]
    H --> I["Create Release"]

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style E fill:#CC78BC,color:#000
    style G fill:#029E73,color:#fff
    style H fill:#029E73,color:#fff
```

```yaml
# File: .github/workflows/ci-cd.yml

name: CI/CD Pipeline

on:
  push:
    branches: [main, develop]
    tags:
      - "v*"
  pull_request:
    branches: [main]

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  # Job 1: Build and test
  build-test:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write

    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v2
        # => Enables advanced build features (caching, multi-platform)

      - name: Log in to GitHub Container Registry
        uses: docker/login-action@v2
        with:
          registry: ${{ env.REGISTRY }}
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Extract metadata
        id: meta
        uses: docker/metadata-action@v4
        with:
          images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
          tags: |
            type=ref,event=branch
            type=ref,event=pr
            type=semver,pattern={{version}}
            type=semver,pattern={{major}}.{{minor}}
            type=sha
          # => Generates tags: main, pr-123, v1.0.0, 1.0, sha-abc123

      - name: Build and push Docker image
        uses: docker/build-push-action@v4
        with:
          context: .
          push: ${{ github.event_name != 'pull_request' }}
          tags: ${{ steps.meta.outputs.tags }}
          labels: ${{ steps.meta.outputs.labels }}
          cache-from: type=registry,ref=${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:buildcache
          cache-to: type=registry,ref=${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:buildcache,mode=max
          # => Registry cache for faster subsequent builds

      - name: Run tests in container
        run: |
          docker run --rm ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:sha-${{ github.sha }} npm test

  # Job 2: Security scanning
  security-scan:
    runs-on: ubuntu-latest
    needs: build-test
    permissions:
      security-events: write

    steps:
      - name: Run Trivy scanner
        uses: aquasecurity/trivy-action@master
        with:
          image-ref: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:sha-${{ github.sha }}
          format: "sarif"
          output: "trivy-results.sarif"
          severity: "CRITICAL,HIGH"

      - name: Upload Trivy results
        uses: github/codeql-action/upload-sarif@v2
        with:
          sarif_file: "trivy-results.sarif"

  # Job 3: Deploy to staging
  deploy-staging:
    runs-on: ubuntu-latest
    needs: [build-test, security-scan]
    if: github.ref == 'refs/heads/develop'
    environment:
      name: staging
      url: https://staging.example.com

    steps:
      - name: Deploy to staging
        run: |
          # SSH to staging server and update deployment
          echo "${{ secrets.STAGING_SSH_KEY }}" > staging_key
          chmod 600 staging_key
          ssh -i staging_key -o StrictHostKeyChecking=no deploy@staging.example.com << 'EOF'
            docker pull ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:develop
            docker stop myapp || true
            docker rm myapp || true
            docker run -d --name myapp -p 80:3000 \
              ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:develop
          EOF

  # Job 4: Deploy to production
  deploy-production:
    runs-on: ubuntu-latest
    needs: [build-test, security-scan]
    if: startsWith(github.ref, 'refs/tags/v')
    environment:
      name: production
      url: https://example.com

    steps:
      - name: Deploy to production Kubernetes
        run: |
          echo "${{ secrets.KUBECONFIG }}" > kubeconfig
          kubectl --kubeconfig=kubeconfig set image \
            deployment/myapp \
            app=${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:${{ github.ref_name }}
          kubectl --kubeconfig=kubeconfig rollout status deployment/myapp

      - name: Create GitHub release
        uses: actions/create-release@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          tag_name: ${{ github.ref }}
          release_name: Release ${{ github.ref }}
          draft: false
          prerelease: false
```

```bash
# Trigger workflow by pushing to main
git add .
git commit -m "feat: add new feature"
git push origin main
# => Triggers CI/CD pipeline
# => Builds, tests, scans, deploys to staging

# Create release tag (triggers production deployment)
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0
# => Triggers production deployment workflow

# View workflow runs
# GitHub UI: Actions tab shows all workflow runs

# Pull image built by GitHub Actions
docker pull ghcr.io/username/repo:v1.0.0
# => Downloads image from GitHub Container Registry
```

**Key Takeaway**: GitHub Actions provides powerful CI/CD automation with native Docker support. Use matrix builds for multi-platform images, registry caching for speed, and environment protection rules for safe deployments. Always scan for vulnerabilities before deploying to production.

**Why It Matters**: GitHub Actions integration with GitHub Container Registry enables complete CI/CD workflows without context switching or third-party tools, reducing build pipeline complexity by 60% compared to traditional CI/CD systems. Matrix builds enable multi-platform image creation with single workflow definition, supporting ARM64, AMD64, and ARMv7 architectures required for IoT and edge deployments. Environment protection rules with required reviewers prevent unauthorized production deployments - deployments require approval from security or operations teams, satisfying change management requirements for SOC 2 and ISO 27001 compliance.

---

### Example 66: Docker Stack Deployment

Docker Stack enables declarative multi-service deployment using Compose files with Swarm orchestration features.

**Stack Deployment Flow:**

```mermaid
graph TD
    A["docker-compose.yml"] --> B["docker stack deploy"]
    B --> C{Parse Services}
    C --> D["Create Networks"]
    C --> E["Create Volumes"]
    C --> F["Create Services"]
    F --> G["Schedule Tasks"]
    G --> H["Pull Images on Nodes"]
    H --> I["Start Containers"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style I fill:#029E73,color:#fff
```

```yaml
# File: docker-compose-stack.yml
# Stack deployment with production features

version: "3.8"

services:
  web:
    image: nginx:alpine
    # => Uses official Nginx Alpine image
    deploy:
      replicas: 3
      # => Creates 3 web service replicas
      update_config:
        parallelism: 1
        # => Updates 1 replica at a time
        delay: 10s
        # => Waits 10s between updates
        failure_action: rollback
        # => Rolls back if update fails
      restart_policy:
        condition: on-failure
        # => Restarts only on failure
        delay: 5s
        # => Waits 5s before restart
        max_attempts: 3
        # => Maximum 3 restart attempts
      placement:
        constraints:
          - node.role == worker
          # => Deploys only on worker nodes
    ports:
      - "8080:80"
      # => Maps port 8080 on host to port 80 in container
    networks:
      - frontend
      # => Connects to frontend overlay network

  app:
    image: myapp:latest
    # => Uses custom application image
    deploy:
      replicas: 5
      # => Creates 5 app service replicas
      resources:
        limits:
          cpus: "0.5"
          # => Limits each replica to 0.5 CPU
          memory: 512M
          # => Limits each replica to 512MB RAM
        reservations:
          cpus: "0.25"
          # => Reserves minimum 0.25 CPU
          memory: 256M
          # => Reserves minimum 256MB RAM
      placement:
        preferences:
          - spread: node.labels.datacenter
          # => Spreads replicas across datacenters
    environment:
      DATABASE_URL: postgresql://db:5432/prod
      # => Database connection string
    networks:
      - frontend
      - backend
      # => Connects to both networks
    depends_on:
      - db
      # => Starts after db service (ordering hint)

  db:
    image: postgres:15-alpine
    # => Uses PostgreSQL 15 Alpine
    deploy:
      replicas: 1
      # => Single database instance (stateful)
      placement:
        constraints:
          - node.labels.database == true
          # => Deploys only on nodes labeled for database
      restart_policy:
        condition: any
        # => Always restart database
    volumes:
      - db-data:/var/lib/postgresql/data
      # => Persistent database storage
    networks:
      - backend
      # => Only accessible from backend network
    secrets:
      - db_password
      # => Mounts secret as file

networks:
  frontend:
    driver: overlay
    # => Creates overlay network for frontend
  backend:
    driver: overlay
    # => Creates overlay network for backend

volumes:
  db-data:
    driver: local
    # => Creates local volume for database

secrets:
  db_password:
    external: true
    # => References externally created secret
```

```bash
# Create secret before stack deployment
echo "super_secret_password" | docker secret create db_password -
# => Secret created: db_password
# => Secret is encrypted at rest and in transit

# Label node for database placement
docker node update --label-add database=true worker1
# => Node worker1 labeled with database=true

# Deploy stack
docker stack deploy -c docker-compose-stack.yml myapp
# => Creating network myapp_frontend
# => Creating network myapp_backend
# => Creating service myapp_web
# => Creating service myapp_app
# => Creating service myapp_db
# => Stack deployed successfully

# List stacks
docker stack ls
# => NAME     SERVICES   ORCHESTRATOR
# => myapp    3          Swarm

# List stack services
docker stack services myapp
# => ID       NAME        MODE        REPLICAS  IMAGE
# => abc123   myapp_web   replicated  3/3       nginx:alpine
# => def456   myapp_app   replicated  5/5       myapp:latest
# => ghi789   myapp_db    replicated  1/1       postgres:15-alpine

# Inspect service
docker service inspect myapp_app --pretty
# => ID:           def456ghi789
# => Name:         myapp_app
# => Mode:         Replicated
# =>  Replicas:    5
# => Placement:
# =>  Preferences: spread=node.labels.datacenter
# => UpdateConfig:
# =>  Parallelism: 1
# =>  Delay:       10s
# =>  Failure action: rollback
# => Resources:
# =>  Limits:      0.5 CPUs, 512MB Memory
# =>  Reservations: 0.25 CPUs, 256MB Memory

# View service logs
docker service logs myapp_app --tail 10
# => Shows last 10 log lines from all app replicas
# => [myapp_app.1] Application started on port 3000
# => [myapp_app.2] Application started on port 3000
# => [myapp_app.3] Application started on port 3000

# Scale service
docker service scale myapp_app=10
# => myapp_app scaled to 10 replicas
# => Swarm schedules 5 additional tasks

# Update service image
docker service update --image myapp:v2 myapp_app
# => myapp_app updated
# => Rolling update: 1 replica at a time
# => Waits 10s between updates
# => Automatically rolls back on failure

# Rollback service update
docker service rollback myapp_app
# => myapp_app rolled back to previous version

# View stack networks
docker stack ps myapp --filter "desired-state=running"
# => Shows all running tasks in stack
# => ID      NAME           NODE      CURRENT STATE
# => abc123  myapp_web.1    worker1   Running 5 minutes ago
# => def456  myapp_web.2    worker2   Running 5 minutes ago
# => ghi789  myapp_app.1    worker1   Running 5 minutes ago

# Remove stack
docker stack rm myapp
# => Removing service myapp_web
# => Removing service myapp_app
# => Removing service myapp_db
# => Removing network myapp_frontend
# => Removing network myapp_backend
# => Stack removed successfully

# Remove secret
docker secret rm db_password
# => Secret removed: db_password
```

**Key Takeaway**: Docker Stack provides declarative infrastructure-as-code for Swarm deployments with built-in rolling updates, automatic rollback, resource management, placement constraints, and overlay networking. Use update_config for zero-downtime deployments and placement constraints for node specialization.

**Why It Matters**: Stack deployments enable GitOps workflows where infrastructure configuration lives in version control and deployments are reproducible across environments. Rolling updates with automatic rollback prevent prolonged outages - if new version fails health checks, Swarm automatically reverts to previous version within seconds. Placement constraints enable specialized node pools (GPU nodes for ML workloads, SSD nodes for databases) while spread preferences ensure high availability across failure domains, preventing single datacenter outages from impacting services.

---

### Example 67: Docker Swarm Service Constraints

Service constraints control task placement based on node attributes, enabling specialized workloads and high availability.

**Constraint Placement Strategy:**

```mermaid
graph TD
    A["Service Definition"] --> B{Constraints?}
    B -->|Node Role| C["Manager vs Worker"]
    B -->|Node Label| D["Custom Labels"]
    B -->|Node ID| E["Specific Nodes"]
    B -->|Engine Label| F["OS/Architecture"]

    C --> G["Schedule Tasks"]
    D --> G
    E --> G
    F --> G

    G --> H["Filter Nodes"]
    H --> I["Place Containers"]

    style A fill:#0173B2,color:#fff
    style H fill:#DE8F05,color:#000
    style I fill:#029E73,color:#fff
```

```bash
# Label nodes for specialized workloads
docker node update --label-add environment=production worker1
# => Node worker1 labeled: environment=production

docker node update --label-add gpu=true worker2
# => Node worker2 labeled: gpu=true

docker node update --label-add storage=ssd worker3
# => Node worker3 labeled: storage=ssd

docker node update --label-add datacenter=us-west worker4
# => Node worker4 labeled: datacenter=us-west

docker node update --label-add datacenter=us-east worker5
# => Node worker5 labeled: datacenter=us-east

# Create service with role constraint
docker service create \
  --name manager-only \
  --constraint 'node.role == manager' \
  nginx:alpine
# => Service created: manager-only
# => Tasks scheduled only on manager nodes

# Create service with label constraint
docker service create \
  --name production-app \
  --constraint 'node.labels.environment == production' \
  --replicas 3 \
  myapp:latest
# => Service created: production-app
# => All 3 replicas scheduled on worker1 (only production node)

# Create GPU service
docker service create \
  --name ml-training \
  --constraint 'node.labels.gpu == true' \
  --reserve-memory 4G \
  --reserve-cpu 4.0 \
  tensorflow/tensorflow:latest-gpu
# => Service created: ml-training
# => Task scheduled on worker2 (GPU node)
# => Reserves 4GB RAM and 4 CPUs

# Create database service with SSD constraint
docker service create \
  --name postgres-db \
  --constraint 'node.labels.storage == ssd' \
  --mount type=volume,source=db-data,target=/var/lib/postgresql/data \
  postgres:15-alpine
# => Service created: postgres-db
# => Task scheduled on worker3 (SSD node)

# Multiple constraints (AND logic)
docker service create \
  --name secure-api \
  --constraint 'node.role == worker' \
  --constraint 'node.labels.environment == production' \
  --constraint 'node.labels.datacenter == us-west' \
  api:latest
# => Service created: secure-api
# => Task scheduled only on worker nodes in production environment in us-west datacenter

# Constraint with node ID
NODE_ID=$(docker node ls -q -f name=worker1)
docker service create \
  --name pinned-service \
  --constraint "node.id == $NODE_ID" \
  myapp:latest
# => Service created: pinned-service
# => Task pinned to specific node (worker1)

# OS constraint
docker service create \
  --name linux-service \
  --constraint 'node.platform.os == linux' \
  alpine:latest
# => Service created: linux-service
# => Runs only on Linux nodes

# Architecture constraint
docker service create \
  --name arm-service \
  --constraint 'node.platform.arch == aarch64' \
  arm64v8/alpine:latest
# => Service created: arm-service
# => Runs only on ARM64 nodes

# Hostname constraint
docker service create \
  --name hostname-service \
  --constraint 'node.hostname == worker1' \
  nginx:alpine
# => Service created: hostname-service
# => Scheduled on node with hostname "worker1"

# Spread preference (soft constraint, not hard)
docker service create \
  --name distributed-app \
  --replicas 6 \
  --placement-pref 'spread=node.labels.datacenter' \
  myapp:latest
# => Service created: distributed-app
# => 3 replicas on us-west nodes, 3 on us-east nodes
# => Spreads evenly across datacenter labels

# Combine constraints and preferences
docker service create \
  --name ha-database \
  --replicas 3 \
  --constraint 'node.labels.storage == ssd' \
  --placement-pref 'spread=node.labels.datacenter' \
  postgres:15-alpine
# => Service created: ha-database
# => Scheduled only on SSD nodes
# => Spread across datacenters for high availability

# Inspect service placement
docker service ps distributed-app
# => ID      NAME                 NODE      CURRENT STATE
# => abc123  distributed-app.1    worker4   Running
# => def456  distributed-app.2    worker5   Running
# => ghi789  distributed-app.3    worker4   Running
# => jkl012  distributed-app.4    worker5   Running
# => Shows task distribution across nodes

# Update service constraints
docker service update \
  --constraint-add 'node.labels.environment == staging' \
  production-app
# => production-app updated
# => Adds additional constraint (now must match both production AND staging)

# Remove constraint
docker service update \
  --constraint-rm 'node.labels.environment == staging' \
  production-app
# => production-app updated
# => Removes staging constraint

# View node labels
docker node inspect worker1 --format '{{.Spec.Labels}}'
# => map[datacenter:us-west environment:production]

# Remove node label
docker node update --label-rm environment worker1
# => Node worker1: label environment removed
```

**Key Takeaway**: Service constraints enable specialized workload placement (GPU nodes for ML, SSD nodes for databases, production-labeled nodes for critical services) and high availability through spread preferences across datacenters. Combine hard constraints (must match) with soft preferences (spread evenly) for optimal resource utilization.

**Why It Matters**: Placement constraints enable heterogeneous cluster management where expensive resources (GPUs, SSDs, high-memory nodes) are used efficiently - ML workloads automatically schedule on GPU nodes, databases on SSD-backed nodes, and CPU-bound workloads on standard nodes. Datacenter spread preferences ensure geographic redundancy, preventing single availability zone failures from impacting services. Organizations using constraint-based placement report 40% reduction in infrastructure costs by matching workload requirements to node capabilities, eliminating resource waste from over-provisioning.

---

### Example 68: Docker Swarm Rolling Updates and Rollback

Swarm rolling updates enable zero-downtime deployments with automatic health-check-based rollback.

**Rolling Update Process:**

```mermaid
graph TD
    A["docker service update"] --> B["Stop 1 Replica"]
    B --> C["Start New Version"]
    C --> D{Health Check}
    D -->|Pass| E["Wait Delay Period"]
    D -->|Fail| F["Automatic Rollback"]
    E --> G{More Replicas?}
    G -->|Yes| B
    G -->|No| H["Update Complete"]
    F --> I["Restore Previous Version"]

    style A fill:#0173B2,color:#fff
    style D fill:#DE8F05,color:#000
    style H fill:#029E73,color:#fff
    style F fill:#CC78BC,color:#000
```

```bash
# Create service with health check
docker service create \
  --name web-app \
  --replicas 5 \
  --health-cmd "curl -f http://localhost/health || exit 1" \
  --health-interval 10s \
  --health-timeout 5s \
  --health-retries 3 \
  --update-parallelism 1 \
  --update-delay 10s \
  --update-failure-action rollback \
  --rollback-parallelism 2 \
  --rollback-delay 5s \
  myapp:v1
# => Service created: web-app
# => Health check: curl /health every 10s
# => Update parallelism: 1 task at a time
# => Update delay: 10s between tasks
# => Failure action: automatic rollback
# => Rollback parallelism: 2 tasks at a time (faster recovery)

# Verify service is running
docker service ps web-app
# => ID      NAME         IMAGE       NODE      CURRENT STATE
# => abc123  web-app.1    myapp:v1    worker1   Running (healthy)
# => def456  web-app.2    myapp:v1    worker2   Running (healthy)
# => ghi789  web-app.3    myapp:v1    worker3   Running (healthy)
# => jkl012  web-app.4    myapp:v1    worker1   Running (healthy)
# => mno345  web-app.5    myapp:v1    worker2   Running (healthy)

# Perform rolling update to v2
docker service update --image myapp:v2 web-app
# => web-app updated
# => Update in progress...
# => Stopping web-app.1
# => Starting web-app.1 with image myapp:v2
# => Health check passed for web-app.1
# => Waiting 10s before next update
# => Stopping web-app.2
# => Starting web-app.2 with image myapp:v2
# => (continues for all replicas)

# Monitor update progress
docker service ps web-app
# => ID      NAME         IMAGE       NODE      CURRENT STATE
# => pqr678  web-app.1    myapp:v2    worker1   Running (healthy)
# => abc123   \_ web-app.1 myapp:v1    worker1   Shutdown 2 minutes ago
# => stu901  web-app.2    myapp:v2    worker2   Running (healthy)
# => def456   \_ web-app.2 myapp:v1    worker2   Shutdown 1 minute ago
# => Shows current and previous tasks

# Simulate failed update (v3 has failing health check)
docker service update --image myapp:v3-broken web-app
# => web-app updated
# => Update in progress...
# => Stopping web-app.1
# => Starting web-app.1 with image myapp:v3-broken
# => Health check failed for web-app.1 (3 consecutive failures)
# => Automatic rollback triggered
# => Stopping web-app.1 (v3-broken)
# => Starting web-app.1 with image myapp:v2
# => Rollback in progress (2 tasks at a time)
# => Service rolled back to myapp:v2

# Check service after failed update
docker service ps web-app --filter "desired-state=running"
# => All replicas running myapp:v2
# => Failed v3-broken tasks in shutdown state

# Manual rollback to previous version
docker service rollback web-app
# => web-app rolled back
# => Reverting to previous image myapp:v1
# => Rolling back 2 tasks at a time
# => Rollback complete

# Update with custom settings
docker service update \
  --image myapp:v4 \
  --update-parallelism 2 \
  --update-delay 30s \
  --update-max-failure-ratio 0.2 \
  --update-monitor 60s \
  web-app
# => Updates 2 tasks at a time
# => Waits 30s between batches
# => Rolls back if >20% of tasks fail
# => Monitors each task for 60s before considering it stable

# Pause update mid-rollout
docker service update --update-parallelism 0 web-app
# => Update paused (parallelism set to 0)
# => Current tasks remain running
# => No new tasks started

# Resume update
docker service update \
  --image myapp:v4 \
  --update-parallelism 1 \
  web-app
# => Update resumed with parallelism 1

# Force update (recreate all tasks even with same image)
docker service update --force web-app
# => Forces recreation of all tasks
# => Useful for picking up configuration changes

# Update with order (start-first vs stop-first)
docker service update \
  --image myapp:v5 \
  --update-order start-first \
  web-app
# => Starts new task before stopping old task
# => Ensures capacity is maintained (useful for stateless services)
# => Alternative: --update-order stop-first (default, for stateful services)

# Inspect update config
docker service inspect web-app --format '{{.Spec.UpdateConfig}}'
# => {1 10s 0s 0.2 pause 60s start-first}
# => Parallelism: 1
# => Delay: 10s
# => Max failure ratio: 0.2 (20%)
# => Monitor period: 60s
# => Order: start-first

# View service update history
docker service inspect web-app --format '{{.PreviousSpec.TaskTemplate.ContainerSpec.Image}}'
# => Shows previous image version

# Configure global update defaults for all services
docker swarm update \
  --task-history-limit 10 \
  --dispatcher-heartbeat 10s
# => Sets task history retention to 10 previous versions
# => Sets dispatcher heartbeat to 10s
```

**Key Takeaway**: Rolling updates with health checks and automatic rollback enable zero-downtime deployments with instant recovery from bad releases. Configure update parallelism, delay, and failure thresholds to balance deployment speed with risk tolerance. Use start-first order for stateless services to maintain capacity during updates.

**Why It Matters**: Automated rollback based on health checks eliminates prolonged outages from failed deployments - when new version fails health checks, Swarm automatically reverts to previous version within seconds without manual intervention. Rolling updates with configurable parallelism enable risk control, updating one replica at a time for critical services or multiple replicas for faster deployments. Organizations using health-check-based rollback report 95% reduction in deployment-related incidents and 80% faster recovery times compared to manual rollback procedures.

---

### Example 74: Distributed Tracing with Jaeger

Implement distributed tracing to debug performance issues across microservices.

**Tracing Architecture:**

```mermaid
graph TD
    A["Client Request"] --> B["Web Service<br/>Span: /api/users"]
    B --> C["Auth Service<br/>Span: /auth/verify"]
    B --> D["User Service<br/>Span: /users/get"]
    D --> E["Database<br/>Span: SELECT"]

    B -.Trace ID.-> F["Jaeger Collector"]
    C -.Trace ID.-> F
    D -.Trace ID.-> F
    E -.Trace ID.-> F

    F --> G["Jaeger Storage<br/>Elasticsearch/Cassandra"]
    G --> H["Jaeger UI<br/>Query & Visualize"]

    style A fill:#0173B2,color:#fff
    style F fill:#DE8F05,color:#000
    style H fill:#029E73,color:#fff
```

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  # Jaeger all-in-one (dev/testing)
  jaeger:
    image: jaegertracing/all-in-one:latest
    environment:
      COLLECTOR_ZIPKIN_HOST_PORT: :9411
    ports:
      - "5775:5775/udp" # Compact thrift (deprecated)
      - "6831:6831/udp" # Jaeger thrift (binary)
      - "6832:6832/udp" # Jaeger thrift (compact)
      - "5778:5778" # Serve configs
      - "16686:16686" # Jaeger UI
      - "14250:14250" # Jaeger gRPC
      - "14268:14268" # Jaeger HTTP
      - "9411:9411" # Zipkin compatible endpoint

  # Application with tracing
  web:
    build: ./web
    environment:
      JAEGER_AGENT_HOST: jaeger
      JAEGER_AGENT_PORT: 6831
    depends_on:
      - jaeger
    ports:
      - "3000:3000"

  auth:
    build: ./auth
    environment:
      JAEGER_AGENT_HOST: jaeger
      JAEGER_AGENT_PORT: 6831
    depends_on:
      - jaeger

  user-service:
    build: ./user-service
    environment:
      JAEGER_AGENT_HOST: jaeger
      JAEGER_AGENT_PORT: 6831
    depends_on:
      - jaeger
```

```javascript
// File: web/tracing.js (Node.js/OpenTelemetry)
const { NodeTracerProvider } = require("@opentelemetry/sdk-trace-node");
const { JaegerExporter } = require("@opentelemetry/exporter-jaeger");
const { Resource } = require("@opentelemetry/resources");
const { SemanticResourceAttributes } = require("@opentelemetry/semantic-conventions");
const { registerInstrumentations } = require("@opentelemetry/instrumentation");
const { HttpInstrumentation } = require("@opentelemetry/instrumentation-http");
const { ExpressInstrumentation } = require("@opentelemetry/instrumentation-express");

// Create tracer provider
const provider = new NodeTracerProvider({
  resource: new Resource({
    [SemanticResourceAttributes.SERVICE_NAME]: "web-service",
    [SemanticResourceAttributes.SERVICE_VERSION]: "1.0.0",
  }),
});

// Configure Jaeger exporter
const jaegerExporter = new JaegerExporter({
  endpoint: `http://${process.env.JAEGER_AGENT_HOST}:14268/api/traces`,
});

provider.addSpanProcessor(new SimpleSpanProcessor(jaegerExporter));
provider.register();

// Auto-instrument HTTP and Express
registerInstrumentations({
  instrumentations: [new HttpInstrumentation(), new ExpressInstrumentation()],
});

// Manual span creation
const tracer = provider.getTracer("web-service");

app.get("/api/users/:id", async (req, res) => {
  const span = tracer.startSpan("get-user");
  span.setAttribute("user.id", req.params.id);

  try {
    // Call auth service (auto-traced)
    const authResult = await fetch("http://auth/verify", {
      headers: {
        authorization: req.headers.authorization,
      },
    });

    // Call user service
    const userResult = await fetch(`http://user-service/users/${req.params.id}`);

    span.setStatus({ code: SpanStatusCode.OK });
    res.json(await userResult.json());
  } catch (error) {
    span.setStatus({
      code: SpanStatusCode.ERROR,
      message: error.message,
    });
    span.recordException(error);
    res.status(500).json({ error: error.message });
  } finally {
    span.end();
  }
});
```

```bash
# Start services
docker compose up -d

# Access Jaeger UI
# => http://localhost:16686

# Generate trace data
curl http://localhost:3000/api/users/123

# View trace in Jaeger UI:
# => Service: web-service
# => Operation: GET /api/users/:id
# => Trace timeline showing:
# =>   - web-service: GET /api/users/:id (50ms total)
# =>     - auth-service: /verify (10ms)
# =>     - user-service: /users/123 (35ms)
# =>       - database: SELECT (30ms) ← Bottleneck identified!

# Query traces programmatically
curl 'http://localhost:16686/api/traces?service=web-service&limit=10'
# => Returns JSON with recent traces

# Production deployment (Elasticsearch backend)
cat > docker-compose.prod.yml << 'EOF'
version: '3.8'

services:
  elasticsearch:
    image: docker.elastic.co/elasticsearch/elasticsearch:7.17.0
    environment:
      - discovery.type=single-node
    volumes:
      - es-data:/usr/share/elasticsearch/data

  jaeger-collector:
    image: jaegertracing/jaeger-collector:latest
    environment:
      SPAN_STORAGE_TYPE: elasticsearch
      ES_SERVER_URLS: http://elasticsearch:9200
    ports:
      - "14250:14250"
      - "14268:14268"
    depends_on:
      - elasticsearch

  jaeger-query:
    image: jaegertracing/jaeger-query:latest
    environment:
      SPAN_STORAGE_TYPE: elasticsearch
      ES_SERVER_URLS: http://elasticsearch:9200
    ports:
      - "16686:16686"
    depends_on:
      - elasticsearch

volumes:
  es-data:
EOF
```

**Key Takeaway**: Distributed tracing provides end-to-end visibility across microservices. Use Jaeger for collecting, storing, and visualizing traces. Instrument services with OpenTelemetry for automatic HTTP/gRPC tracing. Identify performance bottlenecks by analyzing trace timelines. Tag spans with custom attributes for debugging context.

**Why It Matters**: Distributed tracing solves the debugging nightmare of microservices where single requests span dozens of services - without tracing, identifying which service causes latency requires manual correlation across separate logs. Jaeger's visual timeline instantly reveals bottlenecks - seeing that 80% of request time is database queries (not application logic) redirects optimization efforts from code to query tuning. Organizations using tracing report 70% reduction in mean-time-to-resolution for performance issues - problems that took days to diagnose through logs are identified in minutes through trace analysis.

---

### Example 75: AppArmor Security Profiles

Enhance container security with AppArmor profiles limiting system call access and file operations.

```bash
# Check AppArmor status
sudo aa-status | grep docker
# => docker-default (enforce)
# => Shows active Docker AppArmor profile

# View default Docker AppArmor profile
cat /etc/apparmor.d/docker
# => # Default Docker profile
# => #include <tunables/global>
# => profile docker-default flags=(attach_disconnected,mediate_deleted) {
# =>   #include <abstractions/base>
# =>   deny @{PROC}/* w,   # deny write to /proc
# =>   deny /sys/[^f]** wklx,
# =>   ...
# => }

# Create custom AppArmor profile
sudo tee /etc/apparmor.d/docker-nginx << 'EOF'
#include <tunables/global>

profile docker-nginx flags=(attach_disconnected,mediate_deleted) {
  #include <abstractions/base>

  # Allow network access
  network inet tcp,
  network inet udp,

  # Allow reading nginx files
  /etc/nginx/** r,
  /var/www/** r,
  /usr/share/nginx/** r,

  # Allow writing logs
  /var/log/nginx/** w,

  # Allow executing nginx
  /usr/sbin/nginx ix,

  # Deny everything else
  deny /proc/** w,
  deny /sys/** w,
  deny /root/** rwx,
  deny /home/** rwx,
}
EOF

# Load AppArmor profile
sudo apparmor_parser -r -W /etc/apparmor.d/docker-nginx
# => Profile loaded

# Verify profile loaded
sudo aa-status | grep docker-nginx
# => docker-nginx (enforce)

# Run container with custom AppArmor profile
docker run -d \
  --name secure-nginx \
  --security-opt apparmor=docker-nginx \
  -v $(pwd)/html:/var/www:ro \
  nginx:alpine
# => Container runs with docker-nginx AppArmor profile

# Test restrictions
docker exec secure-nginx sh -c 'echo test > /root/test.txt'
# => sh: can't create /root/test.txt: Permission denied
# => AppArmor denies write to /root

docker exec secure-nginx sh -c 'cat /etc/nginx/nginx.conf'
# => (file contents displayed)
# => AppArmor allows reading nginx configs

# Monitor AppArmor denials
sudo dmesg | grep DENIED | tail -5
# => [12345.678] audit: type=1400 audit(1234567890.123:456): apparmor="DENIED" operation="open" profile="docker-nginx" name="/root/test.txt" requested_mask="wc"
# => Shows blocked operations

# Disable AppArmor for container (not recommended)
docker run -d \
  --security-opt apparmor=unconfined \
  nginx:alpine
# => Runs without AppArmor protection (insecure)

# Docker Compose with AppArmor
cat > docker-compose.yml << 'EOF'
version: '3.8'

services:
  web:
    image: nginx:alpine
    security_opt:
      - apparmor:docker-nginx
    volumes:
      - ./html:/var/www:ro
EOF

# Advanced: Generate AppArmor profile from container behavior
# Install aa-genprof
sudo apt-get install apparmor-utils

# Run container in complain mode (log only, don't enforce)
docker run -d --name nginx-learn \
  --security-opt apparmor=docker-nginx \
  nginx:alpine

# Generate profile from logs
sudo aa-logprof
# => Analyzes denials and suggests profile updates
```

**Key Takeaway**: AppArmor provides mandatory access control (MAC) for containers. Create custom profiles restricting file access, network operations, and system calls. Default docker-default profile provides basic protection, custom profiles enable least-privilege security. Monitor denials with dmesg to refine profiles. Never disable AppArmor in production unless absolutely necessary.

**Why It Matters**: AppArmor prevents container escape exploits by enforcing kernel-level access controls that survive even if container runtime is compromised. Default Docker profiles block 90% of common container breakout attempts by denying writes to /proc and /sys, but custom profiles enable defense-in-depth - web servers get read-only access to static files, preventing tampering even if application code is compromised. Compliance frameworks including PCI DSS and NIST 800-53 mandate mandatory access controls, making AppArmor essential for regulated deployments.

---

### Example 76: Seccomp Security Profiles

Restrict container syscalls using seccomp (Secure Computing Mode) profiles for defense-in-depth.

```json
// File: seccomp-profile.json (custom seccomp profile)
{
  "defaultAction": "SCMP_ACT_ERRNO",
  "architectures": ["SCMP_ARCH_X86_64", "SCMP_ARCH_X86", "SCMP_ARCH_ARM", "SCMP_ARCH_AARCH64"],
  "syscalls": [
    {
      "names": [
        "accept",
        "accept4",
        "access",
        "arch_prctl",
        "bind",
        "brk",
        "chdir",
        "clone",
        "close",
        "connect",
        "dup",
        "dup2",
        "epoll_create",
        "epoll_create1",
        "epoll_ctl",
        "epoll_wait",
        "execve",
        "exit",
        "exit_group",
        "fcntl",
        "fstat",
        "futex",
        "getcwd",
        "getdents",
        "getpid",
        "getppid",
        "getuid",
        "ioctl",
        "listen",
        "lseek",
        "mmap",
        "mprotect",
        "munmap",
        "open",
        "openat",
        "pipe",
        "poll",
        "read",
        "readlink",
        "recvfrom",
        "recvmsg",
        "rt_sigaction",
        "rt_sigprocmask",
        "rt_sigreturn",
        "select",
        "sendmsg",
        "sendto",
        "set_robust_list",
        "setsockopt",
        "socket",
        "stat",
        "write"
      ],
      "action": "SCMP_ACT_ALLOW"
    },
    {
      "names": ["reboot"],
      "action": "SCMP_ACT_ERRNO",
      "comment": "Block reboot syscall"
    }
  ]
}
```

```bash
# Run container with custom seccomp profile
docker run -d \
  --name secure-app \
  --security-opt seccomp=seccomp-profile.json \
  my-app:latest
# => Container runs with restricted syscalls

# Test restrictions
docker exec secure-app reboot
# => reboot: Operation not permitted
# => Seccomp blocks reboot syscall

# Docker default seccomp profile
# Located at: https://github.com/moby/moby/blob/master/profiles/seccomp/default.json
# Blocks ~44 dangerous syscalls including:
# - Kernel module loading (init_module, finit_module)
# - Clock manipulation (clock_adjtime, clock_settime)
# - Reboot (reboot)
# - System resource limits (set_mempolicy, mbind)

# Disable seccomp (not recommended)
docker run -d \
  --security-opt seccomp=unconfined \
  my-app:latest
# => Runs without syscall restrictions (insecure)

# Docker Compose with seccomp
cat > docker-compose.yml << 'EOF'
version: '3.8'

services:
  app:
    image: my-app:latest
    security_opt:
      - seccomp:seccomp-profile.json
EOF

# Audit syscalls used by container
# Install strace
docker run -d --name app my-app:latest

# Trace syscalls
docker run --rm \
  --pid=container:app \
  --cap-add SYS_PTRACE \
  alpine sh -c '
    apk add --no-cache strace &&
    strace -c -p 1
  '
# => Shows syscall frequency (helps build minimal profiles)

# Generate minimal seccomp profile from traces
# Use docker-slim or sysdig to analyze container behavior
docker run --rm \
  -v /var/run/docker.sock:/var/run/docker.sock \
  dslim/docker-slim build \
    --http-probe=false \
    --include-path=/app \
    my-app:latest
# => Generates minimal image with custom seccomp profile

# Kubernetes seccomp integration
cat > pod-seccomp.yaml << 'EOF'
apiVersion: v1
kind: Pod
metadata:
  name: secure-pod
spec:
  securityContext:
    seccompProfile:
      type: Localhost
      localhostProfile: profiles/seccomp-profile.json
  containers:
    - name: app
      image: my-app:latest
EOF
```

**Key Takeaway**: Seccomp restricts syscalls at kernel level, preventing containers from executing dangerous operations. Use custom profiles allowing only necessary syscalls for least-privilege security. Default Docker seccomp profile blocks kernel module loading, reboot, and clock manipulation. Never disable seccomp in production. Audit container syscall usage to create minimal profiles.

**Why It Matters**: Seccomp provides kernel-level syscall filtering that prevents entire classes of exploits - container escape attempts often rely on dangerous syscalls like `ptrace` or kernel module loading, and seccomp blocks these at the kernel interface before attackers can exploit them. Defense-in-depth through layered security (AppArmor + seccomp + capabilities) ensures that even if one protection mechanism is bypassed, others prevent compromise. Security-conscious organizations run production containers with custom seccomp profiles allowing only the 20-30 syscalls applications actually need, reducing attack surface by 95% compared to unrestricted containers.

---

### Example 77: Container Resource Quotas and Limits

Prevent resource exhaustion and ensure fair resource allocation using cgroups-based quotas.

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  # Web service (high priority)
  web:
    image: nginx:alpine
    deploy:
      resources:
        limits:
          cpus: "2.0"
          memory: 1G
        reservations:
          cpus: "1.0"
          memory: 512M
    # => Guaranteed 1 CPU + 512M, can burst to 2 CPU + 1G

  # API service (medium priority)
  api:
    image: my-api:latest
    deploy:
      resources:
        limits:
          cpus: "1.5"
          memory: 2G
          pids: 200
        reservations:
          cpus: "0.5"
          memory: 1G
    # => pids: 200 limits process/thread count

  # Worker (low priority, burstable)
  worker:
    image: my-worker:latest
    deploy:
      resources:
        limits:
          cpus: "4.0"
          memory: 4G
        reservations:
          cpus: "0.25"
          memory: 256M
    # => Minimal reservation, can use excess capacity
```

```bash
# Run container with CPU limits
docker run -d \
  --name cpu-limited \
  --cpus="1.5" \
  --cpu-shares=512 \
  stress:latest --cpu 4
# => --cpus: Maximum 1.5 CPUs (hard limit)
# => --cpu-shares: Relative weight when CPU contention (soft limit)

# Run container with memory limits
docker run -d \
  --name mem-limited \
  --memory="512m" \
  --memory-reservation="256m" \
  --memory-swap="1g" \
  --oom-kill-disable=false \
  my-app:latest
# => --memory: Hard limit (512MB)
# => --memory-reservation: Soft limit (256MB)
# => --memory-swap: Total memory + swap (1GB)
# => --oom-kill-disable=false: Kill if OOM (default, safe)

# Block I/O limits
docker run -d \
  --name io-limited \
  --device-read-bps /dev/sda:10mb \
  --device-write-bps /dev/sda:5mb \
  --device-read-iops /dev/sda:1000 \
  --device-write-iops /dev/sda:500 \
  my-app:latest
# => Read: 10 MB/s, 1000 IOPS
# => Write: 5 MB/s, 500 IOPS

# Process/thread limits
docker run -d \
  --name pid-limited \
  --pids-limit 100 \
  my-app:latest
# => Maximum 100 processes/threads

# CPU affinity (pin to specific cores)
docker run -d \
  --name cpu-pinned \
  --cpuset-cpus="0,1" \
  my-app:latest
# => Runs only on CPU cores 0 and 1

# Monitor resource usage
docker stats
# => CONTAINER    CPU %  MEM USAGE / LIMIT   MEM %
# => cpu-limited  150%   256MiB / 512MiB     50%
# => (150% = 1.5 CPUs)

# Stress test resource limits
docker run -d \
  --name stress-test \
  --cpus="1" \
  --memory="256m" \
  progrium/stress \
    --cpu 4 \
    --io 2 \
    --vm 2 \
    --vm-bytes 512M \
    --timeout 60s
# => Attempts to exceed limits

docker stats stress-test --no-stream
# => CPU usage capped at 100% (1 CPU)
# => Memory capped at 256MB (kills if exceeded)

# Dynamic resource updates
docker update \
  --cpus="2" \
  --memory="1g" \
  api
# => Updates running container limits

# Swarm service resource constraints
docker service create \
  --name prod-api \
  --replicas 5 \
  --limit-cpu 1.5 \
  --limit-memory 1G \
  --reserve-cpu 0.5 \
  --reserve-memory 512M \
  my-api:latest
# => Swarm enforces limits across all replicas

# Resource quotas for user namespaces
cat > /etc/security/limits.conf << 'EOF'
docker_users hard nofile 65536
docker_users soft nofile 32768
docker_users hard nproc 2048
EOF
# => Limits file descriptors and processes for Docker users
```

**Key Takeaway**: Use CPU and memory limits to prevent resource exhaustion. Set reservations for guaranteed resources, limits for maximum usage. Use `--pids-limit` to prevent fork bombs. Configure block I/O limits for disk-intensive applications. Pin CPU affinity for latency-sensitive workloads. Update limits dynamically without restarting containers.

**Why It Matters**: Resource quotas prevent noisy neighbor problems where single containers consume all cluster resources, starving other services. Production outages often stem from uncontrolled resource growth - without memory limits, memory leaks crash entire nodes instead of just offending containers. CPU limits ensure fair scheduling where critical user-facing services maintain low latency even when batch processing jobs run concurrently. Organizations report 80% reduction in resource-related incidents through quota enforcement - containers that previously monopolized nodes now operate within bounded resource envelopes, maintaining cluster stability.

---

### Example 78: Docker Registry Garbage Collection

Reclaim storage space in private Docker registries by removing unused layers and manifests.

```yaml
# File: registry-config.yml

version: 0.1
log:
  level: info
  fields:
    service: registry
storage:
  cache:
    blobdescriptor: inmemory
  filesystem:
    rootdirectory: /var/lib/registry
  delete:
    enabled: true
    # => Enable layer deletion (required for GC)
http:
  addr: :5000
  headers:
    X-Content-Type-Options: [nosniff]
health:
  storagedriver:
    enabled: true
    interval: 10s
    threshold: 3
```

```bash
# Start registry with deletion enabled
docker run -d \
  --name registry \
  -p 5000:5000 \
  -v $(pwd)/registry-data:/var/lib/registry \
  -v $(pwd)/registry-config.yml:/etc/docker/registry/config.yml \
  registry:2
# => Registry with deletion enabled

# Push images
docker tag my-app:v1.0.0 localhost:5000/my-app:v1.0.0
docker push localhost:5000/my-app:v1.0.0

docker tag my-app:v2.0.0 localhost:5000/my-app:v2.0.0
docker push localhost:5000/my-app:v2.0.0

# Check registry disk usage
du -sh $(pwd)/registry-data
# => 1.5G registry-data/
# => Storage before garbage collection

# Delete old image manifest (mark for GC)
# Get digest
DIGEST=$(curl -I -H "Accept: application/vnd.docker.distribution.manifest.v2+json" \
  http://localhost:5000/v2/my-app/manifests/v1.0.0 2>/dev/null | \
  grep Docker-Content-Digest | awk '{print $2}' | tr -d '\r')

echo "Digest: $DIGEST"

# Delete manifest
curl -X DELETE \
  http://localhost:5000/v2/my-app/manifests/$DIGEST
# => Marks v1.0.0 for deletion (layers not yet removed)

# Run garbage collection
docker exec registry bin/registry garbage-collect \
  /etc/docker/registry/config.yml
# => 0 blobs marked, 0 blobs eligible for deletion
# => 1234 blobs deleted
# => 5678 blobs marked
# => (Numbers vary by actual usage)

# Verify disk space reclaimed
du -sh $(pwd)/registry-data
# => 800M registry-data/
# => ~700MB reclaimed

# Garbage collection in read-only mode (dry run)
docker exec registry bin/registry garbage-collect \
  --dry-run \
  /etc/docker/registry/config.yml
# => 1234 blobs eligible for deletion (SIMULATION)
# => No actual deletion

# Automated garbage collection (cron job)
cat > /etc/cron.weekly/registry-gc << 'EOF'
#!/bin/bash
# Stop accepting new pushes (read-only mode)
docker exec registry kill -s HUP 1

# Run garbage collection
docker exec registry bin/registry garbage-collect \
  /etc/docker/registry/config.yml

# Resume accepting pushes
docker restart registry

# Log results
echo "$(date): Registry GC completed" >> /var/log/registry-gc.log
EOF

chmod +x /etc/cron.weekly/registry-gc

# Advanced: S3 backend garbage collection
cat > registry-s3-config.yml << 'EOF'
version: 0.1
storage:
  s3:
    region: us-east-1
    bucket: my-registry-bucket
    accesskey: AKIAIOSFODNN7EXAMPLE
    secretkey: wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
  delete:
    enabled: true
http:
  addr: :5000
EOF

# Run GC with S3 backend
docker run --rm \
  -v $(pwd)/registry-s3-config.yml:/etc/docker/registry/config.yml \
  registry:2 \
  bin/registry garbage-collect /etc/docker/registry/config.yml
# => Removes orphaned layers from S3

# Monitor registry metrics
cat > prometheus-registry.yml << 'EOF'
scrape_configs:
  - job_name: 'registry'
    static_configs:
      - targets: ['registry:5000']
    metrics_path: '/metrics'
EOF

# Registry storage usage query
curl http://localhost:5000/v2/_catalog | jq
# => {
# =>   "repositories": ["my-app", "another-app"]
# => }

# List tags per repository
curl http://localhost:5000/v2/my-app/tags/list | jq
# => {
# =>   "name": "my-app",
# =>   "tags": ["v2.0.0", "latest"]
# => }
```

**Key Takeaway**: Enable storage deletion in registry configuration. Delete manifests via API to mark images for removal. Run `registry garbage-collect` to physically delete unused layers. Use `--dry-run` to preview deletions. Schedule weekly garbage collection via cron. Monitor storage usage and reclaim space proactively.

**Why It Matters**: Registry storage grows unbounded without garbage collection - continuous deployments generate hundreds of gigabytes of orphaned layers monthly, increasing storage costs and degrading performance. Garbage collection reclaims 60-80% of registry storage in typical production environments where only latest few versions are actively used. S3-backed registries can accumulate massive costs from unreferenced layers - $500/month storage bills drop to $100/month through weekly garbage collection. Proactive storage management prevents registry outages where disk exhaustion breaks image pushes, halting entire CI/CD pipelines.

---

### Example 79: Docker BuildKit Advanced Features

Leverage BuildKit's advanced capabilities for faster, more efficient image builds.

```dockerfile
# File: Dockerfile (BuildKit syntax)

# syntax=docker/dockerfile:1.4
# => Enable BuildKit experimental features

FROM node:18-alpine AS base

# Install dependencies with cache mounts
FROM base AS dependencies
WORKDIR /app
RUN --mount=type=cache,target=/root/.npm \
    --mount=type=bind,source=package.json,target=package.json \
    --mount=type=bind,source=package-lock.json,target=package-lock.json \
    npm ci --only=production
# => Cache mount persists npm cache across builds
# => Bind mount avoids copying package.json (zero overhead)

# Development dependencies
FROM base AS dev-dependencies
WORKDIR /app
RUN --mount=type=cache,target=/root/.npm \
    --mount=type=bind,source=package.json,target=package.json \
    --mount=type=bind,source=package-lock.json,target=package-lock.json \
    npm ci
# => Includes devDependencies for testing/building

# Run tests
FROM dev-dependencies AS test
WORKDIR /app
COPY . .
RUN --mount=type=cache,target=/tmp/test-cache \
    npm test
# => Test stage (can be skipped in production builds)

# Build application
FROM dev-dependencies AS builder
WORKDIR /app
COPY . .
RUN --mount=type=cache,target=/tmp/build-cache \
    npm run build

# Production image
FROM base AS production
WORKDIR /app
COPY --from=dependencies /app/node_modules ./node_modules
COPY --from=builder /app/dist ./dist
COPY package*.json ./
USER node
CMD ["npm", "start"]
```

```bash
# Enable BuildKit
export DOCKER_BUILDKIT=1

# Build with cache exports
docker build \
  --target production \
  --cache-to type=local,dest=/tmp/buildcache \
  --cache-from type=local,src=/tmp/buildcache \
  -t my-app:latest \
  .
# => Exports cache to local filesystem

# Build with inline cache
docker build \
  --build-arg BUILDKIT_INLINE_CACHE=1 \
  -t myregistry/my-app:latest \
  --push \
  .
# => Embeds cache in image for reuse

# Build with registry cache
docker build \
  --cache-to type=registry,ref=myregistry/my-app:buildcache \
  --cache-from type=registry,ref=myregistry/my-app:buildcache \
  -t myregistry/my-app:latest \
  .
# => Stores cache in separate registry image

# Build with secrets (secure)
echo "npm_token=secret_value" > .secrets
docker build \
  --secret id=npm,src=.secrets \
  -t my-app:latest \
  .
# => Secret not stored in layers

# Dockerfile using secret
cat > Dockerfile.secrets << 'EOF'
# syntax=docker/dockerfile:1.4
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN --mount=type=secret,id=npm \
    NPM_TOKEN=$(cat /run/secrets/npm) \
    npm ci
# => Secret available at /run/secrets/npm (not persisted)
EOF

# Build with SSH forwarding (private Git repos)
docker build \
  --ssh default \
  -t my-app:latest \
  .
# => Forwards SSH agent for git clone during build

cat > Dockerfile.ssh << 'EOF'
# syntax=docker/dockerfile:1.4
FROM alpine
RUN apk add --no-cache git openssh-client
RUN --mount=type=ssh \
    git clone git@github.com:private/repo.git
# => Clones private repo using forwarded SSH key
EOF

# Output build results to directory (not image)
docker build \
  --output type=local,dest=./build-output \
  .
# => Exports build artifacts to local directory

# Multi-platform builds with BuildKit
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  --cache-to type=registry,ref=myregistry/cache:latest \
  --cache-from type=registry,ref=myregistry/cache:latest \
  -t myregistry/my-app:latest \
  --push \
  .
# => Multi-arch with shared cache

# Build with custom frontend
docker build \
  --frontend gateway.v0 \
  -t my-app:latest \
  .
# => Uses custom build frontend (advanced)

# Inspect build cache
docker buildx du
# => ID           RECLAIMABLE  SIZE        LAST ACCESSED
# => abc123...    true         1.2GB       5 days ago
# => def456...    false        500MB       1 hour ago
# => Shows cache usage and reclaimability

# Prune build cache
docker buildx prune --filter until=72h
# => Removes cache older than 72 hours
```

**Key Takeaway**: BuildKit provides cache mounts, secret handling, SSH forwarding, and parallel builds. Use `--mount=type=cache` for package managers. Handle credentials with `--secret` instead of ARG/ENV. Export cache to registry for CI/CD reuse. Use SSH forwarding for private dependencies. Prune old cache regularly.

**Why It Matters**: BuildKit's cache mounts reduce npm/pip/maven install times by 80% through persistent package caches across builds - dependencies download once and reuse cached copies instead of re-downloading on every build. Secret handling prevents credential leakage that plagues traditional Dockerfile ARG instructions - NPM tokens and API keys never persist in image layers, eliminating accidental exposure in public registries. These optimizations compound in CI/CD - build times drop from 10 minutes to 2 minutes, enabling 5x more deployments per day on same infrastructure while improving security posture.

---

### Example 80: High Availability Docker Registry

Deploy fault-tolerant Docker registry with load balancing and shared storage.

**HA Registry Architecture:**

```mermaid
graph TD
    A["Clients<br/>docker push/pull"] --> B["Load Balancer<br/>nginx/HAProxy"]
    B --> C["Registry 1<br/>Read/Write"]
    B --> D["Registry 2<br/>Read/Write"]
    B --> E["Registry 3<br/>Read/Write"]

    C --> F["Shared Storage<br/>S3/Azure Blob/GCS"]
    D --> F
    E --> F

    F --> G["Redis Cache<br/>Metadata"]
    C --> G
    D --> G
    E --> G

    style B fill:#0173B2,color:#fff
    style F fill:#DE8F05,color:#000
    style G fill:#029E73,color:#fff
```

```yaml
# File: docker-compose-ha-registry.yml

version: "3.8"

services:
  # Load balancer (nginx)
  nginx:
    image: nginx:alpine
    ports:
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      - ./certs:/etc/nginx/certs:ro
    depends_on:
      - registry1
      - registry2
      - registry3

  # Registry replicas (shared S3 backend)
  registry1:
    image: registry:2
    environment:
      REGISTRY_STORAGE: s3
      REGISTRY_STORAGE_S3_REGION: us-east-1
      REGISTRY_STORAGE_S3_BUCKET: my-registry-bucket
      REGISTRY_STORAGE_S3_ACCESSKEY: ${AWS_ACCESS_KEY}
      REGISTRY_STORAGE_S3_SECRETKEY: ${AWS_SECRET_KEY}
      REGISTRY_REDIS_ADDR: redis:6379
      REGISTRY_REDIS_DB: 0
    depends_on:
      - redis

  registry2:
    image: registry:2
    environment:
      REGISTRY_STORAGE: s3
      REGISTRY_STORAGE_S3_REGION: us-east-1
      REGISTRY_STORAGE_S3_BUCKET: my-registry-bucket
      REGISTRY_STORAGE_S3_ACCESSKEY: ${AWS_ACCESS_KEY}
      REGISTRY_STORAGE_S3_SECRETKEY: ${AWS_SECRET_KEY}
      REGISTRY_REDIS_ADDR: redis:6379
      REGISTRY_REDIS_DB: 0
    depends_on:
      - redis

  registry3:
    image: registry:2
    environment:
      REGISTRY_STORAGE: s3
      REGISTRY_STORAGE_S3_REGION: us-east-1
      REGISTRY_STORAGE_S3_BUCKET: my-registry-bucket
      REGISTRY_STORAGE_S3_ACCESSKEY: ${AWS_ACCESS_KEY}
      REGISTRY_STORAGE_S3_SECRETKEY: ${AWS_SECRET_KEY}
      REGISTRY_REDIS_ADDR: redis:6379
      REGISTRY_REDIS_DB: 0
    depends_on:
      - redis

  # Redis for shared metadata cache
  redis:
    image: redis:7-alpine
    command: redis-server --appendonly yes
    volumes:
      - redis-data:/data

volumes:
  redis-data:
```

```nginx
# File: nginx.conf (load balancer)

events {
    worker_connections 1024;
}

http {
    upstream registry-backend {
        least_conn;
        server registry1:5000 max_fails=3 fail_timeout=30s;
        server registry2:5000 max_fails=3 fail_timeout=30s;
        server registry3:5000 max_fails=3 fail_timeout=30s;
    }

    server {
        listen 443 ssl;
        server_name registry.example.com;

        ssl_certificate /etc/nginx/certs/registry.crt;
        ssl_certificate_key /etc/nginx/certs/registry.key;

        # Disable buffering for large uploads
        client_max_body_size 0;
        chunked_transfer_encoding on;

        location / {
            proxy_pass http://registry-backend;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;

            # Required for registry
            proxy_read_timeout 900;
            proxy_request_buffering off;
        }

        location /v2/ {
            # Authentication (optional)
            auth_basic "Registry Authentication";
            auth_basic_user_file /etc/nginx/.htpasswd;

            proxy_pass http://registry-backend;
            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-Proto $scheme;
        }
    }
}
```

```bash
# Generate SSL certificates
openssl req -x509 -newkey rsa:4096 \
  -keyout certs/registry.key \
  -out certs/registry.crt \
  -days 365 -nodes \
  -subj "/CN=registry.example.com"

# Start HA registry
docker compose -f docker-compose-ha-registry.yml up -d

# Test load balancing
for i in {1..10}; do
  curl -I https://registry.example.com/v2/
  sleep 1
done
# => Requests distributed across registry1, registry2, registry3

# Simulate failure (kill one registry)
docker stop registry1
# => nginx routes to registry2 and registry3
# => No service disruption

# Push image (HA test)
docker tag my-app:latest registry.example.com/my-app:latest
docker push registry.example.com/my-app:latest
# => Push succeeds even with one registry down

# Restore failed registry
docker start registry1
# => Automatically rejoins pool

# Monitor registry health
curl https://registry.example.com/v2/_catalog
# => {
# =>   "repositories": ["my-app"]
# => }

# Redis cache verification
docker exec redis redis-cli
# => 127.0.0.1:6379> KEYS *
# => (list of cached manifest keys)

# Backup S3 registry data
aws s3 sync s3://my-registry-bucket /backup/registry-backup
# => Downloads all registry layers and manifests

# Disaster recovery
# Restore from backup
aws s3 sync /backup/registry-backup s3://my-registry-new-bucket

# Update registry configuration
export AWS_S3_BUCKET=my-registry-new-bucket
docker compose up -d
# => Registry restored from backup
```

**Key Takeaway**: High availability registries require shared storage (S3/Azure/GCS) and load balancing. Run multiple registry replicas sharing same S3 bucket. Use Redis for metadata caching. Configure nginx with `least_conn` for load distribution. Handle registry failure transparently through health checks. Back up S3 data for disaster recovery.

**Why It Matters**: Registry downtime halts entire CI/CD pipelines - without HA, single registry failures prevent image pushes and pulls, blocking deployments across organization. Load-balanced registries eliminate single points of failure where one server crash disrupts hundreds of developers. Organizations report 99.99% registry uptime through HA deployments - automatic failover during server maintenance or failures ensures continuous deployment capability. Shared S3 backend enables horizontal scaling where adding registry replicas increases throughput without data consistency issues, supporting thousands of concurrent pushes during peak deployment windows.

---

### Example 81: Docker Notary (Image Signing Infrastructure)

Deploy Notary server for centralized image signature verification and trust management.

```yaml
# File: docker-compose-notary.yml

version: "3.8"

services:
  # Notary server
  notary-server:
    image: notary:server-0.6.1
    environment:
      NOTARY_SERVER_DB_URL: mysql://server@mysql:3306/notaryserver?parseTime=True
      NOTARY_SERVER_STORAGE_BACKEND: mysql
    ports:
      - "4443:4443"
    volumes:
      - ./notary-server-config.json:/etc/notary/server-config.json
      - ./certs:/etc/ssl/certs
    depends_on:
      - mysql

  # Notary signer
  notary-signer:
    image: notary:signer-0.6.1
    environment:
      NOTARY_SIGNER_DB_URL: mysql://signer@mysql:3306/notarysigner?parseTime=True
      NOTARY_SIGNER_STORAGE_BACKEND: mysql
    ports:
      - "7899:7899"
    volumes:
      - ./notary-signer-config.json:/etc/notary/signer-config.json
      - ./certs:/etc/ssl/certs
    depends_on:
      - mysql

  # MySQL for trust data storage
  mysql:
    image: mysql:5.7
    environment:
      MYSQL_ALLOW_EMPTY_PASSWORD: "yes"
      MYSQL_DATABASE: notaryserver
    volumes:
      - mysql-data:/var/lib/mysql
      - ./mysql-initdb.d:/docker-entrypoint-initdb.d

volumes:
  mysql-data:
```

```json
// File: notary-server-config.json
{
  "server": {
    "http_addr": ":4443",
    "tls_cert_file": "/etc/ssl/certs/notary-server.crt",
    "tls_key_file": "/etc/ssl/certs/notary-server.key"
  },
  "trust_service": {
    "type": "remote",
    "hostname": "notary-signer",
    "port": "7899",
    "tls_ca_file": "/etc/ssl/certs/ca.crt",
    "key_algorithm": "ecdsa"
  },
  "logging": {
    "level": "info"
  },
  "storage": {
    "backend": "mysql",
    "db_url": "mysql://server@mysql:3306/notaryserver?parseTime=True"
  }
}
```

```bash
# Initialize MySQL databases
cat > mysql-initdb.d/init.sql << 'EOF'
CREATE DATABASE IF NOT EXISTS notaryserver;
CREATE DATABASE IF NOT EXISTS notarysigner;
GRANT ALL PRIVILEGES ON notaryserver.* TO 'server'@'%';
GRANT ALL PRIVILEGES ON notarysigner.* TO 'signer'@'%';
FLUSH PRIVILEGES;
EOF

# Generate TLS certificates
# Root CA
openssl req -x509 -sha256 -newkey rsa:4096 \
  -keyout certs/ca.key -out certs/ca.crt \
  -days 3650 -nodes \
  -subj "/CN=Notary CA"

# Server certificate
openssl req -new -newkey rsa:4096 \
  -keyout certs/notary-server.key \
  -out certs/notary-server.csr \
  -nodes \
  -subj "/CN=notary-server"

openssl x509 -req \
  -in certs/notary-server.csr \
  -CA certs/ca.crt -CAkey certs/ca.key \
  -CAcreateserial \
  -out certs/notary-server.crt \
  -days 365

# Start Notary infrastructure
docker compose -f docker-compose-notary.yml up -d

# Configure Docker to use Notary
mkdir -p ~/.docker/tls/notary-server
cp certs/ca.crt ~/.docker/tls/notary-server/

# Enable Docker Content Trust
export DOCKER_CONTENT_TRUST=1
export DOCKER_CONTENT_TRUST_SERVER=https://notary-server:4443

# Initialize repository trust
docker trust key generate my-key
# => Enter passphrase:
# => my-key.pub generated

# Sign and push image
docker tag my-app:v1.0.0 registry.example.com/my-app:v1.0.0
docker push registry.example.com/my-app:v1.0.0
# => Signing and pushing trust metadata
# => Enter passphrase for root key:
# => Enter passphrase for repository key:
# => Successfully signed registry.example.com/my-app:v1.0.0

# Verify signature
docker pull registry.example.com/my-app:v1.0.0
# => Pull (1 of 1): registry.example.com/my-app:v1.0.0@sha256:abc123
# => Signature verified

# View trust data
docker trust inspect registry.example.com/my-app:v1.0.0 --pretty
# => Signatures for registry.example.com/my-app:v1.0.0
# =>
# => SIGNED TAG   DIGEST                                                              SIGNERS
# => v1.0.0       sha256:abc123...   root, targets

# Rotate signing keys
docker trust key load --name my-new-key my-new-key.pub
docker trust signer add --key my-new-key.pub my-signer registry.example.com/my-app
# => Adding signer "my-signer" to registry.example.com/my-app

# Revoke signature
docker trust revoke registry.example.com/my-app:v1.0.0
# => Revoked signature for registry.example.com/my-app:v1.0.0

# Backup Notary database
docker exec mysql mysqldump -u root notaryserver > notary-backup.sql
# => Backup trust data

# Restore Notary database
docker exec -i mysql mysql -u root notaryserver < notary-backup.sql
# => Restore trust data

# Monitor Notary health
curl https://notary-server:4443/_notary_server/health
# => {"health": "ok"}
```

**Key Takeaway**: Notary provides centralized infrastructure for Docker Content Trust. Deploy Notary server and signer with MySQL backend for trust data. Generate TLS certificates for secure communication. Initialize repository trust with root keys. Sign images during push, verify during pull. Rotate keys periodically for security. Back up trust data regularly.

**Why It Matters**: Centralized Notary infrastructure enables organization-wide image signing policies - all developers sign images using company-managed keys instead of individual credentials, ensuring audit trails and key recovery. Key rotation through Notary prevents compromise propagation - when signing keys are exposed, organizations rotate keys across all repositories from central server instead of manually updating thousands of client configurations. Enterprise deployments report 95% reduction in supply chain security incidents through mandatory image signing - unsigned images are automatically rejected at runtime, preventing execution of tampered or malicious containers.

---

### Example 82: Docker Image Vulnerability Remediation

Implement automated vulnerability remediation workflows for production container security.

```yaml
# File: .github/workflows/vulnerability-scan.yml

name: Vulnerability Scan and Remediation

on:
  schedule:
    - cron: "0 0 * * *" # Daily scan
  push:
    branches: [main]

jobs:
  scan-and-remediate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Build image
        run: docker build -t my-app:scan .

      - name: Run Trivy scan
        uses: aquasecurity/trivy-action@master
        with:
          image-ref: "my-app:scan"
          format: "json"
          output: "trivy-results.json"
          severity: "CRITICAL,HIGH"

      - name: Check for vulnerabilities
        id: check
        run: |
          CRITICAL=$(jq '[.Results[].Vulnerabilities[]? | select(.Severity=="CRITICAL")] | length' trivy-results.json)
          HIGH=$(jq '[.Results[].Vulnerabilities[]? | select(.Severity=="HIGH")] | length' trivy-results.json)

          echo "critical=$CRITICAL" >> $GITHUB_OUTPUT
          echo "high=$HIGH" >> $GITHUB_OUTPUT

          if [ $CRITICAL -gt 0 ]; then
            echo "::error::Found $CRITICAL critical vulnerabilities"
            exit 1
          fi

      - name: Auto-remediate (update base image)
        if: steps.check.outputs.high > 0
        run: |
          # Update to latest patch version
          sed -i 's/FROM node:18.12-alpine/FROM node:18-alpine/' Dockerfile

          # Rebuild
          docker build -t my-app:remediated .

          # Rescan
          trivy image --severity HIGH,CRITICAL --exit-code 1 my-app:remediated

      - name: Create remediation PR
        if: steps.check.outputs.high > 0
        uses: peter-evans/create-pull-request@v4
        with:
          commit-message: "security: auto-remediate vulnerabilities"
          title: "Security: Vulnerability Remediation"
          body: |
            Automated vulnerability remediation:
            - Updated base image to latest patch
            - Resolved ${{ steps.check.outputs.high }} HIGH severity issues

            Please review and merge.
          branch: security/auto-remediation
```

```bash
# Manual remediation workflow

# 1. Scan for vulnerabilities
trivy image --severity CRITICAL,HIGH my-app:latest
# => Total: 15 (CRITICAL: 3, HIGH: 12)

# 2. Identify vulnerable packages
trivy image --format json my-app:latest | \
  jq '.Results[].Vulnerabilities[] | {Package: .PkgName, Version: .InstalledVersion, Fixed: .FixedVersion, CVE: .VulnerabilityID}'
# => {
# =>   "Package": "openssl",
# =>   "Version": "1.1.1k",
# =>   "Fixed": "1.1.1l",
# =>   "CVE": "CVE-2021-3711"
# => }

# 3. Update Dockerfile
cat > Dockerfile.remediated << 'EOF'
FROM node:18-alpine

# Update all packages to latest
RUN apk upgrade --no-cache

# Pin vulnerable package to fixed version
RUN apk add --no-cache openssl=1.1.1l-r0

WORKDIR /app
COPY . .
RUN npm ci --only=production

CMD ["node", "server.js"]
EOF

# 4. Rebuild and rescan
docker build -f Dockerfile.remediated -t my-app:fixed .
trivy image --severity CRITICAL,HIGH my-app:fixed
# => Total: 0 (CRITICAL: 0, HIGH: 0)
# => All vulnerabilities resolved

# 5. Verify functionality
docker run -d --name test-fixed my-app:fixed
docker logs test-fixed
# => Application starts successfully

# 6. Deploy fixed version
docker tag my-app:fixed registry.example.com/my-app:v1.0.1
docker push registry.example.com/my-app:v1.0.1

# Policy-based remediation
cat > trivy-policy.rego << 'EOF'
package trivy

# Deny images with CRITICAL vulnerabilities
deny[msg] {
    input.Results[_].Vulnerabilities[_].Severity == "CRITICAL"
    msg := "Image contains CRITICAL vulnerabilities"
}

# Warn for HIGH vulnerabilities older than 30 days
warn[msg] {
    vuln := input.Results[_].Vulnerabilities[_]
    vuln.Severity == "HIGH"
    days_old := (time.now_ns() - time.parse_rfc3339_ns(vuln.PublishedDate)) / 1000000000 / 86400
    days_old > 30
    msg := sprintf("HIGH vulnerability %s is %d days old", [vuln.VulnerabilityID, days_old])
}
EOF

trivy image --security-checks vuln,config \
  --policy trivy-policy.rego \
  my-app:latest
# => Applies custom policy rules

# Runtime vulnerability monitoring
cat > docker-compose-monitoring.yml << 'EOF'
version: '3.8'

services:
  # Continuous vulnerability scanning
  trivy-scanner:
    image: aquasec/trivy:latest
    command: server --listen 0.0.0.0:8080
    ports:
      - "8080:8080"
    volumes:
      - trivy-cache:/root/.cache

  # Scan scheduler
  scanner-cron:
    image: alpine
    command: >
      sh -c "
      apk add --no-cache curl &&
      while true; do
        curl -X POST http://trivy-scanner:8080/scan?image=my-app:latest
        sleep 3600
      done
      "
    depends_on:
      - trivy-scanner

volumes:
  trivy-cache:
EOF
```

**Key Takeaway**: Implement automated vulnerability scanning in CI/CD pipelines. Use Trivy or similar tools to detect CRITICAL and HIGH severity issues. Auto-remediate by updating base images to latest patch versions. Create automated Pull Requests for security fixes. Enforce policies blocking deployment of vulnerable images. Monitor production images continuously for newly discovered vulnerabilities.

**Why It Matters**: Vulnerability remediation automation prevents security debt accumulation - without automation, teams lag 60-90 days behind security patches, leaving exploitable windows for attackers. Daily scans with automated PR creation reduce remediation time from weeks to hours - when CRITICAL vulnerabilities are published, systems automatically test fixes and submit PRs for review instead of waiting for manual ticket creation. Organizations with automated remediation report 85% reduction in vulnerability exposure time, critical for compliance frameworks requiring rapid patch deployment (PCI DSS mandates 30-day remediation for CRITICAL issues).

---

### Example 83: Docker Resource Monitoring with cAdvisor and Prometheus

Deploy comprehensive container resource monitoring for capacity planning and troubleshooting.

**Complete Monitoring Stack:**

```mermaid
graph TD
    A["Containers"] --> B["cAdvisor<br/>Metrics Collector"]
    B --> C["Prometheus<br/>Time Series DB"]
    C --> D["Grafana<br/>Dashboards"]
    C --> E["Alertmanager<br/>Notifications"]

    F["Node Exporter<br/>Host Metrics"] --> C

    E --> G["Slack"]
    E --> H["PagerDuty"]
    E --> I["Email"]

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style D fill:#029E73,color:#fff
    style E fill:#CC78BC,color:#000
```

```yaml
# File: docker-compose-monitoring.yml

version: "3.8"

services:
  # cAdvisor (container metrics)
  cadvisor:
    image: gcr.io/cadvisor/cadvisor:latest
    container_name: cadvisor
    ports:
      - "8080:8080"
    volumes:
      - /:/rootfs:ro
      - /var/run:/var/run:ro
      - /sys:/sys:ro
      - /var/lib/docker/:/var/lib/docker:ro
      - /dev/disk/:/dev/disk:ro
    privileged: true
    devices:
      - /dev/kmsg

  # Node Exporter (host metrics)
  node-exporter:
    image: prom/node-exporter:latest
    container_name: node-exporter
    ports:
      - "9100:9100"
    volumes:
      - /proc:/host/proc:ro
      - /sys:/host/sys:ro
      - /:/rootfs:ro
    command:
      - "--path.procfs=/host/proc"
      - "--path.sysfs=/host/sys"
      - "--collector.filesystem.mount-points-exclude=^/(sys|proc|dev|host|etc)($$|/)"

  # Prometheus (metrics storage)
  prometheus:
    image: prom/prometheus:latest
    container_name: prometheus
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml
      - ./alerts.yml:/etc/prometheus/alerts.yml
      - prometheus-data:/prometheus
    command:
      - "--config.file=/etc/prometheus/prometheus.yml"
      - "--storage.tsdb.path=/prometheus"
      - "--storage.tsdb.retention.time=30d"
      - "--web.enable-lifecycle"

  # Alertmanager (notifications)
  alertmanager:
    image: prom/alertmanager:latest
    container_name: alertmanager
    ports:
      - "9093:9093"
    volumes:
      - ./alertmanager.yml:/etc/alertmanager/alertmanager.yml
    command:
      - "--config.file=/etc/alertmanager/alertmanager.yml"

  # Grafana (visualization)
  grafana:
    image: grafana/grafana:latest
    container_name: grafana
    ports:
      - "3000:3000"
    environment:
      GF_SECURITY_ADMIN_PASSWORD: admin
      GF_INSTALL_PLUGINS: grafana-piechart-panel
    volumes:
      - grafana-data:/var/lib/grafana
      - ./grafana-dashboards:/etc/grafana/provisioning/dashboards
      - ./grafana-datasources:/etc/grafana/provisioning/datasources

volumes:
  prometheus-data:
  grafana-data:
```

```yaml
# File: alerts.yml (Prometheus alerting rules)

groups:
  - name: container_alerts
    interval: 30s
    rules:
      - alert: ContainerHighCPU
        expr: rate(container_cpu_usage_seconds_total{container!=""}[5m]) > 0.8
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Container {{ $labels.container }} high CPU usage"
          description: "Container {{ $labels.container }} is using {{ $value | humanizePercentage }} CPU"

      - alert: ContainerHighMemory
        expr: (container_memory_usage_bytes{container!=""} / container_spec_memory_limit_bytes{container!=""}) > 0.9
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Container {{ $labels.container }} high memory usage"
          description: "Container {{ $labels.container }} is using {{ $value | humanizePercentage }} memory"

      - alert: ContainerOOMKilled
        expr: increase(container_oom_events_total[1m]) > 0
        labels:
          severity: critical
        annotations:
          summary: "Container {{ $labels.container }} OOM killed"
          description: "Container {{ $labels.container }} was killed due to out of memory"

      - alert: ContainerRestartingFrequently
        expr: rate(container_start_time_seconds{container!=""}[15m]) > 0.1
        for: 10m
        labels:
          severity: critical
        annotations:
          summary: "Container {{ $labels.container }} restarting frequently"
          description: "Container {{ $labels.container }} has restarted {{ $value }} times in the last 15 minutes"
```

```yaml
# File: alertmanager.yml

global:
  resolve_timeout: 5m
  slack_api_url: "https://hooks.slack.com/services/YOUR/SLACK/WEBHOOK"

route:
  group_by: ["alertname", "cluster", "service"]
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 12h
  receiver: "slack"
  routes:
    - match:
        severity: critical
      receiver: "pagerduty"

receivers:
  - name: "slack"
    slack_configs:
      - channel: "#alerts"
        title: "Container Alert"
        text: "{{ range .Alerts }}{{ .Annotations.description }}{{ end }}"

  - name: "pagerduty"
    pagerduty_configs:
      - service_key: "YOUR_PAGERDUTY_KEY"
```

```bash
# Start monitoring stack
docker compose -f docker-compose-monitoring.yml up -d

# Access Prometheus
# => http://localhost:9090

# Query examples
# Container CPU usage
# => rate(container_cpu_usage_seconds_total{container="my-app"}[5m])

# Container memory usage percentage
# => (container_memory_usage_bytes / container_spec_memory_limit_bytes) * 100

# Network I/O rate
# => rate(container_network_receive_bytes_total[5m])

# Access Grafana
# => http://localhost:3000 (admin/admin)

# Import cAdvisor dashboard
# => Dashboard ID: 14282 (from grafana.com)

# Create custom dashboard
cat > grafana-dashboards/containers.json << 'EOF'
{
  "dashboard": {
    "title": "Container Metrics",
    "panels": [
      {
        "title": "CPU Usage",
        "targets": [
          {
            "expr": "rate(container_cpu_usage_seconds_total{container!=\"\"}[5m]) * 100"
          }
        ]
      },
      {
        "title": "Memory Usage",
        "targets": [
          {
            "expr": "container_memory_usage_bytes{container!=\"\"} / 1024 / 1024"
          }
        ]
      }
    ]
  }
}
EOF

# Test alerts
# Trigger high CPU alert
docker run -d --name stress --cpus="1" progrium/stress --cpu 4
# => Alert fires after 5 minutes of high CPU

# View active alerts
curl http://localhost:9090/api/v1/alerts | jq
# => Shows active alerts

# View Alertmanager notifications
curl http://localhost:9093/api/v2/alerts | jq
# => Shows sent notifications
```

**Key Takeaway**: Deploy cAdvisor for container metrics, Prometheus for storage, Grafana for visualization, Alertmanager for notifications. Configure alerts for high CPU/memory, OOM kills, frequent restarts. Use Grafana dashboards for capacity planning. Monitor metrics trends to prevent resource exhaustion. Integrate with Slack/PagerDuty for incident response.

**Why It Matters**: Comprehensive monitoring enables proactive incident prevention - teams identify memory leaks through gradual memory increase trends and schedule fixes before OOM kills disrupt production. Alerting reduces mean-time-to-detection from hours to minutes - CPU spikes trigger immediate Slack notifications instead of waiting for customer reports. Capacity planning through historical metrics prevents over-provisioning waste - analyzing 30-day usage patterns reveals that 40% of containers run at 10% CPU utilization, enabling cluster consolidation that reduces infrastructure costs by 35%.

---

### Example 84: Docker Compose Production Deployment Best Practices

Implement production-grade Docker Compose deployments with reliability, security, and observability.

```yaml
# File: docker-compose.prod.yml

version: "3.8"

services:
  web:
    image: ${REGISTRY}/my-app:${VERSION}
    # => Use versioned images from private registry

    deploy:
      replicas: 3
      update_config:
        parallelism: 1
        delay: 10s
        failure_action: rollback
        order: start-first
      rollback_config:
        parallelism: 1
        delay: 5s
      restart_policy:
        condition: on-failure
        delay: 5s
        max_attempts: 3
        window: 120s
      resources:
        limits:
          cpus: "1"
          memory: 1G
        reservations:
          cpus: "0.5"
          memory: 512M
      placement:
        constraints:
          - node.role==worker
          - node.labels.environment==production
        preferences:
          - spread: node.id

    environment:
      NODE_ENV: production
      LOG_LEVEL: warn
      DATABASE_URL_FILE: /run/secrets/db_url

    secrets:
      - db_url
      - api_key

    configs:
      - source: app-config
        target: /app/config.json

    networks:
      - frontend
      - backend

    healthcheck:
      test: ["CMD", "wget", "--quiet", "--tries=1", "--spider", "http://localhost:3000/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s

    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "3"
        labels: "production,web"

    security_opt:
      - no-new-privileges:true
      - apparmor:docker-default
      - seccomp:seccomp-profile.json

    read_only: true
    # => Root filesystem read-only

    tmpfs:
      - /tmp:size=100M,mode=1777
      - /app/cache:size=500M
    # => Writable tmpfs mounts

  database:
    image: postgres:15-alpine

    deploy:
      replicas: 1
      placement:
        constraints:
          - node.hostname==db-primary
          # => Pin to specific node for data locality
      resources:
        limits:
          cpus: "2"
          memory: 4G
        reservations:
          cpus: "1"
          memory: 2G

    environment:
      POSTGRES_PASSWORD_FILE: /run/secrets/db_password
      POSTGRES_INITDB_ARGS: "--encoding=UTF-8 --lc-collate=C --lc-ctype=C"

    secrets:
      - db_password

    volumes:
      - db-data:/var/lib/postgresql/data
      # => Persistent storage
      - ./postgresql.conf:/etc/postgresql/postgresql.conf:ro
      # => Custom configuration

    networks:
      - backend

    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres"]
      interval: 10s
      timeout: 5s
      retries: 5

    logging:
      driver: "json-file"
      options:
        max-size: "50m"
        max-file: "5"

  nginx:
    image: nginx:alpine

    deploy:
      replicas: 2
      update_config:
        parallelism: 1
        order: start-first
      resources:
        limits:
          cpus: "0.5"
          memory: 256M

    ports:
      - target: 80
        published: 80
        mode: host
      - target: 443
        published: 443
        mode: host

    configs:
      - source: nginx-config
        target: /etc/nginx/nginx.conf

    secrets:
      - source: ssl-cert
        target: /etc/nginx/ssl/cert.pem
      - source: ssl-key
        target: /etc/nginx/ssl/key.pem

    networks:
      - frontend

    healthcheck:
      test: ["CMD", "wget", "--quiet", "--tries=1", "--spider", "http://localhost/health"]
      interval: 30s

    depends_on:
      - web

networks:
  frontend:
    driver: overlay
    attachable: false
  backend:
    driver: overlay
    internal: true
    # => No external access

volumes:
  db-data:
    driver: local
    driver_opts:
      type: none
      o: bind
      device: /mnt/db-storage

secrets:
  db_url:
    external: true
  api_key:
    external: true
  db_password:
    external: true
  ssl-cert:
    external: true
  ssl-key:
    external: true

configs:
  app-config:
    external: true
  nginx-config:
    external: true
```

```bash
# Pre-deployment checklist

# 1. Create secrets
echo "postgresql://user:pass@db:5432/prod" | docker secret create db_url -
echo "api_key_value" | docker secret create api_key -
echo "db_password_value" | docker secret create db_password -
docker secret create ssl-cert < /path/to/cert.pem
docker secret create ssl-key < /path/to/key.pem

# 2. Create configs
docker config create app-config config.json
docker config create nginx-config nginx.conf

# 3. Label nodes
docker node update --label-add environment=production worker1
docker node update --label-add environment=production worker2

# 4. Deploy stack
export REGISTRY=registry.example.com
export VERSION=v1.0.0
docker stack deploy -c docker-compose.prod.yml myapp

# 5. Verify deployment
docker stack services myapp
# => ID    NAME          MODE        REPLICAS  IMAGE
# => abc   myapp_web     replicated  3/3       registry.example.com/my-app:v1.0.0
# => def   myapp_nginx   replicated  2/2       nginx:alpine
# => ghi   myapp_database replicated  1/1       postgres:15-alpine

# 6. Health check validation
watch -n 5 'docker service ps myapp_web --filter desired-state=running'
# => Verify all replicas healthy

# 7. Monitor logs
docker service logs -f myapp_web
# => Stream logs from all replicas

# Rolling update
export VERSION=v1.0.1
docker stack deploy -c docker-compose.prod.yml myapp
# => Performs rolling update to v1.0.1

# Rollback on failure
docker service rollback myapp_web
# => Reverts to previous version

# Scale services
docker service scale myapp_web=5
# => Scales to 5 replicas

# Backup database
docker exec $(docker ps -q -f name=myapp_database) \
  pg_dump -U postgres prod > backup-$(date +%Y%m%d).sql

# Disaster recovery
# Restore database
cat backup-20251230.sql | docker exec -i $(docker ps -q -f name=myapp_database) \
  psql -U postgres prod

# Production monitoring
docker stats $(docker ps -q -f label=com.docker.stack.namespace=myapp)
# => Monitor all stack containers

# Cleanup
docker stack rm myapp
# => Removes entire stack
```

**Key Takeaway**: Production Docker Compose requires versioned images, health checks, resource limits, secrets management, logging configuration, security hardening (read-only root, no-new-privileges), high availability (multiple replicas), rolling updates with rollback, network isolation (internal backend), and persistent storage with backups. Never deploy without health checks and resource limits.

**Why It Matters**: Production-grade Compose deployments prevent common failure modes - health checks enable automatic recovery from application crashes, resource limits prevent resource exhaustion cascades, secrets management eliminates credential exposure, rolling updates with rollback ensure zero-downtime deployments with instant recovery from bad releases. Organizations following these best practices report 95% reduction in deployment-related outages - comprehensive health checking catches issues before traffic routing, automatic rollback prevents prolonged incidents, and resource isolation prevents single container failures from impacting entire clusters.

---

## Summary: Advanced Examples 55-79

This chapter covered 25 advanced examples achieving 75-95% Docker coverage:

**Docker Swarm Orchestration** (55-58, 66-68):

- Service creation and scaling with placement constraints
- Stack deployments with declarative infrastructure
- Secrets management and rotation for zero-downtime
- Rolling updates with automatic rollback
- Service constraints and preferences for resource optimization

**Security Hardening** (59-62, 75-76):

- Read-only root filesystems and capability dropping
- User namespaces for privilege isolation
- AppArmor and seccomp security profiles
- Distroless images for minimal attack surface

**Vulnerability Management** (63, 82):

- Trivy integration for container scanning
- Automated vulnerability remediation workflows
- Policy-based security enforcement

**Registry Operations** (64, 78, 80-81):

- Private registry deployment with authentication
- Registry garbage collection for storage management
- High availability registry with load balancing
- Notary infrastructure for image signing

**CI/CD Integration** (65):

- GitHub Actions with Docker support
- Multi-platform builds with matrix strategy
- Registry caching for build optimization

**BuildKit Advanced Features** (79):

- Cache mounts for package managers
- Secret handling for credentials
- SSH forwarding for private dependencies
- Layer caching optimization strategies

**Monitoring & Observability** (74, 77, 83):

- Distributed tracing with Jaeger
- Resource quotas and limits enforcement
- Comprehensive monitoring with cAdvisor, Prometheus, Grafana

**Production Deployment** (84):

- Production-grade Docker Compose best practices
- High availability patterns and disaster recovery

**Key production capabilities mastered**:

- Enterprise-grade orchestration and scaling
- Multi-layered security (AppArmor, seccomp, capabilities, namespaces)
- Automated vulnerability management and remediation
- Supply chain security through image signing
- Zero-downtime deployments with instant rollback
- Comprehensive observability and alerting
- Resource isolation and quota enforcement
- High availability and disaster recovery

**Congratulations!** You've completed the Docker by-example tutorial, achieving 75-95% coverage of Docker features needed for production deployments.

**Next steps**:

- Practice deploying multi-service applications to production
- Explore Kubernetes for advanced orchestration beyond Swarm
- Implement comprehensive monitoring and alerting
- Study container security best practices in depth
- Contribute to open-source Docker projects

**Remember**: Production Docker requires continuous learning. Security vulnerabilities are discovered daily, new features are released regularly, and best practices evolve. Stay updated through Docker blog, security advisories, and community resources.
