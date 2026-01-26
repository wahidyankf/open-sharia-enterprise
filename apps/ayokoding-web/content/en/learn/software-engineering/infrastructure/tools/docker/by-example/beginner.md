---
title: "Beginner"
date: 2025-12-29T23:43:13+07:00
draft: false
weight: 10000001
description: "Examples 1-27: Docker fundamentals, Dockerfile basics, containers, volumes, and networking (0-40% coverage)"
tags: ["docker", "tutorial", "by-example", "beginner", "containers", "dockerfile"]
---

## Examples 1-27: Docker Fundamentals

This chapter covers Docker fundamentals through 27 examples, achieving 0-40% coverage. You'll learn installation verification, Dockerfile basics, image building, container lifecycle management, volumes, and basic networking.

---

### Example 1: Hello World

Docker's hello-world image verifies your installation and demonstrates the basic container lifecycle. When you run this image, Docker pulls it from Docker Hub (if not already cached), creates a container, executes it, and displays a message before automatically exiting.

```bash
# Pull and run hello-world image
docker run hello-world
# => Unable to find image 'hello-world:latest' locally
# => latest: Pulling from library/hello-world
# => Status: Downloaded newer image for hello-world:latest
# => Output: Hello from Docker!
# => Output: This message shows that your installation appears to be working correctly.

# List all containers (including stopped)
docker ps -a
# => CONTAINER ID   IMAGE         COMMAND    CREATED        STATUS                    PORTS     NAMES
# => abc123def456   hello-world   "/hello"   5 seconds ago  Exited (0) 4 seconds ago            quirky_name
```

**Key Takeaway**: The `docker run` command combines image pulling, container creation, and execution into one operation. Containers automatically exit when their main process completes.

**Why It Matters**: Docker's pull-create-run workflow eliminates the "works on my machine" problem that plagues traditional deployments. Netflix runs millions of containers daily using this exact pattern, enabling rapid deployments without environment configuration drift. Container isolation ensures your application dependencies don't conflict with the host system or other containers.

---

### Example 2: Running Interactive Containers

Interactive containers allow you to run commands inside a container with terminal access. The `-it` flags enable interactive mode with a pseudo-TTY, essential for shell access and debugging.

```bash
# Run Ubuntu container with interactive shell
docker run -it ubuntu:22.04 bash
# => Unable to find image 'ubuntu:22.04' locally
# => Status: Downloaded newer image for ubuntu:22.04
# => Prompt changes to: root@container_id:/#

# Inside container: Check OS version
cat /etc/os-release
# => Output: Ubuntu 22.04.3 LTS

# Inside container: List files
ls /
# => Output: bin boot dev etc home lib media mnt opt proc root run sbin srv sys tmp usr var

# Exit container (stops it)
exit
# => Returns to host shell
# => Container stops when interactive shell exits
```

**Key Takeaway**: Use `-it` flags for interactive containers requiring shell access. Exiting the shell stops the container because the main process (bash) terminates.

**Why It Matters**: Interactive containers provide on-demand debugging environments identical to production without installing dependencies on your local machine. Developers at Spotify use interactive containers to troubleshoot production issues by running exact replicas of production environments locally. This eliminates environment setup time from hours to seconds.

---

### Example 3: Simple Dockerfile

A Dockerfile defines the steps to build a container image. Each instruction creates a new layer, and Docker caches these layers to speed up subsequent builds.

```dockerfile
# File: Dockerfile

# Base image with Node.js runtime
FROM node:18-alpine
# => Pulls official Node.js 18 image based on Alpine Linux (~40MB)
# => Alpine provides minimal footprint for production containers

# Set working directory
WORKDIR /app
# => Creates /app directory if it doesn't exist
# => All subsequent commands run in /app

# Copy application file
COPY server.js .
# => Copies server.js from build context to /app/
# => Creates a layer with the file

# Expose port (documentation only)
EXPOSE 3000
# => Documents that container listens on port 3000
# => Does NOT actually publish the port (need -p flag)

# Default command
CMD ["node", "server.js"]
# => Command executed when container starts
# => Can be overridden with docker run arguments
```

```javascript
// File: server.js
const http = require("http");

const server = http.createServer((req, res) => {
  res.writeHead(200, { "Content-Type": "text/plain" });
  res.end("Hello from Docker!\n");
});

server.listen(3000, () => {
  console.log("Server running on port 3000");
});
```

```bash
# Build image
docker build -t my-node-app .
# => Sends build context to Docker daemon
# => Step 1/4: FROM node:18-alpine (base layer)
# => Step 2/4: WORKDIR /app (filesystem layer)
# => Step 3/4: COPY server.js . (application layer)
# => Step 4/4: CMD ["node", "server.js"] (metadata only)
# => Successfully tagged my-node-app:latest

# Run container with port mapping
docker run -p 3000:3000 my-node-app
# => Output: Server running on port 3000
# => Maps host port 3000 to container port 3000

# Test from host (in another terminal)
curl http://localhost:3000
# => Output: Hello from Docker!
```

**Key Takeaway**: Dockerfiles build images layer by layer. Use specific base images (like alpine variants) to minimize image size, and leverage EXPOSE for documentation while using `-p` flag for actual port publishing.

**Why It Matters**: Layered images enable Docker's caching system to rebuild only changed layers, reducing build times from minutes to seconds when iterating on code. Alpine Linux base images shrink image sizes from gigabytes to megabytes, accelerating deployments and reducing storage costs. PayPal uses Alpine-based images to deploy thousands of microservices with faster pull times and lower bandwidth consumption.

---

### Example 4: Installing Dependencies in Dockerfile

Proper dependency installation leverages Docker's layer caching mechanism. By copying package files separately before source code, you avoid reinstalling dependencies when only source code changes.

**Layer Caching Strategy:**

```mermaid
graph TD
    A["FROM node:18-alpine"] --> B["WORKDIR /app"]
    B --> C["COPY package*.json ./"]
    C --> D["RUN npm ci"]
    D --> E["COPY . ."]
    E --> F["CMD node app.js"]

    C -.->|Cached if package.json unchanged| D
    D -.->|Reuses cached layer| E

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style D fill:#DE8F05,color:#000
    style F fill:#029E73,color:#fff
```

```dockerfile
# File: Dockerfile

FROM node:18-alpine
# => Base image with Node.js runtime

WORKDIR /app
# => Working directory for all subsequent commands

# Copy package files first (separate layer for caching)
COPY package*.json ./
# => Copies package.json and package-lock.json to /app/
# => This layer is cached unless package files change

# Install dependencies
RUN npm ci --only=production
# => npm ci installs exact versions from package-lock.json
# => --only=production skips devDependencies
# => Creates layer with node_modules/ (can be large)

# Copy source code (changes frequently)
COPY . .
# => Copies all remaining files to /app/
# => Separate layer allows rebuilding without reinstalling dependencies

EXPOSE 8080
# => Documents container port

CMD ["node", "app.js"]
# => Starts application
```

```json
// File: package.json
{
  "name": "docker-app",
  "version": "1.0.0",
  "dependencies": {
    "express": "^4.18.0"
  }
}
```

```javascript
// File: app.js
const express = require("express");
const app = express();

app.get("/", (req, res) => {
  res.send("Express app running in Docker!");
});

app.listen(8080, () => {
  console.log("App listening on port 8080");
});
```

```bash
# First build (installs dependencies)
docker build -t my-express-app .
# => [1/5] FROM node:18-alpine
# => [2/5] WORKDIR /app
# => [3/5] COPY package*.json ./
# => [4/5] RUN npm ci --only=production (takes ~30 seconds)
# => [5/5] COPY . .
# => Build time: ~35 seconds

# Modify app.js and rebuild
echo "// Updated" >> app.js
docker build -t my-express-app .
# => [1/5] FROM node:18-alpine (cached)
# => [2/5] WORKDIR /app (cached)
# => [3/5] COPY package*.json ./ (cached)
# => [4/5] RUN npm ci --only=production (cached - package files unchanged!)
# => [5/5] COPY . . (rebuilds - source changed)
# => Build time: ~2 seconds (dependency layer cached!)
```

**Key Takeaway**: Copy dependency manifests before source code to leverage Docker's layer caching. This dramatically speeds up builds when only source code changes, as dependencies aren't reinstalled.

**Why It Matters**: Proper layer ordering transforms CI/CD pipeline performance by caching dependency installations that rarely change. Teams at Airbnb reduced build times from 10 minutes to under 30 seconds using this pattern, enabling dozens of deployments daily instead of a few per week. Cache efficiency directly impacts developer productivity and deployment frequency in modern DevOps workflows.

---

### Example 5: ARG for Build-Time Variables

ARG instructions define build-time variables that can be passed during image building. Unlike ENV, ARG values don't persist in the final image, making them suitable for build configuration without exposing sensitive values.

```dockerfile
# File: Dockerfile

FROM node:18-alpine
# => Base image

# Define build argument with default value
ARG NODE_ENV=production
# => Declares NODE_ENV variable available during build
# => Default value is "production" if not overridden

# Use build argument during build
RUN echo "Building for environment: $NODE_ENV"
# => Accesses ARG value during image build
# => Output shows during docker build execution

WORKDIR /app
COPY package*.json ./

# Conditional dependency installation based on build arg
RUN if [ "$NODE_ENV" = "development" ]; then \
      npm ci; \
    else \
      npm ci --only=production; \
    fi
# => Installs all dependencies for development
# => Skips devDependencies for production
# => Bash conditional statement inside RUN

COPY . .

# Convert ARG to ENV for runtime access
ENV NODE_ENV=$NODE_ENV
# => Makes NODE_ENV available at runtime
# => ARG alone would not be available when container runs

EXPOSE 8080
CMD ["node", "app.js"]
```

```bash
# Build with default ARG value (production)
docker build -t my-app:prod .
# => Uses ARG NODE_ENV=production (default)
# => Output: Building for environment: production
# => Installs only production dependencies

# Build with custom ARG value (development)
docker build --build-arg NODE_ENV=development -t my-app:dev .
# => Overrides NODE_ENV to "development"
# => Output: Building for environment: development
# => Installs all dependencies including devDependencies

# Inspect environment variables in running container
docker run --rm my-app:prod printenv NODE_ENV
# => Output: production (ENV persists at runtime)
```

**Key Takeaway**: Use ARG for build-time configuration that can vary between builds. Convert ARG to ENV if the value needs to be available at runtime. ARG values don't persist in the final image, improving security.

**Why It Matters**: Build arguments enable single Dockerfiles to generate multiple image variants (development, staging, production) without duplication, reducing maintenance burden and configuration drift. ARG's security advantage—values don't persist in layers—prevents accidental credential leakage that could expose sensitive data to anyone inspecting image history. This separation of build-time and runtime configuration is critical for secure CI/CD pipelines.

---

### Example 6: ENV for Runtime Variables

ENV instructions set environment variables that persist in the container at runtime. These variables configure application behavior and are visible in running containers.

**Environment Variable Lifecycle:**

```mermaid
graph TD
    A["Dockerfile ENV"] --> B["Build Time"]
    B --> C["Image Layer"]
    C --> D["Container Runtime"]
    D --> E["Application Reads ENV"]

    B -.->|Available during RUN| F["npm ci uses NODE_ENV"]
    D -.->|Available in container| G["process.env.PORT"]

    style A fill:#0173B2,color:#fff
    style C fill:#DE8F05,color:#000
    style E fill:#029E73,color:#fff
```

```dockerfile
# File: Dockerfile

FROM node:18-alpine
# => Base image

WORKDIR /app

# Set environment variables (persist at runtime)
ENV NODE_ENV=production
# => Sets NODE_ENV for all RUN, CMD, and ENTRYPOINT instructions
# => Available in running container

ENV PORT=3000
# => Application port configuration

ENV LOG_LEVEL=info
# => Application logging configuration

COPY package*.json ./
RUN npm ci --only=production
# => npm respects NODE_ENV variable during installation

COPY . .

EXPOSE $PORT
# => Uses PORT variable value (expands to 3000)

CMD ["node", "server.js"]
# => Application reads ENV variables at runtime
```

```javascript
// File: server.js
const http = require("http");

// Read environment variables
const port = process.env.PORT || 3000; // => 3000 from ENV PORT
const nodeEnv = process.env.NODE_ENV || "dev"; // => "production" from ENV NODE_ENV
const logLevel = process.env.LOG_LEVEL || "debug"; // => "info" from ENV LOG_LEVEL

console.log(`Environment: ${nodeEnv}`); // => Output: Environment: production
console.log(`Log Level: ${logLevel}`); // => Output: Log Level: info

const server = http.createServer((req, res) => {
  res.writeHead(200, { "Content-Type": "application/json" });
  res.end(
    JSON.stringify({
      env: nodeEnv,
      port: port,
      logLevel: logLevel,
    }),
  );
});

server.listen(port, () => {
  console.log(`Server on port ${port}`); // => Output: Server on port 3000
});
```

```bash
# Run with default ENV values
docker run -p 3000:3000 --name app1 my-app
# => Output: Environment: production
# => Output: Log Level: info
# => Output: Server on port 3000

# Override ENV at runtime with -e flag
docker run -p 3001:3001 -e PORT=3001 -e LOG_LEVEL=debug --name app2 my-app
# => Overrides PORT and LOG_LEVEL
# => Output: Environment: production (not overridden)
# => Output: Log Level: debug (overridden)
# => Output: Server on port 3001 (overridden)

# Check environment variables in running container
docker exec app1 printenv
# => Output includes: NODE_ENV=production, PORT=3000, LOG_LEVEL=info
```

**Key Takeaway**: ENV variables persist at runtime and can be overridden with `-e` flag. Use ENV for configuration that changes between environments (development, staging, production) while keeping the same image.

**Why It Matters**: Runtime environment variables enable the "build once, run anywhere" principle fundamental to modern container deployments. A single image tested in staging can be promoted to production with only configuration changes, eliminating rebuild-related deployment risks. Kubernetes and Docker Swarm rely heavily on ENV for service discovery and configuration injection across thousands of container instances.

---

### Example 7: LABEL for Image Metadata

LABEL instructions add metadata to images as key-value pairs. Labels document image purpose, version, maintainer, and other information queryable via `docker inspect`.

```dockerfile
# File: Dockerfile

FROM python:3.11-slim
# => Base image with Python runtime

# Add metadata labels
LABEL maintainer="devops@example.com"
# => Contact information for image maintainer

LABEL version="1.0.0"
# => Semantic version of this image

LABEL description="Python web application with Flask"
# => Human-readable description

LABEL org.opencontainers.image.source="https://github.com/example/repo"
# => Source repository URL (OCI standard)

LABEL org.opencontainers.image.licenses="MIT"
# => License information (OCI standard)

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt
# => --no-cache-dir reduces image size

COPY app.py .

EXPOSE 5000
CMD ["python", "app.py"]
```

```python
# File: app.py
from flask import Flask

app = Flask(__name__)

@app.route('/')
def hello():
    return 'Flask app in Docker!'

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)
```

```txt
# File: requirements.txt
Flask==2.3.0
```

```bash
# Build image with labels
docker build -t my-flask-app:1.0.0 .
# => Successfully tagged my-flask-app:1.0.0

# Inspect image labels
docker inspect my-flask-app:1.0.0 --format='{{json .Config.Labels}}' | jq
# => Output (formatted JSON):
# => {
# =>   "maintainer": "devops@example.com",
# =>   "version": "1.0.0",
# =>   "description": "Python web application with Flask",
# =>   "org.opencontainers.image.source": "https://github.com/example/repo",
# =>   "org.opencontainers.image.licenses": "MIT"
# => }

# Filter images by label
docker images --filter "label=version=1.0.0"
# => Shows only images with version=1.0.0 label

# Use labels in container orchestration
docker run -d \
  --label environment=production \
  --label team=backend \
  -p 5000:5000 \
  my-flask-app:1.0.0
# => Adds runtime labels to container (separate from image labels)
```

**Key Takeaway**: Use LABEL for documentation and automation. Follow OCI (Open Container Initiative) label standards for interoperability. Labels enable filtering, auditing, and automated tooling in production environments.

**Why It Matters**: Labels transform container images into self-documenting artifacts with embedded metadata for license compliance, security scanning, and deployment automation. Enterprise platforms like OpenShift and Rancher use labels extensively for image filtering, policy enforcement, and vulnerability tracking. Standardized OCI labels ensure your images integrate seamlessly with third-party container security and compliance tools.

---

### Example 8: Image Listing and Management

Docker provides commands to list, inspect, tag, and remove images. Understanding image management is essential for cleaning up disk space and organizing builds.

```bash
# List all images
docker images
# => REPOSITORY       TAG       IMAGE ID       CREATED         SIZE
# => my-flask-app     1.0.0     abc123def456   2 minutes ago   145MB
# => node             18-alpine 789ghi012jkl   2 weeks ago     119MB
# => python           3.11-slim 345mno678pqr   1 month ago     126MB

# List images with digests (immutable identifier)
docker images --digests
# => Shows SHA256 digest after tag column
# => Digest uniquely identifies image content

# Filter images by name
docker images my-flask-app
# => Shows only my-flask-app images

# Filter dangling images (untagged intermediate layers)
docker images --filter "dangling=true"
# => Lists images not referenced by any tag
# => Often left over from rebuilds

# Show image history (layer details)
docker history my-flask-app:1.0.0
# => IMAGE          CREATED        CREATED BY                                      SIZE
# => abc123def456   5 minutes ago  CMD ["python" "app.py"]                         0B
# => <missing>      5 minutes ago  EXPOSE 5000                                     0B
# => <missing>      5 minutes ago  COPY app.py . # buildkit                        1.2kB
# => <missing>      5 minutes ago  RUN pip install --no-cache-dir -r requirements  15MB
# => Shows each layer with size and command

# Tag image with additional name
docker tag my-flask-app:1.0.0 my-flask-app:latest
# => Creates new tag pointing to same image
# => No disk space used (same image ID)

# Remove specific image tag
docker rmi my-flask-app:1.0.0
# => Untagged: my-flask-app:1.0.0
# => Image still exists as my-flask-app:latest

# Remove image and all tags
docker rmi $(docker images my-flask-app -q)
# => Removes all tags for my-flask-app
# => -q returns only image IDs

# Prune dangling images (cleanup)
docker image prune
# => Removes all dangling images
# => Frees disk space

# Prune all unused images
docker image prune -a
# => Removes all images not used by containers
# => Use with caution - removes ALL unused images
```

**Key Takeaway**: Regularly prune unused images to free disk space. Use tags to organize image versions, and inspect image history to understand layer sizes and optimize Dockerfiles.

**Why It Matters**: Image bloat silently consumes disk space on build servers and production nodes, causing deployments to fail when storage runs out. Automated pruning in CI/CD pipelines prevents disk exhaustion that can halt entire deployment infrastructures. Understanding layer history helps identify wasteful Dockerfile instructions that add hundreds of megabytes unnecessarily, directly reducing cloud storage and transfer costs.

---

### Example 9: Container Lifecycle Management

Understanding container states and lifecycle commands is fundamental for debugging and managing applications. Containers transition between created, running, paused, stopped, and removed states.

```mermaid
%% Container lifecycle state transitions
stateDiagram-v2
    [*] --> Created: docker create
    Created --> Running: docker start
    Running --> Paused: docker pause
    Paused --> Running: docker unpause
    Running --> Stopped: docker stop
    Stopped --> Running: docker start
    Created --> [*]: docker rm
    Stopped --> [*]: docker rm
    Running --> [*]: docker rm -f

    style Created fill:#0173B2,color:#fff
    style Running fill:#029E73,color:#fff
    style Paused fill:#DE8F05,color:#000
    style Stopped fill:#CC78BC,color:#000
```

```bash
# Create container without starting it
docker create --name my-nginx -p 8080:80 nginx:alpine
# => Creates container in "Created" state
# => Container ID: def456ghi789
# => Port mapping configured but not active yet

# List all containers (including created/stopped)
docker ps -a
# => CONTAINER ID   IMAGE         STATUS    PORTS                  NAMES
# => def456ghi789   nginx:alpine  Created   0.0.0.0:8080->80/tcp   my-nginx

# Start created container
docker start my-nginx
# => Transitions container to "Running" state
# => Port 8080 now accessible

# Check running containers only
docker ps
# => CONTAINER ID   IMAGE         STATUS        PORTS                  NAMES
# => def456ghi789   nginx:alpine  Up 5 seconds  0.0.0.0:8080->80/tcp   my-nginx

# Pause running container (freezes all processes)
docker pause my-nginx
# => Container state: Paused
# => Processes suspended but container still exists
# => Port still mapped but requests hang

# Unpause container
docker unpause my-nginx
# => Resumes all processes
# => Container returns to Running state

# Stop container gracefully (SIGTERM, then SIGKILL after 10s)
docker stop my-nginx
# => Sends SIGTERM to main process
# => Waits up to 10 seconds for graceful shutdown
# => Sends SIGKILL if still running after timeout
# => Container state: Exited

# Restart stopped container
docker restart my-nginx
# => Equivalent to docker stop + docker start
# => Container returns to Running state

# Stop container with custom timeout
docker stop -t 30 my-nginx
# => Waits 30 seconds before SIGKILL (instead of default 10)

# Kill container immediately (SIGKILL, no graceful shutdown)
docker kill my-nginx
# => Sends SIGKILL immediately
# => Use when container doesn't respond to stop

# Remove stopped container
docker rm my-nginx
# => Deletes container completely
# => Cannot be started again

# Remove running container (force)
docker rm -f my-nginx
# => Stops (SIGKILL) and removes in one command
# => Use with caution - no graceful shutdown
```

**Key Takeaway**: Use `docker stop` for graceful shutdown (allows cleanup), and `docker kill` only when necessary. Always remove stopped containers to free disk space and avoid name conflicts.

**Why It Matters**: Graceful shutdown via SIGTERM allows applications to flush buffers, close database connections, and finish in-flight requests, preventing data loss and corruption. Platforms like Kubernetes rely on proper shutdown handling for zero-downtime deployments during rolling updates. Immediate SIGKILL termination can corrupt databases or lose queued messages, causing production outages.

---

### Example 10: Container Logs and Inspection

Container logs capture stdout and stderr from the main process. Inspection provides detailed container configuration, networking, and state information.

```bash
# Run container that generates logs
docker run -d --name web-app -p 8080:80 nginx:alpine
# => Container ID: abc123def456
# => Runs in background (-d detached mode)

# View logs (stdout and stderr)
docker logs web-app
# => /docker-entrypoint.sh: Launching /docker-entrypoint.d/10-listen-on-ipv6-by-default.sh
# => /docker-entrypoint.sh: Configuration complete; ready for start up
# => Shows all logs since container start

# Follow logs in real-time (like tail -f)
docker logs -f web-app
# => Streams new log lines as they appear
# => Press Ctrl+C to stop following

# Show timestamps with logs
docker logs -t web-app
# => 2025-12-29T10:30:15.123456789Z /docker-entrypoint.sh: Launching...
# => 2025-12-29T10:30:15.234567890Z Configuration complete
# => Each line prefixed with RFC3339 timestamp

# Show only last N lines
docker logs --tail 20 web-app
# => Shows last 20 log lines
# => Useful for large log volumes

# Show logs since specific time
docker logs --since 10m web-app
# => Shows logs from last 10 minutes
# => Accepts format: 10s, 5m, 2h, 2023-12-29T10:00:00

# Inspect container details (JSON output)
docker inspect web-app
# => Returns JSON with full container configuration
# => Includes: networking, volumes, environment, state, etc.

# Extract specific information with format
docker inspect --format='{{.State.Status}}' web-app
# => Output: running

docker inspect --format='{{.NetworkSettings.IPAddress}}' web-app
# => Output: 172.17.0.2 (container IP on bridge network)

docker inspect --format='{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' web-app
# => Output: 172.17.0.2
# => More reliable for multiple networks

docker inspect --format='{{json .Config.Env}}' web-app | jq
# => Shows environment variables as formatted JSON

# View container stats (CPU, memory, network, I/O)
docker stats web-app
# => CONTAINER ID   NAME      CPU %   MEM USAGE / LIMIT   MEM %   NET I/O       BLOCK I/O
# => abc123def456   web-app   0.01%   5.5MiB / 7.77GiB    0.07%   1.2kB / 0B    0B / 0B
# => Updates every second (live monitoring)

# View stats once (no streaming)
docker stats --no-stream web-app
# => Shows current stats snapshot
# => Exits immediately
```

**Key Takeaway**: Use `docker logs` for troubleshooting application issues. Use `docker inspect` to understand container configuration and networking. Use `docker stats` to monitor resource usage in real-time.

**Why It Matters**: Real-time log access via `docker logs` eliminates the need to SSH into production servers, reducing security risks and speeding up incident response. Container inspection reveals networking and configuration issues instantly, cutting debugging time from hours to minutes. Resource monitoring with `docker stats` identifies memory leaks and CPU bottlenecks before they cause outages in production systems.

---

### Example 11: Executing Commands in Running Containers

The `docker exec` command runs processes inside running containers without stopping them. Essential for debugging, running maintenance tasks, and interactive exploration.

```bash
# Start a web container
docker run -d --name web-server -p 8080:80 nginx:alpine
# => Container running in background

# Execute single command in container
docker exec web-server ls -la /usr/share/nginx/html
# => total 8
# => drwxr-xr-x 2 root root 4096 Dec 29 10:00 .
# => drwxr-xr-x 3 root root 4096 Dec 29 10:00 ..
# => -rw-r--r-- 1 root root  615 Nov 10 12:00 index.html
# => Lists files in nginx web root

# Execute command with output redirection
docker exec web-server sh -c 'echo "Hello Docker" > /tmp/greeting.txt'
# => Creates file inside container
# => Returns no output (stdout redirected to file)

docker exec web-server cat /tmp/greeting.txt
# => Output: Hello Docker

# Start interactive shell in running container
docker exec -it web-server sh
# => Opens interactive shell inside container
# => Prompt changes to: / #

# Inside container: Install debugging tools
apk add curl
# => Installs curl inside container
# => Changes persist only while container runs

curl http://localhost
# => Tests nginx from inside container
# => Output: <!DOCTYPE html>...

exit
# => Returns to host shell
# => Container keeps running (exec doesn't affect main process)

# Execute command as specific user
docker exec -u nginx web-server whoami
# => Output: nginx
# => Runs command as nginx user instead of root

# Execute command with environment variables
docker exec -e DEBUG=true web-server sh -c 'echo $DEBUG'
# => Output: true
# => Temporary environment variable for this exec only

# Execute command in specific working directory
docker exec -w /etc/nginx web-server pwd
# => Output: /etc/nginx
# => Sets working directory for command

# Run background process inside container
docker exec -d web-server sh -c 'while true; do date >> /tmp/heartbeat.log; sleep 5; done'
# => Starts background process inside container
# => Returns immediately (detached)

# Check background process output
docker exec web-server tail -5 /tmp/heartbeat.log
# => Sun Dec 29 10:30:15 UTC 2025
# => Sun Dec 29 10:30:20 UTC 2025
# => Shows last 5 entries
```

**Key Takeaway**: Use `docker exec -it` for interactive debugging and `docker exec` for automation scripts. Changes made via exec are temporary and lost when container stops unless they modify mounted volumes.

**Why It Matters**: Runtime exec commands provide emergency access to production containers without rebuilding images or redeploying services, critical for time-sensitive debugging during outages. Temporary changes ensure troubleshooting doesn't permanently modify production containers, maintaining infrastructure immutability. This capability is essential for investigating production issues without disrupting running services.

---

### Example 12: Container Port Mapping

Port mapping exposes container services to the host network. Docker supports TCP/UDP protocols and can map multiple ports simultaneously.

```mermaid
%% Port mapping flow
graph TD
    A["Host<br/>localhost:8080"] --> B["Docker Bridge<br/>172.17.0.1"]
    B --> C["Container<br/>172.17.0.2:80"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
```

```bash
# Run container with single port mapping
docker run -d --name web1 -p 8080:80 nginx:alpine
# => Maps host port 8080 to container port 80 (TCP default)
# => Accessible at http://localhost:8080

# Run with specific host IP
docker run -d --name web2 -p 127.0.0.1:8081:80 nginx:alpine
# => Only accessible from localhost (not external network)
# => Accessible at http://127.0.0.1:8081

# Run with random host port
docker run -d --name web3 -p 80 nginx:alpine
# => Docker assigns random available host port
# => Check with: docker port web3

docker port web3
# => 80/tcp -> 0.0.0.0:32768
# => Host port 32768 mapped to container port 80

# Map multiple ports
docker run -d --name app \
  -p 8080:80 \
  -p 8443:443 \
  nginx:alpine
# => HTTP on 8080, HTTPS on 8443

# Map UDP port
docker run -d --name dns-server -p 53:53/udp my-dns-image
# => Maps UDP port 53
# => Requires /udp suffix (default is /tcp)

# Map both TCP and UDP for same port
docker run -d --name multi-protocol \
  -p 8080:8080/tcp \
  -p 8080:8080/udp \
  my-app
# => Same port number, different protocols

# Expose all EXPOSE'd ports with random host ports
docker run -d --name auto-ports -P nginx:alpine
# => -P (capital P) publishes all EXPOSE'd ports
# => Docker assigns random host ports

docker port auto-ports
# => 80/tcp -> 0.0.0.0:32769
# => Shows automatically assigned port

# Check port mapping for running container
docker inspect --format='{{range $p, $conf := .NetworkSettings.Ports}}{{$p}} -> {{(index $conf 0).HostPort}}{{println}}{{end}}' web1
# => 80/tcp -> 8080

# Test connection from host
curl http://localhost:8080
# => Output: <!DOCTYPE html>... (nginx welcome page)

# View network connections
netstat -tlnp | grep 8080
# => tcp6    0    0 :::8080    :::*    LISTEN    1234/docker-proxy
# => Shows docker-proxy listening on port 8080
```

**Key Takeaway**: Use `-p HOST:CONTAINER` for explicit port mapping and `-P` for automatic mapping. Bind to `127.0.0.1` to restrict access to localhost only. Remember to specify `/udp` for UDP ports.

**Why It Matters**: Port mapping enables running multiple services on a single host without port conflicts, dramatically increasing server utilization compared to VMs where each service needs dedicated ports or separate VMs. Localhost binding provides security by preventing external access to development containers or internal services. This flexibility allows hosting dozens of microservices on commodity hardware instead of requiring separate servers for each service.

---

### Example 13: Named Volumes for Data Persistence

Named volumes provide persistent storage managed by Docker. Data survives container removal and can be shared between containers.

```mermaid
%% Volume architecture
graph TD
    A["Container 1<br/>/app/data"] --> B["Named Volume<br/>my-data"]
    C["Container 2<br/>/backup/data"] --> B
    B --> D["Host Filesystem<br/>/var/lib/docker/volumes/my-data"]

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#0173B2,color:#fff
    style D fill:#029E73,color:#fff
```

```bash
# Create named volume explicitly
docker volume create my-data
# => my-data
# => Creates volume in /var/lib/docker/volumes/

# List volumes
docker volume ls
# => DRIVER    VOLUME NAME
# => local     my-data

# Inspect volume details
docker volume inspect my-data
# => [
# =>   {
# =>     "CreatedAt": "2025-12-29T10:30:00Z",
# =>     "Driver": "local",
# =>     "Mountpoint": "/var/lib/docker/volumes/my-data/_data",
# =>     "Name": "my-data",
# =>     "Scope": "local"
# =>   }
# => ]

# Run container with named volume
docker run -d --name db \
  -v my-data:/var/lib/postgresql/data \
  -e POSTGRES_PASSWORD=secret \
  postgres:15-alpine
# => Mounts my-data volume to /var/lib/postgresql/data
# => Database files persist in volume

# Write data to database (survives container removal)
docker exec -it db psql -U postgres -c "CREATE DATABASE testdb;"
# => CREATE DATABASE
# => Data written to volume

# Stop and remove container
docker stop db
docker rm db
# => Container removed but volume persists

# Create new container with same volume
docker run -d --name db2 \
  -v my-data:/var/lib/postgresql/data \
  -e POSTGRES_PASSWORD=secret \
  postgres:15-alpine
# => Uses existing volume with data intact

# Verify data persisted
docker exec -it db2 psql -U postgres -c "\l"
# => Output includes: testdb | postgres | UTF8 | ...
# => Database from previous container still exists!

# Create volume with auto-creation (no explicit volume create)
docker run -d --name app \
  -v app-logs:/var/log/app \
  my-app-image
# => Docker creates app-logs volume automatically if it doesn't exist

# Share volume between containers
docker run -d --name writer \
  -v shared:/data \
  alpine sh -c 'while true; do date >> /data/log.txt; sleep 5; done'
# => Writes timestamps to /data/log.txt in volume

docker run -d --name reader \
  -v shared:/input:ro \
  alpine sh -c 'while true; do tail -1 /input/log.txt; sleep 5; done'
# => Reads from same volume (read-only mount with :ro)

# Check reader output
docker logs -f reader
# => Sun Dec 29 10:35:00 UTC 2025
# => Sun Dec 29 10:35:05 UTC 2025
# => Shows timestamps written by writer container

# Remove volume (only when no containers use it)
docker stop writer reader
docker rm writer reader
docker volume rm shared
# => Deletes volume and all data
# => Cannot remove volume while in use
```

**Key Takeaway**: Use named volumes for database persistence, application state, and cross-container data sharing. Volumes survive container removal and are managed by Docker, making them more portable than bind mounts.

**Why It Matters**: Named volumes solve the critical problem of data persistence in ephemeral containers, enabling stateful applications like databases to survive container restarts and upgrades. Docker-managed volumes are portable across different host filesystems (Linux, Windows, macOS), unlike bind mounts tied to specific host paths. Production databases at scale rely on volumes with backup strategies to prevent catastrophic data loss during infrastructure failures.

---

### Example 14: Bind Mounts for Development

Bind mounts map host directories into containers, enabling live code reloading during development. Changes on the host immediately reflect in the container.

**Bind Mount Mapping:**

```mermaid
graph TD
    A["Host Directory<br/>~/myapp"] <--> B["Container Path<br/>/app"]
    A --> C["server.js<br/>message.txt<br/>package.json"]
    B --> D["Live Updates<br/>File changes sync instantly"]

    style A fill:#0173B2,color:#fff
    style B fill:#029E73,color:#fff
    style D fill:#DE8F05,color:#000
```

```bash
# Prepare application files on host
mkdir -p ~/myapp
cd ~/myapp

cat > server.js << 'EOF'
const http = require('http');
const fs = require('fs');

const server = http.createServer((req, res) => {
  // Read message from file (can be updated on host)
  const message = fs.readFileSync('/app/message.txt', 'utf8');
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end(message);
});

server.listen(3000, () => {
  console.log('Server listening on port 3000');
});
EOF
# => Creates server.js in current directory

echo "Hello from bind mount!" > message.txt
# => Creates message.txt with initial content

cat > package.json << 'EOF'
{
  "name": "bind-mount-demo",
  "version": "1.0.0",
  "main": "server.js"
}
EOF
# => Creates package.json

# Run container with bind mount (absolute path required)
docker run -d --name dev-app \
  -v "$(pwd)":/app \
  -w /app \
  -p 3000:3000 \
  node:18-alpine \
  node server.js
# => Mounts current directory to /app in container
# => $(pwd) expands to absolute path (required for -v)
# => -w sets working directory to /app

# Test initial response
curl http://localhost:3000
# => Output: Hello from bind mount!

# Update message.txt on host (WITHOUT restarting container)
echo "Updated message!" > message.txt
# => Changes file on host filesystem

# Test updated response (no container restart needed!)
curl http://localhost:3000
# => Output: Updated message!
# => Container reads updated file immediately

# Bind mount with read-only access
docker run -d --name readonly-app \
  -v "$(pwd)":/app:ro \
  -w /app \
  -p 3001:3000 \
  node:18-alpine \
  node server.js
# => :ro makes bind mount read-only
# => Container cannot modify host files

# Try to write from container (fails with read-only mount)
docker exec readonly-app sh -c 'echo "test" > /app/test.txt'
# => sh: can't create /app/test.txt: Read-only file system
# => Write operation denied

# Bind mount with specific user permissions
docker run -d --name owned-app \
  -v "$(pwd)":/app \
  -w /app \
  -u $(id -u):$(id -g) \
  -p 3002:3000 \
  node:18-alpine \
  node server.js
# => Runs container as current user (not root)
# => Files created in container have host user ownership

# Clean up
docker stop dev-app readonly-app owned-app
docker rm dev-app readonly-app owned-app
```

**Key Takeaway**: Use bind mounts for development workflows requiring live code updates. Always use absolute paths with `-v` flag. Add `:ro` suffix for read-only access. For production, prefer named volumes over bind mounts for better portability and security.

**Why It Matters**: Bind mounts revolutionize development workflows by eliminating container rebuilds after code changes, reducing iteration cycles from minutes to milliseconds. Live code reloading with frameworks like React or Node.js provides instant feedback, dramatically improving developer productivity. However, bind mounts expose host filesystem paths, creating security risks in production where named volumes provide better isolation and abstraction from host infrastructure.

---

### Example 15: tmpfs Mounts for Temporary Data

tmpfs mounts store data in host memory (RAM) without writing to disk. Ideal for sensitive data, temporary caches, or high-performance temporary storage.

```bash
# Run container with tmpfs mount
docker run -d --name temp-app \
  --tmpfs /tmp:rw,size=100m,mode=1777 \
  -p 3000:3000 \
  node:18-alpine \
  sh -c 'while true; do echo "Data: $(date)" > /tmp/cache.txt; sleep 2; done'
# => Creates tmpfs mount at /tmp with 100MB limit
# => rw: read-write access
# => size=100m: maximum size 100 megabytes
# => mode=1777: permissions (sticky bit + rwxrwxrwx)

# Verify tmpfs mount inside container
docker exec temp-app df -h /tmp
# => Filesystem      Size  Used  Avail  Use%  Mounted on
# => tmpfs           100M  4.0K  100M    1%   /tmp
# => Shows tmpfs mount with 100MB size limit

# Write data to tmpfs
docker exec temp-app sh -c 'dd if=/dev/zero of=/tmp/testfile bs=1M count=10'
# => 10+0 records in
# => 10+0 records out
# => Writes 10MB to tmpfs (stored in RAM, not disk)

docker exec temp-app du -sh /tmp/testfile
# => 10.0M   /tmp/testfile

# Verify data exists in memory
docker exec temp-app cat /tmp/cache.txt
# => Data: Sun Dec 29 10:40:15 UTC 2025
# => Data stored in RAM

# Stop and restart container (tmpfs data is LOST)
docker stop temp-app
docker start temp-app

# Check if data persists (it doesn't!)
docker exec temp-app ls /tmp/
# => Empty output or only new files
# => All previous tmpfs data lost on container stop

# Use case: Sensitive credential caching
docker run -d --name secure-app \
  --tmpfs /run/secrets:rw,size=10m,mode=0700 \
  alpine sh -c 'echo "secret-token" > /run/secrets/token; while true; do sleep 3600; done'
# => Stores secrets in memory only
# => mode=0700: only owner can read/write (more restrictive)

docker exec secure-app cat /run/secrets/token
# => Output: secret-token
# => Data never written to disk

# Multiple tmpfs mounts
docker run -d --name multi-tmp \
  --tmpfs /tmp:size=100m \
  --tmpfs /var/cache:size=50m \
  --tmpfs /var/log:size=50m \
  alpine sleep 3600
# => Multiple tmpfs mounts in single container
# => Different size limits for different purposes

# Verify all tmpfs mounts
docker exec multi-tmp df -h
# => tmpfs     100M     0   100M   0%  /tmp
# => tmpfs      50M     0    50M   0%  /var/cache
# => tmpfs      50M     0    50M   0%  /var/log
```

**Key Takeaway**: Use tmpfs for temporary data that doesn't need persistence (caches, temporary processing, secrets). Data is fast (RAM-based) but lost when container stops. Never use tmpfs for data that must survive container restarts.

**Why It Matters**: RAM-based tmpfs storage eliminates disk I/O bottlenecks for temporary data, providing orders of magnitude faster performance than disk volumes for workloads like compilation artifacts or session caches. Security-sensitive applications use tmpfs for credentials that should never touch disk, preventing forensic recovery of secrets. Financial services companies use tmpfs for processing sensitive transaction data that must be securely erased after use.

---

### Example 16: Bridge Network Basics

Docker's default bridge network enables container-to-container communication. Containers on the same bridge network can reach each other by container name (automatic DNS).

```mermaid
%% Bridge network topology
graph TD
    A["Host<br/>eth0: 192.168.1.10"] --> B["Docker Bridge<br/>docker0: 172.17.0.1"]
    B --> C["Container 1<br/>web: 172.17.0.2"]
    B --> D["Container 2<br/>api: 172.17.0.3"]
    B --> E["Container 3<br/>db: 172.17.0.4"]
    C -.->|"DNS: api"| D
    D -.->|"DNS: db"| E

    style A fill:#0173B2,color:#fff
    style B fill:#DE8F05,color:#000
    style C fill:#029E73,color:#fff
    style D fill:#029E73,color:#fff
    style E fill:#029E73,color:#fff
```

```bash
# Create custom bridge network
docker network create my-bridge
# => Network ID: abc123def456789...
# => Creates isolated bridge network

# List networks
docker network ls
# => NETWORK ID     NAME         DRIVER    SCOPE
# => abc123def456   my-bridge    bridge    local
# => def456ghi789   bridge       bridge    local (default)
# => ghi789jkl012   host         host      local
# => jkl012mno345   none         null      local

# Inspect network details
docker network inspect my-bridge
# => [
# =>   {
# =>     "Name": "my-bridge",
# =>     "Driver": "bridge",
# =>     "Subnet": "172.18.0.0/16",
# =>     "Gateway": "172.18.0.1",
# =>     "Containers": {}
# =>   }
# => ]

# Run database on custom bridge
docker run -d --name postgres-db \
  --network my-bridge \
  -e POSTGRES_PASSWORD=secret \
  postgres:15-alpine
# => Container gets IP on my-bridge network
# => Accessible by name "postgres-db" within network

# Run API server on same network
docker run -d --name api-server \
  --network my-bridge \
  -p 8080:8080 \
  -e DB_HOST=postgres-db \
  -e DB_PASSWORD=secret \
  my-api-image
# => Can connect to database using hostname "postgres-db"
# => Docker's built-in DNS resolves container names

# Test DNS resolution from api-server
docker exec api-server nslookup postgres-db
# => Server:    127.0.0.11 (Docker's embedded DNS server)
# => Name:      postgres-db
# => Address:   172.18.0.2 (container IP on my-bridge)

# Test connectivity between containers
docker exec api-server ping -c 2 postgres-db
# => PING postgres-db (172.18.0.2): 56 data bytes
# => 64 bytes from 172.18.0.2: seq=0 ttl=64 time=0.123 ms
# => 64 bytes from 172.18.0.2: seq=1 ttl=64 time=0.098 ms
# => Containers can ping each other by name

# Connect existing container to additional network
docker network connect my-bridge some-existing-container
# => Container now connected to both original and my-bridge networks
# => Can communicate with containers on both networks

# Disconnect container from network
docker network disconnect my-bridge some-existing-container
# => Removes container from my-bridge network

# Run container on default bridge (no automatic DNS)
docker run -d --name old-style-db postgres:15-alpine
# => Runs on default bridge network
# => Other containers on default bridge CANNOT use name-based DNS

# Containers on default bridge need IP addresses or links (legacy)
docker inspect old-style-db --format='{{.NetworkSettings.IPAddress}}'
# => 172.17.0.2 (must use IP address, not name)

# Remove network (only when no containers attached)
docker network rm my-bridge
# => Error: network my-bridge has active endpoints
# => Must stop/remove containers first
```

**Key Takeaway**: Always create custom bridge networks for multi-container applications. Custom bridges provide automatic DNS resolution by container name, better isolation, and easier configuration than the default bridge.

**Why It Matters**: Custom bridge networks enable service discovery without hardcoded IP addresses, allowing containers to communicate using stable DNS names even as underlying IPs change during scaling or restarts. This is foundational for microservices architectures where dozens of services must discover each other dynamically. Compared to the default bridge requiring legacy container linking, custom networks provide modern, maintainable service communication patterns.

---

### Example 17: Container to Container Communication

Containers on the same network communicate using container names as hostnames. Docker's embedded DNS server resolves names to container IP addresses automatically.

```bash
# Create application network
docker network create app-network
# => Network for frontend, backend, database

# Start PostgreSQL database
docker run -d --name database \
  --network app-network \
  -e POSTGRES_USER=appuser \
  -e POSTGRES_PASSWORD=apppass \
  -e POSTGRES_DB=appdb \
  postgres:15-alpine
# => Database accessible at hostname "database"

# Start Node.js backend API
cat > backend.js << 'EOF'
const express = require('express');
const { Pool } = require('pg');

const pool = new Pool({
  host: 'database',              // Container name as hostname
  user: 'appuser',
  password: 'apppass',
  database: 'appdb',
  port: 5432,
});

const app = express();

app.get('/api/status', async (req, res) => {
  try {
    const result = await pool.query('SELECT NOW()');
    res.json({
      status: 'ok',
      database_time: result.rows[0].now,
    });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.listen(3000, () => {
  console.log('Backend API listening on port 3000');
});
EOF
# => Backend connects to database using "database" hostname

docker build -t my-backend -f- . << 'EOF'
FROM node:18-alpine
WORKDIR /app
RUN npm install express pg
COPY backend.js .
CMD ["node", "backend.js"]
EOF
# => Builds backend image

docker run -d --name backend \
  --network app-network \
  my-backend
# => Backend can resolve "database" hostname
# => Not exposing port yet (internal only)

# Test backend to database connection
docker exec backend wget -qO- http://localhost:3000/api/status
# => {"status":"ok","database_time":"2025-12-29T10:45:00.123Z"}
# => Backend successfully connects to database

# Start Nginx frontend
cat > nginx.conf << 'EOF'
server {
    listen 80;
    location /api/ {
        proxy_pass http://backend:3000/api/;  # Container name as hostname
    }
    location / {
        return 200 'Frontend served by Nginx\n';
        add_header Content-Type text/plain;
    }
}
EOF

docker run -d --name frontend \
  --network app-network \
  -p 8080:80 \
  -v "$(pwd)/nginx.conf:/etc/nginx/conf.d/default.conf:ro" \
  nginx:alpine
# => Frontend proxies /api/ requests to backend:3000
# => Uses container name "backend" in nginx config

# Test full stack: frontend -> backend -> database
curl http://localhost:8080/api/status
# => {"status":"ok","database_time":"2025-12-29T10:45:00.123Z"}
# => Request flows: host -> frontend -> backend -> database

# Verify DNS resolution inside containers
docker exec frontend nslookup backend
# => Server:    127.0.0.11 (Docker DNS)
# => Name:      backend
# => Address:   172.19.0.3

docker exec backend nslookup database
# => Server:    127.0.0.11
# => Name:      database
# => Address:   172.19.0.2

# Check network connectivity between containers
docker exec frontend ping -c 1 backend
# => 64 bytes from 172.19.0.3: seq=0 ttl=64 time=0.123 ms

docker exec backend ping -c 1 database
# => 64 bytes from 172.19.0.2: seq=0 ttl=64 time=0.089 ms

# Inspect network to see all connected containers
docker network inspect app-network --format='{{range .Containers}}{{.Name}}: {{.IPv4Address}}{{"\n"}}{{end}}'
# => database: 172.19.0.2/16
# => backend: 172.19.0.3/16
# => frontend: 172.19.0.4/16
```

**Key Takeaway**: Use container names as hostnames for inter-container communication. Docker's embedded DNS (127.0.0.11) automatically resolves names to IP addresses on custom bridge networks, enabling service discovery without hardcoded IPs.

**Why It Matters**: Built-in DNS service discovery eliminates the need for external service discovery tools like Consul or Etcd for simple multi-container applications, reducing infrastructure complexity. Container name-based communication ensures configuration remains valid across deployments even when container IPs change, critical for reliable microservices communication. This pattern scales from local development to production Kubernetes clusters using the same DNS principles.

---

### Example 18: Environment Variables from File

Environment variable files (`.env` files) provide configuration without hardcoding values in Dockerfiles or command lines. Essential for credentials and environment-specific settings.

```bash
# Create environment variable file
cat > database.env << 'EOF'
POSTGRES_USER=myuser
POSTGRES_PASSWORD=mypassword
POSTGRES_DB=mydb
POSTGRES_HOST_AUTH_METHOD=scram-sha-256
EOF
# => Stores database configuration in file

cat > app.env << 'EOF'
NODE_ENV=production
LOG_LEVEL=info
API_KEY=secret-key-12345
DATABASE_URL=postgresql://myuser:mypassword@db:5432/mydb
EOF
# => Stores application configuration in file

# Start database with env file
docker run -d --name db \
  --env-file database.env \
  --network app-net \
  postgres:15-alpine
# => Loads all variables from database.env
# => Equivalent to multiple -e flags

# Verify environment variables loaded
docker exec db printenv | grep POSTGRES
# => POSTGRES_USER=myuser
# => POSTGRES_PASSWORD=mypassword
# => POSTGRES_DB=mydb
# => POSTGRES_HOST_AUTH_METHOD=scram-sha-256

# Start application with env file
docker run -d --name app \
  --env-file app.env \
  --network app-net \
  -p 3000:3000 \
  my-app-image
# => Loads all variables from app.env

# Override specific variables while using env file
docker run -d --name app-dev \
  --env-file app.env \
  -e NODE_ENV=development \
  -e LOG_LEVEL=debug \
  --network app-net \
  -p 3001:3000 \
  my-app-image
# => Loads app.env, then overrides NODE_ENV and LOG_LEVEL
# => -e flag takes precedence over --env-file

# Use multiple env files
docker run -d --name app-multi \
  --env-file common.env \
  --env-file production.env \
  my-app-image
# => Loads variables from both files
# => Later files override earlier ones if keys conflict

# Template env file for documentation
cat > .env.example << 'EOF'
# Database Configuration
POSTGRES_USER=changeme
POSTGRES_PASSWORD=changeme
POSTGRES_DB=mydb

# Application Configuration
NODE_ENV=production
LOG_LEVEL=info
API_KEY=your-api-key-here

# Feature Flags
ENABLE_CACHE=true
ENABLE_METRICS=false
EOF
# => Template for developers (commit to version control)
# => Actual .env file (with secrets) goes in .gitignore

# Create .gitignore to prevent committing secrets
cat > .gitignore << 'EOF'
*.env
!.env.example
EOF
# => Prevents committing actual .env files
# => Allows committing .env.example template

# Inspect container to view environment variables
docker inspect app --format='{{range .Config.Env}}{{println .}}{{end}}'
# => NODE_ENV=production
# => LOG_LEVEL=info
# => API_KEY=secret-key-12345
# => DATABASE_URL=postgresql://myuser:mypassword@db:5432/mydb
# => Shows all environment variables (CAREFUL: exposes secrets!)
```

**Key Takeaway**: Use `--env-file` for configuration management and keep actual `.env` files out of version control. Provide `.env.example` templates for documentation. Override variables with `-e` when needed, as it takes precedence over `--env-file`.

**Why It Matters**: Environment files separate secrets from source code, preventing accidental credential commits that expose API keys and database passwords in version control history. This separation enables the same codebase to run across development, staging, and production with environment-specific configurations. Teams at GitHub use environment files extensively to manage thousands of service configurations without hardcoding credentials in application code.

---

### Example 19: Docker Compose Basics

Docker Compose defines multi-container applications in YAML files. It simplifies orchestration by managing services, networks, and volumes together.

```yaml
# File: docker-compose.yml

version: "3.8"
# => Compose file format version (3.8 is widely compatible)

services:
  # Database service
  db:
    image: postgres:15-alpine
    # => Uses official PostgreSQL 15 image
    container_name: my-postgres
    # => Custom container name (default: project_db_1)
    environment:
      POSTGRES_USER: appuser
      POSTGRES_PASSWORD: apppass
      POSTGRES_DB: appdb
      # => Environment variables passed to container
    volumes:
      - db-data:/var/lib/postgresql/data
      # => Named volume for database persistence
    networks:
      - backend
      # => Connects to backend network
    restart: unless-stopped
    # => Restart policy: always restart unless explicitly stopped

  # Backend API service
  api:
    build:
      context: ./api
      dockerfile: Dockerfile
      # => Builds image from ./api/Dockerfile
    container_name: my-api
    environment:
      DATABASE_URL: postgresql://appuser:apppass@db:5432/appdb
      # => Database connection using service name "db"
    depends_on:
      - db
      # => Ensures db starts before api
      # => NOTE: Does NOT wait for db to be ready, only started
    networks:
      - backend
      - frontend
      # => Connects to both networks for db and web communication
    restart: unless-stopped

  # Web frontend service
  web:
    image: nginx:alpine
    # => Uses official Nginx alpine image
    container_name: my-web
    ports:
      - "8080:80"
      # => Maps host port 8080 to container port 80
    volumes:
      - ./web/nginx.conf:/etc/nginx/conf.d/default.conf:ro
      # => Mounts nginx config as read-only
      - ./web/html:/usr/share/nginx/html:ro
      # => Mounts static files as read-only
    depends_on:
      - api
      # => Ensures api starts before web
    networks:
      - frontend
      # => Connects only to frontend network (no direct db access)
    restart: unless-stopped

networks:
  frontend:
    driver: bridge
    # => Custom bridge network for frontend services
  backend:
    driver: bridge
    # => Custom bridge network for backend services
    # => Separates frontend and backend layers

volumes:
  db-data:
    # => Named volume managed by Docker
    # => Persists database data across container recreations
```

```bash
# Start all services defined in docker-compose.yml
docker compose up -d
# => Creates networks: project_frontend, project_backend
# => Creates volume: project_db-data
# => Starts containers: my-postgres, my-api, my-web
# => -d runs in detached mode (background)

# View running services
docker compose ps
# => NAME          IMAGE              STATUS        PORTS
# => my-postgres   postgres:15-alpine Up 30 seconds
# => my-api        project_api        Up 25 seconds
# => my-web        nginx:alpine       Up 20 seconds 0.0.0.0:8080->80/tcp

# View logs from all services
docker compose logs
# => Shows combined logs from all services
# => Color-coded by service

# View logs from specific service
docker compose logs -f api
# => Follows logs from api service only
# => -f streams logs in real-time

# Stop all services (keeps containers)
docker compose stop
# => Stops all containers without removing them

# Start stopped services
docker compose start
# => Starts previously stopped containers

# Restart specific service
docker compose restart api
# => Stops and starts api service

# Stop and remove all services, networks (keeps volumes)
docker compose down
# => Removes containers and networks
# => Preserves named volumes (db-data persists)

# Remove everything including volumes (DESTRUCTIVE)
docker compose down -v
# => Removes containers, networks, AND volumes
# => Database data is deleted!

# Rebuild images and restart services
docker compose up -d --build
# => Rebuilds images before starting
# => Useful after code changes

# Scale a service (create multiple instances)
docker compose up -d --scale api=3
# => Creates 3 instances of api service
# => Requires removing container_name (generates unique names)

# Execute command in running service
docker compose exec api sh
# => Opens shell in api service container
# => Similar to docker exec

# View service configuration
docker compose config
# => Shows resolved configuration (interpolated variables, defaults)
```

**Key Takeaway**: Docker Compose simplifies multi-container applications with declarative YAML configuration. Use `depends_on` for startup order, separate networks for layer isolation, and named volumes for data persistence. Always use `-d` flag for production deployments.

**Why It Matters**: Docker Compose transforms complex multi-container orchestration from dozens of shell commands into a single declarative YAML file, reducing deployment complexity and human error. Declarative configuration enables version-controlled infrastructure where entire application stacks can be recreated identically across environments. Companies like Lyft use Compose for local development environments that mirror production architecture, catching integration issues before deployment.

---

### Example 20: Docker Compose with Build Context

Docker Compose can build custom images from Dockerfiles during `docker compose up`. Build context and arguments enable flexible image customization.

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  app:
    build:
      context: .
      # => Build context is current directory
      # => All files in this directory available to COPY/ADD
      dockerfile: Dockerfile.prod
      # => Uses Dockerfile.prod instead of default Dockerfile
      args:
        - NODE_VERSION=18
        - BUILD_DATE=2025-12-29
        # => Build arguments passed to Dockerfile
      target: production
      # => Builds only up to "production" stage (multi-stage builds)
    image: my-app:latest
    # => Tags built image as my-app:latest
    ports:
      - "3000:3000"
    environment:
      NODE_ENV: production

  dev-app:
    build:
      context: .
      dockerfile: Dockerfile.prod
      args:
        NODE_VERSION: 18
      target: development
      # => Builds development stage from same Dockerfile
    image: my-app:dev
    # => Different image tag for development
    volumes:
      - .:/app
      # => Bind mount for live code reloading
    ports:
      - "3001:3000"
    environment:
      NODE_ENV: development
```

```dockerfile
# File: Dockerfile.prod

ARG NODE_VERSION=18
# => Build argument for Node.js version

FROM node:${NODE_VERSION}-alpine as base
# => Uses build arg in FROM instruction
# => Names stage as "base"

WORKDIR /app
COPY package*.json ./

# Development stage
FROM base as development
# => Inherits from base stage
RUN npm install
# => Installs all dependencies (including devDependencies)
COPY . .
EXPOSE 3000
CMD ["npm", "run", "dev"]
# => Development command with hot reloading

# Production stage
FROM base as production
# => Inherits from base stage (NOT development)
RUN npm ci --only=production
# => Installs only production dependencies
COPY . .
RUN npm run build
# => Builds optimized production bundle
EXPOSE 3000
CMD ["npm", "start"]
# => Production startup command

# Build information label
ARG BUILD_DATE
LABEL build_date=${BUILD_DATE}
# => Adds build date to image metadata
```

```bash
# Build and start production service
docker compose up -d app
# => Builds image using production target
# => Tags as my-app:latest
# => Starts container in production mode

# Build and start development service
docker compose up -d dev-app
# => Builds image using development target
# => Tags as my-app:dev
# => Starts container with bind mount for live reloading

# Rebuild images (force, ignore cache)
docker compose build --no-cache
# => Rebuilds all services from scratch
# => Useful when dependencies change

# Build specific service
docker compose build app
# => Rebuilds only app service image

# Build with different build args
docker compose build --build-arg NODE_VERSION=20 app
# => Overrides NODE_VERSION build argument
# => Uses Node.js 20 instead of default 18

# View build output
docker compose build app --progress=plain
# => Shows full build output (not condensed)
# => Useful for debugging build issues

# Pull base images before building
docker compose build --pull app
# => Pulls latest base image (node:18-alpine)
# => Ensures base image is up-to-date before building

# Build and push to registry
docker compose build app
docker compose push app
# => Builds image and pushes to Docker registry
# => Requires image name to be fully qualified (registry/image:tag)
```

**Key Takeaway**: Use multi-stage Dockerfiles with different targets for development and production. Docker Compose build arguments enable flexible image customization without duplicating Dockerfiles. Always tag images explicitly to track versions.

**Why It Matters**: Build targets eliminate Dockerfile duplication, reducing maintenance burden when supporting multiple environments from a single source of truth. Arguments enable parameterized builds where base image versions, build flags, and optimization levels adapt to deployment contexts without file proliferation. Image tagging provides traceability linking deployed containers back to specific code commits, critical for debugging production issues and rolling back problematic releases.

---

### Example 21: Docker Compose Environment Variables

Docker Compose supports multiple ways to pass environment variables: inline in compose file, from `.env` files, or from host environment.

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  app:
    image: my-app:latest
    environment:
      # Method 1: Inline key-value pairs
      NODE_ENV: production
      LOG_LEVEL: info
      # Method 2: Value from .env file or host environment
      DATABASE_URL:
      # => If no value specified, reads from .env or host
      # Method 3: Default value with substitution
      API_TIMEOUT: ${API_TIMEOUT:-5000}
      # => Uses API_TIMEOUT from .env, or defaults to 5000
      PORT: ${PORT:-3000}
      # => Uses PORT from .env, or defaults to 3000
    env_file:
      - .env
      - .env.local
      # => Loads variables from files (later files override earlier)
    ports:
      - "${PORT:-3000}:3000"
      # => Port mapping uses variable substitution

  db:
    image: postgres:15-alpine
    environment:
      POSTGRES_USER: ${DB_USER}
      POSTGRES_PASSWORD: ${DB_PASSWORD}
      POSTGRES_DB: ${DB_NAME:-myapp}
      # => Required variables (fail if not set)
    env_file:
      - database.env
      # => Additional database-specific variables
```

```bash
# File: .env (automatically loaded by Docker Compose)
DATABASE_URL=postgresql://user:pass@db:5432/myapp
API_TIMEOUT=10000
PORT=8080
DB_USER=appuser
DB_PASSWORD=secret123
DB_NAME=production_db
```

```bash
# File: .env.local (overrides .env values)
LOG_LEVEL=debug
PORT=8081
```

```bash
# Start services with .env files
docker compose up -d
# => Reads .env automatically (no flag needed)
# => Also reads .env.local (specified in env_file)
# => Variables: DATABASE_URL, API_TIMEOUT=10000, PORT=8081 (from .env.local)

# Verify environment variables in container
docker compose exec app printenv | grep -E "DATABASE_URL|API_TIMEOUT|PORT"
# => DATABASE_URL=postgresql://user:pass@db:5432/myapp
# => API_TIMEOUT=10000
# => PORT=8081 (overridden by .env.local)

# Override variables from command line
PORT=9000 docker compose up -d
# => Host environment variable overrides .env and .env.local
# => App runs on port 9000

# Override with docker compose run
docker compose run -e LOG_LEVEL=trace app npm test
# => Overrides LOG_LEVEL for this specific run
# => Other variables still loaded from .env files

# Check variable substitution
docker compose config
# => Shows resolved configuration with all variables substituted
# => Useful for debugging variable precedence

# Example output of docker compose config:
# => services:
# =>   app:
# =>     environment:
# =>       NODE_ENV: production
# =>       LOG_LEVEL: debug (from .env.local)
# =>       DATABASE_URL: postgresql://user:pass@db:5432/myapp
# =>       API_TIMEOUT: 10000
# =>       PORT: 8081
# =>     ports:
# =>       - "8081:3000"

# Test with missing required variable
unset DB_PASSWORD
docker compose up -d
# => Error: environment variable DB_PASSWORD is not set
# => Compose fails when required variable (no default) is missing
```

**Key Takeaway**: Docker Compose variable precedence (highest to lowest): 1) command-line environment, 2) `.env.local` or later env_file entries, 3) `.env` or earlier env_file entries, 4) inline defaults (`${VAR:-default}`). Use `.env.example` as template and add actual `.env` to `.gitignore` to protect secrets.

**Why It Matters**: Well-defined variable precedence rules enable predictable configuration overrides for testing and emergency hotfixes without editing files. Layered environment files support configuration inheritance where base settings apply globally while environment-specific overrides customize deployment behavior. Understanding precedence prevents configuration surprises where values come from unexpected sources, a common cause of production incidents.

---

### Example 22: Docker Compose Volumes

Docker Compose manages both named volumes and bind mounts. Named volumes provide persistence across container recreations, while bind mounts enable development workflows.

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  # Database with named volume
  db:
    image: postgres:15-alpine
    volumes:
      - db-data:/var/lib/postgresql/data
      # => Named volume (defined in volumes section)
      # => Managed by Docker, persists across recreations
    environment:
      POSTGRES_PASSWORD: secret

  # Application with multiple volume types
  app:
    build: .
    volumes:
      # Bind mount for source code (development)
      - ./src:/app/src:ro
      # => Host ./src directory mounted as read-only
      # => Changes on host immediately visible in container

      # Bind mount for configuration
      - ./config/app.conf:/etc/app/app.conf:ro
      # => Single file bind mount

      # Named volume for generated assets
      - app-cache:/app/.cache
      # => Persists compiled assets across rebuilds

      # Tmpfs for temporary processing
      - type: tmpfs
        target: /tmp
        tmpfs:
          size: 100M
      # => RAM-based temporary storage
    depends_on:
      - db

  # Nginx with bind mount for static content
  web:
    image: nginx:alpine
    volumes:
      - ./web/nginx.conf:/etc/nginx/conf.d/default.conf:ro
      # => Nginx configuration
      - ./web/html:/usr/share/nginx/html:ro
      # => Static HTML files
      - web-logs:/var/log/nginx
      # => Named volume for log persistence
    ports:
      - "8080:80"

# Named volumes definition
volumes:
  db-data:
    driver: local
    # => Default driver (stores on host filesystem)
  app-cache:
    driver: local
  web-logs:
    driver: local
    driver_opts:
      type: none
      o: bind
      device: /var/log/myapp/web
      # => Custom mount options (advanced)
```

```bash
# Start services (creates volumes automatically)
docker compose up -d
# => Creates named volumes: project_db-data, project_app-cache, project_web-logs
# => Bind mounts connect to host directories

# List volumes created by Compose
docker volume ls --filter label=com.docker.compose.project=myproject
# => DRIVER    VOLUME NAME
# => local     myproject_db-data
# => local     myproject_app-cache
# => local     myproject_web-logs

# Inspect named volume
docker volume inspect myproject_db-data
# => [
# =>   {
# =>     "Name": "myproject_db-data",
# =>     "Driver": "local",
# =>     "Mountpoint": "/var/lib/docker/volumes/myproject_db-data/_data",
# =>     "Labels": {
# =>       "com.docker.compose.project": "myproject",
# =>       "com.docker.compose.version": "2.20.0",
# =>       "com.docker.compose.volume": "db-data"
# =>     }
# =>   }
# => ]

# Write data to database (persists in named volume)
docker compose exec db psql -U postgres -c "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT);"
# => CREATE TABLE

# Stop and remove containers (preserves named volumes)
docker compose down
# => Removes containers but keeps volumes

# Start again (data still exists in volumes)
docker compose up -d

# Verify data persisted
docker compose exec db psql -U postgres -c "SELECT * FROM users;"
# => id | name
# => ----+------
# => (0 rows)
# => Table still exists (volume preserved)

# Update bind-mounted file on host (reflects immediately in container)
echo "Updated content" >> ./web/html/index.html
docker compose exec web cat /usr/share/nginx/html/index.html
# => Shows updated content immediately (no restart needed)

# Backup named volume
docker run --rm \
  -v myproject_db-data:/data \
  -v $(pwd):/backup \
  alpine tar czf /backup/db-backup.tar.gz /data
# => Creates tar archive of volume contents

# Restore named volume
docker run --rm \
  -v myproject_db-data:/data \
  -v $(pwd):/backup \
  alpine tar xzf /backup/db-backup.tar.gz -C /
# => Extracts tar archive to volume

# Remove volumes with containers
docker compose down -v
# => Removes containers AND named volumes (DESTRUCTIVE!)
# => Use for complete cleanup
```

**Key Takeaway**: Named volumes provide persistent, portable storage managed by Docker. Use bind mounts for development (live code updates) and configuration files. Always backup named volumes before running `docker compose down -v`.

**Why It Matters**: Combining named volumes for data with bind mounts for code creates optimal development workflows where databases persist across container recreations while source code updates instantly. Volume portability across Docker hosts enables seamless infrastructure migrations without data movement complexities. Compose-managed volumes with backup strategies prevent catastrophic data loss that can occur from accidental `down -v` commands destroying production databases.

---

### Example 23: Docker Compose Depends On

The `depends_on` directive controls service startup order but does NOT wait for services to be ready. Use health checks for readiness dependencies.

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  # Database service
  db:
    image: postgres:15-alpine
    environment:
      POSTGRES_USER: appuser
      POSTGRES_PASSWORD: apppass
      POSTGRES_DB: appdb
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U appuser"]
      # => Checks if PostgreSQL accepts connections
      interval: 5s
      # => Run check every 5 seconds
      timeout: 3s
      # => Fail check if it takes longer than 3 seconds
      retries: 3
      # => Mark unhealthy after 3 consecutive failures
      start_period: 10s
      # => Grace period before checking (allows startup time)

  # Redis cache
  cache:
    image: redis:7-alpine
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      # => Checks if Redis responds to PING
      interval: 5s
      timeout: 3s
      retries: 3
      start_period: 5s

  # API service (basic depends_on)
  api-basic:
    build: ./api
    depends_on:
      - db
      - cache
      # => Starts db and cache BEFORE api-basic
      # => Does NOT wait for db/cache to be ready
      # => API may crash if it connects before db is ready
    environment:
      DATABASE_URL: postgresql://appuser:apppass@db:5432/appdb
      REDIS_URL: redis://cache:6379

  # API service (depends_on with health checks)
  api-healthy:
    build: ./api
    depends_on:
      db:
        condition: service_healthy
        # => Waits for db health check to pass
      cache:
        condition: service_healthy
        # => Waits for cache health check to pass
    environment:
      DATABASE_URL: postgresql://appuser:apppass@db:5432/appdb
      REDIS_URL: redis://cache:6379
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:3000/health"]
      # => Checks API /health endpoint
      interval: 10s
      timeout: 5s
      retries: 3
      start_period: 30s
      # => Longer start_period for API initialization

  # Web frontend (depends on healthy API)
  web:
    image: nginx:alpine
    depends_on:
      api-healthy:
        condition: service_healthy
        # => Waits for API to be healthy before starting
    ports:
      - "8080:80"
    volumes:
      - ./web/nginx.conf:/etc/nginx/conf.d/default.conf:ro
```

```bash
# Start services (observe startup order)
docker compose up -d
# => Step 1: Starts db and cache (no dependencies)
# => Step 2: Waits for db and cache health checks to pass
# => Step 3: Starts api-basic (doesn't wait for health) and api-healthy (waits for health)
# => Step 4: Waits for api-healthy health check to pass
# => Step 5: Starts web (after api-healthy is healthy)

# Monitor service health status
docker compose ps
# => NAME      STATUS                    HEALTH
# => db        Up 30 seconds             healthy
# => cache     Up 30 seconds             healthy
# => api-basic Up 25 seconds             (no health check)
# => api-healthy Up 20 seconds           starting (health: starting)
# => web       Created                   (waiting for api-healthy)

# Wait a bit for health checks
sleep 30
docker compose ps
# => NAME      STATUS                    HEALTH
# => db        Up 1 minute               healthy
# => cache     Up 1 minute               healthy
# => api-basic Up 55 seconds             (no health check)
# => api-healthy Up 50 seconds           healthy
# => web       Up 20 seconds             (no health check)

# View health check logs
docker inspect myproject-db-1 --format='{{json .State.Health}}' | jq
# => {
# =>   "Status": "healthy",
# =>   "FailingStreak": 0,
# =>   "Log": [
# =>     {
# =>       "Start": "2025-12-29T10:50:00Z",
# =>       "End": "2025-12-29T10:50:00Z",
# =>       "ExitCode": 0,
# =>       "Output": "accepting connections"
# =>     }
# =>   ]
# => }

# Test difference between basic and healthy depends_on
docker compose restart db
# => Restarts database

# api-basic may crash immediately (connects before db ready)
docker compose logs api-basic
# => Error: connection refused (database not ready)

# api-healthy waits for db to become healthy again
docker compose ps api-healthy
# => STATUS: Up (health: starting) - waiting for db to be healthy
```

**Key Takeaway**: Use `depends_on` with `condition: service_healthy` for true startup orchestration. Basic `depends_on` only ensures start order, not readiness. Always implement health checks for services that other services depend on.

**Why It Matters**: Health check-based dependencies eliminate race conditions where applications crash trying to connect to databases that haven't finished initializing, a common source of deployment failures. Proper orchestration reduces startup-related outages and enables reliable automated deployments without manual intervention. This pattern is foundational for zero-downtime rolling updates where new containers must wait for dependencies before accepting traffic.

---

### Example 24: Docker Compose Networks

Docker Compose creates isolated networks for services. Multiple networks enable network segmentation (frontend, backend, database layers).

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  # Public-facing web server (frontend network only)
  web:
    image: nginx:alpine
    networks:
      - frontend
      # => Only connected to frontend network
      # => Cannot directly access database
    ports:
      - "8080:80"

  # API application (both networks)
  api:
    build: ./api
    networks:
      - frontend
      # => Can communicate with web
      - backend
      # => Can communicate with database
    environment:
      DATABASE_URL: postgresql://user:pass@db:5432/mydb

  # Database (backend network only)
  db:
    image: postgres:15-alpine
    networks:
      - backend
      # => Only connected to backend network
      # => Not accessible from web (network isolation)
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: pass
      POSTGRES_DB: mydb

  # Admin tool (backend network only)
  admin:
    image: dpage/pgadmin4
    networks:
      - backend
      # => Can access database directly
    ports:
      - "5050:80"
    environment:
      PGADMIN_DEFAULT_EMAIL: admin@example.com
      PGADMIN_DEFAULT_PASSWORD: admin

networks:
  frontend:
    driver: bridge
    # => Network for public-facing services
  backend:
    driver: bridge
    # => Network for backend services
    internal: true
    # => Internal-only network (no external internet access)
```

```mermaid
%% Network segmentation topology
graph TD
    A["Internet"] --> B["Host Port 8080"]
    B --> C["web<br/>frontend network"]
    C --> D["api<br/>frontend + backend"]
    D --> E["db<br/>backend network"]
    F["admin<br/>backend network"] --> E

    style A fill:#0173B2,color:#fff
    style C fill:#029E73,color:#fff
    style D fill:#DE8F05,color:#000
    style E fill:#CC78BC,color:#000
    style F fill:#CC78BC,color:#000
```

```bash
# Start services with network segmentation
docker compose up -d
# => Creates networks: myproject_frontend, myproject_backend

# List networks
docker network ls --filter label=com.docker.compose.project=myproject
# => NETWORK ID     NAME                  DRIVER    SCOPE
# => abc123def456   myproject_frontend    bridge    local
# => def456ghi789   myproject_backend     bridge    local

# Verify web cannot access db directly (different networks)
docker compose exec web ping -c 1 db
# => ping: bad address 'db'
# => Name resolution fails (not on same network)

# Verify api can access both web and db (on both networks)
docker compose exec api ping -c 1 web
# => 64 bytes from 172.20.0.2: seq=0 ttl=64 time=0.123 ms
# => Success (both on frontend network)

docker compose exec api ping -c 1 db
# => 64 bytes from 172.21.0.2: seq=0 ttl=64 time=0.089 ms
# => Success (both on backend network)

# Verify admin can access db (same backend network)
docker compose exec admin ping -c 1 db
# => 64 bytes from 172.21.0.2: seq=0 ttl=64 time=0.095 ms

# Inspect network to see connected services
docker network inspect myproject_frontend --format='{{range .Containers}}{{.Name}}{{"\n"}}{{end}}'
# => myproject-web-1
# => myproject-api-1
# => Only web and api on frontend network

docker network inspect myproject_backend --format='{{range .Containers}}{{.Name}}{{"\n"}}{{end}}'
# => myproject-api-1
# => myproject-db-1
# => myproject-admin-1
# => Only api, db, and admin on backend network

# Create external network (shared across projects)
docker network create shared-network
# => Network exists outside Compose project

# Use external network in compose file
cat >> docker-compose.yml << 'EOF'
  external-service:
    image: my-service
    networks:
      - shared-network

networks:
  shared-network:
    external: true
    # => References existing network (doesn't create new one)
EOF

docker compose up -d external-service
# => Uses existing shared-network
```

**Key Takeaway**: Use multiple networks for security layer segmentation. Services on different networks cannot communicate unless explicitly connected to both. Set `internal: true` for networks that should not access external internet.

**Why It Matters**: Network segmentation implements defense-in-depth security by preventing compromised frontend containers from directly accessing backend databases, limiting attack surface. Internal-only networks protect sensitive infrastructure components from internet access even if container security is breached. Financial and healthcare applications use network segmentation to meet compliance requirements mandating data tier isolation from public-facing services.

---

### Example 25: Docker Compose Restart Policies

Restart policies control container behavior after crashes or host reboots. Choose policies based on service criticality and statelessness.

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  # Critical service: always restart
  database:
    image: postgres:15-alpine
    restart: always
    # => Restarts container on any exit (success or failure)
    # => Restarts after Docker daemon restarts
    # => Restarts after host reboot (if Docker starts at boot)
    environment:
      POSTGRES_PASSWORD: secret

  # Production app: restart unless explicitly stopped
  app:
    build: .
    restart: unless-stopped
    # => Restarts on failure and after Docker daemon restart
    # => Does NOT restart if manually stopped with docker stop
    # => Prevents unwanted restarts during maintenance
    depends_on:
      - database

  # Background job: restart on failure only
  worker:
    build: ./worker
    restart: on-failure:5
    # => Restarts only if container exits with non-zero code
    # => Maximum 5 restart attempts
    # => Stops trying after 5 consecutive failures
    environment:
      MAX_RETRIES: 3

  # Development service: no restart
  dev-tools:
    image: alpine
    restart: "no"
    # => Never restarts automatically
    # => Must be quoted ("no") to avoid YAML boolean conversion
    command: sh -c "echo 'Dev tools ready' && sleep 3600"

  # One-time task: no restart
  migration:
    build: ./migrations
    restart: "no"
    # => Runs once and exits
    # => Does not restart on completion
    command: npm run migrate
    depends_on:
      database:
        condition: service_healthy
```

```bash
# Start services with different restart policies
docker compose up -d
# => All services start with configured restart policies

# Verify restart policies
docker inspect myproject-database-1 --format='{{.HostConfig.RestartPolicy.Name}}'
# => always

docker inspect myproject-app-1 --format='{{.HostConfig.RestartPolicy.Name}}'
# => unless-stopped

docker inspect myproject-worker-1 --format='{{json .HostConfig.RestartPolicy}}'
# => {"Name":"on-failure","MaximumRetryCount":5}

# Test "always" restart policy
docker compose exec database pkill postgres
# => Kills PostgreSQL process inside container
# => Container exits

sleep 2
docker compose ps database
# => STATUS: Up 1 second (restarting)
# => Automatically restarted by Docker

# Test "unless-stopped" restart policy
docker compose stop app
# => Manually stops app container

# Restart Docker daemon (simulates host reboot)
sudo systemctl restart docker
# => Docker daemon restarts

sleep 5
docker compose ps
# => database: Up (restarted by Docker)
# => app: Exited (NOT restarted - manually stopped)
# => worker: Up (restarted if it was running)

# Test "on-failure" restart policy
docker compose exec worker sh -c 'exit 1'
# => Forces worker to exit with error code 1

docker compose logs worker
# => Shows restart attempts (up to 5 times)
# => If still failing after 5 attempts, stays stopped

# Test "no" restart policy
docker compose exec dev-tools pkill sh
# => Kills main process

sleep 2
docker compose ps dev-tools
# => STATUS: Exited (1)
# => No restart attempted

# Change restart policy for running container
docker update --restart=always myproject-dev-tools-1
# => Changes restart policy without recreating container
# => Useful for adjusting policies without downtime

# View restart count
docker inspect myproject-worker-1 --format='{{.RestartCount}}'
# => 0 (if healthy) or number of restarts performed
```

**Key Takeaway**: Use `unless-stopped` for production services (allows maintenance), `always` for critical infrastructure (databases), `on-failure` for batch jobs with retry logic, and `"no"` for one-time tasks. Restart policies survive Docker daemon restarts except for manually stopped containers with `unless-stopped`.

**Why It Matters**: Automatic restart policies transform container platforms into self-healing infrastructure that recovers from transient failures without human intervention. The `unless-stopped` policy enables maintenance windows by preventing unwanted restarts during planned downtime, balancing automation with operational control. Production platforms like Kubernetes build on these restart primitives, making understanding Docker's restart behavior foundational for container orchestration.

---

### Example 26: Docker Compose Profiles

Profiles enable selective service activation, useful for development, testing, and optional services. Services without profiles always start.

```yaml
# File: docker-compose.yml

version: "3.8"

services:
  # Core services (no profile - always start)
  db:
    image: postgres:15-alpine
    environment:
      POSTGRES_PASSWORD: secret

  app:
    build: .
    depends_on:
      - db
    ports:
      - "3000:3000"

  # Development tools (dev profile)
  dev-tools:
    image: alpine
    profiles:
      - dev
      # => Only starts when 'dev' profile is active
    command: sh -c "echo 'Dev tools running' && tail -f /dev/null"

  debugger:
    image: node:18-alpine
    profiles:
      - dev
      - debug
      # => Starts with either 'dev' or 'debug' profile
    command: sh -c "npm install -g node-inspect && tail -f /dev/null"

  # Testing services (test profile)
  test-db:
    image: postgres:15-alpine
    profiles:
      - test
      # => Separate database for testing
    environment:
      POSTGRES_PASSWORD: testpass

  test-runner:
    build:
      context: .
      target: test
      # => Builds test stage from Dockerfile
    profiles:
      - test
    command: npm test
    depends_on:
      - test-db

  # Monitoring (monitoring profile)
  prometheus:
    image: prom/prometheus
    profiles:
      - monitoring
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml:ro

  grafana:
    image: grafana/grafana
    profiles:
      - monitoring
    ports:
      - "3001:3000"
    depends_on:
      - prometheus

  # Documentation server (docs profile)
  docs:
    image: nginx:alpine
    profiles:
      - docs
    ports:
      - "8080:80"
    volumes:
      - ./docs:/usr/share/nginx/html:ro
```

```bash
# Start core services only (no profiles)
docker compose up -d
# => Starts: db, app
# => Skips: dev-tools, debugger, test-db, test-runner, prometheus, grafana, docs

# List services (including inactive profiles)
docker compose ps -a
# => NAME            STATUS        PROFILES
# => db              Up            -
# => app             Up            -
# => dev-tools       Created       dev
# => debugger        Created       debug,dev
# => test-db         Created       test
# => test-runner     Created       test
# => prometheus      Created       monitoring
# => grafana         Created       monitoring
# => docs            Created       docs

# Start with development profile
docker compose --profile dev up -d
# => Starts: db, app, dev-tools, debugger
# => dev-tools and debugger activated by 'dev' profile

# Start with multiple profiles
docker compose --profile dev --profile monitoring up -d
# => Starts: db, app, dev-tools, debugger, prometheus, grafana
# => Combines dev and monitoring services

# Start specific profile service explicitly
docker compose up -d prometheus
# => ERROR: service "prometheus" has profile "monitoring" which is not active
# => Cannot start profile service without activating profile

# Enable profile for specific service
docker compose --profile monitoring up -d prometheus
# => Starts: prometheus (and its dependencies)

# Start all services regardless of profiles
docker compose --profile dev --profile test --profile monitoring --profile docs up -d
# => Starts all services from all profiles

# Use environment variable for profiles
export COMPOSE_PROFILES=dev,monitoring
docker compose up -d
# => Equivalent to --profile dev --profile monitoring
# => Useful for persistent profile activation

# Stop profile services
docker compose --profile dev down
# => Stops and removes: dev-tools, debugger
# => Keeps running: db, app (no profile)

# View configuration with active profiles
docker compose --profile dev --profile monitoring config --services
# => db
# => app
# => dev-tools
# => debugger
# => prometheus
# => grafana
# => Lists services that would start with these profiles
```

**Key Takeaway**: Use profiles to organize optional services by purpose (dev, test, monitoring, docs). Core services should have no profile to always start. Activate profiles with `--profile` flag or `COMPOSE_PROFILES` environment variable for development, testing, and debugging workflows.

**Why It Matters**: Profiles eliminate duplicate Compose files for different purposes, reducing maintenance overhead and configuration drift across development, testing, and production contexts. Selective service activation prevents resource waste from running unnecessary services like monitoring tools or documentation servers in development environments. Teams can maintain single-source-of-truth Compose configurations while supporting diverse workflow requirements.

---

### Example 27: Docker Compose Override Files

Override files customize compose configuration for different environments without modifying the base file. Docker Compose automatically applies `docker-compose.override.yml`.

```yaml
# File: docker-compose.yml (base configuration)

version: "3.8"

services:
  app:
    build: .
    ports:
      - "3000:3000"
    environment:
      NODE_ENV: production

  db:
    image: postgres:15-alpine
    environment:
      POSTGRES_PASSWORD: secret
```

```yaml
# File: docker-compose.override.yml (automatically applied)

version: "3.8"

services:
  app:
    # Override build target for development
    build:
      context: .
      target: development
    # Add bind mount for live reload
    volumes:
      - ./src:/app/src
    # Override environment
    environment:
      NODE_ENV: development
      DEBUG: "app:*"
    # Add command override
    command: npm run dev

  # Add development-only service
  debug-tools:
    image: alpine
    command: tail -f /dev/null
```

```yaml
# File: docker-compose.test.yml (test environment)

version: "3.8"

services:
  app:
    build:
      target: test
    environment:
      NODE_ENV: test
      DATABASE_URL: postgresql://test:test@test-db:5432/testdb
    command: npm test

  db:
    environment:
      POSTGRES_USER: test
      POSTGRES_PASSWORD: test
      POSTGRES_DB: testdb

  # Test-specific service
  test-db:
    image: postgres:15-alpine
    environment:
      POSTGRES_USER: test
      POSTGRES_PASSWORD: test
      POSTGRES_DB: testdb
```

```yaml
# File: docker-compose.prod.yml (production overrides)

version: "3.8"

services:
  app:
    restart: unless-stopped
    environment:
      NODE_ENV: production
    # Production logging driver
    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "3"

  db:
    restart: always
    # Production volume
    volumes:
      - db-prod-data:/var/lib/postgresql/data
    # Resource limits
    deploy:
      resources:
        limits:
          cpus: "2"
          memory: 2G
        reservations:
          cpus: "1"
          memory: 1G

volumes:
  db-prod-data:
    driver: local
```

```bash
# Development: Uses base + override automatically
docker compose up -d
# => Loads: docker-compose.yml + docker-compose.override.yml
# => App runs with development target, bind mounts, dev environment

# Production: Explicitly specify files (no override)
docker compose -f docker-compose.yml -f docker-compose.prod.yml up -d
# => Loads: docker-compose.yml + docker-compose.prod.yml
# => Skips docker-compose.override.yml
# => App runs with production configuration

# Testing: Use test override
docker compose -f docker-compose.yml -f docker-compose.test.yml up -d
# => Loads: docker-compose.yml + docker-compose.test.yml
# => Runs tests with test database

# Disable automatic override loading
docker compose -f docker-compose.yml up -d
# => Loads only docker-compose.yml
# => Ignores docker-compose.override.yml

# Multiple override files (merge in order)
docker compose \
  -f docker-compose.yml \
  -f docker-compose.common.yml \
  -f docker-compose.prod.yml \
  up -d
# => Files merged left-to-right (later files override earlier)
# => Allows layered configuration (base -> common -> prod)

# View merged configuration
docker compose -f docker-compose.yml -f docker-compose.prod.yml config
# => Shows final merged configuration
# => Useful for debugging override precedence

# Example merged output:
# => services:
# =>   app:
# =>     build:
# =>       context: .
# =>     environment:
# =>       NODE_ENV: production (from prod override)
# =>     restart: unless-stopped (from prod override)
# =>     ports:
# =>       - "3000:3000" (from base)

# Environment-specific script
cat > start-prod.sh << 'EOF'
#!/bin/bash
docker compose -f docker-compose.yml -f docker-compose.prod.yml up -d
EOF
chmod +x start-prod.sh
# => Production startup script with correct file order
```

**Key Takeaway**: Use `docker-compose.override.yml` for local development (auto-loaded). Create environment-specific overrides (`prod.yml`, `test.yml`) and explicitly load with `-f` flag. Files merge left-to-right, with later files taking precedence. Use `docker compose config` to debug merged configuration.

**Why It Matters**: Compose override files enable environment-specific customization without forking base configurations, maintaining single-source-of-truth infrastructure definitions while supporting divergent deployment requirements. Automatic override loading for local development eliminates accidental production configuration usage, preventing developers from deploying insecure or resource-limited configurations to production. This layering approach scales from solo developers to large teams managing complex multi-environment deployments.

---

## Summary: Beginner Examples 1-27

You've completed the beginner level, covering:

- **Installation & Basics** (1-2): Hello world, interactive containers
- **Dockerfile Fundamentals** (3-7): FROM, WORKDIR, COPY, RUN, CMD, EXPOSE, ARG, ENV, LABEL
- **Image Management** (8): Listing, tagging, removing, history
- **Container Lifecycle** (9-12): States, logs, inspection, exec, port mapping
- **Data Persistence** (13-15): Named volumes, bind mounts, tmpfs
- **Networking** (16-17): Bridge networks, container communication
- **Configuration** (18): Environment variables from files
- **Docker Compose** (19-27): Basics, build context, environment, volumes, depends_on, networks, restart policies, profiles, overrides

**Next steps**: Continue to [Intermediate](/en/learn/software-engineering/infrastructure/tools/docker/by-example/intermediate) for multi-stage builds, health checks, resource limits, and production patterns (40-75% coverage).
