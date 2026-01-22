# Multi-stage Dockerfile for Go applications
# Go Version: 1.25.6
# Base Image: Alpine Linux (minimal, secure)
# Last Updated: 2026-01-22

# Stage 1: Build Stage
FROM golang:1.25.6-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    git \
    ca-certificates \
    tzdata

# Set working directory
WORKDIR /build

# Copy go.mod and go.sum for dependency caching
COPY go.mod go.sum ./

# Download dependencies (cached layer if go.mod/go.sum unchanged)
RUN go mod download
RUN go mod verify

# Copy source code
COPY . .

# Build application
# CGO_ENABLED=0 for static binary
# -ldflags for binary optimization and version info
# -trimpath for reproducible builds (removes file system paths)
ARG VERSION=dev
ARG COMMIT=unknown
ARG BUILD_DATE=unknown

RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build \
    -ldflags="-w -s -X main.version=${VERSION} -X main.commit=${COMMIT} -X main.buildDate=${BUILD_DATE}" \
    -trimpath \
    -o /build/app \
    ./cmd/server

# Stage 2: Runtime Stage
FROM alpine:3.21

# Install runtime dependencies
RUN apk add --no-cache \
    ca-certificates \
    tzdata

# Create non-root user
RUN addgroup -g 1000 appgroup && \
    adduser -D -u 1000 -G appgroup appuser

# Set working directory
WORKDIR /app

# Copy binary from builder
COPY --from=builder /build/app /app/app

# Copy additional files if needed (config, migrations, etc.)
# COPY --from=builder /build/config /app/config
# COPY --from=builder /build/migrations /app/migrations

# Change ownership to non-root user
RUN chown -R appuser:appgroup /app

# Switch to non-root user
USER appuser

# Expose port (document only, doesn't publish)
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD ["/app/app", "health"] || exit 1

# Set environment variables
ENV APP_ENV=production
ENV LOG_LEVEL=info

# Run application
ENTRYPOINT ["/app/app"]
CMD ["server"]

# Build and run instructions:
#
# Build image:
#   docker build -t my-go-app:latest .
#
# Build with version info:
#   docker build \
#     --build-arg VERSION=1.0.0 \
#     --build-arg COMMIT=$(git rev-parse HEAD) \
#     --build-arg BUILD_DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ") \
#     -t my-go-app:1.0.0 .
#
# Run container:
#   docker run -d -p 8080:8080 --name my-app my-go-app:latest
#
# Run with environment variables:
#   docker run -d -p 8080:8080 \
#     -e APP_ENV=production \
#     -e DATABASE_URL=postgres://... \
#     --name my-app my-go-app:latest
#
# Run with volume mount for config:
#   docker run -d -p 8080:8080 \
#     -v $(pwd)/config:/app/config:ro \
#     --name my-app my-go-app:latest

# Docker Compose example:
#
# version: '3.8'
# services:
#   app:
#     build:
#       context: .
#       args:
#         VERSION: 1.0.0
#     ports:
#       - "8080:8080"
#     environment:
#       - APP_ENV=production
#       - DATABASE_URL=postgres://db:5432/myapp
#     depends_on:
#       - db
#     restart: unless-stopped
#
#   db:
#     image: postgres:16-alpine
#     environment:
#       POSTGRES_DB: myapp
#       POSTGRES_USER: user
#       POSTGRES_PASSWORD: password
#     volumes:
#       - db-data:/var/lib/postgresql/data
#     restart: unless-stopped
#
# volumes:
#   db-data:

# Best Practices Demonstrated:
#
# 1. Multi-stage build
#    - Smaller final image (Alpine vs full Go image)
#    - Only runtime dependencies in final image
#    - Build cache optimization with go.mod/go.sum
#
# 2. Security
#    - Non-root user (appuser)
#    - Minimal base image (Alpine Linux)
#    - Static binary (CGO_ENABLED=0)
#    - Read-only root filesystem compatible
#
# 3. Reproducibility
#    - Pinned Go version (1.25.6)
#    - Pinned Alpine version (3.21)
#    - go mod verify for integrity
#    - -trimpath for reproducible builds
#
# 4. Observability
#    - Version info via ldflags
#    - Health check endpoint
#    - Structured logging
#
# 5. Performance
#    - Static binary (no dynamic linking)
#    - Optimized with -ldflags "-w -s"
#    - Dependency caching
#
# 6. Size optimization
#    - Multi-stage build (~10-20MB final image)
#    - Alpine base (minimal footprint)
#    - No build tools in final image

# Alternative: Distroless image (more secure, even smaller)
#
# FROM gcr.io/distroless/static:nonroot
# COPY --from=builder /build/app /app
# EXPOSE 8080
# USER nonroot:nonroot
# ENTRYPOINT ["/app"]

# Alternative: Scratch image (smallest possible)
#
# FROM scratch
# COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
# COPY --from=builder /usr/share/zoneinfo /usr/share/zoneinfo
# COPY --from=builder /build/app /app
# EXPOSE 8080
# ENTRYPOINT ["/app"]
