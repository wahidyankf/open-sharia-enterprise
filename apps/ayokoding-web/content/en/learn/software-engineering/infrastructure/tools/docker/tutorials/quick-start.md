---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn core Docker concepts and containerization patterns"
tags:
  - docker
  - containers
  - devops
  - quick-start
---

Learn core Docker concepts and containerization patterns. This Quick Start teaches essential Docker skills.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- Images and containers
- Dockerfiles and building images
- Docker Compose for multi-container apps
- Volumes and networking

## 📋 Prerequisites

- Docker installed (see [Initial Setup](/en/learn/software-engineering/infrastructure/tools/docker/tutorials/initial-setup))

## 📦 Dockerfile

Create `Dockerfile`:

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY . .
EXPOSE 3000
CMD ["node", "server.js"]
```

Build:

```bash
docker build -t myapp .
docker run -p 3000:3000 myapp
```

## 🔧 Docker Compose

Create `docker-compose.yml`:

```yaml
version: "3.8"
services:
  web:
    build: .
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=production
    depends_on:
      - db

  db:
    image: postgres:15
    environment:
      POSTGRES_PASSWORD: secret
    volumes:
      - pgdata:/var/lib/postgresql/data

volumes:
  pgdata:
```

Run:

```bash
docker-compose up -d
docker-compose ps
docker-compose logs
docker-compose down
```

## 💾 Volumes

```bash
docker volume create mydata
docker run -v mydata:/data alpine
```

## 🌐 Networking

```bash
docker network create mynetwork
docker run --network mynetwork --name app1 nginx
docker run --network mynetwork --name app2 alpine ping app1
```

## ✅ Next Steps

You now understand Docker essentials!

1. **Try the examples**: Build images and run containers
2. **Explore By Example**: [Docker By Example](/en/learn/software-engineering/infrastructure/tools/docker/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Write Dockerfiles
- [ ] Build and run containers
- [ ] Use Docker Compose
- [ ] Work with volumes and networks
