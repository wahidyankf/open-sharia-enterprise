---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Install PostgreSQL and execute your first queries"
tags:
  - postgresql
  - database
  - installation
  - sql
---

Get PostgreSQL installed and execute your first queries. This guide walks you through installation on any operating system and basic database operations.

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ PostgreSQL installed and running
- ✅ psql command-line tool configured
- ✅ Your first database created
- ✅ Basic SQL queries executed

## 📋 Prerequisites

- Basic familiarity with your computer's terminal/command line
- No prior PostgreSQL or database experience required

## 💾 Step 1: Install PostgreSQL

### Windows

Download the installer from [postgresql.org](https://www.postgresql.org/download/windows/):

1. Run the installer
2. Choose installation directory
3. Select components (PostgreSQL Server, pgAdmin, Command Line Tools)
4. Set password for postgres superuser (remember this!)
5. Set port (default: 5432)
6. Choose locale
7. Complete installation

### macOS

Using Homebrew:

```bash
brew install postgresql@16
brew services start postgresql@16
```

Or download [Postgres.app](https://postgresapp.com/) for a GUI experience.

### Linux (Ubuntu/Debian)

```bash
sudo apt update
sudo apt install postgresql postgresql-contrib
sudo systemctl start postgresql
sudo systemctl enable postgresql
```

## ✅ Step 2: Verify Installation

```bash
psql --version
```

Expected output:

```
psql (PostgreSQL) 16.x
```

## 🔐 Step 3: Access PostgreSQL

### Linux/macOS

```bash
sudo -u postgres psql
```

### Windows

Open "SQL Shell (psql)" from Start menu.

### Create Your User (Linux/macOS)

```sql
CREATE USER yourname WITH PASSWORD 'yourpassword';
ALTER USER yourname CREATEDB;
\q
```

Then connect as your user:

```bash
psql -U yourname -d postgres
```

## 🗄️ Step 4: Create Your First Database

```sql
CREATE DATABASE myapp;
\l
```

Connect to your database:

```sql
\c myapp
```

## 📊 Step 5: Create Your First Table

```sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

Verify table creation:

```sql
\dt
\d users
```

## 🚀 Step 6: Insert and Query Data

Insert data:

```sql
INSERT INTO users (name, email) VALUES
    ('Alice Johnson', 'alice@example.com'),
    ('Bob Smith', 'bob@example.com'),
    ('Charlie Brown', 'charlie@example.com');
```

Query data:

```sql
SELECT * FROM users;
SELECT name, email FROM users WHERE name LIKE 'A%';
SELECT COUNT(*) FROM users;
```

## ✅ Verification Checklist

Before moving forward, verify:

- [ ] `psql --version` shows PostgreSQL installed
- [ ] You can connect to PostgreSQL with psql
- [ ] You created a database successfully
- [ ] You can create tables and insert data
- [ ] You can query data with SELECT

## 🎉 You're Done

You've successfully installed PostgreSQL and executed your first queries. You're ready to learn more advanced database operations.

## 📚 What's Next?

**Quick learner**: [PostgreSQL Quick Start](/en/learn/software-engineering/data/databases/postgresql/tutorials/quick-start)

- Learn essential SQL operations and PostgreSQL features
- Understand enough to work with databases independently

**Code-first learner**: [PostgreSQL By Example](/en/learn/software-engineering/data/databases/postgresql/tutorials/by-example)

- Practical examples covering common database operations
- Best for learning through real-world scenarios

- Progressive learning path from beginner to advanced
- Deep understanding of PostgreSQL capabilities

## 🆘 Troubleshooting

### Connection Issues

**Problem**: "psql: error: connection to server failed"

**Solution**: Verify PostgreSQL service is running:

```bash
# macOS
brew services list

# Linux
sudo systemctl status postgresql

# Windows
# Check Services app for "postgresql-x64-16"
```

### Authentication Failed

**Problem**: "psql: FATAL: password authentication failed"

**Solution**: Reset postgres password:

```bash
# Linux/macOS
sudo -u postgres psql
ALTER USER postgres WITH PASSWORD 'newpassword';
```

### Port Already in Use

**Problem**: "Port 5432 is already in use"

**Solution**: Either stop the existing PostgreSQL instance or configure a different port in `postgresql.conf`.
