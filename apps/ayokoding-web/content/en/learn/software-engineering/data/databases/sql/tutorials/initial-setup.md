---
title: "Initial Setup"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100001
description: "Learn SQL fundamentals and execute your first queries"
tags:
  - sql
  - database
  - query
---

Learn SQL fundamentals and execute your first queries. This guide introduces SQL syntax and basic operations using SQLite (no installation required).

## 🎯 What You'll Accomplish

By the end of this tutorial, you'll have:

- ✅ SQLite installed and ready to use
- ✅ Understanding of basic SQL syntax
- ✅ Your first database and table created
- ✅ Basic queries executed

## 📋 Prerequisites

- Basic familiarity with your computer's terminal/command line
- No prior SQL or database experience required

## 💾 Step 1: Install SQLite

SQLite comes pre-installed on macOS and most Linux distributions. For Windows, download from [sqlite.org](https://www.sqlite.org/download.html).

### Verify Installation

```bash
sqlite3 --version
```

Expected output:

```
3.x.x
```

## 🗄️ Step 2: Create Your First Database

```bash
sqlite3 myapp.db
```

This creates a new database file and opens the SQLite shell.

## 📊 Step 3: Create Your First Table

```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
```

View tables:

```sql
.tables
.schema users
```

## 🚀 Step 4: Insert and Query Data

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

- [ ] `sqlite3 --version` shows SQLite installed
- [ ] You created a database file
- [ ] You can create tables and insert data
- [ ] You can query data with SELECT

## 🎉 You're Done!

You've successfully learned basic SQL operations. You're ready for more advanced queries.

## 📚 What's Next?

**Quick learner**: [SQL Quick Start](/en/learn/software-engineering/data/databases/sql/tutorials/quick-start)

**Code-first learner**: [SQL By Example](/en/learn/software-engineering/data/databases/sql/tutorials/by-example)

## 🆘 Troubleshooting

### SQLite Not Found

**Problem**: "sqlite3: command not found"

**Solution**: Install SQLite:

```bash
# macOS
brew install sqlite

# Ubuntu/Debian
sudo apt install sqlite3

# Windows: Download from sqlite.org
```
