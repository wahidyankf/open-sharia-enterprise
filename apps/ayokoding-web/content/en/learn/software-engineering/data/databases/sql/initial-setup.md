---
title: "Initial Setup"
date: 2026-01-29T00:00:00+07:00
draft: false
weight: 100001
description: "Get SQLite installed and running on your system - installation, verification, and your first working database"
tags: ["sql", "sqlite", "installation", "setup", "beginner", "database"]
---

**Want to start learning SQL?** This initial setup guide gets SQLite installed and working on your system. By the end, you'll have SQLite running and will create your first database with queries.

This tutorial provides 0-5% coverage - just enough to get SQL working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/data/databases/sql/quick-start) (5-30% coverage).

## Prerequisites

Before installing SQLite, you need:

- A computer running Windows, macOS, or Linux
- A terminal/command prompt
- Basic command-line navigation skills
- A text editor (VS Code, Notepad++, Vim, or any editor)

No prior SQL or database experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** SQLite command-line tools on your operating system
2. **Verify** that SQLite is installed correctly and check the version
3. **Create** your first database and table
4. **Execute** basic SQL queries (INSERT, SELECT, UPDATE, DELETE)
5. **Navigate** SQLite's command-line interface

## Why SQLite?

**SQLite is perfect for learning SQL** because it:

- **Requires no server** - Just a single executable file
- **Zero configuration** - No setup or administration needed
- **Cross-platform** - Runs identically on Windows, macOS, Linux
- **Lightweight** - Database is a single file on disk
- **Full SQL support** - Implements most SQL standard features
- **Production-ready** - Powers mobile apps, browsers, embedded systems

Learn SQL with SQLite, then apply knowledge to PostgreSQL, MySQL, or any SQL database.

## Platform-Specific Installation

Choose your operating system and follow the installation steps.

### Windows Installation

**Step 1: Download SQLite**

1. Visit [sqlite.org/download.html](https://sqlite.org/download.html)
2. Scroll to "Precompiled Binaries for Windows"
3. Download **sqlite-tools-win32-x86-XXXXXXX.zip** (command-line tools)

**Step 2: Extract and Install**

1. Extract ZIP file to a permanent location (e.g., `C:\sqlite`)
2. The extracted folder should contain `sqlite3.exe`

**Step 3: Add to PATH**

1. Open System Properties:
   - Right-click "This PC" → Properties
   - Click "Advanced system settings"
   - Click "Environment Variables"
2. Under "System variables", select "Path" and click "Edit"
3. Click "New" and add `C:\sqlite` (or your extraction path)
4. Click "OK" on all dialogs

**Step 4: Verify Installation**

Open new Command Prompt or PowerShell:

```cmd
sqlite3 --version
```

**Expected output**:

```
3.45.0 2024-01-15 14:20:47 ...
```

**Troubleshooting Windows**:

- If `sqlite3 --version` fails, restart Command Prompt to reload PATH
- Verify `sqlite3.exe` exists in the directory you added to PATH
- Try running with full path: `C:\sqlite\sqlite3.exe --version`

### macOS Installation

SQLite comes pre-installed on macOS, but you may want the latest version.

**Option 1: Use Pre-installed SQLite**

Check if SQLite is already installed:

```bash
sqlite3 --version
```

**Expected output**:

```
3.39.5 2022-10-14 20:58:05 ...
```

If output shows a version, SQLite is ready. Skip to "Your First Database" section.

**Option 2: Install Latest Version via Homebrew**

For the latest features:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install sqlite3

echo 'export PATH="/usr/local/opt/sqlite/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc

sqlite3 --version
```

**Expected output**:

```
3.45.0 2024-01-15 14:20:47 ...
```

**Option 3: Manual Installation**

1. Visit [sqlite.org/download.html](https://sqlite.org/download.html)
2. Download "Precompiled Binaries for macOS" (sqlite-tools-osx-x86-XXXXXXX.zip)
3. Extract and copy `sqlite3` to `/usr/local/bin`:

```bash
unzip sqlite-tools-osx-x86-XXXXXXX.zip

sudo cp sqlite-tools-osx-x86-XXXXXXX/sqlite3 /usr/local/bin/

sudo chmod +x /usr/local/bin/sqlite3

sqlite3 --version
```

**Troubleshooting macOS**:

- If Homebrew installation conflicts with system SQLite, use full path: `/usr/local/opt/sqlite/bin/sqlite3`
- If permission denied, use `sudo` for copy operations
- Verify PATH priority: `which sqlite3`

### Linux Installation

SQLite is often pre-installed, but you can install the latest version via package manager.

**Ubuntu/Debian**:

```bash
sudo apt update

sudo apt install -y sqlite3

sqlite3 --version
```

**Fedora/RHEL/CentOS**:

```bash
sudo dnf install -y sqlite

sqlite3 --version
```

**Arch Linux**:

```bash
sudo pacman -S sqlite

sqlite3 --version
```

**Expected output** (all distributions):

```
3.45.0 2024-01-15 14:20:47 ...
```

**Troubleshooting Linux**:

- If `sqlite3` not found after installation, verify package name with package manager
- Check if binary is in PATH: `which sqlite3`
- Try using full path: `/usr/bin/sqlite3 --version`

### Docker Installation (Cross-Platform)

Docker provides isolated SQLite environments ideal for experimentation.

**Prerequisites**: Docker installed ([docker.com](https://www.docker.com/products/docker-desktop))

**Option 1: Official SQLite Alpine Image**

```bash
docker pull alpine

docker run -it --rm alpine sh

apk add --no-cache sqlite
sqlite3 --version
```

**Option 2: Custom SQLite Container**

```bash
docker pull nouchka/sqlite3

docker run --name sqlite-tutorial \
  -v sqlite-data:/data \
  -d nouchka/sqlite3:latest tail -f /dev/null

docker exec -it sqlite-tutorial sqlite3 /data/tutorial.db
```

**Verify Docker SQLite**:

```bash
docker exec -it sqlite-tutorial sqlite3 --version
```

**Expected output**:

```
3.45.0 2024-01-15 14:20:47 ...
```

## Your First Database

Create your first SQLite database and table.

### Start SQLite

Open SQLite command-line interface:

```bash
sqlite3 myapp.db
```

**What happens**:

- SQLite creates `myapp.db` file in current directory (if not exists)
- Opens interactive SQL shell connected to database
- Shows `sqlite>` prompt

**SQLite prompt**:

```
SQLite version 3.45.0 2024-01-15 14:20:47
Enter ".help" for usage hints.
sqlite>
```

### Create Table

Create a `users` table:

```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL UNIQUE,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);
```

**Expected output**:

```
sqlite>
```

No output means success. SQLite only shows output on errors or queries returning data.

**Table structure explained**:

- `id INTEGER PRIMARY KEY AUTOINCREMENT`: Auto-incrementing integer primary key
- `username TEXT NOT NULL UNIQUE`: Text field, required, unique
- `email TEXT NOT NULL UNIQUE`: Text field, required, unique
- `created_at TEXT DEFAULT CURRENT_TIMESTAMP`: Timestamp, defaults to current time

### Verify Table Creation

```sql
-- List all tables in database
.tables

-- Show table structure
.schema users
```

**Expected output**:

```
users

CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    email TEXT NOT NULL UNIQUE,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);
```

## Your First SQL Queries

Execute basic SQL operations: INSERT, SELECT, UPDATE, DELETE.

### Insert Data

```sql
-- Insert single user
INSERT INTO users (username, email)
VALUES ('alice', 'alice@example.com');

-- Insert multiple users
INSERT INTO users (username, email)
VALUES
    ('bob', 'bob@example.com'),
    ('charlie', 'charlie@example.com'),
    ('diana', 'diana@example.com');
```

**Expected output**:

```
sqlite>
```

No output means success.

### Select Data

```sql
-- Select all users
SELECT * FROM users;

-- Select specific columns
SELECT id, username FROM users;

-- Select with condition
SELECT * FROM users WHERE username = 'alice';

-- Select with sorting
SELECT * FROM users ORDER BY created_at DESC;

-- Select with limit
SELECT * FROM users LIMIT 2;
```

**Expected output** (SELECT \* FROM users):

```
1|alice|alice@example.com|2026-01-29 10:30:45
2|bob|bob@example.com|2026-01-29 10:30:45
3|charlie|charlie@example.com|2026-01-29 10:30:45
4|diana|diana@example.com|2026-01-29 10:30:46
```

**Note**: Default output is pipe-separated. Change format with `.mode` command (see "Useful SQLite Commands").

### Update Data

```sql
-- Update single user
UPDATE users
SET email = 'alice.updated@example.com'
WHERE username = 'alice';

-- Update multiple users
UPDATE users
SET created_at = CURRENT_TIMESTAMP
WHERE id IN (2, 3);
```

**Expected output**:

```
sqlite>
```

### Delete Data

```sql
-- Delete single user
DELETE FROM users WHERE username = 'diana';

-- Delete with condition
DELETE FROM users WHERE id > 10;
```

**Expected output**:

```
sqlite>
```

### Count Rows

```sql
-- Count all users
SELECT COUNT(*) FROM users;

-- Count with condition
SELECT COUNT(*) FROM users WHERE created_at > '2026-01-29';
```

**Expected output**:

```
3
```

## Useful SQLite Commands

Navigate and manage SQLite using dot commands (start with `.`).

### Database Commands

```sql
-- Show all databases attached
.databases

-- Attach another database
ATTACH DATABASE 'other.db' AS other;

-- Detach database
DETACH DATABASE other;

-- Exit SQLite
.quit
-- Or use shortcut
.exit
```

### Table Commands

```sql
-- List all tables
.tables

-- Show table schema
.schema users

-- Show all schemas
.schema

-- Show table info
.fullschema users
```

### Output Formatting

```sql
-- Change output mode
.mode column        -- Column-aligned (readable)
.mode csv           -- Comma-separated values
.mode json          -- JSON array format
.mode list          -- Default pipe-separated
.mode table         -- ASCII table borders

-- Show column headers
.headers on

-- Example: Formatted output
.mode column
.headers on
SELECT * FROM users;
```

**Expected output** (column mode):

```
id  username  email                       created_at
--  --------  --------------------------  -------------------
1   alice     alice.updated@example.com   2026-01-29 10:30:45
2   bob       bob@example.com             2026-01-29 10:35:12
3   charlie   charlie@example.com         2026-01-29 10:35:12
```

### Import and Export Data

```sql
-- Export table to CSV
.mode csv
.output users.csv
SELECT * FROM users;
.output stdout      -- Reset output to terminal

-- Import CSV to table
.mode csv
.import users.csv users

-- Export entire database to SQL
.output backup.sql
.dump
.output stdout

-- Execute SQL from file
.read script.sql
```

### Query History and Timing

```sql
-- Show query execution time
.timer on

-- Execute query (shows timing)
SELECT COUNT(*) FROM users;

-- Turn off timer
.timer off

-- Save query history to file
.output history.txt
.show
.output stdout
```

### Help and Information

```sql
-- Show SQLite version
.version

-- Show all settings
.show

-- Show help for dot commands
.help

-- Show help for specific command
.help mode
```

## SQLite Data Types

SQLite uses dynamic typing with five storage classes.

### Storage Classes

| Storage Class | Description                    | SQL Type Affinity |
| ------------- | ------------------------------ | ----------------- |
| **NULL**      | Null value                     | NULL              |
| **INTEGER**   | Signed integer (1-8 bytes)     | INTEGER           |
| **REAL**      | Floating point (8 bytes)       | REAL              |
| **TEXT**      | Text string (UTF-8/UTF-16)     | TEXT              |
| **BLOB**      | Binary data (exactly as input) | BLOB              |

### Type Affinity Examples

```sql
-- Integer types
CREATE TABLE numbers (
    small_int INTEGER,
    big_int BIGINT,
    tiny_int TINYINT
);

-- Text types
CREATE TABLE texts (
    name TEXT,
    description VARCHAR(255),
    bio CLOB
);

-- Real types
CREATE TABLE decimals (
    price REAL,
    weight DOUBLE,
    ratio FLOAT
);

-- Date/time stored as TEXT or INTEGER
CREATE TABLE events (
    event_time TEXT,                -- ISO 8601 format
    timestamp INTEGER               -- Unix timestamp
);
```

**Important**: SQLite is flexible with types. You can store any value in any column regardless of declared type (except PRIMARY KEY).

## Working with Files

SQLite databases are single files - easy to copy, backup, and share.

### Database File Location

```sql
-- Show current database file
.databases
```

**Expected output**:

```
main: /path/to/myapp.db r/w
```

### Copy Database

**macOS/Linux**:

```bash
cp myapp.db myapp_backup.db

cp myapp.db myapp_$(date +%Y%m%d).db
```

**Windows**:

```cmd
REM Copy database file
copy myapp.db myapp_backup.db
```

### In-Memory Database

Create temporary database in memory (lost on exit):

```bash
sqlite3 :memory:
```

**Use case**: Testing, temporary calculations, fast operations.

### Attach Multiple Databases

Work with multiple databases simultaneously:

```sql
-- Attach second database
ATTACH DATABASE 'other.db' AS other;

-- Query across databases
SELECT u.username, o.order_id
FROM users u
JOIN other.orders o ON u.id = o.user_id;

-- Copy table between databases
CREATE TABLE other.users AS SELECT * FROM main.users;

-- Detach database
DETACH DATABASE other;
```

## Configuration and Settings

Configure SQLite behavior using pragma statements.

### Common PRAGMA Commands

```sql
-- Show foreign key enforcement (off by default)
PRAGMA foreign_keys;

-- Enable foreign key enforcement
PRAGMA foreign_keys = ON;

-- Show database encoding
PRAGMA encoding;

-- Show journal mode
PRAGMA journal_mode;

-- Set write-ahead logging (better concurrency)
PRAGMA journal_mode = WAL;

-- Show cache size
PRAGMA cache_size;

-- Set cache size (negative = KB, positive = pages)
PRAGMA cache_size = -8000;  -- 8MB cache

-- Show database integrity
PRAGMA integrity_check;

-- Show quick integrity check
PRAGMA quick_check;

-- Show database statistics
PRAGMA database_list;
PRAGMA table_info(users);
```

### Performance Settings

```sql
-- Disable synchronous writes (faster but less safe)
PRAGMA synchronous = OFF;   -- Use only for testing

-- Enable memory-mapped I/O
PRAGMA mmap_size = 268435456;  -- 256MB

-- Set temp store to memory
PRAGMA temp_store = MEMORY;
```

**Warning**: `PRAGMA synchronous = OFF` risks database corruption on system crash. Use only for temporary databases.

## Common SQL Patterns

Master fundamental SQL patterns for daily use.

### Filtering and Sorting

```sql
-- WHERE clause
SELECT * FROM users WHERE id > 2;

-- Multiple conditions
SELECT * FROM users WHERE id > 1 AND username LIKE 'a%';

-- IN operator
SELECT * FROM users WHERE id IN (1, 2, 3);

-- ORDER BY
SELECT * FROM users ORDER BY created_at DESC;

-- LIMIT and OFFSET (pagination)
SELECT * FROM users LIMIT 10 OFFSET 20;  -- Page 3 (rows 21-30)
```

### Aggregation

```sql
-- Count rows
SELECT COUNT(*) FROM users;

-- Count non-null values
SELECT COUNT(email) FROM users;

-- Distinct values
SELECT COUNT(DISTINCT username) FROM users;

-- Group by
SELECT username, COUNT(*) as order_count
FROM orders
GROUP BY username;

-- Group by with filtering
SELECT username, COUNT(*) as order_count
FROM orders
GROUP BY username
HAVING COUNT(*) > 5;
```

### Joins

```sql
-- Create related table
CREATE TABLE orders (
    id INTEGER PRIMARY KEY,
    user_id INTEGER,
    product TEXT,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- INNER JOIN
SELECT u.username, o.product
FROM users u
INNER JOIN orders o ON u.id = o.user_id;

-- LEFT JOIN (include all users, even without orders)
SELECT u.username, o.product
FROM users u
LEFT JOIN orders o ON u.id = o.user_id;
```

## Next Steps

You now have SQLite installed and working. Here's what to learn next:

1. **[Quick Start](/en/learn/software-engineering/data/databases/sql/quick-start)** - Build a complete application with relationships, indexes, and complex queries (5-30% coverage)
2. **[By-Example Tutorial](/en/learn/software-engineering/data/databases/sql/by-example)** - Learn through 85 annotated examples covering 95% of SQL
3. **[SQLite Documentation](https://sqlite.org/docs.html)** - Comprehensive reference

For production databases with advanced features (JSONB, full-text search, replication), consider:

- **[PostgreSQL](/en/learn/software-engineering/data/databases/postgresql)** - Advanced open-source database
- **[MySQL](/en/learn/software-engineering/data/databases/mysql)** - Popular open-source database

## Summary

In this initial setup tutorial, you learned how to:

1. Install SQLite command-line tools on Windows, macOS, or Linux
2. Start SQLite and create your first database
3. Create tables with various data types and constraints
4. Execute basic SQL queries (INSERT, SELECT, UPDATE, DELETE)
5. Use SQLite dot commands for database management
6. Format query output for readability
7. Work with database files (copy, backup, attach)
8. Configure SQLite behavior with PRAGMA statements

You're now ready to explore SQL's powerful features: joins, subqueries, indexes, transactions, and complex queries. Continue to the Quick Start tutorial to build a real application.

## Common Issues and Solutions

### Database is Locked

**Problem**: "database is locked" error

**Solutions**:

1. Close other programs accessing the database
2. Check for stale lock files (`.db-journal`, `.db-wal`)
3. Use `PRAGMA busy_timeout = 5000;` to wait for locks
4. Enable WAL mode: `PRAGMA journal_mode = WAL;`

### Permission Denied

**Problem**: Cannot create or open database file

**Solutions**:

1. Check file permissions: `ls -la myapp.db`
2. Verify write permissions on directory
3. Try creating database in home directory: `~/myapp.db`

### Syntax Error

**Problem**: SQL statement fails with syntax error

**Solutions**:

1. Check for missing semicolon at end of statement
2. Verify table and column names are correct
3. Use `.schema` to see exact table structure
4. Check SQLite documentation for supported syntax

### Foreign Key Not Working

**Problem**: Foreign key constraints not enforced

**Solution**: Enable foreign keys (disabled by default):

```sql
PRAGMA foreign_keys = ON;
```

**Note**: This must be set for each database connection.

## Additional Resources

- [Official SQLite Documentation](https://sqlite.org/docs.html)
- [SQLite Tutorial](https://www.sqlitetutorial.net/)
- [DB Browser for SQLite](https://sqlitebrowser.org/) - GUI tool
- [SQLite FAQ](https://sqlite.org/faq.html)
- [SQL Practice Platforms](https://www.sql-practice.com/)
