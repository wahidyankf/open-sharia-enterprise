---
title: "Initial Setup"
date: 2026-01-29T00:00:00+07:00
draft: false
weight: 100001
description: "Get PostgreSQL installed and running on your system - installation, verification, and your first working database"
tags: ["postgresql", "installation", "setup", "beginner", "database"]
---

**Want to start working with PostgreSQL?** This initial setup guide gets PostgreSQL installed and working on your system. By the end, you'll have PostgreSQL running and will create your first database with queries.

This tutorial provides 0-5% coverage - just enough to get PostgreSQL working on your machine. For deeper learning, continue to [Quick Start](/en/learn/software-engineering/data/databases/postgresql/quick-start) (5-30% coverage).

## Prerequisites

Before installing PostgreSQL, you need:

- A computer running Windows, macOS, or Linux
- Administrator/sudo access for installation
- A terminal/command prompt
- Basic command-line navigation skills
- A text editor or SQL client (psql, pgAdmin, DBeaver)

No prior PostgreSQL or database experience required - this guide starts from zero.

## Learning Objectives

By the end of this tutorial, you will be able to:

1. **Install** PostgreSQL server and client tools on your operating system
2. **Verify** that PostgreSQL is installed correctly and running
3. **Connect** to PostgreSQL using psql command-line client
4. **Create** your first database and table
5. **Execute** basic SQL queries (INSERT, SELECT, UPDATE, DELETE)

## Platform-Specific Installation

Choose your operating system and follow the installation steps.

### Windows Installation

**Step 1: Download the Installer**

1. Visit [postgresql.org/download/windows](https://www.postgresql.org/download/windows/)
2. Click "Download the installer" from EnterpriseDB
3. Select PostgreSQL version 16 (latest stable)
4. Choose Windows x86-64 installer

**Step 2: Run the Installer**

1. Double-click the downloaded `.exe` file
2. Follow the installation wizard:
   - Click **Next** on welcome screen
   - Keep default installation directory (`C:\Program Files\PostgreSQL\16`)
   - Select components:
     - ✓ PostgreSQL Server (required)
     - ✓ pgAdmin 4 (GUI tool, recommended)
     - ✓ Command Line Tools (required)
     - ✓ Stack Builder (optional)
   - Keep default data directory (`C:\Program Files\PostgreSQL\16\data`)
   - Set superuser password (remember this - you'll need it)
   - Keep default port: `5432`
   - Keep default locale
   - Click **Next** and **Install**

**Step 3: Verify Installation**

Open Command Prompt or PowerShell and run:

```cmd
psql --version
```

Expected output:

```
psql (PostgreSQL) 16.X
```

**Step 4: Start PostgreSQL Service**

PostgreSQL runs as a Windows service. Verify it's running:

```cmd
REM Check service status
sc query postgresql-x64-16

REM Start service if stopped
net start postgresql-x64-16
```

**Expected service status**:

```
STATE              : 4  RUNNING
```

**Troubleshooting Windows**:

- If `psql --version` fails, add `C:\Program Files\PostgreSQL\16\bin` to PATH
- If service fails to start, check port 5432 is not in use
- If connection fails, verify firewall allows localhost connections

### macOS Installation

**Using Homebrew** (recommended):

**Step 1: Install Homebrew**

If not already installed:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

**Step 2: Install PostgreSQL**

```bash
brew install postgresql@16

echo 'export PATH="/usr/local/opt/postgresql@16/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

**Step 3: Start PostgreSQL Service**

```bash
brew services start postgresql@16

brew services list | grep postgresql
```

**Expected output**:

```
postgresql@16 started
```

**Step 4: Verify Installation**

```bash
psql --version
```

**Expected output**:

```
psql (PostgreSQL) 16.X
```

**Using Postgres.app** (alternative, GUI-friendly):

1. Download Postgres.app from [postgresapp.com](https://postgresapp.com/)
2. Move Postgres.app to Applications folder
3. Open Postgres.app and click "Initialize"
4. Add to PATH: `sudo mkdir -p /etc/paths.d && echo /Applications/Postgres.app/Contents/Versions/latest/bin | sudo tee /etc/paths.d/postgresapp`

**Troubleshooting macOS**:

- If `psql` not found, verify PATH includes `/usr/local/opt/postgresql@16/bin`
- If service fails, check logs: `brew services info postgresql@16`
- If port conflict, change port in `postgresql.conf`

### Linux Installation

**Ubuntu/Debian**:

**Step 1: Update Package List**

```bash
sudo apt update
```

**Step 2: Install PostgreSQL**

```bash
sudo apt install -y postgresql-16 postgresql-contrib-16

```

**Step 3: Verify Installation**

```bash
psql --version
```

**Expected output**:

```
psql (PostgreSQL) 16.X (Ubuntu 16.X-1.pgdg22.04+1)
```

**Step 4: Check Service Status**

```bash
sudo systemctl status postgresql
```

**Expected output**:

```
● postgresql.service - PostgreSQL RDBMS
   Active: active (running)
```

**Fedora/RHEL/CentOS**:

```bash
sudo dnf install -y postgresql16-server postgresql16-contrib

sudo postgresql-16-setup initdb

sudo systemctl start postgresql-16
sudo systemctl enable postgresql-16

psql --version
```

**Arch Linux**:

```bash
sudo pacman -S postgresql

sudo -u postgres initdb -D /var/lib/postgres/data

sudo systemctl start postgresql
sudo systemctl enable postgresql

psql --version
```

**Troubleshooting Linux**:

- If service fails to start, check logs: `sudo journalctl -u postgresql`
- If authentication fails, edit `/etc/postgresql/16/main/pg_hba.conf`
- If port conflict, check `postgresql.conf` for port setting

## Docker Installation (Cross-Platform)

Docker provides isolated PostgreSQL instances ideal for development.

### Prerequisites

Install Docker Desktop:

- **Windows/macOS**: Download from [docker.com](https://www.docker.com/products/docker-desktop)
- **Linux**: Install via package manager (docker.io or docker-ce)

### Pull and Run PostgreSQL Container

```bash
docker pull postgres:16

docker run --name postgres-tutorial \
  -e POSTGRES_PASSWORD=mypassword \
  -e POSTGRES_USER=tutorialuser \
  -e POSTGRES_DB=tutorialdb \
  -p 5432:5432 \
  -d postgres:16
```

**Flags explained**:

- `--name postgres-tutorial`: Container name
- `-e POSTGRES_PASSWORD=mypassword`: Set superuser password
- `-e POSTGRES_USER=tutorialuser`: Create user (default: postgres)
- `-e POSTGRES_DB=tutorialdb`: Create database
- `-p 5432:5432`: Map port 5432 (host:container)
- `-d`: Run in background (detached)
- `postgres:16`: Use PostgreSQL 16 image

### Verify Docker Container

```bash
docker ps | grep postgres-tutorial

docker logs postgres-tutorial
```

**Expected log output**:

```
PostgreSQL init process complete; ready for start up.
database system is ready to accept connections
```

### Connect to Docker PostgreSQL

```bash
docker exec -it postgres-tutorial psql -U tutorialuser -d tutorialdb
```

You should see the PostgreSQL prompt:

```
tutorialdb=#
```

## First Connection

Connect to PostgreSQL using the `psql` command-line client.

### macOS/Linux Connection

**Using default postgres user**:

```bash
sudo -u postgres psql

psql -U postgres
```

**Using custom user** (if created during installation):

```bash
psql -U yourusername -d postgres
```

### Windows Connection

Open Command Prompt or PowerShell:

```cmd
REM Connect as postgres superuser
psql -U postgres

REM Enter password when prompted (set during installation)
```

### Docker Connection

```bash
docker exec -it postgres-tutorial psql -U tutorialuser -d tutorialdb
```

### Verify Connection

You should see the PostgreSQL prompt:

```
postgres=#
```

**Troubleshooting Connection**:

- If "psql: command not found", PostgreSQL bin directory not in PATH
- If "connection refused", PostgreSQL service not running
- If "authentication failed", check password or edit `pg_hba.conf`

## Your First Database

Create your first database and table.

### Create Database

From psql prompt:

```sql
-- Create a new database
CREATE DATABASE myapp;
```

**Expected output**:

```
CREATE DATABASE
```

### Connect to New Database

```sql
-- Connect to the new database
\c myapp
```

**Expected output**:

```
You are now connected to database "myapp" as user "postgres".
```

### Create Table

```sql
-- Create a users table
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(100) NOT NULL UNIQUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

**Expected output**:

```
CREATE TABLE
```

**Table structure explained**:

- `id SERIAL PRIMARY KEY`: Auto-incrementing integer primary key
- `username VARCHAR(50) NOT NULL UNIQUE`: Text up to 50 chars, required, unique
- `email VARCHAR(100) NOT NULL UNIQUE`: Text up to 100 chars, required, unique
- `created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP`: Timestamp, defaults to now

### Verify Table Creation

```sql
-- List all tables in current database
\dt

-- Describe table structure
\d users
```

**Expected output**:

```
          List of relations
 Schema | Name  | Type  |  Owner
--------+-------+-------+----------
 public | users | table | postgres

                                          Table "public.users"
   Column   |            Type             | Collation | Nullable |              Default
------------+-----------------------------+-----------+----------+-----------------------------------
 id         | integer                     |           | not null | nextval('users_id_seq'::regclass)
 username   | character varying(50)       |           | not null |
 email      | character varying(100)      |           | not null |
 created_at | timestamp without time zone |           |          | CURRENT_TIMESTAMP
Indexes:
    "users_pkey" PRIMARY KEY, btree (id)
    "users_email_key" UNIQUE CONSTRAINT, btree (email)
    "users_username_key" UNIQUE CONSTRAINT, btree (username)
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
INSERT 0 1
INSERT 0 3
```

The number after `INSERT 0` indicates rows inserted.

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

**Expected output (SELECT \* FROM users)**:

```
 id | username |       email         |       created_at
----+----------+---------------------+------------------------
  1 | alice    | alice@example.com   | 2026-01-29 10:30:45.123
  2 | bob      | bob@example.com     | 2026-01-29 10:30:45.456
  3 | charlie  | charlie@example.com | 2026-01-29 10:30:45.789
  4 | diana    | diana@example.com   | 2026-01-29 10:30:46.012
(4 rows)
```

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
UPDATE 1
UPDATE 2
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
DELETE 1
DELETE 0
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
 count
-------
     3
(1 row)
```

## Useful psql Commands

Navigate and manage PostgreSQL using psql meta-commands.

### Database Commands

```sql
-- List all databases
\l

-- Connect to different database
\c database_name

-- Show current database
SELECT current_database();

-- Drop database (use with caution!)
DROP DATABASE database_name;
```

### Table Commands

```sql
-- List all tables in current database
\dt

-- Describe table structure
\d table_name

-- Show table with indexes
\d+ table_name

-- List all schemas
\dn

-- Drop table (use with caution!)
DROP TABLE table_name;
```

### User and Permission Commands

```sql
-- List all users/roles
\du

-- Show current user
SELECT current_user;

-- Create new user
CREATE USER newuser WITH PASSWORD 'password';

-- Grant privileges
GRANT ALL PRIVILEGES ON DATABASE myapp TO newuser;
```

### Query and Output Commands

```sql
-- Execute SQL from file
\i /path/to/file.sql

-- Toggle expanded output (vertical format)
\x

-- Set output format
\pset format aligned

-- Save query output to file
\o output.txt

-- Time query execution
\timing

-- Show query history
\s
```

### Help and Exit

```sql
-- Get help on SQL commands
\h CREATE TABLE

-- Get help on psql commands
\?

-- Exit psql
\q
```

## Environment Variables

Configure PostgreSQL client behavior using environment variables.

### Key Environment Variables

**PGHOST**: PostgreSQL server hostname (default: localhost)

```bash
export PGHOST=localhost
```

**PGPORT**: PostgreSQL server port (default: 5432)

```bash
export PGPORT=5432
```

**PGUSER**: Default user for connections

```bash
export PGUSER=postgres
```

**PGPASSWORD**: Password (not recommended for security reasons)

```bash
export PGPASSWORD=mypassword  # Better to use .pgpass file
```

**PGDATABASE**: Default database to connect to

```bash
export PGDATABASE=myapp
```

### Secure Password Storage (.pgpass)

Create `~/.pgpass` file for password storage:

**macOS/Linux**:

```bash
cat > ~/.pgpass <<EOF
localhost:5432:*:postgres:mypassword
localhost:5432:myapp:postgres:mypassword
EOF

chmod 600 ~/.pgpass
```

**Windows**: Create `%APPDATA%\postgresql\pgpass.conf`:

```
localhost:5432:*:postgres:mypassword
localhost:5432:myapp:postgres:mypassword
```

**Format**: `hostname:port:database:username:password`

Use `*` as wildcard for any database.

### Verify Environment

```bash
env | grep PG

psql  # Connects using PGHOST, PGPORT, PGUSER, PGDATABASE
```

## Configuration Files

PostgreSQL configuration files control server behavior.

### postgresql.conf

Main configuration file (performance, connections, logging).

**Location**:

- **Ubuntu/Debian**: `/etc/postgresql/16/main/postgresql.conf`
- **Fedora/RHEL**: `/var/lib/pgsql/16/data/postgresql.conf`
- **macOS (Homebrew)**: `/usr/local/var/postgresql@16/postgresql.conf`
- **Windows**: `C:\Program Files\PostgreSQL\16\data\postgresql.conf`

**Key settings**:

```conf
max_connections = 100

listen_addresses = 'localhost'

port = 5432

shared_buffers = 128MB

wal_level = replica
```

### pg_hba.conf

Client authentication configuration (who can connect from where).

**Location**: Same directory as `postgresql.conf`

**Example entries**:

```conf
local   all             all                                     peer
host    all             all             127.0.0.1/32            md5
host    all             all             ::1/128                 md5
```

**Authentication methods**:

- `peer`: Use OS username (local connections)
- `md5`: Password authentication (encrypted)
- `trust`: Allow without password (development only)
- `scram-sha-256`: Modern password authentication

**Reload configuration** after changes:

```bash
sudo systemctl reload postgresql

SELECT pg_reload_conf();
```

## Next Steps

You now have PostgreSQL installed and working. Here's what to learn next:

1. **[Quick Start](/en/learn/software-engineering/data/databases/postgresql/quick-start)** - Build a complete application with schemas, queries, and indexes (5-30% coverage)
2. **[By-Example Tutorial](/en/learn/software-engineering/data/databases/postgresql/by-example)** - Learn through 85 annotated examples covering 95% of PostgreSQL
3. **[Official PostgreSQL Documentation](https://www.postgresql.org/docs/)** - Comprehensive reference and guides

## Summary

In this initial setup tutorial, you learned how to:

1. Install PostgreSQL on Windows, macOS, or Linux (or using Docker)
2. Start and verify PostgreSQL service
3. Connect to PostgreSQL using psql client
4. Create your first database and table
5. Execute basic SQL queries (INSERT, SELECT, UPDATE, DELETE)
6. Use psql meta-commands for database management
7. Configure environment variables and authentication

You're now ready to explore PostgreSQL's powerful features: advanced queries, indexes, JSON support, full-text search, and more. Continue to the Quick Start tutorial to build a real application.

## Common Issues and Solutions

### Service Won't Start

**Problem**: PostgreSQL service fails to start

**Solutions**:

1. Check port 5432 is not already in use: `netstat -an | grep 5432`
2. Review logs for errors: `sudo journalctl -u postgresql` (Linux)
3. Verify data directory permissions: `ls -la /var/lib/postgresql/16/main`

### Connection Refused

**Problem**: psql connection refused

**Solutions**:

1. Verify PostgreSQL service is running: `sudo systemctl status postgresql`
2. Check `listen_addresses` in `postgresql.conf` includes your connection source
3. Verify firewall allows port 5432

### Authentication Failed

**Problem**: Password authentication fails

**Solutions**:

1. Check `pg_hba.conf` for correct authentication method
2. Verify password is correct (use `.pgpass` file)
3. Try `trust` method temporarily for local debugging (change back for production)

### psql Command Not Found

**Problem**: `psql` command not recognized

**Solutions**:

1. Add PostgreSQL bin directory to PATH
2. Verify installation completed successfully
3. Restart terminal to reload PATH

## Additional Resources

- [Official PostgreSQL Documentation](https://www.postgresql.org/docs/)
- [PostgreSQL Wiki](https://wiki.postgresql.org/)
- [PostgreSQL Tutorial](https://www.postgresqltutorial.com/)
- [pgAdmin Documentation](https://www.pgadmin.org/docs/)
- [PostgreSQL Mailing Lists](https://www.postgresql.org/list/)
