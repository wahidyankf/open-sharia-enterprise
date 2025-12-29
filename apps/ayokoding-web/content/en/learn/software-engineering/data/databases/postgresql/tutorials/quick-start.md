---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn essential PostgreSQL operations and SQL fundamentals"
tags:
  - postgresql
  - sql
  - database
  - quick-start
---

Learn essential PostgreSQL operations and SQL fundamentals to work with databases effectively. This Quick Start teaches core concepts you need to build database-driven applications.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- CRUD operations (Create, Read, Update, Delete)
- Joins and relationships
- Indexes and performance
- Transactions and data integrity

## 📋 Prerequisites

- PostgreSQL installed and running (see [Initial Setup](/en/learn/software-engineering/data/databases/postgresql/tutorials/initial-setup))
- Basic SQL knowledge helpful but not required

## 📊 CRUD Operations

### Create (INSERT)

```sql
INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com');

INSERT INTO users (name, email) VALUES
    ('Bob', 'bob@example.com'),
    ('Charlie', 'charlie@example.com');

INSERT INTO users (name, email) VALUES ('Diana', 'diana@example.com') RETURNING *;
```

### Read (SELECT)

```sql
SELECT * FROM users;
SELECT name, email FROM users;
SELECT * FROM users WHERE email LIKE '%@example.com';
SELECT * FROM users ORDER BY created_at DESC;
SELECT * FROM users LIMIT 10;
SELECT * FROM users LIMIT 10 OFFSET 20;
```

### Update (UPDATE)

```sql
UPDATE users SET email = 'newemail@example.com' WHERE id = 1;
UPDATE users SET name = 'Alice Johnson', email = 'alice.j@example.com' WHERE id = 1;
UPDATE users SET updated_at = CURRENT_TIMESTAMP WHERE id = 1 RETURNING *;
```

### Delete (DELETE)

```sql
DELETE FROM users WHERE id = 1;
DELETE FROM users WHERE created_at < '2023-01-01';
```

## 🔗 Relationships and Joins

### One-to-Many Relationship

```sql
CREATE TABLE posts (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id) ON DELETE CASCADE,
    title VARCHAR(200) NOT NULL,
    content TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO posts (user_id, title, content) VALUES
    (1, 'First Post', 'Hello World'),
    (1, 'Second Post', 'PostgreSQL is great'),
    (2, 'Bobs Post', 'My first post');
```

### INNER JOIN

```sql
SELECT users.name, posts.title
FROM users
INNER JOIN posts ON users.id = posts.user_id;
```

### LEFT JOIN

```sql
SELECT users.name, COUNT(posts.id) as post_count
FROM users
LEFT JOIN posts ON users.id = posts.user_id
GROUP BY users.id, users.name;
```

## ⚡ Indexes for Performance

```sql
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_posts_user_id ON posts(user_id);
CREATE UNIQUE INDEX idx_users_email_unique ON users(email);

\d users
```

## 🔒 Transactions

```sql
BEGIN;
INSERT INTO users (name, email) VALUES ('Eve', 'eve@example.com');
INSERT INTO posts (user_id, title) VALUES (CURRVAL('users_id_seq'), 'Eves First Post');
COMMIT;

BEGIN;
UPDATE users SET email = 'wrong@example.com' WHERE id = 1;
ROLLBACK;
```

## 🎨 Common Patterns

### Aggregate Functions

```sql
SELECT COUNT(*) FROM users;
SELECT AVG(id) FROM users;
SELECT MAX(created_at), MIN(created_at) FROM users;
SELECT user_id, COUNT(*) as post_count FROM posts GROUP BY user_id;
SELECT user_id, COUNT(*) as post_count FROM posts GROUP BY user_id HAVING COUNT(*) > 1;
```

### Subqueries

```sql
SELECT * FROM users WHERE id IN (SELECT DISTINCT user_id FROM posts);
SELECT name FROM users WHERE id = (SELECT user_id FROM posts WHERE title = 'First Post');
```

### JSON Support

```sql
CREATE TABLE settings (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    preferences JSONB DEFAULT '{}'::jsonb
);

INSERT INTO settings (user_id, preferences) VALUES
    (1, '{"theme": "dark", "notifications": true}'),
    (2, '{"theme": "light", "notifications": false}');

SELECT preferences->>'theme' as theme FROM settings WHERE user_id = 1;
SELECT * FROM settings WHERE preferences @> '{"theme": "dark"}';
```

## ✅ Next Steps

You now understand PostgreSQL essentials! To deepen your knowledge:

1. **Try the examples**: Execute each SQL statement in psql
2. **Explore By Example**: [PostgreSQL By Example](/en/learn/software-engineering/data/databases/postgresql/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Perform CRUD operations with SQL
- [ ] Create relationships between tables
- [ ] Use JOIN operations to query related data
- [ ] Create indexes for performance
- [ ] Use transactions for data integrity
- [ ] Work with aggregate functions and grouping
