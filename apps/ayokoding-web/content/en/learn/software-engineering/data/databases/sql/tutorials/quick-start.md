---
title: "Quick Start"
date: 2025-12-30T06:12:21+07:00
draft: false
weight: 100002
description: "Learn essential SQL operations and query patterns"
tags:
  - sql
  - database
  - query
  - quick-start
---

Learn essential SQL operations and query patterns for effective database work. This Quick Start teaches core SQL concepts applicable to all SQL databases.

## 🎯 What You'll Learn

By the end of this tutorial, you'll understand:

- SELECT queries with filtering and sorting
- Joins and relationships
- Aggregate functions and grouping
- Subqueries and advanced patterns

## 📋 Prerequisites

- SQLite installed (see [Initial Setup](/en/learn/software-engineering/data/databases/sql/tutorials/initial-setup))
- Basic understanding of relational databases

## 📊 SELECT Queries

### Basic SELECT

```sql
SELECT * FROM users;
SELECT name, email FROM users;
SELECT DISTINCT email FROM users;
```

### Filtering with WHERE

```sql
SELECT * FROM users WHERE age > 25;
SELECT * FROM users WHERE name LIKE 'A%';
SELECT * FROM users WHERE email = 'alice@example.com';
SELECT * FROM users WHERE age BETWEEN 20 AND 30;
SELECT * FROM users WHERE name IN ('Alice', 'Bob', 'Charlie');
```

### Sorting with ORDER BY

```sql
SELECT * FROM users ORDER BY name;
SELECT * FROM users ORDER BY created_at DESC;
SELECT * FROM users ORDER BY age DESC, name ASC;
```

### Limiting Results

```sql
SELECT * FROM users LIMIT 10;
SELECT * FROM users LIMIT 10 OFFSET 20;
```

## 🔗 Joins

### INNER JOIN

```sql
SELECT users.name, posts.title
FROM users
INNER JOIN posts ON users.id = posts.user_id;
```

### LEFT JOIN

```sql
SELECT users.name, posts.title
FROM users
LEFT JOIN posts ON users.id = posts.user_id;
```

### RIGHT JOIN

```sql
SELECT users.name, posts.title
FROM users
RIGHT JOIN posts ON users.id = posts.user_id;
```

## 📈 Aggregate Functions

```sql
SELECT COUNT(*) FROM users;
SELECT AVG(age) FROM users;
SELECT MAX(age), MIN(age) FROM users;
SELECT SUM(price) FROM orders;
```

### GROUP BY

```sql
SELECT user_id, COUNT(*) as post_count
FROM posts
GROUP BY user_id;

SELECT user_id, COUNT(*) as post_count
FROM posts
GROUP BY user_id
HAVING COUNT(*) > 5;
```

## 🎨 Subqueries

```sql
SELECT * FROM users
WHERE id IN (SELECT DISTINCT user_id FROM posts);

SELECT name FROM users
WHERE id = (SELECT user_id FROM posts WHERE title = 'First Post');
```

## ✅ Next Steps

You now understand SQL essentials! To deepen your knowledge:

1. **Try the examples**: Execute each SQL statement
2. **Explore By Example**: [SQL By Example](/en/learn/software-engineering/data/databases/sql/tutorials/by-example)

## 🎯 Self-Assessment

After completing this Quick Start, you should be able to:

- [ ] Write SELECT queries with filtering
- [ ] Use JOIN operations
- [ ] Apply aggregate functions
- [ ] Group and filter results
- [ ] Write subqueries
