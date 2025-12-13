---
title: "DDL: CREATE"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL, or Structured Query Language, is designed to manage data in a relational database management system (RDBMS). SQL includes several types of statements, but we will focus on the Data Definition Language (DDL) subset, particularly the `CREATE` statement.

## Overview

In SQL, the `CREATE` statement is a part of the Data Definition Language (DDL) used to create objects like databases, tables, indexes, etc., in a database. Once an object is created, you can perform different operations like inserting data, updating data, or drop the object.

## Creating Databases

To create a new database, you use the `CREATE DATABASE` statement. The syntax is as follows:

```sql
CREATE DATABASE database_name;
```

For example, to create a new database named `students_db`, you use the following statement:

```sql
CREATE DATABASE students_db;
```

## Creating Tables

The `CREATE TABLE` statement is used to create a new table in a database. The syntax is as follows:

```sql
CREATE TABLE table_name (
    column1 datatype,
    column2 datatype,
    ...
);
```

For example, to create a new table named `student` with columns `id`, `name`, and `age`, you use the following statement:

```sql
CREATE TABLE student (
    id INT,
    name VARCHAR(100),
    age INT
);
```

## Creating Indexes

An index is used to speed up the performance of queries. It makes the query-fetching process faster. You use the `CREATE INDEX` statement to create an index. The syntax is as follows:

```sql
CREATE INDEX index_name
ON table_name (column1, column2, ...);
```

For example, to create an index on the `name` column of the `student` table, you use the following statement:

```sql
CREATE INDEX idx_student_name
ON student (name);
```

## Constraints in SQL

Constraints are used to limit the type of data that can go into a table. This ensures the accuracy and reliability of the data in the table. Constraints could be column level or table level. The column-level constraints apply to a column, and table-level constraints apply to the whole table.

The following are commonly used constraints available in SQL:

- `NOT NULL`: Ensures that a column cannot have a NULL value.
- `UNIQUE`: Ensures that all values in a column are different.
- `PRIMARY KEY`: A combination of a `NOT NULL` and `UNIQUE`. Uniquely identifies each row in a table.
- `FOREIGN KEY`: Uniquely identifies a row/record in another table.
- `CHECK`: Ensures that all values in a column satisfy certain conditions.
- `DEFAULT`: Sets a default value for a column if none is specified.

Here is an example of a `CREATE TABLE` statement with constraints:

```sql
CREATE TABLE employee (
    id INT PRIMARY KEY,
    name TEXT NOT NULL,
    age INT NOT NULL,
    salary REAL DEFAULT 50000.00,
    email TEXT UNIQUE CHECK (email LIKE '%_@__%.__%')
);
```

## Further Readings

1. [SQL CREATE DATABASE Statement](https://www.w3schools.com/sql/sql_create_db.asp)
2. [SQL CREATE TABLE Statement](https://www.w3schools.com/sql/sql_create_table.asp)
3. [SQL CREATE INDEX Statement](https://www.w3schools.com/sql/sql_create_index.asp)
4. [SQL Constraints](https://www.w3schools.com/sql/sql_constraints.asp)
5. [SQL - Quick Guide](https://www.tutorialspoint.com/sql/sql-quick-guide.htm)
