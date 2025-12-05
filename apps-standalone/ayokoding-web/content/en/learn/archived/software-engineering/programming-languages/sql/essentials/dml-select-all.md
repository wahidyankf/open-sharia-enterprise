---
title: "DML: SELECT ALL"
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Overview

SQL, or Structured Query Language, is a standard language for managing data in relational databases. DML, or Data Manipulation Language, is a subset of SQL and includes commands that allow you to manipulate data in a database. This typically includes operations like insert, delete, update, and select.

The `SELECT ALL` statement is a DML command in SQL that is used to fetch all the data from a database table. Here's how you can use the command:

```sql
SELECT ALL * FROM table_name;
```

In this command, `*` is a wildcard for "all columns". The `FROM` clause specifies the table from where the data should be fetched.

## Examples

Let's consider a database table `employee` with the following data:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 30  | IT         |
| 2   | Jane       | Doe       | 32  | HR         |
| 3   | Jim        | Smith     | 45  | Sales      |
| 4   | Jill       | Johnson   | 25  | Marketing  |
| 5   | Jack       | Brown     | 35  | IT         |

To fetch all the data from this table, you can use the `SELECT ALL` statement as follows:

```sql
SELECT ALL * FROM employee;
```

This will return:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 30  | IT         |
| 2   | Jane       | Doe       | 32  | HR         |
| 3   | Jim        | Smith     | 45  | Sales      |
| 4   | Jill       | Johnson   | 25  | Marketing  |
| 5   | Jack       | Brown     | 35  | IT         |

Note that `SELECT ALL` is the same as `SELECT` - the `ALL` keyword is optional and is generally omitted in practice.

## Further Readings

- [SQL SELECT Statement](https://www.w3schools.com/sql/sql_select.asp) - A beginner-friendly guide from W3Schools on how to use the SQL SELECT statement.
- [SQL Tutorial](https://www.postgresqltutorial.com/) - Comprehensive SQL guide, covering many topics including DML commands.
