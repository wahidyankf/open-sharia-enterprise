---
title: "DML: SELECT"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

SQL, or Structured Query Language, is a standard language for manipulating relational databases. SQL can be subdivided into multiple components, one of which is Data Manipulation Language (DML). DML allows us to modify the database instance by inserting, modifying, and querying data. This document focuses on the `SELECT` statement, a fundamental aspect of DML that allows data retrieval from a database.

## Basics of SELECT

The `SELECT` statement is used to query data from one or more tables in your database. The simplest form of a `SELECT` statement is:

```sql
SELECT column_name FROM table_name;
```

In this query, `column_name` is the name of the column from which you want to select data, and `table_name` is the name of the table where that column resides.

**Example:**

Let's assume we have a table named `employee` with the columns `employee_id`, `first_name`, `last_name`, and `salary`. To select all the first names from this table, you'd write:

```sql
SELECT first_name FROM employee;
```

## SELECT DISTINCT

The `SELECT DISTINCT` statement returns only distinct (different) values. This is useful when you want to know all the unique entries in a particular column.

```sql
SELECT DISTINCT column_name FROM table_name;
```

**Example:**

To select all distinct salaries from the `employee` table, you'd write:

```sql
SELECT DISTINCT salary FROM employee;
```

## SELECT ALL

By default, SQL `SELECT` statement returns all the rows that satisfy the criteria of the query. The `ALL` keyword is used to explicitly state this default behavior.

```sql
SELECT ALL column_name FROM table_name;
```

## SELECT with WHERE Clause

The `WHERE` clause is used to filter records. It is used to extract only those records that fulfill a specified condition.

```sql
SELECT column1, column2, ...
FROM table_name
WHERE condition;
```

**Example:**

To select the first names of employees with a salary greater than 50000 from the `employee` table, you'd write:

```sql
SELECT first_name FROM employee WHERE salary > 50000;
```

## SELECT with JOINs

SQL JOINs combine rows from two or more tables based on a related column.

The most common types of SQL JOINs are `INNER JOIN`, `LEFT JOIN`, `RIGHT JOIN`, and `FULL JOIN`.

**Example:**

Assume we have another table `department` and we want to select all employees and their department names.

```sql
SELECT employee.first_name, department.department_name
FROM employee
INNER JOIN department
ON employee.department_id = department.department_id;
```

## Conclusion

The SQL `SELECT` statement is a critical tool for any data professional. Its uses are extensive, from basic data retrieval to complex data manipulation with JOINs and WHERE conditions. Mastery of this statement is a crucial step in becoming proficient in SQL.

## Further Readings

- [SQL SELECT Statement - W3Schools](https://www.w3schools.com/sql/sql_select.asp)
- [Introduction to SQL SELECT statement - SQL Server](https://learn.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql)
- [SQL SELECT Statement - Mode Analytics](https://mode.com/sql-tutorial/sql-select-statement/)
