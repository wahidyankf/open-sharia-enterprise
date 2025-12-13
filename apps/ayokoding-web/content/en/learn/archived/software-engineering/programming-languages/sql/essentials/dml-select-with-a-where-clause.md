---
title: "DML: SELECT with a WHERE clause"
weight: 603
date: 2025-03-16T07:20:00+07:00
draft: false
---

## Basic Introduction

The `SELECT` statement is one of the fundamental elements of SQL (Structured Query Language). It allows you to retrieve data from a database. However, if you want to retrieve specific records from one or more tables in a database, the `WHERE` clause is what you use. The `WHERE` clause describes the condition for a record to be selected.

Here's the basic syntax:

```sql
SELECT column1, column2, ..., columnN
FROM table_name
WHERE condition;
```

You can replace `column1, column2, ..., columnN` with the specific columns you want to select from the table. If you want to select all columns, use `*`.

The `condition` is a criterion that must be met for a row in the table to be included in the result set. It could involve logical operators such as `=`, `<>`, `<`, `>`, `<=`, `>=`, etc.

## Examples

Suppose we have the following `employee` table:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 28  | IT         |
| 2   | Jane       | Doe       | 32  | Sales      |
| 3   | Mark       | Smith     | 30  | IT         |
| 4   | Sarah      | Johnson   | 29  | HR         |

Now, we want to select employees who work in the IT department. We can write:

```sql
SELECT *
FROM employee
WHERE department = 'IT';
```

The result will be:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 28  | IT         |
| 3   | Mark       | Smith     | 30  | IT         |

We can also combine conditions using logical operators like `AND`, `OR`, `NOT`. For instance, to select employees in the IT department who are older than 28:

```sql
SELECT *
FROM employee
WHERE department = 'IT' AND age > 28;
```

The result will be:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 3   | Mark       | Smith     | 30  | IT         |

### More Examples

**Multiple Conditions**

Suppose you want to get the details of employees from the 'IT' department and whose age is less than or equal to 28. You can use the `AND` keyword to combine these conditions.

```sql
SELECT *
FROM employee
WHERE department = 'IT' AND age <= 28;
```

The result will be:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 28  | IT         |

**Select with `OR`**

If you want to select employees who are either from the 'IT' department or from the 'Sales' department, you can use the `OR` keyword:

```sql
SELECT *
FROM employee
WHERE department = 'IT' OR department = 'Sales';
```

The result will be:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 28  | IT         |
| 2   | Jane       | Doe       | 32  | Sales      |
| 3   | Mark       | Smith     | 30  | IT         |

**Using `NOT`**

The `NOT` keyword can be used to select records where the condition is NOT TRUE. For instance, if you want to select employees who are not in the 'HR' department:

```sql
SELECT *
FROM employee
WHERE NOT department = 'HR';
```

The result will be:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 28  | IT         |
| 2   | Jane       | Doe       | 32  | Sales      |
| 3   | Mark       | Smith     | 30  | IT         |

**Using `LIKE`**

The `WHERE` clause uses the `LIKE` operator to search for a specific pattern. If we want to select all employees whose first name begins with 'J':

```sql
SELECT *
FROM employee
WHERE first_name LIKE 'J%';
```

The result will be:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 1   | John       | Doe       | 28  | IT         |
| 2   | Jane       | Doe       | 32  | Sales      |

**Using `BETWEEN`**

The `BETWEEN` operator selects values within a given range. The values can be numbers, text, or dates. For instance, to select employees who are between 30 and 40 years old:

```sql
SELECT *
FROM employee
WHERE age BETWEEN 30 AND 40;
```

The result will be:

| id  | first_name | last_name | age | department |
| --- | ---------- | --------- | --- | ---------- |
| 2   | Jane       | Doe       | 32  | Sales      |
| 3   | Mark       | Smith     | 30  | IT         |

## Further Readings

1. [SQL SELECT Statement - W3Schools](https://www.w3schools.com/sql/sql_select.asp)
2. [SQL WHERE Clause - W3Schools](https://www.w3schools.com/sql/sql_where.asp)
3. [SQL: SELECT Statement](https://www.sqltutorial.org/sql-select/)
4. [SQL WHERE Clause - SQLTutorial](https://www.sqltutorial.org/sql-where/)
