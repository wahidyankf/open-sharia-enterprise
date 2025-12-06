---
title: "DML: SELECT DISTINCT"
date: 2025-03-16T07:20:00+07:00
draft: false
---

`SELECT DISTINCT` is a statement in SQL that returns only distinct (different) values. The `DISTINCT` keyword eliminates duplicate records from the results. This command is particularly useful when dealing with large datasets where duplicate entries can distort the analysis.

## Syntax

The basic syntax for `SELECT DISTINCT` in SQL is:

```sql
SELECT DISTINCT column_name_1, column_name_2, ...
FROM table_name;
```

- `SELECT DISTINCT` specifies the columns you want to retrieve.
- `FROM` specifies the table from which to retrieve the data.

Here, `column_name_1, column_name_2, ...` are the names of the columns in the table from which you want to select data.

## Examples

Consider the following table `order`.

| order_id | customer | amount |
| -------- | -------- | ------ |
| 1        | John     | 30     |
| 2        | Jane     | 45     |
| 3        | John     | 20     |
| 4        | Jane     | 30     |
| 5        | John     | 30     |

### Example 1: Selecting distinct customers

If you want to select all distinct customers from the `order` table, you will use the following SQL statement:

```sql
SELECT DISTINCT customer
FROM order;
```

This will return:

customer

---

John

---

Jane

---

### Example 2: Selecting distinct amount values

If you want to select all distinct amounts from the `order` table, you will use the following SQL statement:

```sql
SELECT DISTINCT amount
FROM order;
```

This will return:

amount

---

30

---

45

---

20

---

### Example 3: Selecting across multiple columns

The `SELECT DISTINCT` statement can also be used for two or more columns. Suppose you want to select all unique `customer` and `amount` combinations; you would use the following SQL statement:

```sql
SELECT DISTINCT customer, amount
FROM order;
```

This will return:

| customer | amount |
| -------- | ------ |
| John     | 30     |
| Jane     | 45     |
| John     | 20     |
| Jane     | 30     |

## Important Points

- `DISTINCT` keyword can be used with more than one column. In this case, the DISTINCT keyword will eliminate those rows where all the selected fields are identical.
- The `DISTINCT` keyword keeps one row for each group of duplicates.
- The `DISTINCT` keyword will consider NULL as a unique value. Therefore, if you have multiple NULLs in your column, `SELECT DISTINCT` will only show one NULL.

## Further Readings

1. [W3Schools SQL SELECT DISTINCT Statement](https://www.w3schools.com/sql/sql_distinct.asp)
2. [SQL Server SELECT DISTINCT | Microsoft Docs](https://docs.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver15)
3. [Oracle / PLSQL: SELECT Statement](https://www.techonthenet.com/oracle/select.php)
4. [PostgreSQL SELECT DISTINCT](https://www.postgresqltutorial.com/postgresql-select-distinct/)
